#!/usr/bin/env python3
"""
Bloomberg Terminal Data Puller
Supports multiple Bloomberg APIs: Desktop API, Server API, and BQNT
"""

import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import logging
from typing import List, Dict, Optional, Union
import warnings
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import seaborn as sns
from matplotlib.figure import Figure
from matplotlib.axes import Axes
import requests
import os

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Set style for better-looking charts
plt.style.use('seaborn-v0_8')
sns.set_palette("husl")

# Try to import fredapi, fall back to requests if not available
try:
    from fredapi import Fred
    FREDAPI_AVAILABLE = True
except ImportError:
    FREDAPI_AVAILABLE = False
    logger.warning("fredapi not installed. Install with: pip install fredapi")

class BloombergDataPuller:
    """
    Bloomberg data puller for Desktop API only
    Automatically sets date ranges to 5 years from current date
    """
    
    def __init__(self):
        """
        Initialize Bloomberg Desktop API connection
        """
        self.session = None
        self.service = None
        self.current_date = datetime.now()
        self.five_years_ago = self.current_date - timedelta(days=5*365)
        
        try:
            self._init_desktop_api()
        except ImportError as e:
            logger.error(f"Bloomberg API not available: {e}")
            logger.info("Make sure Bloomberg Terminal is running and Python API is installed")
            raise
    
    def _init_desktop_api(self):
        """Initialize Bloomberg Desktop API (blpapi)"""
        try:
            import blpapi
            
            # Start session
            sessionOptions = blpapi.SessionOptions()
            sessionOptions.setServerHost('localhost')
            sessionOptions.setServerPort(8194)
            
            self.session = blpapi.Session(sessionOptions)
            
            if not self.session.start():
                raise Exception("Failed to start Bloomberg session")
            
            if not self.session.openService("//blp/refdata"):
                raise Exception("Failed to open Bloomberg reference data service")
            
            self.service = self.session.getService("//blp/refdata")
            logger.info(f"Bloomberg Desktop API initialized successfully")
            logger.info(f"Default date range: {self.five_years_ago.strftime('%Y-%m-%d')} to {self.current_date.strftime('%Y-%m-%d')}")
            
        except ImportError:
            raise ImportError("blpapi not installed. Install with: pip install blpapi")
    
    def get_reference_data(self, securities: Union[str, List[str]], 
                          fields: Union[str, List[str]]) -> pd.DataFrame:
        """
        Get reference data for securities
        
        Args:
            securities: Security or list of securities (e.g., 'AAPL US Equity')
            fields: Field or list of fields (e.g., 'PX_LAST', 'VOLUME')
        
        Returns:
            DataFrame with reference data
        """
        return self._get_reference_data_blpapi(securities, fields)
    
    def _get_reference_data_blpapi(self, securities, fields):
        """Get reference data using blpapi"""
        import blpapi
        
        # Ensure inputs are lists
        if isinstance(securities, str):
            securities = [securities]
        if isinstance(fields, str):
            fields = [fields]
        
        # Create request
        request = self.service.createRequest("ReferenceDataRequest")
        
        # Add securities
        for security in securities:
            request.getElement("securities").appendValue(security)
        
        # Add fields
        for field in fields:
            request.getElement("fields").appendValue(field)
        
        # Send request
        self.session.sendRequest(request)
        
        # Process response
        data = []
        while True:
            event = self.session.nextEvent(100)
            if event.eventType() == blpapi.Event.RESPONSE:
                for msg in event:
                    securityDataArray = msg.getElement("securityData")
                    for i in range(securityDataArray.numValues()):
                        securityData = securityDataArray.getValue(i)
                        security = securityData.getElement("security").getValue()
                        
                        row = {'Security': security}
                        
                        if securityData.hasElement("fieldData"):
                            fieldData = securityData.getElement("fieldData")
                            for field in fields:
                                if fieldData.hasElement(field):
                                    row[field] = fieldData.getElement(field).getValue()
                                else:
                                    row[field] = None
                        
                        data.append(row)
                break
        
        return pd.DataFrame(data)
    
    def get_historical_data(self, securities: Union[str, List[str]], 
                           fields: Union[str, List[str]],
                           start_date: Optional[str] = None, 
                           end_date: Optional[str] = None,
                           frequency: str = 'DAILY') -> pd.DataFrame:
        """
        Get historical data for securities
        Default date range is 5 years from current date
        
        Args:
            securities: Security or list of securities
            fields: Field or list of fields
            start_date: Start date (YYYY-MM-DD), defaults to 5 years ago
            end_date: End date (YYYY-MM-DD), defaults to current date
            frequency: Data frequency ('DAILY', 'WEEKLY', 'MONTHLY')
        
        Returns:
            DataFrame with historical data
        """
        # Set default dates if not provided
        if start_date is None:
            start_date = self.five_years_ago.strftime('%Y-%m-%d')
        if end_date is None:
            end_date = self.current_date.strftime('%Y-%m-%d')
            
        logger.info(f"Pulling historical data from {start_date} to {end_date}")
        return self._get_historical_data_blpapi(securities, fields, start_date, end_date, frequency)
    
    def _get_historical_data_blpapi(self, securities, fields, start_date, end_date, frequency):
        """Get historical data using blpapi"""
        import blpapi
        
        # Ensure inputs are lists
        if isinstance(securities, str):
            securities = [securities]
        if isinstance(fields, str):
            fields = [fields]
        
        # Create request
        request = self.service.createRequest("HistoricalDataRequest")
        
        # Add securities
        for security in securities:
            request.getElement("securities").appendValue(security)
        
        # Add fields
        for field in fields:
            request.getElement("fields").appendValue(field)
        
        # Set dates and frequency
        request.set("startDate", start_date.replace('-', ''))
        request.set("endDate", end_date.replace('-', ''))
        request.set("periodicitySelection", frequency)
        
        # Send request
        self.session.sendRequest(request)
        
        # Process response
        data = []
        while True:
            event = self.session.nextEvent(100)
            if event.eventType() == blpapi.Event.RESPONSE:
                for msg in event:
                    securityData = msg.getElement("securityData")
                    security = securityData.getElement("security").getValue()
                    
                    fieldDataArray = securityData.getElement("fieldData")
                    for i in range(fieldDataArray.numValues()):
                        fieldData = fieldDataArray.getValue(i)
                        date = fieldData.getElement("date").getValue()
                        
                        row = {'Security': security, 'Date': date}
                        
                        for field in fields:
                            if fieldData.hasElement(field):
                                row[field] = fieldData.getElement(field).getValue()
                            else:
                                row[field] = None
                        
                        data.append(row)
                break
        
        df = pd.DataFrame(data)
        if not df.empty:
            df['Date'] = pd.to_datetime(df['Date'])
            df = df.sort_values(['Security', 'Date'])
            
            # Add metadata for multi-source analysis
            df['DataSource'] = 'Bloomberg'
            df['PullTimestamp'] = self.current_date
            
            # Ensure Date is the first column for easier time-series work
            cols = ['Date', 'Security', 'DataSource'] + [col for col in df.columns if col not in ['Date', 'Security', 'DataSource', 'PullTimestamp']] + ['PullTimestamp']
            df = df[cols]
        
        return df
    
    def get_current_intraday_data(self, security: str, hours_back: int = 1) -> pd.DataFrame:
        """
        Get intraday data from current time going back specified hours
        This pulls data only when the script is run (real-time)
        
        Args:
            security: Security identifier
            hours_back: Number of hours to go back from current time
        
        Returns:
            DataFrame with intraday data
        """
        # Set end time to current time
        end_time = self.current_date
        # Set start time to hours back from current time
        start_time = end_time - timedelta(hours=hours_back)
        
        start_datetime = start_time.strftime('%Y-%m-%d %H:%M:%S')
        end_datetime = end_time.strftime('%Y-%m-%d %H:%M:%S')
        
        logger.info(f"Pulling intraday data for {security} from {start_datetime} to {end_datetime}")
        
        import blpapi
        
        # Create request
        request = self.service.createRequest("IntradayTickRequest")
        request.set("security", security)
        request.set("startDateTime", start_datetime)
        request.set("endDateTime", end_datetime)
        request.set("includeConditionCodes", True)
        
        # Send request
        self.session.sendRequest(request)
        
        # Process response
        data = []
        while True:
            event = self.session.nextEvent(100)
            if event.eventType() == blpapi.Event.RESPONSE:
                for msg in event:
                    tickDataArray = msg.getElement("tickData").getElement("tickData")
                    for i in range(tickDataArray.numValues()):
                        tick = tickDataArray.getValue(i)
                        
                        row = {
                            'DateTime': tick.getElement("time").getValue(),
                            'Price': tick.getElement("value").getValue(),
                            'Size': tick.getElement("size").getValue() if tick.hasElement("size") else None,
                            'Type': tick.getElement("type").getValue() if tick.hasElement("type") else None
                        }
                        data.append(row)
                break
        
        df = pd.DataFrame(data)
        if not df.empty:
            df['DateTime'] = pd.to_datetime(df['DateTime'])
            df = df.sort_values('DateTime')
            
            # Add metadata
            df['DataSource'] = 'Bloomberg'
            df['PullTimestamp'] = self.current_date
        
        return df
    
    def search_securities(self, query: str, max_results: int = 20) -> pd.DataFrame:
        """
        Search for securities matching a query
        
        Args:
            query: Search query
            max_results: Maximum number of results
        
        Returns:
            DataFrame with search results
        """
        # This would implement security search functionality
        logger.warning("Security search method needs implementation")
        return pd.DataFrame(data)
    
    def create_chart_ready_dataset(self, securities: Union[str, List[str]], 
                                  fields: Union[str, List[str]],
                                  include_reference: bool = True) -> pd.DataFrame:
        """
        Create a comprehensive dataset optimized for charting and analysis
        Combines historical data with optional current reference data
        
        Args:
            securities: Security or list of securities
            fields: Field or list of fields  
            include_reference: Whether to include current reference data
            
        Returns:
            DataFrame optimized for time-series charting and multi-source analysis
        """
        # Get historical data (5-year default range)
        hist_data = self.get_historical_data(securities, fields)
        
        if include_reference and not hist_data.empty:
            # Get current reference data to ensure we have latest values
            ref_data = self.get_reference_data(securities, fields)
            
            if not ref_data.empty:
                # Convert reference data to same format as historical
                ref_formatted = []
                for _, row in ref_data.iterrows():
                    for field in fields if isinstance(fields, list) else [fields]:
                        if field in ref_data.columns:
                            ref_formatted.append({
                                'Date': self.current_date.date(),
                                'Security': row['Security'],
                                'DataSource': 'Bloomberg',
                                field: row[field],
                                'PullTimestamp': self.current_date,
                                'IsLatest': True
                            })
                
                if ref_formatted:
                    ref_df = pd.DataFrame(ref_formatted)
                    ref_df['Date'] = pd.to_datetime(ref_df['Date'])
                    
                    # Add IsLatest flag to historical data
                    hist_data['IsLatest'] = False
                    
                    # Combine datasets, removing any duplicate current day data
                    hist_data = hist_data[hist_data['Date'].dt.date != self.current_date.date()]
                    
                    # Combine
                    combined_data = pd.concat([hist_data, ref_df], ignore_index=True)
                    combined_data = combined_data.sort_values(['Security', 'Date'])
                    
                    return combined_data
        
        # Add IsLatest flag for consistency
        hist_data['IsLatest'] = hist_data['Date'].dt.date == hist_data['Date'].dt.date.max()
        
        return hist_data

class FredDataPuller:
    """
    Federal Reserve Economic Data (FRED) puller
    Provides access to 800K+ US and international economic time series
    """
    
    def __init__(self, api_key: Optional[str] = None):
        """
        Initialize FRED API connection
        
        Args:
            api_key: FRED API key. If None, will try to get from environment variable FRED_API_KEY
        """
        self.api_key = api_key or os.getenv('FRED_API_KEY')
        self.current_date = datetime.now()
        self.five_years_ago = self.current_date - timedelta(days=5*365)
        
        if not self.api_key:
            raise ValueError(
                "FRED API key required. Get one free at https://fred.stlouisfed.org/docs/api/api_key.html\n"
                "Set it as environment variable FRED_API_KEY or pass it to the constructor."
            )
        
        if FREDAPI_AVAILABLE:
            self.fred = Fred(api_key=self.api_key)
            logger.info("FRED API initialized with fredapi library")
        else:
            self.fred = None
            self.base_url = 'https://api.stlouisfed.org/fred'
            logger.info("FRED API initialized with requests (fredapi not available)")
    
    def get_series(self, series_id: str, 
                   start_date: Optional[str] = None, 
                   end_date: Optional[str] = None,
                   frequency: Optional[str] = None) -> pd.DataFrame:
        """
        Get economic data series from FRED
        
        Args:
            series_id: FRED series ID (e.g., 'GDP', 'UNRATE', 'FEDFUNDS')
            start_date: Start date (YYYY-MM-DD), defaults to 5 years ago
            end_date: End date (YYYY-MM-DD), defaults to current date
            frequency: Frequency ('d', 'w', 'm', 'q', 'a') - daily, weekly, monthly, quarterly, annual
            
        Returns:
            DataFrame with date index and series data
        """
        # Set default dates if not provided
        if start_date is None:
            start_date = self.five_years_ago.strftime('%Y-%m-%d')
        if end_date is None:
            end_date = self.current_date.strftime('%Y-%m-%d')
        
        logger.info(f"Pulling FRED series {series_id} from {start_date} to {end_date}")
        
        try:
            if FREDAPI_AVAILABLE and self.fred:
                # Use fredapi library
                data = self.fred.get_series(
                    series_id, 
                    observation_start=start_date,
                    observation_end=end_date,
                    frequency=frequency
                )
                
                # Convert to DataFrame
                df = pd.DataFrame({
                    'date': data.index,
                    series_id: data.values
                })
                df['date'] = pd.to_datetime(df['date'])
                df = df.reset_index(drop=True)
                
            else:
                # Use direct API calls
                df = self._get_series_direct(series_id, start_date, end_date, frequency)
            
            # Add metadata
            df['DataSource'] = 'FRED'
            df['SeriesID'] = series_id
            df['PullTimestamp'] = self.current_date
            
            # Reorder columns
            cols = ['date', 'SeriesID', 'DataSource'] + [col for col in df.columns if col not in ['date', 'SeriesID', 'DataSource', 'PullTimestamp']] + ['PullTimestamp']
            df = df[cols]
            
            return df
            
        except Exception as e:
            logger.error(f"Error fetching FRED series {series_id}: {e}")
            return pd.DataFrame()
    
    def _get_series_direct(self, series_id: str, start_date: str, end_date: str, frequency: Optional[str]) -> pd.DataFrame:
        """Get series data using direct API calls"""
        url = f"{self.base_url}/series/observations"
        params = {
            'series_id': series_id,
            'api_key': self.api_key,
            'file_type': 'json',
            'observation_start': start_date,
            'observation_end': end_date
        }
        
        if frequency:
            params['frequency'] = frequency
        
        response = requests.get(url, params=params)
        response.raise_for_status()
        
        data = response.json()
        observations = data.get('observations', [])
        
        if not observations:
            return pd.DataFrame()
        
        # Convert to DataFrame
        df_data = []
        for obs in observations:
            try:
                value = float(obs['value']) if obs['value'] != '.' else np.nan
                df_data.append({
                    'date': pd.to_datetime(obs['date']),
                    series_id: value
                })
            except (ValueError, TypeError):
                continue
        
        return pd.DataFrame(df_data)
    
    def get_multiple_series(self, series_dict: Dict[str, str],
                           start_date: Optional[str] = None,
                           end_date: Optional[str] = None) -> pd.DataFrame:
        """
        Get multiple economic series and combine them
        
        Args:
            series_dict: Dictionary mapping friendly names to FRED series IDs
                        e.g., {'GDP': 'GDP', 'Unemployment': 'UNRATE', 'Fed Funds': 'FEDFUNDS'}
            start_date: Start date
            end_date: End date
            
        Returns:
            DataFrame with all series combined by date
        """
        all_data = []
        
        for name, series_id in series_dict.items():
            series_data = self.get_series(series_id, start_date, end_date)
            if not series_data.empty:
                # Rename the series column to the friendly name
                series_data = series_data.rename(columns={series_id: name})
                all_data.append(series_data)
        
        if not all_data:
            return pd.DataFrame()
        
        # Merge all series on date
        combined_data = all_data[0]
        for data in all_data[1:]:
            combined_data = pd.merge(combined_data, data[['date', list(data.columns)[3]]], 
                                   on='date', how='outer')
        
        combined_data = combined_data.sort_values('date').reset_index(drop=True)
        return combined_data
    
    def search_series(self, search_text: str, limit: int = 10) -> pd.DataFrame:
        """
        Search for FRED series by text
        
        Args:
            search_text: Text to search for
            limit: Maximum number of results
            
        Returns:
            DataFrame with search results
        """
        try:
            if FREDAPI_AVAILABLE and self.fred:
                results = self.fred.search(search_text, limit=limit)
                return results
            else:
                # Direct API search
                url = f"{self.base_url}/series/search"
                params = {
                    'search_text': search_text,
                    'api_key': self.api_key,
                    'file_type': 'json',
                    'limit': limit
                }
                
                response = requests.get(url, params=params)
                response.raise_for_status()
                
                data = response.json()
                series_list = data.get('seriess', [])
                
                if series_list:
                    return pd.DataFrame(series_list)
                else:
                    return pd.DataFrame()
                    
        except Exception as e:
            logger.error(f"Error searching FRED series: {e}")
            return pd.DataFrame()
    
    def get_key_economic_indicators(self, start_date: Optional[str] = None,
                                   end_date: Optional[str] = None) -> pd.DataFrame:
        """
        Get a comprehensive set of key economic indicators
        
        Args:
            start_date: Start date (YYYY-MM-DD), defaults to 5 years ago
            end_date: End date (YYYY-MM-DD), defaults to current date
        
        Returns:
            DataFrame with major economic indicators
        """
        # Set default dates if not provided
        if start_date is None:
            start_date = self.five_years_ago.strftime('%Y-%m-%d')
        if end_date is None:
            end_date = self.current_date.strftime('%Y-%m-%d')
            
        logger.info(f"Pulling key economic indicators from {start_date} to {end_date}")
        
        key_indicators = {
            'GDP': 'GDP',                           # Gross Domestic Product
            'Unemployment_Rate': 'UNRATE',          # Unemployment Rate
            'Inflation_CPI': 'CPIAUCSL',           # Consumer Price Index
            'Fed_Funds_Rate': 'FEDFUNDS',          # Federal Funds Rate
            'S&P_500': 'SP500',                    # S&P 500
            'Industrial_Production': 'INDPRO',      # Industrial Production Index
            'Consumer_Confidence': 'UMCSENT',       # Consumer Sentiment
            'Housing_Starts': 'HOUST',             # Housing Starts
            'Retail_Sales': 'RSAFS',              # Retail Sales
            'Payroll_Employment': 'PAYEMS'          # Non-farm Payroll Employment
        }
        
        return self.get_multiple_series(key_indicators, start_date, end_date)
    
    def get_ny_fed_specific_data(self, start_date: Optional[str] = None,
                                end_date: Optional[str] = None) -> pd.DataFrame:
        """
        Get data series specifically from or related to NY Fed
        
        Args:
            start_date: Start date (YYYY-MM-DD), defaults to 5 years ago
            end_date: End date (YYYY-MM-DD), defaults to current date
        
        Returns:
            DataFrame with NY Fed specific indicators
        """
        # Set default dates if not provided
        if start_date is None:
            start_date = self.five_years_ago.strftime('%Y-%m-%d')
        if end_date is None:
            end_date = self.current_date.strftime('%Y-%m-%d')
            
        logger.info(f"Pulling NY Fed specific data from {start_date} to {end_date}")
        
        ny_fed_indicators = {
            'Empire_State_Index': 'GACDISA066MSFRBNY',    # Empire State Manufacturing Index
            'NY_Recession_Probability': 'RECPROUSM156N',   # Recession Probability
            'Term_Spread_10Y_3M': 'T10Y3M',               # 10-Year Treasury - 3-Month Treasury
            'Credit_Conditions': 'DRCCLACBS',             # Credit Conditions
            'Weekly_Economic_Index': 'NYFEDWEI',          # NY Fed Weekly Economic Index (if available)
        }
        
        return self.get_multiple_series(ny_fed_indicators, start_date, end_date)
    
    def create_fred_chart_dataset(self, series_dict: Dict[str, str],
                                 start_date: Optional[str] = None,
                                 end_date: Optional[str] = None) -> pd.DataFrame:
        """
        Create a chart-ready dataset from FRED data (similar to Bloomberg format)
        
        Args:
            series_dict: Dictionary of friendly names to FRED series IDs
            start_date: Start date
            end_date: End date
            
        Returns:
            DataFrame in long format ready for charting and merging with Bloomberg data
        """
        data = self.get_multiple_series(series_dict, start_date, end_date)
        
        if data.empty:
            return pd.DataFrame()
        
        # Convert to long format (similar to Bloomberg structure)
        value_cols = [col for col in data.columns if col not in ['date', 'DataSource', 'SeriesID', 'PullTimestamp']]
        
        long_data = pd.melt(data, 
                           id_vars=['date', 'DataSource', 'PullTimestamp'],
                           value_vars=value_cols,
                           var_name='Series',
                           value_name='Value')
        
        # Add IsLatest flag
        long_data['IsLatest'] = long_data['date'].dt.date == long_data['date'].dt.date.max()
        
        return long_data.sort_values(['Series', 'date']).reset_index(drop=True)

class IntegratedDataManager:
    """
    Manages and integrates data from Bloomberg and FRED sources
    Provides unified interface for multi-source financial and economic analysis
    """
    
    def __init__(self, fred_api_key: Optional[str] = None):
        """
        Initialize integrated data manager
        
        Args:
            fred_api_key: FRED API key (optional if set as environment variable)
        """
        self.bloomberg = None
        self.fred = None
        
        # Initialize Bloomberg if available
        try:
            self.bloomberg = BloombergDataPuller()
            logger.info("Bloomberg API initialized")
        except Exception as e:
            logger.warning(f"Bloomberg API not available: {e}")
        
        # Initialize FRED
        try:
            self.fred = FredDataPuller(fred_api_key)
            logger.info("FRED API initialized")
        except Exception as e:
            logger.warning(f"FRED API not available: {e}")
    
    def get_integrated_dataset(self, 
                              bloomberg_securities: Optional[List[str]] = None,
                              bloomberg_fields: Optional[List[str]] = None,
                              fred_series: Optional[Dict[str, str]] = None,
                              start_date: Optional[str] = None,
                              end_date: Optional[str] = None) -> pd.DataFrame:
        """
        Create integrated dataset combining Bloomberg and FRED data
        
        Args:
            bloomberg_securities: List of Bloomberg securities
            bloomberg_fields: List of Bloomberg fields
            fred_series: Dictionary of FRED series (name -> series_id)
            start_date: Start date (YYYY-MM-DD), defaults to 5 years ago
            end_date: End date (YYYY-MM-DD), defaults to current date
            
        Returns:
            Combined DataFrame with both Bloomberg and FRED data
        """
        # Set default dates if not provided
        if start_date is None:
            start_date = (datetime.now() - timedelta(days=5*365)).strftime('%Y-%m-%d')
        if end_date is None:
            end_date = datetime.now().strftime('%Y-%m-%d')
            
        logger.info(f"Creating integrated dataset from {start_date} to {end_date}")
        
        combined_data = []
        
        # Get Bloomberg data
        if self.bloomberg and bloomberg_securities and bloomberg_fields:
            try:
                bb_data = self.bloomberg.get_historical_data(
                    bloomberg_securities, bloomberg_fields, start_date, end_date
                )
                if not bb_data.empty:
                    # Also get current reference data if end_date is recent
                    end_date_dt = datetime.strptime(end_date, '%Y-%m-%d')
                    if (datetime.now() - end_date_dt).days <= 7:  # Within last week
                        current_data = self.bloomberg.get_reference_data(bloomberg_securities, bloomberg_fields)
                        if not current_data.empty:
                            # Convert current data to historical format
                            current_historical = []
                            for _, row in current_data.iterrows():
                                for field in bloomberg_fields:
                                    if field in current_data.columns:
                                        current_historical.append({
                                            'Date': datetime.now().date(),
                                            'Security': row['Security'],
                                            field: row[field]
                                        })
                            
                            if current_historical:
                                current_df = pd.DataFrame(current_historical)
                                current_df['Date'] = pd.to_datetime(current_df['Date'])
                                # Remove any duplicate current day data
                                bb_data = bb_data[bb_data['Date'].dt.date != datetime.now().date()]
                                bb_data = pd.concat([bb_data, current_df], ignore_index=True)
                    
                    # Standardize Bloomberg data format
                    bb_standardized = self._standardize_bloomberg_data(bb_data)
                    combined_data.append(bb_standardized)
                    logger.info(f"Bloomberg data: {len(bb_standardized)} records")
            except Exception as e:
                logger.error(f"Error getting Bloomberg data: {e}")
        
        # Get FRED data with specified date range
        if self.fred and fred_series:
            try:
                fred_data = self.fred.create_fred_chart_dataset(
                    fred_series, start_date, end_date
                )
                if not fred_data.empty:
                    combined_data.append(fred_data)
                    logger.info(f"FRED data: {len(fred_data)} records")
            except Exception as e:
                logger.error(f"Error getting FRED data: {e}")
        
        # Combine datasets
        if combined_data:
            integrated_df = pd.concat(combined_data, ignore_index=True)
            integrated_df = integrated_df.sort_values(['DataSource', 'date']).reset_index(drop=True)
            
            logger.info(f"Integrated dataset: {len(integrated_df)} total records")
            logger.info(f"Date range: {integrated_df['date'].min()} to {integrated_df['date'].max()}")
            logger.info(f"Data sources: {integrated_df['DataSource'].unique()}")
            
            return integrated_df
        else:
            logger.warning("No data available from any source")
            return pd.DataFrame()
    
    def _standardize_bloomberg_data(self, bb_data: pd.DataFrame) -> pd.DataFrame:
        """Convert Bloomberg data to standard format for integration"""
        # Rename columns to match FRED format
        bb_data = bb_data.rename(columns={
            'Security': 'Series',
            'PX_LAST': 'Value',
            'VOLUME': 'Volume',
            'Date': 'date'
        })
        
        # Convert to long format if needed
        if 'Value' in bb_data.columns:
            # Already in correct format
            result = bb_data[['date', 'Series', 'Value', 'DataSource', 'IsLatest', 'PullTimestamp']].copy()
        else:
            # Convert multiple value columns to long format
            value_cols = [col for col in bb_data.columns 
                         if col not in ['date', 'Series', 'DataSource', 'IsLatest', 'PullTimestamp']]
            
            result = pd.melt(bb_data,
                           id_vars=['date', 'Series', 'DataSource', 'IsLatest', 'PullTimestamp'],
                           value_vars=value_cols,
                           var_name='Field',
                           value_name='Value')
            
            # Combine Series and Field for unique identifier
            result['Series'] = result['Series'] + '_' + result['Field']
            result = result.drop('Field', axis=1)
        
        return result
    

    
    def get_market_economic_dashboard_data(self, start_date: Optional[str] = None,
                                          end_date: Optional[str] = None) -> pd.DataFrame:
        """
        Get comprehensive data for market-economic dashboard and export final dataset
        
        Args:
            start_date: Start date (YYYY-MM-DD), defaults to 5 years ago
            end_date: End date (YYYY-MM-DD), defaults to current date
        
        Returns:
            Integrated DataFrame ready for charting and analysis
        """
        # Set default dates if not provided
        if start_date is None:
            start_date = (datetime.now() - timedelta(days=5*365)).strftime('%Y-%m-%d')
        if end_date is None:
            end_date = datetime.now().strftime('%Y-%m-%d')
            
        logger.info(f"Creating market-economic dashboard data from {start_date} to {end_date}")
        
        # Define key series
        bloomberg_securities = ['SPY US Equity', 'QQQ US Equity', 'TLT US Equity', 'GLD US Equity']
        bloomberg_fields = ['PX_LAST']
        
        fred_series = {
            'Fed_Funds_Rate': 'FEDFUNDS',
            'Unemployment_Rate': 'UNRATE',
            'Inflation_CPI': 'CPIAUCSL', 
            'GDP_Growth': 'GDP',
            'VIX': 'VIXCLS'
        }
        
        # Get integrated data with specified time window
        integrated_data = self.get_integrated_dataset(
            bloomberg_securities=bloomberg_securities,
            bloomberg_fields=bloomberg_fields,
            fred_series=fred_series,
            start_date=start_date,
            end_date=end_date
        )
        
        # Export final merged dataset
        if not integrated_data.empty:
            current_date = datetime.now().strftime('%Y%m%d')
            filename = f'final_merged_dataset_{current_date}.csv'
            integrated_data.to_csv(filename, index=False)
            logger.info(f"Final merged dataset exported as: {filename}")
            logger.info(f"Dataset covers period: {start_date} to {end_date}")
        
        return integrated_data
    
    def close(self):
        """Close all connections"""
        if self.bloomberg:
            self.bloomberg.close()
        # FRED doesn't need explicit closing

class BloombergChartBuilder:
    """
    Chart builder for Bloomberg data with multiple visualization options
    Supports line charts, bar charts, single/multiple tickers, and various time periods
    """
    
    def __init__(self, data: pd.DataFrame):
        """
        Initialize chart builder with Bloomberg data
        
        Args:
            data: DataFrame from BloombergDataPuller (chart-ready format)
        """
        self.data = data.copy()
        self.fig = None
        self.axes = None
        
        # Ensure date column is datetime
        if 'date' in self.data.columns:
            self.data['date'] = pd.to_datetime(self.data['date'])
        elif 'Date' in self.data.columns:
            self.data['Date'] = pd.to_datetime(self.data['Date'])
            self.data = self.data.rename(columns={'Date': 'date'})
    
    def create_line_chart(self, securities: Union[str, List[str]], 
                         field: str = 'Price',
                         title: Optional[str] = None,
                         date_range: Optional[tuple] = None,
                         figsize: tuple = (12, 6),
                         save_path: Optional[str] = None) -> Figure:
        """
        Create line chart for one or multiple securities
        
        Args:
            securities: Single security or list of securities to plot
            field: Field to plot (e.g., 'Price', 'Volume')
            title: Chart title (auto-generated if None)
            date_range: Tuple of (start_date, end_date) as strings or datetime
            figsize: Figure size as (width, height)
            save_path: Path to save chart (optional)
            
        Returns:
            matplotlib Figure object
        """
        # Ensure securities is a list
        if isinstance(securities, str):
            securities = [securities]
        
        # Filter data
        chart_data = self._filter_data(securities, date_range)
        
        # Create figure
        fig, ax = plt.subplots(figsize=figsize)
        
        # Plot each security
        for security in securities:
            security_data = chart_data[chart_data['Security'] == security].sort_values('date')
            if not security_data.empty and field in security_data.columns:
                ax.plot(security_data['date'], security_data[field], 
                       label=security.replace(' US Equity', ''), linewidth=2)
        
        # Formatting
        ax.set_xlabel('Date')
        ax.set_ylabel(field)
        ax.set_title(title or f'{field} - {", ".join([s.replace(" US Equity", "") for s in securities])}')
        ax.legend()
        ax.grid(True, alpha=0.3)
        
        # Format x-axis dates
        ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m'))
        ax.xaxis.set_major_locator(mdates.MonthLocator(interval=3))
        plt.xticks(rotation=45)
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')
            logger.info(f"Chart saved to {save_path}")
        
        self.fig = fig
        self.axes = ax
        return fig
    
    def create_price_volume_chart(self, security: str,
                                 price_field: str = 'PX_LAST',
                                 volume_field: str = 'VOLUME',
                                 title: Optional[str] = None,
                                 date_range: Optional[tuple] = None,
                                 figsize: tuple = (12, 8),
                                 save_path: Optional[str] = None) -> Figure:
        """
        Create combined price and volume chart (price line + volume bars)
        
        Args:
            security: Single security to plot
            price_field: Price field name (e.g., 'PX_LAST', 'Price', 'Value')
            volume_field: Volume field name (e.g., 'VOLUME', 'Volume')
            title: Chart title
            date_range: Date range filter
            figsize: Figure size
            save_path: Path to save chart
            
        Returns:
            matplotlib Figure object with dual y-axis (price line + volume bars)
        """
        # Filter data for the security
        chart_data = self._filter_data_universal([security], price_field, date_range)
        
        if chart_data.empty:
            logger.warning(f"No data available for price-volume chart: {security}")
            return None
        
        chart_data = chart_data.sort_values('date')
        
        # Check if volume data is available
        has_volume = False
        volume_data = None
        
        if self.data_format == 'bloomberg':
            # For Bloomberg data, try to get volume from original data
            volume_data = self._filter_data_universal([security], volume_field, date_range)
            has_volume = not volume_data.empty and 'value' in volume_data.columns
        elif self.data_format == 'integrated':
            # For integrated data, check if volume series exists
            volume_series_name = f"{security}_{volume_field}" if volume_field != 'Value' else f"{security}_Volume"
            volume_data = self.data[self.data['Series'] == volume_series_name]
            if volume_data.empty:
                # Try alternative volume naming
                volume_data = self.data[self.data['Series'].str.contains('Volume', case=False) & 
                                      self.data['Series'].str.contains(security.split()[0], case=False)]
            has_volume = not volume_data.empty
        
        # Create figure with dual y-axis
        fig, ax1 = plt.subplots(figsize=figsize)
        
        # Plot price on primary y-axis
        color_price = 'tab:blue'
        ax1.set_xlabel('Date')
        ax1.set_ylabel(f'{price_field}', color=color_price)
        
        # Handle data source styling
        label = security.replace(' US Equity', '').replace('_', ' ')
        linestyle = '-'
        if 'DataSource' in chart_data.columns:
            source = chart_data['DataSource'].iloc[0]
            linestyle = '-' if source == 'Bloomberg' else '--'
            label += f" ({source})"
        
        line1 = ax1.plot(chart_data['date'], chart_data['value'], 
                        color=color_price, linewidth=2, linestyle=linestyle, label=f'{label} Price')
        ax1.tick_params(axis='y', labelcolor=color_price)
        ax1.grid(True, alpha=0.3)
        
        # Plot volume on secondary y-axis if available
        if has_volume and volume_data is not None:
            ax2 = ax1.twinx()
            color_volume = 'tab:orange'
            
            volume_data = volume_data.sort_values('date')
            
            # Ensure date alignment between price and volume
            merged_data = pd.merge(
                chart_data[['date', 'value']].rename(columns={'value': 'price'}),
                volume_data[['date', 'value']].rename(columns={'value': 'volume'}),
                on='date', how='inner'
            )
            
            if not merged_data.empty:
                ax2.bar(merged_data['date'], merged_data['volume'], 
                       color=color_volume, alpha=0.3, width=1, label=f'{label} Volume')
                ax2.set_ylabel('Volume', color=color_volume)
                ax2.tick_params(axis='y', labelcolor=color_volume)
                
                # Format volume y-axis
                ax2.yaxis.set_major_formatter(plt.FuncFormatter(lambda x, p: f'{x/1e6:.1f}M' if x >= 1e6 else f'{x/1e3:.0f}K'))
        else:
            logger.warning(f"Volume data not available for {security}")
        
        # Formatting
        ax1.set_title(title or f'{security.replace(" US Equity", "")} - Price and Volume')
        
        # Format x-axis dates
        ax1.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m'))
        ax1.xaxis.set_major_locator(mdates.MonthLocator(interval=2))
        plt.xticks(rotation=45)
        
        # Combine legends
        lines1, labels1 = ax1.get_legend_handles_labels()
        if has_volume and volume_data is not None:
            lines2, labels2 = ax2.get_legend_handles_labels()
            ax1.legend(lines1 + lines2, labels1 + labels2, loc='upper left')
        else:
            ax1.legend(loc='upper left')
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')
            logger.info(f"Price-volume chart saved to {save_path}")
        
        self.fig = fig
        self.axes = (ax1, ax2 if has_volume else None)
        return fig
    
    def create_volume_bar_chart(self, securities: Union[str, List[str]],
                               volume_field: str = 'VOLUME',
                               title: Optional[str] = None,
                               date_range: Optional[tuple] = None,
                               figsize: tuple = (12, 6),
                               save_path: Optional[str] = None) -> Figure:
        """
        Create standalone volume bar chart for one or multiple securities
        
        Args:
            securities: Single security or list of securities
            volume_field: Volume field name
            title: Chart title
            date_range: Date range filter
            figsize: Figure size
            save_path: Path to save chart
            
        Returns:
            matplotlib Figure object
        """
        if isinstance(securities, str):
            securities = [securities]
        
        fig, ax = plt.subplots(figsize=figsize)
        
        colors = plt.cm.Set1(np.linspace(0, 1, len(securities)))
        
        for i, security in enumerate(securities):
            # Get volume data
            if self.data_format == 'bloomberg':
                volume_data = self._filter_data_universal([security], volume_field, date_range)
            elif self.data_format == 'integrated':
                # Look for volume series
                volume_series = f"{security}_{volume_field}" if volume_field != 'Value' else f"{security}_Volume"
                volume_data = self.data[self.data['Series'] == volume_series]
                if volume_data.empty:
                    # Try alternative naming
                    volume_data = self.data[self.data['Series'].str.contains('Volume', case=False) & 
                                          self.data['Series'].str.contains(security.split()[0], case=False)]
                volume_data = volume_data.rename(columns={'Value': 'value'})
            else:
                volume_data = self._filter_data_universal([security], volume_field, date_range)
            
            if not volume_data.empty:
                volume_data = volume_data.sort_values('date')
                
                label = security.replace(' US Equity', '').replace('_', ' ')
                if 'DataSource' in volume_data.columns:
                    source = volume_data['DataSource'].iloc[0]
                    label += f" ({source})"
                
                # Create bars with slight offset for multiple securities
                width = 0.8 / len(securities) if len(securities) > 1 else 0.8
                offset = (i - len(securities)/2 + 0.5) * width if len(securities) > 1 else 0
                
                ax.bar(volume_data['date'] + pd.Timedelta(days=offset), 
                      volume_data['value'], 
                      width=width, 
                      color=colors[i], 
                      alpha=0.7, 
                      label=label)
        
        # Formatting
        ax.set_xlabel('Date')
        ax.set_ylabel('Volume')
        ax.set_title(title or f'Volume - {", ".join([s.replace(" US Equity", "") for s in securities])}')
        ax.legend()
        ax.grid(True, alpha=0.3, axis='y')
        
        # Format volume y-axis
        ax.yaxis.set_major_formatter(plt.FuncFormatter(lambda x, p: f'{x/1e6:.1f}M' if x >= 1e6 else f'{x/1e3:.0f}K'))
        
        # Format x-axis dates
        ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m'))
        ax.xaxis.set_major_locator(mdates.MonthLocator(interval=2))
        plt.xticks(rotation=45)
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')
            logger.info(f"Volume bar chart saved to {save_path}")
        
        self.fig = fig
        self.axes = ax
        return fig
    
    def create_bar_chart(self, securities: Union[str, List[str]], 
                        field: str = 'Price',
                        period: str = 'monthly',
                        aggregation: str = 'mean',
                        title: Optional[str] = None,
                        date_range: Optional[tuple] = None,
                        figsize: tuple = (12, 6),
                        save_path: Optional[str] = None) -> Figure:
        """
        Create bar chart showing aggregated data by time period
        
        Args:
            securities: Single security or list of securities
            field: Field to plot
            period: 'monthly', 'quarterly', or 'yearly'
            aggregation: 'mean', 'sum', 'last', 'first'
            title: Chart title
            date_range: Date range filter
            figsize: Figure size
            save_path: Path to save chart
            
        Returns:
            matplotlib Figure object
        """
        if isinstance(securities, str):
            securities = [securities]
        
        chart_data = self._filter_data(securities, date_range)
        
        # Create period grouping
        if period == 'monthly':
            chart_data['period'] = chart_data['date'].dt.to_period('M')
            period_label = 'Month'
        elif period == 'quarterly':
            chart_data['period'] = chart_data['date'].dt.to_period('Q')
            period_label = 'Quarter'
        elif period == 'yearly':
            chart_data['period'] = chart_data['date'].dt.to_period('Y')
            period_label = 'Year'
        
        # Aggregate data
        agg_data = chart_data.groupby(['Security', 'period'])[field].agg(aggregation).reset_index()
        
        # Create figure
        fig, ax = plt.subplots(figsize=figsize)
        
        # Plot bars
        if len(securities) == 1:
            # Single security - simple bar chart
            security_data = agg_data[agg_data['Security'] == securities[0]]
            bars = ax.bar(range(len(security_data)), security_data[field])
            ax.set_xticks(range(len(security_data)))
            ax.set_xticklabels([str(p) for p in security_data['period']], rotation=45)
        else:
            # Multiple securities - grouped bar chart
            periods = sorted(agg_data['period'].unique())
            x = np.arange(len(periods))
            width = 0.8 / len(securities)
            
            for i, security in enumerate(securities):
                security_data = agg_data[agg_data['Security'] == security]
                values = [security_data[security_data['period'] == p][field].iloc[0] 
                         if len(security_data[security_data['period'] == p]) > 0 else 0 
                         for p in periods]
                
                ax.bar(x + i * width, values, width, 
                      label=security.replace(' US Equity', ''))
            
            ax.set_xticks(x + width * (len(securities) - 1) / 2)
            ax.set_xticklabels([str(p) for p in periods], rotation=45)
            ax.legend()
        
        # Formatting
        ax.set_xlabel(period_label)
        ax.set_ylabel(f'{field} ({aggregation})')
        ax.set_title(title or f'{field} by {period_label} ({aggregation}) - {", ".join([s.replace(" US Equity", "") for s in securities])}')
        ax.grid(True, alpha=0.3, axis='y')
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')
            logger.info(f"Chart saved to {save_path}")
        
        self.fig = fig
        self.axes = ax
        return fig
    
    def create_comparison_chart(self, securities: List[str], 
                              field: str = 'Price',
                              normalize: bool = True,
                              title: Optional[str] = None,
                              date_range: Optional[tuple] = None,
                              figsize: tuple = (12, 6),
                              save_path: Optional[str] = None) -> Figure:
        """
        Create normalized comparison chart to show relative performance
        
        Args:
            securities: List of securities to compare
            field: Field to compare
            normalize: Whether to normalize to starting value (for relative performance)
            title: Chart title
            date_range: Date range filter
            figsize: Figure size
            save_path: Path to save chart
            
        Returns:
            matplotlib Figure object
        """
        chart_data = self._filter_data(securities, date_range)
        
        fig, ax = plt.subplots(figsize=figsize)
        
        for security in securities:
            security_data = chart_data[chart_data['Security'] == security].sort_values('date')
            if not security_data.empty and field in security_data.columns:
                values = security_data[field].values
                dates = security_data['date'].values
                
                if normalize and len(values) > 0:
                    # Normalize to first value (percentage change from start)
                    values = (values / values[0] - 1) * 100
                
                ax.plot(dates, values, label=security.replace(' US Equity', ''), linewidth=2)
        
        # Formatting
        ax.set_xlabel('Date')
        ax.set_ylabel(f'{field}' + (' (% Change from Start)' if normalize else ''))
        ax.set_title(title or f'{"Relative Performance" if normalize else "Comparison"} - {field}')
        ax.legend()
        ax.grid(True, alpha=0.3)
        
        if normalize:
            ax.axhline(y=0, color='black', linestyle='--', alpha=0.5)
        
        # Format x-axis dates
        ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m'))
        ax.xaxis.set_major_locator(mdates.MonthLocator(interval=3))
        plt.xticks(rotation=45)
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')
            logger.info(f"Chart saved to {save_path}")
        
        self.fig = fig
        self.axes = ax
        return fig
    
    def create_dashboard(self, securities: List[str], 
                        save_path: Optional[str] = None,
                        figsize: tuple = (16, 12)) -> Figure:
        """
        Create a comprehensive dashboard with multiple chart types
        
        Args:
            securities: List of securities for dashboard
            save_path: Path to save dashboard
            figsize: Figure size
            
        Returns:
            matplotlib Figure object
        """
        fig, axes = plt.subplots(2, 2, figsize=figsize)
        fig.suptitle(f'Bloomberg Data Dashboard - {", ".join([s.replace(" US Equity", "") for s in securities])}', 
                    fontsize=16, fontweight='bold')
        
        # 1. Price comparison (top-left)
        ax1 = axes[0, 0]
        for security in securities:
            security_data = self.data[self.data['Security'] == security].sort_values('date')
            if not security_data.empty:
                ax1.plot(security_data['date'], security_data.get('Price', security_data.get('PX_LAST', [])), 
                        label=security.replace(' US Equity', ''), linewidth=2)
        ax1.set_title('Price History')
        ax1.set_ylabel('Price')
        ax1.legend()
        ax1.grid(True, alpha=0.3)
        
        # 2. Volume bars (top-right)
        ax2 = axes[0, 1]
        recent_data = self.data[self.data['date'] >= (self.data['date'].max() - timedelta(days=30))]
        volume_data = recent_data.groupby('Security')['Volume'].mean() if 'Volume' in recent_data.columns else \
                     recent_data.groupby('Security').get('VOLUME', pd.Series()).mean()
        
        if not volume_data.empty:
            bars = ax2.bar(range(len(volume_data)), volume_data.values)
            ax2.set_xticks(range(len(volume_data)))
            ax2.set_xticklabels([s.replace(' US Equity', '') for s in volume_data.index], rotation=45)
            ax2.set_title('Average Volume (Last 30 Days)')
            ax2.set_ylabel('Volume')
        
        # 3. Relative performance (bottom-left)
        ax3 = axes[1, 0]
        for security in securities:
            security_data = self.data[self.data['Security'] == security].sort_values('date')
            if not security_data.empty:
                price_col = 'Price' if 'Price' in security_data.columns else 'PX_LAST'
                if price_col in security_data.columns:
                    prices = security_data[price_col].values
                    if len(prices) > 0:
                        rel_perf = (prices / prices[0] - 1) * 100
                        ax3.plot(security_data['date'], rel_perf, 
                               label=security.replace(' US Equity', ''), linewidth=2)
        
        ax3.set_title('Relative Performance (%)')
        ax3.set_ylabel('% Change from Start')
        ax3.legend()
        ax3.grid(True, alpha=0.3)
        ax3.axhline(y=0, color='black', linestyle='--', alpha=0.5)
        
        # 4. Recent price changes (bottom-right)
        ax4 = axes[1, 1]
        if not self.data.empty:
            latest_data = self.data[self.data['IsLatest'] == True] if 'IsLatest' in self.data.columns else \
                         self.data.groupby('Security').last()
            
            if len(latest_data) > 0:
                # Calculate recent changes (you can modify this logic)
                price_col = 'Price' if 'Price' in latest_data.columns else 'PX_LAST'
                if price_col in latest_data.columns:
                    # Simple visualization of latest prices
                    bars = ax4.bar(range(len(latest_data)), latest_data[price_col])
                    ax4.set_xticks(range(len(latest_data)))
                    ax4.set_xticklabels([s.replace(' US Equity', '') for s in latest_data['Security']], rotation=45)
                    ax4.set_title('Latest Prices')
                    ax4.set_ylabel('Price')
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')
            logger.info(f"Dashboard saved to {save_path}")
        
        self.fig = fig
        self.axes = axes
        return fig
    
    def _filter_data(self, securities: List[str], date_range: Optional[tuple] = None) -> pd.DataFrame:
        """Helper method to filter data by securities and date range"""
        # Filter by securities
        filtered_data = self.data[self.data['Security'].isin(securities)].copy()
        
        # Filter by date range if provided
        if date_range:
            start_date, end_date = date_range
            if isinstance(start_date, str):
                start_date = pd.to_datetime(start_date)
            if isinstance(end_date, str):
                end_date = pd.to_datetime(end_date)
            
            filtered_data = filtered_data[
                (filtered_data['date'] >= start_date) & 
                (filtered_data['date'] <= end_date)
            ]
        
        return filtered_data

    def create_bar_chart(self, securities: Union[str, List[str]], 
                        field: str = 'Value',
                        period: str = 'monthly',
                        aggregation: str = 'mean',
                        title: Optional[str] = None,
                        date_range: Optional[tuple] = None,
                        figsize: tuple = (12, 6),
                        save_path: Optional[str] = None) -> Figure:
        """
        Create bar chart showing aggregated data by time period
        Enhanced to handle both Bloomberg and FRED data
        """
        if isinstance(securities, str):
            securities = [securities]
        
        chart_data = self._filter_data_universal(securities, field, date_range)
        
        if chart_data.empty:
            logger.warning("No data available for bar chart")
            return None
        
        # Create period grouping
        if period == 'monthly':
            chart_data['period'] = chart_data['date'].dt.to_period('M')
            period_label = 'Month'
        elif period == 'quarterly':
            chart_data['period'] = chart_data['date'].dt.to_period('Q')
            period_label = 'Quarter'
        elif period == 'yearly':
            chart_data['period'] = chart_data['date'].dt.to_period('Y')
            period_label = 'Year'
        
        # Aggregate data
        agg_data = chart_data.groupby(['identifier', 'period'])['value'].agg(aggregation).reset_index()
        
        # Create figure
        fig, ax = plt.subplots(figsize=figsize)
        
        # Plot bars
        if len(securities) == 1:
            # Single security - simple bar chart
            security_data = agg_data[agg_data['identifier'] == securities[0]]
            bars = ax.bar(range(len(security_data)), security_data['value'])
            ax.set_xticks(range(len(security_data)))
            ax.set_xticklabels([str(p) for p in security_data['period']], rotation=45)
        else:
            # Multiple securities - grouped bar chart
            periods = sorted(agg_data['period'].unique())
            x = np.arange(len(periods))
            width = 0.8 / len(securities)
            
            for i, security in enumerate(securities):
                security_data = agg_data[agg_data['identifier'] == security]
                values = [security_data[security_data['period'] == p]['value'].iloc[0] 
                         if len(security_data[security_data['period'] == p]) > 0 else 0 
                         for p in periods]
                
                ax.bar(x + i * width, values, width, 
                      label=security.replace(' US Equity', '').replace('_', ' '))
            
            ax.set_xticks(x + width * (len(securities) - 1) / 2)
            ax.set_xticklabels([str(p) for p in periods], rotation=45)
            ax.legend()
        
        # Formatting
        ax.set_xlabel(period_label)
        ax.set_ylabel(f'{field} ({aggregation})')
        ax.set_title(title or f'{field} by {period_label} ({aggregation}) - {", ".join([s.replace(" US Equity", "").replace("_", " ") for s in securities])}')
        ax.grid(True, alpha=0.3, axis='y')
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')
            logger.info(f"Bar chart saved to {save_path}")
        
        self.fig = fig
        self.axes = ax
        return fig
    
    def create_comparison_chart(self, securities: List[str], 
                              field: str = 'Value',
                              normalize: bool = True,
                              title: Optional[str] = None,
                              date_range: Optional[tuple] = None,
                              figsize: tuple = (12, 6),
                              save_path: Optional[str] = None) -> Figure:
        """
        Create normalized comparison chart to show relative performance
        Enhanced to handle both Bloomberg and FRED data
        """
        chart_data = self._filter_data_universal(securities, field, date_range)
        
        if chart_data.empty:
            logger.warning("No data available for comparison chart")
            return None
        
        fig, ax = plt.subplots(figsize=figsize)
        
        colors = plt.cm.Set1(np.linspace(0, 1, len(securities)))
        
        for i, security in enumerate(securities):
            security_data = chart_data[chart_data['identifier'] == security].sort_values('date')
            if not security_data.empty:
                values = security_data['value'].values
                dates = security_data['date'].values
                
                if normalize and len(values) > 0:
                    # Normalize to first value (percentage change from start)
                    values = (values / values[0] - 1) * 100
                
                # Handle data source styling
                label = security.replace(' US Equity', '').replace('_', ' ')
                linestyle = '-'
                if 'DataSource' in security_data.columns:
                    source = security_data['DataSource'].iloc[0]
                    linestyle = '-' if source == 'Bloomberg' else '--'
                    label += f" ({source})"
                
                ax.plot(dates, values, label=label, linewidth=2, color=colors[i], linestyle=linestyle)
        
        # Formatting
        ax.set_xlabel('Date')
        ax.set_ylabel(f'{field}' + (' (% Change from Start)' if normalize else ''))
        ax.set_title(title or f'{"Relative Performance" if normalize else "Comparison"} - {field}')
        ax.legend()
        ax.grid(True, alpha=0.3)
        
        if normalize:
            ax.axhline(y=0, color='black', linestyle='--', alpha=0.5)
        
        # Format x-axis dates
        ax.xaxis.set_major_formatter(mdates.DateFormatter('%Y-%m'))
        ax.xaxis.set_major_locator(mdates.MonthLocator(interval=3))
        plt.xticks(rotation=45)
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')
            logger.info(f"Comparison chart saved to {save_path}")
        
        self.fig = fig
        self.axes = ax
        return fig

    def create_dashboard(self, securities: List[str], 
                        save_path: Optional[str] = None,
                        figsize: tuple = (16, 12)) -> Figure:
        """
        Create a comprehensive dashboard with multiple chart types
        Enhanced to handle both Bloomberg and FRED data
        """
        fig, axes = plt.subplots(2, 2, figsize=figsize)
        fig.suptitle(f'Financial Data Dashboard - {", ".join([s.replace(" US Equity", "").replace("_", " ") for s in securities])}', 
                    fontsize=16, fontweight='bold')
        
        try:
            # 1. Price/Value comparison (top-left)
            ax1 = axes[0, 0]
            for i, security in enumerate(securities):
                security_data = self._filter_data_universal([security], 'Value')
                if not security_data.empty:
                    security_data = security_data.sort_values('date')
                    label = security.replace(' US Equity', '').replace('_', ' ')
                    if 'DataSource' in security_data.columns:
                        source = security_data['DataSource'].iloc[0]
                        linestyle = '-' if source == 'Bloomberg' else '--'
                        label += f" ({source})"
                    else:
                        linestyle = '-'
                    ax1.plot(security_data['date'], security_data['value'], 
                            label=label, linewidth=2, linestyle=linestyle)
            ax1.set_title('Price/Value History')
            ax1.set_ylabel('Value')
            ax1.legend()
            ax1.grid(True, alpha=0.3)
            
            # 2. Recent performance (top-right)
            ax2 = axes[0, 1]
            recent_data = self.data[self.data['date'] >= (self.data['date'].max() - timedelta(days=30))]
            if not recent_data.empty:
                for i, security in enumerate(securities[:3]):  # Limit to 3 for readability
                    sec_data = self._filter_data_universal([security], 'Value')
                    if not sec_data.empty:
                        sec_data = sec_data[sec_data['date'] >= (sec_data['date'].max() - timedelta(days=30))]
                        sec_data = sec_data.sort_values('date')
                        if len(sec_data) > 1:
                            # Calculate percentage change from 30 days ago
                            pct_change = (sec_data['value'].iloc[-1] / sec_data['value'].iloc[0] - 1) * 100
                            ax2.bar(i, pct_change, label=security.replace(' US Equity', '').replace('_', ' '))
                
                ax2.set_title('30-Day Performance (%)')
                ax2.set_ylabel('% Change')
                ax2.legend()
                ax2.grid(True, alpha=0.3, axis='y')
                ax2.axhline(y=0, color='black', linestyle='-', alpha=0.5)
            
            # 3. Normalized comparison (bottom-left)
            ax3 = axes[1, 0]
            for i, security in enumerate(securities):
                security_data = self._filter_data_universal([security], 'Value')
                if not security_data.empty:
                    security_data = security_data.sort_values('date')
                    values = security_data['value'].values
                    if len(values) > 0:
                        normalized_values = (values / values[0] - 1) * 100
                        label = security.replace(' US Equity', '').replace('_', ' ')
                        if 'DataSource' in security_data.columns:
                            source = security_data['DataSource'].iloc[0]
                            linestyle = '-' if source == 'Bloomberg' else '--'
                            label += f" ({source})"
                        else:
                            linestyle = '-'
                        ax3.plot(security_data['date'], normalized_values, 
                               label=label, linewidth=2, linestyle=linestyle)
            
            ax3.set_title('Relative Performance (%)')
            ax3.set_ylabel('% Change from Start')
            ax3.legend()
            ax3.grid(True, alpha=0.3)
            ax3.axhline(y=0, color='black', linestyle='--', alpha=0.5)
            
            # 4. Data summary (bottom-right)
            ax4 = axes[1, 1]
            if not self.data.empty:
                # Show data availability by source
                if 'DataSource' in self.data.columns:
                    source_counts = self.data['DataSource'].value_counts()
                    ax4.pie(source_counts.values, labels=source_counts.index, autopct='%1.1f%%')
                    ax4.set_title('Data Sources')
                else:
                    ax4.text(0.5, 0.5, 'Data Summary\nAvailable', 
                            horizontalalignment='center', verticalalignment='center', 
                            transform=ax4.transAxes, fontsize=14)
                    ax4.set_title('Data Available')
            
        except Exception as e:
            logger.error(f"Error creating dashboard: {e}")
        
        plt.tight_layout()
        
        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')
            logger.info(f"Dashboard saved to {save_path}")
        
        self.fig = fig
        self.axes = axes
        return fig
    
    def show(self):
        """Display the current chart"""
        plt.show()
    
    def close(self):
        """Close all figures"""
        plt.close('all')
    
    def show(self):
        """Display the current chart"""
        plt.show()
    
    def close(self):
        """Close all figures"""
        plt.close('all')
    
    def close(self):
        """Close Bloomberg session"""
        if self.session:
            self.session.stop()
            logger.info("Bloomberg session closed")

def main():
    """Example usage with integrated Bloomberg and FRED data, with customizable time windows"""
    
    try:
        print("=== Integrated Bloomberg & FRED Data Pipeline ===")
        
        # *** CUSTOMIZABLE TIME WINDOW ***
        # Option 1: Use default 5-year window
        start_date = None  # Will default to 5 years ago
        end_date = None    # Will default to current date
        
        # Option 2: Specify custom date range (uncomment to use)
        # start_date = '2020-01-01'  # Custom start date
        # end_date = '2024-12-31'    # Custom end date
        
        # Option 3: Recent data only (uncomment to use)
        # start_date = (datetime.now() - timedelta(days=365)).strftime('%Y-%m-%d')  # Last 1 year
        # end_date = datetime.now().strftime('%Y-%m-%d')
        
        # Option 4: Specific analysis period (uncomment to use)
        # start_date = '2019-01-01'  # Pre-COVID to present
        # end_date = datetime.now().strftime('%Y-%m-%d')
        
        print(f"Time window: {start_date or 'Default (5 years ago)'} to {end_date or 'Current date'}")
        
        # Initialize integrated data manager
        # Note: Set FRED_API_KEY environment variable or pass it directly
        manager = IntegratedDataManager()
        
        # Get comprehensive market and economic dataset with specified time window
        print("1. Pulling integrated dataset...")
        integrated_data = manager.get_market_economic_dashboard_data(
            start_date=start_date,
            end_date=end_date
        )
        
        if integrated_data.empty:
            print("No data available. Check API connections and keys.")
            return
        
        print(f"Integrated data: {len(integrated_data)} records")
        print(f"Actual date range: {integrated_data['date'].min().strftime('%Y-%m-%d')} to {integrated_data['date'].max().strftime('%Y-%m-%d')}")
        print(f"Data sources: {integrated_data['DataSource'].unique()}")
        print(f"Series available: {integrated_data['Series'].unique()}")
        print()
        
        # Initialize chart builder with integrated data
        print("2. Creating integrated charts...")
        chart_builder = BloombergChartBuilder(integrated_data)
        
        # Chart 1: Market indices with economic context
        print("   - Market vs Economic Indicators")
        chart_builder.create_line_chart(
            securities=['SPY US Equity', 'Fed_Funds_Rate', 'Unemployment_Rate'],
            title='Market Performance vs Economic Indicators',
            save_path='market_vs_economic.png',
            show_data_sources=True
        )
        
        # Chart 2: Multiple asset classes
        print("   - Multi-Asset Class Performance")
        chart_builder.create_line_chart(
            securities=['SPY US Equity', 'TLT US Equity', 'GLD US Equity'],
            title='Multi-Asset Class Performance (Stocks, Bonds, Gold)',
            save_path='multi_asset_performance.png',
            show_data_sources=True
        )
        
        # Chart 3: Economic indicators only
        print("   - Economic Indicators Dashboard")
        chart_builder.create_line_chart(
            securities=['Fed_Funds_Rate', 'Unemployment_Rate', 'Inflation_CPI'],
            title='Key Economic Indicators',
            save_path='economic_indicators.png',
            show_data_sources=True
        )
        
        # Chart 4: Market risk indicators
        print("   - Market Risk Indicators")
        chart_builder.create_line_chart(
            securities=['VIX', 'SPY US Equity'],
            title='Market Volatility vs S&P 500',
            save_path='market_risk.png',
            show_data_sources=True
        )
        
        # Chart 5: Comprehensive economic dashboard
        print("   - Comprehensive Economic Dashboard")
        chart_builder.create_economic_dashboard(
            save_path='comprehensive_economic_dashboard.png'
        )
        
        # Chart 6: Recent data focus (last 6 months within the specified window)
        print("   - Recent Performance (6 months)")
        recent_start = max(
            integrated_data['date'].max() - timedelta(days=180),
            integrated_data['date'].min()
        )
        chart_builder.create_line_chart(
            securities=['SPY US Equity', 'Fed_Funds_Rate', 'VIX'],
            title='Recent Market & Economic Performance (6 Months)',
            date_range=(recent_start, integrated_data['date'].max()),
            save_path='recent_6month_performance.png',
            show_data_sources=True
        )
        
        print("\n=== Chart Creation Complete ===")
        print("Charts created:")
        print("- market_vs_economic.png (Market vs Economic indicators)")
        print("- multi_asset_performance.png (Stocks, Bonds, Gold)")
        print("- economic_indicators.png (Fed rates, unemployment, inflation)")
        print("- market_risk.png (VIX vs S&P 500)")
        print("- comprehensive_economic_dashboard.png (Full dashboard)")
        print("- recent_6month_performance.png (Recent trends)")
        
        # Display final dataset info
        current_date_str = datetime.now().strftime('%Y%m%d')
        print(f"\n=== Final Dataset Export ===")
        print(f"Final merged dataset exported as: final_merged_dataset_{current_date_str}.csv")
        print(f"Dataset contains {len(integrated_data)} records with the following columns:")
        print(f"- {list(integrated_data.columns)}")
        
        # Sample of the final dataset
        print(f"\nSample of final dataset:")
        print(integrated_data.head(10))
        
        # Data summary by source
        print(f"\nData summary by source:")
        source_summary = integrated_data.groupby('DataSource').agg({
            'Series': 'nunique',
            'date': ['min', 'max'],
            'Value': 'count'
        })
        print(source_summary)
        
        # Clean up
        chart_builder.close()
        manager.close()
        
        print(f"\n=== Pipeline Complete ===")
        print("All charts created and final dataset exported successfully!")
        print(f"Time window used: {integrated_data['date'].min().strftime('%Y-%m-%d')} to {integrated_data['date'].max().strftime('%Y-%m-%d')}")
        
    except Exception as e:
        logger.error(f"Error in main pipeline: {e}")
        print("Make sure Bloomberg Terminal is running and FRED API key is set")
        print("Set FRED API key: export FRED_API_KEY='your_key' or set as environment variable")

if __name__ == "__main__":
    main()



library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(ggExtra)

sod_2018 <- read.csv("C:/Users/maxxj/OneDrive/Desktop/Projects/trade-elasticities/Elasticities_Soderbery2018.csv")
sod_2018 <- sod_2018 %>%
  mutate(hs4 = str_pad(hs4, 4, side = "left", pad = "0"),
         sod_e_d = -sigma,
         sod_e_s = 1/omega)


ccodes <- read.csv("C:/Users/maxxj/OneDrive/Desktop/Projects/trade-elasticities/BACI_HS92_V202601/country_codes_V202601.csv") %>%
  dplyr::select(country_code, country_iso3) %>%
  mutate(country_code = as.character(country_code))

df_v1 <- readRDS("C:/Users/maxxj/OneDrive/Desktop/Projects/trade-elasticities/trade_elast_baci_hs92_v202601_hs4/baci_hs92_v202601_elast_country_hs4_fixed_sigma.rds")

df_v1 <- df_v1 %>%
  left_join(ccodes, by = c("importer" = "country_code")) %>%
  rename(importer_iso3 = country_iso3) %>%
  left_join(ccodes, by = c("exporter" = "country_code")) %>%
  rename(exporter_iso3 = country_iso3) %>%
  left_join(sod_2018 %>%
              dplyr::select(iiso, eiso, hs4, sod_e_d, sod_e_s),
            by = c("importer_iso3" = "iiso",
                   "exporter_iso3" = "eiso",
                   "good" = "hs4")) %>%
  mutate(cook_e_d = -sigma,
         cook_e_s = 1 / gamma,
         e_d_diff = sod_e_d - cook_e_d,
         e_s_diff = sod_e_s - cook_e_s)



# Reshape from wide to long format
df_sigma_long <- df_v1 %>%
  filter(convergence == 0, cook_e_d >= -30, sod_e_d >= -30) %>%
  pivot_longer(cols = c(cook_e_d, sod_e_d),
               names_to = "measure",
               values_to = "value")

df_gamma_long <- df_v1 %>%
  filter(convergence == 0, cook_e_s <= 20, sod_e_s <= 20) %>%
  pivot_longer(cols = c(cook_e_s, sod_e_s),
               names_to = "measure",
               values_to = "value")


ggplot(df_sigma_long, aes(x = value, fill = measure)) +
  geom_histogram(alpha = 0.4, 
                 position = "identity",
                 binwidth = 0.1) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Import Demand Elasticity",
    x = "Value (-sigma)",
    y = "Count"
  ) +
  theme_minimal()


ggplot(df_gamma_long, aes(x = value, fill = measure)) +
  geom_histogram(alpha = 0.4, 
                 position = "identity",
                 binwidth = 0.1) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Export Supply Elasticity",
    x = "Value (1/gamma)",
    y = "Count"
  ) +
  theme_minimal()



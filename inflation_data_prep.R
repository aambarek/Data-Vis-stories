library(tidyverse)



# Morocco data

data_morocco_divisions <- readxl::read_excel("data-raw/ipc par division.xlsx")
data_morocco_grandes_divisions <- readxl::read_excel("data-raw/IPC par grandes divisions.xlsx")

data_morocco <- data_morocco_divisions %>% 
  left_join(select(data_morocco_grandes_divisions, c(1,4)))

names(data_morocco)[-1] <- paste(c(1:12,0), names(data_morocco)[-1], sep = "/")

morocco_inflation_rates <- data_morocco %>% 
  pivot_longer(cols = -1, names_to = "Division", values_to = "CPI") %>%
  separate(Division, into = c("coicop", "Division"), sep = "/") %>% 
  mutate(coicop = as.numeric(coicop)) %>% 
  rename(date = Mois) %>% 
  mutate(date = paste(date, "01", sep = "/"),
         date = lubridate::date(date),
         month = lubridate::month(date),
         year = lubridate::year(date)) %>% 
  group_by(coicop, month) %>% 
  mutate(CPI_previous = lag(CPI, order_by = year),
         inflation_rate = 100 * ((CPI/CPI_previous) - 1)) %>% 
  ungroup()



# OECD countries data

worldwide_data <- read_csv("data-raw/PRICES_CPI_19082022152352463.csv")


worldwide_data_clean <- worldwide_data %>% 
  filter(MEASURE == "GY", FREQUENCY == "M") %>% 
  filter(SUBJECT %in% c("CP010000", "CP030000", "CP070000", "CPALTT01")) %>% 
  select(-MEASURE, -FREQUENCY, -Measure, -Frequency) %>% 
  mutate(date = lubridate::date(paste(TIME, "01", sep = "-")))



# binding morocco and oecd countries data

world_morocco_data <- worldwide_data_clean %>% 
  mutate(coicop = str_extract(SUBJECT, "[1-9]") %>% as.numeric(),
         coicop = ifelse(SUBJECT == "CPALTT01", 0, coicop),
         month = lubridate::month(date),
         year = lubridate::year(date)) %>% 
  left_join(distinct(morocco_inflation_rates, coicop, Division)) %>% 
  select(LOCATION, Country, date, coicop, Division, month, year, inflation_rate = Value) %>% 
  bind_rows(select(morocco_inflation_rates, -starts_with("CPI")) %>% mutate(LOCATION = "MAR", Country = "Morocco", .before = 1))



Indice_global <- world_morocco_data %>% 
  filter(coicop == 0) %>% 
  select(LOCATION, date, Indice_Global = inflation_rate)


#considering only 15 countries with highest inflation rates in july 2022
TOP_15_inflation <- world_morocco_data %>% 
  filter(date == "2022-07-01", coicop %in% c(0,1,7)) %>%
  group_by(coicop) %>% 
  slice_max(order_by = inflation_rate, n = 15) %>% 
  ungroup() %>% 
  distinct(LOCATION, Country, date)




#Contribution to inflation


## CPI weights for morocco
ponderations_ipc <- read.table("data-raw/pondérations ipc.txt", sep = "\t")

morocco_ponderations_ipc_2014 <- ponderations_ipc %>% 
  mutate(coicop = str_extract(V1, "^..") %>% as.numeric,
         Ponderation = str_extract_all(V1, "[0-9]{1,3},[0-9]{3}"),
         Ponderation = map(Ponderation, ~ str_replace(.[2], ",", "") %>% as.numeric) %>% unlist) %>% 
  select(-V1) %>% 
  filter(!is.na(coicop)) %>% 
  mutate(Ponderation = 100 * Ponderation / sum(Ponderation))


morocco_contributions <- morocco_inflation_rates %>% 
  left_join(morocco_ponderations_ipc_2014) %>% 
  filter(year == 2022, month == 7) %>% 
  mutate(Ponderation = ifelse(is.na(Ponderation), 100, Ponderation)) %>% 
  mutate(CPI_0 = (sum(CPI * Ponderation, na.rm = T) / 2) / 100,
         CPI_0_previous = (sum(CPI_previous * Ponderation, na.rm = T) / 2) / 100) %>% 
  mutate(contribution_to_inflation = (CPI_previous / CPI_0_previous) * Ponderation * inflation_rate / 100) %>% 
  # arrange(desc(contribution_to_inflation)) %>% 
  filter(coicop %in% c(1,7,0)) %>% 
  pivot_wider(id_cols = c(date, month, year), names_from = coicop, values_from = contribution_to_inflation) %>% 
  mutate(Contribution_all_less_food_transport = `0` - `1` - `7`) %>% 
  pivot_longer(cols = -c(1:3), names_to = "coicop", values_to = "contribution_to_inflation") %>% 
  mutate(coicop = as.numeric(coicop)) %>% 
  left_join(distinct(morocco_inflation_rates, coicop, Division)) %>% 
  mutate(coicop = ifelse(is.na(coicop), -1, coicop),
         Division = ifelse(is.na(Division), "All less Food and Transport", Division)) %>% 
  mutate(LOCATION = "MAR", Country = "Morocco", .before = 1)


##other countries contributions
worldwide_contributions <- worldwide_data %>% 
  filter(MEASURE == "CTGY", TIME == "2022-07") %>% 
  filter(SUBJECT %in% c("CP010000", "CP070000", "CPALTT01")) %>% 
  mutate(coicop = str_extract(SUBJECT, "[1-9]") %>% as.numeric(),
         coicop = ifelse(SUBJECT == "CPALTT01", 0, coicop),
         date = lubridate::date(paste(TIME, "01", sep = "-")),
         month = lubridate::month(date),
         year = lubridate::year(date)) %>% 
  pivot_wider(id_cols = c(LOCATION, Country, date, month, year), names_from = coicop, values_from = Value) %>% 
  mutate(Contribution_all_less_food_transport = `0` - `1` - `7`) %>% 
  pivot_longer(cols = -c(1:5), names_to = "coicop", values_to = "contribution_to_inflation") %>% 
  mutate(coicop = as.numeric(coicop)) %>% 
  left_join(distinct(morocco_inflation_rates, coicop, Division)) %>% 
  mutate(coicop = ifelse(is.na(coicop), -1, coicop),
         Division = ifelse(is.na(Division), "All less Food and Transport", Division))


##binding contributions data
world_morocco_contributions <- worldwide_contributions %>% 
  bind_rows(morocco_contributions)



#binding all together

contributions <- TOP_15_inflation %>% 
  left_join(world_morocco_contributions) %>% 
  select(LOCATION, Country, date, coicop, Division, month, year, value = contribution_to_inflation) %>% 
  mutate(facet_ = "Contributions") %>% 
  filter(coicop != 0) %>% 
  group_by(LOCATION) %>%
  mutate(value = 100 * value / sum(value)) 



inflation_rates <- TOP_15_inflation %>% 
  left_join(world_morocco_data) %>% 
  mutate(facet_ = Division) %>%
  rename(value = inflation_rate) %>%
  filter(coicop %in% c(-1,0,1,7))


inflation_contributions <- bind_rows(inflation_rates, contributions)


write_csv(inflation_contributions, file = "data-clean/inflation_clean_data.csv")
write_csv(morocco_inflation_rates, file = "data-clean/morocco_inflation_clean_data.csv")
write_csv(Indice_global, file = "data-clean/all_items_inflation.csv")
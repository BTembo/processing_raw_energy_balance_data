
## Load packages
library(readxl)
library(tidyr)
library(plyr, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)

##

## Set working directory
setwd("~/Documents/modelling/Energy Modelling/cleaning_energy_balance") # Note: this will be different on your computer


## Input other data that will be used to process the energy balance
# flow data
df_oed_flow <- readxl::read_xlsx("other_energy_data.xlsx",
                                 sheet = "flow",
                                 col_names = T) %>% 
  dplyr::distinct()

df_oed_flow

# product data
df_oed_prod <- readxl::read_xlsx("other_energy_data.xlsx",
                                 sheet = "product",
                                 col_names = T) %>%
  dplyr::distinct()

df_oed_prod

# conversion factors data
df_conv_factors <- readxl::read_xlsx("other_energy_data.xlsx",
                                     sheet = "conversion_factors",
                                     col_names = T) %>%
  dplyr::select(-source) %>% 
  dplyr::distinct()

df_conv_factors


## Paths to the location of raw data
esnon_file <- "./raw_eb_data/Zambia_Energy_Balances_esnon.xlsx" # esnon data
eeb_file <- "./raw_eb_data/Zambia_ExtendedEB_1990-.csv" # eeb data


## Input the raw data from esnon

# Extract the observation's unit
get_unit_esnon <- function(InputData){
  
  listVal <- InputData
  
  if(str_detect(listVal, "\\(kt\\)") == T){
    "kt"
  } else if(str_detect(listVal, "\\(TJ\\)") == T){
    "TJ"
  } else   if(str_detect(listVal, "\\(GWh\\)") == T){
    "GWh"
  } else if(str_detect(listVal, "\\(TJ-net\\)") == T){
    "TJ net"
  } else if(str_detect(listVal, "\\(TJ-gross\\)") == T){
    "TJ gross"
  } else if(str_detect(listVal, "\\(direct use in TJ-net\\)") == T){
    "TJ net"
  } else {
    ""
  }
  
}

# add variables to the raw data
get_processed_esnon_data <- function(InputData){
  
  dataVal <- InputData
  
  dataVal$product <- plyr::mapvalues(dataVal$Product,
                                     from = df_oed_prod$product_esnon,
                                     to = df_oed_prod$product_common)
  
  dataVal$flow <- plyr::mapvalues(dataVal$flow_index,
                                  from = df_oed_flow$flow_esnon_eeb_index,
                                  to = df_oed_flow$flow_common)
  
  dataVal$stage <- plyr::mapvalues(dataVal$flow_index,
                                   from = df_oed_flow$flow_esnon_eeb_index,
                                   to = df_oed_flow$Category2)
  
  listVal <- dataVal$Product
  listVal2 <- listVal[!is.na(listVal)]
  
  listVal = as.character(lapply(listVal2, get_unit_esnon))
  
  dataVal2 <- as_tibble(dataVal)  %>% 
    dplyr::bind_cols(unit = listVal) %>%
    dplyr::select(source, product, flow, stage, unit, year, value) # %>%
  # tidyr::spread(key = year, value = value)
  
  
}

# Combine the all the esnon data into one dataframe
combine_esnon_data <- function(InputDataPath){
  
  dataVal_flow <- df_oed_flow %>% 
    filter(!is.na(flow_esnon))
  
  file_path <- InputDataPath
  
  sheets <- excel_sheets(path = file_path)
  
  dataVal_mega <- NULL
  
  for (sheet in sheets){
    
    dataVal <- readxl::read_xlsx(file_path,
                                 sheet = sheet,
                                 skip = 6,
                                 col_names = T) %>%
      dplyr::filter(PRODUCT %in% df_oed_flow$flow_esnon,
                    !is.na(PRODUCT)) %>%
      dplyr::mutate(source = "esnon", # include the source of the data
                    year = as.numeric(sheet),
                    flow_index = dataVal_flow$flow_esnon_eeb_index) %>% 
      tidyr::gather("Product", "value", -PRODUCT, -source, -year, -flow_index) %>% 
      dplyr::mutate(value = as.numeric(value))
      
    dataVal <- get_processed_esnon_data(dataVal)
    
    dataVal_mega = dplyr::bind_rows(dataVal_mega, dataVal)
    
  }
  
  return(dataVal_mega)
  
}

df_esnon <- combine_esnon_data(esnon_file)
df_esnon


## Input the raw data from eeb
get_processed_eeb_data <- function(InputData){
  
  dataVal <- InputData
  
  dataVal <- read_csv(InputData,
                     show_col_types = FALSE,
                     col_names = T) %>%
    dplyr::mutate(source = "eeb") # Include the source of the data
  
  dataVal$product <- plyr::mapvalues(dataVal$Product,
                                     from = df_oed_prod$products_eeb2,
                                     to = df_oed_prod$product_common)
  
  dataVal$flow <- plyr::mapvalues(dataVal$Flow,
                                  from = df_oed_flow$flow_eeb2,
                                  to = df_oed_flow$flow_common)
  
  dataVal$stage <- plyr::mapvalues(dataVal$Flow,
                                   from = df_oed_flow$flow_eeb2,
                                   to = df_oed_flow$Category2)
  
  dataVal2 <- as_tibble(dataVal)
  
  dataVal <- dataVal2 %>%
    dplyr::rename(year = Time,
                  unit = Unit,
                  value = Value) %>%
    dplyr::select(source, product, flow, stage, unit, year, value)
  
}


df_eeb <- get_processed_eeb_data(eeb_file)
df_eeb

## Combine the eeb and esnon data into one large file
df_combined <- dplyr::bind_rows(df_esnon, df_eeb) %>% 
  dplyr::full_join(df_conv_factors, by = c('product', 'unit')) %>% 
  dplyr::mutate(value_TJ = value * unit_value_in_TJ)

df_combined


## remove this files
rm("df_conv_factors", "df_eeb", "df_esnon", "df_oed_flow", "df_oed_prod")


## Do some data exploration

# Get the series in a specified range of years
get_series <- function(InputData, InputYear1, InputYear2){
  
  dataVal <- InputData %>%
    dplyr::filter(year >= InputYear1 & year <= InputYear2,
                  # Flow == "Production",
                  stage ==  "Final consumption"
                  )
  
}

df <- get_series(df_combined, 1985, 2020) %>% 
  dplyr::arrange(year, product) %>% 
  dplyr::filter(flow == "Final consumption",
                # product == #"Electricity"
                #   # "Hydro"
                #   # "Gas/diesel oil excl. biofuels"
                #   # "Fuel oil"
                #   "Liquefied petroleum gases (LPG)"
                )

df


# Compare the two data sources
df %>% 
  filter(value_TJ > 0,
         !product %in% c("Total",
                        "Memo: Renewables")) %>% 
  ggplot2::ggplot(aes(year, value_TJ/1e3, colour = source)) +
  facet_wrap(~ product, 
             nrow = 2) + 
  geom_line() +
  xlab("Years") +
  ylab("Final Consumption in PJ") +
  theme(legend.position = "bottom") -> plt_all

plt_all






## The script ends here ##
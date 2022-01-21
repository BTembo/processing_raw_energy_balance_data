

library(readxl)
library(tidyr)
library(plyr, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)

###

## Set working directory
setwd("~/Documents/modelling/Energy Modelling/cleaning_energy_balance") # Note: this will be different on your computer


##

# Input other data that will be used to process the energy balance
df_oed_flow <- readxl::read_xlsx("other_energy_data.xlsx",
                                 sheet = "flow",
                                 col_names = T) %>% 
  dplyr::distinct()

df_oed_flow

# length(sort(unique(df_oed_flow$Flow)))


df_oed_prod <- readxl::read_xlsx("other_energy_data.xlsx",
                                 sheet = "product",
                                 col_names = T) %>%
  dplyr::distinct()

df_oed_prod
# 
# length(sort(unique(df_oed_prod$Flow)))

df_conv_factors <- readxl::read_xlsx("other_energy_data.xlsx",
                                     sheet = "conversion_factors",
                                     col_names = T) %>%
  dplyr::select(-source) %>% 
  dplyr::distinct()

df_conv_factors


## Paths to the location of raw data
esnon_file <- "./raw_eb_data/Zambia_Energy_Balances_esnon.xlsx"
eeb_file <- "./raw_eb_data/Zambia_ExtendedEB_1990-.csv"


# Input the raw data from esnon


#
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

# 
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


# df_esnon2 <- get_processed_esnon_data(df)
# df_esnon2


# Combine the data into one dataframe
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



# length(unique(df_esnon$PRODUCT))
# length(unique(df_oed_flow$flow_esnon))
# 
# 
# dfx1 <- df_oed_flow %>% 
#   filter(!is.na(flow_esnon))
# 
# 
# df_esnon$PRODUCT == dfx1$flow_esnon
# 
# df <- df_esnon %>% 
#   tidyr::gather("Product", "value", -PRODUCT, -source, -year, -flow_index)
#   
# df



#Input the raw data
# df_eeb <- read_csv(eeb_file,
#                    show_col_types = FALSE,
#                    col_names = T) %>%
#   dplyr::mutate(source = "eeb") # Include the source of the data
# 
# df_eeb
# 
# 
# unique(df_eeb$'Flag Codes')
# unique(df_eeb$Unit)
# 
# length(sort(unique(df_eeb$Product))) == length(sort(unique(df_eeb$Flow)))


## get the names of the products in the dataset
# df_raw %>% 
#   dplyr::select(PRODUCT, Product) %>% 
#   dplyr::distinct() -> df_prod
# 
# write.csv(df_prod, 'eb_products.csv', row.names = F)


## get the names of the flow in the dataset
# df_raw %>%
#   dplyr::select(FLOW, Flow) %>%
#   dplyr::distinct() -> df_flow
# 
# write.csv(df_flow, 'eb_flow.csv', row.names = F)

# 
# # Input other data that will be used to process the energy balance
# df_oed_flow <- readxl::read_xlsx("other_energy_data.xlsx",
#                              sheet = "flow",
#                              col_names = T) %>% 
#   dplyr::distinct()
# 
# df_oed_flow
# 
# # length(sort(unique(df_oed_flow$Flow)))
# 
# 
# df_oed_prod <- readxl::read_xlsx("other_energy_data.xlsx",
#                              sheet = "product",
#                              col_names = T) %>%
#   dplyr::distinct()
# 
# df_oed_prod
# # 
# # length(sort(unique(df_oed_prod$Flow)))




# ##
# 
# get_unit_esnon <- function(InputData){
#   
#   listVal <- InputData
#   
#   if(str_detect(listVal, "\\(kt\\)") == T){
#     "kt"
#   } else if(str_detect(listVal, "\\(TJ\\)") == T){
#     "TJ"
#   } else   if(str_detect(listVal, "\\(GWh\\)") == T){
#     "GWh"
#   } else if(str_detect(listVal, "\\(TJ-net\\)") == T){
#     "TJ net"
#   } else if(str_detect(listVal, "\\(TJ-gross\\)") == T){
#     "TJ gross"
#   } else if(str_detect(listVal, "\\(direct use in TJ-net\\)") == T){
#     "TJ net"
#   } else {
#     ""
#   }
#   
# }
# 
# get_processed_esnon_data <- function(InputData){
#   
#   dataVal <- InputData
#   
#   dataVal$product <- plyr::mapvalues(dataVal$Product,
#                                      from = df_oed_prod$product_esnon,
#                                      to = df_oed_prod$product_common)
#   
#   dataVal$flow <- plyr::mapvalues(dataVal$flow_index,
#                                   from = df_oed_flow$flow_esnon_eeb_index,
#                                   to = df_oed_flow$flow_common)
#   
#   dataVal$stage <- plyr::mapvalues(dataVal$flow_index,
#                                    from = df_oed_flow$flow_esnon_eeb_index,
#                                    to = df_oed_flow$Category2)
#   
#   listVal <- dataVal$Product
#   listVal2 <- listVal[!is.na(listVal)]
# 
#   listVal = as.character(lapply(listVal2, get_unit_esnon))
#   
#   dataVal2 <- as_tibble(dataVal)  %>% 
#     dplyr::bind_cols(unit = listVal) %>%
#     dplyr::select(source, product, flow, stage, unit, year, value) # %>%
#     # tidyr::spread(key = year, value = value)
# 
#   
# }
# 
# 
# df_esnon2 <- get_processed_esnon_data(df)
# df_esnon2
# 
# 
# ##



get_processed_eeb_data <- function(InputData){ #, InputYear1, InputYear2
  
  dataVal <- InputData
  
  dataVal <- read_csv(InputData,
                     show_col_types = FALSE,
                     col_names = T) %>%
    dplyr::mutate(source = "eeb") # Include the source of the data
  
  # df_eeb
  #
  # print(dim(dataVal))
  
  dataVal$product <- plyr::mapvalues(dataVal$Product,
                                     from = df_oed_prod$products_eeb2,
                                     to = df_oed_prod$product_common)
  
  # print(dim(dataVal))
  #
  dataVal$flow <- plyr::mapvalues(dataVal$Flow,
                                  from = df_oed_flow$flow_eeb2,
                                  to = df_oed_flow$flow_common)
  
  dataVal$stage <- plyr::mapvalues(dataVal$Flow,
                                   from = df_oed_flow$flow_eeb2,
                                   to = df_oed_flow$Category2)
  
  # print(dim(dataVal))
  #  
  dataVal2 <- as_tibble(dataVal)
  # rm(dataVal)
  
  dataVal <- dataVal2 %>%
    dplyr::rename(year = Time,
                  unit = Unit,
                  value = Value) %>%
    dplyr::select(source, product, flow, stage, unit, year, value) #%>%
    # dplyr::filter(year >= InputYear1 & year <= InputYear2,
    #               # Flow == "Production"
    # ) #%>%
    # tidyr::spread(key = year, value = value) %>%
    # as_tibble()
  
}


df_eeb <- get_processed_eeb_data(eeb_file)
df_eeb


df_combined <- dplyr::bind_rows(df_esnon, df_eeb) %>% 
  dplyr::full_join(df_conv_factors, by = c('product', 'unit')) %>% 
  dplyr::mutate(value_TJ = value * unit_value_in_TJ)

df_combined

unique(df_combined$product)



# df_units <- df_combined %>% 
#   dplyr::select(product, unit) %>% 
#   dplyr::distinct() %>% 
#   dplyr::filter(unit != 'TJ')
# 
# df_units
# 
# # write.csv(df_units, 'units_factors.csv', row.names = F)

# df_units2 <- df_combined %>%
#   dplyr::select(product, unit) %>%
#   dplyr::distinct() %>%
#   dplyr::filter(!unit %in% c('TJ gross', 'GWh', 'kt'))
# 
# df_units2
# 
# # write.csv(df_units2, 'units_factors2.csv', row.names = F)


# Get the series in a specified range of years
get_series <- function(InputData, InputYear1, InputYear2){
  
  dataVal <- InputData %>%
    dplyr::filter(year >= InputYear1 & year <= InputYear2,
                  # Flow == "Production",
                  stage ==  "Final consumption"
                  ) #%>%
  
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
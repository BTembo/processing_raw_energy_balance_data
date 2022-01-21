

library(readxl)
library(tidyr)
library(plyr, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)

###

## Set working directory
setwd("~/Documents/modelling/Energy Modelling/cleaning_energy_balance") # Note: this will be different on your computer


## Paths to the location of raw data
esnon_file <- "./raw_eb_data/Zambia_Energy_Balances_esnon.xlsx"
eeb_file <- "./raw_eb_data/Zambia_ExtendedEB_1990-.csv"


# Input the raw data from esnon
combine_esnon_data <- function(InputDataPath){
  
  dataVal_flow <- df_oed_flow %>% 
    filter(!is.na(flow_esnon))
  # df_esnon$PRODUCT == dfx1$flow_esnon
  
  file_path <- InputDataPath
  
  sheets <- excel_sheets(path = file_path)
  
  print(sheets)
  
  for (sheet in sheets){
    
    dataVal <- readxl::read_xlsx(file_path,
                                 sheet = sheet,
                                 skip = 6,
                                 col_names = T) %>%
      # dplyr::mutate(source = "esnon", # Include the source of the data
      #               year = sheet) %>% 
      dplyr::filter(PRODUCT %in% df_oed_flow$flow_esnon,
                    !is.na(PRODUCT)) %>%
      dplyr::mutate(source = "esnon", # Include the source of the data
                    year = sheet,
                    flow_index = dataVal_flow$flow_esnon_eeb_index)
    
    print(dataVal)
    
  }
  
  return(dataVal)
  
} # Continue woeking on this function # Look out for the error under the row with BKB etc

df_esnon <- combine_esnon_data(esnon_file)




length(unique(df_esnon$PRODUCT))
length(unique(df_oed_flow$flow_esnon))


dfx1 <- df_oed_flow %>% 
  filter(!is.na(flow_esnon))


df_esnon$PRODUCT == dfx1$flow_esnon

df <- df_esnon %>% 
  tidyr::gather("Product", "value", -PRODUCT, -source, -year, -flow_index)
  
df



#Input the raw data
df_eeb <- read_csv(eeb_file,
                   show_col_types = FALSE,
                   col_names = T) %>%
  dplyr::mutate(source = "eeb") # Include the source of the data

df_eeb


unique(df_eeb$'Flag Codes')
unique(df_eeb$Unit)

length(sort(unique(df_eeb$Product))) == length(sort(unique(df_eeb$Flow)))


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




##

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


df_esnon2 <- get_processed_esnon_data(df)
df_esnon2


# df2 = df_oed_prod$product_esnon %>% 
#   dplyr::mutate(unit = ifesle(str_detect(product_esnon, "\\(kt\\)") == T,
#                               "kt", "nothings"))
# 
# df2
# 
# unit = ifelse(str_detect(df_oed_prod$product_esnon, "\\(kt\\)") == T,
#               "kt", "nothings")
# 
# # 
# # get_unit_esnon <- function(InputData){
# #   
# #   listVal <- InputData
# #   # print((listVal))
# #   # listVal2 <- listVal[!is.na(listVal)]
# #   # print((listVal2))
# #   
# #   if(str_detect(listVal, "\\(kt\\)") == T){
# #     "kt"
# #     } else if(str_detect(listVal, "\\(TJ\\)") == T){
# #       "TJ"
# #     } else   if(str_detect(listVal, "\\(GWh\\)") == T){
# #       "GWh"
# #     } else if(str_detect(listVal, "\\(TJ-net\\)") == T){
# #       "TJ net"
# #     } else if(str_detect(listVal, "\\(TJ-gross\\)") == T){
# #       "TJ gross"
# #     } else if(str_detect(listVal, "\\(direct use in TJ-net\\)") == T){
# #       "TJ net"
# #     } else {
# #       ""
# #     }
# #   
# # }
# 
# # x = c("Fuel oil (kt)", "Other recovered gases (TJ-gross)", "Heat (TJ)", "Source")
# # 
# # class(x)
# # x1 = !is.na(df_oed_prod$product_esnon)
# 
# lst_product_esnon <- df_oed_prod$product_esnon
# lst_product_esnon2 <- lst_product_esnon[!is.na(lst_product_esnon)]
# # lst_product_esnon2
# # class(x1)
# # 
# # # xunit <- get_unit_esnon(df_oed_prod$product_esnon)
# # lapply(x, get_unit_esnon)
# # x1 = as.character(lapply(lst_product_esnon2, get_unit_esnon))
# # x1
# 
# x1a = as.character(lapply(lst_product_esnon, get_unit_esnon))
# x1a
##



get_processed_eeb_data <- function(InputData, InputYear1, InputYear2){
  
  dataVal <- InputData
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
    dplyr::select(source, product, flow, stage, unit, year, value) %>%
    dplyr::filter(year >= InputYear1 & year <= InputYear2,
                  # Flow == "Production"
    ) #%>%
    # tidyr::spread(key = year, value = value) %>%
    # as_tibble()
  
}


df_eeb2 <- get_processed_eeb_data(df_eeb, 2008, 2010)
df_eeb2


unique(df_eeb2$product)

dfx = df_eeb2 %>% 
  filter(stage == "Final consumption",
         product == #"Electricity"
         "Gas/diesel oil excl. biofuels" )
  # tidyr::spread(key = year, value = value)

dfx

df_eeb2 <- df_eeb %>% 
  dplyr::select(Product, Flow, Unit, Time, Value) %>% 
  dplyr::filter(Time > 2007 & Time < 2012,
                # Flow == "Production"
                ) %>%
  dplyr::mutate(Source = "eeb") %>% 
  tidyr::spread(key = Time, value = Value)

df_eeb2


df_eeb2$product <- plyr::mapvalues(df_eeb2$Product,
                                   from = df_oed_prod$products_eeb2, 
                                   to = df_oed_prod$product_common)


df_eeb2$flow <- plyr::mapvalues(df_eeb2$Flow,
                                   from = df_oed_flow$flow_eeb2, 
                                   to = df_oed_flow$flow_common)


unique(df_eeb2$product)


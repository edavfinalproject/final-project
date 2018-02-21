



library(data.table)
library(tidyverse)


pop_zip <- "http://api.worldbank.org/v2/en/indicator/SP.POP.GROW?downloadformat=csv"
energy_zip <- "http://api.worldbank.org/v2/en/indicator/EG.USE.PCAP.KG.OE?downloadformat=csv"

zip_list <- c(pop_zip, energy_zip)

for (i in zip_list){
    
    td = tempdir()
    tf = tempfile(tmpdir=td, fileext=".zip")
    
    download.file(i, tf, mode = "wb")
    zip_folder <- unzip(tf, list = TRUE)
    
    for (j in seq_along(zip_folder$Name)) {
        
        file_name = unzip(tf, list = TRUE)$Name[j]
        unzip(tf, files = file_name, exdir = td, overwrite = TRUE)
        file_path <- file.path(td, file_name)
        csv <- fread(file_path)
        assign(file_name, csv)
        
    }
    
}

pop_df <-  API_SP.POP.GROW_DS2_en_csv_v2.csv; rm(API_SP.POP.GROW_DS2_en_csv_v2.csv)
energy_df <- API_EG.USE.PCAP.KG.OE_DS2_en_csv_v2.csv; rm(API_EG.USE.PCAP.KG.OE_DS2_en_csv_v2.csv)
pop_meta <- Metadata_Country_API_SP.POP.GROW_DS2_en_csv_v2.csv; rm(Metadata_Country_API_SP.POP.GROW_DS2_en_csv_v2.csv)
energy_meta <- Metadata_Country_API_EG.USE.PCAP.KG.OE_DS2_en_csv_v2.csv; rm(Metadata_Country_API_EG.USE.PCAP.KG.OE_DS2_en_csv_v2.csv)

setnames(pop_df, old = c("Country Name", "Country Code", "Indicator Name", "Indicator Code"), 
         new = c("COUNTRY_NAME", "COUNTRY_CODE", "INDICATOR_NAME", "INDICATOR_CODE"))
setnames(energy_df, old = c("Country Name", "Country Code", "Indicator Name", "Indicator Code"), 
         new = c("COUNTRY_NAME", "COUNTRY_CODE", "INDICATOR_NAME", "INDICATOR_CODE"))
setnames(pop_meta, old = c("Country Code"), 
         new = c("COUNTRY_CODE"))
setnames(energy_meta, old = c("Country Code"), 
         new = c("COUNTRY_CODE"))

pop_meta <- select(pop_meta, names(pop_meta)[1:3])
pop_df <- select(pop_df, -V63)


pop_country_year <- pop_df %>%
    select(-COUNTRY_NAME, -INDICATOR_NAME, -INDICATOR_CODE) %>%
    gather(key = "YEAR", value = "POP_GROWTH_RATE", 
           -COUNTRY_CODE) %>%
    mutate(COUNTRY_YEAR = paste0(COUNTRY_CODE,"_",YEAR))

energy_country_year <- energy_df %>%
    select(-COUNTRY_NAME, -INDICATOR_NAME, -INDICATOR_CODE) %>%
    gather(key = "YEAR", value = "ENERGY_USE", 
           -COUNTRY_CODE) %>%
    mutate(COUNTRY_YEAR = paste0(COUNTRY_CODE,"_",YEAR))

# Scatter plot of energy use and growth rate by country-year
country_year <- 
    left_join(pop_country_year, energy_country_year, by = "COUNTRY_YEAR") %>%
    select(COUNTRY_YEAR, POP_GROWTH_RATE, ENERGY_USE) %>%
    filter(POP_GROWTH_RATE != '' & ENERGY_USE != '') %>%
    mutate(POP_GROWTH_RATE = as.numeric(POP_GROWTH_RATE)) %>%
    mutate(ENERGY_USE = as.numeric(ENERGY_USE))

ggplot(country_year, aes(POP_GROWTH_RATE, ENERGY_USE)) + 
    geom_point()

# Counts of countries by region and income group
ggplot(pop_meta %>% filter(IncomeGroup != ""), aes(Region)) +
    geom_bar() + coord_flip()

ggplot(pop_meta %>% filter(IncomeGroup != ""), aes(IncomeGroup)) +
    geom_bar() + coord_flip()


# Population growth rate over time by geographic region
pop_year_world <- pop_country_year %>%
    filter(COUNTRY_CODE %in% c("SSH", "SAS", "NAC", "MEA", 
                               "LCN", "ECS", "EAS")) %>%
    mutate(YEAR = as.integer(YEAR)) %>%
    mutate(POP_GROWTH_RATE = as.numeric(POP_GROWTH_RATE))

ggplot(pop_year_world, 
       aes(YEAR, POP_GROWTH_RATE, color = COUNTRY_CODE)) + geom_line()







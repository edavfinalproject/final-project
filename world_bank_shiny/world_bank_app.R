#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

needed_packages <- c("data.table", "tidyverse", "shiny", "WDI", "extracat")
library(data.table)
library(tidyverse)
library(WDI)
library(extracat)
# 
# for (p in needed_packages) {
#      if (!(p %in% installed.packages())){
#           install.packages(p, repos = "http://cran.us.r-project.org")
#      }
#      library(p, character.only = TRUE)
# }


pop_growth_df <- fread("./API_SP.POP.GROW_DS2_en_csv_v2.csv")
new_header <- str_to_upper(gsub(pattern = " ", replacement = "_", x = names(pop_growth_df)))
setnames(pop_growth_df, new_header)

rm(API_SP.POP.GROW_DS2_en_csv_v2.csv)
pop_growth_meta <- fread("./Metadata_Country_API_SP.POP.GROW_DS2_en_csv_v2.csv")
new_header <- str_to_upper(gsub(pattern = " ", replacement = "_", x = names(pop_growth_meta)))
setnames(pop_growth_meta, new_header)
pop_growth_df <- merge(pop_growth_df, subset(pop_growth_meta, select=-c(4,5,6)), by = "COUNTRY_CODE")

pop_df <- fread("./API_SP.POP.TOTL_DS2_en_csv_v2.csv")
new_header <- str_to_upper(gsub(pattern = " ", replacement = "_", x = names(pop_df)))
setnames(pop_df, new_header)
pop_df$V64 <- NULL
pop_meta <- fread("./Metadata_Country_API_SP.POP.TOTL_DS2_en_csv_v2.csv")
new_header <- str_to_upper(gsub(pattern = " ", replacement = "_", x = names(pop_meta)))
setnames(pop_meta, new_header)
pop_df <- merge(pop_df, subset(pop_meta, select=-c(4,5,6)), by = "COUNTRY_CODE")

energy_df <- fread("./API_EG.USE.PCAP.KG.OE_DS2_en_csv_v2.csv")
new_header <- str_to_upper(gsub(pattern = " ", replacement = "_", x = names(energy_df)))
setnames(energy_df, new_header)
energy_df_meta <- fread("./Metadata_Country_API_EG.USE.PCAP.KG.OE_DS2_en_csv_v2.csv")
new_header <- str_to_upper(gsub(pattern = " ", replacement = "_", x = names(energy_df_meta)))
setnames(energy_df_meta, new_header)
energy_df <- merge(energy_df, subset(energy_df_meta, select=-c(4,5,6)), by = "COUNTRY_CODE")


# Tidy each of the three data frames
pop_df <- pop_df %>%
     gather(key = "YEAR", value = "VALUE", -COUNTRY_CODE, -COUNTRY_NAME,
            -INDICATOR_NAME, -INDICATOR_CODE, -REGION, -INCOMEGROUP) %>%
     mutate(YEAR = as.numeric(YEAR)) %>%
     mutate(VALUE = as.numeric(VALUE))

energy_df <- energy_df %>%
     gather(key = "YEAR", value = "VALUE", -COUNTRY_CODE, -COUNTRY_NAME,
            -INDICATOR_NAME, -INDICATOR_CODE, -REGION, -INCOMEGROUP) %>%
     mutate(YEAR = as.numeric(YEAR)) %>%
     mutate(VALUE = as.numeric(VALUE))

pop_growth_df <- pop_growth_df %>%
     gather(key = "YEAR", value = "VALUE", -COUNTRY_CODE, -COUNTRY_NAME,
            -INDICATOR_NAME, -INDICATOR_CODE, -REGION, -INCOMEGROUP) %>%
     mutate(YEAR = as.numeric(YEAR)) %>%
     mutate(VALUE = as.numeric(VALUE))

# CREATE INITIAL COMBINED TIDY DATAFRAME
tidy_pop_energy <- rbind(pop_growth_df, pop_df, energy_df, fill = TRUE)
tidy_pop_energy$V63 <- NULL

# CLEAN UP ATTRIBUTES
tidy_pop_energy <- tidy_pop_energy %>%
     mutate(REGION = str_trim(REGION)) %>% #trailing whitespace in some regions
     mutate(YEAR = as.numeric(YEAR)) %>%
     mutate(VALUE = as.numeric(VALUE))

# LIST OF COUNTRIES WHICH ARE NOT ACTUALLY COUNTRIES
not_country = c("Arab World", "Central Europe and the Baltics", "Early-demographic dividend", "Europe & Central Asia (excluding high income)", "Euro area", "Fragile and conflict affected situations", "Heavily indebted poor countries (HIPC)", "IBRD only","IDA total", "Not classified", "Latin America & Caribbean (excluding high income)", "Latin America & Caribbean", "Low income", "Low & middle income", "Late-demographic dividend", "Middle income", "North America", "Post-demographic dividend", "Small states", "East Asia & Pacific (IDA & IBRD countries)", "Latin America & the Caribbean (IDA & IBRD countries)", "Middle East & North Africa (IDA & IBRD countries)", "South Asia (IDA & IBRD)", "World", "Upper middle income", "Sub-Saharan Africa (IDA & IBRD countries)", "Europe & Central Asia (IDA & IBRD countries)", "Sub-Saharan Africa", "Sub-Saharan Africa (excluding high income)", "Pacific island small states", "Pre-demographic dividend", "Other small states", "OECD members", "Middle East & North Africa (excluding high income)", "Middle East & North Africa", "Lower middle income", "Least developed countries: UN classification", "IDA only", "IDA blend", "IDA & IBRD total", "High income", "European Union", "Europe & Central Asia", "East Asia & Pacific", "East Asia & Pacific (excluding high income)", "Caribbean small states", "South Asia")

# FILTER SO ONLY COUNTRIES ARE IN TIDY FRAME, SORT ALPHABETICALLY
tidy_pop_energy <- filter(tidy_pop_energy, !COUNTRY_NAME %in% not_country)
tidy_pop_energy <- tidy_pop_energy %>% arrange(COUNTRY_NAME)

# ADDING IN ENERGY TOTAL (FOR TAB 1)
# DIVIDE POPULATION BY 1 MILLION FOR SCALING; RENAME INDICATOR
pop_df <- pop_df %>% mutate(VALUE = VALUE/1000000) %>% 
     mutate(INDICATOR_NAME = c("Population (millions)"))
# DIVIDE ENERGY PER CAPITA BY 1000; RENAME INDICATOR
energy_df <- energy_df %>% mutate(VALUE = VALUE/1000) %>%
     mutate(INDICATOR_NAME = c("Energy use per capita (teragrams of oil equivalent)"))
# CREATE ENERGY TOTAL COLUMN
energy_total_df <- energy_df %>% mutate(VALUE = VALUE * pop_df$VALUE) %>% mutate(INDICATOR_NAME = c("Energy use total (teragrams of oil equivalent)"))

# ADD ENERGY TOTAL TO DATAFRAME (FOR TAB 1)
tidy_pop_energy_for_tab_1 <- rbind(pop_growth_df, pop_df, energy_df, energy_total_df)
tidy_pop_energy_for_tab_1 <- tidy_pop_energy_for_tab_1 %>%
     mutate(YEAR = as.numeric(YEAR)) %>%
     mutate(VALUE = as.numeric(VALUE))

# CREATE DATAFRAME OF SUMMARY REGION STATS
tidy_overview <- filter(tidy_pop_energy_for_tab_1, COUNTRY_NAME %in% c("East Asia & Pacific", "Europe & Central Asia", "Latin America & Caribbean", "Middle East & North Africa", "North America", "South Asia", "Sub-Saharan Africa"))
tidy_overview <- tidy_overview %>% arrange(COUNTRY_NAME)

# REMOVE NON-COUNTRIES FROM TAB 1 DATAFRAME
tidy_pop_energy_for_tab_1 <- filter(tidy_pop_energy_for_tab_1, !COUNTRY_NAME %in% not_country)
tidy_pop_energy_for_tab_1 <- tidy_pop_energy_for_tab_1 %>% arrange(COUNTRY_NAME)


# Reform data
icodes <-  unique(tidy_pop_energy$INDICATOR_CODE)
scatter_data <- tidy_pop_energy[tidy_pop_energy$INDICATOR_CODE == icodes[1],]
scatter_data$pop_growth <- as.numeric(tidy_pop_energy$VALUE[tidy_pop_energy$INDICATOR_CODE == icodes[1]])
scatter_data$pop_tot <- as.numeric(tidy_pop_energy$VALUE[tidy_pop_energy$INDICATOR_CODE == icodes[2]])/1000000
scatter_data$energy_percap <- as.numeric(tidy_pop_energy$VALUE[tidy_pop_energy$INDICATOR_CODE == icodes[3]])
scatter_data$energy_tot <- (scatter_data$energy_percap * scatter_data$pop_tot) / 1000
scatter_data <- scatter_data[complete.cases(scatter_data), ]

# Alter dataset
icodes <- unique(tidy_pop_energy$INDICATOR_CODE)
my_data <- tidy_pop_energy[tidy_pop_energy$INDICATOR_CODE == icodes[2],]
my_data$VALUE <- as.numeric(my_data$VALUE)/1000000
my_data$YEAR <- as.numeric(my_data$YEAR)

# Define UI for application that draws a histogram
ui <- fluidPage(
     navbarPage("Interactive Component",
                tabPanel("Individual Country",
                         verticalLayout(
                              plotOutput("countryhist", width="100%", height = "400px"),
                              wellPanel(
                                   selectInput(inputId = "tab1country", label = "Country:",
                                               choices = as.vector(unique(tidy_pop_energy$COUNTRY_NAME)),
                                               selected = as.vector(unique(tidy_pop_energy$COUNTRY_NAME))[208])
                              )
                         )
                )
                ,
                tabPanel("Country Comparison",
                         
                         verticalLayout(
                              plotOutput("ds_barplot", width = "100%"),
                              fluidRow(
                                   column(
                                        radioButtons(label = "Indicator", inputId = "indicator",
                                                     choices = c("Population Growth" = "Population growth (annual %)",
                                                                 "Total Energy use" = "Total Energy use",
                                                                 "Total Population" = "Population, total"),
                                                     selected = "Population growth (annual %)"), width = 3),
                                   column(
                                        radioButtons(label = "Categories", inputId = "category",
                                                     choices = c("Geographic Region" = "REGION",
                                                                 "Income Group" = "INCOMEGROUP",
                                                                 "None" = "INDICATOR_NAME"),
                                                     selected = "INDICATOR_NAME"), width = 3),
                                   column(
                                        sliderInput(inputId = "year",
                                                    label = "Year:",
                                                    value = 2010,
                                                    min = 1961,
                                                    animate = animationOptions(interval = 333),
                                                    max = 2016,
                                                    step = 1), 
                                        helpText("Click the play button to animate"),
                                        width = 3),
                                   
                                  
                                   column(
                                        sliderInput(inputId = "rank_slider",
                                                    label = "Percentage slice",
                                                    min = 0, max = 1, value = c(0, 1)),width = 3)
                              )
                         )
                )
                ,
                tabPanel("Energy vs. Population",
                         verticalLayout(
                              plotOutput("ds_scatter",  width = "100%"),
                              wellPanel(
                                   sliderInput(inputId = "year_scatter",
                                               label = "Year:",
                                               value = min(tidy_pop_energy$YEAR, na.rm = TRUE),
                                               min = 1971,
                                               max = 2012,
                                               step = 1),
                                   sliderInput(inputId = "pop_max",
                                               label = "Max population:",
                                               value = 1500,
                                               min = 0,
                                               max = 1500,
                                               step = 100),
                                   sliderInput(inputId = "energy_max",
                                               label = "Max energy:",
                                               value = 3000,
                                               min = 0,
                                               max = 3000,
                                               step = 2000)
                              )
                         )
                ),
                tabPanel("Country Races",
                         verticalLayout(
                              plotOutput("countryRace",  width = "100%"),
                              fluidRow(
                                   column(
                                        sliderInput(inputId = "raceYear",
                                                    label = "Year:",
                                                    value = 1971,
                                                    min = 1971,
                                                    animate = animationOptions(interval = 333),
                                                    max = 2012,
                                                    step = 1),
                                        helpText("Click the play button to animate"),
                                        width=3),
                                   column(
                                        selectInput(inputId = "tab4country1", label = "Country 1:",
                                                    choices = as.vector(unique(tidy_pop_energy$COUNTRY_NAME)),
                                                    selected = as.vector(unique(tidy_pop_energy$COUNTRY_NAME))[208]), width=3),
                                   column(
                                        selectInput(inputId = "tab4country2", label = "Country 2:",
                                                    choices = as.vector(unique(tidy_pop_energy$COUNTRY_NAME)),
                                                    selected = as.vector(unique(tidy_pop_energy$COUNTRY_NAME))[42]), width=3)
                              )
                         ))
     )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
     my_indicator <- reactive({input$indicator})
     my_year <- reactive({input$year})
     fill_selection <- reactive({input$category})
     
     rank_range <- reactive({input$rank_slider})
     
     # Fix the bar colors by category so that they are persistent regardless of selection.
     bar_colors <- c(`East Asia & Pacific` = "#F8766D",
                     `Latin America & Caribbean` = "#C49A00",
                     `North America` = "#53B400",
                     `Sub-Saharan Africa` = "#00C094",
                     `Europe & Central Asia` = "#00B6EB",
                     `Middle East & North Africa` = "#A58AFF",
                     `South Asia` = "#FB61D7",
                     `High income` = "#238b45",
                     `Upper middle income` = "#66c2a4",
                     `Lower middle income` = "#b2e2e2",
                     `Low income` = "#edf8fb",
                     `Population growth (annual %)` = "#999999",
                     `Total Energy use` = "#999999",
                     `Population, total` = "#999999")
     
     tidy_barplot <- tidy_pop_energy
     
     tidy_barplot <- tidy_barplot %>% select(COUNTRY_NAME, YEAR, COUNTRY_CODE,
                                             REGION, INCOMEGROUP, INDICATOR_NAME,
                                             INDICATOR_CODE, VALUE) %>%
          filter(INDICATOR_NAME != "Energy use (kg of oil equivalent per capita)")
     
     tidy_barplot_energy_tot <- scatter_data %>%
          select(COUNTRY_NAME, YEAR, COUNTRY_CODE,
                 REGION, INCOMEGROUP, INDICATOR_NAME,
                 INDICATOR_CODE, energy_tot) %>%
          mutate(INDICATOR_NAME = "Total Energy use") %>%
          mutate(INDICATOR_CODE = "EG.USE.TOTAL")
     
     setnames(tidy_barplot_energy_tot, "energy_tot", "VALUE")
     
     # tidy_barplot_energy_tot <- tidy_barplot_energy_tot %>% mutate(VALUE = (VALUE * 1000000))
     
     tidy_barplot_energy_tot <- filter(tidy_barplot_energy_tot,
                                       !is.na(VALUE))
     
     tidy_barplot <- rbind(tidy_barplot, tidy_barplot_energy_tot)
     
     
     # tidy_subset_barplot <- reactive({tidy_pop_energy %>%
     tidy_subset_barplot <- reactive({tidy_barplot %>%
               filter(INDICATOR_NAME == my_indicator() & YEAR == my_year()) %>%
               na.omit() %>%
               arrange(VALUE) %>%
               mutate(INCOMEGROUP = factor(INCOMEGROUP, levels = c("High income", "Upper middle income",
                                                                   "Lower middle income", "Low income"))) %>%
               mutate(value_percent_rank = percent_rank(VALUE)) %>%
               mutate(value_percent_rank = value_percent_rank - .0000001) %>%
               filter(value_percent_rank >= rank_range()[1] & value_percent_rank <= rank_range()[2])
          
     })
     
     output$ds_barplot <- renderPlot({
          ggplot(data = tidy_subset_barplot(),
                 aes(x = reorder(COUNTRY_CODE, VALUE), y = VALUE,
                     fill = get(fill_selection()))) +
               geom_col() + xlab("Country") + ylab(my_indicator()) +
               scale_fill_manual(values = bar_colors, name = "Categories")
          
     })
     
     # Tab 3Energy vs. Population
     # Reform data
     icodes = unique(tidy_pop_energy$INDICATOR_CODE)
     scatter_data <- tidy_pop_energy[tidy_pop_energy$INDICATOR_CODE == icodes[1],]
     scatter_data$pop_growth <- as.numeric(tidy_pop_energy$VALUE[tidy_pop_energy$INDICATOR_CODE == icodes[1]])
     scatter_data$pop_tot <- as.numeric(tidy_pop_energy$VALUE[tidy_pop_energy$INDICATOR_CODE == icodes[2]])/1000000
     scatter_data$energy_percap <- as.numeric(tidy_pop_energy$VALUE[tidy_pop_energy$INDICATOR_CODE == icodes[3]])
     scatter_data$energy_tot <- (scatter_data$energy_percap * scatter_data$pop_tot) / 1000
     scatter_data <- scatter_data[complete.cases(scatter_data), ]
     
     # Most energy using countries
     energy2012 <- scatter_data$energy_tot[scatter_data$YEAR == "2010"]
     energy2012[is.na(energy2012)] <- 0
     energy2012_total <- sum(energy2012)
     sum(sort(energy2012, decreasing=TRUE)[0:2])/energy2012_total # top 2
     
     # Config sliders
     year_scatter <- reactive({input$year_scatter})
     pop_max <- reactive({input$pop_max})
     energy_max <- reactive({input$energy_max})
     
     # Config data
     tidy_subset <- reactive({scatter_data %>%
               filter(YEAR == year_scatter())
     })
     
     # Make plot
     output$ds_scatter <- renderPlot({
          ggplot(data = tidy_subset(), aes(x=pop_tot, y=energy_tot, col=REGION)) +
               geom_point(size=5) + labs(title="Population vs. Energy Use",
                                         x="Population (millions)",
                                         y="Energy use (teragrams of oil equivelent)") +
               coord_cartesian(xlim=c(0, pop_max()), ylim=c(0, energy_max())) +
               scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma) +
               geom_text(aes(label=COUNTRY_NAME),hjust=-0.2, vjust=0)
     })
     
     #ggplot(scatter_data, aes(pop_growth, energy_tot)) + geom_point(size=scatter_data$pop_tot) + labs(title="Population Growth vs. Energy Use")
     
     # Tab 1
     #create the dropdown
     tab1_countries <- reactive({input$tab1country})
     
     # Population Total
     tab1_total <- tidy_pop_energy_for_tab_1
     tab1_total_reactive <- reactive({filter(tab1_total, tab1_total$COUNTRY_NAME == tab1_countries())})
     
     # render graph
     output$countryhist <- renderPlot({
          ggplot(data=tab1_total_reactive(), aes(x=YEAR, y=VALUE, ymin=0)) +
               geom_point() + geom_line() +
               facet_wrap(~INDICATOR_NAME, ncol=2, scale="free") +
               labs(title="Indicators by Country", x="Year") +
               theme(strip.text = element_text(size=15), title = element_text(size=20, face='bold'))
     })
     
     # Tab 4: Country Races
     # Config sliders
     race_year_set <- reactive({input$raceYear})
     pop_max <- reactive({input$pop_max})
     energy_max <- reactive({input$energy_max})
     
     # Config data
     tab4_countries <- reactive({c(input$tab4country1, input$tab4country2)})
     tab4_pop_max <- reactive({max(filter(scatter_data, COUNTRY_NAME %in%
                                               tab4_countries())$pop_tot)})
     tab4_energy_max <- reactive({max(filter(scatter_data, COUNTRY_NAME %in% 
                                                  tab4_countries())$energy_tot)})
     tab4_data <- reactive({filter(scatter_data, COUNTRY_NAME %in% tab4_countries()) %>% 
               filter(YEAR > 1970 & YEAR <= race_year_set())})
     
     output$countryRace <- renderPlot({
          ggplot(data = tab4_data(), aes(x=pop_tot, y=energy_tot, col=COUNTRY_NAME)) +
               geom_point(size=5) + labs(title="Population vs. Energy Use",
                                         x="Population (millions)",
                                         y="Energy use (teragrams of oil equivelent)") +
               coord_cartesian(xlim=c(0, tab4_pop_max()), ylim=c(0, tab4_energy_max())) +
               scale_x_continuous(labels = scales::comma) + scale_y_continuous(labels = scales::comma)
     })
     
}

# Run the application 
shinyApp(ui = ui, server = server)


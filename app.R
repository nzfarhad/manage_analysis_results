#
library(shiny)
library(dplyr)
library(tidyr)
library(readxl)

# read indicator list, two columns ('indicator' and 'indicator_lable')
indicator_list <- read_excel("data/indicator_list.xlsx")

# Define UI 
ui <- fluidPage(

    # Application title
    titlePanel("AFG MSNA 2020 Analysis"),

    # Sidebar
    sidebarLayout(
        sidebarPanel( width = 6,
            # Select Input for analysis datasets
            selectInput("dataSelection", label = "Select the analysis file", 
                        choices = c("Displaced (IDP - Returnee)", 
                                    "National All Pop groups (IDP, Returnee, Refugee, Vulnerable)",
                                     "Vulnerable",
                                      "Displaced Non-HoH")),
            # dynamic select input
            uiOutput('variables'),
            
            # clear selection
            actionButton("uncheck","Clear Selection") ,
            
            # Download buttons
            downloadButton("downloadData", "Download Selected"),
            downloadButton("downloadData_all", "Download All Indicators"),
            
            

            # Indicator list panel
            wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
            # List of indicator names          
            checkboxGroupInput("show_vars", "Indicator List", choiceValues = indicator_list$indicator,
                           choiceNames =  indicator_list$indicator_lable)
            )
            
        ),

        # Main panel
        mainPanel( width = 6,
           tableOutput("indicators")
        ),
        
    )
)

# Sever
server <- function(input, output, session) {
    
    
    # clear Selection
    observeEvent(input$uncheck,{
        updateCheckboxGroupInput(session,"show_vars","Indicator List", choiceValues = indicator_list$indicator, choiceNames = indicator_list$indicator_lable)
    })
        
    
    # Analysis Datasets
    displaced <- read_excel("data/analysis_results_MSNA_displaced_all_disaggs_final_2020-09-26.xlsx")
    national <- read_excel("data/analysis_results_MSNA_national_all_pop_groups_2020-09-26.xlsx")
    vuln <- read_excel("data/analysis_results_MSNA_vulnerable_national_district_2020-09-26.xlsx")
    non_hoh <- read_excel("data/analysis_results_MSNA_displaced_non_hoh_2020-09-29.xlsx")
    avg_percentage <- read_excel("data/ave_percentage.xlsx")
    
    # Switch between analysis datasets
    dataSource <- reactive({
        switch(input$dataSelection,"Displaced (IDP - Returnee)" = displaced,
                                    "National All Pop groups (IDP, Returnee, Refugee, Vulnerable)" = national,
                                    "Vulnerable" = vuln,
                                     "Displaced Non-HoH" = non_hoh)
    })
    # render Disaggregation Select
    output$variables = renderUI({
        selectInput('disagg', 'Select disaggregation', unique(dataSource()$yi))
    })
    
    #### Reshape analysis files
    # Clean variable names
    data_sub <- reactive({
        results_clean <- dataSource() %>% select(
            indicator = xi,
            response = variable,
            disaggregation = yi,
            disaggregation_lable = yi_lab,
            disaggregation_variable_short = rowname,
            disaggregation_variable = rowname_label,
            aggregation_type,
            value,
            confidence_interva_low = CI_lw,
            confidence_interva_up = CI_up,
            count,
            valid_n,
            rank,
            indicator_lable = xi_lab,
            response_label = variable_label
        )
        
        # Change aggregation type where necessary
        results_clean <- results_clean %>% left_join(avg_percentage, by = "indicator")
        results_clean <- results_clean %>% mutate(
            aggregation_type = case_when(
                !is.na(type) ~ type,
                TRUE ~ aggregation_type
            )
        ) %>% select(-c(type))
        
        # Handle missing labels and round values
        results_clean <- results_clean %>% mutate(
            disaggregation_variable = case_when(
                is.na(disaggregation_variable) ~ disaggregation_variable_short, 
                TRUE ~ disaggregation_variable
            ),
            indicator_lable = case_when(
                is.na(indicator_lable) ~ indicator,
                TRUE ~ indicator_lable
            ),
            response_label = case_when(
                is.na(response_label) ~ response,
                TRUE ~ response_label
            ),
            disaggregation_lable = case_when(
                is.na(disaggregation_lable) ~ disaggregation,
                TRUE ~ disaggregation_lable
            ),
            value = round(value,2)
        ) 
        # subset variables
        subset <- filter(results_clean, indicator %in% input$show_vars)  %>% select(
            indicator_lable,
            aggregation_type,
            response_label,
            disaggregation_variable,
            disaggregation_lable,
            disaggregation,
            value,
            indicator,
            response
        )
        
        # filter disaggregation
        subset_2 <- subset %>% filter(disaggregation == input$disagg) %>% select(
            indicator_lable,
            response_label,
            disaggregation_lable,
            aggregation_type,
            value,
            disaggregation_variable,
            indicator,
            response
        )
        
        # Subset - for downloading all variables
        subset_all <- results_clean %>% select(
            indicator_lable,
            aggregation_type,
            response_label,
            disaggregation_variable,
            disaggregation_lable,
            disaggregation,
            value,
            indicator,
            response
        )
        # Subset disaggregation - for downloading all variables
        subset_2_all <- subset_all %>% filter(disaggregation == input$disagg) %>% select(
            indicator_lable,
            response_label,
            disaggregation_lable,
            aggregation_type,
            value,
            disaggregation_variable,
            indicator,
            response
        )
        
        # Reshape subset
        wide <- pivot_wider(subset_2, 
                            id_cols = c("indicator","response", "indicator_lable", "response_label",  "disaggregation_lable", "aggregation_type"),
                            values_from = "value",
                            names_from = c("disaggregation_variable"))
        # Reshape all indicators
        wide2 <- pivot_wider(subset_2_all, 
                            id_cols = c("indicator","response", "indicator_lable", "response_label",  "disaggregation_lable", "aggregation_type"),
                            values_from = "value",
                            names_from = c("disaggregation_variable"))
        
        
        return(list(subs = wide, all_vars = wide2))
    })
    
    # Show selected indicator names
    output$indicators <- renderTable({
        
        unique(select(data_sub()$subs, `Selected Indicators` = indicator_lable ))
        
        })
    
    # Download subset
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$dataSelection,"_", input$disagg,  "_subset.csv", sep = "")
        },
        content = function(file) {
            write.csv(data_sub()$subs, file, row.names = FALSE)
        }
    )
    # Download all variables
    output$downloadData_all <- downloadHandler(
        filename = function() {
            paste(input$dataSelection,"_", input$disagg,  "_All.csv", sep = "")
        },
        content = function(file) {
            write.csv(data_sub()$all_vars, file, row.names = FALSE)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)

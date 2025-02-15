library(shiny)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(jsonlite)

# Read the dataset containing country profile data
Country_Profile_data <- read.csv("hbc_df2_profilevars.csv")

# Define the User Interface (UI) for the Shiny app
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .custom-border {
        border: 2px solid #ADD8E6;
        padding: 10px;
        border-radius: 5px;
        margin-bottom: 10px;
      }
      .custom-text {
        color: #333;
        font-size: 20px;
        margin-top: 20px;
        padding: 10px;
        border: 2px solid #ADD8E6;
        border-radius: 5px;
      }
      .overview-section {
        display: flex;
        justify-content: space-between;
      }
      .overview-section > div {
        width: 45%;
      }
    "))
  ),
  titlePanel("TB Profile"),
  sidebarLayout(
    sidebarPanel(
      div(class = "custom-border",
          selectInput("type", "Select Type:", choices = c("Country", "Group")),
          uiOutput("itemSelect"),
          h2(textOutput("profileTitle"), class = "custom-text"),
          p(textOutput("population"), style = "font-size: 14px; color: #333; margin-top: 10px;")
      )
    ),
    mainPanel(
      div(class = "overview-section",
          div(
            h2("TB Overview"),
            uiOutput("tbNotificationsOverview"),
            uiOutput("tbEstimatesOverview")
          ),
          div(
            h2("Prison Overview"),
            uiOutput("incarcerationOverview"),
            uiOutput("incarcerationRateOverview"),
            uiOutput("tbIncidencePrisonOverview")
          ),
          div(
            h2("TB Guidelines"),
            uiOutput("tbGuidelinesOverview")
          ),
          div(
            h2("TB Management in Prisons"),
            uiOutput("dataManagementOverview")  # Data Management with role variable
          )
      )
    )
  )
)

server <- function(input, output) {
  reactive_data <- reactive({
    if (input$type == "Country") {
      unique(Country_Profile_data$country)
    } else if (input$type == "Group") {
      unique(Country_Profile_data$who_region)
    } else {
      return(NULL)
    }
  })
  
  output$itemSelect <- renderUI({
    div(class = "custom-border",
        selectInput("item", "Select Item:", choices = reactive_data())
    )
  })
  
  output$profileTitle <- renderText({
    req(input$item)
    paste("Tuberculosis Profile:", input$item)
  })
  
  filtered_data <- reactive({
    req(input$item)
    Country_Profile_data %>%
      filter((input$type == "Country" & country == input$item) |
               (input$type == "Group" & who_region == input$item))
  })
  
  output$population <- renderText({
    req(input$item)
    data <- filtered_data()
    if (input$type == "Country") {
      if (nrow(data) > 0) {
        population <- data %>% pull(pop) %>% sum(na.rm = TRUE)
        paste("Population of", input$item, ":", format(population, big.mark = ","))
      } else {
        paste("Population data not available for", input$item)
      }
    } else if (input$type == "Group") {
      if (nrow(data) > 0) {
        population <- data %>% group_by(who_region) %>% summarize(population = sum(pop, na.rm = TRUE))
        paste("Total Population in", input$item, ":", format(population$population[1], big.mark = ","))
      } else {
        paste("Population data not available for", input$item)
      }
    }
  })
  
  output$tbNotificationsOverview <- renderUI({
    req(input$item)
    data <- filtered_data() %>%
      select(country, c_newinc_100k) %>%
      summarize(`National TB case notifications per 100,000 people:` = paste(c_newinc_100k, sep = " "))
    
    tagList(
      p("National TB case notifications per 100,000 people:"),
      p(data[[1]])
    )
  })
  
  output$tbEstimatesOverview <- renderUI({
    req(input$item)
    data <- filtered_data() %>%
      select(country, e_inc_100k, e_inc_100k_lo, e_inc_100k_hi) %>%
      summarize(`National estimated TB incidence per 100,000 people:` = paste(e_inc_100k, "(", e_inc_100k_lo, "-", e_inc_100k_hi, ")", sep = " "))
    
    tagList(
      p("National estimated TB incidence per 100,000 people:"),
      p(data[[1]])
    )
  })
  
  output$incarcerationOverview <- renderUI({
    req(input$item)
    data <- filtered_data()
    if (input$type == "Country") {
      if (nrow(data) > 0) {
        prison_pop <- data %>% pull(prison_pop) %>% sum(na.rm = TRUE)
        tagList(
          p("Incarcerated population:"),
          p(format(prison_pop, big.mark = ","))
        )
      } else {
        tagList(
          p("Incarcerated population data not available for", input$item)
        )
      }
    } else if (input$type == "Group") {
      if (nrow(data) > 0) {
        prison_pop <- data %>% group_by(who_region) %>% summarize(prison_pop = sum(prison_pop, na.rm = TRUE))
        tagList(
          p("Total Incarcerated Population in", input$item, ":"),
          p(format(prison_pop$prison_pop[1], big.mark = ","))
        )
      } else {
        tagList(
          p("Incarcerated population data not available for", input$item)
        )
      }
    }
  })
  
  output$incarcerationRateOverview <- renderUI({
    req(input$item)
    data <- filtered_data()
    if (input$type == "Country") {
      if (nrow(data) > 0) {
        incarceration_rate <- data %>% pull(prison_pop_rate100k) %>% sum(na.rm = TRUE)
        tagList(
          p("Incarceration rate (number of people in prison per 100,000 national population):"),
          p(format(incarceration_rate, big.mark = ","))
        )
      } else {
        tagList(
          p("Incarceration rate data not available for", input$item)
        )
      }
    } else if (input$type == "Group") {
      if (nrow(data) > 0) {
        incarceration_rate <- data %>% group_by(who_region) %>% summarize(prison_pop_rate100k = sum(prison_pop_rate100k, na.rm = TRUE))
        tagList(
          p("Total Incarceration Rate in", input$item, ":"),
          p(format(incarceration_rate$prison_pop_rate100k[1], big.mark = ","))
        )
      } else {
        tagList(
          p("Incarceration rate data not available for", input$item)
        )
      }
    }
  })
  
  output$tbIncidencePrisonOverview <- renderUI({
    req(input$item)
    data <- filtered_data()
    
    if (input$type == "Country") {
      if (nrow(data) > 0) {
        tb_inc_estimate_prisons100k <- data %>% pull(TB_inc_estimate_prisons100k) %>% sum(na.rm = TRUE)
        tb_inc_estimate_prisons100k_LCI <- data %>% pull(TB_inc_estimate_prisons100k_LCI) %>% sum(na.rm = TRUE)
        tb_inc_estimate_prisons100k_UCI <- data %>% pull(TB_inc_estimate_prisons100k_UCI) %>% sum(na.rm = TRUE)
        
        tagList(
          p("Estimated TB incidence in prison, per 100,000 people incarcerated:"),
          p(paste(tb_inc_estimate_prisons100k, "(", tb_inc_estimate_prisons100k_LCI, "-", tb_inc_estimate_prisons100k_UCI, ")", sep = " "))
        )
      } else {
        tagList(
          p("Estimated TB incidence in prison data not available for", input$item)
        )
      }
    } else if (input$type == "Group") {
      if (nrow(data) > 0) {
        tb_inc_estimate_prisons100k <- data %>%
          group_by(who_region) %>%
          summarize(tb_inc_estimate_prisons100k = sum(TB_inc_estimate_prisons100k, na.rm = TRUE)) %>%
          pull(tb_inc_estimate_prisons100k)
        
        tb_inc_estimate_prisons100k_LCI <- data %>%
          group_by(who_region) %>%
          summarize(tb_inc_estimate_prisons100k_LCI = sum(TB_inc_estimate_prisons100k_LCI, na.rm = TRUE)) %>%
          pull(tb_inc_estimate_prisons100k_LCI)
        
        tb_inc_estimate_prisons100k_UCI <- data %>%
          group_by(who_region) %>%
          summarize(tb_inc_estimate_prisons100k_UCI = sum(TB_inc_estimate_prisons100k_UCI, na.rm = TRUE)) %>%
          pull(tb_inc_estimate_prisons100k_UCI)
        
        tagList(
          p("Estimated TB incidence in prison, per 100,000 people incarcerated:"),
          p(paste(tb_inc_estimate_prisons100k[1], "(", tb_inc_estimate_prisons100k_LCI[1], "-", tb_inc_estimate_prisons100k_UCI[1], ")", sep = " "))
        )
      } else {
        tagList(
          p("Estimated TB incidence in prison data not available for", input$item)
        )
      }
    }
  })
  
  output$tbGuidelinesOverview <- renderUI({
    req(input$item)
    
    data <- Country_Profile_data %>% filter(country == input$item)
    
    if (nrow(data) > 0) {
      guidelines <- c(
        "Lack of national TB guidelines specific to TB management in prison settings" = data$barriers_guidelines_lack,
        "Lack of prioritization of incarcerated population in overall TB elimination efforts" = data$barriers_guidelines_priorities,
        "Fragmentation of healthcare delivery in prisons due to being under the mandate of the ministry of defense or justice" = data$barriers_guidelines_fragment,
        "Lack of healthcare staff in prisons" = data$barriers_guidelines_staffing,
        "Lack of funding" = data$barriers_guidelines_funding,
        "Lack of healthcare infrastructure in prisons" = data$barriers_guidelines_infra,
        "Logistical challenges related to prison environment (e.g., security)" = data$barriers_guidelines_logistic,
        "Prison overcrowding" = data$barriers_guidelines_crowding,
        "Lack of supplies (equipment, tests, PPE for staff, etc.)" = data$barriers_guidelines_supplies,
        "I don't know" = data$barriers_guidelines_idk,
        "Other (Please specify)" = data$barriers_guidelines_other
      )
      
      tagList(
        lapply(names(guidelines), function(guideline) {
          value <- ifelse(guidelines[[guideline]] == 1, "Yes", "No")
          p(paste(guideline, ":", value))
        })
      )
    } else {
      tagList(
        p("Guideline data not available for", input$item)
      )
    }
  })
  
  output$dataManagementOverview <- renderUI({
    req(input$item)
    
    # Use the "role" variable for the "Data Management" section
    data <- filtered_data() %>% select(role) %>% summarize(role = paste(role, collapse = ", "))
    
    tagList(
      p(strong("Role/Position(s):"), data[[1]])  # Displaying the "role" variable in bold
    )
  })
}

shinyApp(ui = ui, server = server)
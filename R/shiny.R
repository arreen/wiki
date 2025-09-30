library(shiny)
library(jsonlite)

ui <- fluidPage(
  titlePanel("Justering av PurpleAir mätvärden"),
  fluidRow(p("")),
  fluidRow(p("")),

  # Styling
  tags$head(
    tags$style(HTML("
      .scrollable-table {
        max-height: 500px;
        overflow-y: auto;
        border: 1px solid #ccc;
        padding: 10px;
        background-color: #f9f9f9;
      }
      .well-section {
        margin-bottom: 20px;
      }
    "))
  ),

  fluidRow(
    column(4,
           div(class = "well-section",
               selectInput(inputId = "lan", label = "Choose a language",
                           choices = c("Polish", "English", "Swedish"))
           ), column(10,
                     textOutput("related"),
           )
    ),
    column(4,
           h4("Justerade värden"),
           div(class = "scrollable-table",
               textOutput("predictions")
           )
    )
  )

  )


server <- function(input, output) {

  api_data <- reactive({api_wiki_data(current_article = "Smoltification")})

  abstract <- reactive({

  langs <- c(pol = "Polish", en = "English", swe = "Swedish")

  parse_data(api_data(), lan = names(langs)[which(input$lan == langs)])$abstract_text

  })

  related <- reactive({

    langs <- c(pol = "Polish", en = "English", swe = "Swedish")

    parse_data(api_data(), lan = names(langs)[which(input$lan == langs)])$related_topics

  })

  output$downloadData <- downloadHandler(
    filename = "PurpleAir data - justerad.csv",
    content = function(file) {
      write.csv(final_df(), file, row.names = FALSE)
    }
  )

  output$predictions <- renderText({
    abstract()
  })

  output$related <- renderText({
    related()
  })

}



# renv::init()
# renv::snapshot()

#rsconnect::deployApp("C:/Users/arone/Desktop/Kandidat/Kalmar/Shinyfiles", appPrimaryDoc = "PurpleAir.R")
#shiny::runApp("C:/Users/arone/Desktop/Kandidat/Kalmar/Shinyfiles/PurpleAir.R")

shinyApp(ui, server)

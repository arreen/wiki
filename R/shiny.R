library(shiny)

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
               selectInput(inputId = "Tidsupplosning", label = "Välj tidsupplösning",
                           choices = c("30-minuters medelvärden", "Dygnsmedelvärden"))
           ),
           div(class = "well-section",
               fileInput("file", "Ladda upp API-data från PurpleAir")
           ),
           div(class = "well-section",
               downloadButton("downloadData", "Ladda ned justerade värden")
           ), column(10,
                     p("Efter du laddat upp datamaterialet, vänta 30-60 sekunder på att justeringen utförs. Du måste ladda upp minst 7 dagar. De första 2 dagarna av ditt data kommer tas bort, det behöver göras för att kunna justera värdena"),
           )
    ),
    column(4,
           h4("Justerade värden"),
           div(class = "scrollable-table",
               tableOutput("predictions")
           )
    )
  ),
  fluidRow(
    column(12, align = "center",
           br(),
           a(href = "PurpleAir_API_Guide.pdf", target = "_blank", "Kort guide för att ladda ned data från PurpleAirs databas via API:n")
    )
  )
)

server <- function(input, output) {
  final_df <- reactive({
  iris

  })

  output$downloadData <- downloadHandler(
    filename = "PurpleAir data - justerad.csv",
    content = function(file) {
      write.csv(final_df(), file, row.names = FALSE)
    }
  )

  output$predictions <- renderTable({
    final_df()
  })

}



# renv::init()
# renv::snapshot()

#rsconnect::deployApp("C:/Users/arone/Desktop/Kandidat/Kalmar/Shinyfiles", appPrimaryDoc = "PurpleAir.R")
#shiny::runApp("C:/Users/arone/Desktop/Kandidat/Kalmar/Shinyfiles/PurpleAir.R")

shinyApp(ui, server)

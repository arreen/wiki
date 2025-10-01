library(shiny)
library(jsonlite)
library(memoise)
library(stringr)

# memoise cachar funktionen körd med samma värden memoise()
api_wiki_data <- function(current_article = "Smoltification") {

  db <- "http://dbpedia.org/resource/"
  current_article <- current_article
  link <- paste0(db, current_article)

  data <- list(json = fromJSON(paste0("http://dbpedia.org/data/", current_article, ".json")), link = link)



  return(data)
}

parse_data <- function(api_data, lan = "en"){
  data <- api_data$json
  information <- data[[api_data$link]]
  abstract <- information[[which(str_detect(names(information), "abstract")) ]]
  if (lan %in% abstract$lang) {
    abstract_text <- abstract$value[abstract$lang == lan]
  } else {

    langs <- c(pl = "Polish", en = "English", sv = "Swedish")
    abstract_text <- paste0("This article does not exist in ", langs[lan], ".")

  }


  related_topics <- str_remove(information[[which(str_detect(names(information), "wikiPageWikiLink")) ]]$value, "http://dbpedia.org/resource/")

  return(list("related_topics" = related_topics, "abstract_text" = abstract_text))

}

ui <- fluidPage(
  titlePanel("Fewest clicks to Poland"),
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
                     uiOutput("dynamic_buttons"),
           )
    ),
    column(4,
           h4("Abstract"),
           div(class = "scrollable-table",
               textOutput("predictions")
           )
    )
  )

  )


server <- function(input, output) {

  current_article <- reactiveVal("Blood")

  api_data <- reactive({api_wiki_data( current_article = current_article() )})



  abstract <- reactive({

  langs <- c(pl = "Polish", en = "English", sv = "Swedish")

  parse_data(api_data(), lan = names(langs)[which(input$lan == langs)])$abstract_text

  })

  related <- reactive({

    langs <- c(pl = "Polish", en = "English", sv = "Swedish")
    # Only takes the 15 first topics
    parse_data(api_data(), lan = names(langs)[which(input$lan == langs)])$related_topics[1:15]

  })

  output$predictions <- renderText({
    abstract()
  })

  output$related <- renderText({
    related()
  })

  output$dynamic_buttons <- renderUI({

    tagList(
    lapply(seq_along(related()), function(i) {
      actionButton(
        inputId = paste0("btn_", i ),
        label = related()[i]
      )
    })
    )
  })

  observeEvent(input$dynamic_buttons, { }, ignoreInit = TRUE)

  observe(
    lapply(seq_along(related()), function(i) {
      btn_id <- paste0("btn_", i)
      observeEvent(input[[btn_id]], {
        current_article(related()[i])

      })
    })
  )



}


shinyApp(ui, server,options = list(test.mode = TRUE))

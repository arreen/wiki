library(stringr)

#' @title
#' api_wiki_data
#' @description
#' This function uses the dbpedia.org api to get a specific article and returns it as a .json file.
#' Note: Keep in mind that dbpedia.org does not contain every article on wikipedia. https://dbpedia.org/ is also
#' case sensitive, so "Helen_Abbey" must be written as "Helen_Abbey" and not "helen_abbey", "Helen_abbey" or any other
#' variant.
#'
#' @param current_article The article that should return a .json.
#'
#' @returns a .json file of the article
#' @export
#'
#' @examples api_wiki_data(current_article = "Helen_Abbey")

api_wiki_data <- memoise::memoise(function(current_article = "Smoltification") {



  sparql_query_abstract <- paste0("PREFIX dbo: <http://dbpedia.org/ontology/>
                                  SELECT ?abstract
                                WHERE {
                                  <http://dbpedia.org/resource/", current_article, "> dbo:abstract ?abstract

                                  FILTER (lang(?abstract) IN ('en', 'sv', 'pl'))
                                }")

  sparsql_query_topics <- paste0("PREFIX dbo: <http://dbpedia.org/ontology/>
                                 SELECT ?link
                               WHERE {
                                 <http://dbpedia.org/resource/", current_article, "> dbo:wikiPageWikiLink ?link .
                               }
                                 LIMIT 15")

  resp <- request("https://dbpedia.org/sparql") %>%
    req_url_query(
      query  = sparql_query_abstract,
      format = "application/sparql-results+json"
    ) %>%
    req_perform()

  body <- resp_body_json(resp)
  abstract <- body[["results"]][["bindings"]]

  resp <- request("https://dbpedia.org/sparql") %>%
    req_url_query(
      query  = sparsql_query_topics,
      format = "application/sparql-results+json"
    ) %>%
    req_perform()

  # Här sätter man typ return istället för body <- resp_body_json(...)
  non_parsed_links <- resp_body_json(resp)


  data <- list(abstracts = abstract, links = non_parsed_links)
  return(data) # return the output
})

a <- api_wiki_data("Poland")


#' @title parse_data
#' @description
#' parses an output from api_wiki_data
#'
#' @param api_data The output you want to parse
#' @param lan The language you want to print it out in. You can choose betweeen English ("en"), Polish ("pl") and Swedish ("sv").
#'
#' @returns A list with related topics and the article abstract.
#' @export
#'
#' @examples parse_data(api_wiki_data(current_article = "Helen_Abbey"))
#'
#'
parse_data <- function(api_data, lan = "en"){

  body <- api_data$links
  links <- c()
  for (i in 1:length(body[["results"]][["bindings"]])) {
    links <- c(links, body[["results"]][["bindings"]][[i]][["link"]][["value"]])
  }

  related_topics <- str_remove(links, "http://dbpedia.org/resource/")


  # ----------------------------------------

  list("pl" = "This article does not exist in Polish.", "sv" = "This article does not exist in Swedish")
  abstract_text <- ""
    for (i in 1:length(api_data$abstracts)) {

      if (api_data$abstracts[[i]]$abstract$`xml:lang` == lan) {
        abstract_text <- paste(abstract_text, api_data$abstracts[[i]]$abstract$value)
      }
    }
  if (abstract_text == "") {
    langs <- c(pl = "Polish", en = "English", sv = "Swedish")
    abstract_text <- paste0("This article does not exist in ", langs[lan], ".")
  }
  # ----------------------------------------

  return(list("related_topics" = related_topics, "abstract_text" = abstract_text))

}

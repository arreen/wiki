library(httr2)
library(jsonlite)
library(dplyr)


# Test variabler ######

article <- "Helen_Abbey"
article <- "Albert_Einstein"
article <- "Poland"

article <- "Smoltification"

# Query ######

sparql_query_abstract <- paste0("PREFIX dbo: <http://dbpedia.org/ontology/>
                                  SELECT ?abstract
                                WHERE {
                                  <http://dbpedia.org/resource/", article, "> dbo:abstract ?abstract

                                  FILTER (lang(?abstract) IN ('en', 'sv', 'pl'))
                                }")

sparsql_query_topics <- paste0("PREFIX dbo: <http://dbpedia.org/ontology/>
                                 SELECT ?link
                               WHERE {
                                 <http://dbpedia.org/resource/", article, "> dbo:wikiPageWikiLink ?link . LIMIT 15
                               }
                               LIMIT 15")


# Ersättande text av resp ######
resp <- request("https://dbpedia.org/sparql") %>%
  req_url_query(
    query  = sparql_query_abstract,
    format = "application/sparql-results+json"
  ) %>%
  req_perform()

# Här sätter man typ return istället för body <- resp_body_json(...)
body <- resp_body_json(resp)
abstract <- body[["results"]][["bindings"]][[1]][["abstract"]]

# Översätter parse_data
resp <- request("https://dbpedia.org/sparql") %>%
  req_url_query(
    query  = sparsql_query_topics,
    format = "application/sparql-results+json"
  ) %>%
  req_perform()

# Här sätter man typ return istället för body <- resp_body_json(...)
body <- resp_body_json(resp)
links <- c()

for (i in 1:length(body[["results"]][["bindings"]])) {
  links <- c(links, body[["results"]][["bindings"]][[i]][["link"]][["value"]])
}


abstract <- body[["results"]][["bindings"]][[1]][["abstract"]]


# Problem med koden
# 1. Den gillar att göra [truncated] exempelvis på Smoltification.
# 2. Den hittar inte related topics
# 3. Det kan tydlien finnas två abstrakt. Se exemplet "Poland".
# Annars bör denna lösning vara bättre än vår första.

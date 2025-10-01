library(httr2)
library(jsonlite)

# Test variabler ######
article <- "Smoltification"
article <- "Helen_Abbey"
article <- "Albert_Einstein"
article <- "Poland"

# Query ######
sparql_query <- paste0("
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?comment
WHERE {
  <http://dbpedia.org/resource/", article, "> rdfs:comment ?comment .
  FILTER (lang(?comment) IN ('en', 'sv', 'pl'))
}
")

# Ersättande text av resp ######
resp <- request("https://dbpedia.org/sparql") %>%
  req_url_query(
    query  = sparql_query,
    format = "application/sparql-results+json"
  ) %>%
  req_perform()

# Här sätter man typ return istället för body <- resp_body_json(...)
body <- resp_body_json(resp)


# Översätter parse_data
comments <- sapply(X = 1:length(body$results$bindings),
                   FUN = function(x){
                     return(body$results$bindings[[x]]$comment[2:3])
                   },
                   simplify = TRUE)
comments


# Problem med koden
# 1. Den gillar att göra [truncated] exempelvis på Smoltification.
# 2. Den hittar inte related topics
# 3. Det kan tydlien finnas två abstrakt. Se exemplet "Poland".
# Annars bör denna lösning vara bättre än vår första.

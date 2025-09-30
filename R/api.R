library(jsonlite)
library(stringr)


test2 <- fromJSON("http://dbpedia.org/resource/Na+/K+-ATPase")
test <- fromJSON("https://dbpedia.org/data/Smoltification.json")


a <- read_html("https://dbpedia.org/data/Sodium%E2%80%93potassium_pump.json")


api_wiki_data <- function(current_article = "Smoltification") {

  db <- "http://dbpedia.org/resource/"
  current_article <- current_article
    link <- paste0(db, current_article)

  data <- fromJSON(paste0("http://dbpedia.org/data/", current_article, ".json"))

  return(data)
}

parse_data <- function(data, lan = "en"){

  information <- data[[link]]
  abstract <- information[[which(str_detect(names(information), "abstract")) ]]
  if (lan %in% abstract$lang) {
    abstract_text <- abstract$value[abstract$lang == lan]
  } else {

    langs <- c(pol = "Polish", en = "English", swe = "Swedish")
    abstract_text <- paste0("This article does not exist in ", langs[lan], ".")

  }


  related_topics <- str_remove(information[[which(str_detect(names(information), "wikiPageWikiLink")) ]]$value, "http://dbpedia.org/resource/")

  return(list("related_topics" = related_topics, "abstract_text" = abstract_text))

}

a <- api_wiki_data()


"abstract"


test$`http://dbpedia.org/resource/Smoltification`$`http://dbpedia.org/ontology/wikiPageLength`

# SPRÅK
test$`http://dbpedia.org/resource/Smoltification`$`http://dbpedia.org/ontology/abstract`

# Related topics,
test$`http://dbpedia.org/resource/Smoltification`$`http://dbpedia.org/ontology/wikiPageWikiLink`

#Back button Forward button

http://dbpedia.org/ontology/wikiPageWikiLink

test2$´http://dbpedia.org/ontology/wikiPageWikiLink´

tibble(test2)


FASTEST CLICK TO POLAND https://en.wikipedia.org/wiki/Poland
List of 1000 random wiki articles


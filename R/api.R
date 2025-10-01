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

api_wiki_data <- function(current_article = "Smoltification") {
  db <- "http://dbpedia.org/resource/" # all article starts with "http://dbpedia.org/resource/*."
  link <- paste0(db, current_article) # we add the subfolder to the article
  data <- list(json = fromJSON(paste0("http://dbpedia.org/data/", current_article, ".json")), link = link) # the output contains the .json it's link
  if(length(data[["json"]]) == 0){warning("There is no article for that input.\nEither the article does not exist or you have written the article wrong\nNote: current_article is hard case sensitive")}
  return(data) # return the output
}

#' @title parse_data
#' @description
#' parses an output from api_wiki_data
#'
#' @param api_data The output you want to parse
#' @param lan The language you want to print it out in. You can choose betweeen English, Polish and Swedish.
#'
#' @returns A list with related topics and the article abstract.
#' @export
#'
#' @examples parse_data(api_wiki_data(current_article = "Helen_Abbey"))
#'
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

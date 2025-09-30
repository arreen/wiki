
library(jsonlite)

test <- fromJSON("https://dbpedia.org/data/Smoltification.json")
test2 <- fromJSON("https://dbpedia.org/data/Fresh_water.json")

test$`http://dbpedia.org/resource/Smoltification`$`http://dbpedia.org/ontology/wikiPageLength`

# SPRÅK
test$`http://dbpedia.org/resource/Smoltification`$`http://dbpedia.org/ontology/abstract`

# Related topics,
test$`http://dbpedia.org/resource/Smoltification`$`http://dbpedia.org/ontology/wikiPageWikiLink`

http://dbpedia.org/ontology/wikiPageWikiLink

test2$´http://dbpedia.org/ontology/wikiPageWikiLink´

tibble(test2)

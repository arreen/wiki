# Pre-test preperations #####
data("women_in_statistics")

test_sample <- c(60, 707, 146) # These are taken from a sample with the seed 6765
# set.seed(seed = 6765)
# test_sample <- sample(which(women_in_statistics$can_be_accessed), 3)

# Tests for api_wiki_data ####
context("api_wiki_data")

test_that("Can it run correctly",{
  # This test does not work, don't know why though, but it fails at requests :(
  expect_length(api_wiki_data("Smoltification"), 2)
  expect_length(api_wiki_data(women_in_statistics[test_sample[1], 1]), 2)
  expect_length(api_wiki_data(women_in_statistics[test_sample[2], 1]), 2)
  expect_length(api_wiki_data(women_in_statistics[test_sample[3], 1]), 2)
  expect_length(api_wiki_data("Sigismund_III_Vasa"), 2)
})

test_that("Can it get an abstract",{
  expect_equal("Smoltification (also known as Parr-Smolt transformation) is a complex series of physiological changes where young salmonid fish adapt from living in fresh water to living in seawater. Physiological changes during smoltification include modified body shape, increased skin reflectance (the measure of the proportion of light or other radiation striking a surface which is reflected off it.), and increased Na+/K+-ATPase in the gills. A number of mechanisms assist with osmoregulation." %in% unlist(api_wiki_data("Smoltification")), TRUE)
  expect_equal("Lynne Billard (born 1943) is an Australian statistician and professor at the University of Georgia, known for her statistics research, leadership, and advocacy for women in science. She has served as president of the American Statistical Association, and the International Biometric Society, one of a handful of people to have led both organizations." %in% unlist(api_wiki_data(women_in_statistics[test_sample[1], 1])), TRUE)
  expect_equal("Grace Yun Yi is a professor of the University of Western Ontario where she currently holds a Tier I Canada Research Chair in Data Science. She was a professor at the University of Waterloo, Canada, where she holds a University Research Chair in Statistical and Actuarial Science. Her research concerns event history analysis with missing data and its applications in medicine, engineering, and social science." %in% unlist(api_wiki_data(women_in_statistics[test_sample[2], 1])), TRUE)
  expect_equal("Susmita Datta is an Indian biostatistician. She is a professor of biostatistics at the University of Florida, and is the former president of the Caucus for Women in Statistics. She is also a musician who has published three CDs of Bengali folk songs." %in% unlist(api_wiki_data(women_in_statistics[test_sample[3], 1])), TRUE)
  expect_equal("The Konik or Polish Konik, Polish: konik polski, is a Polish breed of pony. There are semi-feral populations in some regions. They are usually mouse dun or striped dun in color. The Bilgoray, Polish: konik biłgorajski, of south-eastern Poland is a sub-type of the breed influenced by Arab and Thoroughbred blood; it is close to extinction. The extinct Sweyki or Schweike sub-type of East Prussia contributed to the development of the Trakehner. The word \"konik\" means 'small horse'. It may be used in a wider sense to describe the Polish Konik and other similar breeds, among them the Hutsul of the Carpathian Mountains, the of Belarus and the Žemaitukas of Lithuania." %in% unlist(api_wiki_data("Konik")), TRUE)
})

test_that("Can get a reference",{
  expect_equal("http://dbpedia.org/resource/Seawater" %in% unlist(api_wiki_data("Smoltification")))
  expect_output("http://dbpedia.org/resource/Naval_Postgraduate_School" %in% unlist(api_wiki_data(women_in_statistics[test_sample[1], 1])))
  expect_output("http://dbpedia.org/resource/Sichuan_University" %in% unlist(api_wiki_data(women_in_statistics[test_sample[2], 1])))
  expect_output("http://dbpedia.org/resource/Metabolomics" %in% unlist(api_wiki_data(women_in_statistics[test_sample[3], 1])))
  expect_output("http://dbpedia.org/resource/Carpathian_Mountains" %in% unlist(api_wiki_data("Konik")), TRUE)
})

test_that("Will it reject non string inputs?",{
  expect_error(api_wiki_data(1))
  expect_error(api_wiki_data(NA))
  expect_error(api_wiki_data(TRUE))
})

test_that("Will it reject inputs without articles", {expect_error(api_wiki_data("Krzysztof Bartoszek"))})

# Tests for parse_data ####
context("parse_data")

test_that("Will it reject invalid api_data",{
  expect_error(parse_data(api_data = 1, lan = "en"))
  expect_error(parse_data(api_data = "some string", lan = "en"))
  expect_error(parse_data(api_data = list(), lan = "en"))
})

test_that("input must come from api_wiki_data (put class before output)",{
  expect_error(parse_data(api_data = list("related_topics" = NA, "abstract_text" = NA)))
})

test_that("lan must be acceptable input",{
  expect_error(parse_data(api_wiki_data(), lan = 1))
  expect_error(parse_data(api_wiki_data(), lan = NULL))
  expect_error(parse_data(api_wiki_data(), lan = NA))
  expect_error(parse_data(api_wiki_data(), lan = ""))
  expect_error(parse_data(api_wiki_data(), lan = "xx"))
  expect_error(parse_data(api_wiki_data(), lan = "e"))
  expect_error(parse_data(api_wiki_data(), lan = "eng"))
})


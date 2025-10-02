context("api_wiki_data")
context("parse_data")

data("women_in_statistics")

test_sample <- c(86, 238, 136) # These are taken from a sample with the seed 6765
# set.seed(seed = 6765)
# test_sample <- sample(which(women_in_statistics$can_be_accessed), 3)

# Tests for api_wiki_data ####

test_that("Can it run correctly",{
  # This test does not work, don't know why though, but it fails at requests :(
  expect_output(length(api_wiki_data("Smoltification")), 2)
  expect_output(length(api_wiki_data(women_in_statistics[test_sample[1], 1])), 2)
})

test_that("Can it run correctly",{
  # This test does not work, don't know why though, but it fails at requests :(
  expect_output(api_wiki_data("Smoltification")$abstracts$value, '[1] "Smoltification (also known as Parr-Smolt transformation) is a complex series of physiological changes where young salmonid fish adapt from living in fresh water to living in seawater. Physiological changes during smoltification include modified body shape, increased skin reflectance (the measure of the proportion of light or other radiation striking a surface which is reflected off it.), and increased Na+/K+-ATPase in the gills. A number of mechanisms assist with osmoregulation."')
  expect_output(api_wiki_data(women_in_statistics[test_sample[1], 1])$abstracts$value, '[1] "Lynne Billard (born 1943) is an Australian statistician and professor at the University of Georgia, known for her statistics research, leadership, and advocacy for women in science. She has served as president of the American Statistical Association, and the International Biometric Society, one of a handful of people to have led both organizations."')
  expect_output(api_wiki_data(women_in_statistics[test_sample[2], 1])$abstracts$value, '[1] "Grace Yun Yi is a professor of the University of Western Ontario where she currently holds a Tier I Canada Research Chair in Data Science. She was a professor at the University of Waterloo, Canada, where she holds a University Research Chair in Statistical and Actuarial Science. Her research concerns event history analysis with missing data and its applications in medicine, engineering, and social science."')
  expect_output(api_wiki_data(women_in_statistics[test_sample[3], 1])$abstracts$value, '[1] "Susmita Datta is an Indian biostatistician. She is a professor of biostatistics at the University of Florida, and is the former president of the Caucus for Women in Statistics. She is also a musician who has published three CDs of Bengali folk songs."')
})

test_that("Will it reject non string inputs?",{
  expect_error(api_wiki_data(1))
  expect_error(api_wiki_data(NA))
  expect_error(api_wiki_data(TRUE))
})

test_that("Will it reject inputs without articles", {expect_error(api_wiki_data("Krzysztof Bartoszek"))})

# Tests for parse_data ####
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


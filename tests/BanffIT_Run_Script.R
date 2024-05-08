# # loading dataset
# library(banffIT)
# library(fs)
# library(dplyr)
# library(rlang)
# library(tools)
# library(readr)
# library(stringr)
# library(lubridate)
# library(tidyr)
# library(fabR)
# library(madshapR)
# library(crayon)
# library(Rmonize)
# library(janitor)
#
# input_file = "tests/CHUQ_for_test_3.csv"
# output_folder = 'tests'
# language = 'label:en'
# option_filter = quote(adequacy == 1)
# detail = TRUE
# include_banff_dictionary = TRUE

library(banffIT)

input_file = system.file("extdata", "banff_example.xlsx", package = "banffIT")

banff_launcher(
  input_file = input_file,
  output_folder = 'tests',
  language = 'label:en',
  option_filter = adequacy == 1,
  detail = TRUE)




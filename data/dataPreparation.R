################################################################################
# Title   : Génération d'un fichier TSV et JSON à partir de données de gapminder
# Source  : Gapminder (https://www.gapminder.org/data/)
# Dev     : Thomas DENECKER
# Date    : 2022-06-08
# GitHub  : https://github.com/IFB-ElixirFr/comparison_JS_graphics_libraries
################################################################################

################################################################################
###                                Library                                   ###
################################################################################

library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(jsonlite)

################################################################################
###                              FUNCTION                                    ###
################################################################################


convert_units <- function(x) {
  
  if (class(x) == "numeric") return(x)

  # named vector of scalings (you can add to this)
  unit_scale <- c("k" = 1e3, "m" = 1e6, "b" = 1e9)
  
  # clean up some potential nuisances with the input
  x_str <- gsub(",", "", trimws(tolower(as.character(x))))
  
  # extract out the letters
  unit_char <- gsub("[^a-z]", "", x_str)
  
  # extract out the numbers and convert to numeric
  x_num <- as.numeric(gsub("[a-z]", "", x_str), "", x_str)
  
  # develop a vector of multipliers
  multiplier <- unit_scale[match(unit_char, names(unit_scale))]
  multiplier[is.na(multiplier)] <- 1
  
  # multiply
  x_num * multiplier
}

################################################################################
###                                 MAIN                                     ###
################################################################################

#-------------------------------------------------------------------------------
# Lecture des données
#-------------------------------------------------------------------------------

income <- read.csv2("raw/income_per_person.csv", sep = ",")
live_expectancy <- read.csv2("raw/life_expectancy_years.csv", sep = ",")
pop_total <- read.csv2("raw/population_total.csv", sep = ",")
regions <- read.csv2("raw/list-of-countries-etc.tsv", sep = "\t")

#-------------------------------------------------------------------------------
# Extract by year (2021)
#-------------------------------------------------------------------------------

# Income
income_2021 <- income  %>% 
  select(country, X2021) %>% 
  rename(income_2021 = X2021) %>% 
  mutate(income_2021 =  convert_units(income_2021))

# live_expectancy
live_expectancy_2021 <- live_expectancy  %>% 
  select(country, X2021) %>% 
  rename(live_expectancy_2021 = X2021) %>% 
  mutate(live_expectancy_2021 =  as.numeric(live_expectancy_2021))

# pop_total
pop_total_2021 <- pop_total  %>% 
  select(country, X2021) %>% 
  rename(pop_total_2021 = X2021) %>% 
  mutate(pop_total_2021 =  convert_units(pop_total_2021))

# region
regions_area <- regions %>% 
  select(name, six_regions) %>% 
  rename(country = name) %>%
  mutate(six_regions = gsub("_", " ", str_to_title(six_regions)))

#-------------------------------------------------------------------------------
#  Merge
#-------------------------------------------------------------------------------

final_dataset <- regions_area %>% full_join(pop_total_2021, by='country') %>% 
  full_join(income_2021, by='country') %>% 
  full_join(live_expectancy_2021, by='country') %>% drop_na()

#-------------------------------------------------------------------------------
#  Export
#-------------------------------------------------------------------------------

# TSV
write_tsv(final_dataset, "compileDataGapminder.tsv")

# Json
write(toJSON(final_dataset), "compileDataGapminder.json" )


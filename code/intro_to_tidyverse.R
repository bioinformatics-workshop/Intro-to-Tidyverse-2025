###############################################################################
# Installing the Tidyverse package
###############################################################################

## CRAN
# install.packages("tidyverse", Ncpus = 6)

## Github
# require("devtools")
# library(devtools)
# devtools::install_github("tidyverse/tidyverse") 

## Source
# install.packages(path_to_file, repos = NULL, type = "source") 

###############################################################################
# Loading the package
###############################################################################

library(tidyverse)
library(palmerpenguins)

###############################################################################
# Palmerpenguins dataset
###############################################################################

penguins
glimpse(penguins)

###############################################################################
# Importing Data with `readr`
###############################################################################
## Delimited file
pg_delim <- read_delim(file = "data/penguins.txt", delim = ":", col_names = TRUE)

## CSV
pg_csv <- read_csv(file = "data/penguins.csv", col_names = TRUE)

## TSV
pg_tsv <- read_csv(file = "data/penguins.csv", col_names = TRUE)

## Excel
require(readxl)
pg_xls <- read_xlsx(path = "data/penguins.xlsx", sheet = NULL, col_names = TRUE)

## Googlesheet
require(googlesheets4)
URL <- "https://docs.google.com/spreadsheets/d/1dFh-U1P0PpJurRXpmXbDzFalLpZMsMn7HvjPZ--vznw/edit?usp=sharing"
pg_gsheet <- read_sheet(ss = URL, sheet = NULL, col_names = TRUE)

###############################################################################
# Data Wrangling/Transformation with `dplyr`
###############################################################################

###############################################################################
# Filter rows with `filter()`
###############################################################################
# female penguins only
filter(penguins, sex == "female") 

# data collected from 2007 or 2008
filter(penguins, year == 2007 | year == 2008) 
filter(penguins, year %in% c(2007,2008))

# penguins with bill_length < 40 or bill_depth < 20
filter(penguins, !(bill_length_mm > 40 | bill_depth_mm < 20))
filter(penguins, bill_length_mm <= 40, bill_depth_mm < 20)

# penguins with bill_length > 40 & body_mass > 3500
filter(penguins, bill_length_mm > 45 & body_mass_g > 4000)

# remove rows containing NA in bill length
filter(penguins, !is.na(bill_length_mm))

###############################################################################
# Arrange rows with `arrange()`
###############################################################################

# sort penguins by sex, species, island
arrange(penguins, sex, species, island)

# sort penguins by bill length, in descending order
arrange(penguins, desc(bill_length_mm))


###############################################################################
# Select columns with `select()`
###############################################################################

select(penguins, species, bill_length_mm, body_mass_g)

# select all columns between species and bill depth (inclusive)
select(penguins, species:bill_depth_mm)

# select all columns except those from island to flipper length (inclusive)
select(penguins, -(island:flipper_length_mm))

# select the species column and all columms that begins with "bill"
select(penguins, species, starts_with("bill")) 

# select the species column and all columns that ends with "mm"
select(penguins, species, ends_with("_mm"))

# select the species column and all columns with "length"
select(penguins, species, contains("length"))

# rename a variable (e.g. species to genera)
rename(penguins, genera = species)


###############################################################################
# Add new column variables with `mutate()`
###############################################################################

# create a subset of penguins data
penguins_sml <- select(penguins, 
                       -c(island, year)
)

# create variable bill_length_cm
mutate(penguins_sml,
       flipper_length_cm = flipper_length_mm / 10,
       log10_body_mass_g = log10(body_mass_g) 
)

# create new variables using other variables
mutate(penguins_sml,
       ratio_bill_len_dep_mm = bill_length_mm / bill_depth_mm
)

# only display the new variables
transmute(penguins_sml,
          ratio_bill_len_dep_mm = bill_length_mm / bill_depth_mm
)

###############################################################################
# Grouped summaries with  `summarise()` and `group_by()`
###############################################################################

# mean bill length for all penguins surveyed
summarise(penguins, mean_bill_len = mean(bill_length_mm, na.rm = TRUE))

# mean bill length by species and island
species_island <- group_by(penguins, species, island)
summarise(species_island, mean_bill_len = mean(bill_length_mm, na.rm = TRUE))

# summarize by muliple conditions on grouped data (species, island)
# number penguins, mean bill length, median flipper length, minimum body mass, maximum body mass
species_island <- group_by(penguins, species, island)
summarise(species_island, no_penguins = n(),
          mean_bill_len = mean(bill_length_mm, na.rm = TRUE),
          median_flipper_len = median(flipper_length_mm, na.rm = TRUE),
          min_body_mass_g = min(body_mass_g, na.rm = TRUE),
          max_body_mass_g = max(body_mass_g, na.rm = TRUE)
)

###############################################################################
# Using the pipe operator `%>%` or `|>` to link multiple commands
###############################################################################

species_island <- group_by(penguins, species, island)
summarise(species_island, mean_bill_len = mean(bill_length_mm, na.rm = TRUE))

# mean bill length by species and island (with pipe)
penguins |> 
  group_by(species, island) |>
  summarise(mean_bill_len = mean(bill_length_mm, na.rm = TRUE))

# combine mulitple functions with pipe
penguins |> 
  group_by(species, island) |>
  summarise(mean_bill_len = mean(bill_length_mm, na.rm = TRUE)) |>
  filter(species == "Adelie")

###############################################################################
# Useful Commands
###############################################################################

# removing all rows containing NA in any column
penguins |> na.omit()

# removing rows containing NA from specific columns (e.g., bill_length, bill_depth)
filter_at(penguins, vars(bill_length_mm:sex), all_vars(!is.na(.)))

# renaming columns using select()
select(penguins, penguin_type = species, collection_year = year)

# write over previous column data
mutate(penguins, sex = case_when(
  sex == "female" ~ "F",
  sex == "male" ~ "M",
  TRUE ~ NA
))
###############################################################################
# Exporting files using `readr`
###############################################################################
## Writing to delimited file (e.g. ":")
# write_delim(object_name, file = "data/table.txt", delim = ":", col_names = TRUE)

## Writing to CSV
# write_csv(object_name, file = "data/table.csv", col_names = TRUE)

## Writing to TSV
# write_tsv(object_name, file = "data/table.tsv", col_names = TRUE)

## Writing to Excel
# write_excel_csv(object_name, file = "data/table.xls", col_names = TRUE)

## Write to Googlesheets
# write_delim(object_name, ss = "googlesheet_name", sheet = NULL)

## Write to compressed file
# write_csv(object_name, file = "data/table.csv.gz", col_names = TRUE)



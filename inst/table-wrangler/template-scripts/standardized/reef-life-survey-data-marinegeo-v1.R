# Process Reef Life Survey Excel data

## How to use this script ####
# Code entered in the 'MarineGEO Table Wrangler Start' section can be
# run in the Table Wrangler application to assist processing data.
# IMPORTANT: Any objects used in this script between the 'Start' and 'End' section
# must also be available in the application or else it will crash.
# In addition, functions used in this section must be sourced from the `tidyverse`, `marinegeo.utils`,
# or from `base` R packages.

# to update or install `marinegeo.utils`, run (requires `devtools` package):
# devtools::install_github('https://github.com/MarineGEO/marinegeo-R-utilities')

# to launch the app, run marinegeo.utils::shiny_launch_table_wrangler()

# Load Packages
library(tidyverse)
library(readxl)
library(marinegeo.utils)

input_file_path <- '__INPUT_FILE_PATH__'

## Destination table metadata
table_out <- 'reef-life-survey-data-marinegeo-v1'
req_cols <- marinegeo.utils::utl_mg_column_order(table_out)

## MarineGEO Table Wrangler Start ####
df_out <- utl_rls_load_excel(input_file_path, "DATA") %>%
  mutate(taxonomic_id = as.numeric(
    gsub("urn:lsid:marinespecies.org:taxname:", "", utl_mg_get_scientific_id(species))
  )) # %>%

  # Make necessary curations to depth and date before creating the sample event ID column,
  # mutate(sample_event_id = paste(gsub(" ", "_", site_name), "RLS", date, depth, sep = "_"))


## MarineGEO Table Wrangler End ##

# Test Output and write data ####
# The select(all_of(req_cols)) code will create an error if required columns are not present
# It also reorders the columns to match the target table format
# Both of these are very important to maintaining data workflows for the target table
# and shouldn't be skipped.

df_out %>%
  select(all_of(req_cols)) %>%
  marinegeo.utils::utl_mg_test_data_types(table_out) %>%
	write_csv('__OUTPUT_FILE_PATH__')

library(tidyverse)
library(rvest)
library(glue)

html_file <- read_html("https://datacenter.kidscount.org/data/customreports/5343,5344,5345,5346,5347,5348,5349,5350,5351,5352,5353,5354,5355,5356,5357,5358,5359,5360,5361,5362,5363,5364,5365,5366,5367,5368,5369,5370,5371,5372,5373,5374,5375,5376,5377,5378/any")

ids <- html_file %>% 
  html_nodes(xpath = '//*[@id="content_cont"]/div[2]/div[2]') %>% 
  html_children() %>% 
  html_attrs() %>% 
  map("id") 

ids <- ids[!map_lgl(ids, is.null)]
xpath_tbl_title <- glue('//*[@id="{ids}"]/h3/a')
xpath_tbl_data <- glue('//*[@id="{ids}"]/div[1]/div/div[1]/table')

pull_varname <- function(xpath) {
  txt <- html_file %>% 
    html_nodes(xpath = xpath) %>% 
    html_text()
  txt <- trimws(gsub("\r|\n|\t|\\(|\\)|,", "", txt), which = "both")
  tolower(gsub(" ", "_", txt))
}

pull_tbl <- function(xpath_tbl, xpath_varname) {
  varname <- pull_varname(xpath_varname)
  
  tbl <- html_file %>% 
    html_nodes(xpath = xpath_tbl) %>% 
    html_table()
  
  tbl <- tbl[[1]] %>% 
    select(-`Data Type`) %>% 
    gather(year, !!sym(varname), matches("\\d")) %>% 
    as_tibble() %>% 
    mutate(year = gsub(".+-\\s(\\d\\d)", "20\\1", year))
  
  if(any(grepl("Race", names(tbl)))) {
    tbl <- tbl %>% 
      mutate(Race = paste0(Race, "_", varname)) %>% 
      spread(Race, !!sym(varname))
  }
  tbl
}

l <- map2(xpath_tbl_data, xpath_tbl_title, pull_tbl)

d <- reduce(l, left_join)

old_names <- d %>% 
  gather(var, val) %>% 
  pull(var) %>% 
  unique()

new_names <- c(
  "location", "year", "n_totaln_children", "n_05_children", 
  "percent_hispanic_all", "percent_amind_all", "percent_asian_all", 
  "percent_black_all", "percent_pacisl_all", "percent_multi_all", 
  "percent_white_all", "percent_hispanic_children", "percent_amind_children", 
  "percent_asian_children", "percent_black_children", 
  "percent_pacisl_children", "percent_multi_children", 
  "percent_white_children", "percent_unemployed_all", 
  "n_suppnutrition_children", "n_tempassist_children", 
  "percent_poverty_children", "percent_foodinsecure_children", 
  "percent_frl_children", "per100_childcareslots_children", 
  "n_emprelatedaycare_children", "percent_noschoolage34_children",
  "percent_g3readprof_children", "percent_g3mathprof_children",
  "percent_g8readprof_children", "percent_g8mathprof_children",
  "percent_cohortgrad_children", "percent_homeless_children",
  "per1000_infantmortality_all", "per1000_teenpreg1517_all",
  "per1000_teenpreg1519_all", "per1000_lowbirthweight_all",
  "percent_adequateprenatcare_all", "percent_nohealthinsur_children",
  "percent_immuniz4313_children", "percent_immuniz4313314_children", 
  "per1000_abuseneglect_children", "per1000_threatharm_children",
  "per1000_juvarrest_children", "per1000_juvjusref_children",
  "n_fostercare_children", "percent_fostercare_children", 
  "percent_stablefostercare_children", "n_exitfostercare_children",
  "percent_ageoutfostercare_children"
)

new_names_df <- tibble(old_names, new_names)

d <- d[ ,!map_lgl(d, ~sum(is.na(.x)) == length(.x))]

d <- d %>% 
  gather(old_names, val, -Location, -year) %>% 
  left_join(new_names_df) %>% 
  select(-old_names) %>%
  mutate(val = ifelse(val == "N.A." |val == "—"| val == "?"| val == "*",
                      NA_character_, 
                      val),
         val = parse_number(val)) %>% 
  spread(new_names, val) 

write_csv(d, here::here("data", "kidscount", "kidscount.csv"))
 

# txt <- pdf_text(list.files(here::here("data", "kidscount"), 
#                            pattern = "^CFFO",
#                            full.names = TRUE))
# 
# tmp <- txt[7]
# 
# separate_lines <- function(page) {
#   strsplit(page, split = "\n")[[1]]
# }
# 
# extract_county <- function(page_separated) {
#   splt <- strsplit(page_separated[1], split = " ")[[1]]
#   non_empty <- map_dbl(splt, nchar)
#   first_entry <- seq_along(non_empty)[map_lgl(non_empty, ~.x > 0)][1]
#   county <- splt[c(first_entry, grep("County", splt))]
#   paste(county, collapse = " ")
# }
# 
# tmp %>% 
#   separate_lines() %>% 
#   extract_county()
# 
# map_chr(txt[7:42], ~separate_lines(.x) %>% extract_county())

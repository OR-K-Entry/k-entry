library(tidyverse)
library(sf)

# oka
oka <- map_df(list.files(here::here("data", "oka"), 
                         full.names = TRUE),
              readxl::read_xlsx) %>% 
  janitor::clean_names() %>% 
  mutate(year = as.numeric(gsub(".+-(.+)", "20\\1", schl_yr)))

oka <- oka %>% 
  select(year, a_dist_id, a_schl_id, ssid = rpt_chk_digit_stdnt_id, gender, 
         ethnic_cd, self_reg_av = al_s_ravg, interpersonal = al_i_savg,
         math_score = e_ma_to_tscore, 
         ln_score1 = e_lit_l_ncorr, ln_score2 = e_lit_m1corr, 
         ls_score1 = e_lit_l_scorr, ls_score2 = e_lit_m2corr, 
         ls_score3 = e_lit_m6corr,
         ln_uppercase_score = e_lit_m4corr,
         ln_lowercase_score = e_lit_m5corr) %>% 
  mutate(ln = coalesce(ln_score1, ln_score2),
         ls = coalesce(ls_score1, ls_score2, ls_score3)) %>% 
  select(-matches("\\d"))
oka
# kidscount
kidscount <- read_csv(here::here("data", "kidscount", "kidscount.csv")) %>%
  mutate(county = paste(Location, "County")) %>%
  select(-Location)

# NCES
nces <- read_csv(here::here("data", "nces", "nces.csv")) %>% 
  rename(county = nmcnty) %>%
  mutate(year = as.numeric(gsub(".+-(.+)", "\\1", school_year))) %>%
  select(-school_year) %>% 
  mutate(
    a_schl_id = abs(
      parse_number(
        gsub(".+-(.+)", "\\1", st_schid)
      )
    ),
    a_dist_id = abs(parse_number(st_leaid))
  )

# census
census <- readRDS(here::here("data", "census", "census-percents.rds"))

d <- full_join(kidscount, nces)

# Now do a spatial join to merge in the OR stuff
census <- census %>%
  rename(county_census = county)

school_locs <- nces %>%
  select(ncessch, year, lat, lon) %>%
  distinct()

d <- d %>%
  group_by(ncessch) %>%
  fill(lat, lon, .direction = "updown") %>%
  drop_na(lat, lon) %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(st_crs(census)) %>%
  left_join(school_locs) %>%
  ungroup()

d2 <- st_join(census, d) 

d2 %>% 
  filter(year == 2015 & a_schl_id == 1) 

ggplot(d2) +
  geom_sf() +
  geom_point(aes(lon, lat), color = "blue")

d2 %>% 
  filter(county != county_census)

d2 %>% 
  count(year, a_schl_id) %>% 
  drop_na(a_schl_id) %>% 
  filter(n > 1)

d3 <- left_join(d2, oka)

d3

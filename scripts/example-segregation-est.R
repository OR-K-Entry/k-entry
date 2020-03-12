# Script to demonstrate calculation of 
library(tidyverse)
library(tidycensus)
library(sp)
library(sf)
library(seg)

# https://www.socialexplorer.com/data/ACS2010/metadata/?ds=ACS10&table=B03002
hisp_white_tracts <- get_acs("tract",
              variables = c(hisp = "B03002_012",
                            white = "B03002_003"),
              state = "OR",
              output = "wide",
              geometry = FALSE) %>% 
  mutate(NAMELSAD = gsub("^(.+\\d),\\s.+", "\\1", NAME),
         county = gsub(".+\\d,\\s(.+),\\s.+", "\\1", NAME)) %>% 
  select(NAMELSAD, county, hisp = hispE, white = whiteE)

geo_tracts <- tigris::tracts("OR")

tracts <- st_as_sf(geo_tracts) %>% 
  select(NAMELSAD) %>% 
  left_join(hisp_white_tracts)

calc_seg <- function(data, var1, var2, bw = c(0.3, 0.7)) {
  mods <- map(bw,
    ~spseg(as_Spatial(data$geometry), 
      select(st_drop_geometry(data), 
        !!enquo(var1), 
        !!enquo(var2)), 
      smoothing = "kernel", 
      sigma = .x)
  )
  names(mods) <- bw
  mods
}

pull_seg_estimates <- function(l) {
  tibble(binwidth = names(l),
         d = map_dbl(l, ~.x@d),
         r = map_dbl(l, ~.x@r),
         h = map_dbl(l, ~.x@h))
}

tracts_seg <- tracts %>% 
  nest(data = c(NAMELSAD, hisp, white, geometry)) %>% 
  mutate(seg_mod = map(data, calc_seg, hisp, white, bw = 0.3),
         seg_est = map(seg_mod, pull_seg_estimates)) %>% 
  select(county, seg_est) %>% 
  unnest(seg_est)

write_csv(tracts_seg, here::here("data", "or-county-seg.csv"))

#tracts_seg <- read_csv(here::here("data", "or-county-seg.csv"))

counties <- tigris::counties("OR", cb = TRUE) %>%
  st_as_sf() %>%
  mutate(county = paste(NAME, "County"))

left_join(counties, tracts_seg)  %>%
  mapview::mapview(zcol = "d")

districts <- tigris::school_districts("OR") %>%
  st_as_sf()




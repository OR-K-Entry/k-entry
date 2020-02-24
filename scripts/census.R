library(tidyverse)
library(tidycensus)
library(colorspace)

theme_set(theme_minimal(15) +
            theme(legend.position = "bottom",
                  legend.key.width = unit(4, "cm"))
)

vars <-  load_variables(2017, "acs5", cache = TRUE)

#View(vars)

or <- get_acs("tract",
              variables = c(ed_attain_n = "B06009_001",
                            ed_attain_lths = "B06009_002",
                            ed_attain_hsgrad = "B06009_003",
                            ed_attain_somecol = "B06009_004",
                            ed_attain_bach = "B06009_005",
                            ed_attain_gradsch = "B06009_006",
                            prim_lang_n = "B16001_001",
                            prim_lang_engonly = "B16001_002",
                            geo_mobil_n = "B07001PR_001",
                            geo_mobility_onetofour = "B07001PR_002",
                            home_value_n = "B25075_001",
                            home_value_lt10k = "B25075_002",
                            home_value_10kto15k = "B25075_003",
                            home_value_15kto20k = "B25075_004",
                            home_value_20kto25k = "B25075_005",
                            home_value_25kto30k = "B25075_006",
                            home_value_30kto35k = "B25075_007",
                            home_value_35kto40k = "B25075_008",
                            home_value_40kto50k = "B25075_009",
                            home_value_50kto60k = "B25075_010",
                            home_value_60kto70k = "B25075_011",
                            home_value_70kto80k = "B25075_012",
                            home_value_80kto90k = "B25075_013",
                            home_value_90kto100k = "B25075_014",
                            home_value_100kto125k = "B25075_015",
                            home_value_125kto150k = "B25075_016",
                            home_value_150kto175k = "B25075_017",
                            home_value_175kto200k = "B25075_018",
                            home_value_200kto250k = "B25075_019",
                            home_value_250kto300k = "B25075_020",
                            home_value_300kto400k = "B25075_021",
                            home_value_400kto500k = "B25075_022",
                            home_value_500kto750k = "B25075_023",
                            home_value_750kto1000k = "B25075_024",
                            home_value_1000kto1500k = "B25075_025",
                            home_value_1500kto2000k = "B25075_026",
                            home_value_gt2000k = "B25075_027",
                            home_built_n = "B25034_001",
                            home_built_past2013 = "B25034_002",
                            home_built_2010to2013 = "B25034_003",
                            home_built_2000to2009 = "B25034_004",
                            home_built_1990to1999 = "B25034_005",
                            home_built_1980to1989 = "B25034_006",
                            home_built_1970to1979 = "B25034_007",
                            home_built_1960to1969 = "B25034_008",
                            home_built_1950to1959 = "B25034_009",
                            home_built_1940to1949 = "B25034_010",
                            home_built_pre1940 = "B25034_011",
                            foodstamps_snap_n = "B22010_001",
                            foodstamps_snap_received = "B22010_002",
                            health_insurance_n = "B27003_001",
                            health_insurance = "B27003_002",
                            ratio_incpov_n = "B05010_001",
                            ratio_incpov_lt1 = "B05010_002",
                            ratio_incpov_1to2 = "B05010_010",
                            ratio_incpov_gt2 = "B05010_018"),
              state = "OR",
              geometry = TRUE) %>% 
  mutate(county = gsub(".+\\d,\\s(.+),.+", "\\1", NAME))

or_wide <- or %>% 
  select(-moe) %>% 
  spread(variable, estimate)

or_wide <- or_wide %>% 
  mutate(
    percent_nohighschool = ed_attain_lths/ed_attain_n,
    percent_highschool = ed_attain_hsgrad/ed_attain_n,
    percent_somecollege = ed_attain_somecol/ed_attain_n,
    percent_bachelors = ed_attain_bach/ed_attain_n,
    percent_gradsch = ed_attain_gradsch/ed_attain_n,
    percent_mobile14 = geo_mobility_onetofour/geo_mobil_n,
    percent_healthinsurance = health_insurance/health_insurance_n,
    percent_homespre40 = home_built_pre1940/home_built_n,
    percent_homes4049 = home_built_1940to1949/home_built_n,
    percent_homes5059 = home_built_1950to1959/home_built_n,
    percent_homes6069 = home_built_1960to1969/home_built_n,
    percent_homes7079 = home_built_1970to1979/home_built_n,
    percent_homes8089 = home_built_1980to1989/home_built_n,
    percent_homes9099 = home_built_1990to1999/home_built_n,
    percent_homes0009 = home_built_2000to2009/home_built_n,
    percent_homes1013 = home_built_2010to2013/home_built_n,
    percent_homespast13 = home_built_past2013/home_built_n,
    percent_homevallt10k = home_value_lt10k/home_value_n,
    percent_homeval10to15k = home_value_10kto15k/home_value_n,
    percent_homeval15to20k = home_value_15kto20k/home_value_n,
    percent_homeval20to25k = home_value_20kto25k/home_value_n,
    percent_homeval25to30k = home_value_25kto30k/home_value_n,
    percent_homeval30to35k = home_value_30kto35k/home_value_n,
    percent_homeval35to40k = home_value_35kto40k/home_value_n,
    percent_homeval40to50k = home_value_40kto50k/home_value_n,
    percent_homeval50to60k = home_value_50kto60k/home_value_n,
    percent_homeval60to70k = home_value_60kto70k/home_value_n,
    percent_homeval70to80k = home_value_70kto80k/home_value_n,
    percent_homeval80to90k = home_value_80kto90k/home_value_n,
    percent_homeval90to100k = home_value_90kto100k/home_value_n,
    percent_homeval100to125k = home_value_100kto125k/home_value_n,
    percent_homeval125to150k = home_value_125kto150k/home_value_n,
    percent_homeval150to175k = home_value_150kto175k/home_value_n,
    percent_homeval175to200k = home_value_175kto200k/home_value_n,
    percent_homeval200to250k = home_value_200kto250k/home_value_n,
    percent_homeval250to300k = home_value_250kto300k/home_value_n,
    percent_homeval300to400k = home_value_300kto400k/home_value_n,
    percent_homeval400to500k = home_value_400kto500k/home_value_n,
    percent_homeval500to750k = home_value_500kto750k/home_value_n,
    percent_homeval750to1000k = home_value_750kto1000k/home_value_n,
    percent_homeval1000to1500k = home_value_1000kto1500k/home_value_n,
    percent_homeval1000to1500k = home_value_1000kto1500k/home_value_n,
    percent_homeval1500to2000k = home_value_1500kto2000k/home_value_n,
    percent_homevalgt2000k = home_value_gt2000k/home_value_n,
    percent_englishonly = prim_lang_engonly/prim_lang_n,
    percent_incpovlt1 = ratio_incpov_lt1/ratio_incpov_n,
    percent_incpov12 = ratio_incpov_1to2/ratio_incpov_n,
    percent_incpovgt2 = ratio_incpov_gt2/ratio_incpov_n
  ) %>% 
  select(GEOID:county, starts_with("percent")) %>% 
  mutate_if(is.numeric, ~.*100)

ed <- or_wide %>% 
  select(GEOID:percent_gradsch) %>% 
  gather(ed_attain, percent, starts_with("percent"))

ggplot(ed) +
  geom_sf(aes(fill = percent)) +
  scale_fill_continuous_diverging("Blue-Red 3",
                                  name = "Estimate\n",
                                  mid = mean(ed$percent,
                                             na.rm = TRUE),
                                  limits = c(0, 100),
                                  rev = TRUE) +
  facet_wrap(~ed_attain) +
  labs(title = "Percent Educational Attainment")

#dir.create(here::here("data", "census"))
saveRDS(or_wide, here::here("data", "census", "census-percents.rds"))
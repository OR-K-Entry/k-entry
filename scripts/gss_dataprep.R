#Setup ----
# Packages, load
install.packages("pacman")
library(pacman)
p_load(tidyverse, tidymodels, caret, foreign)
p_load(magrittr, dplyr, ggplot2, yardstick)
p_load(skimr, naniar, readr, here)

# Data, load
gss.raw <- read.dta("C:/My_R/582_Global/data_raw/gss/GSS7218_R1.dta")

## My Variables ----
keeps = c("year", "id", "ballot", "wrkstat", "fear", "hrs1",
          "hrs2", "evwork", "rhlthend", "physhlth", "mntlhlth",
          "hlthdays","abinspay", "hyperten", "arthrtis", "diabetes",
          "depress", "misswork", "conschls", "abhlth", "crimlose",
          "inteduc", "prvdschl", "health", "natcrime", "fepresch",
          "spschool", "safehlth", "fammhneg", "hlthcare", "immcrime",
          "hlthstrt", "hlthphys", "hlthmntl", "natcrime", "PRESTG10",
          "savejobs", "strngun", "trdunion")

gss1  <- gss.raw[gss.raw$year >= 2013, (names(gss.raw) %in% keeps)]

gss1%<>% mutate(
  yr_bal_id = paste0(year, ballot, id)
  )


gssdf = gss1 %>%
  group_by(year) %>%
  skim() %>%
  select(skim_variable, year, complete_rate)

p1  = ggplot(data = gssdf) + 
  geom_point(mapping = aes(skim_variable, complete_rate, alpha = 0.4))+
  facet_wrap(~year)

plot(p1)

p2 = gg_miss_var(gss1, facet = year) +
  labs(title ="GSS Data Missingness for Variables of Interest 2014 - 2018") # visual of the missing data
p2


xnames = as.data.frame(names(gss1))
#summary(gss1)

# Save intermediate output
write.csv(gss1, file = "data/gss/gss1.csv")
write.csv(xnames, file = "data/gss/gss1_xnames.csv")
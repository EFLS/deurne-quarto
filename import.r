library(tidyverse)
library(janitor)
library(sf)
library(leaflet)
library(leaflegend)
library(htmlwidgets)
library(gt)
library(scales)
library(patchwork)

# Gegevens over buurten
import <- read_delim("./data/demografie_buurten.csv",
                     delim = ";",
                     name_repair = make_clean_names)

# Filter op Deurne
import <- filter(import, 
                 str_detect(buurten, "R"))

# Lege tibble met zelfde aantal rijen
d <- import[,0]

# Data importeren en overzetten
# NOOT: . > "Verborgen waarde"
#       - > "Niet van toepassing"
#
# Buurten
d$buurt_nis        <- import$buurten
d$buurt_naam       <- factor(str_to_title(import$naam_van_de_buurt))
#
# Absolute aantallen
d$kinderen_aantal    <- parse_number(import$aantal_0_tem_17_jarigen, na = ".")
d$volwassenen_aantal <- parse_number(import$aantal_18_tem_64_jarigen, na = ".")
d$ouderen_aantal     <- parse_number(import$aantal_65_plussers, na = ".")
d$ouderen_80_tem_89  <- parse_number(import$aantal_80_tem_89_jarigen, na = ".")
d$ouderen_90_plus    <- parse_number(import$aantal_90_ers, na = ".")
#
# Meer kengetallen
d$interne_vergrijzing            <- parse_number(import$interne_vergrijzing, na = ".")
d$bevolkingsdichtheid            <- parse_number(import$bevolkingsdichtheid_inw_ha, na = ".")
d$bevolkingsdichtheid_woongebied <- parse_number(import$bevolkingsdichtheid_woongebied, na = c("-", "."))

# OPM: Bevolkingsdichtheid is inw/ha
#      Bevolkingsdichtheid woongebied is pers / km2

d <- d %>%
  mutate(bevolking_totaal = kinderen_aantal + volwassenen_aantal + ouderen_aantal,
         bevolking_rest = bevolking_totaal - ouderen_aantal,
         ouderen_perc = ouderen_aantal / bevolking_totaal,
         ouderen_dichtheid = ouderen_perc * bevolkingsdichtheid)

# Filters
# Problemen bij berekening
# TOOD: Afkomstig van artefacten in data? Buurten zonder bewoners maar met 65+?
d <- d %>%
  filter(!is.na(bevolking_totaal | ouderen_aantal | bevolkingsdichtheid_woongebied))

# Orden namen van buurten, vnl. voor grafieken
# Sorteer wijken volgens aantal ouderen
d$buurt_naam <- fct_reorder(d$buurt_naam, d$ouderen_aantal, .na_rm = TRUE)


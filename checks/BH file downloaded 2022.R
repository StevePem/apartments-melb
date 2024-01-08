# Review of the 'BH' file downloaded from https://datashare.maps.vic.gov.au/ 
# for 2022

# Unlike the earlier year 'broadhectare' (BH) files, the 2022 file is 
# entitled 'greenfields' (GF2022) 

# The GF2022 file, unlike the earlier year BH files, does not contain 
# a development status field.  It contains 'TOTALLOTS', which is either 0, 1 or
# a substantial number, and 'LOTSPERHA', which is either NA (usually when 
# 'Totallots' is 0 or 1, or a completed number)

# This file largely repeats the analysis in section 3 of 'BH files downloaded 2013-21.R'
# for the 2022 file, on the basis that (without a development status field)
# any of the lots could be approved (but developments with only 0 or 1 lot
# are not of interest)

# Note also email from D Matthews, DELWP, 21 Sep 2202 (mentioned in section 3
# of 'BH files downloaded 2013-21.R': from 2018, instead of 'developed', lots 
# are shown as 'approved with a title'; and so they're probably only
# approved if they have mopre than one title


library(dplyr)
library(sf)
library(ggplot2)

source("./functions/readZippedGIS.R")

options(scipen = 1000)

# 1 Read in file ----
# -----------------------------------------------------------------------------#
GF_2022 <- read_zipped_GIS("../Data/UDP2022_GF.zip", 
                           subpath = "/gda2020_vicgrid/esrishape/whole_of_dataset/victoria/UDP") %>%
  st_transform(., crs = 7899)


# 2 Check adequacy of AREAHA field ----
# -----------------------------------------------------------------------------#
test <- GF_2022 %>% 
  mutate(test_area = as.numeric(st_area(geometry) / 10000),  # sq m to ha
         area_diff = abs(AREAHA - test_area))
max(test$area_diff)  # 0.00000827386

# conclusion - it's accurate


# 3 Find developments with lots_ha 100+ ----
# -----------------------------------------------------------------------------#
# checking - calculation based on DELWP's stated 'Areaha'
lots100plus <- GF_2022 %>%
  filter(TOTALLOTS / AREAHA >= 100)  
# returns 497 obs, of which all have 1 lot - that is, there are none > 100


# 4 Check multi-lot developments with high lots/ha ----
# -----------------------------------------------------------------------------#
# While there are no projects with lots_ha 100+, look at the highest 5
# lots per ha (as reported by DWELP)
high.densities <- GF_2022 %>%
  arrange(desc(LOTSPERHA)) %>%
  head()

high.densities %>% st_drop_geometry()
# LOTSPERHA      AREAHA TOTALLOTS     LGA ZONECODE                           PSPNAME           SUBURB
# 1  95.75027  0.09399731         9   CASEY     UGZ9                 Berwick Waterways          BERWICK
# 2  75.00000  0.11315614         8   CASEY     UGZ9                 Berwick Waterways          BERWICK
# 3  70.52220 20.10879235      1418   CASEY    UGZ10         Casey Central Town Centre CRANBOURNE NORTH
# 4  70.52220  2.15122695       152   CASEY    UGZ10         Casey Central Town Centre CRANBOURNE NORTH
# 5  66.57289  6.00808562       400 WYNDHAM    UGZ14 East Werribee Employment Precinct         WERRIBEE

# Reviewed in QGIS / google earth (imagery dates noted below)
# 1 & 2 (Berwick Waterways) is undeveloped as at Google Earth May 2022, 
#   and adjacent developed areas are all closely-packed houses
# 3 & 4 (Casey Central Town Centre) are undeveloped as at Google Earth May 2022
# 5 (East Werribee Employment Precinct) is undeveloped as at Google Earth Feb 2022,
#   and anyway is going to be an employment precinct, not housing
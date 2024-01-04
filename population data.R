# File to load population data (including SEIFA)
library(dplyr)
library(sf)
library(readxl)

source("./functions/readZippedGIS.R")


MB.counts.2006.file.SEXP <- "../Data/SEXP 2006 Vic CD.csv"
MB.counts.2006.file.STRD <- "../Data/STRD 2006 Vic CD.csv"
MB.counts.2011.file <- "../Data/censuscounts_mb_2011_aust.csv"
MB.counts.2016.file <- "../Data/2016 census mesh block counts.csv"
MB.counts.2021.file <- "../Data/Mesh Block Counts, 2021.xlsx"
MB.2006.file <- "../Data/1259030002_cd06avic_shape.zip"
MB.2011.file <- "../Data/1270055001_mb_2011_vic_shape.zip"
MB.2016.file <- "../Data/1270055001_mb_2016_vic_shape.zip"
MB.2021.file <- "../Data/MB_2021_AUST_SHP_GDA2020.zip"
SA2.counts.file <- "../Data/32180DS0003_2001-22.xlsx"
SA2.2021.file <- "../Data/SA2_2021_AUST_SHP_GDA2020.zip"

SEIFA.2006.file <- "../Data/SEIFA_2006.xls"
SEIFA.2011.file <- "../Data/SEIFA_2011.xls"
SEIFA.2016.file <- "../Data/SEIFA_2016.xls"
SEIFA.2021.file <- "../Data/SEIFA_2021.xlsx"


# load meshblock counts (note 'meshblocks' were 'CCDs' in 2006)
MB.counts.2006 <- read.csv(MB.counts.2006.file.SEXP, skip = 9)  %>%
  # omit first 1 and last 5 rows, which don't contain meshblock data
  slice(., 2:(n()-5)) %>%
  rename(MB = SEXP.Sex, Person = Total) %>%
  # join in dwelling data, which is separate for 2006
  left_join(read.csv(MB.counts.2006.file.STRD, skip = 9) %>%
              # omit first 1 and last 5 rows, which don't contain meshblock data
              slice(., 2:(n()-5)) %>%
              rename(MB = STRD.Dwelling.Structure, Dwelling = Total),
            by = "MB") %>%
  dplyr::select(MB, Person, Dwelling)

MB.counts.2011 <- read.csv(MB.counts.2011.file) %>%
  # omit last 2 rows, which don't contain meshblock data
  slice(., 1:(n()-2)) %>%
  dplyr::select(MB = Mesh_Block_ID, Person = Persons_Usually_Resident,
                Dwelling = Dwellings)

MB.counts.2016 <- read.csv(MB.counts.2016.file) %>%
  # omit last 5 rows, which don't contain meshblock data
  slice(., 1:(n()-5)) %>%
  dplyr::select(MB = MB_CODE_2016, Person, Dwelling)

MB.counts.2021 <- rbind(  # note Victoria is Tables 2 and 2.1
  read_xlsx(MB.counts.2021.file,
            sheet = "Table 2") %>%
    # colnames from row 6
    setNames(., .[6,]) %>%
    # omit first 6 and last 4 rows, which don't contain meshblock data
    slice(., 7:(n()-4)),
  read_xlsx(MB.counts.2021.file,
            sheet = "Table 2.1") %>%
    # colnames from row 6
    setNames(., .[6,]) %>%
    # omit first 6 and last 4 rows, which don't contain meshblock data
    slice(., 7:(n()-4))) %>%
  mutate(Person = as.numeric(Person), Dwelling = as.numeric(Dwelling)) %>%
  dplyr::select(MB = MB_CODE_2021, Person, Dwelling)


# load SEIFA (IRSAD table, State deciles)
SEIFA.2006 <- read_xls(SEIFA.2006.file, 
                       sheet = "Table2") %>%
  # colnames from rows 4 and 5 (and manual avoiding duplicates)
  setNames(., c("CD", .[4, 2], .[5, 3], "...4", 
                "Aust.rank", "Aust.decile", "Aust.percentile", "...8",
                "State", "State.rank", "State.decile", "State.percentile")) %>%
  # omit first 5 and last 3 rows, which don't contain meshblock data
  slice(., 6:(n()-3)) %>%
  mutate(State.decile = as.numeric(State.decile)) %>%
  dplyr::select(CD, Decile = State.decile)

SEIFA.2011 <- read_xls(SEIFA.2011.file,
                       sheet = "Table 2") %>%
  # colnames from rows 4 and 5 (and manual avoiding duplicates)
  setNames(., c("SA1", .[4, 2], .[5, 3], "...4", 
                "Aust.rank", "Aust.decile", "Aust.percentile", "...8",
                "State", "State.rank", "State.decile", "State.percentile")) %>%
  # omit first 5 and last 3 rows, which don't contain meshblock data
  slice(., 6:(n()-3)) %>%
  mutate(State.decile = as.numeric(State.decile)) %>%
  dplyr::select(SA1, Decile = State.decile)

SEIFA.2016 <- read_xls(SEIFA.2016.file,
                       sheet = "Table 3") %>%
  # colnames from rows 4 and 5 (and manual avoiding duplicates)
  setNames(., c(.[4, 1], "SA1", .[4, 3], .[5, 4], "...5", 
                "Aust.rank", "Aust.decile", "Aust.percentile", "...9",
                "State", "State.rank", "State.decile", "State.percentile")) %>%
  # omit first 5 and last 2 rows, which don't contain meshblock data
  slice(., 6:(n()-2)) %>%
  mutate(State.decile = as.numeric(State.decile)) %>%
  dplyr::select(SA1, Decile = State.decile)

SEIFA.2021 <- read_xlsx(SEIFA.2021.file,
                       sheet = "Table 3") %>%
  # colnames from rows 4 and 5 (and manual avoiding duplicates)
  setNames(., c("SA1", .[5, 2:3], "...4", 
                "Aust.rank", "Aust.decile", "Aust.percentile", "...8",
                "State", "State.rank", "State.decile", "State.percentile")) %>%
  # omit first 5 and last 3 rows, which don't contain meshblock data
  slice(., 6:(n()-3)) %>%
  mutate(State.decile = as.numeric(State.decile)) %>%
  dplyr::select(SA1, Decile = State.decile)


# load meshblocks (note 'meshblocks' were 'CCDs' in 2006) and join counts and SEIFA
MB.2006 <- read_zipped_GIS(zipfile = MB.2006.file) %>%
  st_transform(7899) %>%
  rename(MB = CD_CODE06) %>%
  # add counts
  left_join(MB.counts.2006, by = "MB") %>%
  # add SEIFA 
  left_join(SEIFA.2006, by = c("MB" = "CD")) %>%
  # calculate original area
  mutate(orig.area = st_area(geometry))

MB.2011 <- read_zipped_GIS(zipfile = MB.2011.file) %>%
  st_transform(7899) %>%
  rename(MB = MB_CODE11) %>%
  # add counts
  left_join(MB.counts.2011, by = "MB") %>%
  # add SEIFA
  left_join(SEIFA.2011, by = c("SA1_7DIG11" = "SA1")) %>%
  # calculate original area
  mutate(orig.area = st_area(geometry))

MB.2016 <- read_zipped_GIS(zipfile = MB.2016.file) %>%
  st_transform(7899) %>%
  rename(MB = MB_CODE16) %>%
  # add counts
  left_join(MB.counts.2016, by = "MB") %>%
  # add SEIFA
  left_join(SEIFA.2016, by = c("SA1_MAIN16" = "SA1")) %>%
  # calculate original area
  mutate(orig.area = st_area(geometry))

MB.2021 <- read_zipped_GIS(zipfile = MB.2021.file) %>%
  st_transform(7899) %>%
  rename(MB = MB_CODE21) %>%
  # filter to Victoria
  filter(STE_NAME21 == "Victoria") %>%
  # add counts
  left_join(MB.counts.2021, by = "MB") %>%
  # add SEIFA
  left_join(SEIFA.2021, by = c("SA1_CODE21" = "SA1")) %>%
  # calculate original area
  mutate(orig.area = st_area(geometry))


#  load estimates for SA2s (2021 boundaries) 2001-22, load SA2s and join counts
SA2.counts <- read_xlsx(SA2.counts.file,
                        sheet = "Table 1") %>%
  # colnames from rows 7 & 6
  setNames(., c(.[7, 1:10], .[6, 11:32])) %>%
  # omit first 8 and last 7 rows, which don't contain SA2 data
  slice(., 9:(n()-7)) %>%
  dplyr::select(SA2_CODE21 = 'SA2 code', any_of(as.character(2001:2022))) %>%
  # rename year columns
  rename_with(~ paste0("pop_", .), any_of(as.character(2001:2022)))

SA2.2021 <- read_zipped_GIS(zipfile = SA2.2021.file) %>%
  st_transform(7899) %>%
  # filter to Victoria
  filter(STE_NAME21 == "Victoria") %>%
  # add counts
  left_join(SA2.counts, by = "SA2_CODE21") %>%
  # calculate original area
  mutate(orig.area = st_area(geometry))


# 01_vitals_ca_LTE.R

# 0. Global settings ----

source("00_common.R")
# if (T) {
#     packages <- c("magrittr","ggplot2","stringr", "forcats")
#     lapply(packages, library, character.only = TRUE)
#     library(lubridate,  quietly=T); options(lubridate.week.start =  1)
#     library(data.table); options(datatable.print.class=TRUE)
#
#     dateToday <- format(Sys.time(), '%d %B, %Y') %>% dmy; dateToday
#     "%wo%" <- function(x, y) setdiff(x,y)
#     `%ni%` <-  Negate(`%in%`)
# }

# 1. Read cashed Table 13-10-0810-01 merged with GEO -----

# Canadian Vital Statistics Death (CVSD) Database
# Leading causes of death, total population
# Provisional weekly death counts, by selected grouped causes of death
# Table: 13-10-0810-01
# Release date: 2021-11-08

# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310081001
# https://www150.statcan.gc.ca/n1/tbl/csv/13100810-eng.zip

# if (T) {  
#   # packages <- c("magrittr","ggplot2","stringr", "forcats") # forcats, DOES NOT WORK WHEN DEPLOYING IN RSCONNECT!
#   # lapply(packages, library, character.only = TRUE)
#   library(lubridate,  quietly=T); options(lubridate.week.start =  1)
#   library(data.table); options(datatable.print.class=TRUE)
#   library(plotly); library(DT); library(heatmaply);  library(ggpubr)
#   library(stringr); library(forcats) 
#     
#   library(cansim) # (See https://cran.r-project.org/web/packages/cansim) 
#   
#   
#   dateToday <- format(Sys.time(), '%d %B, %Y') %>% dmy; dateToday
#   "%wo%" <- function(x, y) setdiff(x,y) 
#   `%ni%` <-  Negate(`%in%`)
# }


# source ("LTE-vitals-ca.R")
# 1. Read cashed Table 13-10-0810-01 merged with GEO -----

# Canadian Vital Statistics Death (CVSD) Database
# Leading causes of death, total population 
# Provisional weekly death counts, by selected grouped causes of death
# Table: 13-10-0810-01
# Release date: 2021-11-08

# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310081001
# https://www150.statcan.gc.ca/n1/tbl/csv/13100810-eng.zip


if (F) { # 1.a Read and cache live CANSIM data ----
  
  # NB: Works only if StatCan site is up and running.
  # You can test if it is running by clicking here: https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310081001
  
  dt <- cansim::get_cansim("13-10-0810-01") %>%  setDT(dt)
  # Quick view of the data
  dt
  dt %>% names 
  # Remove unneeded columns and simplify values
  # dt[, (names(dt)[c(1,3:20,24)]):=NULL]
  dt <- dt[, c("Date", "GEO", "val_norm", "Cause of death (ICD-10)")]
  dt[, Date := ymd(Date)]; dt$Date %>% max # "2021-10-02"
  dt$GEO %>% unique() %>% sort
  dt[, GEO := gsub(", place of occurrence", "", GEO)]
  
  # Add timestamp to cached file----
  # dt[1, cached := dateToday]
  
  dtTimeStamp <- dt[1][ , lapply(.SD, function(x) NA), .SDcols=names(dt)] [, Date:=dateToday]
  dt <- rbind(dt, dtTimeStamp)
  
  
  saveRDS(dt, paste0("13100810-cached.Rds")) # save locally as compressed Rds file
  fwrite(dt, paste0("13100810-cached.csv"), sep = "\t")
  fwrite(dt[Date >= ymd("2019-09-01")], paste0("13100810-after-20150901-cached.csv"), sep = "\t")
}

if  (F)  { # 1.b Read from https://github.com/open-canada/datasets/statcan ----
  
  # downloader::download("https://github.com/open-canada/cansim-examples/raw/main/13100810-20211206.Rds", "13100810-20211206b.Rds") # order way
  curl::curl_download("https://github.com/open-canada/datasets/raw/main/statcan/13100810.Rds", "13100810.Rds")
  
  dt <- readRDS("13100810.Rds") 
  
  # Or load csv (for just last two years)
  dt <- fread ("https://github.com/open-canada/datasets/raw/main/statcan/13100810.csv") 
}

# 1.c Read local cached copy ----


dt <- readRDS(paste0("13100810-cached.Rds"))
dateCached <- "2021-12-23" %>% ymd
dt <- dt[1:(.N-1)]
# if ( is.na(dt[.N]$GEO) ) {
#   dateCached <- dt[.N]$Date %>% ymd
# 
#   # # Remove last line in Cached Date by reference
#   # dt <- dt %>% dt.rmRow(nrow(dt))   # Much faster and memory efficient than than
#   dt <- dt[1:(.N-1)]
# }


# dt[, GEO := gsub(", place of occurrence", "", GEO)]
dt %>% setDT

dateMax <- dt$Date %>% max (na.rm=T) %>% ymd; dateMax
dt[c(1,.N)]
dt %>% names

choicesGEO <-  dt$GEO %>% unique(); choicesGEO
choicesCauses <- dt$`Cause of death (ICD-10)` %>% unique(); choicesCauses

# dt <- dt[Date >= ymd("2019-09-01")]

# 2. Merge with population, compute rates per million----

if (T) {
  dtGeo <- data.table(
    GEO = c(  "Ontario", "Quebec", "British Columbia", "Alberta",
              "Manitoba", "Saskatchewan", "Nova Scotia", "New Brunswick",
              "Newfoundland and Labrador", "Prince Edward Island", "Northwest Territories", "Nunavut", "Yukon", "Canada"  ),
    population = c( 14826276, 8604495, 5214805, 4442879, 1383765, 1179844, 992055, 789225, 
                    520553, 164318, 45504, 39403, 42986, 38246108 )
  )
  fwrite(dtGeo, "dtGeoCanada.csv", sep = "\t")
} else {
  dtGeo <- fread("dtGeoCanada.csv")
}

dt <- dtGeo[dt, on="GEO"]
dt[, GEO:=fct_relevel(GEO, choicesGEO)]

# 3. Merge with Vaccination data -----

# Read live  or cached data
dtVac <- fread("https://health-infobase.canada.ca/src/data/covidLive/vaccination-coverage-byAgeAndSex-overTimeDownload.csv")
# fwrite(dtVac, "vaccination-coverage-byAgeAndSex-overTimeDownload.csv")
# saveRDS(dtVac, "vaccination-coverage-byAgeAndSex-overTimeDownload.Rds")
#
# downloader::download("https://github.com/open-canada/cansim-examples/raw/main/vaccination-coverage-byAgeAndSex-overTimeDownload-2021-12-07.Rds", "vaccination-coverage-byAgeAndSex-overTimeDownload-2021-12-07.Rds")
# dtVac <- readRDS("vaccination-coverage-byAgeAndSex-overTimeDownload-2021-12-07.Rds")

dtVac
dtVac %>% names()
dtVac <- dtVac[sex == "All sexes", c(2, 4:9)]
dtVac[, week_end := ymd(week_end)]
setnames(dtVac, old = c("prename", "week_end"), new = c("GEO", "Date"))

dtVac %>% names()
colValues <- 5:7
dtVac[, (colValues) := lapply(.SD, as.numeric), .SDcols = colValues]
dtVac[, GEO := fct_relevel(GEO, choicesGEO)]

# get totals for all ages and sexes, and merge  -----

dtVacAllAgesAllSexes <- # because causes don report age ad sex
  dtVac[sex == "All sexes", lapply(.SD, sum, na.rm = T), by = c("GEO", "Date"), .SD = colValues]

# dtAll <- dtVacAllAgesAllSexes[dt, on=.(prename=GEO, week_end=Date)]
dtAll <- dtVacAllAgesAllSexes[dt, on = c("GEO", "Date")]

dtAll[, GEO := fct_relevel(GEO, choicesGEO)]

# 4. Set configurable parameters ----

in0 <- list(
  state = c("Canada"),
  # state = c( "Quebec" , "Ontario", "Alberta", "British Columbia" ),
  cause = choicesCauses[c(2, 5, 12, 15:17)], # Largest groups
  date = c("2019-10-01", as.character(dateToday))
)
input <- in0


if (F) {
  
  # ```{r r.dt0 r.dtAll} -----
  
  
  dt0 <- dtAll[Date >= input$date[1] & Date <= input$date[2] & GEO %in% input$state & as.character(`Cause of death (ICD-10)`) %in% input$cause]
  
  dtAll[, rate := round(1000000 * val_norm / population)]
  dtAll[, numtotal_atleast1dose := round(100 * numtotal_atleast1dose / population)]
  dtAll[, numtotal_fully := round(100 * numtotal_fully / population)]
  
  
  q <- quote(Date >= input$date[1] & Date <= input$date[2] & GEO %in% input$state & as.character(`Cause of death (ICD-10)`) %in% input$cause)
  
  dt0 <- dtAll[eval(q)] # for testing offline
  # r.dtAll()[
  
  
  
  # ```{r r.dt0_wideGeo1 dygraphs} ----
  
  
  region1 <- dt0$GEO %>%
    unique() %>%
    .[1]
  dt0_wideGeo1 <- dt0[Date >= "2020-01-01" & GEO == region1, !c("GEO", "val_norm"), with = T] %>% dcast(... ~ `Cause of death (ICD-10)`, value.var = "rate")
  dt0_wideGeo1
  
  
  dt0_wideGeo1 <- r.dt0_wideGeo1()
  
  setcolorder(dt0_wideGeo1, "Date")
  dts <- as.xts.data.table(dt0_wideGeo1)
  
  dygraph.title(dts)
  
  
  
  
  # 5. Visualize data ----
  
  
  # Plot GEO vertically - allows comparison across Causes
  
  g3 <- ggplot(dt0) +
    guides(col = "none") +
    geom_step(aes(Date, val_norm, col = `Cause of death (ICD-10)`)) +
    facet_grid(GEO ~ `Cause of death (ICD-10)`, scales = "free") +
    labs(
      title = NULL, x = NULL, y = NULL,
      caption = "Source: Statistics Canada - Table 13-10-0810-01"
    )
  g3
  
  g4 <- ggplot(dt0) +
    guides(col = "none") +
    geom_line(aes(Date, numtotal_fully), col = "red") +
    geom_line(aes(Date, numtotal_atleast1dose), col = "red", linetype = 2) +
    facet_grid(GEO ~ ., scales = "free") +
    labs(
      title = NULL, x = NULL, y = NULL,
      caption = "Source: https://health-infobase.canada.ca/covid-19/vaccination-coverage/"
    )
  
  g <- ggpubr::ggarrange(g3, g4, ncol = 2, widths = c(5, 1))
  g
  
}

if (F) {
  # Provisional weekly death counts, by age group and sex1 -----
  id <- "13-10-0768-01" # Age
  dtAge <- readRDS(paste0(id, ".Rds")) %>% setDT()
  dtAge <- dtAge[, c("Date", "GEO", "val_norm", "Age at time of death", "Sex")]
  
  # Provisional weekly estimates of the number of deaths, expected number of deaths and excess mortality, by age group and sex ----
  
  # Used in Provisional deaths and excess mortality in Canada dashboard: https://www150.statcan.gc.ca/n1/pub/71-607-x/71-607-x2021028-eng.htm
  
  id <- "13-10-0792-01" # Age
  dtAge <- readRDS(paste0(id, ".Rds")) %>% setDT()
  dtAge <- dtAge[, c("Date", "GEO", "val_norm", "Age at time of death", "Sex")]
  
  dtAge$GEO %>%
    unique() %>%
    sort()
  dtAge[, GEO := gsub(", place of occurrence", "", GEO)]
  dtAge[, `Age at time of death` := gsub("Age at time of death, ", "", `Age at time of death`)]
  # dtAge[, GEO:=str_remove_overlap(GEO)][]
  
  dtTimeStamp <- dtAge[.1][, lapply(.SD, function(x) NA), .SDcols = names(dtAge)][, Date := dateToday]
  dtAge <- rbindlist(list(dtAge, dtTimeStamp))
  
  dtAge <- readRDS(paste0(id, ".Rds")) %>% setDT()
  if (is.na(dtAge[.N]$GEO)) {
    dateCached <- dtAge[.N]$Date %>% ymd()
    dtAge <- dtAge[1:(.N - 1)]
  }
  
  setnames(dtAge, old = c("Age at time of death", "Sex"), new = c("age", "sex"))
  
  dtAge[, GEO := fct_relevel(GEO, choicesGEO)]
  
  dtAge$sex %>% unique() # factor, all others are char
  dtVac$sex %>% unique()
  dtAge$sex %>% str()
  dtVac$sex %>% str()
  
  dtAge$age %>% unique()
  dtVac$age %>% unique()
  dtAge$age %>% str()
  dtVac$age %>% str()
  
  
  g1 <- dtAge[Date >= ymd("2019-12-01") & GEO %in% (input$state %wo% "Canada") & sex != "Both sexes" & age != "all ages"] %>%
    ggplot() +
    theme(legend.position = "bottom") +
    # guides(col="none") +
    geom_line(aes(Date, val_norm, col = sex)) +
    facet_grid(GEO ~ age, scales = "free") +
    # facet_grid(age ~ GEO, scales = "free") +
    labs(
      title = NULL, x = NULL, y = "Deaths in a million / week",
      caption = "Source: Statistics Canada - Table 13-10-0768-01"
    )
  g1
  
  ggsave("ts-age2.png", width = 1800, height = 1500, units = "px")
  
}

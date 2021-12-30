


# 6. Correlation analysis ----

# 6a Smooth data with convolution and compute derivatives ----

convolution_window <- 3 # 5
difference_window <- 1
setkey(dtAll, Date)
dtAll[, val_norm_smoothed := frollmean(val_norm, convolution_window, align = "right", fill = 0), by = .(GEO, `Cause of death (ICD-10)`)]

dtAll[, diff_death := val_norm1 - shift(val_norm1, difference_window), by = .(GEO, `Cause of death (ICD-10)`)]
dtAll[, diff_fully := numtotal_fully - shift(numtotal_fully, difference_window), by = .(GEO, `Cause of death (ICD-10)`)]


dt0 <- dtAll[Date >= "2021-05-01" & GEO == "Canada" &
               `Cause of death (ICD-10)` == choicesCauses[16]] # CAUSE="Blank (NA)"

# 6b cor.test ----

# .. on raw smoothed values -----

dt0 %$%
  cor.test(val_norm_smoothed, numtotal_fully, method = "kendall")

# cor = 0.7553008, p-value = 0.000118 # pearson
# tau = 0.9894737 # kendall
# rho = 0.9984962 # spearman

# .. on difference values -----

dt0 %$% # CAUSE="Blank (NA)"
  cor.test(diff_death, diff_fully, method = "kendall")

# 6c heatmaply ----


# 8 Other COVID / Vax data -----




# Alternative source:
# dtVac2 <- fread("vaccination-coverage-byVaccineType-2021-12-07.csv", stringsAsFactors = T)


# strVac <- "vaccination-coverage-byAgeAndSex-overTimeDownload-2021-12-07.csv"
# strVac <- "https://health-infobase.canada.ca/src/data/covidLive/vaccination-coverage-byAgeAndSex-overTimeDownload.csv"
# fwrite(dtVac, "vaccination-coverage-byAgeAndSex-overTimeDownload-2021-12-07.csv")
# dtVac <- fread(strVac, stringsAsFactors = F)
# saveRDS(dtVac, "vaccination-coverage-byAgeAndSex-overTimeDownload-2021-12-07.Rds")
dtVac <- readRDS("vaccination-coverage-byAgeAndSex-overTimeDownload-2021-12-07.Rds")



13 - 10 - 0810 - 01
dt <- cansim::get_cansim("13-10-0714") %>% setDT(dt)


# https://health-infobase.canada.ca/covid-19/vaccine-administration/ -----

# Doses administered nationally
# url = "https://health-infobase.canada.ca/src/data/covidLive/vaccination-administration.csv"
#
# Number of COVID-19 vaccine doses administered in Canada by dose number and report week, December 18, 2021
#
# url ="https://health-infobase.canada.ca/src/data/covidLive/vaccination-administration-bydosenumber2.csv"
#
#
# Cumulative number of COVID-19 vaccine doses administered in Canada by vaccine product and dose number, as of December 18, 2021
#
# url = "https://health-infobase.canada.ca/src/data/covidLive/vaccination-administration-bydosenumber2.csv"
#
#
# `National vaccination coverage` <- "https://health-infobase.canada.ca/src/data/covidLive/vaccination-coverage-map.csv"
#
#
#
# Figure 4.
# Cumulative number
# of people who have received
# at least 1 dose
# of a COVID-19 vaccine in
# Canada
# by age group and sex, December 18, 2021
#
# url <- "https://health-infobase.canada.ca/src/data/covidLive/vaccination-coverage-byAgeAndSexDownload.csv"
#
# Figure 5.
# Cumulative percent
# of
# people
# who have received
# at least 1 dose
# of a COVID-19 vaccine in
# Canada
# by age group and report week, December 18, 2021
#
# url <- "https://health-infobase.canada.ca/src/data/covidLive/vaccination-coverage-byAgeAndSex-overTimeDownload.csv"
#
#
#
#
# COVID-19 daily epidemiology update
# url <- "https://health-infobase.canada.ca/src/data/covidLive/covid19-download.csv"
#
#
#
# Figure 2. Weekly variant breakdown Updated: December 24, 2021, 4 pm EDT
# url <- "https://health-infobase.canada.ca/src/data/covidLive/covid19-epiSummary-variants.csv"


# 9. Other cansim data----


library(cansim)
o <- search_cansim_cubes("Vital") %>% setDT()
o
fwrite(o, "cansim-Vitals.csv")


o <- search_cansim_cubes("Vaccin") %>% setDT()
o
fwrite(o, "cansim-Vaccin.csv")
# 5 old
# 13100082	105-0245	2003-01-01	2003-01-01	2017-03-06T13:30:00Z	TRUE	13	3226	13		Geography, Age group, Sex, Influenza immunization, Characteristics


o <- search_cansim_cubes("Hospital") %>% setDT()
o
fwrite(o, "cansim-Hospital.csv")


o <- search_cansim_cubes("Covid") %>% setDT()
o # 51
fwrite(o, "cansim-Covid.csv")

# from BOOK list

dt <- cansim::get_cansim("13-10-0810-01") %>% setDT(dt)

# Provisional weekly death counts, by age group and sex
dt <- cansim::get_cansim("13-10-0768-01") %>% setDT(dt)




in0 <- list(
  cansim = "13-10-0768-01"
)
input <- in0
input$cansim <- "13-10-0768-01"
input$cansim <- "13-10-0783-01"

input$cansim <- "13-10-0427-01"
input$cansim <- "13-10-0395-01"

input$cansim <- "13-10-0415-01"
input$cansim <- "13-10-0418-01"
input$cansim <- "13-10-0415-01"
input$cansim <- "13-10-0768-01"


# Crude birth rate, age-specific fertility rates and total fertility rate (live births)
# Table: 13-10-0418-01 (formerly: CANSIM 102-4505)
#
# Live births, by month
# Table: 13-10-0415-01 (formerly: CANSIM 102-4502)
#
# Live births, by age of mother
# Table: 13-10-0416-01 (formerly: CANSIM 102-4503)


dt <- cansim::get_cansim(input$cansim) %>% setDT(dt)


dt[, Date := ymd(Date)]
dt$Date %>% max(na.rm = T)
dt$Date %>% unique()
dt %>% names()
dt$Date %>% min(na.rm = T)
saveRDS(dt, paste0(input$cansim, ".Rds"))



# Quick view of the data
# dt

# Remove unneeded columns and simplify values
# dt[, (names(dt)[c(1,3:20,24)]):=NULL]
dt <- dt[, c("Date", "GEO", "val_norm", "Cause of death (ICD-10)")]
dt <- dt[, c("Date", "GEO", "val_norm", "Cause of death (ICD-10)")]

#


# Vital Statistics - Birth Database
# https://www150.statcan.gc.ca/n1/en/surveys/3231


# 3. Crude birth rate, age-specific fertility rates and total fertility rate (live births)
# Table: 13-10-0418-01 (formerly: CANSIM 102-4505)

# 9. Live births, by month
# Table: 13-10-0415-01 (formerly: CANSIM 102-4502)
#
# Live births, by age of mother
# Table: 13-10-0416-01 (formerly: CANSIM 102-4503)

# Fertility rates, women aged 15 to 19 years (per 1,000 women)
# Table: 13-10-0418-02

# 8. Live births, by place of residence of mother
# Table: 13-10-0414-01 (formerly: CANSIM 102-4501)


# DEATHS


# 17. Fetal deaths (20 weeks or more of gestation) and late fetal deaths (28 weeks or more of gestation)
# Table: 13-10-0427-01 (formerly: CANSIM 102-4514)
# Perinatal mortality (late fetal deaths and early neonatal deaths)
# Table: 13-10-0714-01 (formerly: CANSIM 102-0508)
#
# 21. Leading causes of death, infants
# Table: 13-10-0395-01 (formerly: CANSIM 102-0562)
#
# 27. Leading causes of death, total population (age standardization using 2011 population)

# Provisional weekly death counts, by selected grouped causes of death
# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310081001



if (F) {
  
  # . Other StatCan data ----
  # ... by age group and sex1 -----
  id <- "13-10-0768-01" # Age
  dtAge <- readRDS(paste0(id, ".Rds")) %>% setDT()
  dtAge <- dtAge[, c("Date", "GEO", "val_norm", "Age at time of death", "Sex")]
  
  # ... estimates of the number of deaths, expected number of deaths and excess mortality, by age group and sex ----
  
  # Used in Provisional deaths and excess mortality in Canada dashboard: https://www150.statcan.gc.ca/n1/pub/71-607-x/71-607-x2021028-eng.htm
  
  id <- "13-10-0792-01" # Age
  dtAge <- readRDS(paste0(id, ".Rds")) %>% setDT()
  dtAge <- dtAge[, c("Date", "GEO", "val_norm", "Age at time of death", "Sex")]
  
  dtAge$GEO %>%
    unique() %>%
    sort()
  
  dtAge[, GEO := gsub(", place of occurrence", "", GEO)]
  dtAge[, `Age at time of death` := gsub("Age at time of death", "", `Age at time of death`) ]
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

# cansim13100810-correlations-with-vaccinations.R 

if (T) { # 0. global settings ----
    require(magrittr); library(ggplot2); library(lubridate)
    library(data.table); options(datatable.print.class=TRUE)
    dateToday <- format(Sys.time(), '%d %B, %Y') %>% dmy; dateToday
    "%wo%" <- function(x, y) setdiff(x,y) 
    `%ni%` <-  Negate(`%in%`)
}

# 1. Read CANSIM Table: 13-10-0810-01 data -----

# Canadian Vital Statistics Death (CVSD) Database

# Leading causes of death, total population 
# Provisional weekly death counts, by selected grouped causes of death

# Table: 13-10-0810-01
# Release date: 2021-11-08

# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310081001
# https://www150.statcan.gc.ca/n1/tbl/csv/13100810-eng.zip


if (F) {
    # Read StatCan data directly from StatCan site. 
    # NB: Works only if StatCan site is up and running.
    # You can test it by clicking in the links above.
    
    library(cansim) # (See https://cran.r-project.org/web/packages/cansim) 
    dt <- cansim::get_cansim("13-10-0810-01") %>%  setDT(dt)
    dt[, Date := ymd(Date)]
    dt <- dt[Date >= ymd("2019-09-01")]
    saveRDS(dt, paste0("13100810-", dateToday, ".Rds")) # save locally as compressed file
    fwrite(dt, paste0("13100810-after-20190901.csv"))
}

# Read cached 13-10-0810-01 data  

# Load all data (since 2010)
# downloader::download("https://github.com/open-canada/cansim-examples/raw/main/13100810-20211206.Rds", "13100810-20211206b.Rds") # order way
curl::curl_download("https://github.com/open-canada/datasets/raw/main/statcan/13100810-20211206.Rds", "13100810-20211206a.Rds")
dt <- readRDS("13100810-20211206.Rds") 

# Or load just last two years
dt <- fread ("https://github.com/open-canada/datasets/raw/main/statcan/13100810-after-20190901.csv") 


# Quick view of the data
dt
dt %>% names 

# Remove unneeded columns and simplify values
dt[, (names(dt)[c(1,3:20,24)]):=NULL]
dt[, Date := ymd(Date)]
dt <- dt[Date >= ymd("2019-09-01")]
dt$GEO %>% unique() %>% sort
dt[, GEO := gsub(", place of occurrence", "", GEO)]

# Get familiar with causes names:

choicesGEO <-  dt$GEO %>% unique(); choicesGEO # Ordered by location, used for displaying
dt[, GEO:=fct_relevel(GEO, choicesGEO)]
choicesCauses <- dt$`Cause of death (ICD-10)` %>% unique(); choicesCauses


# 2. Add population, compute rates per million----

dtGeo <- data.table(
    GEO = c(  "Ontario", "Quebec", "British Columbia", "Alberta",
              "Manitoba", "Saskatchewan", "Nova Scotia", "New Brunswick",
              "Newfoundland and Labrador", "Prince Edward Island", "Northwest Territories", "Nunavut", "Yukon", "Canada"  ),
    population = c( 14826276, 8604495, 5214805, 4442879, 1383765, 1179844, 992055, 789225, 
                    520553, 164318, 45504, 39403, 42986, 38246108 )
)
dtGeo[, GEO:=fct_relevel(GEO, choicesGEO)]

dt <- dtGeo[dt, on="GEO"]
# dt [, rate:=round(1000000*val_norm/population)]

# 3. Merge it with Vaccination data -----

# Read live  or cached data 
dtVac <- fread ("https://health-infobase.canada.ca/src/data/covidLive/vaccination-coverage-byAgeAndSex-overTimeDownload.csv") 
# fwrite(dtVac, "vaccination-coverage-byAgeAndSex-overTimeDownload.csv")
# saveRDS(dtVac, "vaccination-coverage-byAgeAndSex-overTimeDownload.Rds")
# 
# downloader::download("https://github.com/open-canada/cansim-examples/raw/main/vaccination-coverage-byAgeAndSex-overTimeDownload-2021-12-07.Rds", "vaccination-coverage-byAgeAndSex-overTimeDownload-2021-12-07.Rds")
# dtVac <- readRDS("vaccination-coverage-byAgeAndSex-overTimeDownload-2021-12-07.Rds")

dtVac
dtVac %>% names
dtVac <- dtVac[sex=="All sexes", c(2, 4:9)]
dtVac[, week_end := ymd(week_end)]
setnames(dtVac, old=c("prename", "week_end"), new=c("GEO", "Date"))

dtVac %>% names
colValues <- 5:7
dtVac[, (colValues):=lapply(.SD, as.numeric), .SDcols=colValues]
dtVac[, GEO:=fct_relevel(GEO, choicesGEO)]

# get totals for all ages and sexes, and merge

dtVacAllAges <- 
    dtVac [, lapply(.SD, sum, na.rm=T), by=c("GEO", "Date"), .SD=colValues]

dtAll <- dtVacAllAges[dt, on=c("GEO", "Date")]

# 4. Set configurable parameters ----

in0 <- list(
    state =  c( "Canada" ),
    #state = c( "Quebec" , "Ontario", "Alberta", "British Columbia" ),
    cause = choicesCauses[c(2,5, 12, 15:17)], # Largest groups
    date = c("2019-10-01", as.character(dateToday))
); input <- in0

# 5. Visualize data ----


dt0 <- dtAll[ Date >= input$date[1] &  Date <= input$date[2] & GEO %in% input$state & as.character(`Cause of death (ICD-10)`) %in% input$cause  ]


# Plot GEO vertically - allows comparison across Causes

g3 <- ggplot(dt0) +    guides(col = "none") +
    geom_step(aes(Date, val_norm, col = `Cause of death (ICD-10)`)) +
    facet_grid(GEO ~ `Cause of death (ICD-10)`, scales = "free") +
    labs( title = NULL, x = NULL, y = NULL,
          caption = "Source: Statistics Canada - Table 13-10-0810-01"  )
g3

g4 <-  ggplot(dt0) +   guides(col="none") +
    
    geom_line(aes(Date, numtotal_fully), col = "red") +
    geom_line(aes(Date, numtotal_atleast1dose), col = "red", linetype=2) + 
    facet_grid( GEO ~ . , scales = "free") +
    labs( title = NULL,         x = NULL,        y = NULL, 
          caption = "Source: https://health-infobase.canada.ca/covid-19/vaccination-coverage/"  )

g <- ggpubr::ggarrange(g3, g4, ncol = 2, widths=c(5,1))
g

# Plot Causes vertically - allows comparison across GEO

# g1 <- ggplot(dt0) +  theme(legend.position = "bottom") +
#     geom_step(aes(Date, val_norm, col = `Cause of death (ICD-10)`)) +
#     facet_grid(`Cause of death (ICD-10)` ~ GEO, scales = "free") +
#     labs(  title = NULL, x = NULL, y = NULL,
#            caption = "Source: Statistics Canada - Table 13-10-0810-01"    )
# g1


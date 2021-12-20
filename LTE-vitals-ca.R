# cansim13100810-correlations-with-vaccinations.R 

#0. Global settings ----

if (T) {  
    packages <- c("magrittr","ggplot2","stringr", "forcats")
    lapply(packages, library, character.only = TRUE)
    library(lubridate,  quietly=T); options(lubridate.week.start =  1)
    library(data.table); options(datatable.print.class=TRUE)
    
    dateToday <- format(Sys.time(), '%d %B, %Y') %>% dmy; dateToday
    "%wo%" <- function(x, y) setdiff(x,y) 
    `%ni%` <-  Negate(`%in%`)
}

# 1. Read CANSIM Table 13-10-0810-01, merged it with GEO -----

# Canadian Vital Statistics Death (CVSD) Database
# Leading causes of death, total population 
# Provisional weekly death counts, by selected grouped causes of death
# Table: 13-10-0810-01
# Release date: 2021-11-08

# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310081001
# https://www150.statcan.gc.ca/n1/tbl/csv/13100810-eng.zip


if (F) { # 1.a Save cached from live CANSIM data ----
    
    # Read StatCan data directly from StatCan site. 
    # NB: Works only if StatCan site is up and running.
    # You can test if it is running by clicking here: https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1310081001
    
    library(cansim) # (See https://cran.r-project.org/web/packages/cansim) 
    dt <- cansim::get_cansim("13-10-0810-01") %>%  setDT(dt)
    # Quick view of the data
    dt
    dt %>% names 
    # Remove unneeded columns and simplify values
    # dt[, (names(dt)[c(1,3:20,24)]):=NULL]
    dt <- dt[, c("Date", "GEO", "val_norm", "Cause of death (ICD-10)")]
    dt[, Date := ymd(Date)]
    dt$GEO %>% unique() %>% sort
    dt[, GEO := gsub(", place of occurrence", "", GEO)]
    
    saveRDS(dt, paste0("13100810-", dateToday, ".Rds")) # save locally as compressed Rds file
    fwrite(dt, paste0("13100810-", dateToday, ".csv"), sep = "\t")
    fwrite(dt[Date >= ymd("2019-09-01")], paste0("13100810-", dateToday, "-after-20150901.csv"), sep = "\t")
}

if  (F)  { # 1.b Read from https://github.com/open-canada/datasets/statcan ----
    
    # downloader::download("https://github.com/open-canada/cansim-examples/raw/main/13100810-20211206.Rds", "13100810-20211206b.Rds") # order way
    curl::curl_download("https://github.com/open-canada/datasets/raw/main/statcan/13100810.Rds", "13100810.Rds")
    dt <- readRDS("13100810.Rds") 
    
    # Or load csv (for just last two years)
    dt <- fread ("https://github.com/open-canada/datasets/raw/main/statcan/13100810.csv") 
}

# 1.c Read local cached copy ----

dateCached <- ymd("20211218")
dt <- readRDS(paste0("13100810-", dateCached, ".Rds"))
# dt[, GEO := gsub(", place of occurrence", "", GEO)]
dt %>% setDT

dateMax <- dt$Date %>% max (na.rm=T) %>% ymd; dateMax
dt[c(1,.N)]
dt %>% names

choicesGEO <-  dt$GEO %>% unique(); choicesGEO
choicesCauses <- dt$`Cause of death (ICD-10)` %>% unique(); choicesCauses

dt <- dt[Date >= ymd("2019-09-01")]

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

dtGeo[, GEO:=fct_relevel(GEO, choicesGEO)]

dt <- dtGeo[dt, on="GEO"]
dt [, rate:=round(1000000*val_norm/population)]


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


if (F) {
    
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
    
    
    # 6. Correlation analysis ----
    
    # 6a Smooth data with convolution and compute derivatives ----
    
    convolution_window =3 #5
    difference_window=1
    setkey(dtAll,Date)
    dtAll[, val_norm_smoothed:= frollmean(val_norm, convolution_window, align = "right", fill=0),  by=.(GEO, `Cause of death (ICD-10)`)]
    
    dtAll[ , diff_death:= val_norm1 - shift(val_norm1, difference_window), by=.(GEO, `Cause of death (ICD-10)`)]
    dtAll[ , diff_fully := numtotal_fully - shift(numtotal_fully, difference_window), by=.(GEO, `Cause of death (ICD-10)`)]
    
    
    dt0 <- dtAll[Date >= "2021-05-01" & GEO=="Canada" &
                     `Cause of death (ICD-10)` == choicesCauses [16]]  # CAUSE="Blank (NA)"
    
    # 6b cor.test ----
    
    # .. on raw smoothed values -----
    
    dt0 %$% 
        cor.test(val_norm_smoothed, numtotal_fully, method="kendall")
    
    # cor = 0.7553008, p-value = 0.000118 # pearson
    # tau = 0.9894737 # kendall
    # rho = 0.9984962 # spearman
    
    # .. on difference values -----
    
    dt0 %$% # CAUSE="Blank (NA)"
        cor.test(diff_death, diff_fully, method="kendall")
    
    # 6c Plot correlations
    
    
    library("heatmaply")
    
    heatmaply(mtcars)
    
    
    heatmaply_cor(
        cor(mtcars),
        xlab = "Features",
        ylab = "Features",
        k_col = 2,
        k_row = 2
    )
    
    
    r <- cor(mtcars)
    ## We use this function to calculate a matrix of p-values from correlation tests
    ## https://stackoverflow.com/a/13112337/4747043
    cor.test.p <- function(x){
        FUN <- function(x, y) cor.test(x, y)[["p.value"]]
        z <- outer(
            colnames(x), 
            colnames(x), 
            Vectorize(function(i,j) FUN(x[,i], x[,j]))
        )
        dimnames(z) <- list(colnames(x), colnames(x))
        z
    }
    p <- cor.test.p(mtcars)
    
    heatmaply_cor(
        r,
        node_type = "scatter",
        point_size_mat = -log10(p), 
        point_size_name = "-log10(p-value)",
        label_names = c("x", "y", "Correlation")
    )
    
    
    heatmaply(
        mtcars, 
        scale = "column",
        # normalize(mtcars),
        # percentize(mtcars),
        xlab = "Features",
        ylab = "Cars", 
        
        main = "Data transformation using 'scale'"
    )
    
    
}

# 9. Collection of  other Open data from StatCan ----


library(cansim)
o <- search_cansim_cubes("Vital") %>% setDT
o 
fwrite(o, "cansim-Vitals.csv")

o <- search_cansim_cubes("Vaccin") %>% setDT
o 
fwrite(o, "cansim-Vaccin.csv")

o <- search_cansim_cubes("Hospital") %>% setDT
o 
fwrite(o, "cansim-Hospital.csv")


o <- search_cansim_cubes("Covid") %>% setDT
o 
fwrite(o, "cansim-Covid.csv")

# from BOOK list

dt <- cansim::get_cansim("13-10-0810-01") %>%  setDT(dt)

# Provisional weekly death counts, by age group and sex
dt <- cansim::get_cansim("13-10-0768-01") %>%  setDT(dt)




in0 <- list(
    cansim="13-10-0768-01"
); 
input <- in0
input$cansim =   "13-10-0768-01"
input$cansim =  "13-10-0783-01"

input$cansim =   "13-10-0427-01"
input$cansim =   "13-10-0395-01"

input$cansim =   "13-10-0415-01"
input$cansim =   "13-10-0418-01"
input$cansim =   "13-10-0415-01"
input$cansim =   "13-10-0768-01"


Crude birth rate, age-specific fertility rates and total fertility rate (live births)
Table: 13-10-0418-01 (formerly: CANSIM 102-4505)

Live births, by month
Table: 13-10-0415-01 (formerly: CANSIM 102-4502)

Live births, by age of mother
Table: 13-10-0416-01 (formerly: CANSIM 102-4503)


dt <- cansim::get_cansim(input$cansim ) %>%  setDT(dt)


dt[, Date := ymd(Date)]
dt$Date %>% max(na.rm = T)
dt$Date %>% unique()
dt %>% names 
dt$Date %>% min(na.rm = T)
saveRDS(dt, paste0(input$cansim, ".Rds")) 



# Quick view of the data
# dt

# Remove unneeded columns and simplify values
# dt[, (names(dt)[c(1,3:20,24)]):=NULL]
dt <- dt[, c("Date", "GEO", "val_norm", "Cause of death (ICD-10)")]
dt <- dt[, c("Date", "GEO", "val_norm", "Cause of death (ICD-10)")]

dt$GEO %>% unique() %>% sort
dt[, GEO := gsub(", place of occurrence", "", GEO)]

saveRDS(dt, paste0("13100810-", dateToday, ".Rds")) # save locally as compressed Rds file
fwrite(dt, paste0("13100810-", dateToday, ".csv"), sep = "\t")
fwrite(dt[Date >= ymd("2019-09-01")], paste0("13100810-", dateToday, "-after-20150901.csv"), sep = "\t")
}
# This will be a shiny app that will:
# 1) grab a bunch of data series from quandl(?) (ideally I'll eventually cache this as a... json?)
# 2) find random sets that are correlated.
# 3) Interactivity will be in the form of: 
#       Next button
#       Potentially a 'detrend' button to see whether those relationships disappear?
library(Quandl)
library(ggplot2)
library(data.table)
library(stringr)
library(scales)


pullData <- function() {
  Quandl.auth(readLines('quandl.config', warn = FALSE)[1])
  
  lst.symbols <- list(
    list(symbol = "GOOG/NASDAQ_AAPL", name = 'Apple Stock Price ($/Share)', colname = 'Close'),
    list(name = 'U.S. Ending Stocks of Crude Oil (thousand bbl)', symbol = "DOE/MCRSTUS1", colname = "Value"),
    list(name = "EUR to USD \nExchange Rate", symbol = "CURRFX/EURUSD", colname = "Rate"),
    list(name = "10 Year Treasury \nYield Curve (%)", symbol = "USTREASURY/YIELD", colname = "10 Yr"),
    list(name = "Central American and Ecuador \nBanana Price (USD/mt FOB)", symbol = "ODA/PBANSOP_USD", colname = "Value"),
    list(name = "Year over Year Inflation \nRate in Argentina (%)", symbol = "RATEINF/INFLATION_ARG", colname = "CPI"),
    list(name = "Bitcoin Market Price to USD", symbol = 'BCHAIN/MKPRU', colname = "Value"),
    list(name = "Zillow Foreclosure Resales \nin Arizona (%)", symbol = "ZILLOW/MSTATE_PCTTRANSACTIONSTHATAREPREVIOUSLYFORECLOSUREDHOMES_ALLHOMES_ARIZONA", colname = "Value"),
    list(name = "Civilian US Unemployment Rate (%)", symbol = "FRED/UNRATE", colname = "Value"),
    list(name = "German Unemployment Rate (%)", symbol = "BCB/3785", colname = "Value"),
    list(name = "No. 1 Hard Red Winter Wheat\nFOB Gulf of Mexico ($/mt)", symbol = "ODA/PWHEAMT_USD", colname = "Value"),
    list(name = "Brazilian exports of unmilled maize\n(US$)", symbol = 'BCB/20213', colname = "Value"),
    list(name = "US Population\n(Thousands)", symbol = "FRED/POPTHM", colname = "Value"),
    list(name = "Yahoo! Reach Per Million\nPer Alexa", symbol = "ALEXA/YAHOO", colname = "Page Views Per Million"),
    list(name = "Median Pre-Money Valuations\nFor Series A Funded Startups\n($M USD)", symbol = "COOLEY/VC_VALUE_BY_SERIES", colname = "Series A"),
    list(name = "Estimated number of births in Canada\n(Thousand per quarter)", symbol = "CANSIM/053_0001_CANADA", colname = "Births")
    
  )
  
  getQuandlData <- function(lst.symbol) {
    dt.data <- data.table(Quandl(lst.symbol[['symbol']]))
    setnames(dt.data, tolower(names(dt.data)))
    dt.data <- dt.data[, c('date',tolower(lst.symbol[['colname']])), with=FALSE]
    setnames(dt.data, c('date','value'))
    dt.data[, date:=as.character(date)]
    dt.data[, name:=lst.symbol[['name']]]
    return(dt.data)
  }
  
  dt.data <- rbindlist(lapply(lst.symbols, getQuandlData))
  
  # Aggregate everything to monthly level
  dt.data <- dt.data[, list(value = mean(value,na.rm=T)), by=list(name, date = paste0(substr(date,1,8), '01'))]
  #lst.data <- list()
  #lst.data[] <- dt.data
  
  #dt.data <- rbindlist(lst.data)
  return(dt.data)
}

# not sure where or how to save the data yet. 
saveData <- function() {
  dt.data <- pullData()
  saveRDS(dt.data, file = 'data.rds', compress = TRUE)
  return(TRUE)
}

# similarly unsure.
retrieveData <- function() {
  dt.data <- readRDS('data.rds')
  return(dt.data)
}


#dt.data <- pullData()
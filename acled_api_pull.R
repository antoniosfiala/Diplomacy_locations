###########################################################
##                                                       ##
##       Function for ingesting from ACLED API           ##
##                                                       ##
###########################################################

# define function

getACLED <- function(country = NULL, year = NULL) {
  
  # Wrap country name with quotes.
  # Normally ACLED returns only 500 records per request.
  # This function overides that constraint and returns
  # all records matching your country and year parameters.
  # This may occasionally exceed ACLED API memory limits and
  # throw an error.
  # If you specify a country and no year, you are requesting
  # all records of that country. If you get an error, you
  # have likely exceeded the memory limit.
  # Add a year to your request and see if you get data back.
  # If you do not specifiy either argument, this function
  # returns the 10,000 most recent records from ACLED.
  
  # Required packages: jsonlite, data.table, lubridate, stringr
  
  library(jsonlite)
  library(data.table)
  library(lubridate)
  library(stringr)
  library(ggplot2)
  
  # the base url for the api, your argument parameters will build on this
  
  myurl <- "https://api.acleddata.com/acled/read?"
  
  # check user argument parameters and build uri endpoint
  
  # first replace spaces
  country2 <- str_replace_all(country, " ", "%20")
  
  if(is.null(country2) & is.null(year)) {
    myurl <- "https://api.acleddata.com/acled/read?&limit=10000"
  } else if (!is.null(country2) & is.null(year)) {
    myurl <- paste0(myurl, "&country=", country2, "&country_where=%3D", "&limit=0")
  } else if (is.null(country2) & !is.null(year)) {
    myurl <- paste0(myurl, "&year=", year, "&limit=0")
  } else {
    myurl <- paste0(myurl, "&country=", country2, "&country_where=%3D", "&year=", year, "&limit=0")
  }
  
  # print resulting api url so user can see it in console
  
  message(paste0("the uri for ", country, " was ", myurl))
  
  # go get the data! the api returns a list
  
  acledEvents <- fromJSON(myurl)
  
  # select just the data from the returned list
  
  acledEvents <- as.data.table(acledEvents[3])
  
  # work on the columns a bit
  
  # drop 'data.' from all column names
  names(acledEvents) <- str_replace_all(names(acledEvents), 'data.', '')
  # convert event_date column to date class
  acledEvents[, event_date := ymd(event_date)]
  # convert latitude column to double(numeric) class
  acledEvents[, latitude := as.double(latitude)]
  # convert longitude column to double(numeric) class
  acledEvents[, longitude := as.double(longitude)]
  # convert fatalities column to double(numeric) class
  acledEvents[, fatalities := as.numeric(fatalities)]
  
  message("Done")
  return(acledEvents)
  
}

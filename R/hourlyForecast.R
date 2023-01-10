#' @title hourlyForecast
#'
#' @description This function will download hourly forecast data from weather.gov for a given lat long.
#'
#' @param lat Latitude of the site of interest
#' @param long Longitude of the site of interest
#'
#' @return A data frame object that contains a forecasted rainfall (mm)
#'  temperature (celsius), dewpoint (celsius), and relative humidity (percent).
#'
#' @examples
#' hourlyForecast(lat = 38.966754, long = -76.569252)
#'
#' @export
hourlyForecast <- function(lat,long){
  #Retrieve JSON file for specified lat and long from Weather.gov
  withRestarts({
    tryCatch(
      {
        forecast <- RJSONIO::fromJSON(paste0("https://api.weather.gov/points/",lat,",",long))$properties$forecastGridData
        message("Successfully retrieved JSON file for specified lat and long from Weather.gov")
      },
      warning=function(e) {invokeRestart("logger", level="warning", message=e ) },
      error=function(e) {invokeRestart("logger", level="error", message=e ) }
    )
  },
  logger = function(level,message) {
    message(date()," [",level,"]: ",message[['message']])
  }
  )

  #Retrieve JSON grid prediction file from Weather.gov
  withRestarts({
    tryCatch(
      {
        gridjson_file <- RJSONIO::fromJSON(forecast)
        message("Successfully retrieved JSON grid prediction file from Weather.gov")
      },
      warning=function(e) {invokeRestart("logger", level="warning", message=e ) },
      error=function(e) {invokeRestart("logger", level="error", message=e ) }
    )
  },
  logger = function(level,message) {
    message(date()," [",level,"]: ",message[['message']])
  }
  )

  gridjson_file <- lapply(gridjson_file,function(x){
    x[sapply(x,is.null)]<-NA
    unlist(x)
  })


  raindf <- as.data.frame(do.call("cbind",gridjson_file[-c(1:4)])) %>%
    dplyr::mutate(colNam = rownames(.)) %>%
    magrittr::set_rownames(NULL) %>%
    dplyr::filter(grepl("quantitativePrecipitation",colNam) & !grepl("quantitativePrecipitation.uom",colNam)) %>%
    dplyr::mutate(group = as.numeric(stringr::str_extract_all(colNam,"[0-9]+")),
                  group = ifelse(is.na(group),0,group),
                  colNam = stringr::str_replace_all(colNam,".[0-9]+",""))%>%
    dplyr::select(c(group,colNam, properties)) %>%
    tidyr::pivot_wider(names_from = colNam,values_from = properties) %>%
    dplyr::mutate(predictInt=as.integer(gsub("[^0-9.-]", "",stringr::str_sub(quantitativePrecipitation.values.validTime, start= -4))),
                  dateTime = stringr::str_sub(quantitativePrecipitation.values.validTime, end= -12),
                  dateTime = as.POSIXct(gsub("T", " ", dateTime),tz="UTC")) %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(dateTime= seq(dateTime,dateTime+lubridate::hours(predictInt-1),
                                   by = '1 hour'),
                     precip_mm = as.numeric(quantitativePrecipitation.values.value)) %>%
    dplyr::add_count(., group) %>%
    dplyr::mutate(precip_mm = precip_mm/n) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(group,n)) %>%
    lubridate::with_tz(dateTime, tzone = "America/New_York")

  tempdf <- as.data.frame(do.call("cbind",gridjson_file[-c(1:4)])) %>%
    dplyr::mutate(colNam = rownames(.)) %>%
    magrittr::set_rownames(NULL) %>%
    dplyr::filter(grepl("temperature.values",colNam)) %>%
    dplyr::mutate(group = as.numeric(stringr::str_extract_all(colNam,"[0-9]+")),
                  group = ifelse(is.na(group),0,group),
                  colNam = stringr::str_replace_all(colNam,".[0-9]+",""))%>%
    dplyr::select(c(group,colNam, properties)) %>%
    tidyr::pivot_wider(names_from = colNam,values_from = properties) %>%
    dplyr::mutate(predictInt=as.integer(gsub("[^0-9.-]", "",stringr::str_sub(temperature.values.validTime, start= -4))),
                  dateTime = stringr::str_sub(temperature.values.validTime, end= -12),
                  dateTime = as.POSIXct(gsub("T", " ", dateTime),tz="UTC")) %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(dateTime= seq(dateTime,dateTime+lubridate::hours(predictInt-1),
                                   by = '1 hour'),
                     temperature_degC = as.numeric(temperature.values.value)) %>%  # need to interpolate here
    dplyr::ungroup() %>%
    dplyr::select(-c(group)) %>%
    lubridate::with_tz(dateTime, tzone = "America/New_York")

  relativeHumiditydf <- as.data.frame(do.call("cbind",gridjson_file[-c(1:4)])) %>%
    dplyr::mutate(colNam = rownames(.)) %>%
    magrittr::set_rownames(NULL) %>%
    dplyr::filter(grepl("relativeHumidity",colNam)  & !grepl("relativeHumidity.uom",colNam)) %>%
    dplyr::mutate(group = as.numeric(stringr::str_extract_all(colNam,"[0-9]+")),
                  group = ifelse(is.na(group),0,group),
                  colNam = stringr::str_replace_all(colNam,".[0-9]+",""))%>%
    dplyr::select(c(group,colNam, properties)) %>%
    tidyr::pivot_wider(names_from = colNam,values_from = properties) %>%
    dplyr::mutate(predictInt=as.integer(gsub("[^0-9.-]", "",stringr::str_sub(relativeHumidity.values.validTime, start= -4))),
                  dateTime = stringr::str_sub(relativeHumidity.values.validTime, end= -12),
                  dateTime = as.POSIXct(gsub("T", " ", dateTime),tz="UTC")) %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(dateTime= seq(dateTime,dateTime+lubridate::hours(predictInt-1),
                                   by = '1 hour'),
                     relativeHumidity_perc = as.numeric(relativeHumidity.values.value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(group)) %>%
    lubridate::with_tz(dateTime, tzone = "America/New_York")


  dewpointdf <- as.data.frame(do.call("cbind",gridjson_file[-c(1:4)])) %>%
    dplyr::mutate(colNam = rownames(.)) %>%
    magrittr::set_rownames(NULL) %>%
    dplyr::filter(grepl("dewpoint",colNam)  & !grepl("dewpoint.uom",colNam)) %>%
    dplyr::mutate(group = as.numeric(stringr::str_extract_all(colNam,"[0-9]+")),
                  group = ifelse(is.na(group),0,group),
                  colNam = stringr::str_replace_all(colNam,".[0-9]+",""))%>%
    dplyr::select(c(group,colNam, properties)) %>%
    tidyr::pivot_wider(names_from = colNam,values_from = properties) %>%
    dplyr::mutate(predictInt=as.integer(gsub("[^0-9.-]", "",stringr::str_sub(dewpoint.values.validTime, start= -4))),
                  dateTime = stringr::str_sub(dewpoint.values.validTime, end= -12),
                  dateTime = as.POSIXct(gsub("T", " ", dateTime),tz="UTC")) %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(dateTime= seq(dateTime,dateTime+lubridate::hours(predictInt-1),
                                   by = '1 hour'),
                     dewpoint_degC = round(as.numeric(dewpoint.values.value),2)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(group)) %>%
    lubridate::with_tz(dateTime, tzone = "America/New_York")


  forecast_hrly <- dplyr::left_join(raindf,dplyr::left_join(tempdf,dplyr::left_join(dewpointdf, relativeHumiditydf)))

  return(forecast_hrly)

}

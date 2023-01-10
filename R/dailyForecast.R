#' @title dailyForecast
#'
#' @description This function will download daily forecast data from weather.gov for a given lat long.
#'
#' @param lat Latitude of the site of interest
#' @param long Longitude of the site of interest
#'
#' @return A data frame object that contains a forecasted rainfall (mm),
#'  mean temperature (celsius), max temperature (celsius), min temperature (celsius),
#'  dewpoint (celsius), relative humidity (percent), and estimates of evapotranspiration using
#'  the McGuinness-Bordne Formulation.
#'
#' @examples
#' hourlyForecast(lat = 38.966754, long = -76.569252)
#'
#' @export
dailyForecast <- function(lat,long){
  #'Retrieve JSON file for specified lat and long from Weather.gov
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

  #'Retrieve JSON grid prediction file from Weather.gov
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


  mintempdf <- as.data.frame(do.call("cbind",gridjson_file[-c(1:4)])) %>%
    dplyr::mutate(colNam = rownames(.)) %>%
    magrittr::set_rownames(NULL) %>%
    dplyr::filter(grepl("minTemperature.values",colNam)) %>%
    dplyr::mutate(group = as.numeric(stringr::str_extract_all(colNam,"[0-9]+")),
                  group = ifelse(is.na(group),0,group),
                  colNam = stringr::str_replace_all(colNam,".[0-9]+",""))%>%
    dplyr::select(c(group,colNam, properties)) %>%
    tidyr::pivot_wider(names_from = colNam,values_from = properties) %>%
    dplyr::mutate(date =sub("\\T.*", "", minTemperature.values.validTime),
                  minTemperature_degC = round(as.numeric(minTemperature.values.value),2)) %>%
    dplyr::select(-c(group, minTemperature.values.validTime, minTemperature.values.value))


  maxtempdf <- as.data.frame(do.call("cbind",gridjson_file[-c(1:4)])) %>%
    dplyr::mutate(colNam = rownames(.)) %>%
    magrittr::set_rownames(NULL) %>%
    dplyr::filter(grepl("maxTemperature.values",colNam)) %>%
    dplyr::mutate(group = as.numeric(stringr::str_extract_all(colNam,"[0-9]+")),
                  group = ifelse(is.na(group),0,group),
                  colNam = stringr::str_replace_all(colNam,".[0-9]+",""))%>%
    dplyr::select(c(group,colNam, properties)) %>%
    tidyr::pivot_wider(names_from = colNam,values_from = properties) %>%
    dplyr::mutate(date =sub("\\T.*", "", maxTemperature.values.validTime),
                  maxTemperature_degC = round(as.numeric(maxTemperature.values.value),2)) %>%
    dplyr::select(-c(group,maxTemperature.values.validTime, maxTemperature.values.value))


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


  forecast_daily <- dplyr::left_join(maxtempdf %>%
                                       dplyr::mutate(date = date(date)),
                                     dplyr::left_join(mintempdf %>%
                                                        dplyr::mutate(date = date(date)),
                                                      dplyr::left_join(dewpointdf %>%
                                                                         dplyr::mutate(date = date(dateTime)) %>%
                                                                         dplyr::group_by(date) %>%
                                                                         dplyr::summarize(dewpoint_degC = mean(dewpoint_degC)),
                                                                       dplyr::left_join(relativeHumiditydf %>%
                                                                                          dplyr::mutate(date = date(dateTime)) %>%
                                                                                          dplyr::group_by(date) %>%
                                                                                          dplyr::summarize(relativeHumidity_perc = mean(relativeHumidity_perc)),
                                                                                        dplyr::left_join(tempdf %>%
                                                                                                           dplyr::mutate(date = date(dateTime)) %>%
                                                                                                           dplyr::group_by(date) %>%
                                                                                                           dplyr::summarize(temperature_degC = mean(temperature_degC)), raindf %>%
                                                                                                           dplyr::mutate(date = date(dateTime)) %>%
                                                                                                           dplyr::group_by(date) %>%
                                                                                                           dplyr::summarize(precip_mm = sum(precip_mm))))))) %>%
    dplyr::mutate(Year = lubridate::year(date),Month = lubridate::month(date), Day = lubridate::day(date))


  forecast_daily <- forecast_daily%>%dplyr::left_join(Evapotranspiration::ET.McGuinnessBordne(ReadInputs(c("Date.daily","Year", "Month", "Day", "Tmax","Tmin"),
                                                                                                         forecast_daily %>% dplyr::select(c(date, Year, Month, Day, maxTemperature_degC, minTemperature_degC)) %>%
                                                                                                           rename(Date.daily=date,
                                                                                                                  Tmax = maxTemperature_degC,
                                                                                                                  Tmin = minTemperature_degC),
                                                                                                         stopmissing = c(1,1,1), timestep = "daily",
                                                                                                         interp_missing_days = FALSE,
                                                                                                         interp_missing_entries = FALSE,
                                                                                                         interp_abnormal = FALSE,
                                                                                                         missing_method = NULL,
                                                                                                         abnormal_method = NULL,
                                                                                                         message = "yes"),
                                                                                              list(Elev = elevatr::get_elev_point(data.frame(x = long, y = lat), prj = "EPSG:4326", src = "epqs")$elevation,
                                                                                                   lambda = 2.45,
                                                                                                   lat_rad = -0.6094882,
                                                                                                   Gsc = 0.082),
                                                                                              ts="daily", message="yes", AdditionalStats="no", save.csv="no")$ET.Daily %>%
                                                        zoo::fortify.zoo() %>%
                                                        rename(date=1,evapotranspiration_mm=2) %>%
                                                        dplyr::mutate(evapotranspiration_mm = round(evapotranspiration_mm,2)))

  return(forecast_daily)

}

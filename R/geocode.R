
#' Using GEOCODE services
#'
#' Resolve the geographical coordination of places, using APIs
#'
#' You need to set your API KEY as follows:
#'
#' - open your `.renviron` file (use `usethis::edit_r_environ()` if possible);
#' - put api key into it
#'
#' ```
#' bing_geo_api_key=XXXXX
#' google_geo_api_key=xxxxx
#' ```
#' @name geocode
#' @param address an address
#' @param service now support Bing, Google
#' @param obj is a geocode object returned by `geocode()`
#'
#' @return an geocode object, which is structured data with class refer to different sources
#' @export
#'
#' @examples
#' res <- geocode(address = "Huazhong Agricultural University, Wuhan, China")
#' geocode_country(res); geocode_province(res)
geocode <- function(address, service = c("bing","google")){
  service <- match.arg(service)
  address <- trimws(address)
  address <- gsub(address,pattern=" +",replacement="+")
  switch(service,
         bing = {
           bing_geocode(address)
         },
         google = {
           google_geocode(address)
         }
  )
}

#' @rdname geocode
bing_geocode <- function(address){
  base_url <- "http://dev.virtualearth.net/REST/v1/Locations?"
  api_key <- Sys.getenv("bing_geo_api_key")
  url <- paste0(base_url,"q=",address,"&key=",api_key)
  url <- URLencode(enc2utf8(url))
  data <- rjson::fromJSON(file = url)
  structure(data, class = "bingGeocode")
}

#' @rdname geocode
google_geocode <- function(address){
  base_url <- "https://maps.googleapis.com/maps/api/geocode/json?"
  api_key <- Sys.getenv("google_geo_api_key")
  url <- paste0(base_url,"address=",address,"&key=",api_key)
  url <- URLencode(enc2utf8(url))
  data <- rjson::fromJSON(file = url)
  structure(data, class="googleGeocode")
}

#' @rdname geocode
geocode_country <- function(obj){
  # for Google geocode result
  if (inherits(obj, "googleGeocode")){
    components <- obj$results[[1]]$address_components
    found <- lapply(components, function(x){
      types <- x$types
      any(types %in% "country")
    })
    id <- which(unlist(found)==TRUE)
    if (length(id)<1) return(NA)
    return(components[[id]]$long_name)
  }

  # for Bing Geocode result
  if (inherits(obj, "bingGeocode")){
    return(obj$resourceSets[[1]]$resources[[1]]$address$countryRegion)
  }
}

#' @rdname geocode
geocode_province <- function(obj){
  # for Google geocode result
  if (inherits(obj, "googleGeocode")){
    components <- obj$results[[1]]$address_components
    found <- lapply(components, function(x){
      types <- x$types
      any(types %in% "administrative_area_level_1")
    })
    id <- which(unlist(found)==TRUE)
    if (length(id)<1) return(NA)
    return(components[[id]]$long_name)
  }

  # for Bing Geocode result
  if (inherits(obj, "bingGeocode")){
    return(obj$resourceSets[[1]]$resources[[1]]$address$adminDistrict)
  }
}


#' @rdname geocode
geocode_state <- geocode_province

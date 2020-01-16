#' Retrieve the complete feature list supported by marmot currently.
#' 
#' @param baseurl The base URL of a marmot service.
#' @return A list containing a name and a description property for each feature.  
#' @examples 
#' marmotR::features()
#' @export
features <- function(baseurl = httr::parse_url("http://marmot-proxy2-dev.us-east-2.elasticbeanstalk.com")) {
    url <- httr::modify_url(baseurl, path = paste(baseurl$path, "/features", sep = ""))
    resp <- httr::GET(url)
    if (httr::http_type(resp) != "application/json") {
        warning("API did not return json", call. = FALSE)
    }
    dplyr::select(jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8")), name, description)
}

#' Retrieve the version of a marmot service.
#' 
#' @param baseurl The base URL of a marmot service.
#' @return The version of the running service.
#' @examples 
#' marmotR::version()
#' @export
version <- function(baseurl = httr::parse_url("http://marmot-proxy2-dev.us-east-2.elasticbeanstalk.com")) {
    url <- httr::modify_url(baseurl, path = paste(baseurl$path, "/version", sep = ""))
    httr::content(httr::GET(url), "text", encoding = "UTF-8")
}

#' Retrieve data for exactly one package
#' 
#' @param id The maven identifier of the package requested
#' @param baseurl The base URL of a marmot service.
#' @return A list containing the request data.
#' @examples 
#' marmotR::retrieve("log4j:log4j:1.2.7")
#' @export
retrieve <- function(id = "", baseurl = httr::parse_url("http://marmot-proxy2-dev.us-east-2.elasticbeanstalk.com")) {
    url <- httr::modify_url(baseurl, path = paste(baseurl$path, "/retrieve/", id, sep = ""))
    resp <- httr::GET(url)
    if (httr::http_type(resp) != "application/json") {
        warning('API did not return json', call. = FALSE)
    } else {
        jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"))  
    }
}

#' Search for matching packages.
#' 
#' @param query The query in marmot query syntax.
#' @param limit The maximum number of results. 
#' @param baseurl The base URL of a marmot service.
#' @return A list containing the request data.
#' @examples 
#' marmotR::search("[metrics.bytecode.ret]>0")
#' @export
search <- function(query = "", limit = 50, baseurl = httr::parse_url("http://marmot-proxy2-dev.us-east-2.elasticbeanstalk.com")) {
    url <- httr::modify_url(baseurl, path = paste(baseurl$path, "/search", sep = ""))
    resp <- httr::POST(url, body = list(query = query, limit = limit), encode = "json")
    respText <- httr::content(resp, "text", encoding = "UTF-8")
    if (httr::http_type(resp) != "application/json") {
        warning(paste('API did not return json.', respText), call. = FALSE)
    } else {
        jsonlite::fromJSON(respText)
    }
}

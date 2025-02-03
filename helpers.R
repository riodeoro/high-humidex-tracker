get_available_years <- function() {
    files <- list.files(
        path = "RDATA",
        pattern = "^\\d{4}_BCWS_WX_OBS\\.RData$"
    )
    years <- sort(
        as.numeric(sub("_BCWS_WX_OBS\\.RData$", "", files)), 
        decreasing = TRUE
    )
    return(years)
}
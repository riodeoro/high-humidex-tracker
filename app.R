# Copyright 2025 Province of British Columbia
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at

# http://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#if (!require("pacman")) {
 #   install.packages("pacman")
#}
#library(pacman)
#pacman::p_load(data.table, bslib, lubridate, shiny, DT, leaflet, dplyr)

library(data.table)
library(bslib)
library(lubridate)
library(shiny)
library(DT)
library(leaflet)
library(dplyr)

addResourcePath("www", "www")

# Load lookup table from RDS
lookup_table <- readRDS("lookup_table.rds")
setDT(lookup_table)

source("helpers.R")
source("output.R")
source("ui_definition.R")



read_year_data <- function(year) {
    filename <- file.path("RDATA", paste0(year, "_BCWS_WX_OBS.RData"))
    if (!file.exists(filename)) {
        return(NULL)
    }
    
    e <- new.env()
    load(filename, envir = e)
    
    data_name <- ls(envir = e)[1]
    data <- get(data_name, envir = e)
    
    setDT(data)
    
    required_cols <- c(
        "DATE_TIME", "STATION_CODE",
        "HOURLY_TEMPERATURE",
        "HOURLY_RELATIVE_HUMIDITY",
        "HOURLY_WIND_SPEED",
        "HOURLY_WIND_DIRECTION",
        "HOURLY_PRECIPITATION"
    )
    
    if (!all(required_cols %in% names(data))) {
        missing_cols <- setdiff(required_cols, names(data))
        warning("Missing columns in data: ", paste(missing_cols, collapse = ", "))
        return(NULL)
    }
    
    data[, DateTime := as.POSIXct(DATE_TIME)]
    data[, Temperature := HOURLY_TEMPERATURE]
    data[, RH := HOURLY_RELATIVE_HUMIDITY]
    data[, WindSpeed := HOURLY_WIND_SPEED]
    data[, WindDir := HOURLY_WIND_DIRECTION]
    data[, Rainfall := HOURLY_PRECIPITATION]
    
    data <- data[hour(DateTime) == 12]
    
    return(data)
}

process_weather_data <- function(data, lookup_table) {
    if (is.null(data) || nrow(data) == 0) return(NULL)
    
    # Process daily summaries
    processed <- data[, .(
        Temperature = mean(Temperature, na.rm = TRUE),
        RH = mean(RH, na.rm = TRUE),
        WindSpeed = mean(WindSpeed, na.rm = TRUE),
        WindDir = mean(WindDir, na.rm = TRUE),
        Rainfall = sum(Rainfall, na.rm = TRUE),
        DateTime = first(DateTime)
    ), by = .(STATION_CODE, date = as.Date(DateTime))]
    
    processed[, Dewpoint := {
        a <- 17.27
        b <- 237.7
        alpha <- ((a * Temperature) / (b + Temperature)) + log(RH/100)
        (b * alpha) / (a - alpha)
    }]
    
    processed[, Humidex := {
        dewpoint_k <- Dewpoint + 273.16
        e <- 6.11 * exp(5417.7530 * ((1/273.16) - (1/dewpoint_k)))
        Temperature + 0.5555 * (e - 10)
    }]
    
    lookup_cols <- lookup_table[, .(
        STATION_CODE,
        Station = STATION,
        FIRE_ZONE,
        FIRE_CENTRE,
        LATITUDE,
        LONGITUDE,
        ELEVATION
    )]
    
    processed <- merge(processed, lookup_cols, 
                      by = "STATION_CODE",
                      all.x = TRUE)
    
    setnames(processed, c("FIRE_ZONE", "FIRE_CENTRE"), 
             c("Fire Zone", "Fire Centre"))
    
    return(processed)
}

server <- function(input, output, session) {
    results_data <- reactiveVal(NULL)
    error_message <- reactiveVal("")
    
    observeEvent(input$analyze, {
    error_message("")
    
    withProgress(message = 'Analyzing data...', {
        tryCatch({
            incProgress(0.3, detail = "Reading weather data")
            raw_data <- read_year_data(input$year)
            
            if (is.null(raw_data)) {
                error_message(paste("No data file found for", input$year))
                return()
            }
            
            incProgress(0.3, detail = "Processing data")
            processed_data <- process_weather_data(raw_data, lookup_table)
            
            if (is.null(processed_data)) {
                error_message(paste("Error processing data for", input$year))
                return()
            }
            
            if (input$fire_centre != "all") {
                processed_data <- processed_data[`Fire Centre` == input$fire_centre]
            }
            
            incProgress(0.4, detail = "Analyzing results")
            warning_results <- processed_data[Humidex >= 35]
            setorder(warning_results, -Humidex)
            
            if (nrow(warning_results) > 0) {
                results_data(warning_results)
                error_message("")
            } else {
                results_data(NULL)
                error_message(paste("No high Humidex warnings found in", input$year))
            }
            
        }, error = function(e) {
            error_message(paste("Error:", e$message))
        })
    })
})
    
    output$error_output <- renderText({
        error_message()
    })
    
    observe({
        req(results_data())
        
        output$fire_centre_summary <- renderText({
            render_fire_centre_summary(results_data())
        })
        
        output$fire_zone_summary <- renderText({
            render_fire_zone_summary(results_data())
        })
        
        output$station_summary <- renderText({
            render_station_summary(results_data())
        })
        
        output$warning_table <- renderDT({
            render_warning_table(results_data())
        })
        
        output$station_map <- renderLeaflet({
            station_coords <- unique(lookup_table[, .(
                STATION_CODE,
                STATION,
                LATITUDE,
                LONGITUDE,
                ELEVATION
            )])
            render_station_map(results_data(), station_coords)
        })
    })
}

shinyApp(ui = ui(lookup_table), server = server)
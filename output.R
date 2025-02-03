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

render_fire_centre_summary <- function(results_data) {
    req(results_data)
    
    centre_counts <- results_data %>%
        group_by(`Fire Centre`) %>%
        summarise(
            Events = n(),
            Stations = n_distinct(STATION_CODE),
            MaxHumidex = max(Humidex)
        ) %>%
        arrange(desc(Events))
    
    header <- "High Humidex Events by Fire Centre\n"
    separator <- paste(rep("=", 65), collapse = "") 
    col_headers <- sprintf("%-22s %16s %16s %16s\n",
                        "Fire Centre",
                        "Total Events",
                        "Unique Stations",
                        "Maximum Humidex")
    
    rows <- sapply(1:nrow(centre_counts), function(i) {
        sprintf("%-22s %16d %16d %16.1f\n",
               substr(centre_counts$`Fire Centre`[i], 1, 22),
               centre_counts$Events[i],
               centre_counts$Stations[i],
               centre_counts$MaxHumidex[i])
    })
    
    paste0(header, separator, "\n", col_headers, paste(rows, collapse = ""))
}

render_fire_zone_summary <- function(results_data) {
    req(results_data)
    
    zone_counts <- results_data %>%
        group_by(`Fire Zone`) %>%
        summarise(
            Events = n(),
            Stations = n_distinct(STATION_CODE),
            MaxHumidex = max(Humidex)
        ) %>%
        arrange(desc(Events)) %>%
        head(10)
    
    header <- "Top 10 Fire Zones by Event Count\n"
    separator <- paste(rep("=", 65), collapse = "")
    col_headers <- sprintf("%-22s %16s %16s %16s\n",
                        "Fire Zone",
                        "Total Events",
                        "Unique Stations",
                        "Maximum Humidex")
    
    rows <- sapply(1:nrow(zone_counts), function(i) {
        sprintf("%-22s %16d %16d %16.1f\n",
               substr(zone_counts$`Fire Zone`[i], 1, 22),
               zone_counts$Events[i],
               zone_counts$Stations[i],
               zone_counts$MaxHumidex[i])
    })
    
    paste0(header, separator, "\n", col_headers, paste(rows, collapse = ""))
}

render_station_summary <- function(results_data) {
    req(results_data)
    
    station_counts <- results_data %>%
        group_by(STATION_CODE, Station, `Fire Centre`, `Fire Zone`) %>%
        summarise(
            Events = n(),
            MaxHumidex = max(Humidex),
            .groups = 'drop'
        ) %>%
        arrange(desc(Events)) %>%
        head(10)
    
    header <- "Top 10 Stations by Event Count\n"
    separator <- paste(rep("=", 90), collapse = "")
    col_headers <- sprintf("%-14s %-20s %-19s %16s %16s\n",
                        "Station",
                        "Fire Centre",
                        "Fire Zone",
                        "Total Events",
                        "Maximum Humidex")
    
    rows <- sapply(1:nrow(station_counts), function(i) {
        sprintf("%-14s %-20s %-19s %16d %16.1f\n",
               substr(station_counts$Station[i], 1, 14),
               substr(station_counts$`Fire Centre`[i], 1, 20),
               substr(station_counts$`Fire Zone`[i], 1, 19),
               station_counts$Events[i],
               station_counts$MaxHumidex[i])
    })
    
    paste0(header, separator, "\n", col_headers, paste(rows, collapse = ""))
}

render_warning_table <- function(results_data) {
    req(results_data)
    datatable(
        results_data[, .(
            Date = date,
            `Station Code` = STATION_CODE,
            Station,
            "Fire Centre" = `Fire Centre`,
            "Fire Zone" = `Fire Zone`,
            Temperature,
            RH,
            WindSpeed,
            WindDir,
            Rainfall,
            Humidex,
            Latitude = LATITUDE,
            Longitude = LONGITUDE,
            Elevation = ELEVATION
        )],
        rownames = FALSE,
        options = list(
            pageLength = 25,
            scrollX = TRUE,
            order = list(list(9, 'desc'))  # Sort by Humidex column by default
        )
    )
}


render_station_map <- function(results_data, station_coords) {
    req(results_data)
    
    # Get unique stations with their maximum Humidex values
    station_summary <- results_data %>%
        group_by(STATION_CODE, Station) %>%
        summarise(
            MaxHumidex = max(Humidex),
            FireCentre = first(`Fire Centre`),
            FireZone = first(`Fire Zone`),
            Events = n(),
            .groups = 'drop'
        )
    
    # Create color palette based on number of events
    pal <- colorNumeric(
        palette = "YlOrRd",
        domain = station_summary$Events
    )
    
    # Initialize the map
    map <- leaflet() %>%
        addProviderTiles(providers$Esri.WorldTopo) %>%
        setView(lng = -123.3656, lat = 54.5, zoom = 5)  # Centered on BC
    
    # Add markers for each station
    for(i in 1:nrow(station_summary)) {
        station_code <- station_summary$STATION_CODE[i]
        
        # Get station coordinates
        station_info <- station_coords[STATION_CODE == station_code]
        
        if(nrow(station_info) > 0) {
            popup_content <- sprintf(
                "<strong>%s</strong><br>
                Station Code: %s<br>
                Fire Centre: %s<br>
                Fire Zone: %s<br>
                Number of Events: %d<br>
                Max Humidex: %.1f<br>
                Elevation: %.1f m",
                station_summary$Station[i],
                station_code,
                station_summary$FireCentre[i],
                station_summary$FireZone[i],
                station_summary$Events[i],
                station_summary$MaxHumidex[i],
                station_info$ELEVATION
            )
            
            map <- map %>%
                addCircleMarkers(
                    lng = station_info$LONGITUDE,
                    lat = station_info$LATITUDE,
                    radius = 8,
                    color = "black",
                    weight = 1,
                    fillColor = pal(station_summary$Events[i]),
                    fillOpacity = 0.7,
                    popup = popup_content,
                    label = station_summary$Station[i]
                )
        }
    }
    
    map %>%
        addLegend(
            position = "bottomright",
            pal = pal,
            values = station_summary$Events,
            title = "Number of Events",
            opacity = 0.7
        )
}
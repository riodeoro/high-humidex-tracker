ui <- function(lookup_table) {
    fire_centres <- c("All Fire Centres" = "all", 
                     sort(unique(lookup_table$`FIRE_CENTRE`)))
    
    available_years <- get_available_years()
    
    fluidPage(
        theme = bs_theme(version = 5),
        
        tags$head(
            tags$meta(name = "viewport", 
                     content = "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"),
            includeCSS("www/styles.css"),
            tags$style(HTML("
                .card {
                    height: 100%;
                    margin-bottom: 20px;
                }
                .card-body {
                    font-family: monospace;
                    white-space: pre;
                }
                            ")),
            tags$script(HTML("
                $(document).ready(function() {
                    $('#fetch_controls_btn').click(function(e) {
                        e.stopPropagation();
                        $('.fetch-controls-dropdown').toggleClass('active');
                    });
                    $(document).click(function(e) {
                        if (!$(e.target).closest('.fetch-controls-dropdown').length) {
                            $('.fetch-controls-dropdown').removeClass('active');
                        }
                    });
                });
            "))
        ),
        
        tags$nav(class = "navbar navbar-default navbar-fixed-top",
            div(class = "container-fluid",
                div(class = "navbar-header",
                    img(src = "www/gov3_bc_logo.png", class = "nav-logo"),
                    span(class = "navbar-title", "High Humidex Tracker")
                )
            )
        ),
        
        div(class = "container-fluid",
            div(class = "header-container",
                div(class = "header-left"),
                div(class = "alert alert-danger",
                    style = "display: none;",
                    textOutput("error_output")
                ),
                
                div(class = "fetch-controls-dropdown",
                    actionButton("fetch_controls_btn", "Data Controls", 
                               class = "btn-primary"),
                    div(class = "fetch-controls-content",
                        selectInput("fire_centre", "Select Fire Centre:",
                                  choices = fire_centres
                        ),
                        selectInput("year",
                                  "Select Year:",
                                  choices = available_years,
                                  selected = if(length(available_years) > 0) available_years[1] else NULL
                        ),
                        actionButton("analyze", "Analyze Conditions", 
                                   class = "btn-primary")
                    )
                )
            ),
            
            div(class = "output-card",
                h4("High Humidex Summary (≥35)"),
                div(
                    class = "alert alert-warning",
                    style = paste0("color: #721c24; background-color: #f8d7da;",
                                 "padding: 15px; border-radius: 4px;",
                                 "margin-bottom: 20px;"),
                    "Analysis of dailies where Humidex reached or exceeded 35"
                ),
                
                layout_columns(
                    col_widths = c(12),
                    card(
                        card_header("Fire Centre Analysis"),
                        verbatimTextOutput("fire_centre_summary")
                    )
                ),
                
                layout_columns(
                    col_widths = c(6, 6),
                    card(
                        card_header("Top Fire Zones"),
                        verbatimTextOutput("fire_zone_summary")
                    ),
                    card(
                        card_header("Station Performance"),
                        verbatimTextOutput("station_summary")
                    )
                ),
                
                h4("Station Locations"),
                div(class = "map-container",
                    leafletOutput("station_map", height = "600px")
                ),
                
                h4("Detailed Entries"),
                div(class = "scrollable-output",
                    DTOutput("warning_table")
                ),
                verbatimTextOutput("error_output")
            )
        ),
        
        tags$footer(id = "footer",
            div(id = "footerWrapper",
                div(id = "footerAdminSection",
                    div(class = "container",
                        div(id = "footerAdminLinks",
                            tags$ul(class = "inline",
                                tags$li(tags$a(href = "https://www2.gov.bc.ca/gov/content/home", 
                                             "Home", target = "_blank")),
                                tags$li(tags$a(href = "https://www2.gov.bc.ca/gov/content/home/disclaimer", 
                                             "Disclaimer", target = "_blank")),
                                tags$li(tags$a(href = "https://www2.gov.bc.ca/gov/content/home/privacy", 
                                             "Privacy", target = "_blank")),
                                tags$li(tags$a(href = "https://www2.gov.bc.ca/gov/content/home/accessibility", 
                                             "Accessibility", target = "_blank")),
                                tags$li(tags$a(href = "https://www2.gov.bc.ca/gov/content/home/copyright", 
                                             "Copyright", target = "_blank")),
                                tags$li(tags$a(href = "https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", 
                                             "Contact", target = "_blank"))
                            )
                        )
                    )
                )
            ),
            tags$img(src = "www/back-to-top.png", 
                    class = "back-to-top", 
                    onclick = "window.scrollTo({top: 0, behavior: 'smooth'});")
        ),

        tags$script(HTML("
            $(document).ready(function() {
                $(window).scroll(function() {
                    if ($(this).scrollTop() > 100) {
                        $('.back-to-top').fadeIn();
                    } else {
                        $('.back-to-top').fadeOut();
                    }
                });

                if ($(window).scrollTop() > 100) {
                    $('.back-to-top').show();
                }
            });
        "))
    )
}
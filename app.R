library(tidyverse)
ori <- "horizontal"
if (ori == "horizontal") {
  hei <- "30px"
  wid <- "200px"
} else if (ori == "vertical") {
  hei <- "200px"
  wid <- "30px"
} else {
  stop("Invalid ori.")
}
deaths <- read.csv("time_series_covid19_deaths_global.csv")
deaths_by_country <- deaths %>% 
  dplyr::group_by(Country.Region) %>% 
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with("X")), sum) %>% 
  dplyr::select(-`Province.State`, -Lat, -Long) %>% 
  dplyr::distinct() %>% 
  tibble::column_to_rownames("Country.Region") %>% 
  t() %>% 
  tibble::as_tibble(rownames = NA) %>% 
  tibble::rownames_to_column("Date") %>% 
  dplyr::mutate(`Date` = gsub("X", "", `Date`)) %>%
  dplyr::mutate(`Date` = factor(`Date`, levels = `Date`))

tack_days_since <- function(x, lst) {
  nm <- names(lst)[x]
  df <- data.frame(days = paste0("d+", 0:(length(lst[[x]])-1)), nm = lst[[x]])
  df$days <- factor(df$days, levels = df[["days"]])
  names(df) <- c("days", nm)
  return(df)
}
dropper <- function(x) {x[x > 0]}

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Days Since X Deaths"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        shinyWidgets::noUiSliderInput(
          inputId = "r", label = "R:", min = 0.01, max = 6, 
          value = 2.2, step = 0.01, orientation = ori, 
          format = shinyWidgets::wNumbFormat(decimals = 2), 
          color = "#2980b9", inline = TRUE,
          height = hei, width = wid), shiny::br(), 
        shinyWidgets::noUiSliderInput(
          inputId = "mind", label = "Minimum deaths:", min = 1, max = 25, 
          value = 10, step = 1, orientation = ori, 
          format = shinyWidgets::wNumbFormat(decimals = 0), 
          color = "#c0392b", inline = TRUE,
          height = hei, width = wid), shiny::br(), 
        shinyWidgets::noUiSliderInput(
          inputId = "days", label = "Days shown:", min = 1, max = 60, 
          value = 10, step = 1, orientation = ori, 
          format = shinyWidgets::wNumbFormat(decimals = 0), 
          color = "#27ae60", inline = TRUE,
          height = hei, width = wid)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotly::plotlyOutput("days_since_min")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   plt <- shiny::reactive({
     countries_with_deaths <- deaths_by_country[c(FALSE, sapply(deaths_by_country[2:length(deaths_by_country)], 
                                                                function(x) max(x, na.rm = T) > input$mind))]
     countries_with_deaths[countries_with_deaths < input$mind] <- 0
     dropped <- apply(countries_with_deaths, 2, dropper)
     tacked <- lapply(1:length(dropped), tack_days_since, dropped)
     merged <- merge(tacked[[1]], tacked[[2]], by = "days", all = TRUE)
     for (i in 3:length(tacked)) {
       merged <- merge(merged, tacked[[i]], by = "days", all = TRUE)
     }
     merged <- merged[1:input$days, ]
     plt <- plotly::plot_ly(merged, 
                            x = ~days, 
                            y = ~Canada, 
                            name = 'Canadian Deaths', 
                            type = 'scatter', 
                            mode = 'lines+markers') %>%
       plotly::add_trace(y = ~China, name = 'Chinese Deaths', mode = 'lines+markers') %>%
       plotly::add_trace(y = ~Italy, name = 'Italian Deaths', mode = 'lines+markers') %>%
       plotly::add_trace(y = ~US, name = 'American Deaths', mode = 'lines+markers') %>% 
       plotly::add_trace(y = ~`Korea, South`, name = 'Korean Deaths', mode = 'lines+markers') %>% 
       plotly::layout(yaxis = list(title = "Deaths"))
     return(plt)
   })
   output$days_since_min <- plotly::renderPlotly({plt()})
}

# Run the application 
shinyApp(ui = ui, server = server)


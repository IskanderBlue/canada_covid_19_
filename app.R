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
  dplyr::select(-China) %>% 
  dplyr::mutate(`Date` = gsub("X", "", `Date`)) %>%
  dplyr::mutate(`Date` = factor(`Date`, levels = `Date`))

tack_days_since <- function(x, lst, dfm, back) {
  nm <- names(lst)[x]
  first <- match(lst[[x]], t(dfm[, x]))[1] - back
  days_strings <- c(paste0("d-", back:1), paste0("d+", 0:(length(lst[[x]])-1)))
  tacked <- data.frame(days = days_strings, nm = dfm[first:nrow(dfm), x])
  tacked$days <- factor(tacked$days, levels = tacked[["days"]])
  names(tacked) <- c("days", nm)
  return(tacked)
}
dropper <- function(x, geq) {x[x >= geq]}

library(shiny)

# Define UI for application that draws a histogram
ui <- shinydashboard::dashboardPage(
  # Application title
  title = "Covid-19 Projection",
  shinydashboard::dashboardHeader(
    title = shiny::textOutput("days_since")),
  shinydashboard::dashboardSidebar(
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
    shiny::uiOutput("country_selector"), shiny::br(),
    shinyWidgets::noUiSliderInput(
      inputId = "days", label = "Days shown:", min = 1, max = 60, 
      value = 10, step = 1, orientation = ori, 
      format = shinyWidgets::wNumbFormat(decimals = 0), 
      color = "#27ae60", inline = TRUE,
      height = hei, width = wid), 
    shinyWidgets::noUiSliderInput(
      inputId = "back", label = "d-x shown:", min = 1, max = 10, 
      value = 3, step = 1, orientation = ori, 
      format = shinyWidgets::wNumbFormat(decimals = 0), 
      color = "#27ae60", inline = TRUE,
      height = hei, width = wid), 
    shiny::numericInput("plt_hei", "Plot Height:", value = 650)
  ),
  shinydashboard::dashboardBody(
    plotly::plotlyOutput("days_since_min")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$days_since <- shiny::renderText({paste0("Days since ", input$mind, " Deaths")})
  
  valid_countries <- shiny::reactive({
    countries_with_deaths <- deaths_by_country[
      c(FALSE, sapply(deaths_by_country[2:length(deaths_by_country)], 
                      function(x) max(x, na.rm = T) > input$mind))]
    # countries_with_deaths[countries_with_deaths < input$mind] <- 0
    dropped <- apply(countries_with_deaths, 2, dropper, input$mind)
    tacked <- lapply(1:length(dropped), tack_days_since, dropped, countries_with_deaths, input$back)
    merged <- merge(tacked[[1]], tacked[[2]], by = "days", all = TRUE)
    for (i in 3:length(tacked)) {
      merged <- merge(merged, tacked[[i]], by = "days", all = TRUE)
    }
    merged <- merged[1:input$days, ]
    return(merged)
  })
  
  plt <- shiny::reactive({
    countries <- input[["countries_selected"]]
    plt <- plotly::plot_ly(valid_countries(), type = 'scatter', mode = 'lines+markers',
                           height = input$plt_hei) 
    for (i in countries) {
      plt_df <- valid_countries() %>% 
        dplyr::rename(y=i) %>% 
        dplyr::mutate(periods = (input$back * -1):(nrow(.)-(input$back + 1)) )
      mdl <- lm(log(plt_df$y) ~ plt_df$periods)
      print(plt_df$periods)
      print(summary(mdl))
      plt_df$preds <- exp(predict(mdl, list(Time=plt_df$periods)))
      print(plt_df$preds)
      trace_col <- paste0("rgb(", paste0(sample(255, 3), collapse=", "), ")")
      
      plt <- plt %>%
        plotly::add_trace(data = plt_df, x = ~days, y = ~y, 
                          name = i, # paste0("Deaths in ", i), 
                          mode = 'markers', 
                          marker = list(color = trace_col)) %>% 
        plotly::add_trace(data = plt_df, x = ~days, y = ~preds, 
                          name = NULL, #paste0("Exponential trend for ", i),
                          line = list(color = trace_col), showlegend = FALSE,
                          mode = 'lines')
        
    }
    plt <- plt %>% plotly::layout(yaxis = list(title = "Deaths")
                                  # , 
                                  # paper_bgcolor = "rgb(0, 0, 0)",
                                  # plot_bgcolor = "rgb(34, 45, 50)"
                                  )
    return(plt)
  })
  output$days_since_min <- plotly::renderPlotly({plt()})
  
  output$country_selector <- shiny::renderUI({
    shiny::selectInput(
      "countries_selected", label = "Select countries:", 
      choices = shiny::isolate(names(valid_countries())[-1]), 
      selected = c("Canada", "US", "Italy", "Korea, South"), 
      multiple = TRUE)
  })
  
  shiny::observeEvent(valid_countries(), {
    selected_countries <- input$countries_selected
    shiny::updateSelectInput(session, "countries_selected", 
                            choices = names(valid_countries())[-1], 
                            selected = selected_countries)
  }, ignoreInit = TRUE)
  
  shiny::observeEvent(input$days, {
    backs <- input$back
    if (input$days <= 10) {
      shinyWidgets::updateNoUiSliderInput(session, "back", value = backs, 
                                          range = c(0, input$days))
    }
  }, ignoreInit = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)
# rsconnect::deployApp()

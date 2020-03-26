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

#' @param x integer, which element of lst, which column of dfm to tack
#' @param lst list, each element contains list of 2 elements: 1) data for a country 2) cutoff position for that country
#' @param dfm dataframe with structure to tack lst data together by
#' @param back integer, how many days back to go
#' @param offset integer, how much to offset the dates by
tack_days_since <- function(x, lst, dfm, back, offset = 0) {
  nm <- names(lst)[x]
  first <- lst[[x]][[2]] - back
  days_strings <- c(paste0("d-", back:1), paste0("d+", 0:(length(lst[[x]][[1]])-1)))
  tacked <- data.frame(days = days_strings, nm = dfm[(first+offset):nrow(dfm), x])
  tacked$days <- factor(tacked$days, levels = tacked[["days"]])
  names(tacked) <- c("days", nm)
  return(tacked)
}
dropper <- function(x, geq) {
  vec <- x[cumsum(x) >= geq]
  frst <- min(which(cumsum(x) >= geq))
  return(list(vec, frst))
}
find_position <- function(vec, pred_per) {
  last_non_na <- max(which(!is.na(vec)))
  max(last_non_na - pred_per, 1):last_non_na
}

library(shiny)

# Define UI for application that draws a histogram
ui <- shinydashboard::dashboardPage(
  # Application title
  title = "Covid-19 Projection",
  shinydashboard::dashboardHeader(
    title = shiny::textOutput("days_since")),
  shinydashboard::dashboardSidebar(
    shiny::uiOutput("country_selector"), shiny::br(),
    shinyWidgets::noUiSliderInput(
      inputId = "r", label = "R:", min = 0.01, max = 6, 
      value = 2.2, step = 0.01, orientation = ori, 
      format = shinyWidgets::wNumbFormat(decimals = 2), 
      color = "#2980b9", inline = TRUE,
      height = hei, width = wid), shiny::br(), 
    shinyWidgets::noUiSliderInput(
      inputId = "pred_per", label = "Max. prediction period:", min = 3, max = 20, 
      value = 10, step = 1, orientation = ori, 
      format = shinyWidgets::wNumbFormat(decimals = 0), 
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
    deaths_by_day <- as.data.frame(diff(as.matrix(countries_with_deaths))) 
    death_growth <- as.data.frame(diff(as.matrix(deaths_by_day)))
    death_dates <- apply(deaths_by_day, 2, function(x) {as.character(deaths_by_country$Date[-1])})
    dropped <- apply(deaths_by_day, 2, dropper, input$mind)
    tacked <- lapply(1:length(dropped), tack_days_since, dropped, deaths_by_day, input$back)
    tacked_dates <- lapply(1:length(dropped), tack_days_since, dropped, death_dates, input$back)
    merged <-       merge(tacked[[1]], tacked[[2]], by = "days", all = TRUE)
    merged_dates <- merge(tacked_dates[[1]], tacked_dates[[2]], by = "days", all = TRUE)
    for (i in 3:length(tacked)) {
      merged <- merge(merged, tacked[[i]], by = "days", all = TRUE)
      merged_dates <- merge(merged_dates, tacked_dates[[i]], by = "days", all = TRUE)
    }
    merged <- merged[1:input$days, ]
    merged_dates <- merged_dates[1:input$days, ]
    return(list(merged, merged_dates))
  })
  # valid_countries <- shiny::reactive({
  #   countries_with_deaths <- deaths_by_country[
  #     c(FALSE, sapply(deaths_by_country[2:length(deaths_by_country)], 
  #                     function(x) max(x, na.rm = T) > input$mind))]
  #   dropped <- apply(countries_with_deaths, 2, dropper, input$mind)
  #   tacked <- lapply(1:length(dropped), tack_days_since, dropped, countries_with_deaths, input$back)
  #   merged <- merge(tacked[[1]], tacked[[2]], by = "days", all = TRUE)
  #   for (i in 3:length(tacked)) {
  #     merged <- merge(merged, tacked[[i]], by = "days", all = TRUE)
  #   }
  #   merged <- merged[1:input$days, ]
  #   return(merged)
  # })
  
  plt <- shiny::reactive({
    countries <- input[["countries_selected"]]
    plt <- plotly::plot_ly(type = 'scatter', mode = 'lines+markers',
                           height = input$plt_hei) 

    for (i in countries) {
      plt_df <- valid_countries()[[1]] %>% 
        dplyr::rename(y=i) %>% 
        dplyr::mutate(periods = (input$back * -1):(nrow(.)-(input$back + 1)) ) %>%
        dplyr::mutate(z = valid_countries()[[2]][, i]) %>% 
        dplyr::mutate(days = c(paste0("d", (input$back * -1):(-1)), 
                               paste0("d+", 0:(nrow(.)-(input$back + 1)))))
      
      fit_pos <- find_position(plt_df$y, input$pred_per)
      periods <- plt_df$periods[fit_pos]
      mdl <- lm(log(plt_df$y[fit_pos]+0.01) ~ periods)
      plt_df$preds <- exp(predict(mdl, list(periods=plt_df$periods)))
      trace_col <- paste0("rgb(", paste0(sample(255, 3), collapse=", "), ")")

      plt <- plt %>%
        plotly::add_trace(data = plt_df, x = ~days, y = ~preds, 
                          name = i, 
                          line = list(color = trace_col), showlegend = FALSE,
                          mode = 'lines', 
                          hoverinfo = "text+name", 
                          hovertemplate = "%{y:.2f} predicted for") %>%
        plotly::add_trace(data = plt_df, x = ~days, y = ~y, 
                          name = i, 
                          mode = 'markers', 
                          marker = list(color = trace_col), 
                          hoverinfo = "text+name",
                          text = ~paste0(z, ", ", y, " deaths in"))
    }
    
    plt <- plt %>% 
      plotly::layout(yaxis = list(title = "Deaths")
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
      choices = shiny::isolate(names(valid_countries()[[1]])[-1]), 
      selected = c("Canada", "US", "Italy", "Korea, South"), 
      multiple = TRUE)
  })
  
  shiny::observeEvent(valid_countries()[[1]], {
    selected_countries <- input$countries_selected
    shiny::updateSelectInput(session, "countries_selected", 
                            choices = names(valid_countries()[[1]])[-1], 
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

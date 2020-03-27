library(tidyverse)

# Loading data
deaths <- read.csv("time_series_covid19_deaths_global.csv")

# Setting slider orientation
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

# Group by country, sum deaths, drop duplicates, transpose, drop China, factorize dates.
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

#' Takes list of countries' death data, joins them together by days since X deaths.  
#' 
#' Takes list of countries' death data, joins them together by days column.  
#' If you run into errors here, there may be bad data; had an error when 
#' Philippines' 3.18.20 deaths were weirdly high and then dropped back down on the 19th.
#' 
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

#' Drops all elements from a vector that are cumulatively smaller than geq.
#' 
#' Drops all elements from a vector that are cumulatively smaller than geq.
#' Returns a list with 
#' 1) the presumably shortened vector and 
#' 2) the position (in the input vector) of the first element in the returned vector.  
#' @param x vector of numerics
#' @param geq numeric value, cutoff
dropper <- function(x, geq) {
  vec <- x[cumsum(x) >= geq]
  frst <- min(which(cumsum(x) >= geq))
  return(list(vec, frst))
}

#' Finds the position of the last non-NA element of a vector.
#' 
#' Finds the position of the last non-NA element of a vector.
#' If the desired maximum prediction period is longer than the vector, returns entire vector.  
#' Otherwise, returns the positions of the last `prediction_period` elements from the vector.
#' @param vec vector, presumably with NA elements at the end.
#' @param pred_per integer, maximium prediction period.
find_position <- function(vec, pred_per) {
  last_non_na <- max(which(!is.na(vec)))
  max(last_non_na - pred_per, 1):last_non_na
}

#' Predicts subsequent values of an exponentially changing vector.  
#' 
#' For a given `coef` and `reduction` of that coefficient, returns `vec` with 
#' an additional `iterations` elements that have increased exponentially by 
#' `coef` with `reduction` added slowly as a lognormal distribution.  
#' See onset to death, https://www.mdpi.com/2077-0383/9/2/538/htm
#' @param vec vector of integers, predicted values
#' @param iterations, integer, number of iterations to extend vec by
predict_for_na <- function(vec, iterations, coef, reduction) {
  predder <- function(x, chng, cf = coef) {
    x * exp(cf - chng)
  }
  reduction <- log(1/(1-reduction)) # Convert countermeasure effectiveness to reduction in growth rate
  mn <- 20.2 # Onset to death, https://www.mdpi.com/2077-0383/9/2/538/htm
  sd <- 11.6 # sd of onset to death
  mu <- log(mn) - 0.5* log((sd/mn)^2 + 1) # lognormal param
  sigma <- sqrt(log((sd/mn)^2+1)) # lognormal param
  prop_reduction <- plnorm(1:iterations,mu,sigma) # proportion of effect up to x days from change
  
  if (iterations > 0) {
    for (i in 1:iterations) {
      vec <- c(vec, predder(vec[length(vec)], chng = prop_reduction[i] * reduction))
      # Even implementation over 10 days
      # if (i < 18) {
      #   vec <- c(vec, predder(vec[length(vec)], chng = 0))
      # } else if (i < 27) {
      #   vec <- c(vec, predder(vec[length(vec)], chng = ((i-17)/10) * reduction))
      # } else {
      #   vec <- c(vec, predder(vec[length(vec)], chng = reduction))
      # }
    }
  }
  return(vec)
}

library(shiny)

# Define UI for application that shows how slowly changes in 
# Covid-19's growth rate affect deaths. 
ui <- shinydashboard::dashboardPage(
  # Application title
  title = "Covid-19 Projection",
  shinydashboard::dashboardHeader(
    title = shiny::textOutput("days_since")), # Adjusts 'days since' according to min deaths inputted.
  
  shinydashboard::dashboardSidebar(
    
    shiny::uiOutput("country_selector"), shiny::br(), # Selects countries to include
    
    shinyWidgets::noUiSliderInput( # Slider to set countermeasure effectiveness
      inputId = "r", label = "Countermeasure effectiveness:", min = 0, max = 0.99, 
      value = 0.5, step = 0.01, orientation = ori, 
      format = shinyWidgets::wNumbFormat(decimals = 2), 
      color = "#2980b9", inline = TRUE,
      height = hei, width = wid), shiny::br(), 
    
    shinyWidgets::awesomeCheckbox("show_options", "Show extra options."), shiny::br(),
    
    shiny::conditionalPanel(
      condition = "input.show_options == true", 
      shinyWidgets::noUiSliderInput( # Slider to set prediction period
        inputId = "pred_per", label = "Prediction period:", min = 3, max = 20, 
        value = 10, step = 1, orientation = ori, 
        format = shinyWidgets::wNumbFormat(decimals = 0), 
        color = "#2980b9", inline = TRUE,
        height = hei, width = wid), shiny::br(), 
      
      shinyWidgets::noUiSliderInput( # Slider to set minimum cumulative deaths at d+0.
        inputId = "mind", label = "Minimum deaths:", min = 10, max = 25, 
        value = 10, step = 1, orientation = ori, 
        format = shinyWidgets::wNumbFormat(decimals = 0), 
        color = "#c0392b", inline = TRUE,
        height = hei, width = wid), shiny::br(), 
      
      shinyWidgets::noUiSliderInput( # Slider to adjust how many days included in graph.
        inputId = "days", label = "Days shown:", min = 3, max = 60, 
        value = 40, step = 1, orientation = ori, 
        format = shinyWidgets::wNumbFormat(decimals = 0), 
        color = "#27ae60", inline = TRUE,
        height = hei, width = wid), shiny::br(),
      
      shinyWidgets::awesomeCheckbox("show_extras", "Show even more extra options."), shiny::br(),
      
      shiny::conditionalPanel(
        condition = "input.show_extras == true", 
        
        shinyWidgets::noUiSliderInput( # Slider to adjust how many days before minimum deaths are reached to include.
          inputId = "back", label = "d-x shown:", min = 1, max = 10, 
          value = 3, step = 1, orientation = ori, 
          format = shinyWidgets::wNumbFormat(decimals = 0), 
          color = "#27ae60", inline = TRUE,
          height = hei, width = wid), 
        
        shiny::numericInput("plt_hei", "Plot Height:", value = 650)
      ) # close conditionalPanel
    ) # close conditionalPanel
    
  ), # close dashboardSidebar
  
  shinydashboard::dashboardBody(
    shiny::fluidRow(
      shiny::column(
        width = 9, 
        plotly::plotlyOutput("days_since_min") # Show plot.
      ), 
      shiny::column(
        width = 3,
        DT::dataTableOutput("dtimes"), 
        shinyWidgets::awesomeCheckbox("show_explanation", "Show explanation.", value = TRUE), shiny::br(),
        shiny::conditionalPanel(
          condition = "input.show_explanation == true", 
          shiny::HTML(
            paste("How long does it take before changes in social distancing,", 
                  " lockdowns, or other Covid-19 countermeasures take effect?", 
                  "Perhaps longer than you'd think.")), 
          shiny::br(), shiny::br(),
          shiny::HTML(
            paste("This shows daily deaths projections (lines) based on", 
                  "the last few days of deaths (circle markers) available.")), 
          shiny::br(), shiny::br(),
          shiny::HTML(
            paste("The projections are currently exponential models. ", 
                  "[They are fitted to the last `Prediction period`, points", 
                  "available, but if fewer points are shown, they will fit to fewer.]", 
                  "Since models are fitted to specific points, the projection and", 
                  "its associated doubling times (shown above) can be tweaked", 
                  "by changing the `Prediction period` or `d-x shown`", 
                  "(under extra options).")), 
          shiny::br(), shiny::br(),
          shiny::HTML(
            paste("`Countermeasure effectiveness` assumes that today,", 
                  "countermeasures are implemented to reduce R.", 
                  "Over the following days, their effectiveness", 
                  "(0 = no change in growth rate, 0.5 = R is halved, etc.)",
                  "fades in according to the lognormal distribution for onset-to-death", 
                  "<a href=https://www.mdpi.com/2077-0383/9/2/538/htm#table_body_display_jcm-09-00538-t002>", 
                  "in Table 2</a>.")), 
          shiny::br(), shiny::br(),
          shinyWidgets::awesomeCheckbox("show_extra_explanation", "Please elaborate."), shiny::br(),
          shiny::conditionalPanel(
            condition = "input.show_extra_explanation == true", 
            shiny::HTML(
              paste("Up to 6 countries can be selected simultaneously,", 
                    "but I don't recommend that; it's easy to show too much.")), 
            shiny::br(), shiny::br(),
            shiny::HTML(
              paste("We use a cutoff for inclusion -- cumulative minimum deaths", 
                    "to avoid dealing with countries for which there is barely", 
                    "any data. `Minimum deaths` lets you tweak it.")),
            shiny::br(), shiny::br(),
            shiny::HTML(
              paste("`Days shown` lets you zoom in and out to focus on seeing", 
                    "the data (useful if there are only a few points) or seeing", 
                    "the shape of the projection.")), 
            shiny::br(), shiny::br(),
            shiny::HTML(
              paste("You can change the height of the plot under extra options.")), 
            shiny::br(), shiny::br(),
            shiny::HTML(
              paste("To comment, offer help (including helping me find better", 
                    "references or improve the math behind the models),", 
                    "criticize, or borrow code, see",
                    "<a href=https://github.com/IskanderBlue/canada_covid_19_>", 
                    "https://github.com/IskanderBlue/canada_covid_19_</a>."))
          ) # close conditionalPanel
        ) # close conditionalPanel
      ) # close column
    ) # close fluidRow
  ) # close dashboardBody
) # close dashboardPage

# Define server logic required
server <- function(input, output, session) {
  # Adjusts 'days since' according to min deaths inputted.
  output$days_since <- shiny::renderText({paste0("Days since ", input$mind, " Deaths")})
  # Returns a list of dataframes with 
  # 1) daily deaths joined by days since cumulative minimum deaths reached, and 
  # 2) the dates of those deaths.
  valid_countries <- shiny::reactive({
    countries_with_deaths <- deaths_by_country[ # Drop countries with insufficient deaths.
      c(FALSE, sapply(deaths_by_country[2:length(deaths_by_country)], 
                      function(x) max(x, na.rm = T) > input$mind))]
    deaths_by_day <- as.data.frame(diff(as.matrix(countries_with_deaths))) # Subtract each row from next; get daily change in deaths.
    death_dates <- apply(deaths_by_day, 2, function(x) {as.character(deaths_by_country$Date[-1])}) # Replace all death numbers with their dates (for later use)
    dropped <- apply(deaths_by_day, 2, dropper, input$mind) # Get list (won't fit in data.frame) of vectors of deaths after the cumulative minimum is reached (along with the position of the first element)
    tacked <- lapply(1:length(dropped), tack_days_since, dropped, deaths_by_day, input$back) # Tack days since cumulative minimum reached onto daily deaths for later joining.
    tacked_dates <- lapply(1:length(dropped), tack_days_since, dropped, death_dates, input$back) # Tack days since cumulative minimum reached onto daily deaths' dates for later joining.
    # Merge the data (and then the dates) together by days since cumulative minimum was reached.
    merged <-       merge(tacked[[1]], tacked[[2]], by = "days", all = TRUE)
    merged_dates <- merge(tacked_dates[[1]], tacked_dates[[2]], by = "days", all = TRUE) 
    for (i in 3:length(tacked)) {
      merged <- merge(merged, tacked[[i]], by = "days", all = TRUE)
      merged_dates <- merge(merged_dates, tacked_dates[[i]], by = "days", all = TRUE)
    }
    # Truncate (or extend) rows according to how many days we want to show in the graph.
    merged <- merged[1:input$days, ]
    merged_dates <- merged_dates[1:input$days, ]
    return(list(merged, merged_dates))
  })

  # Build plotly plot 
  plt <- shiny::reactive({
    countries <- input[["countries_selected"]]
    plt <- plotly::plot_ly(type = 'scatter', mode = 'lines+markers',
                           height = input$plt_hei) 
    
    doubling_times <- list()
    for (i in countries) {
      plt_df <- valid_countries()[[1]] %>% 
        dplyr::rename(y=i) %>% # Rename the country to y for ease of use
        dplyr::mutate(periods = (input$back * -1):(nrow(.)-(input$back + 1)) ) %>% # get periods eg. c(-3, -1, -2, 0, 1, 2, 3, 4)
        dplyr::mutate(z = valid_countries()[[2]][, i]) %>% # Add row with dates for country y
        dplyr::mutate(days = c(paste0("d", (input$back * -1):(-1)), # add dates for all rows (otherwise no x values beyond last current date)
                               paste0("d+", 0:(nrow(.)-(input$back + 1)))))
      
      fit_pos <- find_position(plt_df$y, input$pred_per) # Get positions of deaths to fit model
      periods <- plt_df$periods[fit_pos] # select only periods desired to fit model
      mdl <- lm(log(plt_df$y[fit_pos]+0.01) ~ periods) # fit model
      doubling_times[[i]] <- round(log(2) / mdl$coefficients[[2]], 2)
      itrs <- plt_df$periods[length(plt_df$periods)] - periods[length(periods)] # Number of times to iterate through predict_not_na
      predicted <- exp(predict(mdl, list(periods=plt_df$periods[1:fit_pos[length(fit_pos)]]))) # "Predict" up to present with model
      predicted <- predict_for_na(predicted, itrs, mdl$coefficients[[2]], input$r) # predict based on model and reduction in R
      plt_df$preds <- predicted

      plt <- plt %>%
        # Add prediction lines
        plotly::add_trace(data = plt_df, x = ~days, y = ~preds, 
                          name = i,
                          # line = list(color = trace_col), 
                          showlegend = FALSE,
                          mode = 'lines', 
                          hoverinfo = "text+name", 
                          hovertemplate = "%{y:.2f} predicted for") %>%
        # Add death data points
        plotly::add_trace(data = plt_df, x = ~days, y = ~y, 
                          name = i,
                          mode = 'markers', 
                          # marker = list(color = trace_col), 
                          hoverinfo = "text+name",
                          text = ~paste0(z, ", ", y, " deaths in"))
    }
    plt <- plt %>% # Fix layout: y-axis, colours.
      plotly::layout(yaxis = list(title = "Daily Deaths"),
                     colorway = c("#FFFFFF", RColorBrewer::brewer.pal(
                       n = (length(countries) * 2), name = "Paired"))
                     # paper_bgcolor = "rgb(0, 0, 0)", # Background behind/surrounding plot
                     # plot_bgcolor = "rgb(34, 45, 50)" # Plot background
                     )
    
    doubling_times <- doubling_times %>% as.data.frame() %>% t() 
    colnames(doubling_times) <- "Doubling Times (days)"
    
    return(list(plt, doubling_times))
  })
  output$days_since_min <- plotly::renderPlotly({plt()[[1]]})
  output$dtimes <- DT::renderDataTable({DT::datatable( plt()[[2]] , 
                                                       options = list(dom = 'ti')) })
  
  # UI for selecting countries.
  output$country_selector <- shiny::renderUI({
    shiny::selectizeInput(
      "countries_selected", label = "Select countries:", 
      choices = shiny::isolate(names(valid_countries()[[1]])[-1]), 
      selected = c("Canada"), #, "US", "Italy", "Korea, South"), 
      multiple = TRUE, 
      options = list(maxItems = 6))
  })
  
  # When valid countries changes, change country selection options.
  shiny::observeEvent(valid_countries()[[1]], {
    selected_countries <- input$countries_selected
    shiny::updateSelectizeInput(session, "countries_selected", 
                            choices = names(valid_countries()[[1]])[-1], 
                            selected = selected_countries)
  }, ignoreInit = TRUE)
  
  # Ensure days before min deaths reached never crowd out d+0.
  shiny::observeEvent(input$days, {
    backs <- input$back
    if (input$days <= 10) {
      shinyWidgets::updateNoUiSliderInput(session, "back", value = backs, 
                                          range = c(1, input$days-1))
    } else {
      shinyWidgets::updateNoUiSliderInput(session, "back", value = backs, 
                                          range = c(1, 10))
    }
  }, ignoreInit = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)

# Update shinyapps.io
# rsconnect::deployApp()

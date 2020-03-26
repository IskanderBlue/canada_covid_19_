names(deaths_by_country)
match(c(1, 2, 2, 2, 3), c(2, 3))
x <- c(0, 0, 0, 1, 3, 3, 9, 9, 9, 12)
x>= 3
min(which(x>=3))
cdths <- deaths_by_day$Canada[54:length(deaths_by_day$Canada)]
cdt <- data.frame(index = 1:10, deaths = cdths)
plot(cdt$deaths)
cmod <- lm(log(deaths)~index, data = cdt)
exp(predict(cmod))
exp(predict(cmod, data.frame(index = 20)))

exp(1.693147)
exp(1)*2
doubler <- 0.693147
exp(doubler)
exp(-doubler)
# Use doubler
# take line
# From 17 to 27 days out, (~~17 to 18.5 days from onset to death; ~5 days incubation)
# fade in model$coefficients[[2]] --> model$coefficients[[2]] - doubler
# line's data has to be manually added before graphing
# How to fade the data in?

as.character(deaths_by_country$Date)
  countries_with_deaths <- deaths_by_country[
    c(FALSE, sapply(deaths_by_country[2:length(deaths_by_country)], 
                    function(x) max(x, na.rm = T) > 10))] 
  deaths_by_day <- as.data.frame(diff(as.matrix(countries_with_deaths))) 
  death_dates <- apply(deaths_by_day, 2, function(x) {as.character(deaths_by_country$Date[-1])})
  dropped <- apply(deaths_by_day, 2, dropper, 10)
  tacked <- lapply(1:length(dropped), tack_days_since, dropped, deaths_by_day, 3)
  tacked_dates <- lapply(1:length(dropped), tack_days_since, dropped, death_dates, 3)
  merged <-       merge(tacked[[1]], tacked[[2]], by = "days", all = TRUE)
  merged_dates <- merge(tacked_dates[[1]], tacked_dates[[2]], by = "days", all = TRUE)
  for (i in 3:length(tacked)) {
    merged <- merge(merged, tacked[[i]], by = "days", all = TRUE)
    merged_dates <- merge(merged_dates, tacked_dates[[i]], by = "days", all = TRUE)
  }
  merged <- merged[1:20, ]
  merged_dates <- merged_dates[1:20, ]

  countries <- c("Canada", "US", "Italy", "Korea, South")
  plt <- plotly::plot_ly(merged, type = 'scatter', mode = 'lines+markers',
                         height = 650) 
  for (i in countries) {
    plt_df <- merged %>% dplyr::rename(y=i) 
    plt <- plt %>%
      plotly::add_trace(data = plt_df, x = ~days, y = ~y, 
                        name = i, # paste0("Deaths in ", i), 
                        mode = 'lines+markers')
  }
  plt <- plt %>% plotly::layout(yaxis = list(title = "Deaths"), 
                                # paper_bgcolor = "rgb(0, 0, 0)",
                                paper_bgcolor = "rgb(34, 45, 50)")
  plt

  #### ESTIMATE doubling time -- regression, R0.  
  mdf <- merged %>% mutate(periods = -3:(nrow(.)-4))
  model <- lm(log(mdf$Italy) ~ mdf$periods)
  # anova(model)
  summary(model)
  # plot(model)
  
  
  exp(predict(model,list(Time=timevalues)))
  timevalues <- -3:6
  Counts.exponential2 <- exp(predict(model,list(Time=timevalues)))
  plot(mdf$periods, mdf$Italy,pch=16)
  lines(timevalues, Counts.exponential2,lwd=2, col = "red", xlab = "Time (s)", ylab = "Counts")
    trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
  group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
  weight <- c(ctl, trt)
  lm.D9 <- lm(weight ~ group)
  lm.D90 <- lm(weight ~ group - 1) # omitting intercept
  
  anova(lm.D9)
  summary(lm.D90)
  ?gl
  
  log(2) / log(1 + 0.33)
  diffed <- deaths_by_country %>% select(-Date) %>% apply(2, diff)
plot(as.data.frame(diffed)$Canada)  
# So, we've got a distribution --> 
# new infections based on 
#   existing infections and 
#   R
# Then, after a time lag -- distribution approximated with a normal curve
#   we get deaths.  
# we reverse these to get infections at a given time.  
# We run forwards to to get deaths based on R and a time lag

#Task# 
# Model can made lognormal dist????
# get estimate for SI distribution -- see how they relate to doubling times
### FROM https://www.ijidonline.com/article/S1201-9712(20)30091-6/fulltext
# we assumed the serial interval of COVID-19 on the ship was equal to that of COVID-19 in Wuhan, China, with a mean of 7.5 days and a standard deviation of 3.4 days (Li et al., 2020).
# https://www.nejm.org/doi/10.1056/NEJMoa2001316
# The mean incubation period was 5.2 days (95% confidence interval [CI], 4.1 to 7.0), with the 95th percentile of the distribution at 12.5 days. In its early stages, the epidemic doubled in size every 7.4 days. With a mean serial interval of 7.5 days (95% CI, 5.3 to 19), the basic reproductive number was estimated to be 2.2 (95% CI, 1.4 to 3.9).

# Can tack dates back in; better label than d+x
plot(exp(1:10))
x = rlnorm(500,1,.6)
grid = seq(0,25,.1)
?dlnorm
?density
plot(grid,dlnorm(grid,1,.6),type="h",xlab="x",ylab="f(x)")
lines(density(x),col="red")
?plot
hist(x, breaks = 50, freq = FALSE)
hist(x, breaks = 50,# histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "temp",
     main = "Beaver #1")
lines(density(x), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")
?hist
legend("topright",c("True Density","Estimate"),lty=1,col=1:2)




carrots <- data.frame(length = rlnorm(500, 1, .6))
cukes <- data.frame(length = dlnorm(seq(0,25,.1),1,.6))

x <- 10000
car <- table(cut(rlnorm(x, 1, .6), seq(0,25.1,.1))) %>% unname() %>% as.vector()
carrots <- data.frame(length = car/x)
# Now, combine your two dataframes into one.  
# First make a new column in each that will be 
# a variable to identify where they came from later.
# carrots$veg <- 'carrot'
# cukes$veg <- 'cuke'
plot(x = 1:251, y = (car/1000))
lines(cukes)
lines(dense$y*7.7)
dense <- density(car)
?density
plot(cukes)
# and combine into your new data frame vegLengths
vegLengths <- rbind(carrots, cukes)


ggplot(vegLengths, aes(length, fill = veg)) + geom_density(alpha = 0.2)
sc <- readRDS("simulate_cases.rds")

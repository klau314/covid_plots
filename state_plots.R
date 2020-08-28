install.packages("plotly")
install.packages("lazyeval", lib = 'C:/Users/klau314/Desktop/repos/')
pacman::p_load(data.table, dplyr, ggplot2, plotly)

data <- data.table(new_cases = c(0,2,3,7,5,10,11,31,10,22,34,26,105,99,91,111,74,127,
                                  135,108,175,189,148,269,203,225,248,111,627,516),
                   total_cases = c(1,3,6,13,18,28,39,70,80,102,136,162,267,366,457,568,
                                    642,769,904,1012,1187,1376,1524,1793,1996,2221,2469,
                                    2580,3207,3723),
                   state = 'WA')

data2 <- data.table(total_cases = c(2,2,3,8,11,15,17,27,33,64,66,99,121,146,197,287,
                                    420,555,620,800,1097,1387,1643,2198),
                    new_cases = c(0,0,1,5,3,4,2,10,6,31,2,33,22,25,51,90,133,135,65,
                                  180,297,290,256,555),
                    state = 'GA')
data <- rbind(data,data2)



dt_states <- fread('C:/Users/klau314/Desktop/repos/covid-19-data/us-states.csv')

dt_states <- dt_states[order(date)]
dt_states <- dt_states[, new_cases := rep(diff(cases, lag = 1),length.out = .N), by = 'state']
dt_states[, new_cases_lag := shift(new_cases, n = 1), by = 'state']

dt_states <- dt_states[!is.na(new_cases_lag)]

dt_states[, index := 1:.N, by = 'state']
dt_states[, most_recent := .N, by = 'state']

dt_states[index == 1, new_cases_lag := cases]

dt_states[, new_cases_lag_log := log(new_cases_lag)]
dt_states[new_cases_lag_log < 0, new_cases_lag_log := 0]

ggplot(dt_states[state %in% c('New York', 'Washington', 'New Jersey', 'Georgia', 'California')], aes(x = log(cases), y = log(new_cases_lag),
                 color = state, alpha = 0.1)) +
  geom_point() +
  geom_line() +
  geom_label(data = dt_states[index == most_recent & state %in% c('New York', 'Washington', 'New Jersey', 'Georgia', 'California')], aes(label = state))
  #geom_smooth(method = lm, aes(size = 0.1))


f <- list(
  family = "Calibri",
  size = 18,
  color = "black"
)
x <- list(
  title = "Total Confirmed Cases",
  titlefont = f,
  type = 'log'
)
y <- list(
  title = "New Confirmed Cases Since Previous Day",
  titlefont = f,
  type = 'log'
)

fig <- plot_ly(dt_states,
               x = ~cases, y = ~new_cases_lag,
               text = '',
               hovertext = ~paste('Day: ',index,'Total Cases: ',cases,', New Cases: ', new_cases_lag,', State: ',state),
               hoverinfo = 'text',
               #hoveron = 'points+fills',
               color= ~state, colors = 'Spectral', 
               type = 'scatter', mode = 'lines+markers',
               opacity = 1, 
               selected = list(marker = list(opacity = 1)))

fig <- fig %>% layout(yaxis = y, xaxis = x)
fig




f <- list(
  family = "Calibri",
  size = 18,
  color = "black"
)
x <- list(
  title = "Date",
  titlefont = f,
  type = 'linear'
)
y <- list(
  title = "New Confirmed Cases Since Previous Day",
  titlefont = f,
  type = 'linear'
)

fig <- plot_ly(dt_states,
               x = ~index, y = ~new_cases_lag,
               text = '',
               hovertext = ~paste('Total Cases: ',cases,', New Cases: ', new_cases_lag,', State: ',state),
               hoverinfo = 'text',
               #hoveron = 'points+fills',
               color= ~state, colors = 'Spectral', 
               type = 'scatter', mode = 'lines+markers',
               opacity = 1, 
               selected = list(marker = list(opacity = 1)))

fig <- fig %>% layout(yaxis = y, xaxis = x)
fig


f <- list(
  family = "Calibri",
  size = 18,
  color = "black"
)
x <- list(
  title = "Date",
  titlefont = f,
  type = 'linear'
)



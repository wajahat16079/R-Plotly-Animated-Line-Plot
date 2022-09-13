
library(plotly) ## visualization
library(gapminder) ## animation
library(tidyverse) ## data handling
library(reshape2) ## Melt function 

## Loading Data
df = read.csv("API_NY.GDP.PCAP.CD_DS2_en_csv_v2_4489299.csv", skip = 4)



## keeping only countries and columns of interest 
df = df %>%
  filter(Country.Name %in% c("Pakistan","India","Bangladesh"))%>%
  select(-Country.Code,-Indicator.Name,-Indicator.Code)


## making horizontal data Vertical
Melted = melt(df)

## cleaning Year Column
Melted$variable = gsub("X","",Melted$variable)
Melted$variable = as.numeric(Melted$variable)

## removing extra column
Melted = Melted %>%
  select(-X)

## keeping data for year > 1970 
Melted = Melted %>%
  filter(variable >1970)


## function used to creat data for animation

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}
fig <- Melted %>% accumulate_by(~variable)



fig <- fig %>%
  plot_ly(
    x = ~variable, 
    y = ~value,
    split = ~Country.Name,
    frame = ~frame, 
    type = 'scatter',
    mode = 'lines', 
    line = list(simplyfy = F)
  )

fig <- fig %>% layout(
  title = 'GDP Per Capita: Pakistan, India, Bangladesh',
  xaxis = list(
    title = "Year",
    zeroline = F
  ),
  yaxis = list(
    title = "GDP Per Capita",
    zeroline = F
  ),
  legend = list(x = 0.1, y = 0.9)
) 


fig <- fig %>% animation_opts(
  frame = 200, 
  transition = 0, 
  redraw = FALSE
)
fig <- fig %>% animation_slider(
  hide = T
)
fig <- fig %>% animation_button(
  x = 1, xanchor = "right", y = 0, yanchor = "bottom"
)


fig


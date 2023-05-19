# Correct x ranges
# Add uptake plot


library(shiny)
library(shinyWidgets)
library(glue)
library(shinythemes) # for "yeti" theme
library(shinycssloaders) # for "calculating" spinners
library(Cairo) # for better graphics resolution
options(shiny.usecairo=T)
library(tidyverse)
library(plotly)

# load data
public.app.data.national.totals <- read_csv("data/public_app_data_national_summary.csv")
public.app.uptake.data.national <- read_csv("data/public_app_uptake_data_national.csv")
load("data/dates.RData")

# aesthetics
f1 <- list(
  family = "Arial, sans-serif",
  size = 28,
  color = "black"
)

f2 <- list(
  family = "Arial, sans-serif",
  size = 14,
  color = "grey"
)

# plots

# Plot measures of app uptake: users with app installed and with contact tracing enabled
line.height <- signif(max(public.app.uptake.data.national$app_installed*1.05, na.rm=TRUE),2)
label.height.upper <- line.height*0.3
label.height.lower <- line.height*0.1

uptake_plot <- plot_ly(public.app.uptake.data.national) %>%
  add_lines(x=as.Date("2023-04-27"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2023-04-27"), y=line.height*0.4, text="App\nclosure",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2023-03-28"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2023-03-28"), y=line.height*0.6, text="Announcement that\napp will close down\non 27 April",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2022-04-01"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2022-04-01"), y=label.height.upper, text="End of\nfree testing",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2022-02-24"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2022-02-24"), y=label.height.lower, text="End of\nlegal\nrestrictions",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2022-01-27"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2022-01-27"), y=label.height.upper, text="End of\nPlan B",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-11-27"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-11-27"), y=label.height.lower, text="First\nmeasures\nagainst\nOmicron",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-07-19"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-07-19"), y=label.height.upper, text="Step 4",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-05-17"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-05-17"), y=label.height.lower, text="Step 3",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-04-12"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-04-12"), y=label.height.upper, text="Step 2",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-03-29"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-03-29"), y=label.height.lower, text="Step 1b",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-03-08"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-03-08"), y=label.height.upper, text="Step 1a",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=~date, y=~app_installed,
            line=list(width=4), color=I("#9467bd"), name="App installed") %>%
  add_lines(x=~date, y=~contact_tracing_enabled,
            line=list(width=4), color=I("#e377c2"), name="Contact tracing\nenabled") %>%
  layout(
    xaxis=list(tickfont=f1,
               title="",
               tickvals=tickvals.for.plotting,
               ticktext=format(tickvals.for.plotting, "%b %y")
    ),
    yaxis=list(
      tickfont=f1,
      titlefont=f1,
      title="Measures of app uptake",
      range=c(0,line.height)
    ),
    legend=list(
      font=f1
    )
  )

line.height <- 900000
# Notifications and positive tests on a log scale
N_P_log_plot <- plot_ly(public.app.data.national.totals) %>%
  add_lines(x=as.Date("2023-04-27"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2023-04-27"), y=5, text="App\nclosure",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2023-03-28"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2023-03-28"), y=6, text="Announcement that\napp will close down\non 27 April",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2022-12-06"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2022-12-06"), y=6, text="Self-declaration\nof positive tests",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2022-04-01"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2022-04-01"), y=6, text="End of\nfree testing",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2022-02-24"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2022-02-24"), y=4, text="End of\nlegal\nrestrictions",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2022-01-27"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2022-01-27"), y=6, text="End of\nPlan B",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-11-27"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-11-27"), y=4, text="First\nmeasures\nagainst\nOmicron",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-07-19"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-07-19"), y=6, text="Step 4",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-05-17"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-05-17"), y=5.7, text="Step 3",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-04-12"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-04-12"), y=6, text="Step 2",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-03-29"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-03-29"), y=5.7, text="Step 1b",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-03-08"), y=c(0,line.height), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-03-08"), y=6, text="Step 1a",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=~midweek_date, y=~app_notifications, name="App notifications",
            text=~week_label,
            line=list(width=4), color=I("#1f77b4"),
            hovertemplate = paste(
              '%{y} notifications in the<br>',
              'week %{text}<extra></extra>')
            ) %>%
  add_lines(x=~midweek_date, y=~app_positives, name="Positive tests\nentered into app",
            text=~week_label,
            line=list(width=4), color=I("#ff7f0e"),
            hovertemplate = paste(
              '%{y} positive tests entered<br>',
              'into the app in the<br>',
              'week %{text}<extra></extra>')
            ) %>%
  layout(
    xaxis=list(tickfont=f1,
               title="",
               tickvals=tickvals.for.plotting,
               ticktext=format(tickvals.for.plotting, "%b %y")
    ),
    yaxis=list(
      tickfont=f1,
      titlefont=f1,
      title="Weekly total",
      type="log"
    ),
    legend=list(
      font=f1
    )
  )

# ENPIC = Exposure notifications per index case
ENPIC_plot <- plot_ly(public.app.data.national.totals) %>%
  add_lines(x=as.Date("2023-04-27"), y=c(0,8), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2023-04-27"), y=7, text="App\nclosure",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2023-03-28"), y=c(0,8), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2023-03-28"), y=6, text="Announcement that\napp will close down\non 27 April",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2022-12-06"), y=c(0,8), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2022-12-06"), y=7, text="Self-declaration\nof positive tests",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2022-04-01"), y=c(0,8), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2022-04-01"), y=7, text="End of\nfree testing",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2022-02-24"), y=c(0,8), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2022-02-24"), y=6, text="End of\nlegal\nrestrictions",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2022-01-27"), y=c(0,8), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2022-01-27"), y=7, text="End of\nPlan B",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-11-27"), y=c(0,8), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-11-27"), y=6, text="First\nmeasures\nagainst\nOmicron",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-07-19"), y=c(0,8), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-07-19"), y=7, text="Step 4",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-05-17"), y=c(0,8), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-05-17"), y=6, text="Step 3",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-04-12"), y=c(0,8), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-04-12"), y=7, text="Step 2",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-03-29"), y=c(0,8), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-03-29"), y=6, text="Step 1b",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-03-08"), y=c(0,8), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-03-08"), y=7, text="Step 1a",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=~midweek_date, y=~exposure_notifications_per_index_case,
            text=~week_label,
            line=list(width=4), color=I('darkred'),
            hovertemplate = paste(
              '%{y:.2s} notifications per person who<br>',
              'input a positive test and<br>',
              'agreed to contact tracing<br>',
              'in the week %{text}<extra></extra>')
            ) %>%
  layout(
    xaxis=list(tickfont=f1,
               title="",
               tickvals=tickvals.for.plotting,
               ticktext=format(tickvals.for.plotting, "%b %y")
    ),
    yaxis=list(
      tickfont=f1,
      titlefont=f1,
      title="Exposure notifications per index case"
    ),
    legend=list(
      font=f1
    )
  )

# percent of positive cases from dashboard reported through app 
percent_app_plot <- plot_ly(public.app.data.national.totals) %>%
  filter(midweek_date <= as.Date("2022-12-07")) %>% # the data is no longer comparable after the release of app version 5
  add_lines(x=as.Date("2022-04-01"), y=c(0,100), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2022-04-01"), y=100, text="End of\nfree testing",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2022-02-24"), y=c(0,100), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2022-02-24"), y=80, text="End of\nlegal\nrestrictions",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2022-01-27"), y=c(0,100), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2022-01-27"), y=100, text="End of\nPlan B",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-11-27"), y=c(0,100), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-11-27"), y=80, text="First\nmeasures\nagainst\nOmicron",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-07-19"), y=c(0,100), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-07-19"), y=100, text="Step 4",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-05-17"), y=c(0,100), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-05-17"), y=80, text="Step 3",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-04-12"), y=c(0,100), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-04-12"), y=100, text="Step 2",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-03-29"), y=c(0,100), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-03-29"), y=80, text="Step 1b",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-03-08"), y=c(0,100), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-03-08"), y=100, text="Step 1a",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=~midweek_date, y=~percent_cases_through_app, 
            text=~week_label,
            line=list(width=4), color=I("#2ca02c"),
            hovertemplate = paste(
              '%{y:.2s}% of all positive tests<br>',
              'in England and Wales<br>',
              'were reported through the app<br>',
              'in the week %{text}<extra></extra>')) %>%
  layout(
    xaxis=list(tickfont=f1,
               title="",
               tickvals=tickvals.for.plotting,
               ticktext=format(tickvals.for.plotting, "%b %y"),
               range=c(as.Date("2020-12-17"), as.Date("2022-12-07"))
    ),
    yaxis=list(
      tickfont=f1,
      titlefont=f1,
      title="Percentage of cases\nreported through the app",
      range=c(0,100)
    ),
    legend=list(
      font=f1
    )
  )

# percent of positive cases from dashboard reported through app 
percent_over_16_app_plot <- plot_ly(public.app.data.national.totals) %>%
  filter(midweek_date <= as.Date("2022-12-07")) %>% # the data is no longer comparable after the release of app version 5
  add_lines(x=as.Date("2022-04-01"), y=c(0,100), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2022-04-01"), y=100, text="End of\nfree testing",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2022-02-24"), y=c(0,100), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2022-02-24"), y=80, text="End of\nlegal\nrestrictions",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2022-01-27"), y=c(0,100), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2022-01-27"), y=100, text="End of\nPlan B",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-11-27"), y=c(0,100), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-11-27"), y=80, text="First\nmeasures\nagainst\nOmicron",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-07-19"), y=c(0,100), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-07-19"), y=100, text="Step 4",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-05-17"), y=c(0,100), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-05-17"), y=80, text="Step 3",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-04-12"), y=c(0,100), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-04-12"), y=100, text="Step 2",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-03-29"), y=c(0,100), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-03-29"), y=80, text="Step 1b",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-03-08"), y=c(0,100), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-03-08"), y=100, text="Step 1a",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=~midweek_date, y=~percent_cases_over_16_through_app, 
            text=~week_label,
            line=list(width=4), color=I("#2ca02c"),
            hovertemplate = paste(
              '%{y:.2s}% of all positive tests<br>',
              'amongst 16+ year-olds<br>',
              'in England and Wales<br>',
              'were reported through the app<br>',
              'in the week %{text}<extra></extra>')) %>%
  layout(
    xaxis=list(tickfont=f1,
               title="",
               tickvals=tickvals.for.plotting,
               ticktext=format(tickvals.for.plotting, "%b %y"),
               range=c(as.Date("2020-12-17"), as.Date("2022-12-07"))
    ),
    yaxis=list(
      tickfont=f1,
      titlefont=f1,
      title="Percentage of 16+ cases\nreported through the app",
      range=c(0,100)
    ),
    legend=list(
      font=f1
    )
  )

library(tidyverse)
library(plotly)
library(data.table)
library(glue)
library(here)

############
# LOAD DATA
############

# get public app data from https://stats.app.covid19.nhs.uk/
public.app.data <- read_csv("https://stats.app.covid19.nhs.uk/data/covid19_app_country_specific_dataset.csv?cacheBuster=1627575721036")

# Get national case data
english.case.data <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesBySpecimenDate&format=csv")
welsh.case.data <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=W92000004&metric=newCasesBySpecimenDate&format=csv")

##################
# DATA PROCESSING
##################

public.app.data.totals <- public.app.data %>%
  filter(`Week starting (Wythnos yn dechrau)` >= as.Date("2020-12-17")) %>%
  group_by(`Week starting (Wythnos yn dechrau)`, `Week ending (Wythnos yn gorffen)`) %>%
  summarise("app_positives" = sum(`Positive test results linked to app (Canlyniadau prawf positif)`),
            "app_notifications" = sum(`Contact tracing alert (Hysbysiadau olrhain cyswllt)`)) %>%
  mutate("date" = `Week starting (Wythnos yn dechrau)` + 3) %>%
  mutate("week_label" = glue("{format(date - 3, \"%d %B\")} to {format(date + 3, \"%d %B\")}")) %>%
  ungroup(`Week starting (Wythnos yn dechrau)`,`Week ending (Wythnos yn gorffen)`)

engwales.case.data <- bind_rows(english.case.data, welsh.case.data) %>%
  filter(date >= as.Date("2020-12-01")) %>%
  group_by(date) %>%
  summarise("Cases" = sum(newCasesBySpecimenDate)) %>% # total England + Wales
  mutate("weekly_sum_eng_wales_cases_by_specimen_date" = frollsum(Cases, n=7, fill=NA, align="center")) # since the app data is weekly, we need to compare to a weekly sum

# combine
public.app.data.totals <- left_join(public.app.data.totals, engwales.case.data)

public.app.data.totals <- public.app.data.totals %>%
  mutate("exposure_notifications_per_index_case" = app_notifications / app_positives) %>%
  mutate("percent_cases_through_app" = app_positives / weekly_sum_eng_wales_cases_by_specimen_date * 100)

# tidy
public.app.data.totals <- public.app.data.totals %>%
  select(`Week starting (Wythnos yn dechrau)`,
         `Week ending (Wythnos yn gorffen)`,
         "midweek_date" = date,
         week_label,
         app_positives,
         app_notifications,
         weekly_sum_eng_wales_cases_by_specimen_date,
         exposure_notifications_per_index_case,
         percent_cases_through_app) 

###########
# PLOTTING
###########

# Prepare aesthetics
f1 <- list(
  family = "Helvetica",
  size = 28,
  color = "black"
)

f2 <- list(
  family = "Helvetica",
  size = 22,
  color = "black"
)

first.date <- min(public.app.data.totals$midweek_date)
last.date <- max(public.app.data.totals$midweek_date)
tickvals.for.plotting <- seq(first.date, last.date, by=14)

# Notifications and positive tests on a log scale
N_P_log_plot <- plot_ly(public.app.data.totals) %>%
  add_lines(x=as.Date("2021-07-19"), y=c(0,900000), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-07-19"), y=6, text="Step 4",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-05-17"), y=c(0,900000), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-05-17"), y=5.7, text="Step 3",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-04-12"), y=c(0,900000), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-04-12"), y=6, text="Step 2",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-03-29"), y=c(0,900000), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-03-29"), y=5.7, text="Step 1b",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=as.Date("2021-03-08"), y=c(0,900000), color=I("darkgrey"),
            line=list(width=3), showlegend=FALSE) %>%
  add_annotations(x=as.Date("2021-03-08"), y=6, text="Step 1a",
                  font=f2,
                  xref="x",
                  yref="y",
                  showarrow=FALSE) %>%
  add_lines(x=~midweek_date, y=~app_notifications, name="App notifications",
            line=list(width=4), color=I("#1f77b4")) %>%
  add_lines(x=~midweek_date, y=~app_positives, name="Positive tests\nentered into app",
            line=list(width=4), color=I("#ff7f0e")) %>%
  layout(
    xaxis=list(tickfont=f1,
               title="",
               tickvals=tickvals.for.plotting,
               ticktext=format(tickvals.for.plotting, "%b %d")
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
ENPIC_plot <- plot_ly(public.app.data.totals) %>%
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
            line=list(width=4)) %>%
  layout(
    xaxis=list(tickfont=f1,
               title="",
               tickvals=tickvals.for.plotting,
               ticktext=format(tickvals.for.plotting, "%b %d")
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
percent_app_plot <- plot_ly(public.app.data.totals) %>%
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
  add_lines(x=~midweek_date, y=~percent_cases_through_app, line=list(width=4)) %>%
  layout(
    xaxis=list(tickfont=f1,
               title="",
               tickvals=tickvals.for.plotting,
               ticktext=format(tickvals.for.plotting, "%b %d")
    ),
    yaxis=list(
      tickfont=f1,
      titlefont=f1,
      title="Percentage of all national cases from\ngovernment dashbaord reported through the app",
      range=c(0,100)
    ),
    legend=list(
      font=f1
    )
  )

# save plots using orca
orca(N_P_log_plot, file="plots/N_P_log_plot.png", width=1600, height=800)
orca(ENPIC_plot, file="plots/ENPIC_plot.png", width=1200, height=800)
orca(percent_app_plot, file="plots/percent_app_plot.png", width=1200, height=800)

# save data
write_csv(public.app.data.totals, file="data/public_app_data_summary.csv")

last.datestamp <- Sys.Date()
last.date.of.data <- last.date + 3

# save dates for easy loading
save(first.date, last.date, tickvals.for.plotting,
          last.datestamp, last.date.of.data, 
     file="data/dates.RData")
  
library(tidyverse)
library(plotly)
library(data.table)
library(glue)
library(here)

reticulate::py_run_string("import sys") # to ensure we can use plotly::save_image() later

############
# LOAD DATA
############

# get public app data from https://www.gov.uk/government/publications/nhs-covid-19-app-statistics
public.app.data.national <- read_csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1118462/covid19_app_country_specific_dataset.csv")
public.app.uptake.data.national.raw <- read_csv("https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1118464/covid19_app_data_on_number_of_app_users.csv")

# Get national case data
english.case.data <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesBySpecimenDate&format=csv")
welsh.case.data <- read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=W92000004&metric=newCasesBySpecimenDate&format=csv")

# Get cases by age data; pre-processed via LocalCovidTracker, check they've been updated within the last week
CBA_data_England <- read_csv("https://raw.githubusercontent.com/BDI-pathogens/LocalCovidTracker/master/data/t_E.csv")                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        # for now, manually download from https://coronavirus-staging.data.gov.uk/downloads/demographic/cases/specimenDate_ageDemographic-stacked.csv
CBA_data_Wales <- read_csv("https://raw.githubusercontent.com/BDI-pathogens/LocalCovidTracker/master/data/t_W.csv")                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        # for now, manually download from https://coronavirus-staging.data.gov.uk/downloads/demographic/cases/specimenDate_ageDemographic-stacked.csv

##################
# DATA PROCESSING
##################

public.app.data.national.totals <- public.app.data.national %>%
  mutate("week_starting" = as.Date(`Week starting (Wythnos yn dechrau)`, format="%d/%m/%Y")) %>% 
  filter(week_starting >= as.Date("2020-12-17")) %>%
  group_by(week_starting) %>%
  summarise("app_positives" = sum(`Positive test results linked to app (Canlyniadau prawf positif)`),
            "app_notifications" = sum(`Contact tracing alert (Hysbysiadau olrhain cyswllt)`)) %>%
  mutate("date" = week_starting + 3) %>%
  mutate("week_label" = glue("{format(date - 3, \"%d %B\")} to {format(date + 3, \"%d %B\")}")) %>%
  ungroup()

CBA_data_Wales$cases[which(is.na(CBA_data_Wales$cases))] <- 0 # workaround to cover missing data for Wales

engwales.case.data <- bind_rows(english.case.data, welsh.case.data) %>%
  filter(date >= as.Date("2020-12-01")) %>%
  group_by(date) %>%
  summarise("Cases" = sum(newCasesBySpecimenDate)) %>% # total England + Wales
  mutate("weekly_sum_eng_wales_cases_by_specimen_date" = frollsum(Cases, n=7, fill=NA, align="center")) # since the app data is weekly, we need to compare to a weekly sum

# get the cases in "16 and over"s
CBA_England_and_Wales <- bind_rows(
    CBA_data_England %>% select(date, age_format, cases),
    CBA_data_Wales %>% select(date, age_format, cases)
  ) %>%
  group_by(date, age_format) %>%
  filter(date >= as.Date("2020-12-01")) %>%
  summarise(cases = sum(cases)) %>% 
  ungroup()

cases_16_to_19 <-  CBA_England_and_Wales %>%
  filter(age_format == "15-19") %>%
  mutate("cases_16_to_19" = cases * 4/5) %>%
  select(date, "cases"= cases_16_to_19)

cases_over_19 <- CBA_England_and_Wales %>%
  filter(!age_format %in% c("0-4", "5-9", "10-14", "15-19"))

cases_over_16 <- bind_rows(cases_16_to_19, cases_over_19) %>%
  group_by(date) %>%
  summarise(cases_over_16 = sum(cases)) %>%
  mutate("weekly_sum_eng_wales_cases_by_specimen_date_over_16" = frollsum(cases_over_16, n=7, fill=NA, align="center"))

# plot for sanity check:
# plot_ly(cases_over_16) %>%
#   add_lines(x=~date, y=~cases_over_16) %>%
#   add_lines(x=~date, y=~weekly_sum_eng_wales_cases_by_specimen_date_over_16)

public.app.uptake.data.national <- public.app.uptake.data.national.raw %>%
  mutate("date" = as.Date(`Date (Dyddiad)`, format="%d/%m/%Y"),
         "app_installed" = frollmean(`Users with app installed (Defnyddwyr gyda ap wedi'i osod)`, fill=NA, n=7, align="center"),
         "contact_tracing_enabled" = frollmean(`Users with contact tracing enabled (Defnyddwyr ag olrhain cyswllt wedi'u galluogi)`, fill=NA, n=7, align="center")) %>%
  select(date, app_installed, contact_tracing_enabled) %>%
  ungroup()

# keep note first and last dates of app data
# note that gov dashboard data can be a week "ahead" of app data, so we need to right-censor to the last date of app data
first.date <- min(public.app.data.national.totals$date)
last.date <- max(public.app.data.national.totals$date)

# combine
public.app.data.national.totals <- left_join(public.app.data.national.totals, engwales.case.data)
public.app.data.national.totals <- left_join(public.app.data.national.totals, cases_over_16)

public.app.data.national.totals <- public.app.data.national.totals %>%
  mutate("exposure_notifications_per_index_case" = app_notifications / app_positives) %>%
  mutate("percent_cases_through_app" = app_positives / weekly_sum_eng_wales_cases_by_specimen_date * 100) %>%
  mutate("percent_cases_over_16_through_app" = app_positives / weekly_sum_eng_wales_cases_by_specimen_date_over_16 * 100)

# tidy
public.app.data.national.totals <- public.app.data.national.totals %>%
  select(week_starting,
         "midweek_date" = date,
         week_label,
         app_positives,
         app_notifications,
         weekly_sum_eng_wales_cases_by_specimen_date,
         exposure_notifications_per_index_case,
         percent_cases_through_app,
         percent_cases_over_16_through_app) 

###########
# PLOTTING
###########

# Prepare aesthetics
f1 <- list(
  family = "Arial, sans-serif",
  size = 28,
  color = "black"
)

f2 <- list(
  family = "Arial, sans-serif",
  size = 16,
  color = "grey"
)

tickvals.for.plotting <- seq.Date(as.Date("2020-10-01"), last.date, by="month") # note that gov dashboard data can be a week "ahead" of app data, so we're right-censoring to the last date of app data here

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
            line=list(width=4), color=I("#2ca02c")) %>%
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
      title="Percentage of cases from government dashbaord\nreported through the app",
      range=c(0,100)
    ),
    legend=list(
      font=f1
    )
  )


# percent of positive cases over 16 from dashboard reported through app 
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
  add_lines(x=~midweek_date, y=~percent_cases_over_16_through_app, line=list(width=4), color=I("#2ca02c")) %>%
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
      title="Percentage of cases aged 16+ from government dashbaord\nreported through the app",
      range=c(0,100)
    ),
    legend=list(
      font=f1
    )
  )

# save data
write_csv(public.app.data.national.totals, file="data/public_app_data_national_summary.csv")
write_csv(public.app.uptake.data.national, file="data/public_app_uptake_data_national.csv")

last.datestamp <- Sys.Date()
last.date.of.data <- last.date + 3

# save dates for easy loading
save(first.date, last.date, tickvals.for.plotting,
          last.datestamp, last.date.of.data, 
     file="data/dates.RData")

# save plots as png
save_image(uptake_plot, file="plots/uptake_plot.png", width=2200, height=800)
save_image(N_P_log_plot, file="plots/N_P_log_plot.png", width=1800, height=800)
save_image(ENPIC_plot, file="plots/ENPIC_plot.png", width=1800, height=800)
save_image(percent_app_plot, file="plots/percent_app_plot.png", width=1800, height=800)
save_image(percent_over_16_app_plot, file="plots/percent_over_16_app_plot.png", width=1800, height=800)


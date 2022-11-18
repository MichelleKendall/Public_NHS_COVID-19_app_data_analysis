ui <- fluidPage( 
                 tags$style(type = "text/css",
                            "label { font-size: 16px; }"
                 ),
                 #tags$head(includeHTML(("google-analytics.html"))), # google analytics token
                 # tags$head(
                 #   tags$style(HTML(".leaflet-container { background: #FFFFFF; }"))
                 # ), # make map backgrounds white
                 # tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 26pt !important; }")), # make 'play' button nicer on slider
                 
  theme = shinytheme("yeti"), # change the theme here; use "themeSelector()" below for an easy way to pick
  #shinythemes::themeSelector(), # use this to dynamically select a theme
  
  titlePanel("NHS COVID-19 app statistics"),
  
  # tabsetPanel(
  #   id="tabs",
  #   
  #   tabPanel(
  #     "England and Wales",
  #     value="tab_England_and_Wales",
      
      sidebarLayout(
        sidebarPanel(
          id = "sidePanel.daily",
          style = "overflow-y: auto; max-height: 100vh", 
          
          HTML("<h4>Plotted here are some key statistics about app usage and performance using UKHSA data from
          <a href=\"https://www.gov.uk/government/publications/nhs-covid-19-app-statistics\" target=\"_blank\"> NHS COVID-19 app statistics</a>
          and the <a href=\"https://coronavirus.data.gov.uk\" target=\"_blank\"> UK government COVID-19 dashboard</a>.
              <br>
              <br>
              
              The app statistics are updated every 2 weeks while the UK government dashboard is updated weekly, both on Thursdays.</h4>"),
          
          # "last updated" info
          uiOutput("updatedInfo"),
          
          HTML("<h4>
              This site is created and maintained by <a href=\"https://michellekendall.github.io/\" target=\"_blank\">Michelle Kendall</a>. 
              Please visit our <a href=\"https://github.com/MichelleKendall/Public_NHS_COVID-19_app_data_analysis\" target=\"_blank\">GitHub page</a> 
              to explore the open source code.
               </h4>"),
          
          h4("Download the data:"),
          
          downloadButton("downloadData", "Download csv"),
            
          hr(),
          
          width=3
        ),
        mainPanel(
          style = "overflow-y: auto; max-height: 100vh", 
          
          h3("Please see the notes below for precise definitions of the measures."),
          
          h2("Weekly rolling averages of devices with the app installed and app users with contact tracing enabled"),
          
          withSpinner(plotlyOutput("uptake_plot", height="60vh"), type=7),
          
          br(),
          
          h2("Weekly totals of positive tests reported through the app and contact tracing alerts received"),
          
          withSpinner(plotlyOutput("N_P_log_plot", height="60vh"), type=7),
          
          br(),
          
          h2("Weekly values of the number of contact tracing alerts per positive test reported through the app"),
          
          withSpinner(plotlyOutput("ENPIC_plot", height="60vh"), type=7),
          
          br(),
          
          h2("Weekly measure of the percentage of all cases in England and Wales from the government dashboard which are reported through the app"),
          
          h3("Please note that the government dashboard data and app data are not perfectly comparable, and that this measure may have become more unreliable since December 2021. Please see below for details."),
          
          withSpinner(plotlyOutput("percent_app_plot", height="60vh"), type=7),
          
          br(),
          
          h2("Weekly measure of the percentage of all cases amongst individuals aged 16 and over (eligible to use the app) in England and Wales which are reported through the app"),
          
          h3("Please note that the government dashboard data and app data are not perfectly comparable, and that this measure may have become more unreliable since December 2021. Please see below for details."),
          
          withSpinner(plotlyOutput("percent_over_16_app_plot", height="60vh"), type=7),
          
          HTML("<h3>Full details of the app data are provided within the NHS COVID-19 app statistics
               <a href=\"https://www.gov.uk/government/publications/nhs-covid-19-app-statistics/nhs-covid-19-app-statistics-notes-on-the-data\" target=\"_blank\"> Notes on the data</a> and 
               within the cover sheets of the individual 
               <a href=\"https://www.gov.uk/government/publications/nhs-covid-19-app-statistics\" target=\"_blank\">ODS data files</a>.</h3>"),
          
          h3("We note here some details which are particularly important for interpretation."),
          
          br(),
          
          HTML("<h3>The number of <b>users with the app installed</b> is estimated based on the number of data packets received on a daily basis, as it is not possible to identify individual users, or individual devices the app is installed on.</h3>"),
          
          HTML("<h3>The number of <b>users with contact tracing enabled</b> is estimated based on the number of users with the app installed where the app is deemed 'usable' and 'contact-traceable'. The app is deemed usable if the version of the app is supported and onboarding has been completed. It is deemed contact-traceable if, in addition to this, Bluetooth is enabled and the user is able to receive notifications about them being in close contact with someone who later tested positive for COVID-19.
               The app is deemed not usable or not contact-traceable if the device is running Android OS versions 6 to 10 and location sharing is disabled.</h3>"),
          
          br(),
          
          HTML("<h3>The number of <b>positive tests reported through the app</b> includes tests booked through the app, where the results are linked back to the app automatically, and tests booked or taken outside of the app that the user manually links to their app using a code.</h3>"),
          
          HTML("<h3>The number of <b>contact tracing alerts</b> received is a count of the notifications received by NHS COVID-19 app users who have been in 'close contact' with someone who has tested positive for COVID-19.</h3>"),
          
          br(),
          
          HTML("<h3>Full details of the government dashboard measure <b>newCasesBySpecimenDate</b> are provided here for 
               <a href=\"https://coronavirus.data.gov.uk/metrics/doc/newCasesBySpecimenDate#england\" target=\"_blank\">England</a> and 
               <a href=\"https://coronavirus.data.gov.uk/metrics/doc/newCasesBySpecimenDate#wales\" target=\"_blank\">Wales</a>.</h3>"),
          
          h3("Comparisons to the government dashboard records of positive tests provide a rough measure of changing behaviours over time
             but carry the following interpretational challenges:"),
          h3("- The government dashboard data for England includes PCR and LFD positive tests, whereas for Wales only PCR tests are counted. 
             The app data includes both PCR and LFD positive results. "),
          h3("- If an app user inputs multiple positive tests on the same day, these are counted as one positive test. 
             However, for privacy reasons, it is not possible to distinguish if a user enters positive tests over multiple days, and they may therefore be counted repeatedly."), 
          h3("These factors may be distorting the metric, particularly since the introduction of policies on 22 December 2021 and 17 January 2022 allowing 'early' release from self-isolation with
             negative lateral flow results, which are likely to have increased the numbers of people taking lateral flow tests in the days following a positive LFD or PCR result."),
          
          
          br(),
          br(),
          hr()
        )
      )
      
    #) , # end tabPanel

    # tabPanel(
    #   "About",
    #   value="tab_about",
    #          style = "overflow-y: auto; height: 100%; position:relative;",
    #          withMathJax(includeMarkdown("markdown/about.md")),
    #          verbatimTextOutput("systeminfo") # server info
    # ) # end "About" tab
  #) # end tabsetPanel
  
) # end ui



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
          
          HTML("<h4>Plotted here are some key statistics about app usage and performance using data from
          <a href=\"https://stats.app.covid19.nhs.uk/\" target=\"_blank\">NHS COVID-19 app support</a>
          and <a href=\"https://coronavirus.data.gov.uk/about-data\" target=\"_blank\">UKHSA</a>.
              <br>
              <br>
              
              The app statistics are updated weekly on a Thursday.</h4>"),
          
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
          
          h2("Weekly totals of positive tests entered through the app and self-isolation notifications received"),
          
          withSpinner(plotlyOutput("N_P_log_plot", height="80vh"), type=7),
          
          br(),
          
          h2("Weekly values of ENPIC: the number of exposure notifications per index case"),
          
          withSpinner(plotlyOutput("ENPIC_plot", height="80vh"), type=7),
          
          br(),
          
          h2("Weekly measure of the percentage of all cases in England and Wales from the government dashboard which are reported through the app"),
          
          h3("Please note that the government dashboard data and app data are not perfectly comparable, and that this measure may have become more unreliable since December 2021. Please see below for details."),
          
          withSpinner(plotlyOutput("percent_app_plot", height="80vh"), type=7),
          
          br(),
          
          h2("Weekly measure of the percentage of all cases amongst individuals aged 16 and over (eligible to use the app) in England and Wales which are reported through the app"),
          
          h3("Please note that the government dashboard data and app data are not perfectly comparable, and that this measure may have become more unreliable since December 2021. Please see below for details."),
          
          withSpinner(plotlyOutput("percent_over_16_app_plot", height="80vh"), type=7),
          
          h3("Comparisons to the government dashboard records of positive tests provide a rough measure of changing behaviours over time
             but carry the following interpretational challenges:"),
          h3("- The government dashboard data for England includes PCR and LFD positive tests, whereas for Wales only PCR tests are counted. 
             The app data includes both PCR and LFD positive results. "),
          h3("- If an app user inputs multiple positive tests on the same day, these are counted as one positive test. 
             However, for privacy reasons, it is not possible to distinguish if a user enters positive tests over multiple days, and they may therefore be counted repeatedly."), 
          h3("These factors may be distorting the metric, particularly since the introduction of policies on 22 December 2021 and 17 January 2022 allowing 'early' release from self-isolation with
             negative lateral flow results, which are likely to have increased the numbers of people taking lateral flow tests in the days following a positive LFD or PCR result."),
          
          HTML("<h3>Full details of the government dashboard measure newCasesBySpecimenDate are provided here for 
               <a href=\"https://coronavirus.data.gov.uk/metrics/doc/newCasesBySpecimenDate#england\" target=\"_blank\">England</a> and 
               <a href=\"https://coronavirus.data.gov.uk/metrics/doc/newCasesBySpecimenDate#wales\" target=\"_blank\">Wales</a>
               .</h3>"),
          
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



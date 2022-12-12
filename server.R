# # Define server logic
server <- function(input, output, session) {
  
  output$uptake_plot <- renderPlotly({
    uptake_plot
  })
  
  output$N_P_log_plot <- renderPlotly({
    N_P_log_plot
    })
  
  output$ENPIC_plot <- renderPlotly({
    ENPIC_plot
  })
  
  output$percent_app_plot <- renderPlotly({
    percent_app_plot
  })
  
  output$percent_over_16_app_plot <- renderPlotly({
    percent_over_16_app_plot
  })
  
  output$updatedInfo <- renderUI({
    HTML(glue("<h4>This site was last updated on {format(last.datestamp, \"%d %B\")} using the available app data up to {format(last.date.of.data, \"%d %B\")}.</h4>"))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      glue("nhs.covid-19.app.data.to.{last.date.of.data}.csv")
    },
    content = function(file) {
      write.csv(
        left_join(public.app.data.national.totals, 
                  public.app.uptake.data.national, by=c("midweek_date" = "date")),
        file)
    }
  )
  
} # end server function

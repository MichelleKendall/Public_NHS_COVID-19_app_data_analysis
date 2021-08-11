# # Define server logic
server <- function(input, output, session) {
  
  output$N_P_log_plot <- renderPlotly({
    N_P_log_plot
    })
  
  output$ENPIC_plot <- renderPlotly({
    ENPIC_plot
  })
  
  output$percent_app_plot <- renderPlotly({
    percent_app_plot
  })
  
  output$updatedInfo <- renderUI({
    HTML(glue("<h4>This site was last updated on {format(last.datestamp, \"%d %B\")} using the available data up to {format(last.date.of.data, \"%d %B\")}.</h4>"))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      glue("nhs.covid-19.app.data.to.{last.date.of.data}.csv")
    },
    content = function(file) {
      write.csv(public.app.data.totals, file)
    }
  )
  
} # end server function

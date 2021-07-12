ulog_to_df = function(path){
  con = DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  dplyr::inner_join(
    dplyr::tbl(con, "LoggerData"),
    dplyr::tbl(con, "Logger"),
    by=c("LoggerId"="Id")
  ) %>%
    dplyr::select(Name, Time, CalibratedSensorSignal) %>%
    dplyr::rename(time = Time, conc=CalibratedSensorSignal, logger=Name) %>%
    dplyr::arrange(time) %>%
    dplyr::collect() %>%
    dplyr::mutate(id = 1:length(time), time = (time - time[[1]]) / 1000, conc=as.integer(conc)) %>% 
    dplyr::relocate(id, time, conc)
}

segment = function(df, minseglen){
  cpt_model = EnvCpt::envcpt(df$conc, models="trendcpt", minseglen=minseglen)$trendcpt
  models = purrr::map2(cpt_model@cpts, dplyr::lead(cpt_model@cpts, default=nrow(df)), function(start, stop){
    browser()
    df[start:stop, ] %>% lm(conc ~ time, data=.)
  })
  models
}

make_plot = function(df){
  plotly::plot_ly(df, x = ~time, y = ~conc, type = 'scatter', mode = 'lines')
}

# Define server logic required to draw a histogram
shiny::shinyServer(function(input, output) {
  output$segments = plotly::renderPlotly({
    if (input$files %>% length > 0){
      df = input$files$datapath %>% 
        dplyr::first() %>%
        ulog_to_df() %>%
        dplyr::filter(logger==paste("Logger", input$logger))
      
        segments = segment(df, input$minseglen)
        make_plot(df, segments)
      }
  })
})

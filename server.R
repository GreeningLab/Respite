library(magrittr)

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
    dplyr::mutate(id = 1:length(time), time = (time - time[[1]]) / 1000) %>% 
    dplyr::relocate(id, time, conc)
}

segment = function(df, minseglen){
  cpt_model = EnvCpt::envcpt(df$conc, models="trendcpt", minseglen=minseglen)$trendcpt
  purrr::map2_dfr(dplyr::lag(cpt_model@cpts, default=0), cpt_model@cpts, function(start, stop){
    rows = df[start:stop, ]
    model = lm(conc ~ time, data=rows)
    rows %>% dplyr::mutate(fitted=fitted(model), lm=list(model), env_model=list(cpt_model))
  })
}

make_plot = function(df, segments){
  plotly::plot_ly() %>%
    plotly::add_trace(name="Data", data=df, x = ~time, y = ~conc, type = 'scatter', mode = 'markers') %>%
    plotly::add_trace(name="Fit", data=segments, x=~time, y=~fitted,  type = 'scatter', mode = 'lines', line = list(color = 'red)', width = 2)) %>%
    plotly::layout(xaxis = list(title = "Time (seconds)"), yaxis = list(title = "Concentration"), hoverdistance=100, hovermode="x unified")
    
  # (
  #   ggplot2::ggplot() +
  #     ggplot2::geom_point(ggplot2::aes(x=time, y=conc), data=df) +
  #     ggplot2::geom_line(ggplot2::aes(x=time, y=fitted, color="red"), data=segments)
  # ) %>%
  #   plotly::ggplotly()
}

# Define server logic required to draw a histogram
shiny::shinyServer(function(input, output) {

  df = reactive({
    if (input$files %>% length == 0){ return(NULL) }
    input$files$datapath %>% 
      dplyr::first() %>%
      ulog_to_df() %>%
      dplyr::filter(logger==paste("Logger", input$logger))
  })
  
  segments = reactive({
    if (input$files %>% length == 0){ return(NULL) }
    segment(df(), input$minseglen)
  })
  
  output$segments = plotly::renderPlotly({
    if (input$files %>% length == 0){ return(NULL) }
    make_plot(df(), segments()) %>% event_register("plotly_click")
  })
  
  selected_segment = reactive({
    if (input$files %>% length == 0){ return(NULL) }
    d <- event_data("plotly_click")
    x = d$x[[1]]
    segment = segments()
    segment %>% dplyr::filter(dplyr::near(time, x, tol=0.5))
  })
  
  output$click = renderPrint({
    if (input$files %>% length == 0){ return(NULL) }
    lm = selected_segment() %>% dplyr::pull(lm)
    summary(lm[[1]])
  })
  
  output$fit = renderPlot({
    if (input$files %>% length == 0){ return(NULL) }
    lm = selected_segment() %>% dplyr::pull(lm)
    plot(lm[[1]], which=1)
  })
})

library(magrittr)
library(shiny)
library(dbplyr)

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
  purrr::pmap_dfr(list(
    seq_along(cpt_model@cpts),
    dplyr::lag(cpt_model@cpts, default=0),
    cpt_model@cpts
  ), function(i, start, stop){
    rows = df[start:stop, ]
    model = lm(conc ~ time, data=rows, model=TRUE)
    rows %>% dplyr::mutate(fitted=fitted(model), lm=list(model), env_model=list(cpt_model), model_number=i)
  })
}

vline = function(x = 0, ...) {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(...)
  )
}

make_plot = function(df, segments, selected_segment){
  plotly::plot_ly() %>%
    plotly::add_trace(name="Data", data=df, x = ~time, y = ~conc, type = 'scatter', mode = 'markers') %>%
    plotly::add_trace(name="Fit", data=segments, x=~time, y=~fitted,  type = 'scatter', mode = 'lines', line = list(color = 'orange', width = 2)) %>%
    plotly::add_trace(name="Selected", data=selected_segment, x=~time, y=~fitted,  type = 'scatter', mode = 'lines', line = list(color = 'red', width = 2)) %>%
    plotly::layout(
      xaxis = list(title = "Time (seconds)"), 
      yaxis = list(title = "Concentration (μmol/L)"), 
      hoverdistance=100, 
      hovermode="x unified",
      shapes=segments %>% dplyr::arrange(time) %>% dplyr::distinct(model_number, .keep_all = TRUE) %>% dplyr::pull(time) %>% purrr::map(vline, color="grey", dash='dash')
    )
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
    make_plot(df(), segments(), selected_segment()) %>% plotly::event_register("plotly_click")
  })
  
  selected_point = reactive({
    if (input$files %>% length == 0){ return(NULL) }
    
    # This returns the single row of data corresponding to where the user clicked
    d = plotly::event_data("plotly_click")
    
    # Return an empty df of the right shape so that it doesn't break downstream
    if (is.null(d)){ return( segments() %>% dplyr::slice_head(n=0) ) }
    
    x = d$x[[1]]
    segment = segments()
    segment %>% dplyr::filter(dplyr::near(time, x, tol=0.5))
  })
  
  selected_segment = reactive({
    # This returns all the rows corresponding to all the data points in the current model segment
    sel = selected_point()
    all_seg = segments()
    
    # Return an empty df of the right shape so that it doesn't break downstream
    if (is.null(sel) || nrow(sel) == 0 || nrow(all_seg) == 0){ return( segments() %>% dplyr::slice_head(n=0) ) }
    
    all_seg %>% dplyr::filter(model_number == sel$model_number)
  })
  
  output$description = renderPrint({
    segment = selected_point()
    if (is.null(segment) || nrow(segment) == 0){ return(NULL) }
    
    lm = segment %>% dplyr::pull(lm) %>% dplyr::first()
    cofs = coefficients(lm)
    equation = stringr::str_glue(
      "f(x) = {slope}x + {intercept}",
      slope=formatC(cofs[[2]], digits=3),
      intercept = formatC(cofs[[1]], digits=3)
    )
    rsquared = (lm %>% vcov() %>% cov2cor() %>% `[[`(1, 2))^2
    stringr::str_glue("{equation}\nR Squared: {r2}", equation=equation, r2=rsquared %>% formatC(digits=3))
  })
  
  output$fit = renderPlot({
    segment = selected_point()
    if (is.null(segment) || nrow(segment) == 0){ return(NULL) }
    
    lm = segment %>% dplyr::pull(lm) %>% dplyr::first()
    cofs = coefficients(lm)
    ggplot2::ggplot(lm$model, ggplot2::aes(x=time, y=conc)) +
      ggplot2::geom_point() + 
      ggplot2::geom_abline(intercept = cofs[[1]], slope=cofs[[2]]) +
      ggplot2::xlab("Time (seconds)") +
      ggplot2::ylab("Concentration (μmol/L)") +
      ggplot2::theme_gray(base_size = 15)
      # ggplot2::theme(text = ggplot2::element_text(size=12))
    })
})

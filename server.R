library(magrittr)
library(shiny)
library(dbplyr)

#' Reads a ulog file and outputs a tibble
#'
#' @param path Filepath to the ulog file
#'
#' @return A tibble
#' @export
#'
#' @examples
ulog_to_df = function(path){
  con = DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  dplyr::inner_join(
    dplyr::tbl(con, "LoggerData"),
    dplyr::tbl(con, "Logger"),
    by=c("LoggerId"="Id")
  ) %>%
    dplyr::inner_join(
      dplyr::tbl(con, "Sensor"),
      by=c("SensorId"="Id"),
      suffix=c("Logger", "Sensor")
    ) %>%
    dplyr::select(NameLogger, Time, NameSensor, CalibratedSensorSignal) %>%
    dplyr::rename(time = Time, conc=CalibratedSensorSignal, logger=NameLogger, sensor=NameSensor) %>%
    dplyr::arrange(time) %>%
    dplyr::collect() %>%
    dplyr::mutate(id = 1:length(time), time = (time - time[[1]]) / 1000) %>% 
    dplyr::relocate(id, time, conc)
}

#' Segments a respirometry data frame
#'
#' @param df Containing a "conc" column, which contains the concentration
#' @param ... Extra parameters to pass into `envcpt`
#'
#' @return
#' @export
#'
#' @examples
segment = function(df, ...){
  cpt_model = EnvCpt::envcpt(df$conc, models="trendcpt", ...)$trendcpt
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


#' Creates one or more vertical lines. This should be used as the input to the shapes argument to `plotly::layout`
#'
#' @param x Vector of x coordinates at which to draw the lines
#' @param ... Additional arguments to impact the drawing of the line
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

#' Make the interactive plotly plot for showing the entire time series of a single logger run
#'
#' @param df The data frame for a single logger run, containing `time`, and `conc` columns
#' @param segments A data frame containing segments, containing `time`, and `fitted` columns
#' @param selected_segment A data frame containing one or more selected rows. This has the same format as, and should be a subset of the `segments` argument.
#'
#' @return The plotly object
#' @export
#'
#' @examples
make_macro_plot = function(df, segments, selected_segment){
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
shiny::shinyServer(function(input, output, session) {
  
  # Load the logger file and convert to data.frame
  all_loggers = reactive({
    withProgress(message = 'Loadding ulog file', value = 0, {
      if (input$files %>% length == 0){ return(NULL) }
      input$files$datapath %>% 
        dplyr::first() %>%
        ulog_to_df()
    })
  })
  
  # Update the logger dropdown in response to a file being selected
  observe({
    if (input$files %>% length == 0){ return(NULL) }
    updateSelectInput(session = session, inputId = 'logger', choices=all_loggers() %>% dplyr::pull(logger) %>% unique())
  })
  
  # Update the sensor dropdown in response to a file being selected
  observe({
    if (input$files %>% length == 0){ return(NULL) }
    updateSelectInput(session = session, inputId = 'sensor', choices=all_loggers() %>% dplyr::pull(sensor) %>% unique())
  })
  
  # If we have all the info we need to render the plot
  ready_to_plot = reactive({
    input$files %>% length > 0 && input$logger != ""
  })
  
  # The data.frame for the selected logger
  df = reactive({
    if (!ready_to_plot()){ return(NULL) }
    all_loggers() %>% dplyr::filter(logger == input$logger, sensor == input$sensor)
  })
  
  # The calculated segments
  segments = reactive({
    if (!ready_to_plot()){ return(NULL) }
    withProgress(message = 'Calculating segments', value = 0, {
      segment(df(), input$minseglen)
    })
  })
  
  output$segments = plotly::renderPlotly({
    if (!ready_to_plot()){ return(NULL) }
    withProgress(message = 'Rendering plot', value = 0, {
      make_macro_plot(df(), segments(), selected_segment()) %>% plotly::event_register("plotly_click")
    })    
  })
  
  selected_point = reactive({
    if (!ready_to_plot()){ return(NULL) }
    
    # This returns the single row of data corresponding to where the user clicked
    d = plotly::event_data("plotly_click")
    
    # Return an empty df of the right shape so that it doesn't break downstream
    if (is.null(d)){ return( segments() %>% dplyr::slice_head(n=0) ) }
    
    x = d$x[[1]]
    segment = segments()
    segment %>% dplyr::filter(dplyr::near(time, x, tol=0.05))
  })
  
  selected_segment = reactive({
    if (!ready_to_plot()){ return(NULL) }
    
    # This returns all the rows corresponding to all the data points in the current model segment
    sel = selected_point()
    all_seg = segments()
    
    # Return an empty df of the right shape so that it doesn't break downstream
    if (is.null(sel) || nrow(sel) == 0 || nrow(all_seg) == 0){ return( segments() %>% dplyr::slice_head(n=0) ) }
    
    all_seg %>% dplyr::filter(model_number == sel$model_number)
  })
  
  output$description = renderTable({
    region = selected_segment()
    segment = selected_point()
    if (is.null(segment) || nrow(segment) == 0 || nrow(region) == 0){ return(NULL) }
    lm = segment %>% dplyr::pull(lm) %>% dplyr::first()
    cofs = coefficients(lm)

    list(
      c('Equation', stringr::str_glue(
        "f(x) = {slope}x + {intercept}",
        slope=formatC(cofs[[2]], digits=3),
        intercept = formatC(cofs[[1]], digits=3)
      ) %>% as.character),
      c('R Squared', lm %>% summary %>% `$`('r.squared') %>% as.character),
      c('Intercept (&mu; mol L<sup>-1</sup>)', cofs %>% `[[`(1) %>% as.character),
      c('Rate (&mu; mol L <sup>-1</sup> s<sup>-1</sup>)', cofs %>% `[[`(2) %>% as.character),
      c('Rate (&mu; mol L <sup>-1</sup> min<sup>-1</sup>)', cofs %>% `[[`(2) %>% `*`(60) %>% as.character),
      c('Segment Start (sec)', region$time %>% min %>% as.character),
      c('Segment End (sec)', region$time %>% max %>% as.character)
    ) %>% purrr::map_dfr(function(l){
      tibble::tibble_row(Attribute=l[[1]], Value=l[[2]])
    })
  }, sanitize.text.function = function(x) x, striped=TRUE, width="100%", colnames = FALSE)
  
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

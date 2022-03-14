library(shiny)
library(shinyBS)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Respite"),
    
    fluidRow(
        column(
            width=4,
            tags$form(
                class = "well",
                role = "complementary",
                helpText("Mouse over each component to obtain more information"),
                shinyBS::tipify(
                    fileInput("files", label = "File input", accept = ".ulog"),
                    title="Choose a .ulog logging file"
                ),
                shinyBS::tipify(
                    selectInput(
                        "logger",
                        "Logger Number",
                        choices = NULL
                    ),
                    title = "The number of the logger to analyse, within the whole experiment"
                ),
                shinyBS::tipify(
                    selectInput(
                        "sensor",
                        "Sensor Name",
                        choices = NULL
                    ),
                    title = "The name of the sensor to analyse, for the selected logger"
                ),
                shinyBS::tipify(
                    selectInput(
                        "penalty_type",
                        "Penalty Type",
                        choices = c(
                            `Bayesian (BIC/SIC)` = "SIC",
                            `Modified Bayesian (MBIC)`="MBIC",
                            `Akaike (AIC)` = "AIC",
                            "Hannan-Quinn",
                            "Manual"
                        ),
                        selected = "MBIC"
                    ),
                    title = "Information criterion to use for penalising segments"
                ),
                shinyBS::tipify(
                    numericInput(
                        "penalty_value",
                        "Manual Penalty",
                        min = 0,
                        value = 100,
                        step = 0.1
                    ),
                    title = "For the manual penalty selected above. The cost of adding a new segment."
                ),
                shinyBS::tipify(
                    numericInput(
                        "minseglen",
                        "Minimum Segment Length",
                        min = 2,
                        value = 3
                    ),
                    title = "The minimum number of seconds allowed per segment. Increase this in order to reduce the number of splits"
                )
            ),
        ),
        column(
            width = 8,
            plotly::plotlyOutput("segments"),
            shinyBS::bsTooltip("segments", title="Click on a line segment to obtain the regression statistics for it.")
        )
    ),

    # Sidebar with a slider input for number of bins
    fluidRow(
        column(
            width=4,
            withMathJax(
                shinyBS::tipify(
                    tableOutput("description"),
                    title= "Regression statistics will appear here after you click on a line segment"
                )
            )
        ),
        column(
            width=8,
            plotOutput("fit")
        )
    )
))

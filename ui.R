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
                        "Sensor ID",
                        choices = NULL
                    ),
                    title = "The name of the sensor to analyse, for the selected logger"
                ),
                shinyBS::tipify(
                    numericInput(
                        "minseglen",
                        "Minimum Segment Length:",
                        min = 2,
                        value = 3
                    ),
                    title = "The algorithm will not produce line segments any smaller than this. Increase this in order to reduce the number of splits"
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

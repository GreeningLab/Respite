library(shiny)
library(shinyBS)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Respite"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            helpText("Mouse over each component to obtain more information"),
            shinyBS::tipify(
                fileInput("files", label = "File input", accept = ".ulog"),
                title="Choose a .ulog logging file",
            ),
            shinyBS::tipify(
                numericInput(
                  "logger",
                  "Logger Number",
                  min = 1,
                  max = 100,
                  value = 1
                ),
                title = "The number of the logger to analyse, within the whole experiment"
            ),
            shinyBS::tipify(
                sliderInput(
                  "minseglen",
                  "Minimum Segment Length:",
                  min = 3,
                  max = 100,
                  value = 30
                ),
                title = "The algorithm will not produce line segments any smaller than this. Increase this in order to reduce the number of splits"
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotly::plotlyOutput("segments"),
            shinyBS::bsTooltip("segments", title="Click on a line segment to obtain the regression statistics for it."),
            plotOutput("fit"),
            shinyBS::tipify(
                verbatimTextOutput("description"),
                title= "Regression statistics will appear here after you click on a line segment"
            )
        )
    )
))

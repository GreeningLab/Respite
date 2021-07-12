library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Respite"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput("files", label = h3("File input")),
            sliderInput("logger",
                        "Logger Number",
                        min = 1,
                        max = 100,
                        value = 1),
            sliderInput("minseglen",
                        "Minimum Segment Length:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotly::plotlyOutput("segments")
        )
    )
))

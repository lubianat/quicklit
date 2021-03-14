library(shiny)
source("utils.R")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("QuickLit"),
    h4("A quick suggestion on a contribution to Wikidata"),
    p("(It takes up to 20 seconds to load, though)"),
    # Sidebar with a slider input for number of bins 
    # Show a plot of the generated distribution
    actionButton("covid", "COVID-19 article"),
            dataTableOutput("candidate_qids")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$candidate_qids <- renderDataTable({
        input$covid
        a <- get_covid_df()
        return(a)
    },
    escape = FALSE,
    options = list(dom = "t"))
}

# Run the application 
shinyApp(ui = ui, server = server)

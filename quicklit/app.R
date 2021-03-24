library(shiny)
source("utils.R")

# User Interface ------------

ui <- fluidPage(
    titlePanel("QuickLit"),
    h4("A quick suggestion on a contribution to Wikidata"),
    p("(It takes up to 20 seconds to load, though)"),
    radioButtons(
        inputId = "radio",
        label = "Type of quick articles",
        choices = c(
            "COVID-19 article with author from Brazil",
            "COVID-19 article",
            "Brazilian bioinformatics article",
            "Advanced"
        ),
        selected = "COVID-19 article with author from Brazil"),
    h3("Advanced"),
    radioButtons(
        inputId = "advanced_radio",
        label = "Type of advanced quick articles",
        choices = c("By author"),
        selected = "By author"),
    p("Example: Q42614737 (Helder Nakaya)"),
    textInput(inputId = "qid",
              label = "Q ID of author",
              value = "Q42614737", width = NULL, placeholder = NULL),
    actionButton("submit", "Go"),
    
    p("Tabernacle: Add main subjects and items that the project uses"),
    p("Author Disambiguator: Disambiguate the authors of the paper"),
    
    tags$a(target = "_blank",
           href = "https://github.com/lubianat/quicklit",
           "GitHub Repository"),
    dataTableOutput("candidate_qids")
)

# Server ------------

server <- function(input, output) {
    
    
    text_reactive <- eventReactive( input$submit, {
        qid <- input$qid
        a <- get_articles_by_author(author_qid = qid)
        return(a)
    })
    
    
    output$candidate_qids <- renderDataTable({
        type_of_article <- input$radio
        if (type_of_article == "COVID-19 article") {
            a <- prepare_dataset_for_page(query = "covid")
        } else if (type_of_article == "COVID-19 article with author from Brazil") {
            a <- prepare_dataset_for_page(query = "covid_brazil")
        } else if (type_of_article == "Brazilian bioinformatics article") {
            a <- prepare_dataset_for_page(query = "bioinfo_brazil")
        } else if (type_of_article == "Advanced"){
            a <- text_reactive()

            
            
        }
        return(a)
    },
    escape = FALSE,
    options = list(dom = "t"))
}

# Run the application ---------
shinyApp(ui = ui, server = server)

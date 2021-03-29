library(shiny)
source("utils.R")

# User Interface ------------

ui <- fluidPage(titlePanel("QuickLit"),
                
                sidebarLayout(
                    sidebarPanel(
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
                            selected = "COVID-19 article with author from Brazil"
                        ),
                        h3("Advanced"),
                        fluidRow(
                            column(8, 
                                   
                                   radioButtons(
                                       inputId = "advanced_radio",
                                       label = "Type of advanced quick articles",
                                       choices = c("By author",
                                                   "By institution",
                                                   "By topic"),
                                       selected = "By author"
                                   ),     
                                   
                                   
                            ),
                            column(4, 
                                   textInput(
                                       inputId = "qid",
                                       label = "Q ID",
                                       value = "Q42614737",
                                       width = NULL,
                                       placeholder = NULL
                                   ),
                                   actionButton("submit", "Submit", class = "btn btn-primary"),
                                  
                            )
                        ),
  
                        p("Author example: Q42614737 (Helder Nakaya)"),
                        p(
                            "Institution example: Q102292035 (Graduate Interdisciplinary Program in Bioinformatics (USP))"
                        ),
                        p("Topic example: Q10509939 (grey hair)"),

                        
                        tags$a(target = "_blank",
                               href = "https://github.com/lubianat/quicklit",
                               "GitHub Repository"),
                        
                    ),
                    mainPanel(
                        p("Tabernacle: Add main subjects and items that the project uses"),
                        p("Author Disambiguator: Disambiguate the authors of the paper"),
                        dataTableOutput("candidate_qids")
                    )
                ))

# Server ------------

server <- function(input, output) {
    text_reactive <- eventReactive(input$submit, {
        qid <- input$qid
        category <- input$advanced_radio
        if (category == "By author") {
            a <- get_articles_by_author(author_qid = qid)
        } else if (category == "By institution") {
            a <- get_articles_by_institution(institution_qid = qid)
        } else if (category == "By topic") {
            a <- get_articles_by_topic(subject_qid = qid)
        }
        
        return(a)
    })
    
    
    output$candidate_qids <- renderDataTable({
        type_of_article <- input$radio
        if (type_of_article == "COVID-19 article")
        {
            a <- prepare_dataset_for_page(query = "covid")
        } else if (type_of_article == "COVID-19 article with author from Brazil") {
            a <- prepare_dataset_for_page(query = "covid_brazil")
        } else if (type_of_article == "Brazilian bioinformatics article") {
            a <- prepare_dataset_for_page(query = "bioinfo_brazil")
        } else if (type_of_article == "Advanced") {
            a <- text_reactive()
        }
        return(a)
    },
    escape = FALSE,
    options = list(dom = "t"))
}

# Run the application ---------
shinyApp(ui = ui, server = server)

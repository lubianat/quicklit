library(shiny)
source("utils.R")

# User Interface ------------

ui <- navbarPage(
    "QuickLit",
    tabPanel("Main",
             sidebarLayout(
                 sidebarPanel(
                     h4("Adding topics and authors to Wikidata"),
                     p("Select one of the topics below to get 6 suggestions of articles to fill"),
                     tabsetPanel(
                         id = "tabset",
                         tabPanel(
                             "Basic",
                             radioButtons(
                                 inputId = "radio",
                                 label = "Type of quick articles",
                                 choices = c(
                                     "COVID-19 article with author from Brazil",
                                     "COVID-19 article",
                                     "Brazilian bioinformatics article"
                                 ),
                                 selected = "Brazilian bioinformatics article"
                             )
                         ),
                         tabPanel(
                             "Advanced",
                             fluidRow(
                                 column(
                                     5,
                                     
                                     radioButtons(
                                         inputId = "advanced_radio",
                                         label = "Type of query",
                                         choices = c("By author",
                                                     "By institution",
                                                     "By topic"),
                                         selected = "By author"
                                     ),
                                     
                                     
                                 ),
                                 column(
                                     7,
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
                             br(),
                             p("Author example: Q42614737 (Helder Nakaya)"),
                             p(
                                 "Institution example: Q102292035 (Graduate Interdisciplinary Program in Bioinformatics (USP))"
                             ),
                             p("Topic example: Q10509939 (grey hair)")
                             
                         ),
                         tabPanel(
                             "Canities",
                             
                             p("Special tab for the canities project"),
                             p("Article were pre-selected based on similarity with the subject"),
                             actionButton("canities", "Refresh", class = "btn btn-primary"),
                             
                             
                             
                             
                         )
                         
                         
                     )
                 ),
                 mainPanel(
                     h1("QuickLit: contributions to Wikidata"),
                     
                     br(),
                     h4("QuickLit lists articles about that are in need of curation."),
                     p("It is quick and painless, and you don't need to know the article:"),
                     
                     h3("Quickstart:"),
                     p("- Click on 'add topics' to tell Wikidata what the article is about. 
                     It is totally ok to infer topics from the title."),
                     p("On the new page, you will need to lick on 'login' on the top right of the new page, and then enter info in the 'main subject' field"),
                     p("- Click on 'add authors' to tell Wikidata who authored that work."),
                     p( "Once the information is added, it will become part of the visualizations at the ",
                        a(target = "_blank", href = 'https://scholia.toolforge.org/#/', 'Scholia platform')
                     ),
                     dataTableOutput("candidate_qids")
                 )
             )),
    tabPanel("Tutorial",
             p("Tutorial (TODO)")),
    tabPanel(
        "GitHub",
        tags$a(target = "_blank",
               href = "https://github.com/lubianat/quicklit",
               "GitHub Repository")
    )
)


# Server ------------

server <- function(input, output) {
    canities_reactive <- eventReactive(input$canities, {
        a <- get_articles_for_canities_project()
        return(a)
        
    })
    
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
        tabset <- input$tabset
        
        if (tabset == "Basic") {
            if (type_of_article == "COVID-19 article")
            {
                a <- prepare_dataset_for_page(query = "covid")
            } else if (type_of_article == "COVID-19 article with author from Brazil") {
                a <- prepare_dataset_for_page(query = "covid_brazil")
            } else if (type_of_article == "Brazilian bioinformatics article") {
                a <- prepare_dataset_for_page(query = "bioinfo_brazil")
            }
            
        } else if (tabset == "Advanced") {
            a <- text_reactive()
        } else if (tabset == "Canities") {
            a <- canities_reactive()
        }
        return(a)
    },
    escape = FALSE,
    options = list(dom = "t"))
}

# Run the application ---------
shinyApp(ui = ui, server = server)

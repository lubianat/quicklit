library(shiny)

source("utils.R")

# User Interface ------------
theme <- bslib::bs_theme(
    bg = "white",
    fg = "black",
    primary = "darkgreen",
    secondary = "wheat",
    base_font = "Lato",
    heading_font = "arial",
)

ui <- navbarPage(
    "QuickLit",
    theme = theme,
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
                             ),
                             actionButton("basic-refresh", "Refresh", class = "btn btn-primary"),
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
                         ),
                         h3("Quickstart"),
                         p("Click on the Refresh button to list articles, and on  a link to open a new tab for the editing page. Click on:"),
                         tags$ul(
                             tags$li(tags$b("add topics"), " to tell us what the article is about. 
                     (You can infer from the title)."), 
                             tags$li(tags$b("tag authors"), " to tell us who authored that work."), 
                             tags$li(tags$b("see profile"), " to open the article on Scholia")
                         ),
                         p("Want to batch add a topic to all titles that match a string? Try ",
                           a(target = "_blank", href = 'https://lubianat.shinyapps.io/topictagger/',"TopicTagger"), "!")
                         
                         
                         
                     )
                 ),
                 mainPanel(
                     h4("Help with articles in need of curation."),
                     p("It is quick and painless, and you don't need to know the article"),
                     p("Check the Quickstart on the leftl to get started"),
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
    
    
    basic_reactive <- eventReactive(input$`basic-refresh`, {
        type_of_article <- input$radio
        if (type_of_article == "COVID-19 article")
        {
            a <- prepare_dataset_for_page(query = "covid")
        } else if (type_of_article == "COVID-19 article with author from Brazil") {
            a <- prepare_dataset_for_page(query = "covid_brazil")
        } else if (type_of_article == "Brazilian bioinformatics article") {
            a <- prepare_dataset_for_page(query = "bioinfo_brazil")
        }
        return(a)
        
    })
    
    output$candidate_qids <- renderDataTable({
        type_of_article <- input$radio
        tabset <- input$tabset
        
        if (tabset == "Basic") {
            a <- basic_reactive()
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

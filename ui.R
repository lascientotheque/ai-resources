library(shiny)
library(shinydashboard)
library(DT)
library(shinythemes)
library(markdown)

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  windowTitle = "IA resources",
  titlePanel(withTags(
    div("Catalogue de ressources pédagogiques sur l'intelligence artificielle",
        div(class = 'pull-right',
            a(href = 'https://github.com/lascientotheque/ia-resources',
              icon('github'))), hr()),
  ), 
  ),
  tabsetPanel(
    tabPanel("Accueil",
             tags$div(
               style="margin-bottom:50px;",
             ),
             includeMarkdown("content/About.md")
    ),
    tabPanel("Recherche de ressources",
             fluidPage(
               fluidRow(
                 tags$div(
                   style="margin-bottom:50px;",
                 ),
                 column(6,
                        selectInput("language", "Langue", 
                                    all_languages),
                        selectInput("resource_type", "Type de ressource", 
                                    all_resource_types),
                        selectInput("age", "Age", all_ages),
                 ),
                 column(6,
                        selectInput("category", "Catégorie", 
                                    all_categories),
                        selectInput("connectivity", "Connectivité", 
                                    all_connectivity),
                        selectInput("data_type", "Type de données", 
                                    all_data_types),
                        selectInput("other_keywords", "Autres mots-clefs", 
                                    all_other_keywords),
                 )
               ),
               fluidRow(
                 h3("Ressources"),
                 DT::DTOutput("resources")
               )
             ),
    ),
    tabPanel("Contribuer",
             tags$div(
               style="margin-bottom:50px;",
             ),
             includeMarkdown("content/Contribute.md")
    )
  )
)




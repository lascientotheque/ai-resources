library(shiny)
library(shinydashboard)
library(DT)
library(shinythemes)
library(markdown)
library(shiny.i18n)

i18n <- Translator$new(translation_csvs_path = "translation/")

ui <- fluidPage(
  shiny.i18n::usei18n(i18n),
  div(
    style = "float: right;",
    selectInput(
      'selected_language',
      i18n$t(""),
      choices = i18n$get_languages(),
      selected = i18n$get_key_translation()
    )
  ),
  tags$head(tags$link(rel = "shortcut icon", href = "Robot.ico")),
  theme = shinytheme("cosmo"),
  windowTitle = "AI catalog",
  titlePanel(
    tags$div(
      i18n$t("Artificial intelligence resource directory"),
      hr(),
      tags$head(
        tags$link(rel = "icon", href = "robot.ico"),
        tags$title("IA resources")
      )
    )
  ),
  tabsetPanel(
    tabPanel(
      i18n$t("Home"),
      tags$div(style = "margin-bottom:50px;",),
      column(12, align="justify",
         uiOutput("about_page"),
         img(src='./img/site/footer_sciento.jpg'),
         img(src='./img/site/footer_bemaker.jpg'),
         img(src='./img/site/footer_fari.jpg'),
         img(src='./img/site/footer_ehs.jpg'),
         img(src='./img/site/footer_dbsf.jpg'),
         img(src='./img/site/footer_bosa.jpg'),
      ),
      
    ),
    tabPanel(i18n$t("Search resources"),
             fluidPage(
               fluidRow(
                 tags$div(style = "margin-bottom:50px;",),
                 uiOutput("filter_column_1"),
                 uiOutput("filter_column_2"),
                 uiOutput("filter_column_3"),
               ),
               fluidRow(h3(i18n$t("Resources")),
                        DT::DTOutput("resources"))
             ),),
    tabPanel(
      i18n$t("Contribute"),
      tags$div(style = "margin-bottom:50px;",),
      column(12, align="justify",
             uiOutput("contribute_page")
      )
    )
  )
)

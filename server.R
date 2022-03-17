library(shinythemes)
library(jsonlite)
library(shiny.i18n)

i18n <- Translator$new(translation_csvs_path = "translation/")
i18n$set_translation_language("English")

text_for_resource_filters <-
  read_json("text_for_resource_filters.json", simplifyVector = TRUE)

languages_long_short <- list("English" = "en",
                             "Français" = "fr",
                             "Dutch" = "nl")

server <- function(input, output, session) {
  
  # Language selection
  observeEvent(input$selected_language, {
    shiny.i18n::update_lang(session, input$selected_language)
  })
  
  ################
  # User Interface 
  ################
  
  output$about_page <- renderUI({
    language <- languages_long_short[[input$selected_language]]
    about_file <- paste0("content/About_",language,".md",collapse="")
    includeMarkdown(about_file)
  })
  
  output$contribute_page <- renderUI({
    language <- languages_long_short[[input$selected_language]]
    contribute_file <- paste0("content/Contribute_",language,".md",collapse="")
    includeMarkdown(contribute_file)
  })
  
  output$filter_column_1 <- renderUI({
    language <- languages_long_short[[input$selected_language]]
    
    column(
      4,
      selectInput(
        "language",
        i18n$t("Language"),
        text_for_resource_filters$languages[[language]]
      ),
      selectInput(
        "resource_type",
        i18n$t("Resource type"),
        text_for_resource_filters$resource_types[[language]]
      ),
      selectInput("age", i18n$t("Age"),
                  text_for_resource_filters$ages[[language]]),
      selectInput(
        "category",
        i18n$t("Category"),
        text_for_resource_filters$categories[[language]]
      ),
      #checkboxInput(
      #  "show_advanced_filters",
      #  i18n$t("Advanced filters"),
      #  FALSE
      #),
      
    )
  })
  
  output$filter_column_2 <- renderUI({
    language <- languages_long_short[[input$selected_language]]
    req(input$show_advanced_filters == TRUE)
    
    column(
      4,
      selectInput("connectivity", "Connectivité",
                  all_connectivity),
      selectInput("data_type", "Type de données",
                  all_data_types),
      selectInput("other_keywords", "Autres mots-clefs",
                  all_other_keywords)
    )
  })
  
  output$filter_column_3 <- renderUI({
    language <- languages_long_short[[input$selected_language]]
    req(input$show_advanced_filters == TRUE)
    
    column(4,
           selectInput("licence", "Licence",
                       all_connectivity), )
  })
  
  ################
  # Resource table
  ################
  
   
  full_resources_table <- reactive({

    language <- languages_long_short[[input$selected_language]]
    
    # Get HTML code for image column
    image_column<-get_image_column(raw_resources$Image_Name)
    
    # Get HTML code for content column
    content_column<- get_content_column(raw_resources, language, i18n)
    
    # Get HTML code for description column
    localised_name<-paste0("Description_",language)
    description_column<-get_description_column(raw_resources[[localised_name]])
    
    full_resources_table <- data.frame(image=image_column, 
                                  content=content_column, 
                                   description=description_column)
    full_resources_table
  })
  
   # Display table
  
  output$resources <- DT::renderDataTable(
    DT::datatable({
      data <- resources_table()
    },
    escape = FALSE,
    rownames = FALSE,
    colnames = c("", "", ""),
    options = list(
      dom = 'ltp',
      bSort = FALSE,
      columnDefs = list(
        list(width = '100px', targets = c(0)),
        list(width = '400px', targets = c(1))
      ),
      scrollX = TRUE,
      lengthMenu = list(c(10, -1), c('10', 'All')),
      pageLength = 10
    ),
    selection = 'single')
  )
  
  # Functions for selecting resources based on filters
  
  filter_resources_from_field <-
    function(processed_items_in_raw_resources,
             all_values_meanings,
             selected_value) {
      indices_resources_to_keep <-
        1:length(processed_items_in_raw_resources)
      
      selected_value_index <- 0
      if (!is.null(selected_value)) {
        selected_value_index <-
          which(all_values_meanings == selected_value) - 1
        if (length(selected_value_index)==0) {
          selected_value_index <-0
        }
      }
      
      if (selected_value_index != 0) {
        values_match <- lapply(processed_items_in_raw_resources,
                               function(x) {
                                 length(intersect(x, selected_value_index) > 0)
                               })
        indices_resources_to_keep <- which(unlist(values_match) > 0)
      }
      
      return(indices_resources_to_keep)
    }
  
  # Filter resources
  # For each field:
  
  filter_resources <- function(full_resources_table) {
    language <- isolate(languages_long_short[[input$selected_language]])
    
    indices_resources_to_keep <- 1:nrow(full_resources_table)
    
    indices_to_keep_languages <-
      filter_resources_from_field(
        raw_resources$processed_items_languages,
        text_for_resource_filters$languages[[language]],
        input$language
      )
    
    indices_to_keep_resource_types <-
      filter_resources_from_field(
        raw_resources$processed_items_resource_types,
        text_for_resource_filters$resource_types[[language]],
        input$resource_type
      )
    
    indices_to_keep_ages <-
      filter_resources_from_field(raw_resources$processed_items_ages,
                                  text_for_resource_filters$ages[[language]],
                                  input$age)
    
    indices_to_keep_categories <-
      filter_resources_from_field(raw_resources$processed_items_categories,
                                  text_for_resource_filters$categories[[language]],
                                  input$category)
    
    indices_to_keep_connectivity <-
      filter_resources_from_field(
        raw_resources$processed_items_connectivity,
        all_connectivity,
        input$connectivity
      )
    
    indices_to_keep_data_types <-
      filter_resources_from_field(raw_resources$processed_items_data_types,
                                  all_data_types,
                                  input$data_type)
    
    indices_to_keep_other_keywords <-
      filter_resources_from_field(
        raw_resources$processed_items_other_keywords,
        all_other_keywords,
        input$other_keywords
      )
    
    indices_resources_to_keep <- Reduce(
      intersect,
      list(
        indices_to_keep_languages,
        indices_to_keep_resource_types,
        indices_to_keep_ages,
        indices_to_keep_categories,
        indices_to_keep_connectivity,
        indices_to_keep_data_types,
        indices_to_keep_other_keywords
      )
    )
    
    resources_table = full_resources_table[indices_resources_to_keep,]
    return(resources_table)
  }
  
  resources_table <- reactive({
    filter_resources(full_resources_table())
  })
}

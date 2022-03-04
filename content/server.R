library(shinythemes)

server <- function(input, output) {

  output$resources <- DT::renderDataTable(
    DT::datatable(
      {data <- resources_table()},
      escape = FALSE,
      rownames = FALSE,
      colnames=c("","",""),
      options = list(dom = 't',bSort=FALSE,
                     columnDefs = list(list(width = '100px', targets = c(0)),
                                       list(width = '400px', targets = c(1))
                     ),
                     scrollX = TRUE
                     ),
      selection = 'single'
    )
  )
  
  filter_resources_from_field<-function(processed_items_in_raw_resources, 
                                        all_values_meanings, 
                                        selected_value) {
    
    indices_resources_to_keep <- 1:length(processed_items_in_raw_resources)
    
    selected_value_index <- which(all_values_meanings==selected_value)-1
    
    if (selected_value_index!=0) {
      values_match<-lapply(processed_items_in_raw_resources, 
                             function(x) {length(intersect(x,selected_value_index)>0)})
      indices_resources_to_keep <- which(unlist(values_match)>0)
    }
    
    return(indices_resources_to_keep)
  }
  
  filter_resources<-function(full_resources_table, language) {
    
    indices_resources_to_keep<-1:nrow(full_resources_table)
    
    indices_to_keep_languages <- filter_resources_from_field(raw_resources$processed_items_languages, 
                                                             all_languages, 
                                                             input$language)
    
    indices_to_keep_resource_types <- filter_resources_from_field(raw_resources$processed_items_resource_types, 
                                                                  all_resource_types, 
                                                                  input$resource_type)
    
    indices_to_keep_ages <- filter_resources_from_field(raw_resources$processed_items_ages, 
                                                        all_ages, 
                                                        input$age)
    
    indices_to_keep_categories <- filter_resources_from_field(raw_resources$processed_items_categories, 
                                                              all_categories, 
                                                              input$category)
    
    indices_to_keep_connectivity <- filter_resources_from_field(raw_resources$processed_items_connectivity, 
                                                                all_connectivity, 
                                                                input$connectivity)
    
    indices_to_keep_data_types <- filter_resources_from_field(raw_resources$processed_items_data_types, 
                                                              all_data_types, 
                                                              input$data_type)
    
    indices_to_keep_other_keywords<- filter_resources_from_field(raw_resources$processed_items_other_keywords, 
                                                              all_other_keywords, 
                                                              input$other_keywords)
    
    indices_resources_to_keep <- Reduce(intersect, 
                                        list(indices_to_keep_languages,
                                           indices_to_keep_resource_types,
                                           indices_to_keep_ages,
                                           indices_to_keep_categories,
                                           indices_to_keep_connectivity,
                                           indices_to_keep_data_types,
                                           indices_to_keep_other_keywords
                                        ))
    
    resources_table = full_resources_table[indices_resources_to_keep,]
    return(resources_table)
  }
  
  resources_table <- reactive({
    filter_resources(full_resources_table, 
                     input$language)
  })
    
    

}


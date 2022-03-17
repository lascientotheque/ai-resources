library(tidyverse)

raw_resources <- read.csv("resources.csv",header=T,sep=";")

text_for_resource_filters <-
  read_json("text_for_resource_filters.json", simplifyVector = TRUE)

process_items_from_column<-function(column_content, category_meaning) {
  
  processed_items_column<-list()
  
  for (i in 1:length(column_content)) {
    items_i <- column_content[i] 
    
    if (length(items_i)>0) {
      items <- strsplit(items_i,'/')[[1]]
      for (j in 1:length(items)) {
        items[j] <- str_trim(items[j])
      }
      processed_items_column[i] <- list(items)
    }
  }
  return(processed_items_column)
}

get_image_column<-function(image_names) {
  
  html_code<-c()
  for (i in 1:length(image_names)) {
    html_code<-c(html_code,
                 paste0("<img src='./img/",
                        image_names[i],
                        "' width=150></img>")
    )
  }
  return(html_code)
}

get_content_column<-function(raw_resources, language, i18n) {
  
  html_code<-c()
  for (i in 1:nrow(raw_resources)) {
    languages_i<-paste0(text_for_resource_filters$languages[[language]]
                        [strtoi(c(raw_resources$processed_items_languages[[i]]))+1],collapse=", ")
    resources_types_i<-paste0(text_for_resource_filters$resource_types[[language]]
                              [strtoi(c(raw_resources$processed_items_resource_types[[i]]))+1],collapse=", ")
    ages_i<-paste0(text_for_resource_filters$ages[[language]]
                   [strtoi(c(raw_resources$processed_items_ages[[i]]))+1],collapse=", ")
    categories_i<-paste0(text_for_resource_filters$categories[[language]]
                         [strtoi(c(raw_resources$processed_items_categories[[i]]))+1],collapse=", ")
    data_types_i<-paste0(text_for_resource_filters$data_types[[language]]
                         [strtoi(c(raw_resources$processed_items_data_types[[i]]))+1],collapse=", ")
    connectivity_i<-paste0(text_for_resource_filters$connectivity[[language]]
                           [strtoi(c(raw_resources$processed_items_connectivity[[i]]))+1],collapse=", ")
    other_keywords_i<-paste0(text_for_resource_filters$other_keywords[[language]]
                             [strtoi(c(raw_resources$processed_items_other_keywords[[i]]))+1],collapse=", ")
    
    html_code_resource<-paste0("<a href='",raw_resources$URL[i],"'><h4>",raw_resources$Name[i],"</h2></a>",
                               paste0("<br>",i18n$t("Language(s)"),": ",languages_i),
                               paste0("<br>",i18n$t("Resource type(s)"),": ",resources_types_i),
                               paste0("<br>",i18n$t("Age(s)"),": ",ages_i),
                               paste0("<br>",i18n$t("Connectivity"),": ",connectivity_i),
                               paste0("<br>",i18n$t("Categorie(s)"),": ",categories_i),
                               paste0("<br>",i18n$t("Data type(s)"),": ",data_types_i),
                               paste0("<br>",i18n$t("Other keyword(s)"),": ",other_keywords_i)
                     )
    html_code<-c(html_code,
                 html_code_resource)
  }
  return(html_code)
}



# Get processed items for columns that need to be processed (language, keywords, resource type, etc...)
# This will return, for each column, the list of ID (as integers)
raw_resources$processed_items_languages<-process_items_from_column(raw_resources$Language,all_languages) 
raw_resources$processed_items_resource_types<-process_items_from_column(raw_resources$Resource_Type,all_resource_types) 
raw_resources$processed_items_ages<-process_items_from_column(raw_resources$Age,all_ages) 
raw_resources$processed_items_categories<-process_items_from_column(raw_resources$Category,all_categories) 
raw_resources$processed_items_data_types<-process_items_from_column(raw_resources$Data_Type,all_data_types) 
raw_resources$processed_items_connectivity<-process_items_from_column(raw_resources$Connectivity,all_connectivity) 
raw_resources$processed_items_other_keywords<-process_items_from_column(raw_resources$Keywords,all_other_keywords) 



library(bslib)
library(dplyr)
library(galah)
library(jsonlite)
library(purrr)
library(shiny)
library(stringr)
library(tibble)

# left column shows regions
region_card <- bslib::card(
  card_header("Regions"),
  card_body(
    tableOutput("region_result")
    # verbatimTextOutput("query_result") ## testing only
  ),
  card_footer(
    downloadButton(outputId = "download_json",
                   label = "JSON"),
    downloadButton(outputId = "download_csv",
                   label = "CSV")
  )
)

# right column shows taxa
taxon_card <- bslib:::card(
  card_header("Taxon"),
  tableOutput("taxa_result"),
  card_footer(
    actionButton(inputId = "to_biocache",
                 label = "View Records"),
    actionButton(inputId = "to_bde",
                 label = "View Taxon")
  )
)

# user interface
ui <- page_sidebar(
  # App title ----
  title = "Taxon region visualiser",
  theme = bslib::bs_theme(bootswatch = "litera"),
  sidebar = sidebar(
    textInput(inputId = "taxon_name",
              label = "Taxon name",
              width = "100%"),
    selectInput(inputId = "search_type",
                label = "Search Type",
                choices = c("Name-matching" = "namematching",
                            "Raw Text" = "rawtext")),
    selectInput(inputId = "region_type",
                label = "Regions",
                choices = c("IBRA" = "ibra", 
                            "States & Territories" = "states_territories")),
    # selectInput(inputId = "profile_type",
    #             label = "Data Profile",
    #             choices = c("None" = )) # no way to specify 'none'
    actionButton(inputId = "search",
                 label = "Search")
  ),
  layout_columns(
    region_card,
    taxon_card
  )
)

server <- function(input, output) {
  
  # set up storage infrastructure to cache user inputs
  data_stored <- reactiveValues(
    search_taxa_result = tibble::tibble(),
    region_query = NA,
    region_result = tibble::tibble())
  
  # add actions when `search` is called
  observeEvent(input$search, {
    
    # set region codes
    region_type <- switch(input$region_type,
                          "ibra" = "cl11185",
                          "states_territories" = "cl22")
    
    # if requested, look for taxon name
    if(nchar(input$taxon_name) > 0){
      if(input$search_type == "namematching"){
        data_stored$search_taxa_result <- galah::search_taxa(input$taxon_name)
        
        if(ncol(data_stored$search_taxa_result) > 2){
          
          data_stored$region_query <- galah_call() |> 
            filter(taxonConceptID == data_stored$search_taxa_result$taxon_concept_id ,
                   basisOfRecord == "PRESERVED_SPECIMEN") |> 
            group_by(region_type) |> 
            count() |> 
            arrange(region_type) 
          
          data_stored$region_result <- collect(data_stored$region_query)
        }else{
          showModal(modalDialog(title = "Taxon not found"))
        }
        
      }else{
        data_stored$search_taxa_result <- tibble::tibble(field = "raw_scientificName",
                                                         value = input$taxon_name)
        data_stored$region_query <- galah_call() |> 
          filter(
            raw_scientificName == input$taxon_name,
            basisOfRecord == "PRESERVED_SPECIMEN") |> 
          group_by(region_type) |> 
          count() |>
          arrange(region_type)
        
        query_result <- collect(data_stored$region_query)
        if(nrow(query_result) < 1){
          showModal(modalDialog(title = "No data found"))
          data_stored$region_result <- tibble::tibble()
        }else{
          data_stored$region_result <- query_result
        }
        
      }
    }else{
      showModal(modalDialog(title = "Please enter a taxon name"))
    }
  })
  
  # when data are updated, change the display
  observe({
    # print region columns
    if(nrow(data_stored$region_result) > 0){
      df <- data_stored$region_result
      colnames(df)[1] <- input$region_type
      output$region_result <- renderTable(df)
    }else{
      output$region_result <- renderTable(tibble::tibble())
    }
    
    # print taxon columns
    if(nrow(data_stored$search_taxa_result) > 0){
      list_taxa <- data_stored$search_taxa_result |>
        as.list()
      tibble_taxa <- tibble::tibble(field = names(list_taxa),
                                    value = unlist(list_taxa)) |>
        mutate(field = stringr::str_replace_all(field, "_", " ") |>
                 stringr::str_to_title())
      output$taxa_result <- renderTable(tibble_taxa)
    }else{
      output$taxa_result <- renderTable(tibble::tibble())

    }
    
    ## for testing only
    # output$query_result <- renderPrint(httr2::url_parse(data_stored$region_query$url))
  })
  
  # enable download buttons
  output$download_csv <- downloadHandler(
    filename = "test.csv",
    content = function(file){
      write.csv(data_stored$region_result, 
                file = file,
                row.names = FALSE)
    })
  
  # enable download buttons
  output$download_json <- downloadHandler(
    filename = "test.txt",
    content = function(file){
      data_stored$region_result |>
        as.list() |>
        purrr::list_transpose() |>
        jsonlite::toJSON(auto_unbox = TRUE) |>
        writeLines(con = file)
    })

}

shinyApp(ui = ui, server = server)
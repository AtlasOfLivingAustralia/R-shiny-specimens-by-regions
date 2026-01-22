library(bslib)
library(dplyr)
library(galah)
library(jsonlite)
library(purrr)
library(shiny)
library(stringr)
library(tibble)

# internal functions
make_occurrence_query <- function(x){
  y <- httr2::url_parse(x)
  y$hostname <- "biocache.ala.org.au"
  y$path <- "occurrences/search"
  fq <- y$query$fq |>
    stringr::str_replace_all("\\\"", "") |>
    strsplit(" AND ") |>
    purrr::pluck(!!!list(1)) |>
    stringr::str_replace_all("\\(|\\)", "") |>
    stringr::str_replace("basisOfRecord", "basis_of_record") |>
    stringr::str_replace("taxonConceptID", "lsid") |>
    stringr::str_replace("raw_scientificName", "raw_scientific_name") |>
    as.list()
  names(fq) <- rep("fq", length(fq))
  y$query <- fq
  httr2::url_build(y)
}

make_bie_query <- function(x){
  y <- httr2::url_parse(x)
  fq <- y$query$fq
  if(stringr::str_detect(fq, "taxonConceptID")){ # go to taxon page
    taxon_id <- fq |>
      strsplit(" AND ") |>
      purrr::pluck(!!!list(1, 1)) |>
      stringr::str_replace("\\(taxonConceptID:", "") |>
      stringr::str_replace_all("\"", "") |>
      stringr::str_replace("\\)", "")
    paste0("https://bie.ala.org.au/species/", taxon_id)
  }else{ # run a search on ALA 
    taxon_name <- fq |>
      strsplit(" AND ") |>
      purrr::pluck(!!!list(1, 1)) |>
      stringr::str_replace("\\(raw_scientificName:", "") |>
      stringr::str_replace_all("\"", "") |>
      stringr::str_replace("\\)", "")
    paste0("https://bie.ala.org.au/search?q=", taxon_name)
  }
}

# left column shows regions
region_card <- bslib::card(
  card_header("Regions"),
  card_body(
    tableOutput("region_result")),
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
    uiOutput("to_biocache"),
    uiOutput("to_bie")
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
    region_result = tibble::tibble(),
    ala_occurrences = NA,
    ala_species = NA)
  
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
            arrange(region_type) |>
            collapse()
          
          data_stored$ala_occurrences <- make_occurrence_query(data_stored$region_query$url)
          data_stored$ala_species <- make_bie_query(data_stored$region_query$url)
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
          arrange(region_type) |>
          collapse()
        
        data_stored$ala_occurrences <- make_occurrence_query(data_stored$region_query$url)
        data_stored$ala_species <- make_bie_query(data_stored$region_query$url)
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
  })
  
  # enable download button (csv)
  output$download_csv <- downloadHandler(
    filename = "test.csv",
    content = function(file){
      write.csv(data_stored$region_result, 
                file = file,
                row.names = FALSE)
    })
  
  # enable download button (json)
  output$download_json <- downloadHandler(
    filename = "test.txt",
    content = function(file){
      data_stored$region_result |>
        as.list() |>
        purrr::list_transpose() |>
        jsonlite::toJSON(auto_unbox = TRUE) |>
        writeLines(con = file)
    })
  
  # enable exit to occurrences (new tab)
  output$to_biocache <- renderUI({
    if(!is.na(data_stored$ala_occurrences)){
      tags$a(href = data_stored$ala_occurrences, 
             class = "btn btn-default", 
             target = "_blank",
             "View Occurrences")
    }
  })

  # enable exit to bie (new tab)
  output$to_bie <- renderUI({
    if(!is.na(data_stored$ala_species)){
      tags$a(href = data_stored$ala_species, 
             class = "btn btn-default", 
             target = "_blank",
             "View Taxon")
    }
  })

}

shinyApp(ui = ui, server = server)
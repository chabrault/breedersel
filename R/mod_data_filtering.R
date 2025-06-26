#' data_filtering UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_filtering_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      
      column(width = 7,
             shiny::uiOutput(ns("VarFilt")),
             
      ),
      # column(width = 5,
      #        shinyWidgets::actionBttn(ns("reset_filters"), "Restart filters",
      #                                 icon = icon("redo"),style = "unite", color = "danger")
      # ),
      column(width = 4,
             shinyWidgets::actionBttn(ns("valid_filters"), "Validate filters",
                                      style = "unite", color = "danger")
      ),
      
      column(width = 10,
             datamods::filter_data_ui(ns("filtering"),
                                      show_nrow = TRUE,
                                      max_height = "600px")
      )
    ),
    br(),
    column(
      width = 10,
      shinyWidgets::progressBar(
        id = ns("pbar"),
        value = 0,
        display_pct = TRUE
      )
    ),
    br(),
    shiny::uiOutput(ns("reference_selector")),
    
    fluidRow(
      br(),
      tags$h3("Filtered data"),
      column(12,
             reactable::reactableOutput(outputId = ns("tableFiltPrint"), height = "700px")
      ),
      
      br(),
      column(width = 4,
             shinyWidgets::actionBttn(ns("valid_filtDat"), "Valid",
                                      style = "pill", color = "danger")
      ),
      column(width = 4,
             shinyWidgets::downloadBttn(ns("downloadtabFilt"),
                                        "Download filtered data",
                                        style = "bordered",
                                        color = "danger")
      )
    )
  )
}

#' data_filtering Server Functions
#'
#' @param id id Module id. See [shiny::moduleServer()].
#' @param data_r6 an R6 object containing data. Must contain a final element, which will be used as an input.
#' @return filtered dataset, in data_r6$final object.
#' @noRd

mod_data_filtering_server <- function(id, data_r6){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    all_data <- reactive(req(data_r6$updated()))
    sel_vars <- reactiveVal()  # start with no variables
    
    output$VarFilt <- renderUI({
      req(all_data())
      shinyWidgets::pickerInput(
        #shiny::selectInput(
        inputId=ns("var_selected"),
        label="Variable(s) to filter",
        choices = colnames(all_data()),
        multiple = TRUE,
        selected = sel_vars(),
        options = shinyWidgets::pickerOptions(
          liveSearch = TRUE,
          actionsBox = TRUE,
          virtualScroll = 200,
          maxOptions = 10
        )
      )
    })
    
    
    
    # observeEvent(input$valid_filters, {
    #   cat("Clicked validate filter\n")
    # })
    
    # observeEvent(input$var_selected, {
    #   cat("input$var_selected changed:\n")
    #   print(input$var_selected)
    # })
    
    # Initialize filter module only once
    observeEvent(input$valid_filters,{
      
      # ## print sel_vars() for debugging
      #observe({
      # print("Selected variables for filtering:\n")
      # print(input$var_selected)
      sel_vars(input$var_selected)
      #print(sel_vars())
      
    })
    
    # req(input$var_selected)
    # req(all_data())
    
    res_filter <<- datamods::filter_data_server(
      id = "filtering",
      drop_ids = FALSE,
      data = all_data,
      name = reactive("all_data"),
      vars = sel_vars,
      #default = character(0),#sel_vars,
      #vars =reactive(if (is.null(sel_vars())) character(0) else sel_vars()), ## show none at start
      #defaults = reactive(if (is.null(sel_vars())) character(0) else sel_vars()),#reactive(character(0)),#reactive(NULL),
      widget_num = "slider",
      widget_date = "slider",
      label_na = "NA"
    )
    
    
    ### Update progress bar reactively
    observe({
      req(res_filter$filtered())
      req(all_data())#, )
      shinyWidgets::updateProgressBar(
        session = session,
        id = "pbar",
        value = ifelse(is.null(res_filter$filtered()),0, 
                       nrow(res_filter$filtered())),
        title=paste0("Number of rows remaining: ", 
                     ifelse(is.null(res_filter$filtered()), 0, nrow(res_filter$filtered())),
                     "/",nrow(all_data())),
        total = nrow(all_data())
      )
    })
    
    
    # Reference selector
    output$reference_selector <- renderUI({
      req(all_data())
      shinyWidgets::pickerInput(
        ns("ref_genotypes"),
        label = "Reference genotype(s) to pin on top:",
        choices = unique(all_data()$genotype),
        #search=TRUE,
        options=shinyWidgets::pickerOptions(liveSearch=TRUE,
                                            maxOptions=10,
                                            actionsBox=TRUE,
                                            virtualScroll = 200),
        selected = NULL,
        multiple = TRUE
      )
    })
    
    
    
    # observe({
    #   print("Selected genotypes:\n")
    #   print(input$ref_genotypes)
    # })
    
    # Display filtered table with references on top
    output$tableFiltPrint <- reactable::renderReactable({
      req(res_filter$filtered())
      df_filtered <- res_filter$filtered()
      df_all <- all_data()
      
      if (!is.null(input$ref_genotypes) && length(input$ref_genotypes) > 0) {
        ref_rows <- df_all[df_all$genotype %in% input$ref_genotypes, ]
        rest <- df_filtered[!df_filtered$genotype %in% input$ref_genotypes, ]
        df <- rbind(ref_rows, rest)
      } else {
        df <- df_filtered
      }
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      reactable::reactable(
        df,
        columns = setNames(
          lapply(numeric_cols, function(col) {
            reactable::colDef(
              format = reactable::colFormat(digits = 2),
              align = "center"
            )
          }),
          numeric_cols
        ),
        defaultColDef = reactable::colDef(align = "center"),
        
        rowStyle = function(index) {
          if (df$genotype[index] %in% input$ref_genotypes) {
            list(background = "#ffeeba")
          } else {
            NULL
          }
        }
      )
    })
    
    # Store filtered result + reference genotypes
    merged_data <- reactive({
      req(res_filter$filtered())
      df_filtered <- res_filter$filtered()
      df_all <- all_data()
      
      if (!is.null(input$ref_genotypes) && length(input$ref_genotypes) > 0) {
        ref_rows <- df_all[df_all$genotype %in% input$ref_genotypes, ]
        rest <- df_filtered[!df_filtered$genotype %in% input$ref_genotypes, ]
        df <- rbind(ref_rows, rest)
      } else {
        df <- df_filtered
      }
      df
    })
    #data_r6$final <- reactive(req(res_filter$filtered()))
    data_r6$final <- reactive(merged_data())
    
    output$downloadtabFilt <- downloadHandler(
      filename = paste0("dataFiltered_", Sys.Date(), ".csv"),
      content = function(fname){
        data.table::fwrite(merged_data(), fname)
      }
    )
    # observe({
    #   cat("final data:\n")
    #   print(str(data_r6$final()))
    # })
    
    observeEvent(input$valid_filtDat, {
      #print(str(data_r6$final()))
      return(data_r6)
    }, ignoreInit = TRUE)
  })
}

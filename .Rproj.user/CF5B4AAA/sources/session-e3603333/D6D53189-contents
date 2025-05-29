#' import_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_import_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Try with different Bootstrap version
    # theme = bslib::bs_theme(version = 4),
    fluidRow(
      bs4Dash::box(
        title = tagList(shiny::icon("upload"), "Source"),
        solidHeader = FALSE,
        status = "success",
        maximizable = F,
        closable = F,
        width = 9,

        h5("Importation of external data. The table must contain a column named 'genotype'."),
        br(),
        tags$a(href='ex_tab/table_PlosOne_Laucou2018.csv',
               "Example file of phenotypic data from Laucou et al. (2018)",
               align = "left"),

        br(),
        shinyWidgets::actionBttn(ns("launch_modal"),
                                 "Launch import", color="warning",
                                 size="lg", style="fill"),

        shiny::uiOutput(ns("select_genotype_ui")),
        shinyWidgets::actionBttn(ns("confirm_genotype"),
                                 "Confirm genotype column",
                                 color = "success",
                                 size = "sm", style = "gradient"),
        
        shinyWidgets::actionBttn(ns("test_file"),
                                 "Test file", color="warning",
                                 size="lg", style="bordered")

      )
    ),

    fluidRow(

      column(
        width = 11,
        tags$h3("Imported data:"),
        verbatimTextOutput(outputId = ns("name")),
        # DT::dataTableOutput(ns("data"),
        #                     width = "100%")
        reactable::reactableOutput(ns("data"))
      )

    )
  )
}

#' import_table Server Functions
#'
#' @noRd
#' @importFrom datamods import_ui import_modal import_server import_file_ui
mod_import_table_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #options("datamods.i18n" = "en")
    observeEvent(input$launch_modal, {
      # req(input$from)
      datamods::import_modal(
        id = ns("import_modal"),
        from = c("file","copypaste"),
        file_extensions = c(".csv", ".txt", ".xls", ".xlsx", ".rds", ".fst", ".sas7bdat",".sav",".tsv"),
        size="xl",
        title = "Data import"
      )
    })
    imported <- datamods::import_server("import_modal",
                                        allowed_status="OK",
                                        return_class = "data.frame",
                                        read_fns = list(
                                          xlsx = function(file, sheet, skip) {
                                            readxl::read_excel(path = file, sheet = sheet, skip = skip, na = c("DM"))
                                          },
                                          xls = function(file, sheet, skip) {
                                            readxl::read_excel(path = file, sheet = sheet, skip = skip, na = c("DM"))
                                          }
                                        )
    )


    #dat <- reactive(req(imported$data()))
    
    # store column renamed to 'genotype'
    dat <- reactiveVal()
    
    # show UI to choose the genotype column
    output$select_genotype_ui <- renderUI({
      req(imported$data())
      shiny::selectInput(ns("genotype_column"), "Choose genotype column:",
                         choices = names(imported$data()), selected = NULL)
    })
    
    observeEvent(input$confirm_genotype, {
      req(imported$data(), input$genotype_column)
      df <- imported$data()
      colnames(df)[colnames(df) == input$genotype_column] <- "genotype"
      dat(df)
    })
    
    return(dat)

    
    observeEvent(input$test_file,{
      cols <- isolate(as.character(colnames(req(imported$data()))))
      coltype <- isolate(sapply(req(imported$data()), class))

      msg <- "Everything is perfect!"
      type <- "success"

      if(length(intersect("genotype",cols)) != 1){
        msg <- "Missing 'genotype' column"
        type <- "error"
      }

      if(length(coltype[coltype %in% c("numeric","integer")]) < 2){
        msg <- "Not enough numeric variables"
        type <- "error"
      }
      shinyalert::shinyalert("Test file input",  text=msg, type=type)
    })



    output$name <- renderPrint({
      req(imported$name())
      imported$name()
    })


    output$data <- reactable::renderReactable(
      reactable::reactable(req(dat()),
                           rownames=FALSE,
                           sortable=TRUE,
                           filterable=TRUE,
                           striped = TRUE,
                           resizable=TRUE,
                           pagination=TRUE,
                           searchable=TRUE,
                           highlight=TRUE,
                           bordered=TRUE,
                           defaultColDef=reactable::colDef(align = "center",  filterable = TRUE,
                                                           format=reactable::colFormat(digits=2,
                                                                                       locales="en-US")),
                           height=700
      )
    )

    #return(reactive(imported$data()))
    return(reactive(dat()))
  })

  #})
}

## To be copied in the UI
# mod_import_table_ui("import_table_1")

## To be copied in the server
# mod_import_table_server("import_table_1")

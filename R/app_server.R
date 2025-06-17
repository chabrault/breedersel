#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  shiny::shinyOptions(bootstrapTheme = bslib::bs_theme(version = 4L))
  
  ## initialize translation
  # i18n <- datamods::i18n$new(
  #   translation_csv = "www/translations.csv",
  #   language = "en"
  # )
  # 
  # observeEvent(input$lang, {
  #   i18n$set_language(input$lang)
  # })
  
  # create R6 object to store data
  data_r6 <- R6::R6Class(
    "dataInR6",
    public = list(
      raw=NULL,
      updated=NULL,
      final=NULL
    )
  )

  ## ---------- Import data ------------------------
  ### MODULE 1 mod_import_table ###
  ##options("datamods.i18n" = "en")
  data_r6$raw <- mod_import_table_server("import_table_1")
  #print(isolate(class(data_r6$raw)))
  data_r6$final <- reactive(data_r6$raw())

  ### Table of data
  output$output_data <- reactable::renderReactable(
    reactable::reactable(data_r6$final(),
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
                                                         format=reactable::colFormat(digits=2)),
                         height=700

    )
  )

  ## update data by selecting column type, names and which one to keep
  ##options("datamods.i18n" = "en")
  # observeEvent(input$valid_upVars,{
  #   print(paste0("nrow raw data: ",nrow(isolate(data_r6$raw()))))
  #   print(paste0("ncol raw data: ",ncol(isolate(data_r6$raw()))))
  #
  #   tmp <- datamods::update_variables_server(id="update_data",
  #                                            data=data_r6$raw(),
  #                                            height="600px")
  #   data_r6$updated <- reactive(tmp())
  # },ignoreInit = TRUE)
  #data_r6$final <- reactive(data_r6$updated())

  ## apply filtering on rows for some columns
  observeEvent(input$valid_filtVars,{#req(data_r6$raw()),{#input$valid_filtVars,{
    data_r6$updated <- reactive(data_r6$raw())
    #print(paste0("ncol updated data: ",ncol(isolate(data_r6$updated()))))
    mod_data_filtering_server("filt_data",data_r6=data_r6) 
    #print(paste0("ncol filtered data: ",ncol(isolate(data_r6$final()))))
  }, ignoreInit=TRUE
  )


  ### Plot MGIDI outputs (multivariate selection index)
  mod_MGIDI_server("mgidi_1",data_r6=data_r6)

  ### Esquisse plot
  ##datamods::set_i18n("en",packages=c("datamods","esquisse"))
  observeEvent(input$launch_esquisse,{
    esquisse::esquisse_server(id="esquisse",
                              data_rv = reactiveValues(data=data_r6$final(),
                                                       name="data_r6"),
                              default_aes = c("fill", "color", "size", "group", "facet","label"))
  },ignoreInit = TRUE)



  ## About / Rsession
  output$pkgVersion <- renderText(
    as.character(utils::packageVersion("breedersel")))

  output$Rsession <- renderPrint(
    print(utils::sessionInfo())
  )


}

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

      column(width=7,
             uiOutput(ns("VarFilt"))
      ),

      # column(width=5,
      #        mod_SelectVar_ui(ns("sv_filt"),
      #                         label="Variables à filtrer",
      #                         multiple=TRUE)),


      column(width =10,
             datamods::filter_data_ui(ns("filtering"),
                                      show_nrow = TRUE,
                                      max_height = "600px")
      )
    ),

    column(
      width = 10,
      shinyWidgets::progressBar(
        id = ns("pbar"),
        value = 0,
        #total = 100,
        display_pct = TRUE
      )
    ),
    br(),

    fluidRow(
      br(),
      tags$h3("Filetered data"),
      column(12,
             reactable::reactableOutput(outputId = ns("tableFiltPrint"),height="700px"),
      ),

      br(),
      #verbatimTextOutput(outputId = ns("res_str"))
      column(width=4,
             shinyWidgets::actionBttn(ns("valid_filtDat"),"Valid",
                                      style = "pill",color = "danger")
      ),
      column(width=4,
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
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    #selVarsReac <- shiny::reactiveVal()

    all_data <- reactive(req(data_r6$updated()))
    # data_r6$updated() <- reactive({
    #   req(data_r6$updated())
    #   tmp <- data_r6$updated()
    #   tmp$genotype <- as.factor(tmp$genotype)
    #   return(tmp)
    # })


    output$VarFilt <- renderUI({
      selectInput(
        ns("var_selected"),
        "Variable(s) to filter",
        choices = colnames(req(data_r6$updated())),
        multiple=TRUE,
        selected =""# "genotype"#""#"col"
      )
    })



    shiny::observeEvent(input$var_selected,{
      req(input$var_selected)
      vars <- reactive({req(input$var_selected)})
      #cat("filtering... \n")
      res_filter <- datamods::filter_data_server(
        id = "filtering",
        drop_ids=FALSE,
        data = reactive(all_data()),
        name = reactive("all_data"),#reactive("updated_data"),
        vars = vars,#req(input$var_selected),#selVarsReac(),
        defaults = reactive(NULL),
        widget_num = "slider",
        widget_date = "slider",
        label_na = "NA"
      )

      observeEvent(res_filter$filtered(), {
        shinyWidgets::updateProgressBar(
          session = session, id = "pbar",
          value = nrow(res_filter$filtered()),
          total = nrow(all_data())
        )

        output$tableFiltPrint <- reactable::renderReactable({
          reactable::reactable(res_filter$filtered())

        })
        data_r6$final <- reactive(req(res_filter$filtered()))

        # output$res_str <- renderPrint({
        #   str(as.data.frame.packageIQR(res_filter$final()))
        # })


        ##observe(print(str(data_r6$final())))


        output$downloadtabFilt <- downloadHandler(
          filename = paste0("dataFiltered_",Sys.Date(),".csv"),
          content = function(fname){
            data.table::fwrite(data_r6$final(), fname)
          }
        )

        observeEvent(c(input$valid_filtDat),{

          #print(paste0("nrow data_r6:",nrow(isolate(data_r6$final()))))
          return(data_r6)
        },ignoreInit = TRUE)
      })# observeEvent  res_filter$filtered()   #,ignoreInit=TRUE)
    }, ignoreInit=TRUE) ## observeEvent selVarsReac

  }) # end module


}

## To be copied in the UI
# mod_data_filtering_ui("data_filtering_1")

## To be copied in the server
# mod_data_filtering_server("data_filtering_1")

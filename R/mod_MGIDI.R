#' MGIDI UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom stats density median na.omit quantile
#' @importFrom utils write.table
#'

css <- "
.handsontable.listbox td {
  background: black;
}
.handsontable.listbox tr:hover td {
  background: gray;
}
.handsontable.listbox td.htDimmed {
  color: white;
}
.handsontable.listbox tr td.current {
  background: black;
}
"

mod_MGIDI_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      # theme = shinytheme("darkly"),

      tags$head(tags$style(HTML(css))),
      bs4Dash::box(
        width=12,
        height="350px",
        title="Description and help",
        closable = TRUE,
        collapsed=TRUE,
        maximizable = TRUE,
        status="success",
        solidHeader = TRUE,
        p("MGIDI [1] is an algorithm useful to implement a selection index on several traits to choose the best genotype.",
          "To use it, you must fill the editable table."),
        p(strong("trait"),": Choice of traits to include."),
        p(strong("direction"),": Direction of selection towards the ideotype. One of 'min', 'max', 'opti'."),
        p(strong("opti_val"),": Optimal value (if 'opti' chosen for the previous column)."),
        p(strong("weight"),": Relative weight for each trait, between 0 and 1 (optional)."),
        p("Fill the number of lines requested. Add new lines with ", code("Right click + Add rows")),
        p("Adjust the selection intensity to keep more or less genotypes."),
        footer=p("[1] Olivoto, T., & Nardino, M. (2021). MGIDI: Toward an effective multivariate selection in biological experiments.
                 Bioinformatics, 37(10), 1383–1389.", tags$a(href="https://doi.org/10.1093/bioinformatics/btaa981","https://doi.org/10.1093/bioinformatics/btaa981"))


      ),

      bs4Dash::box(
        width=12,
        #height="550px",
        title="MGIDI analysis",
        closable = FALSE,
        maximizable = TRUE,
        status="maroon",
        solidHeader = TRUE,


        ## Right sidebar: select variables and plot options
        sidebar=bs4Dash::bs4CardSidebar(
          id=ns("sidePlot"),
          #style="overflow-y: hidden; overflow-x: auto;",
          startOpen = TRUE,
          width=33,
          easyClose = FALSE,

          # fluidPage(
          tags$h5("Selection index table"),
          ## editable table
          rhandsontable::rHandsontableOutput(ns("tabVar")),
          br(),
          shinyWidgets::downloadBttn(ns("DWNLD_SI"),"Download table",
                                     style = "bordered",color = "default"),
          br(),
          br(),
          h5("Replace missing values by population average?"),
          shinyWidgets::materialSwitch(ns("avgNA"),status="info",value=TRUE),
          sliderInput(ns("sliderSI"),
                      width="90%",
                      label = "Selection intensity",
                      min=0, max=50, value=5,step = 1,post  = " %",),
          br(),
          shinyWidgets::actionBttn(ns("actionmgidi"),"Calculate",
                                   style = "jelly",color = "default")

          #  )
        ),
        ## plot output part
        column(width=8,
               shinyWidgets::radioGroupButtons(ns("type"),
                                               "Type of plot",
                                               choices=c("radar"=TRUE,"barplot"=FALSE),
                                               status="warning",
                                               checkIcon = list(
                                                 yes = icon("ok",
                                                            lib = "glyphicon")
                                               )
               ),
               # shinycssloaders::withSpinner(
               plotOutput(ns("StrenWeak")),
               br(),
               shinyWidgets::downloadBttn(ns("downloadPlot_SW"),"Download plot",
                                          style = "bordered",color = "primary"),
               br(),
               p("Strength and weakness of the best genotypes for different factors."),
               p("The less the contribution to the factor, the best is the performance of the genotype for the traits related to this factor (see below).")
        )
      ),

      bs4Dash::box(
        width=8,
        #height="500px",
        title="Categorization of traits and selection differential",
        closable = FALSE,
        collapsed=FALSE,
        maximizable = TRUE,
        status="lightblue",
        solidHeader = TRUE,
        reactable::reactableOutput(ns("sel_diff")),
        br(),
        shinyWidgets::downloadBttn(ns("DWNLD_SelDiff"),"Download table",
                                   style = "bordered",color = "primary"),
        br(),
        br(),
        h4("Definition of columns:"),
        p(strong("Xo")," and ", strong("Xs"), "indicate the whole population average and the selected population average."),
        p(strong("direction "), "indicates the wanted direction (min / max)."),
        p(strong("goal "), "indicates if the aim has been reached.")
      ),
      bs4Dash::box(
        width=4,
        height="500px",
        title="Ranking of genotypes",
        closable = FALSE,
        collapsed=FALSE,
        maximizable = TRUE,
        status="lightblue",
        solidHeader = TRUE,

        reactable::reactableOutput(ns("tab_mgidi")),
        br(),
        shinyWidgets::downloadBttn(ns("DWNLD"),"Download table",
                                   style = "bordered",color = "primary")

      ),
      bs4Dash::box(
        width=9,
        #height="500px",
        title="Correlation plot between variables",
        closable = FALSE,
        collapsed=TRUE,
        maximizable = TRUE,
        status="indigo",
        solidHeader = TRUE,
        plotOutput(ns("corrplot")),
        shinyWidgets::downloadBttn(ns("downloadPlotCorr"),"Download plot",
                                   style = "bordered",color = "primary")

      ),

      bs4Dash::box(
        width=12,
        height="500px",
        title="Distribution of population and selected individuals",
        closable = FALSE,
        maximizable = TRUE,
        solidHeader = TRUE,
        sidebar=bs4Dash::bs4CardSidebar(
          id=ns("sidePlot"),
          background = "#333a40",
          startOpen = TRUE,
          width=25,

          easyClose = FALSE,
          uiOutput(ns("vars2Plot")),
          uiOutput(ns("extraGeno"))


        ),
        status="navy",
        column(9,
               plotOutput(ns("DistribInd")),
               shinyWidgets::downloadBttn(ns("downloadPlotDist"),"Download plot",
                                          style = "bordered",color = "primary")
        )

      )
    )
  )
}

#' MGIDI Server Functions
#' @import ggplot2
#'
#' @noRd
mod_MGIDI_server <- function(id,data_r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    RVplots <- reactiveValues()

    ## Create rhandsontable table to fill with selection index
    DFrhand <- as.data.frame(matrix(NA,nrow=6,ncol=4))
    colnames(DFrhand) <- c("trait","direction","opti_val","weight")
    output$tabVar <- rhandsontable::renderRHandsontable({
      rhot <-
        rhandsontable::rhandsontable(data=DFrhand, width=500) %>%
        rhandsontable::hot_rows(rowHeights = 32) %>%
        rhandsontable::hot_table(overflow="auto",highlightCol=TRUE,
                                 highlightRow=TRUE,rowHeaderWidth=0) %>%
        rhandsontable::hot_col(col="trait",type="dropdown",source=c("",colnames(data_r6$final())[sapply(data_r6$final(), is.numeric)]),
                               selectCallback = TRUE) %>%
        rhandsontable::hot_col(col="direction",type="dropdown",source=c("min","max","opti"), strict=TRUE) %>%
        rhandsontable::hot_col(col="opti_val",type="numeric") %>%
        rhandsontable::hot_col(col="weight",type="numeric") %>%
        rhandsontable::hot_validate_numeric(col="weight",min=0, max=1) %>%
        rhandsontable::hot_cols(halign="htCenter",valign="htMiddle",
                                #   manualColumnResize=TRUE,
                                colWidths = c(150,70,75,75),
                                renderer = "function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
              td.style.color = 'black';
           }")
      return(rhot)
    })
    ## Local download for user
    output$DWNLD_SI <- downloadHandler(
      filename = paste0("table_index_",format(Sys.time(),"%Y-%m-%d_%H%M"),".tsv"),
      content = function(fname){
        write.table(req(rhandsontable::hot_to_r(input$tabVar)),sep="\t",
                  fname, row.names=FALSE, fileEncoding="UTF-8")
      }
    )


    ## Calculate MGIDI scores
    observeEvent(c(input$actionmgidi, input$type),{
      res_mgidi <- reactive({
        req(input$actionmgidi)
        dt <- data_r6$final()
        ## Check if the data has the required columns
        if (!"genotype" %in% colnames(dt)) {
          shinyalert::shinyalert("Error", "Missing 'genotype' column in the data.", type = "error")
          return(NULL)
        }
        rhot <- req(rhandsontable::hot_to_r(input$tabVar))
        if (!all(na.omit(rhot$trait) %in% colnames(dt))) {
          shinyalert::shinyalert("Error", "Some selected traits are not found in the data.", type = "error")
          return(NULL)
        }
        
        ## calc_mgidi is a function called from fct_helpers.R
        res <- calc_mgidi(data=dt,
                          rhot_table=rhot,
                          SI=req(input$sliderSI),
                          avg_NA=req(input$avgNA))
        #print(res_mgidi()$sel_gen)
        res
      })

      ## Plot strength / weakness
      RVplots$SW <- isolate({

        metan:::plot.mgidi(req(res_mgidi()$res_mgidi),
                           type="contribution", radar=req(input$type)) +
          ggplot2::theme(text=ggplot2::element_text(size=17),
                         axis.text.x=element_text(angle=ifelse(req(input$type),0,90)))

      }) # end isolate

      output$StrenWeak <- renderPlot({
        req(RVplots$SW)
      })

      ## save strength / weakness plot
      output$downloadPlot_SW <- downloadHandler(
        filename = function(){paste0("StrengthWeakPlot_",format(Sys.time(),"%Y-%m-%d_%H%M"),".png")},
        content = function(file){
          ggplot2::ggsave(file,plot=req(RVplots$SW),width=11, height=6, scale=1.2)
        }
      )

      ## Table of selection differential
      output$sel_diff <- reactable::renderReactable(
        reactable::reactable(req(res_mgidi()$res_mgidi$sel_dif),
                             rownames=FALSE,
                             sortable=FALSE,
                             filterable=FALSE,
                             resizable=TRUE,
                             pagination=FALSE,
                             highlight=TRUE,
                             striped = TRUE,
                             defaultColDef=reactable::colDef(align="center",
                                                             format=reactable::colFormat(digits=2,
                                                                                         locales="en-US")),
                             #height=300,
                             #width=500,
                             fullWidth = TRUE,
                             compact=TRUE

        )
      )

      ## Local download for user
      output$DWNLD_SelDiff <- downloadHandler(
        filename = paste0("table_selectionDiff_",format(Sys.time(),"%Y-%m-%d_%H%M"),".tsv"),
        content = function(fname){
          write.table(req(res_mgidi()$res_mgidi$sel_dif),sep="\t",
                    fname, row.names=FALSE, fileEncoding="UTF-8")
        }
      )


      ## Table of MGIDI scores
      output$tab_mgidi <- reactable::renderReactable(
        reactable::reactable(res_mgidi()$res_mgidi$MGIDI,
                             rownames=FALSE,
                             sortable=TRUE,
                             filterable=TRUE,
                             resizable=TRUE,
                             pagination=FALSE,
                             striped = TRUE,
                             highlight=TRUE,
                             defaultColDef=reactable::colDef(align="center",
                                                             filterable = TRUE,
                                                             format=reactable::colFormat(digits=2,
                                                                                         locales="en-US")),
                             height=400,fullWidth = TRUE,compact=TRUE
        )
      )
      ## Save MGIDI score table
      output$DWNLD <- downloadHandler(
        filename = paste0("MGIDI_scores_",format(Sys.time(),"%Y-%m-%d_%H%M"),".tsv"),
        content = function(fname){
          write.table(req(res_mgidi()$res_mgidi$MGIDI),sep="\t",
                    fname, row.names=FALSE, fileEncoding="UTF-8")
        }
      )

      ## user selection of variable to plot
      output$vars2Plot <- renderUI({
        selectInput(
          ns("variablesPlot"),
          "Variable to plot",
          choices = colnames(res_mgidi()$data_mean)[-1],
          multiple=FALSE,
          selected = ""#"col"
        )
      })
      ## user selection of extra genotypes
      output$extraGeno <- renderUI({
        shinyWidgets::pickerInput(
          ns("genotypesPlot"),
          "Supplementary genotype(s)",
          choices = c(unique(res_mgidi()$data_mean$Genotype)),
          multiple=TRUE,
          options=shinyWidgets::pickerOptions(liveSearch=T,
                                              maxOptions=20,
                                              actionsBox=TRUE,
                                              virtualScroll = 200),
          selected = NULL
        )
      })

      ### Plot distribution of phenotype with selected indiv
      observeEvent(c(input$variablesPlot, input$sliderSI,input$genotypesPlot),{

        req(input$variablesPlot)
        ## prepare plot
        ### gather data for distribution and selected genotypes
        dat <- req(res_mgidi()$data_mean)
        dat.sel <- dat[res_mgidi()$res_mgidi$sel_gen,]
        dat.sel <- merge(dat.sel, res_mgidi()$res_mgidi$MGIDI)

        ## other selected genotypes
        if(length(input$genotypesPlot) > 0){
          dat.supp <- dat[dat$Genotype %in% input$genotypesPlot,]
          dat.sel <- plyr::rbind.fill(dat.sel, dat.supp)
        }
        ### summary statistics
        x <- dat[[input$variablesPlot]]
        q15.9 <- quantile(x, .159,na.rm=TRUE) # 1 Std 68.2%
        q84.1 <- quantile(x, .841,na.rm=TRUE)
        q2.3  <- quantile(x, .023,na.rm=TRUE) # 2 Std 95.4%
        q97.7 <- quantile(x, .977,na.rm=TRUE)
        q0.01 <- quantile(x, .001,na.rm=TRUE) # 3 Std 99.8%
        q99.9 <- quantile(x, .999,na.rm=TRUE)
        meanx <- mean(x,na.rm=TRUE)
        medx  <- median(x,na.rm=TRUE)
        x.dens  <- density(x,na.rm=TRUE)
        df.dens <- data.frame(x=x.dens$x, y=x.dens$y)

        RVplots$Dist <-  isolate({

          ggplot(dat, aes(x=.data[[req(input$variablesPlot)]]))+
            geom_density(color = '#619CFF') +

            geom_area(data = subset(df.dens, x >= q15.9 & x <= q84.1), # 1 Std 68.2%
                      aes(x=x,y=y), fill='#619CFF', alpha=0.8) +
            geom_area(data = subset(df.dens, x >= q2.3 & x <= q97.7), # 2 Std 95.4%
                      aes(x=x,y=y), fill='#619CFF', alpha=0.6) +
            geom_area(data = subset(df.dens, x >= q0.01 & x <= q99.9), # 3 Std 99.8%
                      aes(x=x,y=y), fill='#619CFF', alpha=0.3) +
            geom_vline(xintercept=meanx, color="grey60", linewidth=1.5, linetype="dashed") +
            geom_vline(xintercept=medx, color='#FFFFFF',linewidth=1.5, linetype="dashed") +
            ggtitle(req(input$variablesPlot)) +
            geom_rug(alpha=0.8) +
            ggrepel::geom_label_repel(data=dat.sel,
                                      mapping = aes(x=.data[[input$variablesPlot]],y=0,
                                                    label=Genotype,
                                                    fill=MGIDI),
                                      direction="y",point.padding = 0.01,
                                      force_pull = 0.1,size=6,
                                      vjust=0.6, max.overlaps = Inf,
                                      color="black",fontface="bold",alpha=0.7) +
            scale_fill_viridis_c(begin=0.35, direction=1,option="H")+
            theme_bw() +
            theme(text=element_text(size=16),
                  axis.text.x = element_text(size=rel(1.5)))

        }) # end isolate

        output$DistribInd <- renderPlot({req(RVplots$Dist)})

        output$downloadPlotDist <- downloadHandler(
          filename = function(){
            paste(req(input$variablesPlot),"_DistribIndexPlot_",
                  format(Sys.time(),"%Y-%m-%d_%H%M"),'.png',sep='')},
          content = function(file){
            ggsave(file,plot=req(RVplots$Dist), width=11, height=6, scale=1.2)
          }
        )


        output$corrplot <- renderPlot({
          dat <- req(res_mgidi()$data_mean)%>%
            dplyr::select(-Genotype)
          #print(str(dat))
          plot(x=metan::corr_coef(data=dat, use="pairwise.complete.obs"),
               size.text.cor = 4, size.text.lab=12)
        })

        output$downloadPlotCorr <- downloadHandler(
          filename = function(){
            paste("CorrPlot_",
                  format(Sys.time(),"%Y-%m-%d_%H%M"),'.png',sep='')},
          content = function(file){
            ggsave(file,plot=ggplot2::last_plot(), width=11, height=6, scale=1.2)
          }
        )
      }, ignoreInit=TRUE) #observeEvent input$variablesPlot

    }, ignoreInit=TRUE) # observeEvent calculate MGIDI

  })
}

## To be copied in the UI
# mod_MGIDI_ui("MGIDI_1")

## To be copied in the server
# mod_MGIDI_server("MGIDI_1")

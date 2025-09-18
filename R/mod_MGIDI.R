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
        p(strong("weight"),": Relative weight for each trait (optional)."),
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
        ## Helper text
        # bs4Dash::tooltip(
        #   icon("question-circle"),
        #   title = "Help",
        #   placement = "right",
        #   #trigger = "click",
        #   content = div(
        #     p("This box lets you create a multi-trait selection index (MGIDI)."),
        #     p("Fill the table with traits, directions, weights, then click calculate.")
        #   )
        # ),
        # 
        ## Right sidebar: select variables and plot options
        sidebar=bs4Dash::bs4CardSidebar(
          id=ns("sidePlot"),
          #style="overflow-y: hidden; overflow-x: auto;",
          startOpen = TRUE,
          width=35,
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
          # shinyWidgets::actionBttn(ns("actionmgidi"),"Calculate",
          #                          style = "jelly",color = "default")
          
          ## replace the action button by a switch
          # shinyWidgets::radioGroupButtons(
          #   ns("mode"),
          #   choices = c("Edit", "Analyze"),
          #   selected = "Edit",
          #   status = "primary"
          # )
          
          # inside sidebar (replace existing action button)
          tags$head(tags$style(HTML(
            paste0(
              "#", ns("mode_wrap"), " .btn { color: #888 !important; }",
              "#", ns("mode_wrap"), " .btn.active { color: #222 !important; font-weight: 700 !important; }"
            )
          ))),
          
          div(id = ns("mode_wrap"),
              shinyWidgets::radioGroupButtons(
                inputId = ns("mode"),
                label = NULL,
                choices = c("Edit", "Analyze"),
                selected = "Edit",
                status = "primary",
                justified = TRUE,
                checkIcon = list(yes = icon("ok", lib = "glyphicon"))
              )
          )
          
          
          
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
        p(strong("Xo")," indicates the population average and ",
          strong("Xs"), " the averged of the selected population."),
        p(strong("factor "), "indicates the factor axis the most associated with this trait."),
        p(strong("sense "), "indicates the direction sought (min / max)."),
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
        status="navy",
        sidebar=bs4Dash::bs4CardSidebar(
          id=ns("sidePlot"),
          background = "#333a40",
          startOpen = TRUE,
          width=25,
          easyClose = FALSE,
          uiOutput(ns("vars2Plot")),
          uiOutput(ns("extraGeno"))
        ),
        column(9,
               plotOutput(ns("DistribInd")),
               shinyWidgets::downloadBttn(ns("downloadPlotDist"),"Download plot",
                                          style = "bordered",color = "primary")
        )
        
      ),
      ## adding table of phenotypes with ranked MGIDI
      bs4Dash::box(
        width = 12,
        title = "Table of data with ranked MGIDI",
        status = "olive",
        solidHeader = TRUE,
        shiny::uiOutput(ns("reference_selector2")),
        DT::dataTableOutput(ns("ranked_traits_table"))
      )
    )
  )
}

#' MGIDI Server Functions
#' @import ggplot2 
#'
#' @noRd
mod_MGIDI_server <- function(id, data_r6) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ## Initial rhandsontable template
    DFrhand <- as.data.frame(matrix(NA, nrow = 6, ncol = 4))
    colnames(DFrhand) <- c("trait", "direction", "opti_val", "weight")
    
    ## Store current table content
    rhand_data <- reactiveVal(DFrhand)
    
    ## Mode switch
    output$mode_switch <- renderUI({
      shinyWidgets::radioGroupButtons(
        inputId = ns("mode"),
        choices = c("Edit", "Analyze"),
        selected = "Edit",
        status = "primary"
      )
    })
    
    ## Always render the editable table in Edit mode
    output$tabVar <- rhandsontable::renderRHandsontable({
      req(data_r6$final())
      # if user already edited, keep that state
      df <- rhand_data()
      numeric_cols <- colnames(data_r6$final())[sapply(data_r6$final(), is.numeric)]
      hot <- rhandsontable::rhandsontable(data = df, width = 550,
                                          readOnly = (input$mode == "Analyze")) %>%  # not editable if Analyze
        rhandsontable::hot_rows(rowHeights = 32) %>%
        rhandsontable::hot_table(overflow = "auto", highlightCol = TRUE,
                                 highlightRow = TRUE, rowHeaderWidth = 0) %>%
        rhandsontable::hot_col(col = "trait", type="dropdown", source = c("",numeric_cols),
                               selectCallback = TRUE) %>%
        rhandsontable::hot_col(col = "direction", type = "dropdown",
                               source = c("min", "max", "opti", NA), strict = TRUE) %>%
        rhandsontable::hot_col(col = "opti_val", type = "numeric") %>%
        rhandsontable::hot_col(col = "weight", type = "numeric",format="0.0[0]", step=0.1) %>%
        rhandsontable::hot_validate_numeric(col = "weight", min=0) %>%
        rhandsontable::hot_cols(halign = "htCenter", valign = "htMiddle",
                                colWidths = c(145, 70, 70, 70),
                                manualColumnResize = TRUE,        # enable user resizing
                                renderer = "function (instance, td, row, col, prop, value, cellProperties) {
                              Handsontable.renderers.NumericRenderer.apply(this, arguments);
                              td.style.color = 'black';
                            }")
      # Wrap table in a scrollable div
      htmltools::div(style = "overflow-x: auto; width: 100%;", hot)
      hot
    })
    
    
    
    ## Update stored table whenever edited
    observeEvent(input$tabVar, {
      # only update stored table from the widget if it has content
      # (hot_to_r returns NULL if widget not present)
      df <- tryCatch(rhandsontable::hot_to_r(input$tabVar), error = function(e) NULL)
      if (!is.null(df)) rhand_data(df)
      
      
      ## Local download for user
      output$DWNLD_SI <- downloadHandler(
        filename = paste0("table_index_",format(Sys.time(),"%Y-%m-%d_%H%M"),".tsv"),
        content = function(fname){
          write.table(req(df),sep="\t",fname, row.names=FALSE, fileEncoding="UTF-8")
        }
      )
    }, ignoreInit = TRUE)
    
    
    # store MGIDI result ONLY when Analyze succeeds
    res_mgidi_val <- reactiveVal(NULL)
    
    ## Analysis branch (only when Analyze is active)
    observeEvent(input$mode, {
      
      # If user switched to Edit: clear stored MGIDI so other observers won't run
      if (input$mode == "Edit") {
        res_mgidi_val(NULL)
        return()
      }
      
      if (input$mode == "Analyze") {
        df <- rhand_data()
        # --- Verification checks ---
        if (all(is.na(df$trait))) {
          shinyalert::shinyalert("Error", "The selection index table is empty.", type = "error")
          return()
        }
        if (any(duplicated(na.omit(df$trait)))) {
          shinyalert::shinyalert("Error", "Some traits are duplicated.", type = "error")
          return()
        }
        if (sum(!is.na(df$trait)) < 2) {
          shinyalert::shinyalert("Error", "At least two traits must be selected.", type = "error")
          return()
        }
        
        # --- Run MGIDI ---
        res <- tryCatch({
          calc_mgidi(
            data = data_r6$final(),
            rhot_table = df,
            SI = req(input$sliderSI),
            avg_NA = req(input$avgNA)
          )
        }, error = function(e) {
          shinyalert::shinyalert("Error", paste0("MGIDI failed: ", e$message), type = "error")
          NULL
        })
        
        if (is.null(res)) {
          # calculation failed: revert to Edit mode so user can fix things
          shinyWidgets::updateRadioGroupButtons(session, "mode", selected = "Edit")
          return()
        }
        
        # success: store result so other observers can use it
        res_mgidi_val(res)
        
        
        
        ### ----- Plots and tables ------
        
        # Store plot in a reactive
        sw_plot <- reactive({
          req(res_mgidi_val())
          metan:::plot.mgidi(res_mgidi_val()$res_mgidi, type = "contribution", radar = req(input$type)) +
            ggplot2::theme(text=ggplot2::element_text(size=17),
                           axis.text.x=element_text(angle=ifelse(req(input$type),0,90)))
        })
        
        output$StrenWeak <- renderPlot({sw_plot()})
        ## save strength / weakness plot
        output$downloadPlot_SW <- downloadHandler(
          filename = function(){paste0("StrengthWeakPlot_",format(Sys.time(),"%Y-%m-%d_%H%M"),".png")},
          content = function(file){
            ggplot2::ggsave(file,plot=sw_plot(),width=11, height=6, scale=1.2)
          }
        )
        
        output$sel_diff <- reactable::renderReactable({
          req(res_mgidi_val())
          reactable::reactable(res_mgidi_val()$res_mgidi$sel_dif, 
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
                               compact=TRUE)
        })
        
        # Local download for user
        output$DWNLD_SelDiff <- downloadHandler(
          filename = paste0("table_selectionDiff_",format(Sys.time(),"%Y-%m-%d_%H%M"),".tsv"),
          content = function(fname){
            write.table(req(res_mgidi_val()$res_mgidi$sel_dif),sep="\t",
                        fname, row.names=FALSE, fileEncoding="UTF-8")
          }
        )
        ## Table of MGIDI scores
        output$tab_mgidi <- reactable::renderReactable({
          req(res_mgidi_val())
          reactable::reactable(res_mgidi_val()$res_mgidi$MGIDI, 
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
                               height=400,fullWidth = TRUE,compact=TRUE)
        })
        ## Save MGIDI score table
        output$DWNLD <- downloadHandler(
          filename = paste0("MGIDI_scores_",format(Sys.time(),"%Y-%m-%d_%H%M"),".tsv"),
          content = function(fname){
            write.table(req(res_mgidi_val()$res_mgidi$MGIDI),sep="\t",
                        fname, row.names=FALSE, fileEncoding="UTF-8")
          }
        )
        
        ## user selection of variable to plot
        output$vars2Plot <- renderUI({
          selectInput(
            ns("variablesPlot"),
            "Variable to plot",
            choices = colnames(res_mgidi_val()$data_mean)[-1],
            multiple=FALSE,
            selected = ""#"col"
          )
        })
        ## user selection of extra genotypes
        output$extraGeno <- renderUI({
          shinyWidgets::pickerInput(
            ns("genotypesPlot"),
            "Supplementary genotype(s)",
            choices = c(unique(res_mgidi_val()$data_mean$genotype)),
            multiple=TRUE,
            options=shinyWidgets::pickerOptions(liveSearch=T,
                                                maxOptions=20,
                                                actionsBox=TRUE,
                                                virtualScroll = 200),
            selected = NULL
          )
        })
        
        
        ## Correlation plot
        output$corrplot <- renderPlot({
          req(res_mgidi_val())
          dat <- req(res_mgidi_val()$data_mean)%>%dplyr::select(-genotype)
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
        
        # # Example of distribution plot
        # output$DistribInd <- renderPlot({
        #   ggplot(res$data_mean, aes(x = .data[[req(input$variablesPlot)]])) +
        #     geom_density()
        # })
        
        ### TODO: add the distribution plot code in fct_helpers.R
        RVplots <- reactiveValues()
        ### Plot distribution of phenotype with selected indiv
        observeEvent(c(input$variablesPlot, input$genotypesPlot),{
          req(res_mgidi_val())
          req(input$variablesPlot)
          ## prepare plot
          ### gather data for distribution and selected genotypes
          dat <- req(res_mgidi_val()$data_mean)
          dat.sel <- dat[match(res_mgidi_val()$res_mgidi$sel_gen, dat$genotype),]
          dat.sel <- merge(dat.sel, res_mgidi_val()$res_mgidi$MGIDI, by="genotype")
          
          ## other selected genotypes
          if(length(input$genotypesPlot) > 0){
            dat.supp <- dat[dat$genotype %in% input$genotypesPlot,]
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
              geom_density(color = 'skyblue') +
              geom_area(data = subset(df.dens, x >= q15.9 & x <= q84.1), # 1 Std 68.2%
                        aes(x=x,y=y), fill='skyblue', alpha=0.8) +
              geom_area(data = subset(df.dens, x >= q2.3 & x <= q97.7), # 2 Std 95.4%
                        aes(x=x,y=y), fill='skyblue', alpha=0.6) +
              geom_area(data = subset(df.dens, x >= q0.01 & x <= q99.9), # 3 Std 99.8%
                        aes(x=x,y=y), fill='skyblue', alpha=0.3) +
              geom_vline(xintercept=meanx, color="grey60", linewidth=1.5, linetype="dashed") +
              geom_vline(xintercept=medx, color='#FFFFFF',linewidth=1.5, linetype="dashed") +
              ggtitle(req(input$variablesPlot)) +
              geom_rug(alpha=0.8) +
              ggrepel::geom_label_repel(data=dat.sel,
                                        mapping = aes(x=.data[[input$variablesPlot]],y=0,
                                                      label=genotype,
                                                      fill=MGIDI),
                                        direction="y",point.padding = 0.01,
                                        force_pull = 0.1,size=6,
                                        vjust=0.6, max.overlaps = Inf,
                                        color="black",
                                        #fontface="bold",
                                        alpha=0.7) +
              scale_fill_viridis_c(begin=0.35, direction=1,option="H")+
              theme_bw() +
              theme(text=element_text(size=16),
                    axis.text.x = element_text(size=rel(1.5)))
            
          }) # end isolate
          ## render distribution plot
          output$DistribInd <- renderPlot({req(RVplots$Dist)})
          
          output$downloadPlotDist <- downloadHandler(
            filename = function(){
              paste(req(input$variablesPlot),"_DistribIndexPlot_",
                    format(Sys.time(),"%Y-%m-%d_%H%M"),'.png',sep='')},
            content = function(file){
              ggsave(file,plot=req(RVplots$Dist), width=11, height=6, scale=1.2)
            }
          )
          
          
          # Select genotypes to pin on top for the final table
          output$reference_selector2 <- renderUI({
            req(res_mgidi_val())
            shinyWidgets::pickerInput(
              ns("ref_genotypes2"),
              "Supplementary genotype(s)",
              choices = c(unique(res_mgidi_val()$data_mean$genotype)),
              multiple=TRUE,
              options=shinyWidgets::pickerOptions(liveSearch=T,
                                                  maxOptions=20,
                                                  actionsBox=TRUE,
                                                  virtualScroll = 200),
              selected = NULL
            )
          })
          
          
          ## Final table of ranked MGIDI with all columns
          output$ranked_traits_table <- DT::renderDT({
            req(res_mgidi_val())#, input$actionmgidi)
            
            # Get MGIDI table and mean values
            scores <- res_mgidi_val()$res_mgidi$MGIDI
            data <- data_r6$final()
            #mean_data <- res$data_mean
            
            # Traits used in selection
            rhot <- req(rhandsontable::hot_to_r(input$tabVar))
            selected_traits <- rhot$trait[!is.na(rhot$trait)]
            direction <- rhot$direction[!is.na(rhot$direction)]
            weights <- rhot$weight[!is.na(rhot$weight)]
            # if(length(weights) < selected_traits) {
            #   weights <- rep(1, length(selected_traits))
            # }
            names(direction) <- na.omit(rhot$trait)
            
            # Merge MGIDI score into trait table
            df <- merge(scores, data, by = "genotype", all.x = TRUE)
            df <- df[order(df$MGIDI), ]
            
            ## Add extra genotypes 
            if (!is.null(input$ref_genotypes2) && length(input$ref_genotypes2) > 0) {
              #print(input$ref_genotypes2)
              ref_rows <- df[df$genotype %in% input$ref_genotypes2,]
              ## retrieve missing genotypes
              if (length(input$ref_genotypes2) > nrow(ref_rows)) {
                missing_genos <- setdiff(input$ref_genotypes2, df$genotype)
                dat.supp <- req(res_mgidi_val()$data_mean) %>%
                  dplyr::filter(genotype %in% missing_genos) %>%
                  dplyr::mutate(MGIDI=NA, .after="genotype")
                ref_rows <- rbind(ref_rows, dat.supp)
              }
              rest <- df[!df$genotype %in% input$ref_genotypes2, ]
              df_print <- rbind(ref_rows, rest)
            } else {
              df_print <- df
            }
            
            
            # Subset to relevant columns
            ## order columns based on being in the selection index and by highest weight
            selected_traits <- selected_traits[order(weights, decreasing = T)]
            df_print <- dplyr::relocate(df_print,all_of(c("genotype","MGIDI",selected_traits)))
            
            # Build datatable
            numeric_cols <- names(df_print)[sapply(df_print, is.numeric)]
            dt <- DT::datatable(
              df_print,
              rownames = FALSE,
              extensions =list("ColReorder" = NULL,
                               "Buttons" = NULL),
              #"Scroller"=NULL),
              filter=list(position="top"),#, clear=F,selection = "multiple"),
              options = list(
                scrollX = TRUE,#scrollY=400,
                autoWidth = TRUE,
                #Scroller=TRUE,deferRender =TRUE,scrollY=400,
                pageLength = 10,
                colReorder = TRUE,
                dom = '<<t>Bp>',
                buttons = c('copy', 'excel','csv', 'pdf', 'print'),
                class = 'compact stripe hover row-border order-column',
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              )
            )
            # Round numeric columns to 2 decimals
            numeric_cols <- names(df)[sapply(df, is.numeric)]
            dt <- DT::formatRound(dt, columns = numeric_cols, digits = 2)
            
            # Apply red-green color per trait
            direction[["MGIDI"]] <- "min"
            for (trait in c("MGIDI",selected_traits)) {
              x <- df_print[[trait]]
              brks <- quantile(x, probs = seq(0.05, 0.95, 0.01), na.rm = TRUE)
              cols <- paletteer::paletteer_c(
                "ggthemes::Temperature Diverging",
                n = length(brks) + 1,
                direction = ifelse(direction[[trait]] == "max", -1, 1)
              )
              dt <- DT::formatStyle(dt, trait, backgroundColor = DT::styleInterval(brks, cols))
            }
            
            ## Color the genotype names of reference genotypes in honeydew color
            if (!is.null(input$ref_genotypes2) && length(input$ref_genotypes2) > 0) {
              req(input$ref_genotypes2)
              dt <- DT::formatStyle(dt, "genotype",
                                    backgroundColor = DT::styleEqual(input$ref_genotypes2, "grey"))
            }
            
            return(dt)
            
            
            
          }, server=FALSE) # renderDT
          
          
          
          
        }) ## observeEvent variablesPlot
        
      }## end of if Analyze
    }, ignoreInit = TRUE) ## observeEvent mode
  })
}

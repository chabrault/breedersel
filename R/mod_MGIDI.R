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
#' @import ggplot2
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
          width=40,
          easyClose = FALSE,
          
          # fluidPage(
          tags$h5("Selection index table"),
          ## editable table
          rhandsontable::rHandsontableOutput(ns("tabVar")),
          
          
          div(id = ns("mode_wrap"),
              shinyWidgets::radioGroupButtons(
                inputId = ns("mode"),
                label = NULL,
                choices = c("Edit", "Analyze"),
                selected = "Edit",
                width="90%",
                status = "primary",
                justified = TRUE,
                checkIcon = list(yes = icon("ok", lib = "glyphicon"))
              )
          ),
          br(),
          
          shiny::sliderInput(ns("sliderSI"),
                             width="90%",
                             label = "Selection intensity",
                             min=0, max=50, value=5,step = 1,post  = " %",),
          br(),
          shinyWidgets::downloadBttn(ns("DWNLD_SI"),"Download table",
                                     style = "bordered",color = "default"),
          br(),
          h6("Replace missing values by population average?"),
          shinyWidgets::materialSwitch(ns("avgNA"),status="info",value=TRUE),
          
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
          )))
          
          
          
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
               p("The less the contribution to the factor (closer to the outer part of the radar plot), 
                 the better the performance of the genotype for the traits related to this factor (see below).")
        )
      ),
      
      bs4Dash::box(
        width=7,
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
        p(strong("SD "), "selection differential = Xs-Xo."),
        p(strong("SDperc "), "selection differential percentage"),
        p(strong("Factor "), "indicates the factor axis the most associated with this trait."),
        p(strong("Sense "), "indicates the direction sought (min / max)."),
        p(strong("Goal "), "indicates if the aim has been reached.")
      ),
      bs4Dash::box(
        width=5,
        height="700px",
        title="Ranking of genotypes",
        closable = FALSE,
        collapsed=FALSE,
        maximizable = TRUE,
        status="lightblue",
        solidHeader = TRUE,
        p("Genotype names ordered by increasing MGIDI value (best to worst), with their score for the factor analysis."),
        #reactable::reactableOutput(ns("tab_mgidi")),
        DT::dataTableOutput(ns("tab_mgidi")),
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
        p("Distribution of the population value for one trait. Selected genotypes with the lowest MGIDI are named."),
        p("The background color of the label represents the MGIDI score across all selection index traits."),
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
      ),
      ## adding a table that shows the complementary between the genotypes
      bs4Dash::box(
        width = 12,
        title = "Table of genotype complementarity",
        status = "olive",
        solidHeader = TRUE,
        p("Best complement genotypes among the selected portion of the population and mean of the MGIDI and trait values."),
        fluidRow(
          column(6,
                 shiny::uiOutput(ns("reference_selector3"))
          ),
          column(6,
                 shiny::sliderInput(ns("sliderSIComp"),
                                    width="300px",
                                    label = "Selection intensity",
                                    min=0, max=50, value=10,step = 1,post  = " %",)
          ),
          DT::dataTableOutput(ns("table_complement"))
        )
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
        rhandsontable::hot_col(col = "weight", type = "numeric",format = "0.0[00]",
                               step = 0.01 ) %>%
        rhandsontable::hot_validate_numeric(col = "weight", min=0) %>%
        rhandsontable::hot_cols(halign = "htCenter", valign = "htMiddle",
                                colWidths = c(140, 70, 70, 80),
                                manualColumnResize = TRUE,        # enable user resizing
                                renderer = "function (instance, td, row, col, prop, value, cellProperties) {
                              Handsontable.renderers.NumericRenderer.apply(this, arguments);
                              td.style.color = 'black';
                              if (cellProperties.readOnly) {
                                td.style.background = '#d3d3d3';  // light grey
                                td.style.color = '#555';          // darker text
                              }
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
        
        # --- Run MGIDI ------
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
          metan:::plot.mgidi(res_mgidi_val()$res_mgidi, type = "contribution", genotypes="selected",
                             radar = req(input$type)) +
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
        output$tab_mgidi <- DT::renderDT({
          req(res_mgidi_val())
          dat2plot <- merge(res_mgidi_val()$res_mgidi$MGIDI,
                            res_mgidi_val()$res_mgidi$contri_fac,
                            by.x="genotype", by.y="GEN")
          dat2plot <- dat2plot[order(dat2plot$MGIDI),]
          
          dt.mgidi <- DT::datatable(
            dat2plot,
            rownames = FALSE,
            extensions =list("ColReorder" = NULL,"Buttons" = NULL),
            filter=list(position="top"),#, clear=F,selection = "multiple"),
            options = list(
              scrollX = TRUE,scrollY=400,
              autoWidth = TRUE,
              #pageLength = 8,
              paging = FALSE,
              colReorder = TRUE,
              dom = 't',
              #dom = '<<t>Bp>',
              #buttons = c('copy', 'excel','csv', 'pdf', 'print'),
              class = 'compact stripe hover row-border order-column',
              columnDefs = list(list(className = 'dt-center', targets = "_all"))
            )
          )
          # # Round numeric columns to 2 decimals
          cols.dat <- colnames(dat2plot)[2:ncol(dat2plot)]
          dt.mgidi <- DT::formatRound(dt.mgidi, columns=cols.dat, digits = 2)
          for (c in cols.dat) {
            x <- dat2plot[[c]]
            brks <- quantile(x, probs = seq(0.05, 0.95, 0.01), na.rm = TRUE)
            cols <- paletteer::paletteer_c(
              "ggthemes::Temperature Diverging",
              n = length(brks) + 1,direction =  1)
            dt.mgidi <- DT::formatStyle(dt.mgidi, c, backgroundColor = DT::styleInterval(brks, cols))
          }
          return(dt.mgidi)
        })
        
        
        ## Save MGIDI score table
        output$DWNLD <- downloadHandler(
          filename = paste0("MGIDI_scores_",format(Sys.time(),"%Y-%m-%d_%H%M"),".tsv"),
          content = function(fname){
            write.table(merge(res_mgidi_val()$res_mgidi$MGIDI,
                              res_mgidi_val()$res_mgidi$scores_gen,
                              by.x="genotype", by.y="GEN"),sep="\t",
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
                                                size=10,
                                                actionsBox=TRUE,
                                                virtualScroll = 10),
            selected = NULL
          )
        })
        
        
        ## Correlation plot
        output$corrplot <- renderPlot({
          req(res_mgidi_val())
          dat <- req(res_mgidi_val()$data_mean)%>%dplyr::select(-genotype)
          #print(str(dat))
          plot(x=metan::corr_coef(data=dat, use="pairwise.complete.obs"),
               size.text.cor = 6, size.text.lab=13,size.text.signif=5)
        })
        
        output$downloadPlotCorr <- downloadHandler(
          filename = function(){
            paste("CorrPlot_",
                  format(Sys.time(),"%Y-%m-%d_%H%M"),'.png',sep='')},
          content = function(file){
            ggsave(file,plot=ggplot2::last_plot(), width=11, height=6, scale=1.2)
          }
        )
        
        
        #### --- Distribution plot --- #####
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
              geom_rug(alpha=0.8) + ## TODO: color geom rug for selected geno
              ggrepel::geom_label_repel(data=dat.sel,
                                        mapping = aes(x=.data[[input$variablesPlot]],y=0,
                                                      label=genotype,fill=MGIDI),
                                        direction="y",point.padding = 0.01,
                                        force_pull = 0.1,size=6,
                                        vjust=0.6, max.overlaps = Inf,
                                        color="black",
                                        #fontface="bold",
                                        alpha=0.7) +
              geom_rug(data=dat.sel, mapping = aes(x=.data[[input$variablesPlot]],color=MGIDI),
                       length = unit(0.05, "npc"), linewidth=0.5)+
              scale_fill_viridis_c(begin=0.35, direction=1,option="H")+
              scale_color_viridis_c(begin=0.35, direction=1,option="H")+
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
          
          
          #### --- Table of ranked phenotypes --- ##### 
          
          # Select genotypes to pin on top for the final table
          output$reference_selector2 <- renderUI({
            req(res_mgidi_val())
            shinyWidgets::pickerInput(
              ns("ref_genotypes2"),
              "Supplementary genotype(s)",
              choices = c(unique(res_mgidi_val()$data_mean$genotype)),
              multiple=TRUE, width="auto",
              options=shinyWidgets::pickerOptions(liveSearch=T,
                                                  maxOptions=10,
                                                  size = 10,# number of items visible before scrolling           
                                                  virtualScroll = 10,
                                                  actionsBox=TRUE),
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
            rhot <- rhot[!is.na(rhot$trait),]
            selected_traits <- rhot$trait[!is.na(rhot$trait)]
            direction <- rhot$direction[!is.na(rhot$direction)]
            names(direction) <- na.omit(rhot$trait)
            # weights <- rhot$weight[!is.na(rhot$weight)]
            # if(length(weights) < selected_traits) {
            #   weights <- rep(1, length(selected_traits))
            # }
            if(all(is.na(rhot$weight))) rhot$weight <- 1
            weights <- rhot$weight
            
            
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
            ### Build datatable
            numeric_cols <- names(df_print)[sapply(df_print, is.numeric)]
            dt <- DT::datatable(
              df_print,
              rownames = FALSE,
              extensions =list("ColReorder" = NULL,"Buttons" = NULL),
              #"Scroller"=NULL),
              filter=list(position="top"),#, clear=F,selection = "multiple"),
              options = list(
                scrollX = TRUE,#scrollY=400,
                autoWidth = TRUE,
                #Scroller=TRUE,deferRender =TRUE,scrollY=400,
                pageLength = 10,
                colReorder = TRUE,
                dom = '<<t>Bp>',
                buttons=list(
                  list(extend="copy"),
                  list(extend="csv",title=NULL,filename=paste0("genotype_score_pheno_",format(Sys.time(),"%Y-%m-%d_%H%M"))),
                  list(extend="excel",title=NULL,filename=paste0("genotype_score_pheno_",format(Sys.time(),"%Y-%m-%d_%H%M"))),
                  list(extend="pdf",title=NULL,filename=paste0("genotype_score_pheno_",format(Sys.time(),"%Y-%m-%d_%H%M"))),
                  list(extend="print",title=NULL,filename=paste0("genotype_score_pheno_",format(Sys.time(),"%Y-%m-%d_%H%M")))
                ),
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
              cols <- paletteer::paletteer_c("ggthemes::Temperature Diverging",
                                             n = length(brks) + 1,
                                             direction = ifelse(direction[[trait]] == "max", -1, 1)
              )
              dt <- DT::formatStyle(dt, trait, backgroundColor = DT::styleInterval(brks, cols))
            }
            
            ## Color the genotype names of reference genotypes in grey color
            if (!is.null(input$ref_genotypes2) && length(input$ref_genotypes2) > 0) {
              req(input$ref_genotypes2)
              dt <- DT::formatStyle(dt, "genotype",
                                    backgroundColor = DT::styleEqual(input$ref_genotypes2, "grey"))
            }
            return(dt)
          }, server=FALSE) # renderDT
          
          
        }) ## observeEvent variablesPlot
        
        ### --- Complementary genotypes ---- ####
        
        
        ## 1. Select genotype to complement
        output$reference_selector3 <- renderUI({
          req(res_mgidi_val())
          
          # defensive extraction
          mgidi_df <- res_mgidi_val()$res_mgidi$MGIDI
          if (!is.data.frame(mgidi_df) || !"genotype" %in% colnames(mgidi_df) || !"MGIDI" %in% colnames(mgidi_df)) {
            message("reference_selector3: MGIDI object not in expected format")
            return(NULL)
          }
          
          # ordered character vector of genotypes
          gen.ord.MGIDI <- as.character(mgidi_df$genotype[order(mgidi_df$MGIDI)])
          if (length(gen.ord.MGIDI) == 0) {
            message("reference_selector3: no genotypes available")
            return(NULL)
          }
          
          # debug (safe)
          # message("reference_selector3 sample: ", paste(head(gen.ord.MGIDI, 5), collapse = ", "))
          
          shinyWidgets::pickerInput(
            inputId = ns("ref_genotypes3"),
            label = "Genotype to complement",
            choices = gen.ord.MGIDI,
            multiple = FALSE,
            width = "auto",
            options = shinyWidgets::pickerOptions(
              liveSearch = TRUE,
              actionsBox = TRUE,
              size = 10,# number of items visible before scrolling           
              virtualScroll = 10 
            ),
            selected = NULL  # <- don't use NA
          )
        })
        
        output$table_complement <- DT::renderDT({
          req(res_mgidi_val(), input$ref_genotypes3,input$sliderSIComp, input$tabVar)  
          
          ## 2. Intensity of selection and list of complements
          
          mgidi_df <- res_mgidi_val()$res_mgidi$MGIDI
          gen.ord.MGIDI <- as.character(mgidi_df$genotype[order(mgidi_df$MGIDI)])
          gen.comp.sel <- gen.ord.MGIDI[1:(req(input$sliderSIComp)*length(gen.ord.MGIDI)/100)]
          
          ## 3. Output table of crosses with complementary score and mean value by trait
          compl_sel_gen <-
            res_mgidi_val()$res_mgidi$contri_fac |>
            subset(GEN %in% unique(c(input$ref_genotypes3,gen.comp.sel))) |>
            metan::column_to_rownames("GEN")
          compl_mat <- dist(compl_sel_gen) |> as.matrix()
          
          
          if (!is.null(req(input$ref_genotypes3)) && length(req(input$ref_genotypes3)) > 0 &&
              req(input$ref_genotypes3) %in% rownames(compl_mat)) {
            req(input$ref_genotypes3)
            # print(input$ref_genotypes3)
            # print(gen.comp.sel)
            ## extract the complementarity
            compl_sel_par <- as.data.frame(compl_mat[req(input$ref_genotypes3),,drop=FALSE])
            compl_sel_par <- tidyr::pivot_longer(compl_sel_par, cols=everything(),
                                                 names_to="Parent2", 
                                                 values_to = "Complement")
            ## build a matrix with for each pair of parents, 
            ## the complementarity value and the mean phenotype for the selected traits
            compl_sel_par <- cbind(data.frame(Parent1=req(input$ref_genotypes3)), compl_sel_par)
            compl_sel_par <- compl_sel_par[order(compl_sel_par$Complement, decreasing=T),]
            ##compute mean phenotype for selected_traits
            
            # Traits used in selection
            rhot <- req(rhandsontable::hot_to_r(input$tabVar))
            rhot <- rhot[!is.na(rhot$trait),]
            selected_traits <- rhot$trait[!is.na(rhot$trait)]
            if(all(is.na(rhot$weight))) rhot$weight <- 1
            ## order columns based on being in the selection index and by highest weight
            selected_traits <- selected_traits[order(rhot$weight, decreasing = T)]
            direction <- rhot$direction[!is.na(rhot$direction)]
            names(direction) <- na.omit(rhot$trait)
            
            compl_sel_par_mean <- data.frame()
            
            for(i in 1:nrow(compl_sel_par)){
              mean_traits <- res_mgidi_val()$data_mean |> 
                dplyr::left_join(res_mgidi_val()$res_mgidi$MGIDI, by="genotype") |> 
                filter(genotype %in% compl_sel_par[i,c("Parent1","Parent2")]) |> 
                dplyr::summarize(across(all_of(c("MGIDI",selected_traits)), mean, na.rm=T)) 
              compl_sel_par_mean <- rbind(compl_sel_par_mean, cbind(compl_sel_par[i,],mean_traits))
            }
            
            dt.out <- DT::datatable(compl_sel_par_mean,
                                    rownames = FALSE,
                                    extensions =list("ColReorder" = NULL,"Buttons" = NULL),
                                    #"Scroller"=NULL),
                                    filter=list(position="top"),
                                    options = list(
                                      scrollX = TRUE,#scrollY=400,
                                      autoWidth = TRUE,
                                      pageLength = 10,
                                      colReorder = TRUE,
                                      dom = '<<t>Bp>',
                                      buttons=list(
                                        list(extend="copy"),
                                        list(extend="csv",title=NULL,filename=paste0("genotype-pair_complement-score_pheno_",format(Sys.time(),"%Y-%m-%d_%H%M"))),
                                        list(extend="excel",title=NULL,filename=paste0("genotype-pair_complement-score_pheno_",format(Sys.time(),"%Y-%m-%d_%H%M"))),
                                        list(extend="pdf",title=NULL,filename=paste0("genotype-pair_complement-score_pheno_",format(Sys.time(),"%Y-%m-%d_%H%M"))),
                                        list(extend="print",title=NULL,filename=paste0("genotype-pair_complement-score_pheno_",format(Sys.time(),"%Y-%m-%d_%H%M")))
                                      ),
                                      class = 'compact stripe hover row-border order-column',
                                      columnDefs = list(list(className = 'dt-center', targets = "_all"))
                                    )
            )
            
            ## Round numeric columns to 2 decimals
            numeric_cols <- names(compl_sel_par_mean)[sapply(compl_sel_par_mean, is.numeric)]
            dt.out <- DT::formatRound(dt.out, columns = numeric_cols, digits = 2)
            
            ## add color gradient to the traits
            # Apply red-green color per trait
            direction[["MGIDI"]] <- "min" ; direction[["Complement"]] <- "max"
            for (trait in c("Complement","MGIDI",selected_traits)) {
              x <- compl_sel_par_mean[[trait]]
              brks <- quantile(x, probs = seq(0.05, 0.95, 0.01), na.rm = TRUE)
              cols <- paletteer::paletteer_c("ggthemes::Temperature Diverging",
                                             n = length(brks) + 1,
                                             direction = ifelse(direction[[trait]] == "max", -1, 1)
              )
              dt.out <- DT::formatStyle(dt.out, trait, backgroundColor = DT::styleInterval(brks, cols))
            }
            return(dt.out)
            
          } else {
            print(compl_mat) # debug
          }
          
        }, server=FALSE) ## end renderDT tablle complement
        
      }## end of if Analyze
    }, ignoreInit = TRUE) ## observeEvent mode
  })
}

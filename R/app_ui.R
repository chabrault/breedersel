#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shiny fluidPage fluidRow actionButton column tabsetPanel insertTab
#' @importFrom bs4Dash bs4DashPage dashboardHeader bs4DashSidebar bs4SidebarHeader sidebarMenu bs4SidebarMenu bs4SidebarMenuItem bs4SidebarMenuSubItem dashboardBody tabItem bs4TabItem bs4TabItems tabItems
#' @importFrom datamods i18n
#' @noRd

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    bs4DashPage(skin="light",# = "purple",
                fullscreen = TRUE,
                freshTheme = bslib::bs_theme(version = 4),
                header=dashboardHeader(title="Grape selector",
                                       controlbarIcon=shiny::icon("circle", verify_fa = FALSE)),
                
                # ----------- SIDEBAR ------------- #
                sidebar=bs4DashSidebar(
                  collasped=FALSE,
                  expandOnHover=TRUE,
                  fixed=TRUE,
                  
                  # bs4SidebarHeader("Menu"),
                  #selectInput("lang", "Language", choices = c("English" = "en", "Français" = "fr"), selected = "en"),
                  
                  
                  bs4SidebarMenu(
                    bs4SidebarMenuItem("Data loading",
                                       tabName="TabUpload",icon=icon("upload")),
                    
                    bs4SidebarMenuItem("Table of data",
                                       tabName = "TabSumm",icon=icon("table-columns")),
                    
                    bs4SidebarMenuItem("Data filtering",
                                       tabName = "Tabdata",icon=icon("sliders")),
                    
                    bs4SidebarMenuItem("MGIDI",
                                       tabName = "TabplotMGIDI",icon=icon("plus-minus")),
                    
                    bs4SidebarMenuItem("Magic graph",
                                       tabName = "TabplotEsquisse",icon=icon("wand-magic-sparkles")),
                    bs4SidebarMenuItem("About",
                                       tabName = "Apropos",icon=icon("circle-info")
                    )
                  )
                  
                ), #bs4DashSidebar
                # ----------- BODY ------------- #
                ## Menu load data
                body=bs4Dash::bs4DashBody(
                  bs4TabItems(
                    bs4TabItem(tabName = "TabUpload",
                               shiny::fluidPage(
                                 mod_import_table_ui("import_table_1")
                                 
                               )
                    ),
                    
                    
                    ## Table of data summary
                    bs4TabItem(tabName = "TabSumm",
                               shiny::fluidPage(
                                 h2("Data table"),
                                 #shiny::uiOutput("output_data")
                                 reactable::reactableOutput("output_data")
                               )
                    ),
                    
                    
                    ## Tab for filtering data
                    bs4TabItem(tabName = "Tabdata",
                               shiny::fluidPage(
                                 # shiny::column(width=5,
                                 #               shiny::actionButton("valid_upVars","Modifier les colonnes")),
                                 #
                                 # shiny::column(width=12,
                                 #               datamods::update_variables_ui(id="update_data",title=TRUE),
                                 # ),
                                 shiny::column(width=5,
                                               shinyWidgets::actionBttn("valid_filtVars","Filtering?",
                                                                        style = "pill",
                                                                        color = "danger")),
                                 br(),
                                 shiny::column(width=12,
                                               mod_data_filtering_ui("filt_data")
                                 )
                               )
                    ),
                    
                    bs4TabItem(tabName="TabplotMGIDI",
                               fluidRow(
                                 shiny::column(width=12,
                                               h2("Creation of a multivariate selection index"),
                                               #shiny::actionButton("valid_plotPoint2",label="Lancer le graph"),
                                               mod_MGIDI_ui("mgidi_1")
                                               
                                 )
                               )
                    ),
                    
                    bs4TabItem(tabName="TabplotEsquisse",
                               fluidPage(
                                 h2("Custom plot"),
                                 column(6,
                                        shinyWidgets::actionBttn("launch_esquisse",
                                                                 label="Launch esquisse app",
                                                                 style = "pill",
                                                                 color = "danger"))
                               ),
                               column(12,
                                      
                                      esquisse::esquisse_ui(id="esquisse",
                                                            header = FALSE, # dont display gadget title
                                                            container = esquisse::esquisse_container(height = "700px",
                                                                                                     fixed=FALSE))
                               )
                               
                    ),
                    bs4TabItem(tabName="Apropos",
                               fluidPage(
                                 h2("Grape Selector application"),
                                 column(12,
                                        h5("Author: Charlotte Brault"),
                                        a(actionButton(inputId = "email1", label = "Send email",
                                                       class = "btn-info",
                                                       icon = icon("envelope", lib = "font-awesome")),
                                          href="mailto:charlotte.brault@live.com"),
                                        br(),
                                        h5("Project: Vitis Explorer"),
                                        br(),
                                        h5("Version of the application:"),textOutput("pkgVersion"),
                                        br(),
                                        bs4Dash::bs4Card(
                                          collapsed = TRUE,
                                          title = div(icon("jsfiddle"), "Rsession"), width = 12,
                                          verbatimTextOutput("Rsession"),
                                          br()
                                        ),
                                        
                                        fluidRow(
                                          column(4,tags$img(src='logo/FranceAgriMer-logo.png', height="120px")),
                                          column(4,tags$img(src='logo/Logo-MAA-CASDAR.png', height="100px")),
                                          column(4,tags$img(src='logo/cniv-logo.jpeg', height="100px")),
                                          
                                          column(12,br()),
                                          column(5,tags$img(src='logo/INRAE_color.png', height="100px"), offset=1),
                                          column(5,tags$img(src='logo/IFV_nom.png', height="150px"))
                                          
                                        )
                                 )
                                 
                               )
                               
                    ) # bs4TabItem A propos
                  ) # tabItems
                ) # dashboard body
    ) # dashboardpage
  )
  
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path("www", app_sys("app/www"))
  golem::add_resource_path('logo', app_sys('app/logo'))
  golem::add_resource_path('ex_tab', app_sys('app/ex_tab'))
  #golem::add_resource_path("www", "inst/app/www")
  tags$head(
    favicon(ico = "favicon", resources_path = "www", ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "grapesel"
    ),
    # force Bootstrap tooltip re-init
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        $('[data-toggle=\"tooltip\"]').tooltip({trigger: 'hover'});
      });
    "))
  )
}


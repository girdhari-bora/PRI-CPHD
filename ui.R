options(shiny.sanitize.errors = TRUE)

library(DT)
library(shiny)
library(shinydashboard)
library(dplyr)
library(shiny)
# library(collapsibleTree)
library(shinyBS)
library(shinyjs)
library(highcharter)
# library(shiny)
# library(shinydashboard)
# library(dplyr)
library(RColorBrewer)
# library(dygraphs)
# library(xts)
# library(highcharter)
# library(dplyr)
# library(DT)
# library(data.table)
# library(shinyjs)
library(htmltools)
library(apputils)
# library(collapsibleTree)
#library(data.table)
#library(dplyr)
# require("httr")
# require("jsonlite")
library(httr)
library(jsonlite)
library(rdrop2)
#library(dplyr)
library(apputils)
library(shinycssloaders)



dateRangeMonthsInput <- function(inputId, label, start = NULL, end = NULL,
                                 min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month",
                                 minviewmode="months", # added manually
                                 weekstart = 0, language = "en", separator = " to ", width = NULL) {
  
  # If start and end are date objects, convert to a string with yyyy-mm-dd format
  # Same for min and max
  if (inherits(start, "Date"))  start <- format(start, "%Y-%m-%d")
  if (inherits(end,   "Date"))  end   <- format(end,   "%Y-%m-%d")
  if (inherits(min,   "Date"))  min   <- format(min,   "%Y-%m-%d")
  if (inherits(max,   "Date"))  max   <- format(max,   "%Y-%m-%d")
  
  htmltools::attachDependencies(
    div(id = inputId,
        class = "shiny-date-range-input form-group shiny-input-container",
        style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
        
        controlLabel(inputId, label),
        # input-daterange class is needed for dropdown behavior
        div(class = "input-daterange input-group",
            tags$input(
              class = "input-sm form-control",
              type = "text",
              `data-date-language` = language,
              `data-date-weekstart` = weekstart,
              `data-date-format` = format,
              `data-date-start-view` = startview,
              `data-date-min-view-mode` = minviewmode, # added manually
              `data-min-date` = min,
              `data-max-date` = max,
              `data-initial-date` = start
            ),
            span(class = "input-group-addon", separator),
            tags$input(
              class = "input-sm form-control",
              type = "text",
              `data-date-language` = language,
              `data-date-weekstart` = weekstart,
              `data-date-format` = format,
              `data-date-start-view` = startview,
              `data-date-min-view-mode` = minviewmode, # added manually
              `data-min-date` = min,
              `data-max-date` = max,
              `data-initial-date` = end
            )
        )
    ),
    datePickerDependency
  )
}

`%AND%` <- function(x, y) {
  if (!is.null(x) && !is.na(x))
    if (!is.null(y) && !is.na(y))
      return(y)
  return(NULL)
}

controlLabel <- function(controlName, label) {
  label %AND% tags$label(class = "control-label", `for` = controlName, label)
}

# the datePickerDependency is taken from https://github.com/rstudio/shiny/blob/master/R/input-date.R
datePickerDependency <- htmltools::htmlDependency(
  "bootstrap-datepicker", "1.6.4", c(href = "shared/datepicker"),
  script = "js/bootstrap-datepicker.min.js",
  stylesheet = "css/bootstrap-datepicker3.min.css",
  # Need to enable noConflict mode. See #1346.
  head = "<script>
 (function() {
 var datepicker = $.fn.datepicker.noConflict();
 $.fn.bsDatepicker = datepicker;
 })();
 </script>")


########################################################################################################################
########################################################################################################################

getYear <- function() {
  years <- unique(get_data_df$year)
  years <- sort(as.integer(matrix(unlist(strsplit(unlist(years), "-")),ncol=2, byrow = TRUE)[,1]))
   
 }

###########################

#sidebar and related functions

dbHeader <- dashboardHeader(title = "PERFORMANCE", titleWidth = 250,
                            tags$li(a(href = 'http://www.nadia.gov.in/',
                                      img(src = 'LOGO.png',
                                          title = "CPHD Website", height = "50px"),
                                      style = "padding-top:0px; padding-bottom:0px;"),
                                    class = "dropdown"))

sidebar <- dashboardSidebar(
  
    sidebarMenu(  id = "tabs", 
               
                tags$head(
                 tags$style(HTML(".selectize-input.input-active, .selectize-input.input-active:hover, .selectize-control.multi .selectize-input.focus {border-color: red !important;}
                                 .selectize-dropdown .active {background: yellow !important;}"))
                 ),
               
               selectInput("block_parishad", label = "Panchayat Samiti", choices = choices_block 
                           ,
                           # selected = NULL,
                           multiple=FALSE
                           # , options = list(placeholder = 'select Blocks...',maxOptions = 100)
                ),
               
              selectInput("gram_panchayat", label = "Gram Panchayat", choices = choices_panchayat, 
                          # selected = NULL,
                           multiple=FALSE
                          # , options = list(placeholder = 'select Panchayats...',maxOptions = 100, maxItems = 100)
                ),
               
              selectInput("gram_sansad", label = "Gram Sansad", choices = choices_sansad, 
                          # selected = NULL,
                           multiple=FALSE
                          # , options = list(placeholder = 'select Sansads...',maxOptions = 100, maxItems = 100)
                ),
              
              dateRangeMonthsInput('monthRange',
                             label = "Period (select months)",
                             start = min(as.Date("2017-12-01")),
                             end = max(as.Date(get_data_df_final$year_month_date)),
                             # min = min(as.Date(get_data_df_final$year_month_date)),
                             min = as.Date("2017-12-01"),
                             max = max(as.Date(get_data_df_final$year_month_date)),
                             separator = " to ", format = "M-yyyy",
                             startview = "month", minviewmode="months", language = 'en', weekstart = 0, width = NULL
                        ),
              
              br(),
            menuItem("Key Indicators", tabName = "key_indicators", icon = icon("search-plus"), selected = TRUE),
            menuItem("Health and Family Welfare", tabName = "hfw_report", icon = icon("medkit")),
            menuItem("Child Development", tabName = "cd_report", icon = icon("child")),
            menuItem("Panchayat & Rural Dev.", tabName = "prd_report", icon = icon("users"))
            # ,
  # 
  # dashboard_footer(src = ' ' , href = "https://www.tattvafoundation.org/",label = "Analytics support: Tattva Foundation", width = "100%", height = "50px",
  #                               italic = TRUE, bold = TRUE,
  #                 style = "text-align:center;align: center; padding: 0px; margin: 0px; position: fixed;bottom: 0; z-index:9;color:Gray;font-size:12px")
  
#   tags$footer(title="Your footer here", align = "right", style = "
# position:absolute;
# bottom:0;
# width:100%;
# height:50px; /* Height of the footer */
# color: white;
# padding: 10px;
# background-color: black;
# z-index: 1000;"
#   )
  
)


)


#dashboard main body related functions

body <- dashboardBody ( 
   tags$head(tags$link(rel = "icon", href = "./www/favicon.ico?v=1.1")),

   tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #0392cf;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #f4b943;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #000000;
                              }        

        # /* main sidebar */
        # .skin-blue .main-sidebar {
        #                       background-color: #000000;
        #                       }
        # 
        # /* active selected tab in the sidebarmenu */
        # .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
        #                       background-color: #ff0000;
        #                       }
        # /* other links in the sidebarmenu */
        # .skin-blue .main-sidebar .sidebar .sidebar-menu a{
        #                       background-color: #00ff00;
        #                       color: #000000;
        #                       }
        # 
        # /* other links in the sidebarmenu when hovered */
        #  .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
        #                       background-color: #ff69b4;
        #                       }
        # /* toggle button when hovered  */                    
        #  .skin-blue .main-header .navbar .sidebar-toggle:hover{
        #                       background-color: #ff69b4;
        #  }

                              '))),
# tabsetPanel
  tabItems( 
   
    tabItem( 
            tabName = "hfw_report", 
            fluidRow(
             box(DT::dataTableOutput("health_family_welfare"), width = 12,collapsible = TRUE,
                 status = "primary",solidHeader = TRUE,title = "Health and Family Welfare Indicators"),
              withSpinner(uiOutput("popup"))
              
    ))
    ,
    
    tabItem(  tabName = "cd_report", 
              fluidRow(
          box(DT::dataTableOutput("child_development"), width = 12,collapsible = TRUE,
          status = "primary",solidHeader = TRUE,title = "Child Development Indicators"),
        withSpinner(uiOutput("popup1"))       
              )
    ),

    tabItem( tabName = "prd_report", 
             fluidRow(
             box(DT::dataTableOutput("panchayat_rural_development"), width = 12,collapsible = TRUE,
                 status = "primary",solidHeader = TRUE,title = "Panchayat and Rural Development Indicators"),
            withSpinner(uiOutput("popup2")))      
    )
    ,

     ### tabPanel
     tabItem( tabName = "key_indicators",
             fluidRow(
                 box( withSpinner(highchartOutput("under18Preg", width = "100%", height = "350px")), width = 6,collapsible = TRUE,
                     status = "primary",solidHeader = TRUE,title = "Under-18 Pregnancies (%)"),
                 box(withSpinner(highchartOutput("homeDelivery", width = "100%", height = "350px")), width  = 6,collapsible = TRUE,
                   status = "primary",solidHeader = TRUE,title = "Home Deliveries of Total Deliveries (%)"),
                 box(withSpinner(highchartOutput("completeImmunization", width = "100%", height = "350px")), width = 6,collapsible = TRUE,
                     status = "primary",solidHeader = TRUE,title = "Complete Immunization within 12 months (%)"),
                 box(withSpinner(highchartOutput("reportedDiaChol", width = "100%", height = "350px")), width = 6,collapsible = TRUE,
                     status = "primary",solidHeader = TRUE,title = "Population reported for Diarrhoea/Cholera (%)"),
                 box(withSpinner(highchartOutput("reportedUknownFever", width = "100%", height = "350px")), width = 6,collapsible = TRUE,
                     status = "primary",solidHeader = TRUE,title = "Population reported for Fever (%)"),
               box(withSpinner(highchartOutput("reportedLBW", width = "100%", height = "350px")), width = 6,collapsible = TRUE,
                     status = "primary",solidHeader = TRUE,title = "Low Birth Weight (%)"),
                 box(withSpinner(highchartOutput("reportedSAM", width = "100%", height = "350px")), width = 6,collapsible = TRUE,
                     status = "primary",solidHeader = TRUE,title = "Severely Malnourished Children (%)"),
                 box(withSpinner(highchartOutput("pieToilets", width = "100%", height = "350px")), width = 6,collapsible = TRUE,
                     status = "primary",solidHeader = TRUE,title = "Type of Toilets (all Blocks)"),
                 box(withSpinner(highchartOutput("reportedOpenDefecated", width = "100%", height = "350px")), width = 6,collapsible = TRUE,
                     status = "primary",solidHeader = TRUE,title = "Households Defecated in Open Space (%)"),
                 box(withSpinner(highchartOutput("reportedExcretaToiletPan", width = "100%", height = "350px")), width = 6,collapsible = TRUE,
                     status = "primary",solidHeader = TRUE,title = "Households Excreta Not Disposed In Toilet (%)"),
                 box(withSpinner(highchartOutput("pieSLWMStatus", width = "100%", height = "350px")), width = 6,collapsible = TRUE,
                     status = "primary",solidHeader = TRUE,title = "SLWM Status at Household Level (all Blocks)")

               ))

  )
  
  
)  
  




ui<- dashboardPage( title="PRI CPHD",
  
  dbHeader, 
  sidebar,
  body
)







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

dbHeader <- dashboardHeader(title = "PRI Dashboard", titleWidth = 250,
                            tags$li(a(href = 'http://www.nadia.gov.in/',
                                      img(src = 'CPHD.png',
                                          title = "CPHD Website", height = "40px"),
                                      style = "padding-top:5px; padding-bottom:3px;"),
                                    class = "dropdown"))

sidebar <- dashboardSidebar(
  
  
  sidebarMenu( id = "sidebarmenu",  
               
                tags$head(
                 tags$style(HTML(".selectize-input.input-active, .selectize-input.input-active:hover, .selectize-control.multi .selectize-input.focus {border-color: red !important;}
                                 .selectize-dropdown .active {background: yellow !important;}"))
                 ),
               
               selectizeInput("block_parishad", label = "Block Parishad", choices = choices_block , selected = choices_block[1],
                           multiple=TRUE, options = list(placeholder = 'select Blocks...',
                                                                         maxOptions = 100)
                ),
               
              selectizeInput("gram_panchayat", label = "Gram Panchayat", choices = NULL, selected = NULL,
                           multiple=TRUE, options = list(placeholder = 'select Panchayats...',maxOptions = 100, maxItems = 100)
                ),
               
              selectizeInput("gram_sansad", label = "Gram Sansad", choices = NULL, selected = NULL,
                           multiple=TRUE, options = list(placeholder = 'select Sansads...',maxOptions = 100, maxItems = 100)
                ),
              
               dateRangeMonthsInput('monthRange',
                             label = "Period (select months)",
                             start = min(as.Date(get_data_df_final$year_month_date)),
                             end = max(as.Date(get_data_df_final$year_month_date)),
                             min = min(as.Date(get_data_df_final$year_month_date)),
                             max = max(as.Date(get_data_df_final$year_month_date)),
                             separator = " to ", format = "M-yyyy",
                             startview = "month", minviewmode="months", language = 'en', weekstart = 0, width = NULL
                        )
              
                # br(),
                # # menuItem("Referral Tree", tabName = "referralTree", icon = icon("user-md")),
                # 
                # menuItem("Daily Call Records", tabName = "dailyCalls", icon = icon("table")),
                # br(),
                # 
                # menuItem("Key Indicators (all cases)", tabName = "indicators", icon = icon("line-chart")),
                # br(),
                # menuItem("Diarrhoea Referral Cases", tabName = "referral", icon = icon("medkit")),
                # br()
               
              ),
  
  dashboard_footer(src = ' ' , href = "https://www.tattvafoundation.org/",label = "Analytics support: Tattva Foundation", width = "100%", height = "50px",
                                italic = TRUE, bold = TRUE,
                  style = "text-align:center;align: center; padding: 0px; margin: 0px;")
)


#dashboard main body related functions

body <- dashboardBody ( 
 
  tags$head(tags$link(rel = "icon", href = "./www/favicon.ico?v=1.1")),
  
 
  tabsetPanel(
    
    
    tabPanel( "Health and Family Welfare",
              DT::dataTableOutput("health_family_welfare")
              # DT::dataTableOutput('sc_tbl'),
              # span("Number of movies selected:",
              #      textOutput("n_movies")
    ),
    tabPanel("Child Development",
             DT::dataTableOutput("child_development")
             # DT::dataTableOutput('sc_tbl'),
             # span("Number of movies selected:",
             #      textOutput("n_movies")
    ),
    
    tabPanel( "Panchayat and Rural Development",
              DT::dataTableOutput("panchayat_rural_development")
              # DT::dataTableOutput('sc_tbl'),
              # span("Number of movies selected:",
              #      textOutput("n_movies")
    )
    
    # conditionalPanel(condition="$('html').hasClass('shiny-busy')",
    #                  tags$div("Loading...",id="loadmessage"))
    
  )
  
  
)  
  




ui<- dashboardPage( title="PRI CPHD",
  
  dbHeader, 
  sidebar,
  body
)






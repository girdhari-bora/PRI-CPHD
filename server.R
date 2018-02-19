library(DT)
library(shiny)
library(shinydashboard)
library(dplyr)
library(shiny)
library(collapsibleTree)
library(shinyBS)
library(shinyjs)
library(highcharter)
library(shiny)
library(shinydashboard)
library(dplyr)
library(RColorBrewer)
library(dygraphs)
library(xts)
library(highcharter)
library(dplyr)
library(DT)
library(data.table)
library(shinyjs)
library(htmltools)
library(apputils)
library(collapsibleTree)
library(data.table)
library(dplyr)
require("httr")
require("jsonlite")
library(httr)
library(rdrop2)
library(dplyr)


# This function will create the buttons for the datatable, they will be unique
shinyInput <- function(FUN, len, id, ...) {inputs <- character(len)
for (i in seq_len(len)) {
  inputs[i] <- as.character(FUN(paste0(id, i), ...))}
inputs
}


shinyServer(function(input,session, output) {

  
# observe({
#     if ("Select All" %in% input$block_parishad) {
#       # choose all the choices _except_ "Select All"
#       selected_choices <- setdiff(choices_block , "Select All")
#       updateSelectInput(session, "block_parishad", selected = selected_choices)
#     }
#   })  
  
  
observeEvent(
      input$block_parishad, {
      # Update based on the year change event
        if ("Select All" %in% input$block_parishad) {
         
          updateSelectInput(session, "gram_panchayat", "Gram Panchayat", 
                        choices = c("Select All",
                                    unique(get_data_df$gram_parishad_name[get_data_df$block_parishad_name %in% setdiff(choices_block, "Select All")]))
                        ,selected = "Select All"
                        )
        } else {
          updateSelectInput(session, "gram_panchayat", "Gram Panchayat", 
                            choices = c("Select All",unique(get_data_df$gram_parishad_name[get_data_df$block_parishad_name %in% input$block_parishad]))
                            ,selected = NULL 
                              
          )
          
        }

})   
  
  
observeEvent(
      input$gram_panchayat,{
      # Update based on the year change event
        if ("Select All" %in% input$gram_panchayat) {
        
            updateSelectInput(session, "gram_sansad", "Gram Sansad", 
                            choices = c("Select All",
                              unique(get_data_df$gram_sansad_name[get_data_df$gram_parishad_name %in% setdiff(choices_panchayat,"Select All")]))
                            ,selected = "Select All"
          )}else{
          
          updateSelectInput(session, "gram_sansad", "Gram Sansad", 
                            choices = c("Select All", unique(get_data_df$gram_sansad_name[get_data_df$gram_parishad_name %in% input$gram_panchayat]))
                            ,selected = NULL
          )}
  
})  
  
observeEvent(input$gram_sansad, {
    # ####print("DUMMYYYYYYYYYYYYYYYYYYYYYY")
    # ####print(input$gram_sansad)
  })
  
  
datasetInput <- reactive({
    
    # validate(
    #   need(nrow(datasetInput())>0), "Please select one or more Block Parishad!"
    #   )

    # ####print( "################333333333##################") 
    # ####print(input$block_parishad)
    # ####print(input$gram_panchayat)
    # ####print(input$gram_sansad)
    
    get_data_df[,8:59] <- sapply(get_data_df[,8:59],as.integer)
    
    # ####print("this is first print")
    # ####print(str(get_data_df))
    
    # print("####################### IN DATASETINPUT ##############################")
    
    if ("Select All" %in% input$block_parishad) {
          selected_block <- setdiff(choices_block , "Select All")
    } else{
      selected_block <- input$block_parishad
    }
    # print(selected_block)
    
    
    if ("Select All" %in% input$gram_panchayat) {
      selected_gp <- setdiff(choices_panchayat , "Select All")
    } else{
      selected_gp <- input$gram_panchayat
    }
    # print(selected_gp)
    
    
    if ("Select All" %in% input$gram_sansad) {
      selected_gs <- setdiff(choices_sansad , "Select All")
    }else{
      selected_gs <- input$gram_sansad
    }
    # print(selected_gs)
    
    
    get_data_df_temp <- get_data_df[get_data_df$block_parishad_name %in% selected_block &
                                 get_data_df$gram_parishad_name %in% selected_gp &
                                 get_data_df$gram_sansad_name %in% selected_gs
                                 ,]
    
    # ####print(get_data_df_temp)
    # PREVIOUS CODE without considering 2018 in 2017-18
    # get_data_df_temp_final <- get_data_df_temp %>%
    #   mutate(year_month_date = as.Date(paste(matrix(unlist(strsplit(get_data_df_temp$year, "-")),ncol=2, byrow = TRUE)[,1],
    #                                          month,"01",sep = "-")))
    get_data_df_temp_final <- get_data_df_temp %>%
      mutate(year_month_date = ifelse( month >3 , paste(matrix(unlist(strsplit(get_data_df$year, "-")),ncol=2, byrow = TRUE)[,1],
                                                        month,"01",sep = "-")
                                       , paste(as.integer(matrix(unlist(strsplit(get_data_df$year, "-")),ncol=2, byrow = TRUE)[,1])+1,
                                               month,"01",sep = "-"))
      )
    
   return(filter(get_data_df_temp_final, 
                  get_data_df_temp_final$year_month_date >= input$monthRange[1],
                  get_data_df_temp_final$year_month_date <= input$monthRange[2]))
  })
  

getIndGS <- reactive({

    # print ("Hello in getIndGS()")
    get_data_df <- datasetInput()

    # ####print(str(get_data_df))

    get_data_df[,8:59] <- sapply(get_data_df[,8:59],as.integer)
    # Indicators_PRI <- indicators_master

    get_data_df_gs_ind <-
      mutate(get_data_df,

    i1 =   round((get_data_df$i1_add/get_data_df$i2_add),digits = 0),
    i2 =   round((get_data_df$i3_add/get_data_df$i1_add)*100  ,digits = 0),
    i3 =   round((  get_data_df$i4_add/get_data_df$i1_add)*100  ,digits = 0) ,
    i4 =   round(get_data_df$i5_add, digits = 0),
    i5 =   round((  get_data_df$i6_add/get_data_df$i5_add)*100  ,digits = 0),
    i6 =   round((  get_data_df$i7_add/get_data_df$i8_add)*100  ,digits = 0),
    i7 =   round((  get_data_df$i9_add/get_data_df$i8_add)*100  ,digits = 0) ,
    i8 =   round((  get_data_df$i10_add/get_data_df$i8_add)*100  ,digits = 0) ,
    i9 =   round((  get_data_df$i11_add/(get_data_df$i8_add-get_data_df$i10_add))*100  ,digits = 0)
    ,i10 = round((  get_data_df$i12_add/get_data_df$i3_add)*100  ,digits = 0)
    ,i11 = round((  get_data_df$i13_add/get_data_df$i1_add)*100  ,digits = 0)
    ,i12 = round((  get_data_df$i14_add/get_data_df$i1_add)*100  ,digits = 0)
    ,i13 = round((  get_data_df$i15_add/get_data_df$i3_add)*100  ,digits = 0)
    ,i14 = round((  get_data_df$i16_add/get_data_df$i4_add)*100  ,digits = 0)
    ,i15 = round(get_data_df$i17_add  ,digits = 0)

    #Child Development
    ,i16 = round((  get_data_df$i18_add/get_data_df$i4_add)*100  ,digits = 0)
    # Indicators_PRI$Value[17] =   round((  get_data_df$i19_add/get_data_df$i8_add)*100  ,digits = 0)
    ,i18 =   round((  get_data_df$i20_add/get_data_df$i1_add)*100  ,digits = 0)
    ,i19 =   round((  get_data_df$i21_add/get_data_df$i4_add)*100  ,digits = 0)
    ,i20 =   round((  get_data_df$i22_add/get_data_df$i4_add)*100  ,digits = 0)
    ,i21 =   round((  get_data_df$i23_add/get_data_df$i22_add)*100  ,digits = 0)

    #Panchayat and Rural Development
    ,i23 =   round((  get_data_df$i25_add/get_data_df$i2_add)*100  ,digits = 0)
    ,i24 =   round((  get_data_df$i26_add/get_data_df$i2_add)*100  ,digits = 0)
    ,i25 =   round((  get_data_df$i27_add/get_data_df$i2_add)*100  ,digits = 0)
    ,i26 =   round((  get_data_df$i28_add/get_data_df$i2_add)*100  ,digits = 0)
    ,i27 =   round((  (get_data_df$i2_add- (get_data_df$i25_add +
                                                                    get_data_df$i26_add +
                                                                    get_data_df$i27_add +
                                                                    get_data_df$i28_add ))/
                                              get_data_df$i2_add)*100  ,digits = 0)

    ,i29 =   round((  get_data_df$i30_add/get_data_df$i2_add)*100  ,digits = 0)
    ,i30 =   round((  get_data_df$i31_add/get_data_df$i2_add)*100  ,digits = 0)
    ,i31 =   round((  get_data_df$i32_add/get_data_df$i2_add)*100  ,digits = 0)
    ,i32 =   round((  (get_data_df$i2_add- (get_data_df$i30_add +
                                                                    get_data_df$i31_add +
                                                                    get_data_df$i32_add
    ))/
      get_data_df$i2_add)*100  ,digits = 0)

    ,i33 =   round((  get_data_df$i33_add/get_data_df$i2_add)*100  ,digits = 0)
    ,i34 =   round((  get_data_df$i34_add/get_data_df$i2_add)*100  ,digits = 0)
    ,i35 =   round((  get_data_df$i35_add/get_data_df$i2_add)*100  ,digits = 0)

    # Indicators_PRI$Value[17] =   round((  get_data_df$i19_add/get_data_df$i8_add)*100  ,digits = 0)
    ,i36 =   round((  get_data_df$i37_add/get_data_df$i36_add)*100  ,digits = 0)
    ,i37 =   round((  get_data_df$i38_add/get_data_df$i36_add)*100  ,digits = 0)

    ,i38 =   round((  get_data_df$i39_add/get_data_df$i2_add)*100  ,digits = 0)
    ,i39 =   round((  get_data_df$i40_add/get_data_df$i2_add)*100  ,digits = 0)
    ,i40 =   round((  get_data_df$i41_add/get_data_df$i2_add)*100  ,digits = 0)
    ,i41 =   round((  get_data_df$i42_add/get_data_df$i2_add)*100  ,digits = 0)
    ,i42 =   round((  get_data_df$i43_add/get_data_df$i2_add)*100  ,digits = 0)
    ,i43 =   round((  (get_data_df$i2_add- (  get_data_df$i39_add +
                                                                      get_data_df$i40_add +
                                                                      get_data_df$i41_add +
                                                                      get_data_df$i42_add +
                                                                      get_data_df$i43_add
    ))/
      get_data_df$i2_add)*100  ,digits = 0)

    # ,denominator_45_to_52 = (get_data_df$i45_add + get_data_df$i46_add + get_data_df$i47_add + get_data_df$i48_add + get_data_df$i49_add
    #                          + get_data_df$i50_add + get_data_df$i51_add + get_data_df$i52_add)

    ,i45 =   round((  get_data_df$i45_add/(get_data_df$i45_add + get_data_df$i46_add + get_data_df$i47_add + get_data_df$i48_add + get_data_df$i49_add
                                                                      + get_data_df$i50_add + get_data_df$i51_add + get_data_df$i52_add))*100  ,digits = 0)
    ,i46 =   round((  get_data_df$i46_add/(get_data_df$i45_add + get_data_df$i46_add + get_data_df$i47_add + get_data_df$i48_add + get_data_df$i49_add
                                                                      + get_data_df$i50_add + get_data_df$i51_add + get_data_df$i52_add))*100  ,digits = 0)
    ,i47 =   round((  get_data_df$i47_add/(get_data_df$i45_add + get_data_df$i46_add + get_data_df$i47_add + get_data_df$i48_add + get_data_df$i49_add
                                                                      + get_data_df$i50_add + get_data_df$i51_add + get_data_df$i52_add))*100  ,digits = 0)
    ,i48 =   round((  get_data_df$i48_add/(get_data_df$i45_add + get_data_df$i46_add + get_data_df$i47_add + get_data_df$i48_add + get_data_df$i49_add
                                                                      + get_data_df$i50_add + get_data_df$i51_add + get_data_df$i52_add))*100  ,digits = 0)
    ,i49 =   round((  get_data_df$i49_add/(get_data_df$i45_add + get_data_df$i46_add + get_data_df$i47_add + get_data_df$i48_add + get_data_df$i49_add
                                                                      + get_data_df$i50_add + get_data_df$i51_add + get_data_df$i52_add))*100  ,digits = 0)
    ,i50 =   round((  get_data_df$i50_add/(get_data_df$i45_add + get_data_df$i46_add + get_data_df$i47_add + get_data_df$i48_add + get_data_df$i49_add
                                                                      + get_data_df$i50_add + get_data_df$i51_add + get_data_df$i52_add))*100  ,digits = 0)
    ,i51 =   round((  get_data_df$i51_add/(get_data_df$i45_add + get_data_df$i46_add + get_data_df$i47_add + get_data_df$i48_add + get_data_df$i49_add
                                                                      + get_data_df$i50_add + get_data_df$i51_add + get_data_df$i52_add))*100  ,digits = 0)
    ,i52 =   round((  get_data_df$i52_add/(get_data_df$i45_add + get_data_df$i46_add + get_data_df$i47_add + get_data_df$i48_add + get_data_df$i49_add
                                                                      + get_data_df$i50_add + get_data_df$i51_add + get_data_df$i52_add))*100  ,digits = 0)

    )


    ####print("Hello I am at end of getIndGS")

    # testdata <- Indicators_PRI

    # print(get_data_df_gs_ind)

    return(get_data_df_gs_ind)


  })
  
  
getIndVal <- reactive({
    
    # print ("Hello in getIndVal()")
    get_data_df <- datasetInput()
    
    # ####print(str(get_data_df))
    
    Indicators_PRI <- indicators_master
    
    Indicators_PRI$Value[1] <- round(mean(get_data_df$i1_add/get_data_df$i2_add, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[2] <- round(mean((get_data_df$i3_add/get_data_df$i1_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[3] <- round(mean((get_data_df$i4_add/get_data_df$i1_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[4] <- round(mean(get_data_df$i5_add, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[5] <- round(mean((get_data_df$i6_add/get_data_df$i5_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[6] <- round(mean((get_data_df$i7_add/get_data_df$i8_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[7] <- round(mean((get_data_df$i9_add/get_data_df$i8_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[8] <- round(mean((get_data_df$i10_add/get_data_df$i8_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[9] <- round(mean((get_data_df$i11_add/(get_data_df$i8_add-get_data_df$i10_add))*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[10] <- round(mean((get_data_df$i12_add/get_data_df$i3_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[11] <- round(mean((get_data_df$i13_add/get_data_df$i1_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[12] <- round(mean((get_data_df$i14_add/get_data_df$i1_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[13] <- round(mean((get_data_df$i15_add/get_data_df$i3_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[14] <- round(mean((get_data_df$i16_add/get_data_df$i4_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[15] <- round(sum(get_data_df$i17_add, na.rm = TRUE),digits = 0) 
    
    #Child Development
    Indicators_PRI$Value[16] <- round(mean((get_data_df$i18_add/get_data_df$i4_add)*100, na.rm = TRUE),digits = 0)
    # Indicators_PRI$Value[17] <- round(mean((get_data_df$i19_add/get_data_df$i8_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[18] <- round(mean((get_data_df$i20_add/get_data_df$i1_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[19] <- round(mean((get_data_df$i21_add/get_data_df$i4_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[20] <- round(mean((get_data_df$i22_add/get_data_df$i4_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[21] <- round(mean((get_data_df$i23_add/get_data_df$i22_add)*100, na.rm = TRUE),digits = 0) 
    
    #Panchayat and Rural Development
    Indicators_PRI$Value[23] <- round(mean((get_data_df$i25_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[24] <- round(mean((get_data_df$i26_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[25] <- round(mean((get_data_df$i27_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[26] <- round(mean((get_data_df$i28_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[27] <- round(mean(((get_data_df$i2_add- (get_data_df$i25_add + 
                                                                    get_data_df$i26_add + 
                                                                    get_data_df$i27_add + 
                                                                    get_data_df$i28_add ))/
                                              get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    
    Indicators_PRI$Value[29] <- round(mean((get_data_df$i30_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[30] <- round(mean((get_data_df$i31_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[31] <- round(mean((get_data_df$i32_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[32] <- round(mean(((get_data_df$i2_add- (get_data_df$i30_add + 
                                                                    get_data_df$i31_add + 
                                                                    get_data_df$i32_add 
    ))/
      get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    
    Indicators_PRI$Value[33] <- round(mean((get_data_df$i33_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[34] <- round(mean((get_data_df$i34_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[35] <- round(mean((get_data_df$i35_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    
    # Indicators_PRI$Value[17] <- round(mean((get_data_df$i19_add/get_data_df$i8_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[36] <- round(mean((get_data_df$i37_add/get_data_df$i36_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[37] <- round(mean((get_data_df$i38_add/get_data_df$i36_add)*100, na.rm = TRUE),digits = 0)
    
    Indicators_PRI$Value[38] <- round(mean((get_data_df$i39_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[39] <- round(mean((get_data_df$i40_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[40] <- round(mean((get_data_df$i41_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[41] <- round(mean((get_data_df$i42_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[42] <- round(mean((get_data_df$i43_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[43] <- round(mean(((get_data_df$i2_add- (  get_data_df$i39_add + 
                                                                      get_data_df$i40_add + 
                                                                      get_data_df$i41_add +
                                                                      get_data_df$i42_add + 
                                                                      get_data_df$i43_add 
    ))/
      get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    
    denominator_45_to_52 <- (get_data_df$i45_add + get_data_df$i46_add + get_data_df$i47_add + get_data_df$i48_add + get_data_df$i49_add 
                             + get_data_df$i50_add + get_data_df$i51_add + get_data_df$i52_add)
    
    Indicators_PRI$Value[45] <- round(mean((get_data_df$i45_add/denominator_45_to_52)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[46] <- round(mean((get_data_df$i46_add/denominator_45_to_52)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[47] <- round(mean((get_data_df$i47_add/denominator_45_to_52)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[48] <- round(mean((get_data_df$i48_add/denominator_45_to_52)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[49] <- round(mean((get_data_df$i49_add/denominator_45_to_52)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[50] <- round(mean((get_data_df$i50_add/denominator_45_to_52)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[51] <- round(mean((get_data_df$i51_add/denominator_45_to_52)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[52] <- round(mean((get_data_df$i52_add/denominator_45_to_52)*100, na.rm = TRUE),digits = 0)
    
    # ####print(Indicators_PRI)
    # return(Indicators_PRI)
    
    # ####print("Hello I am at end of getIndVal()")
    
    testdata <- Indicators_PRI 
    
    # ####print(head(testdata))
    
    df_with_button <- 
    #   as.data.frame
    # (
      cbind(
      View = shinyInput(actionButton, nrow(testdata),'button_', label = "View", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' ),
      testdata)
      # )
    
    # df_with_button[23:27,1] <- ""
    # df_with_button[29:32,1] <- ""
    # df_with_button[38:43,1] <- ""
    # df_with_button[45:52,1] <- ""
    
    
    ####print(str(df_with_button))
    
    return(df_with_button)
    
    
})  
  
  
  
  
############################################ TABLE START ######################################  
sketch1 <- htmltools::withTags(table(
    class = "display",
    style = "bootstrap",
    tableHeader(c(
      # "Graph", 
      "ID","Indicator","Unit","Value"))
    # tableFooter(c("", c("","","","")))
  ))
  
opts1 <- list( 
    # footerCallback = JS(
    #   "function( tfoot, data, start, end, display ) {",
    #   "var api = this.api();",
    #   "$( api.column( 3 ).footer() ).html(",
    #   "api.column( 3 ).data().reduce( function ( a, b ) {",
    #   "return a + b;",
    #   "} )",  # remove ; here
    #   ");",
    #   "$( api.column( 2 ).footer() ).html(",
    #   "api.column( 2 ).data().reduce( function ( a, b ) {",
    #   "return a + b;",
    #   "} )",  # remove ; here
    #   ");",
    #   
    #   "}"),
    
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}"),
    searchHighlight = TRUE,
    dom = 'lfrtiBp',
    lengthMenu = list(c(50, 100, -1), c('50', '100', 'All')),
    buttons = 
      list ('print',list(
        extend = 'collection',
        buttons = c('csv','excel', 'pdf'),
        text = 'Download'
      ))
    # pagingType = "full"
    
    
  )
  
  
  
output$health_family_welfare = DT::renderDataTable( 
    
    # ####print("Data set input")
    # 
    # ####print(datasetInput())
    
    DT::datatable(getIndVal()
                   
                  %>% filter(Category == "Health and Family Welfare") 
                  %>% 
                  select(
                    # View,
                    S_No,Indicator,Unit,Value)
                 , container = sketch1, options = opts1, selection = 'single',
                  class = 'cell-border stripe',
                  caption = 'Table 1: Health and Family Welfare Indicators.',
                  extensions = 'Buttons',escape = FALSE,rownames= FALSE
                  
                  # filter = 'top'
    ) %>%
    formatStyle('Value',  color = 'black', backgroundColor = '#fffdd5', fontWeight = 'bold',`font-size` = '18px') %>%
      formatStyle('Indicator',  color = 'black', `font-size` = '14px')
    
    )
  
  # Here I created a reactive to save which row was clicked which can be stored for further analysis
SelectedRow <- eventReactive(input$select_button,{
    as.numeric(strsplit(input$select_button, "_")[[1]][2])
  })
# 
#   # This is needed so that the button is clicked once for modal to show, a bug reported here
#   # https://github.com/ebailey78/shinyBS/issues/57
observeEvent(input$select_button, {
 
  #change in global
  # token <- readRDS("./Data/droptoken.rds")
  # get_data_df <- drop_read_csv("CPHD/get_data_df.csv", dtoken = token)
  # get_data_df %>% mutate_if(is.factor, as.character) -> get_data_df
  # unique(get_data_df$gram_sansad_name)
  
  
  set_config( config( ssl_verifypeer = 0L ))
  get_data <- GET("https://spreadcreativity.org/master_pri/master_pri_data.php")
  get_data_text <- content(get_data, "text")
  get_data_json <- fromJSON(get_data_text, flatten = TRUE)
  get_data_df_new <- as.data.frame(get_data_json[["indicatordata"]])
  get_data_df_new %>% mutate_if(is.factor, as.character) -> get_data_df
  
  # tryCatch({
  # 
  # if (nrow(get_data_df_new) > nrow(get_data_df))
  # {
  #   # print("hello I am here1")
  #   tryCatch({
  #   drop_delete("get_data_df.csv", path = "CPHD", dtoken = token)
  #   }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  #   # write.csv(data, fileName, row.names = TRUE, quote = TRUE)
  # 
  # write.csv(get_data_df_new, "get_data_df.csv")
  #   drop_upload("get_data_df.csv", path = "CPHD", dtoken = token)
  
  get_data_df <- get_data_df_new
  
  #   print("hello I am here2")  
  # }
  
  # }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
  
  
  # get_data_df <- read.csv("./Data/get_data_df.csv", 
  #                        header = TRUE, stringsAsFactors = FALSE ,check.names = TRUE)
  indicators_master <- read.csv("./Data/Indicators PRI.csv", 
                                header = TRUE, stringsAsFactors = FALSE ,check.names = TRUE)
  
  choices_block <- c(unique(get_data_df$block_parishad_name))
  choices_panchayat <- c("Select All", unique(get_data_df$gram_parishad_name))
  choices_sansad <- c("Select All", unique(get_data_df$gram_sansad_name))
  
  get_data_df$month <- as.integer(get_data_df$month)
  
  
  get_data_df_final <- get_data_df %>% 
    mutate(year_month_date = ifelse( month>3 , paste(matrix(unlist(strsplit(get_data_df$year, "-")),ncol=2, byrow = TRUE)[,1],
                                                     month,"01",sep = "-")
                                     , paste(as.integer(matrix(unlist(strsplit(get_data_df$year, "-")),ncol=2, byrow = TRUE)[,1])+1,
                                             month,"01",sep = "-"))
    )
  
  series_list_gp <<- vector("list",0)
  
     ####print("in BUTTON")
     df_ind <- getIndGS()
     ####print("selectrow index number")
     ####print(SelectedRow())
     
     # str(df_ind)
     # df1 <- df_ind
     # head(df_ind)
     # block_df_ind <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("block", "ind_value"))
     # block_df_ind$block <-  sort(unique(getIndGS()[]$block_parishad_name))
     # ind_name <- paste("i",SelectedRow(),sep = "")
     # ####print("INDICATOR NAME")
     # ####print(ind_name)
     # ####print(str(df_ind))
     # # ####print(class(df_ind$paste("i",SelectedRow(),sep = "")))
     # by_block <- df_ind %>% group_by_(.dots = list('block_parishad_name')) 
     #                        %>% summarise_(.dots = setNames(mean(ind_name, na.rm = TRUE), block_ind_val))    
     # ####print(head(by_block))  
     # ####print(by_block)
     
     ind_name <- paste("mean","(","i",SelectedRow(),",","na.rm=TRUE",")",sep = "")
     
     ind_name_gs <- paste0("i",SelectedRow())
     summ_name <- paste0('mean_', "i",SelectedRow())
     
     # FOR BLOCK LEVEL GRAPH
     by_block <- 
       df_ind %>% group_by(block_parishad_name)%>% 
       summarise_(.dots = setNames(ind_name, summ_name))
     by_block <- by_block[complete.cases(by_block), ]
     # ####print("by_BLOBCKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKK")
     # ####print(by_block)
     colnames(by_block)<- c("name","y")
     by_block$y <- as.integer(by_block$y )
     
     by_block$drilldown <- tolower(paste(substr(by_block$name,1,4),'id',sep = "_"))
     block_df_ind <<- as.data.frame(by_block)
     ####print("BLOCK DF")
     ####print(str(block_df_ind))
     
    # FOR GP LEVEL GRAPH
     by_gp <- 
       df_ind %>% group_by(block_parishad_name, gram_parishad_name)%>% 
       summarise_(.dots = setNames(ind_name, summ_name))
     
     by_gp <- by_gp[complete.cases(by_gp), ]
     colnames(by_gp)<- c("block_parishad_name","gram_parishad_name","value")
     by_gp$drilldown <- tolower(paste(substr(by_gp$gram_parishad_name,1,4),'id',sep = "_")) 
     by_gp$block <- tolower(paste(substr(by_gp$block_parishad_name,1,4),'id',sep = "_"))
     gp_df_ind <<- as.data.frame(by_gp)
     
     ####print("GP DF")
     ####print(str(gp_df_ind))
     
     
     ####print("#################################################")
     # ####print(str(block_df_ind))
     # ####print(str(gp_df_ind))
     # ####print(str(series_list_gp))
     # ####print(filter(gp_df_ind, block == block_df_ind$drilldown[1])$gram_parishad_name)
     # ####print(filter(gp_df_ind, block == block_df_ind$drilldown[1])$value)
     
    for (i in 1:nrow(block_df_ind))
       {
        # series_list_gp[[i]] <- "Hello"
        # ####print(series_list_gp)
        
        series_list_gp[[i]] <- list( id = block_df_ind$drilldown[i],
                                         name = "Gram Panchayat",
                                         data = list_parse
                                         (data_frame(
                                           name = filter(gp_df_ind, block == block_df_ind$drilldown[i])$gram_parishad_name,
                                           y = filter(gp_df_ind, block == block_df_ind$drilldown[i])$value ),
                                           drilldown = tolower(paste(substr(by_gp$gram_parishad_name,1,4),'id',sep = "_"))
                                         )
        )
     }
    
    series_list_gp <<- series_list_gp
    ####print("OURSIDE SCOPE")
    # ####print(series_list_gp)
    
    
###################### FOR GS DRILL DOWN
    
 block_parishad <- tolower(paste(substr(df_ind$gram_parishad_name,1,4),'id',sep = "_")) 
    
   # ####print("FILTER COMMAND RESULT")
   # ####print(block_parishad)
   # ####print(gp_df_ind$drilldown[1])
   # ####print(colnames(df_ind))
   # ####print(ind_name_gs)
   # ####print(which(colnames(df_ind)== ind_name_gs))
   # ####print(filter(df_ind, block_parishad == gp_df_ind$drilldown[1])$gram_parishad_name)
   # ####print(filter(df_ind, block_parishad == gp_df_ind$drilldown[1])[,which(colnames(df_ind)== ind_name_gs)])
    
   for (i in 1:nrow(gp_df_ind))
     {
       # series_list_gp[[i]] <- "Hello"
       # ####print(series_list_gp)
   
       series_list_gs[[i]] <- list( id = gp_df_ind$drilldown[i],
                                    # name = "Gram Sansad",
                                    data = list_parse
                                    (data_frame(
                                      name = filter(df_ind, block_parishad == gp_df_ind$drilldown[i])$gram_parishad_name,
                                      y = filter(df_ind, block_parishad == gp_df_ind$drilldown[i])[,which(colnames(df_ind)== ind_name_gs)] 
                                      )
                                    )
       )
     }
   
   series_list_gs <<- series_list_gs
   ####print("OURSIDE SCOPE GP LIST")
   # ####print(series_list_gp)
   ####print(input$tabs)
   
###########################################################
   

   
   
      
   
   
   
   
   
   
   
   
###########################################################   
   
   # if (input$tabs == 1)
   # { 
   toggleModal(session, "modalExample", "open")
    # }
   # else if (input$tabs == 2)
   # {
   #   toggleModal(session, "modalExample1", "open")
   # }
   # else if (input$tabs == 3)
   # {
   #   toggleModal(session, "modalExample2", "open")
   # }
   
   # toggleModal(session, "modalExample1", "open")

 })
   
output$popup <- renderUI({
       bsModal("modalExample", indicators_master$Indicator[SelectedRow()], "", size = "large",
               highchartOutput("indicators", height = "400px"))
      
  })

output$popup1 <- renderUI({
  bsModal("modalExample1", indicators_master$Indicator[SelectedRow()], "", size = "large",
          highchartOutput("indicators1", height = "400px"))
  
})

output$popup2 <- renderUI({
  bsModal("modalExample2", indicators_master$Indicator[SelectedRow()], "", size = "large",
          highchartOutput("indicators2", height = "400px"))

})

output$indicators <- renderHighchart ({
  
  ####print("in SERVER function")
  ####print(str(block_df_ind))
  
  # canvasClickFunction <- JS("function(event) {Shiny.onInputChange('canvasClicked', [this.name, event.point.x]);}")
  
                 highchart() %>%
                     hc_chart(type = "column"
                            # ,
                            # events = list(
                            #   drilldown =
                            #     JS('function(e) {
                            # console.log(e.seriesOptions);
                            # this.setTitle({text: e.seriesOptions.name});
                            # } '))
                     ) %>%
                 
                  # hc_title(text = "High Risk Cases - NCDs") %>%
                  # hc_title(text = "<b>High Risk Cases</b> - NCDs",
                 #          # # margin = 20,
                 #          # align = "centre",
                 #          style = list(color = "#05008b", useHTML = TRUE)) %>%
   
                 # hc_title(text = span( "High Risk Cases - NCDs", style="color:#e32c3e")
                 #           # style = list(fontWeight = "bold")
                 #          ) %>%
   
   
  
                   hc_xAxis(type = "category",
                            title = list(text = "Geography Name")
                            # JS('function(e) {chart.options.yAxis[0].title.text = "new title";  }')
   
                   ) %>%
  
                 hc_yAxis(title = list(text = "Indicator Value")) %>%
                 hc_legend(enabled = FALSE) %>%
                   hc_plotOptions(
                     series = list(
                       boderWidth = 0,
                     dataLabels = list(enabled = TRUE)
                     # ,
                       # colors = scale_color_brewer( ,type = "seq", palette = 1, direction = 1)
                       # colors = rev(colorRampPalette(brewer.pal(9,"Blues"))(nrow(bar_ncd_high_risk))),
                      # events = list(click = canvasClickFunction)
                     )) %>%
   
                  hc_add_series(
                     name = "Block Wise Values",
                     data = list_parse(block_df_ind),
                     colorByPoint = TRUE
                   ) %>%
  
    
  
                    
                  hc_drilldown(
                      allowPointDrilldown = TRUE,
                      series = series_list_gp,
                      colorByPoint = TRUE
                    )
                    
  
  
  
  
  
})
  
output$indicators1 <- renderHighchart ({
  
  ####print("in SERVER function")
  # ####print(series_list_gp)
  
  # canvasClickFunction <- JS("function(event) {Shiny.onInputChange('canvasClicked', [this.name, event.point.x]);}")
  
  highchart() %>%
    hc_chart(type = "column"
             # ,
             # events = list(
             #   drilldown =
             #     JS('function(e) {
             # console.log(e.seriesOptions);
             # this.setTitle({text: e.seriesOptions.name});
             # } '))
    ) %>%
    
    # hc_title(text = "High Risk Cases - NCDs") %>%
    # hc_title(text = "<b>High Risk Cases</b> - NCDs",
    #          # # margin = 20,
    #          # align = "centre",
    #          style = list(color = "#05008b", useHTML = TRUE)) %>%
    
    # hc_title(text = span( "High Risk Cases - NCDs", style="color:#e32c3e")
    #           # style = list(fontWeight = "bold")
    #          ) %>%
    
  
  
  hc_xAxis(type = "category",
           title = list(text = "Geography Name")
           # JS('function(e) {chart.options.yAxis[0].title.text = "new title";  }')
           
  ) %>%
    
    hc_yAxis(title = list(text = "Indicator Value")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE)
        # ,
        # colors = scale_color_brewer( ,type = "seq", palette = 1, direction = 1)
        # colors = rev(colorRampPalette(brewer.pal(9,"Blues"))(nrow(bar_ncd_high_risk))),
        # events = list(click = canvasClickFunction)
      )) %>%
    
    hc_add_series(
      name = "Block Wise Values",
      data = list_parse(block_df_ind),
      colorByPoint = TRUE
    ) %>%
    
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = series_list_gp,
      colorByPoint = TRUE
    )
  
})

output$indicators2 <- renderHighchart ({
  
  ####print("in SERVER function")
  if (SelectedRow() == 22)
  {
      
    
      hchart(call_status,type = "pie", name = "No. of callers")%>% 
      hc_add_theme(hc_theme_smpl())%>%
      hc_legend(enabled = FALSE) %>%
      hc_title(text = paste0("Call Status (all Calls)",span(" (","N=",format(nrow(sci_dump_temp), nsmall=0, big.mark=","),")", style="color:#e32c3e")),
               style = list(fontWeight = "bold")) %>%
      
      hc_plotOptions( pie = list(colors = brewer.pal(6,"Set3"),
                                 # type = "pie",
                                 name = "No. of Calls", 
                                 colorByPoint = TRUE,
                                 # center = c('55%', '50%'),
                                 size = 170, 
                                 dataLabels = list(enabled = TRUE,
                                                   format = '{point.name}: ({point.percentage:.1f}%)'
                                                   # {point.percentage:.1f}
                                 ))) %>% 
      hc_credits(enabled = TRUE,
                 text = "Source: SCI SDI Helpline data,2017-18",
                 href = "https://www.savethechildren.in/",
                 style = list(fontSize = "9px")) %>%
      hc_exporting(enabled = TRUE)
    
    
    
  }
  
    
    
    
  else{
    
    highchart() %>%
    hc_chart(type = "column"
             # ,
             # events = list(
             #   drilldown =
             #     JS('function(e) {
             # console.log(e.seriesOptions);
             # this.setTitle({text: e.seriesOptions.name});
             # } '))
    ) %>%
    
    # hc_title(text = "High Risk Cases - NCDs") %>%
    # hc_title(text = "<b>High Risk Cases</b> - NCDs",
    #          # # margin = 20,
    #          # align = "centre",
    #          style = list(color = "#05008b", useHTML = TRUE)) %>%
    
    # hc_title(text = span( "High Risk Cases - NCDs", style="color:#e32c3e")
    #           # style = list(fontWeight = "bold")
    #          ) %>%
    
  
  
  hc_xAxis(type = "category",
           title = list(text = "Geography Name")
           # JS('function(e) {chart.options.yAxis[0].title.text = "new title";  }')
           
  ) %>%
    
    hc_yAxis(title = list(text = "Indicator Value")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE)
        # ,
        # colors = scale_color_brewer( ,type = "seq", palette = 1, direction = 1)
        # colors = rev(colorRampPalette(brewer.pal(9,"Blues"))(nrow(bar_ncd_high_risk))),
        # events = list(click = canvasClickFunction)
      )) %>%
    
    hc_add_series(
      name = "Block Wise Values",
      data = list_parse(block_df_ind),
      colorByPoint = TRUE
    ) %>%
    
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = series_list_gp,
      colorByPoint = TRUE
    )
  }
  
  
  
})

output$child_development = DT::renderDataTable( 
    
    # ####print("Data set input")
    # 
    # ####print(datasetInput())
   
    DT::datatable(getIndVal() %>% filter(Category == "Child Development") %>% 
                    select(
                      # View,
                      S_No,Indicator,Unit,Value) 
                  , container = sketch1, options = opts1, selection = 'single',
                  class = 'cell-border stripe',
                  caption = 'Table 2: Child Development Indicators.',
                  extensions = 'Buttons',escape = FALSE,rownames= FALSE
                  
                  # filter = 'top'
    )
    %>%
      formatStyle('Value',  color = 'black', backgroundColor = '#fffdd5', fontWeight = 'bold',`font-size` = '18px') %>%
      formatStyle('Indicator',  color = 'black', `font-size` = '14px')
    
    )
  
  output$panchayat_rural_development = DT::renderDataTable( 
    
    # ####print("Data set input")
    # 
    # ####print(datasetInput())
    
    DT::datatable(getIndVal() %>% filter(Category == "Panchayat and Rural Development") %>% 
                    select(
                      # View,
                      S_No,Indicator,Unit,Value) 
                  , container = sketch1, options = opts1, selection = 'single',
                  class = 'cell-border stripe',
                  caption = 'Table 3: Panchayat and Rural Development Indicators.',
                  extensions = 'Buttons',escape = FALSE,rownames= FALSE
                  
                  # filter = 'top'
    )
    %>%
      formatStyle('Value', color = 'black', backgroundColor = '#fffdd5', fontWeight = 'bold',`font-size` = '18px') %>%
      formatStyle('Indicator',  color = 'black', `font-size` = '14px') 
  
  )
  

  
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
  
output$under18Preg <- renderHighchart({
  
  ####print("in PREG CHART")
  df_ind <- getIndGS()
  
  ind_name <- paste("mean","(","i",5,",","na.rm=TRUE",")",sep = "")
  
  ind_name_gs <- paste0("i",5)
  summ_name <- paste0('mean_', "i",5)
  
  # NOTE CHANGE
  # df_ind <- df_ind[,which(colnames(df_ind)== ind_name_gs)]
  # print(df_ind)
  # df_ind <- df_ind[complete.cases(df_ind), ]
  # print(df_ind)
  df_ind$gram_parishad <- tolower(paste(substr(df_ind$gram_parishad_name,1,4),'id',sep = "_")) 
  print(df_ind$gram_parishad)
  # NOTE CHANGE ABOVE
  
  # FOR BLOCK LEVEL GRAPH
  by_block <- 
    df_ind %>% group_by(block_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  by_block <- by_block[complete.cases(by_block), ]
  # ####print("by_BLOBCKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKK")
  # ####print(by_block)
  colnames(by_block)<- c("name","y")
  by_block$y <- as.integer(by_block$y )
  
  by_block$drilldown <- tolower(paste(substr(by_block$name,1,4),'id',sep = "_"))
  block_df_ind <- as.data.frame(by_block)
  
   print("BLOCK DF")
   print(str(block_df_ind))
  
  # FOR GP LEVEL GRAPH
  by_gp <- 
    df_ind %>% group_by(block_parishad_name, gram_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  by_gp <- by_gp[complete.cases(by_gp), ]
  
  # print(str(by_gp))
  
  colnames(by_gp)<- c("block_parishad_name","gram_parishad_name","value")
  
  # print(str(tolower(paste(substr(by_gp$gram_parishad_name,1,4),'id',sep = "_")) ))
  
  by_gp$drilldown <- tolower(paste(substr(by_gp$gram_parishad_name,1,4),'id',sep = "_")) 
  by_gp$block <- tolower(paste(substr(by_gp$block_parishad_name,1,4),'id',sep = "_"))
  by_gp$gp <- tolower(paste(substr(by_gp$gram_parishad_name,1,4),'id',sep = "_"))
  gp_df_ind <- as.data.frame(by_gp)
  
 # CHANGE ADDITION 
  
  # CHANGE ADDITION   
  
  # print("GP DF")
  # print(str(gp_df_ind))
  
  for (i in 1:nrow(block_df_ind))
  {
    # series_list_gp[[i]] <- "Hello"
    # ####print(series_list_gp)
    
    series_list_gp[[i]] <- list( id = block_df_ind$drilldown[i],
                                 name = "Gram Panchayat",
                                 data = list_parse
                                 (data_frame(
                                   name = filter(gp_df_ind, block == block_df_ind$drilldown[i])$gram_parishad_name,
                                   y = round(filter(gp_df_ind, block == block_df_ind$drilldown[i])$value,digits = 0),
                                 # )
                                   drilldown = filter(gp_df_ind, block == block_df_ind$drilldown[i])$gp)  
                                 )
    )
    
  }
  # print("SSSSSSSSSSSSSSSSSSSSS series_list_gp SSSSSSSSSSSSSSSSSSSSSS")
  # print(series_list_gp)
  
 for (i in 1:nrow(gp_df_ind))
  {
  series_list_gs[[i]] <- list( id = gp_df_ind$gp[i],
                               # name = "Gram Sansad",
                               data = list_parse
                               (data_frame(
                                 name = filter(df_ind, gram_parishad == gp_df_ind$gp[i])$gram_sansad_name,
                                 y = filter(df_ind, gram_parishad == gp_df_ind$gp[i])[,which(colnames(df_ind)== ind_name_gs)]

                                 )
                               )
  )
 }
  # print("SSSSSSSSSSSSSSSSSSSSS series_list_gs SSSSSSSSSSSSSSSSSSSSSS")
  # print(series_list_gs)
  
  
  
  series_list_gp <- series_list_gp  
  
  
  highchart() %>%
    hc_chart(type = "column"
             # ,color = "#5e81fd"
             # ,
             # events = list(
             #   drilldown =
             #     JS('function(e) {
             # console.log(e.seriesOptions);
             # this.setTitle({text: e.seriesOptions.name});
             # } '))
    ) %>%
    
    # hc_title(text = "Under-18 Pregnancies (%)") %>%
    # hc_title(text = "<b>High Risk Cases</b> - NCDs",
    #          # # margin = 20,
    #          # align = "centre",
    #          style = list(color = "#05008b", useHTML = TRUE)) %>%
    
    # hc_title(text = span( "High Risk Cases - NCDs", style="color:#e32c3e")
    #           # style = list(fontWeight = "bold")
    #          ) %>%
    
  
  
  hc_xAxis(type = "category",
           title = list(text = "Panchayat Samiti/Gram Panchayat/Gram Sansad")
           # JS('function(e) {chart.options.yAxis[0].title.text = "new title";  }')
           
  ) %>%
    
    hc_yAxis(title = list(text = "Percentage")
             # ,
             # plotLines = list(
             #   list(label = list(text = paste0("District Average"," = ",round(mean(block_df_ind$y),digits = 0))),
             #        color = "#00b300",
             #        width = 1.0,
             #        value = mean(block_df_ind$y)))
             ) %>%
    hc_legend(enabled = FALSE) %>%
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE) 
         # ,
         # colors = scale_color_brewer( ,type = "seq", palette = 1, direction = 1),
         # colors = rev(colorRampPalette(brewer.pal(1,"Blues"))(nrow(block_df_ind)))
        # color = "#5e81fd"
        # colors= c("#FF0000")
        # events = list(click = canvasClickFunction)
      )) %>%
    
    hc_add_series(
      name = "Panchayat Samiti",
      data = list_parse(block_df_ind),
      # colorByPoint = TRUE,
      color = "#2171b5"
      
    ) %>%
    
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = c(series_list_gp,series_list_gs),
      colorByPoint = TRUE
    )
  
 
})  
  

output$homeDelivery <- renderHighchart({
    
   
  ####print("in PREG CHART")
  df_ind <- getIndGS()
  
  ind_name <- paste("mean","(","i",7,",","na.rm=TRUE",")",sep = "")
  
  ind_name_gs <- paste0("i",7)
  summ_name <- paste0('mean_', "i",7)
  
  # NOTE CHANGE
  # df_ind <- df_ind[,which(colnames(df_ind)== ind_name_gs)]
  # print(df_ind)
  # df_ind <- df_ind[complete.cases(df_ind), ]
  # print(df_ind)
  df_ind$gram_parishad <- tolower(paste(substr(df_ind$gram_parishad_name,1,4),'id',sep = "_")) 
  print(df_ind$gram_parishad)
  # NOTE CHANGE ABOVE
  
  # FOR BLOCK LEVEL GRAPH
  by_block <- 
    df_ind %>% group_by(block_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  by_block <- by_block[complete.cases(by_block), ]
  # ####print("by_BLOBCKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKK")
  # ####print(by_block)
  colnames(by_block)<- c("name","y")
  by_block$y <- as.integer(by_block$y )
  
  by_block$drilldown <- tolower(paste(substr(by_block$name,1,4),'id',sep = "_"))
  block_df_ind <- as.data.frame(by_block)
  
  print("BLOCK DF")
  print(str(block_df_ind))
  
  # FOR GP LEVEL GRAPH
  by_gp <- 
    df_ind %>% group_by(block_parishad_name, gram_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  by_gp <- by_gp[complete.cases(by_gp), ]
  
  # print(str(by_gp))
  
  colnames(by_gp)<- c("block_parishad_name","gram_parishad_name","value")
  
  # print(str(tolower(paste(substr(by_gp$gram_parishad_name,1,4),'id',sep = "_")) ))
  
  by_gp$drilldown <- tolower(paste(substr(by_gp$gram_parishad_name,1,4),'id',sep = "_")) 
  by_gp$block <- tolower(paste(substr(by_gp$block_parishad_name,1,4),'id',sep = "_"))
  by_gp$gp <- tolower(paste(substr(by_gp$gram_parishad_name,1,4),'id',sep = "_"))
  gp_df_ind <- as.data.frame(by_gp)
  
  # CHANGE ADDITION 
  
  # CHANGE ADDITION   
  
  # print("GP DF")
  # print(str(gp_df_ind))
  
  for (i in 1:nrow(block_df_ind))
  {
    # series_list_gp[[i]] <- "Hello"
    # ####print(series_list_gp)
    
    series_list_gp[[i]] <- list( id = block_df_ind$drilldown[i],
                                 name = "Gram Panchayat",
                                 data = list_parse
                                 (data_frame(
                                   name = filter(gp_df_ind, block == block_df_ind$drilldown[i])$gram_parishad_name,
                                   y = round(filter(gp_df_ind, block == block_df_ind$drilldown[i])$value,digits = 0),
                                   # )
                                   drilldown = filter(gp_df_ind, block == block_df_ind$drilldown[i])$gp)  
                                 )
    )
    
  }
  # print("SSSSSSSSSSSSSSSSSSSSS series_list_gp SSSSSSSSSSSSSSSSSSSSSS")
  # print(series_list_gp)
  
  for (i in 1:nrow(gp_df_ind))
  {
    series_list_gs[[i]] <- list( id = gp_df_ind$gp[i],
                                 # name = "Gram Sansad",
                                 data = list_parse
                                 (data_frame(
                                   name = filter(df_ind, gram_parishad == gp_df_ind$gp[i])$gram_sansad_name,
                                   y = filter(df_ind, gram_parishad == gp_df_ind$gp[i])[,which(colnames(df_ind)== ind_name_gs)]
                                   
                                 )
                                 )
    )
  }
  # print("SSSSSSSSSSSSSSSSSSSSS series_list_gs SSSSSSSSSSSSSSSSSSSSSS")
  # print(series_list_gs)
  
  
  
  series_list_gp <- series_list_gp  
    
    highchart() %>%
      hc_chart(type = "column"
               # ,
               # events = list(
               #   drilldown =
               #     JS('function(e) {
               # console.log(e.seriesOptions);
               # this.setTitle({text: e.seriesOptions.name});
               # } '))
      ) %>%
      
      # hc_title(text = "Home Deliveries of Total Deliveries (%)") %>%
      # hc_title(text = "<b>High Risk Cases</b> - NCDs",
      #          # # margin = 20,
      #          # align = "centre",
      #          style = list(color = "#05008b", useHTML = TRUE)) %>%
      
      # hc_title(text = span( "High Risk Cases - NCDs", style="color:#e32c3e")
      #           # style = list(fontWeight = "bold")
      #          ) %>%
      
      
      
    hc_xAxis(type = "category",
             title = list(text = "Panchayat Samiti/Gram Panchayat/Gram Sansad")
             # JS('function(e) {chart.options.yAxis[0].title.text = "new title";  }')
             
    ) %>%
      
      hc_yAxis(title = list(text = "Percentage")
               # ,
               # plotLines = list(
               #   list(label = list(text = paste0("District Average"," = ",round(mean(block_df_ind$y),digits = 0))),
               #        color = "#00b300",
               #        width = 1.0,
               #        value = mean(block_df_ind$y)))
      ) %>%
      hc_legend(enabled = FALSE) %>%
      hc_plotOptions(
        series = list(
          boderWidth = 0,
          dataLabels = list(enabled = TRUE) 
          # ,
          # # colors = scale_color_brewer( ,type = "seq", palette = 1, direction = 1),
          # colors = rev(colorRampPalette(brewer.pal(9,"BuPu"))(nrow(block_df_ind)))
          # events = list(click = canvasClickFunction)
        )) %>%
      
      hc_add_series(
        name = "Panchayat Samiti",
        data = list_parse(block_df_ind),
        # colorByPoint = TRUE
        color = "#2171b5"
      ) %>%
      
      hc_drilldown(
        allowPointDrilldown = TRUE,
        series = c(series_list_gp,series_list_gs),
        colorByPoint = TRUE
      )
    
    
  })      
  

output$completeImmunization <- renderHighchart({

  
  ####print("in PREG CHART")
  df_ind <- getIndGS()
  
  ind_name <- paste("mean","(","i",10,",","na.rm=TRUE",")",sep = "")
  
  ind_name_gs <- paste0("i",10)
  summ_name <- paste0('mean_', "i",10)
  
  # NOTE CHANGE
  # df_ind <- df_ind[,which(colnames(df_ind)== ind_name_gs)]
  # print(df_ind)
  # df_ind <- df_ind[complete.cases(df_ind), ]
  # print(df_ind)
  df_ind$gram_parishad <- tolower(paste(substr(df_ind$gram_parishad_name,1,4),'id',sep = "_")) 
  print(df_ind$gram_parishad)
  # NOTE CHANGE ABOVE
  
  # FOR BLOCK LEVEL GRAPH
  by_block <- 
    df_ind %>% group_by(block_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  by_block <- by_block[complete.cases(by_block), ]
  # ####print("by_BLOBCKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKK")
  # ####print(by_block)
  colnames(by_block)<- c("name","y")
  by_block$y <- as.integer(by_block$y )
  
  by_block$drilldown <- tolower(paste(substr(by_block$name,1,4),'id',sep = "_"))
  block_df_ind <- as.data.frame(by_block)
  
  print("BLOCK DF")
  print(str(block_df_ind))
  
  # FOR GP LEVEL GRAPH
  by_gp <- 
    df_ind %>% group_by(block_parishad_name, gram_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  by_gp <- by_gp[complete.cases(by_gp), ]
  
  # print(str(by_gp))
  
  colnames(by_gp)<- c("block_parishad_name","gram_parishad_name","value")
  
  # print(str(tolower(paste(substr(by_gp$gram_parishad_name,1,4),'id',sep = "_")) ))
  
  by_gp$drilldown <- tolower(paste(substr(by_gp$gram_parishad_name,1,4),'id',sep = "_")) 
  by_gp$block <- tolower(paste(substr(by_gp$block_parishad_name,1,4),'id',sep = "_"))
  by_gp$gp <- tolower(paste(substr(by_gp$gram_parishad_name,1,4),'id',sep = "_"))
  gp_df_ind <- as.data.frame(by_gp)
  
  # CHANGE ADDITION 
  
  # CHANGE ADDITION   
  
  # print("GP DF")
  # print(str(gp_df_ind))
  
  for (i in 1:nrow(block_df_ind))
  {
    # series_list_gp[[i]] <- "Hello"
    # ####print(series_list_gp)
    
    series_list_gp[[i]] <- list( id = block_df_ind$drilldown[i],
                                 name = "Gram Panchayat",
                                 data = list_parse
                                 (data_frame(
                                   name = filter(gp_df_ind, block == block_df_ind$drilldown[i])$gram_parishad_name,
                                   y = round(filter(gp_df_ind, block == block_df_ind$drilldown[i])$value,digits = 0),
                                   # )
                                   drilldown = filter(gp_df_ind, block == block_df_ind$drilldown[i])$gp)  
                                 )
    )
    
  }
  # print("SSSSSSSSSSSSSSSSSSSSS series_list_gp SSSSSSSSSSSSSSSSSSSSSS")
  # print(series_list_gp)
  
  for (i in 1:nrow(gp_df_ind))
  {
    series_list_gs[[i]] <- list( id = gp_df_ind$gp[i],
                                 # name = "Gram Sansad",
                                 data = list_parse
                                 (data_frame(
                                   name = filter(df_ind, gram_parishad == gp_df_ind$gp[i])$gram_sansad_name,
                                   y = filter(df_ind, gram_parishad == gp_df_ind$gp[i])[,which(colnames(df_ind)== ind_name_gs)]
                                   
                                 )
                                 )
    )
  }
  # print("SSSSSSSSSSSSSSSSSSSSS series_list_gs SSSSSSSSSSSSSSSSSSSSSS")
  # print(series_list_gs)
  
  
  
  series_list_gp <- series_list_gp   
  
  
  highchart() %>%
    hc_chart(type = "column"
             # ,
             # events = list(
             #   drilldown =
             #     JS('function(e) {
             # console.log(e.seriesOptions);
             # this.setTitle({text: e.seriesOptions.name});
             # } '))
    ) %>%
    
    # hc_title(text = "Complete Immunization within 12 months (%)") %>%
    # hc_title(text = "<b>High Risk Cases</b> - NCDs",
    #          # # margin = 20,
    #          # align = "centre",
    #          style = list(color = "#05008b", useHTML = TRUE)) %>%
    
    # hc_title(text = span( "High Risk Cases - NCDs", style="color:#e32c3e")
    #           # style = list(fontWeight = "bold")
    #          ) %>%
    
    
    
  hc_xAxis(type = "category",
           title = list(text = "Panchayat Samiti/Gram Panchayat/Gram Sansad")
           # JS('function(e) {chart.options.yAxis[0].title.text = "new title";  }')
           
  ) %>%
    
    hc_yAxis(title = list(text = "Percentage")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_plotOptions(
      series = list(
        boderWidth = 0
        ,
        dataLabels = list(enabled = TRUE) 
        # ,
        # # colors = scale_color_brewer( ,type = "seq", palette = 1, direction = 1),
        # colors = rev(colorRampPalette(brewer.pal(4,"Blues"))(nrow(block_df_ind)))
        # events = list(click = canvasClickFunction)
      )) %>%
    
    hc_add_series(
      name = "Panchayat Samiti",
      data = list_parse(block_df_ind),
      # colorByPoint = TRUE
      color = "#2171b5"
    ) %>%
    
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = c(series_list_gp,series_list_gs),
      colorByPoint = TRUE
    )
  
  
})      


output$reportedDiaChol <- renderHighchart({
  
  ####print("in PREG CHART")
  df_ind <- getIndGS()
  ind_name <- paste("mean","(","i",11,",","na.rm=TRUE",")",sep = "")
  
  ind_name_gs <- paste0("i",11)
  summ_name <- paste0('mean_', "i",11)
  
  # FOR BLOCK LEVEL GRAPH
  by_block <- 
    df_ind %>% group_by(block_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  by_block <- by_block[complete.cases(by_block), ]
  # ####print("by_BLOBCKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKK")
  # ####print(by_block)
  colnames(by_block)<- c("name","y")
  by_block$y <- as.integer(by_block$y )
  
  by_block$drilldown <- tolower(paste(substr(by_block$name,1,4),'id',sep = "_"))
  block_df_ind <- as.data.frame(by_block)
  ####print("BLOCK DF")
  ####print(str(block_df_ind))
  
  # FOR GP LEVEL GRAPH
  by_gp <- 
    df_ind %>% group_by(block_parishad_name, gram_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  by_gp <- by_gp[complete.cases(by_gp), ]
  colnames(by_gp)<- c("block_parishad_name","gram_parishad_name","value")
  by_gp$drilldown <- tolower(paste(substr(by_gp$gram_parishad_name,1,4),'id',sep = "_")) 
  by_gp$block <- tolower(paste(substr(by_gp$block_parishad_name,1,4),'id',sep = "_"))
  gp_df_ind <- as.data.frame(by_gp)
  
  ####print("GP DF")
  ####print(str(gp_df_ind))
  
  for (i in 1:nrow(block_df_ind))
  {
    # series_list_gp[[i]] <- "Hello"
    # ####print(series_list_gp)
    
    series_list_gp[[i]] <- list( id = block_df_ind$drilldown[i],
                                 name = "Gram Panchayat",
                                 data = list_parse
                                 (data_frame(
                                   name = filter(gp_df_ind, block == block_df_ind$drilldown[i])$gram_parishad_name,
                                   y = round(filter(gp_df_ind, block == block_df_ind$drilldown[i])$value,digits = 0))
                                   # y = filter(gp_df_ind, block == block_df_ind$drilldown[i])$value )
                                 )
    )
  }
  
  series_list_gp <- series_list_gp  
  
  
  highchart() %>%
    hc_chart(type = "column"
             # ,
             # events = list(
             #   drilldown =
             #     JS('function(e) {
             # console.log(e.seriesOptions);
             # this.setTitle({text: e.seriesOptions.name});
             # } '))
    ) %>%
    
    # hc_title(text = "Population reported for Diarrhoea/Cholera (%)") %>%
    # hc_title(text = "<b>High Risk Cases</b> - NCDs",
    #          # # margin = 20,
    #          # align = "centre",
    #          style = list(color = "#05008b", useHTML = TRUE)) %>%
    
    # hc_title(text = span( "High Risk Cases - NCDs", style="color:#e32c3e")
    #           # style = list(fontWeight = "bold")
    #          ) %>%
    
    
    
  hc_xAxis(type = "category",
           title = list(text = "Block/GramPanchayat Name")
           # JS('function(e) {chart.options.yAxis[0].title.text = "new title";  }')
           
  ) %>%
    
    hc_yAxis(title = list(text = "Percentage")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE) 
        # ,
        # # colors = scale_color_brewer( ,type = "seq", palette = 1, direction = 1),
        # colors = rev(colorRampPalette(brewer.pal(9,"BuPu"))(nrow(block_df_ind)))
        # events = list(click = canvasClickFunction)
      )) %>%
    
    hc_add_series(
      name = "Panchayat Samiti",
      data = list_parse(block_df_ind),
      colorByPoint = TRUE
    ) %>%
    
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = series_list_gp,
      # colorByPoint = TRUE
      color = "#2171b5"
    )
  
  
})   
  

output$reportedUknownFever <- renderHighchart({
  
  ####print("in PREG CHART")
  df_ind <- getIndGS()
  ind_name <- paste("mean","(","i",12,",","na.rm=TRUE",")",sep = "")
  
  ind_name_gs <- paste0("i",12)
  summ_name <- paste0('mean_', "i",12)
  
  # FOR BLOCK LEVEL GRAPH
  by_block <- 
    df_ind %>% group_by(block_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  by_block <- by_block[complete.cases(by_block), ]
  # ####print("by_BLOBCKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKK")
  # ####print(by_block)
  colnames(by_block)<- c("name","y")
  by_block$y <- as.integer(by_block$y )
  
  by_block$drilldown <- tolower(paste(substr(by_block$name,1,4),'id',sep = "_"))
  block_df_ind <- as.data.frame(by_block)
  ####print("BLOCK DF")
  ####print(str(block_df_ind))
  
  # FOR GP LEVEL GRAPH
  by_gp <- 
    df_ind %>% group_by(block_parishad_name, gram_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  by_gp <- by_gp[complete.cases(by_gp), ]
  colnames(by_gp)<- c("block_parishad_name","gram_parishad_name","value")
  by_gp$drilldown <- tolower(paste(substr(by_gp$gram_parishad_name,1,4),'id',sep = "_")) 
  by_gp$block <- tolower(paste(substr(by_gp$block_parishad_name,1,4),'id',sep = "_"))
  gp_df_ind <- as.data.frame(by_gp)
  
  ####print("GP DF")
  ####print(str(gp_df_ind))
  
  for (i in 1:nrow(block_df_ind))
  {
    # series_list_gp[[i]] <- "Hello"
    # ####print(series_list_gp)
    
    series_list_gp[[i]] <- list( id = block_df_ind$drilldown[i],
                                 name = "Gram Panchayat",
                                 data = list_parse
                                 (data_frame(
                                   name = filter(gp_df_ind, block == block_df_ind$drilldown[i])$gram_parishad_name,
                                   y = round(filter(gp_df_ind, block == block_df_ind$drilldown[i])$value,digits = 0))
                                   # y = filter(gp_df_ind, block == block_df_ind$drilldown[i])$value )
                                 )
    )
  }
  
  series_list_gp <- series_list_gp  
  
  
  highchart() %>%
    hc_chart(type = "column"
             # ,
             # events = list(
             #   drilldown =
             #     JS('function(e) {
             # console.log(e.seriesOptions);
             # this.setTitle({text: e.seriesOptions.name});
             # } '))
    ) %>%
    
    # hc_title(text = "Population reported for Unknown Fever (%)") %>%
    # hc_title(text = "<b>High Risk Cases</b> - NCDs",
    #          # # margin = 20,
    #          # align = "centre",
    #          style = list(color = "#05008b", useHTML = TRUE)) %>%
    
    # hc_title(text = span( "High Risk Cases - NCDs", style="color:#e32c3e")
    #           # style = list(fontWeight = "bold")
    #          ) %>%
    
    
    
  hc_xAxis(type = "category",
           title = list(text = "Block/GramPanchayat Name")
           # JS('function(e) {chart.options.yAxis[0].title.text = "new title";  }')
           
  ) %>%
    
    hc_yAxis(title = list(text = "Percentage")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE) 
        # ,
        # # colors = scale_color_brewer( ,type = "seq", palette = 1, direction = 1),
        # colors = rev(colorRampPalette(brewer.pal(9,"Blues"))(nrow(block_df_ind)))
        # events = list(click = canvasClickFunction)
      )) %>%
    
    hc_add_series(
      name = "Panchayat Samiti",
      data = list_parse(block_df_ind),
      # colorByPoint = TRUE
      color = "#2171b5"
    ) %>%
    
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = series_list_gp,
      colorByPoint = TRUE
    )
  
  
})     
  

output$reportedLBW <- renderHighchart({
  
  ####print("in PREG CHART")
  df_ind <- getIndGS()
  ind_name <- paste("mean","(","i",18,",","na.rm=TRUE",")",sep = "")
  
  ind_name_gs <- paste0("i",18)
  summ_name <- paste0('mean_', "i",18)
  
  # FOR BLOCK LEVEL GRAPH
  by_block <- 
    df_ind %>% group_by(block_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  by_block <- by_block[complete.cases(by_block), ]
  # ####print("by_BLOBCKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKK")
  # ####print(by_block)
  colnames(by_block)<- c("name","y")
  by_block$y <- as.integer(by_block$y )
  
  by_block$drilldown <- tolower(paste(substr(by_block$name,1,4),'id',sep = "_"))
  block_df_ind <- as.data.frame(by_block)
  ####print("BLOCK DF")
  ###print(str(block_df_ind))
  
  # FOR GP LEVEL GRAPH
  by_gp <- 
    df_ind %>% group_by(block_parishad_name, gram_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  by_gp <- by_gp[complete.cases(by_gp), ]
  colnames(by_gp)<- c("block_parishad_name","gram_parishad_name","value")
  by_gp$drilldown <- tolower(paste(substr(by_gp$gram_parishad_name,1,4),'id',sep = "_")) 
  by_gp$block <- tolower(paste(substr(by_gp$block_parishad_name,1,4),'id',sep = "_"))
  gp_df_ind <- as.data.frame(by_gp)
  
  ###print("GP DF")
  ###print(str(gp_df_ind))
  
  for (i in 1:nrow(block_df_ind))
  {
    # series_list_gp[[i]] <- "Hello"
    # ###print(series_list_gp)
    
    series_list_gp[[i]] <- list( id = block_df_ind$drilldown[i],
                                 name = "Gram Panchayat",
                                 data = list_parse
                                 (data_frame(
                                   name = filter(gp_df_ind, block == block_df_ind$drilldown[i])$gram_parishad_name,
                                   y = round(filter(gp_df_ind, block == block_df_ind$drilldown[i])$value,digits = 0))
                                   # y = filter(gp_df_ind, block == block_df_ind$drilldown[i])$value )
                                 )
    )
  }
  
  series_list_gp <- series_list_gp  
  
  
  highchart() %>%
    hc_chart(type = "column"
             # ,
             # events = list(
             #   drilldown =
             #     JS('function(e) {
             # console.log(e.seriesOptions);
             # this.setTitle({text: e.seriesOptions.name});
             # } '))
    ) %>%
    
    # hc_title(text = "Low Birth Weight (%)") %>%
    # hc_title(text = "<b>High Risk Cases</b> - NCDs",
    #          # # margin = 20,
    #          # align = "centre",
    #          style = list(color = "#05008b", useHTML = TRUE)) %>%
    
    # hc_title(text = span( "High Risk Cases - NCDs", style="color:#e32c3e")
    #           # style = list(fontWeight = "bold")
    #          ) %>%
    
    
    
  hc_xAxis(type = "category",
           title = list(text = "Block/GramPanchayat Name")
           # JS('function(e) {chart.options.yAxis[0].title.text = "new title";  }')
           
  ) %>%
    
    hc_yAxis(title = list(text = "Percentage")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE) 
        # ,
        # # colors = scale_color_brewer( ,type = "seq", palette = 1, direction = 1),
        # colors = rev(colorRampPalette(brewer.pal(9,"Blues"))(nrow(block_df_ind)))
        # events = list(click = canvasClickFunction)
      )) %>%
    
    hc_add_series(
      name = "Panchayat Samiti",
      data = list_parse(block_df_ind),
      color = "#2171b5"
      # colorByPoint = TRUE
    ) %>%
    
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = series_list_gp,
      colorByPoint = TRUE
    )
  
  
})       
  
  
output$reportedSAM <- renderHighchart({
  
  ###print("in PREG CHART")
  df_ind <- getIndGS()
  ind_name <- paste("mean","(","i",20,",","na.rm=TRUE",")",sep = "")
  
  ind_name_gs <- paste0("i",20)
  summ_name <- paste0('mean_', "i",20)
  
  # FOR BLOCK LEVEL GRAPH
  by_block <- 
    df_ind %>% group_by(block_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  by_block <- by_block[complete.cases(by_block), ]
  # ###print("by_BLOBCKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKK")
  # ###print(by_block)
  colnames(by_block)<- c("name","y")
  by_block$y <- as.integer(by_block$y )
  
  by_block$drilldown <- tolower(paste(substr(by_block$name,1,4),'id',sep = "_"))
  block_df_ind <- as.data.frame(by_block)
  ###print("BLOCK DF")
  ###print(str(block_df_ind))
  
  # FOR GP LEVEL GRAPH
  by_gp <- 
    df_ind %>% group_by(block_parishad_name, gram_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  by_gp <- by_gp[complete.cases(by_gp), ]
  colnames(by_gp)<- c("block_parishad_name","gram_parishad_name","value")
  by_gp$drilldown <- tolower(paste(substr(by_gp$gram_parishad_name,1,4),'id',sep = "_")) 
  by_gp$block <- tolower(paste(substr(by_gp$block_parishad_name,1,4),'id',sep = "_"))
  gp_df_ind <- as.data.frame(by_gp)
  
  ###print("GP DF")
  ###print(str(gp_df_ind))
  
  for (i in 1:nrow(block_df_ind))
  {
    # series_list_gp[[i]] <- "Hello"
    # ###print(series_list_gp)
    
    series_list_gp[[i]] <- list( id = block_df_ind$drilldown[i],
                                 name = "Gram Panchayat",
                                 data = list_parse
                                 (data_frame(
                                   name = filter(gp_df_ind, block == block_df_ind$drilldown[i])$gram_parishad_name,
                                   y = round(filter(gp_df_ind, block == block_df_ind$drilldown[i])$value,digits = 0))
                                   # y = filter(gp_df_ind, block == block_df_ind$drilldown[i])$value )
                                 )
    )
  }
  
  series_list_gp <- series_list_gp  
  
  
  highchart() %>%
    hc_chart(type = "column"
             # ,
             # events = list(
             #   drilldown =
             #     JS('function(e) {
             # console.log(e.seriesOptions);
             # this.setTitle({text: e.seriesOptions.name});
             # } '))
    ) %>%
    
    # hc_title(text = "Severely Malnourished Children (%)") %>%
    # hc_title(text = "<b>High Risk Cases</b> - NCDs",
    #          # # margin = 20,
    #          # align = "centre",
    #          style = list(color = "#05008b", useHTML = TRUE)) %>%
    
    # hc_title(text = span( "High Risk Cases - NCDs", style="color:#e32c3e")
    #           # style = list(fontWeight = "bold")
    #          ) %>%
    
    
    
  hc_xAxis(type = "category",
           title = list(text = "Block/GramPanchayat Name")
           # JS('function(e) {chart.options.yAxis[0].title.text = "new title";  }')
           
  ) %>%
    
    hc_yAxis(title = list(text = "Percentage")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE) 
        # ,
        # # colors = scale_color_brewer( ,type = "seq", palette = 1, direction = 1),
        # colors = rev(colorRampPalette(brewer.pal(9,"Blues"))(nrow(block_df_ind)))
        # events = list(click = canvasClickFunction)
      )) %>%
    
    hc_add_series(
      name = "Panchayat Samiti",
      data = list_parse(block_df_ind),
      color = "#2171b5"
      # colorByPoint = TRUE
    ) %>%
    
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = series_list_gp,
      colorByPoint = TRUE
    )
  
  
})       


output$reportedOpenDefecated <- renderHighchart({
  
  ###print("in PREG CHART")
  df_ind <- getIndGS()
  ind_name <- paste("mean","(","i",33,",","na.rm=TRUE",")",sep = "")
  
  ind_name_gs <- paste0("i",33)
  summ_name <- paste0('mean_', "i",33)
  
  # FOR BLOCK LEVEL GRAPH
  by_block <- 
    df_ind %>% group_by(block_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  by_block <- by_block[complete.cases(by_block), ]
  # ###print("by_BLOBCKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKK")
  # ###print(by_block)
  colnames(by_block)<- c("name","y")
  by_block$y <- as.integer(by_block$y )
  
  by_block$drilldown <- tolower(paste(substr(by_block$name,1,4),'id',sep = "_"))
  block_df_ind <- as.data.frame(by_block)
  ###print("BLOCK DF")
  ###print(str(block_df_ind))
  
  # FOR GP LEVEL GRAPH
  by_gp <- 
    df_ind %>% group_by(block_parishad_name, gram_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  by_gp <- by_gp[complete.cases(by_gp), ]
  colnames(by_gp)<- c("block_parishad_name","gram_parishad_name","value")
  by_gp$drilldown <- tolower(paste(substr(by_gp$gram_parishad_name,1,4),'id',sep = "_")) 
  by_gp$block <- tolower(paste(substr(by_gp$block_parishad_name,1,4),'id',sep = "_"))
  gp_df_ind <- as.data.frame(by_gp)
  
  ###print("GP DF")
  ###print(str(gp_df_ind))
  
  for (i in 1:nrow(block_df_ind))
  {
    # series_list_gp[[i]] <- "Hello"
    # ###print(series_list_gp)
    
    series_list_gp[[i]] <- list( id = block_df_ind$drilldown[i],
                                 name = "Gram Panchayat",
                                 data = list_parse
                                 (data_frame(
                                   name = filter(gp_df_ind, block == block_df_ind$drilldown[i])$gram_parishad_name,
                                   y = round(filter(gp_df_ind, block == block_df_ind$drilldown[i])$value,digits = 0))
                                   # y = filter(gp_df_ind, block == block_df_ind$drilldown[i])$value )
                                 )
    )
  }
  
  series_list_gp <- series_list_gp  
  
  
  highchart() %>%
    hc_chart(type = "column"
             # ,
             # events = list(
             #   drilldown =
             #     JS('function(e) {
             # console.log(e.seriesOptions);
             # this.setTitle({text: e.seriesOptions.name});
             # } '))
    ) %>%
    
    # hc_title(text = "Households Defecated in Open Space (%)") %>%
    # hc_title(text = "<b>High Risk Cases</b> - NCDs",
    #          # # margin = 20,
    #          # align = "centre",
    #          style = list(color = "#05008b", useHTML = TRUE)) %>%
    
    # hc_title(text = span( "High Risk Cases - NCDs", style="color:#e32c3e")
    #           # style = list(fontWeight = "bold")
    #          ) %>%
    
    
    
  hc_xAxis(type = "category",
           title = list(text = "Block/GramPanchayat Name")
           # JS('function(e) {chart.options.yAxis[0].title.text = "new title";  }')
           
  ) %>%
    
    hc_yAxis(title = list(text = "Percentage")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE) 
        # ,
        # # colors = scale_color_brewer( ,type = "seq", palette = 1, direction = 1),
        # colors = rev(colorRampPalette(brewer.pal(9,"BuPu"))(nrow(block_df_ind)))
        # events = list(click = canvasClickFunction)
      )) %>%
    
    hc_add_series(
      name = "Panchayat Samiti",
      data = list_parse(block_df_ind),
      # colorByPoint = TRUE
      color = "#2171b5"
    ) %>%
    
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = series_list_gp,
      colorByPoint = TRUE
    )
  
  
})  
  
output$reportedExcretaToiletPan <- renderHighchart({
  
  ###print("in PREG CHART")
  df_ind <- getIndGS()
  ind_name <- paste("mean","(","i",34,",","na.rm=TRUE",")",sep = "")
  
  ind_name_gs <- paste0("i",34)
  summ_name <- paste0('mean_', "i",34)
  
  # FOR BLOCK LEVEL GRAPH
  by_block <- 
    df_ind %>% group_by(block_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  by_block <- by_block[complete.cases(by_block), ]
  # ###print("by_BLOBCKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKK")
  # ###print(by_block)
  colnames(by_block)<- c("name","y")
  by_block$y <- as.integer(by_block$y )
  
  by_block$drilldown <- tolower(paste(substr(by_block$name,1,4),'id',sep = "_"))
  block_df_ind <- as.data.frame(by_block)
  ###print("BLOCK DF")
  ###print(str(block_df_ind))
  
  # FOR GP LEVEL GRAPH
  by_gp <- 
    df_ind %>% group_by(block_parishad_name, gram_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  by_gp <- by_gp[complete.cases(by_gp), ]
  colnames(by_gp)<- c("block_parishad_name","gram_parishad_name","value")
  by_gp$drilldown <- tolower(paste(substr(by_gp$gram_parishad_name,1,4),'id',sep = "_")) 
  by_gp$block <- tolower(paste(substr(by_gp$block_parishad_name,1,4),'id',sep = "_"))
  gp_df_ind <- as.data.frame(by_gp)
  
  ###print("GP DF")
  ###print(str(gp_df_ind))
  
  for (i in 1:nrow(block_df_ind))
  {
    # series_list_gp[[i]] <- "Hello"
    # ###print(series_list_gp)
    
    series_list_gp[[i]] <- list( id = block_df_ind$drilldown[i],
                                 name = "Gram Panchayat",
                                 data = list_parse
                                 (data_frame(
                                   name = filter(gp_df_ind, block == block_df_ind$drilldown[i])$gram_parishad_name,
                                   y = round(filter(gp_df_ind, block == block_df_ind$drilldown[i])$value,digits = 0))
                                   # y = filter(gp_df_ind, block == block_df_ind$drilldown[i])$value )
                                 )
    )
  }
  
  series_list_gp <- series_list_gp  
  
  
  highchart() %>%
    hc_chart(type = "column"
             # ,
             # events = list(
             #   drilldown =
             #     JS('function(e) {
             # console.log(e.seriesOptions);
             # this.setTitle({text: e.seriesOptions.name});
             # } '))
    ) %>%
    
    # hc_title(text = "Households Excreta Not Disposed In Toilet (%)") %>%
    # hc_title(text = "<b>High Risk Cases</b> - NCDs",
    #          # # margin = 20,
    #          # align = "centre",
    #          style = list(color = "#05008b", useHTML = TRUE)) %>%
    
    # hc_title(text = span( "High Risk Cases - NCDs", style="color:#e32c3e")
    #           # style = list(fontWeight = "bold")
    #          ) %>%
    
    
    
  hc_xAxis(type = "category",
           title = list(text = "Block/GramPanchayat Name")
           # JS('function(e) {chart.options.yAxis[0].title.text = "new title";  }')
           
  ) %>%
    
    hc_yAxis(title = list(text = "Percentage")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE) 
        # ,
        # # colors = scale_color_brewer( ,type = "seq", palette = 1, direction = 1),
        # colors = rev(colorRampPalette(brewer.pal(9,"Blues"))(nrow(block_df_ind)))
        # events = list(click = canvasClickFunction)
      )) %>%
    
    hc_add_series(
      name = "Panchayat Samiti",
      data = list_parse(block_df_ind),
      color = "#2171b5"
      # colorByPoint = TRUE
    ) %>%
    
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = series_list_gp,
      colorByPoint = TRUE
    )
  
  
})  

output$pieToilets <- renderHighchart({
  
  # ###print("PIE VALUES")
  
  ind_pie_toilet <- getIndVal() 
  
  df <- ind_pie_toilet[23:27,c(3,6)]
  
  # ###print("PIE DF VALUES")
  
  # print(df)
 
  df_pie <- c(
    rep("Squatting plate",as.integer(df$Value[1])),
    rep("Single pit",as.integer(df$Value[2])),
    rep("Double pit",as.integer(df$Value[3])),
    rep("Dysfunctional",as.integer(df$Value[4]))
    # rep("Data not available",as.integer(df$Value[5]))
  )
  ###print(df_pie)
  
  
  hchart(df_pie,type = "pie", name = "Percentage Households")%>% 
    # hc_add_theme(hc_theme_smpl())%>%
    hc_legend(enabled = FALSE) %>%
    # hc_title(text = paste0("Call Status (all Calls)",span(" (","N=",format(nrow(sci_dump_temp), nsmall=0, big.mark=","),")", style="color:#e32c3e")),
    #          style = list(fontWeight = "bold")) %>%
    # hc_title(text = "Type of Toilets (all Blocks)") %>%
    # hc_title(text = paste0("Type of Toilets (all Blocks)")
    #          # ,
    #          # style = list(fontWeight = "bold")
    #          ) %>%
    hc_plotOptions( pie = list(colors = brewer.pal(6,"Blues"),
                               # type = "pie",
                               name = "No. of Calls", 
                               colorByPoint = TRUE,
                               # center = c('55%', '50%'),
                               size = 130, 
                               dataLabels = list(enabled = TRUE,
                                                 format = '{point.name}: ({point.percentage:.1f}%)'
                                                 # {point.percentage:.1f}
                               ))) %>% 
    # hc_credits(enabled = TRUE,
    #            text = "Source: SCI SDI Helpline data,2017-18",
    #            href = "https://www.savethechildren.in/",
    #            style = list(fontSize = "9px")) %>%
    hc_exporting(enabled = TRUE)
  
  
})  

output$pieSLWMStatus <- renderHighchart({
  
  # ###print("PIE VALUES")
  
  ind_pie_toilet <- getIndVal() 
  
  df <- ind_pie_toilet[38:43,c(3,6)]
  
  
  # print("PIE DF VALUES")
  
  # print(df)
  
  df_pie <- c(
    rep("Garbage & Soak pit",as.integer(df$Value[1])),
    rep("Only Garbage pit",as.integer(df$Value[2])),
    rep("Only Soak pit",as.integer(df$Value[3])),
    rep("Community collection",as.integer(df$Value[4])),
    rep("No System ",as.integer(df$Value[5])),
    rep("Data not available",as.integer(df$Value[6]))
  )
  ###print(df_pie)
  
  
  hchart(df_pie,type = "pie", name = "Percentage Households")%>% 
    # hc_add_theme(hc_theme_smpl())%>%
    hc_legend(enabled = FALSE) %>%
    # hc_title(text = paste0("Call Status (all Calls)",span(" (","N=",format(nrow(sci_dump_temp), nsmall=0, big.mark=","),")", style="color:#e32c3e")),
    #          style = list(fontWeight = "bold")) %>%
    # hc_title(text = "SLWM Status at Household Level (all Blocks)") %>%
    # hc_title(text = paste0("Type of Toilets (all Blocks)")
    #          # ,
    #          # style = list(fontWeight = "bold")
    #          ) %>%
    hc_plotOptions( pie = list(colors = brewer.pal(6,"BuPu"),
                               # type = "pie",
                               name = "No. of Calls", 
                               colorByPoint = TRUE,
                               # center = c('50%', '50%'),
                               size = 100, 
                               dataLabels = list(enabled = TRUE,
                                                 format = '{point.name}: ({point.percentage:.1f}%)'
                                                 # {point.percentage:.1f}
                               ))) %>% 
    # hc_credits(enabled = TRUE,
    #            text = "Source: SCI SDI Helpline data,2017-18",
    #            href = "https://www.savethechildren.in/",
    #            style = list(fontSize = "9px")) %>%
    hc_exporting(enabled = TRUE)
  
  
})  


  
  
})
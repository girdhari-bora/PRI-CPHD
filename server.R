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
library(shinycssloaders)
library(parallel)
library(foreach)




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
    
    # get_data_df[,8:59] <- sapply(get_data_df[,8:59],as.integer)
   
    
    #NEW CODE BLOCK
    
    # get_data_df <- filter(get_data_df,get_data_df$i1_add!=0,get_data_df$i1_add!="",
    #                       get_data_df$i2_add!=0,get_data_df$i2_add!="")
    
    #NEW CODE BLOCK ENDS
    
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
    
    return(filter(
                  # get_data_df_temp_final,
                 get_data_df_temp, 
                  year_month_date >= input$monthRange[1],
                  year_month_date <= input$monthRange[2]))
    
    
  })
  

getIndGS <- reactive({

    # print ("Hello in getIndGS()")
    get_data_df <- get_data_df_gs_ind
    
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
    # get_data_df_temp_final <- get_data_df_temp %>%
    #   mutate(year_month_date = as.Date(ifelse( month >3 , paste(matrix(unlist(strsplit(get_data_df$year, "-")),ncol=2, byrow = TRUE)[,1],
    #                                                     month,"01",sep = "-")
    #                                    , paste(as.integer(matrix(unlist(strsplit(get_data_df$year, "-")),ncol=2, byrow = TRUE)[,1])+1,
    #                                            month,"01",sep = "-")), "%Y-%m-%d")
    #   )
    
    
    return(filter(get_data_df_temp, 
                  year_month_date >= input$monthRange[1],
                  year_month_date <= input$monthRange[2]))
    

  
  })

getIndBlock <- reactive({
  
  # print ("Hello in getIndGS()")
  get_data_df <- get_data_df_block_ind
  
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
  # get_data_df_temp_final <- get_data_df_temp %>%
  #   mutate(year_month_date = as.Date(ifelse( month >3 , paste(matrix(unlist(strsplit(get_data_df$year, "-")),ncol=2, byrow = TRUE)[,1],
  #                                                             month,"01",sep = "-")
  #                                            , paste(as.integer(matrix(unlist(strsplit(get_data_df$year, "-")),ncol=2, byrow = TRUE)[,1])+1,
  #                                                    month,"01",sep = "-")), "%Y-%m-%d")
  #   )
  
  
  return(filter(get_data_df_temp, 
                year_month_date >= input$monthRange[1],
                year_month_date <= input$monthRange[2]))
  
  
})

getIndGP <- reactive({
  
  get_data_df <- get_data_df_gp_ind
  
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
  # get_data_df_temp_final <- get_data_df_temp %>%
  #   mutate(year_month_date = as.Date(ifelse( month >3 , paste(matrix(unlist(strsplit(get_data_df$year, "-")),ncol=2, byrow = TRUE)[,1],
  #                                                             month,"01",sep = "-")
  #                                            , paste(as.integer(matrix(unlist(strsplit(get_data_df$year, "-")),ncol=2, byrow = TRUE)[,1])+1,
  #                                                    month,"01",sep = "-")), "%Y-%m-%d")
  #   )
  
  
  return(filter(get_data_df_temp, 
                year_month_date >= input$monthRange[1],
                year_month_date <= input$monthRange[2]))
  
  
})

  
getIndVal <- reactive({
    
    # print ("Hello in getIndVal()")
    get_data_df <- datasetInput()
    
    # ####print(str(get_data_df))
    # get_data_df <- filter(get_data_df,get_data_df$i1_add!=0,get_data_df$i1_add!="",
    #                       get_data_df$i2_add!=0,get_data_df$i2_add!="",
    #                       get_data_df$i18_add!=0,get_data_df$i18_add!="",
    #                       get_data_df$i36_add!=0,get_data_df$i36_add!="")
    
    
    Indicators_PRI <- indicators_master
    
    # Indicators_PRI$Value[1] <- round(mean(get_data_df$i1_add/get_data_df$i2_add, na.rm = TRUE),digits = 0)
    
    Indicators_PRI$Value[1] <- round(mean(get_data_df$i1_add/get_data_df$i2_add),digits = 2)
    
    # Indicators_PRI$Value[2] <- round(mean((get_data_df$i3_add/get_data_df$i1_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[2] <- round((sum(get_data_df$i3_add, na.rm = TRUE)/
                           sum(get_data_df[!duplicated(get_data_df$gram_sansad_name), c("gram_sansad_name","i1_add")]$i1_add,na.rm = TRUE))*100,digits = 2)
    
    # Indicators_PRI$Value[3] <- round(mean((get_data_df$i4_add/get_data_df$i1_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[3] <- round((sum(get_data_df$i4_add, na.rm = TRUE)/
                                        sum(get_data_df[!duplicated(get_data_df$gram_sansad_name), c("gram_sansad_name","i1_add")]$i1_add,na.rm = TRUE))*100,digits = 2)
    
    Indicators_PRI$Value[4] <- round(mean(get_data_df$i5_add, na.rm = TRUE),digits = 2) 
    
    # Indicators_PRI$Value[5] <- round(mean((get_data_df$i6_add/get_data_df$i5_add)*100, na.rm = TRUE),digits = 0)
    # pregnant_women_total <- sum(get_data_df$i5_add, na.rm = TRUE)
    Indicators_PRI$Value[5] <- round((sum(get_data_df$i6_add, na.rm = TRUE)/sum(get_data_df$i5_add, na.rm = TRUE))*100,digits = 2)
    
    # delieveries_total <- sum(get_data_df$i8_add, na.rm = TRUE)
    # Indicators_PRI$Value[6] <- round((mean(get_data_df$i7_add, na.rm = TRUE)/delieveries_total)*100,digits = 0)
    Indicators_PRI$Value[6] <- round((sum(get_data_df$i7_add, na.rm = TRUE)/sum(get_data_df$i8_add, na.rm = TRUE))*100,digits = 2)
    
    # Indicators_PRI$Value[7] <- round(mean((get_data_df$i9_add/get_data_df$i8_add)*100, na.rm = TRUE),digits = 0)
    # Indicators_PRI$Value[7] <- round((mean(get_data_df$i9_add, na.rm = TRUE)/delieveries_total)*100,digits = 0)
    Indicators_PRI$Value[7] <- round((sum(get_data_df$i9_add, na.rm = TRUE)/sum(get_data_df$i8_add, na.rm = TRUE))*100,digits = 2)
    
    # Indicators_PRI$Value[8] <- round(mean((get_data_df$i10_add/get_data_df$i8_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[8] <- round((sum(get_data_df$i10_add, na.rm = TRUE)/sum(get_data_df$i8_add, na.rm = TRUE))*100,digits = 2)
    
    # Indicators_PRI$Value[9] <- round(mean((get_data_df$i11_add/(get_data_df$i8_add-get_data_df$i10_add))*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[9] <- round((sum(get_data_df$i11_add, na.rm = TRUE)/
                                        (sum(get_data_df$i8_add, na.rm = TRUE) - sum(get_data_df$i10_add, na.rm = TRUE)))*100,digits = 2)
    
    
    # Indicators_PRI$Value[10] <- round(mean((get_data_df$i12_add/get_data_df$i3_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[10] <- round((sum(get_data_df$i12_add, na.rm = TRUE)/sum(get_data_df$i3_add, na.rm = TRUE))*100,digits = 2)
    
    # Indicators_PRI$Value[11] <- round(sum((get_data_df$i13_add/get_data_df$i1_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[11] <- round((sum(get_data_df$i13_add, na.rm = TRUE)/
                                         sum(get_data_df[!duplicated(get_data_df$gram_sansad_name), c("gram_sansad_name","i1_add")]$i1_add,na.rm = TRUE))*100,digits = 2)
    
    # Indicators_PRI$Value[12] <- round(mean((get_data_df$i14_add/get_data_df$i1_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[12] <- round((sum(get_data_df$i14_add, na.rm = TRUE)/
                                         sum(get_data_df[!duplicated(get_data_df$gram_sansad_name), c("gram_sansad_name","i1_add")]$i1_add,na.rm = TRUE))*100,digits = 2)
    
    # Indicators_PRI$Value[13] <- round(mean((get_data_df$i15_add/get_data_df$i3_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[13] <- round((sum(get_data_df$i15_add, na.rm = TRUE)/sum(get_data_df$i3_add, na.rm = TRUE))*100,digits = 2)
    
    
    # Indicators_PRI$Value[14] <- round(mean((get_data_df$i16_add/get_data_df$i4_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[14] <- round((sum(get_data_df$i16_add, na.rm = TRUE)/sum(get_data_df$i4_add, na.rm = TRUE))*100,digits = 2)
    
    
    Indicators_PRI$Value[15] <- round(sum(get_data_df$i17_add, na.rm = TRUE),digits = 2) 
    
    #####################################################################################################################
    ############################# CHILD DEVELOPMENT #####################################################################
    #####################################################################################################################
    
    # Indicators_PRI$Value[16] <- round(mean((get_data_df$i18_add/get_data_df$i4_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[16] <- round((sum(get_data_df[!duplicated(get_data_df$gram_sansad_name), c("gram_sansad_name","i18_add")]$i18_add,na.rm = TRUE)
                                       / sum(get_data_df$i4_add, na.rm = TRUE))*100,digits = 2)
    
    # Indicators_PRI$Value[17] <- round(mean((get_data_df$i19_add/get_data_df$i8_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[17] <- round((sum(get_data_df$i19_add, na.rm = TRUE)/
                                        (sum(get_data_df$i8_add, na.rm = TRUE) - sum(get_data_df$i10_add, na.rm = TRUE)))*100,digits = 2)
    
    # Indicators_PRI$Value[18] <- round(mean((get_data_df$i20_add/get_data_df$i1_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[18] <- round((sum(get_data_df$i20_add, na.rm = TRUE)/sum(get_data_df$i11_add, na.rm = TRUE))*100,digits = 2)
    
    # Indicators_PRI$Value[19] <- round(mean((get_data_df$i21_add/get_data_df$i4_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[19] <- round((sum(get_data_df$i21_add, na.rm = TRUE)/sum(get_data_df$i4_add, na.rm = TRUE))*100,digits = 2)
    
    # Indicators_PRI$Value[20] <- round(mean((get_data_df$i22_add/get_data_df$i4_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[20] <- round((sum(get_data_df$i22_add, na.rm = TRUE)/sum(get_data_df$i4_add, na.rm = TRUE))*100,digits = 2)
    
    # Indicators_PRI$Value[21] <- round(mean((get_data_df$i23_add/get_data_df$i22_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$Value[21] <- round((sum(get_data_df$i23_add, na.rm = TRUE)/sum(get_data_df$i22_add, na.rm = TRUE))*100,digits = 2)
    
    
    #####################################################################################################################
    ############################# PANCHAYAT AND RURAL DEVELOPMENT########################################################
    #####################################################################################################################
    
    
    Indicators_PRI$Value[23] <- round((sum(get_data_df$i25_add,na.rm = TRUE)/
                                              sum(get_data_df[!duplicated(get_data_df$gram_sansad_name), c("gram_sansad_name","i2_add")]$i2_add,na.rm = TRUE))*100,digits = 2)
    # print("Indicators_PRI[23]")
    # print(Indicators_PRI$Value[23])
    
     # Indicators_PRI$Value[24] <- round(mean((get_data_df$i26_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
     Indicators_PRI$Value[24] <- round((sum(get_data_df$i26_add,na.rm = TRUE)/
                                          sum(get_data_df[!duplicated(get_data_df$gram_sansad_name), c("gram_sansad_name","i2_add")]$i2_add,na.rm = TRUE))*100,digits = 2)
    # # Indicators_PRI$Value[25] <- round(mean((get_data_df$i27_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
     Indicators_PRI$Value[25] <- round((sum(get_data_df$i27_add, na.rm = TRUE)/
                                          sum(get_data_df[!duplicated(get_data_df$gram_sansad_name), c("gram_sansad_name","i2_add")]$i2_add,na.rm = TRUE))*100,digits = 2)
    
    # Indicators_PRI$Value[26] <- round(mean((get_data_df$i28_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
     Indicators_PRI$Value[26] <- round((sum(get_data_df$i28_add, na.rm = TRUE)/
                                          sum(get_data_df[!duplicated(get_data_df$gram_sansad_name), c("gram_sansad_name","i2_add")]$i2_add,na.rm = TRUE))*100,digits = 2)

    # Indicators_PRI$Value[27] <- round(mean(((get_data_df$i2_add- (get_data_df$i25_add + 
    #                                                                 get_data_df$i26_add + 
    #                                                                 get_data_df$i27_add + 
    #                                                                 get_data_df$i28_add ))/
    #                                           get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    
    Indicators_PRI$Value[27] <- round(100 - (Indicators_PRI$Value[23] +Indicators_PRI$Value[24] +Indicators_PRI$Value[25] +Indicators_PRI$Value[26] ),digits = 2)
    
    
    # Indicators_PRI$Value[29] <- round(mean((get_data_df$i30_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[29] <- round((sum(get_data_df$i30_add, na.rm = TRUE)/
                                         sum(get_data_df[!duplicated(get_data_df$gram_sansad_name), c("gram_sansad_name","i2_add")]$i2_add,na.rm = TRUE))*100,digits = 2)
    
    # Indicators_PRI$Value[30] <- round(mean((get_data_df$i31_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[30] <- round((sum(get_data_df$i31_add, na.rm = TRUE)/
                                         sum(get_data_df[!duplicated(get_data_df$gram_sansad_name), c("gram_sansad_name","i2_add")]$i2_add,na.rm = TRUE))*100,digits = 2)
    
    # Indicators_PRI$Value[31] <- round(mean((get_data_df$i32_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[31] <- round((sum(get_data_df$i32_add, na.rm = TRUE)/
                                         sum(get_data_df[!duplicated(get_data_df$gram_sansad_name), c("gram_sansad_name","i2_add")]$i2_add,na.rm = TRUE))*100,digits = 2)
    
    # Indicators_PRI$Value[32] <- round(mean(((get_data_df$i2_add- (get_data_df$i30_add + 
    #                                                                 get_data_df$i31_add + 
    #                                                                 get_data_df$i32_add 
    # ))/
    #   get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[32] <- round(100 - (Indicators_PRI$Value[29] + Indicators_PRI$Value[30] + Indicators_PRI$Value[31]), digits = 2)
    
    # Indicators_PRI$Value[33] <- round(mean((get_data_df$i33_add/
    #                                           get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[33] <- round((sum(get_data_df$i33_add, na.rm = TRUE)/
                                         sum(get_data_df[!duplicated(get_data_df$gram_sansad_name), c("gram_sansad_name","i2_add")]$i2_add,na.rm = TRUE))*100,digits = 2)
    
    # Indicators_PRI$Value[34] <- round(mean((get_data_df$i34_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[34] <- round((sum(get_data_df$i34_add, na.rm = TRUE)/
                                         sum(get_data_df[!duplicated(get_data_df$gram_sansad_name), c("gram_sansad_name","i2_add")]$i2_add,na.rm = TRUE))*100,digits = 2)
    
    # Indicators_PRI$Value[35] <- round(mean((get_data_df$i35_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[35] <- round((sum(get_data_df$i35_add, na.rm = TRUE)/
                                         sum(get_data_df[!duplicated(get_data_df$gram_sansad_name), c("gram_sansad_name","i2_add")]$i2_add,na.rm = TRUE))*100,digits = 2)
    
    # Indicators_PRI$Value[17] <- round(mean((get_data_df$i19_add/get_data_df$i8_add)*100, na.rm = TRUE),digits = 0) 
    # Indicators_PRI$Value[36] <- round(mean((get_data_df$i37_add/get_data_df$i36_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[36] <- round((sum(get_data_df$i37_add, na.rm = TRUE)/
                                         sum(get_data_df[!duplicated(get_data_df$gram_sansad_name), c("gram_sansad_name","i36_add")]$i36_add,na.rm = TRUE))*100,digits = 2)
    
    
    # Indicators_PRI$Value[37] <- round(mean((get_data_df$i38_add/get_data_df$i36_add)*100, na.rm = TRUE),digits = 0)
    
    # Indicators_PRI$Value[38] <- round(mean((get_data_df$i39_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[38] <- round((sum(get_data_df$i39_add, na.rm = TRUE)/
                                         sum(get_data_df[!duplicated(get_data_df$gram_sansad_name), c("gram_sansad_name","i2_add")]$i2_add,na.rm = TRUE))*100,digits = 2)
    
    # Indicators_PRI$Value[39] <- round(mean((get_data_df$i40_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[39] <- round((sum(get_data_df$i40_add, na.rm = TRUE)/
                                         sum(get_data_df[!duplicated(get_data_df$gram_sansad_name), c("gram_sansad_name","i2_add")]$i2_add,na.rm = TRUE))*100,digits = 2)
    
    
    # Indicators_PRI$Value[40] <- round(mean((get_data_df$i41_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[40] <- round((sum(get_data_df$i41_add, na.rm = TRUE)/
                                         sum(get_data_df[!duplicated(get_data_df$gram_sansad_name), c("gram_sansad_name","i2_add")]$i2_add,na.rm = TRUE))*100,digits = 2)
    
    # Indicators_PRI$Value[41] <- round(mean((get_data_df$i42_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[41] <- round((sum(get_data_df$i42_add, na.rm = TRUE)/
                                         sum(get_data_df[!duplicated(get_data_df$gram_sansad_name), c("gram_sansad_name","i2_add")]$i2_add,na.rm = TRUE))*100,digits = 2)
    
    # Indicators_PRI$Value[42] <- round(mean((get_data_df$i43_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$Value[42] <- round((sum(get_data_df$i43_add, na.rm = TRUE)/
                                         sum(get_data_df[!duplicated(get_data_df$gram_sansad_name), c("gram_sansad_name","i2_add")]$i2_add,na.rm = TRUE))*100,digits = 2)
    
    Indicators_PRI$Value[43] <- 100 - (Indicators_PRI$Value[38]+Indicators_PRI$Value[39]+Indicators_PRI$Value[40]+Indicators_PRI$Value[41]
                                       +Indicators_PRI$Value[42])
    
    # Indicators_PRI$Value[43] <- round(mean(((get_data_df$i2_add- (  get_data_df$i39_add + 
    #                                                                   get_data_df$i40_add + 
    #                                                                   get_data_df$i41_add +
    #                                                                   get_data_df$i42_add + 
    #                                                                   get_data_df$i43_add 
    # ))/
    #   get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    
    denominator_45_to_52 <- (sum(get_data_df$i45_add, na.rm = TRUE) +
                               sum(get_data_df$i46_add, na.rm = TRUE) +
                               sum(get_data_df$i47_add, na.rm = TRUE) +
                               sum(get_data_df$i48_add, na.rm = TRUE) +
                               sum(get_data_df$i49_add, na.rm = TRUE) +
                               sum(get_data_df$i50_add, na.rm = TRUE) +
                               sum(get_data_df$i51_add, na.rm = TRUE) +
                               sum(get_data_df$i52_add, na.rm = TRUE))
                              
    Indicators_PRI$Value[45] <- round((sum(get_data_df$i45_add,na.rm = TRUE)/denominator_45_to_52)*100,digits = 2)
    Indicators_PRI$Value[46] <- round((sum(get_data_df$i46_add,na.rm = TRUE)/denominator_45_to_52)*100,digits = 2)
    Indicators_PRI$Value[47] <- round((sum(get_data_df$i47_add,na.rm = TRUE)/denominator_45_to_52)*100,digits = 2)
    Indicators_PRI$Value[48] <- round((sum(get_data_df$i48_add,na.rm = TRUE)/denominator_45_to_52)*100,digits = 2)
    Indicators_PRI$Value[49] <- round((sum(get_data_df$i49_add,na.rm = TRUE)/denominator_45_to_52)*100,digits = 2)
    Indicators_PRI$Value[50] <- round((sum(get_data_df$i50_add,na.rm = TRUE)/denominator_45_to_52)*100,digits = 2)
    Indicators_PRI$Value[51] <- round((sum(get_data_df$i51_add,na.rm = TRUE)/denominator_45_to_52)*100,digits = 2)
    Indicators_PRI$Value[52] <- round((sum(get_data_df$i52_add,na.rm = TRUE)/denominator_45_to_52)*100,digits = 2)
    # Indicators_PRI$Value[46] <- round(mean((get_data_df$i46_add/denominator_45_to_52)*100, na.rm = TRUE),digits = 0)
    # Indicators_PRI$Value[47] <- round(mean((get_data_df$i47_add/denominator_45_to_52)*100, na.rm = TRUE),digits = 0)
    # Indicators_PRI$Value[48] <- round(mean((get_data_df$i48_add/denominator_45_to_52)*100, na.rm = TRUE),digits = 0)
    # Indicators_PRI$Value[49] <- round(mean((get_data_df$i49_add/denominator_45_to_52)*100, na.rm = TRUE),digits = 0)
    # Indicators_PRI$Value[50] <- round(mean((get_data_df$i50_add/denominator_45_to_52)*100, na.rm = TRUE),digits = 0)
    # Indicators_PRI$Value[51] <- round(mean((get_data_df$i51_add/denominator_45_to_52)*100, na.rm = TRUE),digits = 0)
    # Indicators_PRI$Value[52] <- round(mean((get_data_df$i52_add/denominator_45_to_52)*100, na.rm = TRUE),digits = 0)
    # 
    # ####print(Indicators_PRI)
    # return(Indicators_PRI)
    
    # ####print("Hello I am at end of getIndVal()")
    
    testdata <- Indicators_PRI 
    
    # print(testdata)
    
    df_with_button <- 
    #   as.data.frame
    # (
      cbind(
      View = shinyInput(actionButton, nrow(testdata),'button_', label = "View", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' ),
      testdata)
      # )
    
     df_with_button[22,1] <- ""
     df_with_button[28,1] <- ""
     df_with_button[37,1] <- ""
     df_with_button[44,1] <- ""
    
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

sketch2 <- htmltools::withTags(table(
  class = "display",
  style = "bootstrap",
  tableHeader(c(
     "Graph","ID","Indicator","Unit","Value"))
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
  
      DT::datatable(getIndVal()
                   
                  %>% filter(Category == "Health and Family Welfare") 
                  %>% 
                  select(
                     View,
                    S_No,Indicator,Unit,Value)
                 , container = sketch2, options = opts1, selection = 'single',
                  class = 'cell-border stripe',
                  # caption = 'Table 1: Health and Family Welfare Indicators.',
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
 
  df_ind_block <- getIndBlock()
  df_ind_block <- df_ind_block[!duplicated(df_ind_block[,c("block_parishad_name")]),]
  # print("DF IND BLK")
  
  df_ind_gp <- getIndGP()
  df_ind_gp <- df_ind_gp[!duplicated(df_ind_gp[,c("block_parishad_name","gram_parishad_name")]),]
  # print("DF IND GP")
  
  df_ind <- getIndGS()
  # print("DF_INDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD")
  df_ind <- df_ind[!duplicated(df_ind[,c("block_parishad_name","gram_parishad_name","gram_sansad_name")]),]
  # print("DF IND GS")
  # print(str(df_ind))
  
  # ind_name <- paste("mean","(","i",SelectedRow(),",","na.rm=TRUE",")",sep = "")
  ind_name <- paste("round","(","mean","(","i",SelectedRow(),",","na.rm=TRUE",")",",","digits=2",")",sep = "")
  
  ind_name_gs <- paste0("i",SelectedRow())
  summ_name <- paste0('mean_', "i",SelectedRow())
  
#### PRINT #######################################################
# print(ind_name_gs)
  df_ind_block$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_block$gram_parishad_name))
  df_ind_gp$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_gp$gram_parishad_name))
  df_ind$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind$gram_parishad_name))
  
  df_ind_block$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_block$block_parishad_name))
  df_ind_gp$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_gp$block_parishad_name))
  df_ind$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind$block_parishad_name))
  
  
  # {{{{{{  FOR BLOCK LEVEL GRAPH }}}}}}
  
  by_block <- 
    df_ind_block %>% group_by(block_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  colnames(by_block)<- c("name","y")
  by_block$y <- as.integer(by_block$y )
  by_block$drilldown <- tolower(gsub("[[:blank:]]", "", by_block$name))
  block_df_ind1 <<- as.data.frame(by_block)
  
  # print("BLOCK DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF (FOR GRAPH) COLUMNS: NAME,Y, DRILLDOWN ")
  # print(str(block_df_ind1))
  
  
  # {{{{{{{{{{{{{{{{{{ FOR GP LEVEL GRAPH }}}}}}}}}}}}}}}}}}
  by_gp <- 
    df_ind_gp %>% group_by(block_parishad_name, gram_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  colnames(by_gp)<- c("block_parishad_name","gram_parishad_name","value")
  
  by_gp$drilldown <- tolower(gsub("[[:blank:]]", "", paste0(by_gp$block_parishad_name,by_gp$gram_parishad_name)))
  by_gp$block <- tolower(gsub("[[:blank:]]", "", by_gp$block_parishad_name))
  by_gp$gp <- tolower(gsub("[[:blank:]]", "", by_gp$gram_parishad_name))
  by_gp$block_gp <- tolower(gsub("[[:blank:]]", "", paste0(by_gp$block_parishad_name,by_gp$gram_parishad_name)))
  
  gp_df_ind1 <<- as.data.frame(by_gp)
  

  # series_list_gp1 <- vector("list",nrow(block_df_ind1))
  
  # gp_df_ind1 <- as.data.table(gp_df_ind1)
  
  series_list_gp1 <<- apply(block_df_ind1, 1, function(row) {
    
    list(
      id = as.character(row["drilldown"]),
      name = "Gram Panchayat",
      data = list_parse
      (data_frame(
        # name = filter(gp_df_ind1, block == as.character(row["drilldown"]))$gram_parishad_name,
        # name = gp_df_ind1[,gram_parishad_name,block == as.character(row["drilldown"])],
        name = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$gram_parishad_name,
        y = round(setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$value, digits = 2),
        drilldown = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$block_gp
        # y = round(filter(gp_df_ind1, block == as.character(row["drilldown"]))$value,digits = 2),
        # drilldown = filter(gp_df_ind1, block == as.character(row["drilldown"]))$block_gp
        ))
    )
  })
  
  
 #  for (i in 1:nrow(block_df_ind1))
 #  {
 # 
 #    series_list_gp1[[i]] <<- list( id = block_df_ind1$drilldown[i],
 #                                   name = "Gram Panchayat",
 #                                   data = list_parse
 #                                 (data_frame(
 #                                   name = filter(gp_df_ind1, block == block_df_ind1$drilldown[i])$gram_parishad_name,
 #                                   y = round(filter(gp_df_ind1, block == block_df_ind1$drilldown[i])$value,digits = 2),
 #                                   drilldown = filter(gp_df_ind1, block == block_df_ind1$drilldown[i])$block_gp)
 #                                 )
 #    )
 # # print("i am in loop 1")
 #  }
   # print("FOR LOOOOOOOOOOOOOOOOOOOOOOOOOOOOOP")
   # print(series_list_gp1)
  
  ########################## lapply() instead of for loop ####################
  
  # f_split_lapply <- function (df)
  #   { df <- split(df, seq_len(nrow(df)))
  #     lapply(df, function(row) as.list(id = df$drilldown[row],
  #                                      name = "Gram Panchayat",
  #                                      data = list_parse
  #                                      (data_frame(
  #                                        name = filter(gp_df_ind1, block == df$drilldown[row])$gram_parishad_name,
  #                                        y = round(filter(gp_df_ind1, block == df$drilldown[row])$value,digits = 2),
  #                                        # )
  #                                        drilldown = filter(gp_df_ind1, block == df$drilldown[row])$block_gp)  
  #                                      )))
  # }
  #  
  # print("LAPPLY")
  # series_list_gp2 <- f_split_lapply(block_df_ind1)
  # print(head(str(series_list_gp2)))
  
  
  ############################################################################
  
  
  
  
  
   # print("###############################series_list_gp1####################################")
   # print(str(series_list_gp1))
  
 
  # series_list_gs1 <- vector("list",nrow(gp_df_ind1))
  
  series_list_gs1 <<- apply(gp_df_ind1, 1, function(row) {
  # print(which(colnames(df_ind)== ind_name_gs))
  # print(ind_name_gs)
  # print(setDT(df_ind)[block_parishad == as.character(row["block"]) & gram_parishad == as.character(row["gp"])]$gram_sansad_name)
  # print(unlist
  #       (filter(df_ind,block_parishad == row["block"], gram_parishad == row["gp"])[,which(colnames(df_ind)== ind_name_gs)]))
  # print(as.integer(unlist(setDT(df_ind)[block_parishad == as.character(row["block"]) & gram_parishad == as.character(row["gp"]),
  #                     which(colnames(df_ind)== ind_name_gs), with = FALSE])))
    
   list( 
    id = as.character(row["block_gp"]),
    name = "Gram Sansad",
    data = list_parse
    (data_frame(
      name = setDT(df_ind)[block_parishad == as.character(row["block"]) & gram_parishad == as.character(row["gp"])]$gram_sansad_name,
      # name = filter(df_ind, block_parishad == row["block"],gram_parishad == row["gp"])$gram_sansad_name,
       # y = unlist
       # (filter(df_ind,block_parishad == row["block"], gram_parishad == row["gp"])[,which(colnames(df_ind)== ind_name_gs)]) 
      y=as.integer(unlist(setDT(df_ind)[block_parishad == as.character(row["block"]) & gram_parishad == as.character(row["gp"]),
                                        which(colnames(df_ind)== ind_name_gs), with = FALSE]))
 
      )) 
    ) 
  })
  
  # print("apply RESULTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT")
  # print(series_list_gs1)
  
  
  

  # for (i in 1:nrow(gp_df_ind1))
  # {
  #   # print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
  #   # print(filter(df_ind, block_parishad == gp_df_ind2$block[i],gram_parishad == gp_df_ind2$gp[i])$gram_sansad_name)
  #   # print(str(filter(df_ind,block_parishad == gp_df_ind2$block[i], gram_parishad == gp_df_ind2$gp[i])[,which(colnames(df_ind)== ind_name_gs)]))
  #   #
  # 
  #   series_list_gs1[[i]] <<- list( id = gp_df_ind1$block_gp[i],
  #                                name = "Gram Sansad",
  #                                data = list_parse
  #                                (data_frame(
  #                                  name = filter(df_ind, block_parishad == gp_df_ind1$block[i],gram_parishad == gp_df_ind1$gp[i])$gram_sansad_name,
  #                                  y = unlist
  #                                  (round(filter(df_ind,block_parishad == gp_df_ind1$block[i], gram_parishad == gp_df_ind1$gp[i])[,which(colnames(df_ind)== ind_name_gs)]
  #                                                                   ,digits = 2)))
  #                                )
  #   )
  # }


 if(input$tabs == "hfw_report"){
  toggleModal(session, "modalExample", "open")
   }else if (input$tabs == "cd_report"){
  toggleModal(session, "modalExample1", "open")
   }else if (input$tabs == "prd_report"){
     toggleModal(session, "modalExample2", "open")
   }

 })
   
output$popup2 <- renderUI({
  bsModal("modalExample2", indicators_master$Indicator[SelectedRow()], "", size = "large",
          # highchartOutput("indicators", height = "400px")
          column(12, 
                 
                 # print("In HighChart")
                 # print(str(block_df_ind1))
                 # print("Seriessssssssssssssssssssssssss GP")
                 # print(str(series_list_gp1))
                 # print("Seriessssssssssssssssssssssssss GS")
                 # print(str(series_list_gs1))
                 
                 highchart() %>%
                   hc_chart(
                     type = "column",
                     width = 800,
                     events = list(
                       drilldown = 
                         JS('function(e) {
                            console.log(e.seriesOptions);
                            this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
                            
} '),
        drillup = 
          JS('function(e) {
             console.log(e.seriesOptions);
             this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
} ')
        
                         )) %>%  
          
          
          # hc_title(text = "Panchayat Samiti") %>%
          hc_title(text = "Panchayat Samiti",
                   # margin = 20,
                   align = "centre",
                   style = list(color = "#2171b5", useHTML = TRUE)) %>%
          
          hc_xAxis(type = "category"
                   
          ) %>%
          
          hc_yAxis(title = list(text = "")
                   # ,
                   # plotLines = list(
                   #   list(label = list(
                   #     text = ""
                   #     # paste0("District Average"," = ",round(mean(block_df_ind$y),digits = 0))
                   #   ),
                   #   color = "#00ffff",
                   #   width = 3.0
                   #   ,
                   # #   value = mean(block_df_ind1$y)
                   #   ))
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
            data = list_parse(block_df_ind1),
            # colorByPoint = TRUE,
            color = "#2171b5"
            
          ) %>%
          
          hc_drilldown(
            allowPointDrilldown = TRUE,
            series = c(series_list_gp1,series_list_gs1),
            colorByPoint = TRUE
          )
        )
          )
  
})

output$popup1 <- renderUI({
       bsModal("modalExample1", indicators_master$Indicator[SelectedRow()], "", size = "large",
               # highchartOutput("indicators", height = "400px")
               column(12, 
                      
                      # print("In HighChart")
                      # print(str(block_df_ind1))
                      # print("Seriessssssssssssssssssssssssss GP")
                      # print(str(series_list_gp1))
                      # print("Seriessssssssssssssssssssssssss GS")
                      # print(str(series_list_gs1))
                      
                      highchart() %>%
                        hc_chart(
                          type = "column",
                          width = 800,
                          events = list(
                            drilldown = 
                              JS('function(e) {
                                 console.log(e.seriesOptions);
                                 this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
                                 
} '),
        drillup = 
          JS('function(e) {
             console.log(e.seriesOptions);
             this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
} ')
        
                              )) %>%  
                        
                        
                        # hc_title(text = "Panchayat Samiti") %>%
                        hc_title(text = "Panchayat Samiti",
                                 # margin = 20,
                                 align = "centre",
                                 style = list(color = "#2171b5", useHTML = TRUE)) %>%
                        
                        hc_xAxis(type = "category"
                                 
                        ) %>%
                        
          hc_yAxis(title = list(text = "")
                   # ,
                   # plotLines = list(
                   #   list(label = list(
                   #     text = ""
                   #     # paste0("District Average"," = ",round(mean(block_df_ind$y),digits = 0))
                   #   ),
                   #   color = "#00ffff",
                   #   width = 3.0
                   #   ,
                   # #   value = mean(block_df_ind1$y)
                   #   ))
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
                          data = list_parse(block_df_ind1),
                          # colorByPoint = TRUE,
                          color = "#2171b5"
                          
                        ) %>%
                        
                        hc_drilldown(
                          allowPointDrilldown = TRUE,
                          series = c(series_list_gp1,series_list_gs1),
                          colorByPoint = TRUE
                        )
                              )
               )
      
  })

output$popup <- renderUI({
  bsModal("modalExample", 
           indicators_master$Indicator[SelectedRow()],
          "", size = "large",
          # highchartOutput("indicators", height = "400px")
          column(12, 
                 
                 # print("In HighChart")
                 # print(str(block_df_ind1))
                 # print("Seriessssssssssssssssssssssssss GP")
                 # print(str(series_list_gp1))
                 # print("Seriessssssssssssssssssssssssss GS")
                 # print(str(series_list_gs1))
                 
                 highchart() %>%
                   hc_chart(
                     type = "column",
                     width = 800,
                     events = list(
                       drilldown = 
                         JS('function(e) {
                            console.log(e.seriesOptions);
                            this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
                            
} '),
        drillup = 
          JS('function(e) {
             console.log(e.seriesOptions);
             this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
} ')
        
                         )) %>%  
          
          
          # hc_title(text = "Panchayat Samiti") %>%
          hc_title(text = "Panchayat Samiti",
                   # margin = 20,
                   align = "centre",
                   style = list(color = "#2171b5", useHTML = TRUE)) %>%
          
          hc_xAxis(type = "category"
                   
          ) %>%
          
          hc_yAxis(title = list(text = "")
                   # ,
                   # plotLines = list(
                   #   list(label = list(
                   #     text = ""
                   #     # paste0("District Average"," = ",round(mean(block_df_ind$y),digits = 0))
                   #   ),
                   #   color = "#00ffff",
                   #   width = 3.0
                   #   ,
                   # #   value = mean(block_df_ind1$y)
                   #   ))
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
            data = list_parse(block_df_ind1),
            # colorByPoint = TRUE,
            color = "#2171b5"
            
          ) %>%
          
          hc_drilldown(
            allowPointDrilldown = TRUE,
            series = c(series_list_gp1,series_list_gs1),
            colorByPoint = TRUE
          )
                         )
          )
  
})

output$indicators <- renderHighchart ({
  
  # print("In HighChart")
  # print(str(block_df_ind1))
  # print("Seriessssssssssssssssssssssssss GP")
  # print(str(series_list_gp1))
  # print("Seriessssssssssssssssssssssssss GS")
  # print(str(series_list_gs1))
 
  highchart() %>%
    hc_chart(
      type = "column",
      events = list(
        drilldown = 
          JS('function(e) {
             console.log(e.seriesOptions);
             this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
             
} '),
        drillup = 
          JS('function(e) {
             console.log(e.seriesOptions);
             this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
} ')
        
          )) %>%  
    
    
    # hc_title(text = "Panchayat Samiti") %>%
    hc_title(text = "Panchayat Samiti",
             # margin = 20,
             align = "centre",
             style = list(color = "#2171b5", useHTML = TRUE)) %>%
    
    hc_xAxis(type = "category"
             
    ) %>%
    
    hc_yAxis(title = list(text = "Percentage"),
             plotLines = list(
               list(label = list(
                 text = "" 
                 # paste0("District Average"," = ",round(mean(block_df_ind$y),digits = 0))
               ),
               color = "#00ffff",
               width = 3.0
               ,
               value = mean(block_df_ind1$y)
               ))
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
      data = list_parse(block_df_ind1),
      # colorByPoint = TRUE,
      color = "#2171b5"
      
    ) %>%
    
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = c(series_list_gp1,series_list_gs1),
      colorByPoint = TRUE
    )
  
  
  
})
  

output$child_development = DT::renderDataTable( 
    
    # ####print("Data set input")
    # 
    # ####print(datasetInput())
   
    DT::datatable(getIndVal() %>% filter(Category == "Child Development") %>% 
                    select(
                       View,
                      S_No,Indicator,Unit,Value) 
                  , container = sketch2, options = opts1, selection = 'single',
                  class = 'cell-border stripe',
                  # caption = 'Table 2: Child Development Indicators.',
                  extensions = 'Buttons',escape = FALSE,rownames= FALSE
                  
                  # filter = 'top'
    )
    %>%
      formatStyle('Value',  color = 'black', backgroundColor = '#fffdd5', fontWeight = 'bold',`font-size` = '18px') %>%
      formatStyle('Indicator',  color = 'black', `font-size` = '14px')
    
    )
  
output$panchayat_rural_development = DT::renderDataTable( 
    
    DT::datatable(getIndVal() %>% filter(Category == "Panchayat and Rural Development") %>% 
                    select(
                      View,
                      S_No,Indicator,Unit,Value) 
                  , container = sketch2, options = opts1, selection = 'single',
                  class = 'cell-border stripe',
                  # caption = 'Table 3: Panchayat and Rural Development Indicators.',
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
  
  df_ind_block <- getIndBlock()
  df_ind_block <- df_ind_block[!duplicated(df_ind_block[,c("block_parishad_name")]),]
  # print("DF IND BLK")
  
  df_ind_gp <- getIndGP()
  df_ind_gp <- df_ind_gp[!duplicated(df_ind_gp[,c("block_parishad_name","gram_parishad_name")]),]
  # print("DF IND GP")
  
  df_ind <- getIndGS()
  # print("DF_INDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD")
  df_ind <- df_ind[!duplicated(df_ind[,c("block_parishad_name","gram_parishad_name","gram_sansad_name")]),]
  # print("DF IND GS")
  # print(str(df_ind))
  
  ind_name <- paste("mean","(","i",5,",","na.rm=TRUE",")",sep = "")
  
  ind_name_gs <- paste0("i",5)
  summ_name <- paste0('mean_', "i",5)
  
  #### PRINT #######################################################
  # print(ind_name_gs)
  df_ind_block$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_block$gram_parishad_name))
  df_ind_gp$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_gp$gram_parishad_name))
  df_ind$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind$gram_parishad_name))
  
  df_ind_block$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_block$block_parishad_name))
  df_ind_gp$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_gp$block_parishad_name))
  df_ind$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind$block_parishad_name))
  
  
  # {{{{{{  FOR BLOCK LEVEL GRAPH }}}}}}
  
  by_block <- 
    df_ind_block %>% group_by(block_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  colnames(by_block)<- c("name","y")
  by_block$y <- as.integer(by_block$y )
  by_block$drilldown <- tolower(gsub("[[:blank:]]", "", by_block$name))
  block_df_ind1 <<- as.data.frame(by_block)
  
  # print("BLOCK DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF (FOR GRAPH) COLUMNS: NAME,Y, DRILLDOWN ")
  # print(str(block_df_ind1))
  
  
  # {{{{{{{{{{{{{{{{{{ FOR GP LEVEL GRAPH }}}}}}}}}}}}}}}}}}
  by_gp <- 
    df_ind_gp %>% group_by(block_parishad_name, gram_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  colnames(by_gp)<- c("block_parishad_name","gram_parishad_name","value")
  
  by_gp$drilldown <- tolower(gsub("[[:blank:]]", "", paste0(by_gp$block_parishad_name,by_gp$gram_parishad_name)))
  by_gp$block <- tolower(gsub("[[:blank:]]", "", by_gp$block_parishad_name))
  by_gp$gp <- tolower(gsub("[[:blank:]]", "", by_gp$gram_parishad_name))
  by_gp$block_gp <- tolower(gsub("[[:blank:]]", "", paste0(by_gp$block_parishad_name,by_gp$gram_parishad_name)))
  
  gp_df_ind1 <<- as.data.frame(by_gp)
  
  # print(str(gp_df_ind1))
  # print(mean(block_df_ind1$y))
  
  series_list_gp <<- apply(block_df_ind1, 1, function(row) {
    
    list(
      id = as.character(row["drilldown"]),
      name = "Gram Panchayat",
      data = list_parse
      (data_frame(
        # name = filter(gp_df_ind1, block == as.character(row["drilldown"]))$gram_parishad_name,
        # name = gp_df_ind1[,gram_parishad_name,block == as.character(row["drilldown"])],
        name = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$gram_parishad_name,
        y = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$value,
        drilldown = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$block_gp
        # y = round(filter(gp_df_ind1, block == as.character(row["drilldown"]))$value,digits = 2),
        # drilldown = filter(gp_df_ind1, block == as.character(row["drilldown"]))$block_gp
      ))
    )
  })
  
  
  series_list_gs <<- apply(gp_df_ind1, 1, function(row) {
    # print("a")
    # print("name")
    # print(row)
    
    list(
      id = as.character(row["block_gp"]),
      name = "Gram Sansad",
      data = list_parse
      (data_frame(
        name = setDT(df_ind)[block_parishad == as.character(row["block"]) & gram_parishad == as.character(row["gp"])]$gram_sansad_name,
        # name = filter(df_ind, block_parishad == row["block"],gram_parishad == row["gp"])$gram_sansad_name,
        # y = unlist
        # (filter(df_ind,block_parishad == row["block"], gram_parishad == row["gp"])[,which(colnames(df_ind)== ind_name_gs)])
        # y = unlist(setDT(df_ind)[block_parishad == as.character(row["block"]) 
        #                          & gram_parishad == as.character(row["gp"])][,which(colnames(df_ind)== ind_name_gs)])
        y=as.integer(unlist(setDT(df_ind)[block_parishad == as.character(row["block"]) & gram_parishad == as.character(row["gp"]),
                                          which(colnames(df_ind)== ind_name_gs), with = FALSE]))
        
      ))
    )
  })
  
  
  # series_list_gs <- vector("list",nrow(gp_df_ind1))
  # 
  # for (i in 1:nrow(gp_df_ind1))
  # {
  #   # print("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
  #   # print(filter(df_ind, block_parishad == gp_df_ind2$block[i],gram_parishad == gp_df_ind2$gp[i])$gram_sansad_name)
  #   # print(str(filter(df_ind,block_parishad == gp_df_ind2$block[i], gram_parishad == gp_df_ind2$gp[i])[,which(colnames(df_ind)== ind_name_gs)]))
  #   # 
  #   
  #   series_list_gs[[i]] <<- list(id = gp_df_ind1$block_gp[i],
  #                                  name = "Gram Sansad",
  #                                  data = list_parse
  #                                  (data_frame(
  #                                    name = filter(df_ind, block_parishad == gp_df_ind1$block[i],gram_parishad == gp_df_ind1$gp[i])$gram_sansad_name,
  #                                    y = unlist
  #                                    (filter(df_ind,block_parishad == gp_df_ind1$block[i], gram_parishad == gp_df_ind1$gp[i])[,which(colnames(df_ind)== ind_name_gs)]
  #                                    ))
  #                                  )
  #   )
  # }
 
 
# series_list_gp <- series_list_gp  
  
highchart() %>%
    hc_chart(
      type = "column",
      events = list(
        drilldown = 
          JS('function(e) {
             console.log(e.seriesOptions);
             this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
             
            } '),
        drillup = 
          JS('function(e) {
             console.log(e.seriesOptions);
             this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
           } ')
        
        )) %>%  
    
    
    # hc_title(text = "Panchayat Samiti") %>%
     hc_title(text = "Panchayat Samiti",
               # margin = 20,
               align = "centre",
              style = list(color = "#2171b5", useHTML = TRUE)) %>%
    
  hc_xAxis(type = "category"
          
  ) %>%
    
  hc_yAxis(title = list(text = "")
           # ,
           # plotLines = list(
           #   list(label = list(
           #     text = ""
           #     # paste0("District Average"," = ",round(mean(block_df_ind$y),digits = 0))
           #   ),
           #   color = "#00ffff",
           #   width = 3.0
           #   ,
           # #   value = mean(block_df_ind1$y)
           #   ))
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
      data = list_parse(block_df_ind1),
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
    
  df_ind_block <- getIndBlock()
  df_ind_block <- df_ind_block[!duplicated(df_ind_block[,c("block_parishad_name")]),]
  # print("DF IND BLK")
  
  df_ind_gp <- getIndGP()
  df_ind_gp <- df_ind_gp[!duplicated(df_ind_gp[,c("block_parishad_name","gram_parishad_name")]),]
  # print("DF IND GP")
  
  df_ind <- getIndGS()
  # print("DF_INDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD")
  df_ind <- df_ind[!duplicated(df_ind[,c("block_parishad_name","gram_parishad_name","gram_sansad_name")]),]
  # print("DF IND GS")
  # print(str(df_ind))
  
  ind_name <- paste("mean","(","i",7,",","na.rm=TRUE",")",sep = "")
  
  ind_name_gs <- paste0("i",7)
  summ_name <- paste0('mean_', "i",7)
  
  #### PRINT #######################################################
  # print(ind_name_gs)
  df_ind_block$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_block$gram_parishad_name))
  df_ind_gp$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_gp$gram_parishad_name))
  df_ind$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind$gram_parishad_name))
  
  df_ind_block$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_block$block_parishad_name))
  df_ind_gp$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_gp$block_parishad_name))
  df_ind$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind$block_parishad_name))
  
  
  # {{{{{{  FOR BLOCK LEVEL GRAPH }}}}}}
  
  by_block <- 
    df_ind_block %>% group_by(block_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  colnames(by_block)<- c("name","y")
  by_block$y <- as.integer(by_block$y )
  by_block$drilldown <- tolower(gsub("[[:blank:]]", "", by_block$name))
  block_df_ind1 <<- as.data.frame(by_block)
  
  # print("BLOCK DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF (FOR GRAPH) COLUMNS: NAME,Y, DRILLDOWN ")
  # print(str(block_df_ind1))
  
  
  # {{{{{{{{{{{{{{{{{{ FOR GP LEVEL GRAPH }}}}}}}}}}}}}}}}}}
  by_gp <- 
    df_ind_gp %>% group_by(block_parishad_name, gram_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  colnames(by_gp)<- c("block_parishad_name","gram_parishad_name","value")
  
  by_gp$drilldown <- tolower(gsub("[[:blank:]]", "", paste0(by_gp$block_parishad_name,by_gp$gram_parishad_name)))
  by_gp$block <- tolower(gsub("[[:blank:]]", "", by_gp$block_parishad_name))
  by_gp$gp <- tolower(gsub("[[:blank:]]", "", by_gp$gram_parishad_name))
  by_gp$block_gp <- tolower(gsub("[[:blank:]]", "", paste0(by_gp$block_parishad_name,by_gp$gram_parishad_name)))
  
  gp_df_ind1 <<- as.data.frame(by_gp)
  
  # print(str(gp_df_ind1))
  # print(mean(block_df_ind1$y))
  
  
  series_list_gp <<- apply(block_df_ind1, 1, function(row) {
    
    list(
      id = as.character(row["drilldown"]),
      name = "Gram Panchayat",
      data = list_parse
      (data_frame(
        # name = filter(gp_df_ind1, block == as.character(row["drilldown"]))$gram_parishad_name,
        # name = gp_df_ind1[,gram_parishad_name,block == as.character(row["drilldown"])],
        name = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$gram_parishad_name,
        y = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$value,
        drilldown = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$block_gp
        # y = round(filter(gp_df_ind1, block == as.character(row["drilldown"]))$value,digits = 2),
        # drilldown = filter(gp_df_ind1, block == as.character(row["drilldown"]))$block_gp
      ))
    )
  })
  
  
  series_list_gs <<- apply(gp_df_ind1, 1, function(row) {
    # print("a")
    # print("name")
    # print(row)
    
    list(
      id = as.character(row["block_gp"]),
      name = "Gram Sansad",
      data = list_parse
      (data_frame(
        name = setDT(df_ind)[block_parishad == as.character(row["block"]) & gram_parishad == as.character(row["gp"])]$gram_sansad_name,
        # name = filter(df_ind, block_parishad == row["block"],gram_parishad == row["gp"])$gram_sansad_name,
        # y = unlist
        # (filter(df_ind,block_parishad == row["block"], gram_parishad == row["gp"])[,which(colnames(df_ind)== ind_name_gs)])
        y=as.integer(unlist(setDT(df_ind)[block_parishad == as.character(row["block"]) & gram_parishad == as.character(row["gp"]),
                                          which(colnames(df_ind)== ind_name_gs), with = FALSE]))
        
      ))
    )
  })
  
  series_list_gp <- series_list_gp  
  
  
  highchart() %>%
    hc_chart(
      type = "column",
      events = list(
        drilldown = 
          JS('function(e) {
             console.log(e.seriesOptions);
             this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
             
} '),
        drillup = 
          JS('function(e) {
             console.log(e.seriesOptions);
             this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
} ')
        
          )) %>%  
    
    
    # hc_title(text = "Panchayat Samiti") %>%
    hc_title(text = "Panchayat Samiti",
             # margin = 20,
             align = "centre",
             style = list(color = "#2171b5", useHTML = TRUE)) %>%
    
    hc_xAxis(type = "category"
             
    ) %>%
    hc_yAxis(title = list(text = "")
             # ,
             # plotLines = list(
             #   list(label = list(
             #     text = ""
             #     # paste0("District Average"," = ",round(mean(block_df_ind$y),digits = 0))
             #   ),
             #   color = "#00ffff",
             #   width = 3.0
             #   ,
             # #   value = mean(block_df_ind1$y)
             #   ))
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
      data = list_parse(block_df_ind1),
      # colorByPoint = TRUE,
      color = "#2171b5"
      
    ) %>%
    
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = c(series_list_gp,series_list_gs),
      colorByPoint = TRUE
    )
    
    
  })      
  

output$completeImmunization <- renderHighchart({
  
  df_ind_block <- getIndBlock()
  df_ind_block <- df_ind_block[!duplicated(df_ind_block[,c("block_parishad_name")]),]
  # print("DF IND BLK")
  
  df_ind_gp <- getIndGP()
  df_ind_gp <- df_ind_gp[!duplicated(df_ind_gp[,c("block_parishad_name","gram_parishad_name")]),]
  # print("DF IND GP")
  
  df_ind <- getIndGS()
  # print("DF_INDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD")
  df_ind <- df_ind[!duplicated(df_ind[,c("block_parishad_name","gram_parishad_name","gram_sansad_name")]),]
  # print("DF IND GS")
  # print(str(df_ind))
  
  ind_name <- paste("mean","(","i",10,",","na.rm=TRUE",")",sep = "")
  
  ind_name_gs <- paste0("i",10)
  summ_name <- paste0('mean_', "i",10)
  
  #### PRINT #######################################################
  # print(ind_name_gs)
  df_ind_block$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_block$gram_parishad_name))
  df_ind_gp$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_gp$gram_parishad_name))
  df_ind$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind$gram_parishad_name))
  
  df_ind_block$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_block$block_parishad_name))
  df_ind_gp$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_gp$block_parishad_name))
  df_ind$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind$block_parishad_name))
  
  
  # {{{{{{  FOR BLOCK LEVEL GRAPH }}}}}}
  
  by_block <- 
    df_ind_block %>% group_by(block_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  colnames(by_block)<- c("name","y")
  by_block$y <- as.integer(by_block$y )
  by_block$drilldown <- tolower(gsub("[[:blank:]]", "", by_block$name))
  block_df_ind1 <<- as.data.frame(by_block)
  
  # print("BLOCK DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF (FOR GRAPH) COLUMNS: NAME,Y, DRILLDOWN ")
  # print(str(block_df_ind1))
  
  
  # {{{{{{{{{{{{{{{{{{ FOR GP LEVEL GRAPH }}}}}}}}}}}}}}}}}}
  by_gp <- 
    df_ind_gp %>% group_by(block_parishad_name, gram_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  colnames(by_gp)<- c("block_parishad_name","gram_parishad_name","value")
  
  by_gp$drilldown <- tolower(gsub("[[:blank:]]", "", paste0(by_gp$block_parishad_name,by_gp$gram_parishad_name)))
  by_gp$block <- tolower(gsub("[[:blank:]]", "", by_gp$block_parishad_name))
  by_gp$gp <- tolower(gsub("[[:blank:]]", "", by_gp$gram_parishad_name))
  by_gp$block_gp <- tolower(gsub("[[:blank:]]", "", paste0(by_gp$block_parishad_name,by_gp$gram_parishad_name)))
  
  gp_df_ind1 <<- as.data.frame(by_gp)
  
  # print(str(gp_df_ind1))
  # print(mean(block_df_ind1$y))
  
  
  series_list_gp <<- apply(block_df_ind1, 1, function(row) {
    
    list(
      id = as.character(row["drilldown"]),
      name = "Gram Panchayat",
      data = list_parse
      (data_frame(
        # name = filter(gp_df_ind1, block == as.character(row["drilldown"]))$gram_parishad_name,
        # name = gp_df_ind1[,gram_parishad_name,block == as.character(row["drilldown"])],
        name = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$gram_parishad_name,
        y = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$value,
        drilldown = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$block_gp
        # y = round(filter(gp_df_ind1, block == as.character(row["drilldown"]))$value,digits = 2),
        # drilldown = filter(gp_df_ind1, block == as.character(row["drilldown"]))$block_gp
      ))
    )
  })
  
  
  series_list_gs <<- apply(gp_df_ind1, 1, function(row) {
    # print("a")
    # print("name")
    # print(row)
    
    list(
      id = as.character(row["block_gp"]),
      name = "Gram Sansad",
      data = list_parse
      (data_frame(
        name = setDT(df_ind)[block_parishad == as.character(row["block"]) & gram_parishad == as.character(row["gp"])]$gram_sansad_name,
        # name = filter(df_ind, block_parishad == row["block"],gram_parishad == row["gp"])$gram_sansad_name,
        # y = unlist
        # (filter(df_ind,block_parishad == row["block"], gram_parishad == row["gp"])[,which(colnames(df_ind)== ind_name_gs)])
        y=as.integer(unlist(setDT(df_ind)[block_parishad == as.character(row["block"]) & gram_parishad == as.character(row["gp"]),
                                          which(colnames(df_ind)== ind_name_gs), with = FALSE]))
        
      ))
    )
  })
  
  
  series_list_gp <- series_list_gp  
  
  
  highchart() %>%
    hc_chart(
      type = "column",
      events = list(
        drilldown = 
          JS('function(e) {
             console.log(e.seriesOptions);
             this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
             
} '),
        drillup = 
          JS('function(e) {
             console.log(e.seriesOptions);
             this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
} ')
        
          )) %>%  
    
    
    # hc_title(text = "Panchayat Samiti") %>%
    hc_title(text = "Panchayat Samiti",
             # margin = 20,
             align = "centre",
             style = list(color = "#2171b5", useHTML = TRUE)) %>%
    
    hc_xAxis(type = "category"
             
    ) %>%
    
    hc_yAxis(title = list(text = "")
             # ,
             # plotLines = list(
             #   list(label = list(
             #     text = ""
             #     # paste0("District Average"," = ",round(mean(block_df_ind$y),digits = 0))
             #   ),
             #   color = "#00ffff",
             #   width = 3.0
             #   ,
             # #   value = mean(block_df_ind1$y)
             #   ))
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
      data = list_parse(block_df_ind1),
      # colorByPoint = TRUE,
      color = "#2171b5"
      
    ) %>%
    
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = c(series_list_gp,series_list_gs),
      colorByPoint = TRUE
    )
  
  
  
  
})      


output$reportedDiaChol <- renderHighchart({
  
  df_ind_block <- getIndBlock()
  df_ind_block <- df_ind_block[!duplicated(df_ind_block[,c("block_parishad_name")]),]
  # print("DF IND BLK")
  
  df_ind_gp <- getIndGP()
  df_ind_gp <- df_ind_gp[!duplicated(df_ind_gp[,c("block_parishad_name","gram_parishad_name")]),]
  # print("DF IND GP")
  
  df_ind <- getIndGS()
  # print("DF_INDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD")
  df_ind <- df_ind[!duplicated(df_ind[,c("block_parishad_name","gram_parishad_name","gram_sansad_name")]),]
  # print("DF IND GS")
  # print(str(df_ind))
  
  ind_name <- paste("mean","(","i",11,",","na.rm=TRUE",")",sep = "")
  
  ind_name_gs <- paste0("i",11)
  summ_name <- paste0('mean_', "i",11)
  
  #### PRINT #######################################################
  # print(ind_name_gs)
  df_ind_block$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_block$gram_parishad_name))
  df_ind_gp$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_gp$gram_parishad_name))
  df_ind$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind$gram_parishad_name))
  
  df_ind_block$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_block$block_parishad_name))
  df_ind_gp$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_gp$block_parishad_name))
  df_ind$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind$block_parishad_name))
  
  
  # {{{{{{  FOR BLOCK LEVEL GRAPH }}}}}}
  
  by_block <- 
    df_ind_block %>% group_by(block_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  colnames(by_block)<- c("name","y")
  by_block$y <- as.integer(by_block$y )
  by_block$drilldown <- tolower(gsub("[[:blank:]]", "", by_block$name))
  block_df_ind1 <<- as.data.frame(by_block)
  
  # print("BLOCK DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF (FOR GRAPH) COLUMNS: NAME,Y, DRILLDOWN ")
  # print(str(block_df_ind1))
  
  
  # {{{{{{{{{{{{{{{{{{ FOR GP LEVEL GRAPH }}}}}}}}}}}}}}}}}}
  by_gp <- 
    df_ind_gp %>% group_by(block_parishad_name, gram_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  colnames(by_gp)<- c("block_parishad_name","gram_parishad_name","value")
  
  by_gp$drilldown <- tolower(gsub("[[:blank:]]", "", paste0(by_gp$block_parishad_name,by_gp$gram_parishad_name)))
  by_gp$block <- tolower(gsub("[[:blank:]]", "", by_gp$block_parishad_name))
  by_gp$gp <- tolower(gsub("[[:blank:]]", "", by_gp$gram_parishad_name))
  by_gp$block_gp <- tolower(gsub("[[:blank:]]", "", paste0(by_gp$block_parishad_name,by_gp$gram_parishad_name)))
  
  gp_df_ind1 <<- as.data.frame(by_gp)
  
  # print(str(gp_df_ind1))
  # print(mean(block_df_ind1$y))
  
  series_list_gp <<- apply(block_df_ind1, 1, function(row) {
    
    list(
      id = as.character(row["drilldown"]),
      name = "Gram Panchayat",
      data = list_parse
      (data_frame(
        # name = filter(gp_df_ind1, block == as.character(row["drilldown"]))$gram_parishad_name,
        # name = gp_df_ind1[,gram_parishad_name,block == as.character(row["drilldown"])],
        name = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$gram_parishad_name,
        y = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$value,
        drilldown = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$block_gp
        # y = round(filter(gp_df_ind1, block == as.character(row["drilldown"]))$value,digits = 2),
        # drilldown = filter(gp_df_ind1, block == as.character(row["drilldown"]))$block_gp
      ))
    )
  })
  
  
  series_list_gs <<- apply(gp_df_ind1, 1, function(row) {
    # print("a")
    # print("name")
    # print(row)
    
    list(
      id = as.character(row["block_gp"]),
      name = "Gram Sansad",
      data = list_parse
      (data_frame(
        name = setDT(df_ind)[block_parishad == as.character(row["block"]) & gram_parishad == as.character(row["gp"])]$gram_sansad_name,
        # name = filter(df_ind, block_parishad == row["block"],gram_parishad == row["gp"])$gram_sansad_name,
        # y = unlist
        # (filter(df_ind,block_parishad == row["block"], gram_parishad == row["gp"])[,which(colnames(df_ind)== ind_name_gs)])
        y=as.integer(unlist(setDT(df_ind)[block_parishad == as.character(row["block"]) & gram_parishad == as.character(row["gp"]),
                                          which(colnames(df_ind)== ind_name_gs), with = FALSE]))
        
      ))
    )
  })
  
  series_list_gp <- series_list_gp  
  
  
  highchart() %>%
    hc_chart(
      type = "column",
      events = list(
        drilldown = 
          JS('function(e) {
             console.log(e.seriesOptions);
             this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
             
} '),
        drillup = 
          JS('function(e) {
             console.log(e.seriesOptions);
             this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
} ')
        
          )) %>%  
    
    
    # hc_title(text = "Panchayat Samiti") %>%
    hc_title(text = "Panchayat Samiti",
             # margin = 20,
             align = "centre",
             style = list(color = "#2171b5", useHTML = TRUE)) %>%
    
    hc_xAxis(type = "category"
             
    ) %>%
    
    hc_yAxis(title = list(text = "")
             # ,
             # plotLines = list(
             #   list(label = list(
             #     text = ""
             #     # paste0("District Average"," = ",round(mean(block_df_ind$y),digits = 0))
             #   ),
             #   color = "#00ffff",
             #   width = 3.0
             #   ,
             # #   value = mean(block_df_ind1$y)
             #   ))
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
      data = list_parse(block_df_ind1),
      # colorByPoint = TRUE,
      color = "#2171b5"
      
    ) %>%
    
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = c(series_list_gp,series_list_gs),
      colorByPoint = TRUE
    )
  
  
})   
  

output$reportedUknownFever <- renderHighchart({
  
  df_ind_block <- getIndBlock()
  df_ind_block <- df_ind_block[!duplicated(df_ind_block[,c("block_parishad_name")]),]
  # print("DF IND BLK")
  
  df_ind_gp <- getIndGP()
  df_ind_gp <- df_ind_gp[!duplicated(df_ind_gp[,c("block_parishad_name","gram_parishad_name")]),]
  # print("DF IND GP")
  
  df_ind <- getIndGS()
  # print("DF_INDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD")
  df_ind <- df_ind[!duplicated(df_ind[,c("block_parishad_name","gram_parishad_name","gram_sansad_name")]),]
  # print("DF IND GS")
  # print(str(df_ind))
  
  ind_name <- paste("mean","(","i",12,",","na.rm=TRUE",")",sep = "")
  
  ind_name_gs <- paste0("i",12)
  summ_name <- paste0('mean_', "i",12)
  
  #### PRINT #######################################################
  # print(ind_name_gs)
  df_ind_block$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_block$gram_parishad_name))
  df_ind_gp$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_gp$gram_parishad_name))
  df_ind$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind$gram_parishad_name))
  
  df_ind_block$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_block$block_parishad_name))
  df_ind_gp$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_gp$block_parishad_name))
  df_ind$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind$block_parishad_name))
  
  
  # {{{{{{  FOR BLOCK LEVEL GRAPH }}}}}}
  
  by_block <- 
    df_ind_block %>% group_by(block_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  colnames(by_block)<- c("name","y")
  by_block$y <- as.integer(by_block$y )
  by_block$drilldown <- tolower(gsub("[[:blank:]]", "", by_block$name))
  block_df_ind1 <<- as.data.frame(by_block)
  
  # print("BLOCK DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF (FOR GRAPH) COLUMNS: NAME,Y, DRILLDOWN ")
  # print(str(block_df_ind1))
  
  
  # {{{{{{{{{{{{{{{{{{ FOR GP LEVEL GRAPH }}}}}}}}}}}}}}}}}}
  by_gp <- 
    df_ind_gp %>% group_by(block_parishad_name, gram_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  colnames(by_gp)<- c("block_parishad_name","gram_parishad_name","value")
  
  by_gp$drilldown <- tolower(gsub("[[:blank:]]", "", paste0(by_gp$block_parishad_name,by_gp$gram_parishad_name)))
  by_gp$block <- tolower(gsub("[[:blank:]]", "", by_gp$block_parishad_name))
  by_gp$gp <- tolower(gsub("[[:blank:]]", "", by_gp$gram_parishad_name))
  by_gp$block_gp <- tolower(gsub("[[:blank:]]", "", paste0(by_gp$block_parishad_name,by_gp$gram_parishad_name)))
  
  gp_df_ind1 <<- as.data.frame(by_gp)
  
  # print(str(gp_df_ind1))
  # print(mean(block_df_ind1$y))
  
  
  series_list_gp <<- apply(block_df_ind1, 1, function(row) {
    
    list(
      id = as.character(row["drilldown"]),
      name = "Gram Panchayat",
      data = list_parse
      (data_frame(
        # name = filter(gp_df_ind1, block == as.character(row["drilldown"]))$gram_parishad_name,
        # name = gp_df_ind1[,gram_parishad_name,block == as.character(row["drilldown"])],
        name = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$gram_parishad_name,
        y = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$value,
        drilldown = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$block_gp
        # y = round(filter(gp_df_ind1, block == as.character(row["drilldown"]))$value,digits = 2),
        # drilldown = filter(gp_df_ind1, block == as.character(row["drilldown"]))$block_gp
      ))
    )
  })
  
  
  series_list_gs <<- apply(gp_df_ind1, 1, function(row) {
    # print("a")
    # print("name")
    # print(row)
    
    list(
      id = as.character(row["block_gp"]),
      name = "Gram Sansad",
      data = list_parse
      (data_frame(
        name = setDT(df_ind)[block_parishad == as.character(row["block"]) & gram_parishad == as.character(row["gp"])]$gram_sansad_name,
        # name = filter(df_ind, block_parishad == row["block"],gram_parishad == row["gp"])$gram_sansad_name,
        # y = unlist
        # (filter(df_ind,block_parishad == row["block"], gram_parishad == row["gp"])[,which(colnames(df_ind)== ind_name_gs)])
        y=as.integer(unlist(setDT(df_ind)[block_parishad == as.character(row["block"]) & gram_parishad == as.character(row["gp"]),
                                          which(colnames(df_ind)== ind_name_gs), with = FALSE]))
        
      ))
    )
  })
  
  
  series_list_gp <- series_list_gp  
  
  
  highchart() %>%
    hc_chart(
      type = "column",
      events = list(
        drilldown = 
          JS('function(e) {
             console.log(e.seriesOptions);
             this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
             
} '),
        drillup = 
          JS('function(e) {
             console.log(e.seriesOptions);
             this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
} ')
        
          )) %>%  
    
    
    # hc_title(text = "Panchayat Samiti") %>%
    hc_title(text = "Panchayat Samiti",
             # margin = 20,
             align = "centre",
             style = list(color = "#2171b5", useHTML = TRUE)) %>%
    
    hc_xAxis(type = "category"
             
    ) %>%
    
    hc_yAxis(title = list(text = "")
             # ,
             # plotLines = list(
             #   list(label = list(
             #     text = ""
             #     # paste0("District Average"," = ",round(mean(block_df_ind$y),digits = 0))
             #   ),
             #   color = "#00ffff",
             #   width = 3.0
             #   ,
             # #   value = mean(block_df_ind1$y)
             #   ))
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
      data = list_parse(block_df_ind1),
      # colorByPoint = TRUE,
      color = "#2171b5"
      
    ) %>%
    
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = c(series_list_gp,series_list_gs),
      colorByPoint = TRUE
    )
  
  
})     
  

output$reportedLBW <- renderHighchart({
  
  df_ind_block <- getIndBlock()
  df_ind_block <- df_ind_block[!duplicated(df_ind_block[,c("block_parishad_name")]),]
  # print("DF IND BLK")
  
  df_ind_gp <- getIndGP()
  df_ind_gp <- df_ind_gp[!duplicated(df_ind_gp[,c("block_parishad_name","gram_parishad_name")]),]
  # print("DF IND GP")
  
  df_ind <- getIndGS()
  # print("DF_INDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD")
  df_ind <- df_ind[!duplicated(df_ind[,c("block_parishad_name","gram_parishad_name","gram_sansad_name")]),]
  # print("DF IND GS")
  # print(str(df_ind))
  
  ind_name <- paste("mean","(","i",18,",","na.rm=TRUE",")",sep = "")
  
  ind_name_gs <- paste0("i",18)
  summ_name <- paste0('mean_', "i",18)

  #### PRINT #######################################################
  # print(ind_name_gs)
  df_ind_block$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_block$gram_parishad_name))
  df_ind_gp$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_gp$gram_parishad_name))
  df_ind$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind$gram_parishad_name))
  
  df_ind_block$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_block$block_parishad_name))
  df_ind_gp$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_gp$block_parishad_name))
  df_ind$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind$block_parishad_name))
  
  
  # {{{{{{  FOR BLOCK LEVEL GRAPH }}}}}}
  
  by_block <- 
    df_ind_block %>% group_by(block_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  colnames(by_block)<- c("name","y")
  by_block$y <- as.integer(by_block$y )
  by_block$drilldown <- tolower(gsub("[[:blank:]]", "", by_block$name))
  block_df_ind1 <<- as.data.frame(by_block)
  
  # print("BLOCK DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF (FOR GRAPH) COLUMNS: NAME,Y, DRILLDOWN ")
  # print(str(block_df_ind1))
  
  
  # {{{{{{{{{{{{{{{{{{ FOR GP LEVEL GRAPH }}}}}}}}}}}}}}}}}}
  by_gp <- 
    df_ind_gp %>% group_by(block_parishad_name, gram_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  colnames(by_gp)<- c("block_parishad_name","gram_parishad_name","value")
  
  by_gp$drilldown <- tolower(gsub("[[:blank:]]", "", paste0(by_gp$block_parishad_name,by_gp$gram_parishad_name)))
  by_gp$block <- tolower(gsub("[[:blank:]]", "", by_gp$block_parishad_name))
  by_gp$gp <- tolower(gsub("[[:blank:]]", "", by_gp$gram_parishad_name))
  by_gp$block_gp <- tolower(gsub("[[:blank:]]", "", paste0(by_gp$block_parishad_name,by_gp$gram_parishad_name)))
  
  gp_df_ind1 <<- as.data.frame(by_gp)
  
  # print(str(gp_df_ind1))
  # print(mean(block_df_ind1$y))
  
  
  series_list_gp <<- apply(block_df_ind1, 1, function(row) {
    
    list(
      id = as.character(row["drilldown"]),
      name = "Gram Panchayat",
      data = list_parse
      (data_frame(
        # name = filter(gp_df_ind1, block == as.character(row["drilldown"]))$gram_parishad_name,
        # name = gp_df_ind1[,gram_parishad_name,block == as.character(row["drilldown"])],
        name = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$gram_parishad_name,
        y = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$value,
        drilldown = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$block_gp
        # y = round(filter(gp_df_ind1, block == as.character(row["drilldown"]))$value,digits = 2),
        # drilldown = filter(gp_df_ind1, block == as.character(row["drilldown"]))$block_gp
      ))
    )
  })
  
  
  series_list_gs <<- apply(gp_df_ind1, 1, function(row) {
    # print("a")
    # print("name")
    # print(row)
    
    list(
      id = as.character(row["block_gp"]),
      name = "Gram Sansad",
      data = list_parse
      (data_frame(
        name = setDT(df_ind)[block_parishad == as.character(row["block"]) & gram_parishad == as.character(row["gp"])]$gram_sansad_name,
        # name = filter(df_ind, block_parishad == row["block"],gram_parishad == row["gp"])$gram_sansad_name,
        # y = unlist
        # (filter(df_ind,block_parishad == row["block"], gram_parishad == row["gp"])[,which(colnames(df_ind)== ind_name_gs)])
        y=as.integer(unlist(setDT(df_ind)[block_parishad == as.character(row["block"]) & gram_parishad == as.character(row["gp"]),
                                          which(colnames(df_ind)== ind_name_gs), with = FALSE]))
        
      ))
    )
  })
  
  
  
  series_list_gp <- series_list_gp  
  
  
  highchart() %>%
    hc_chart(
      type = "column",
      events = list(
        drilldown = 
          JS('function(e) {
             console.log(e.seriesOptions);
             this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
             
} '),
        drillup = 
          JS('function(e) {
             console.log(e.seriesOptions);
             this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
} ')
        
          )) %>%  
    
    
    # hc_title(text = "Panchayat Samiti") %>%
    hc_title(text = "Panchayat Samiti",
             # margin = 20,
             align = "centre",
             style = list(color = "#2171b5", useHTML = TRUE)) %>%
    
    hc_xAxis(type = "category"
             
    ) %>%
    
    hc_yAxis(title = list(text = "")
             # ,
             # plotLines = list(
             #   list(label = list(
             #     text = ""
             #     # paste0("District Average"," = ",round(mean(block_df_ind$y),digits = 0))
             #   ),
             #   color = "#00ffff",
             #   width = 3.0
             #   ,
             # #   value = mean(block_df_ind1$y)
             #   ))
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
      data = list_parse(block_df_ind1),
      # colorByPoint = TRUE,
      color = "#2171b5"
      
    ) %>%
    
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = c(series_list_gp,series_list_gs),
      colorByPoint = TRUE
    )
  
  
  
})       
  
  
output$reportedSAM <- renderHighchart({
  
  df_ind_block <- getIndBlock()
  df_ind_block <- df_ind_block[!duplicated(df_ind_block[,c("block_parishad_name")]),]
  # print("DF IND BLK")
  
  df_ind_gp <- getIndGP()
  df_ind_gp <- df_ind_gp[!duplicated(df_ind_gp[,c("block_parishad_name","gram_parishad_name")]),]
  # print("DF IND GP")
  
  df_ind <- getIndGS()
  # print("DF_INDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD")
  df_ind <- df_ind[!duplicated(df_ind[,c("block_parishad_name","gram_parishad_name","gram_sansad_name")]),]
  # print("DF IND GS")
  # print(str(df_ind))
  
  ind_name <- paste("mean","(","i",20,",","na.rm=TRUE",")",sep = "")
  
  ind_name_gs <- paste0("i",20)
  summ_name <- paste0('mean_', "i",20)
  
  #### PRINT #######################################################
  # print(ind_name_gs)
  df_ind_block$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_block$gram_parishad_name))
  df_ind_gp$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_gp$gram_parishad_name))
  df_ind$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind$gram_parishad_name))
  
  df_ind_block$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_block$block_parishad_name))
  df_ind_gp$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_gp$block_parishad_name))
  df_ind$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind$block_parishad_name))
  
  
  # {{{{{{  FOR BLOCK LEVEL GRAPH }}}}}}
  
  by_block <- 
    df_ind_block %>% group_by(block_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  colnames(by_block)<- c("name","y")
  by_block$y <- as.integer(by_block$y )
  by_block$drilldown <- tolower(gsub("[[:blank:]]", "", by_block$name))
  block_df_ind1 <<- as.data.frame(by_block)
  
  # print("BLOCK DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF (FOR GRAPH) COLUMNS: NAME,Y, DRILLDOWN ")
  # print(str(block_df_ind1))
  
  
  # {{{{{{{{{{{{{{{{{{ FOR GP LEVEL GRAPH }}}}}}}}}}}}}}}}}}
  by_gp <- 
    df_ind_gp %>% group_by(block_parishad_name, gram_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  colnames(by_gp)<- c("block_parishad_name","gram_parishad_name","value")
  
  by_gp$drilldown <- tolower(gsub("[[:blank:]]", "", paste0(by_gp$block_parishad_name,by_gp$gram_parishad_name)))
  by_gp$block <- tolower(gsub("[[:blank:]]", "", by_gp$block_parishad_name))
  by_gp$gp <- tolower(gsub("[[:blank:]]", "", by_gp$gram_parishad_name))
  by_gp$block_gp <- tolower(gsub("[[:blank:]]", "", paste0(by_gp$block_parishad_name,by_gp$gram_parishad_name)))
  
  gp_df_ind1 <<- as.data.frame(by_gp)
  
  # print(str(gp_df_ind1))
  # print(mean(block_df_ind1$y))
  
  
  series_list_gp <<- apply(block_df_ind1, 1, function(row) {
    
    list(
      id = as.character(row["drilldown"]),
      name = "Gram Panchayat",
      data = list_parse
      (data_frame(
        # name = filter(gp_df_ind1, block == as.character(row["drilldown"]))$gram_parishad_name,
        # name = gp_df_ind1[,gram_parishad_name,block == as.character(row["drilldown"])],
        name = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$gram_parishad_name,
        y = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$value,
        drilldown = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$block_gp
        # y = round(filter(gp_df_ind1, block == as.character(row["drilldown"]))$value,digits = 2),
        # drilldown = filter(gp_df_ind1, block == as.character(row["drilldown"]))$block_gp
      ))
    )
  })
  
  
  series_list_gs <<- apply(gp_df_ind1, 1, function(row) {
    # print("a")
    # print("name")
    # print(row)
    
    list(
      id = as.character(row["block_gp"]),
      name = "Gram Sansad",
      data = list_parse
      (data_frame(
        name = setDT(df_ind)[block_parishad == as.character(row["block"]) & gram_parishad == as.character(row["gp"])]$gram_sansad_name,
        # name = filter(df_ind, block_parishad == row["block"],gram_parishad == row["gp"])$gram_sansad_name,
        # y = unlist
        # (filter(df_ind,block_parishad == row["block"], gram_parishad == row["gp"])[,which(colnames(df_ind)== ind_name_gs)])
        y=as.integer(unlist(setDT(df_ind)[block_parishad == as.character(row["block"]) & gram_parishad == as.character(row["gp"]),
                                          which(colnames(df_ind)== ind_name_gs), with = FALSE]))
        
      ))
    )
  })
  
  series_list_gp <- series_list_gp  
  
  
  highchart() %>%
    hc_chart(
      type = "column",
      events = list(
        drilldown = 
          JS('function(e) {
             console.log(e.seriesOptions);
             this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
             
} '),
        drillup = 
          JS('function(e) {
             console.log(e.seriesOptions);
             this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
} ')
        
          )) %>%  
    
    
    # hc_title(text = "Panchayat Samiti") %>%
    hc_title(text = "Panchayat Samiti",
             # margin = 20,
             align = "centre",
             style = list(color = "#2171b5", useHTML = TRUE)) %>%
    
    hc_xAxis(type = "category"
             
    ) %>%
    
    hc_yAxis(title = list(text = "")
             # ,
             # plotLines = list(
             #   list(label = list(
             #     text = ""
             #     # paste0("District Average"," = ",round(mean(block_df_ind$y),digits = 0))
             #   ),
             #   color = "#00ffff",
             #   width = 3.0
             #   ,
             # #   value = mean(block_df_ind1$y)
             #   ))
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
      data = list_parse(block_df_ind1),
      # colorByPoint = TRUE,
      color = "#2171b5"
      
    ) %>%
    
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = c(series_list_gp,series_list_gs),
      colorByPoint = TRUE
    )
  
  
})       


output$reportedOpenDefecated <- renderHighchart({
  
  df_ind_block <- getIndBlock()
  df_ind_block <- df_ind_block[!duplicated(df_ind_block[,c("block_parishad_name")]),]
  # print("DF IND BLK")
  
  df_ind_gp <- getIndGP()
  df_ind_gp <- df_ind_gp[!duplicated(df_ind_gp[,c("block_parishad_name","gram_parishad_name")]),]
  # print("DF IND GP")
  
  df_ind <- getIndGS()
  # print("DF_INDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD")
  df_ind <- df_ind[!duplicated(df_ind[,c("block_parishad_name","gram_parishad_name","gram_sansad_name")]),]
  # print("DF IND GS")
  # print(str(df_ind))
  
  ind_name <- paste("mean","(","i",33,",","na.rm=TRUE",")",sep = "")
  
  ind_name_gs <- paste0("i",33)
  summ_name <- paste0('mean_', "i",33)
  
  #### PRINT #######################################################
  # print(ind_name_gs)
  df_ind_block$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_block$gram_parishad_name))
  df_ind_gp$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_gp$gram_parishad_name))
  df_ind$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind$gram_parishad_name))
  
  df_ind_block$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_block$block_parishad_name))
  df_ind_gp$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_gp$block_parishad_name))
  df_ind$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind$block_parishad_name))
  
  
  # {{{{{{  FOR BLOCK LEVEL GRAPH }}}}}}
  
  by_block <- 
    df_ind_block %>% group_by(block_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  colnames(by_block)<- c("name","y")
  by_block$y <- as.integer(by_block$y )
  by_block$drilldown <- tolower(gsub("[[:blank:]]", "", by_block$name))
  block_df_ind1 <<- as.data.frame(by_block)
  
  # print("BLOCK DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF (FOR GRAPH) COLUMNS: NAME,Y, DRILLDOWN ")
  # print(str(block_df_ind1))
  
  
  # {{{{{{{{{{{{{{{{{{ FOR GP LEVEL GRAPH }}}}}}}}}}}}}}}}}}
  by_gp <- 
    df_ind_gp %>% group_by(block_parishad_name, gram_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  colnames(by_gp)<- c("block_parishad_name","gram_parishad_name","value")
  
  by_gp$drilldown <- tolower(gsub("[[:blank:]]", "", paste0(by_gp$block_parishad_name,by_gp$gram_parishad_name)))
  by_gp$block <- tolower(gsub("[[:blank:]]", "", by_gp$block_parishad_name))
  by_gp$gp <- tolower(gsub("[[:blank:]]", "", by_gp$gram_parishad_name))
  by_gp$block_gp <- tolower(gsub("[[:blank:]]", "", paste0(by_gp$block_parishad_name,by_gp$gram_parishad_name)))
  
  gp_df_ind1 <<- as.data.frame(by_gp)
  
  # print(str(gp_df_ind1))
  # print(mean(block_df_ind1$y))
  
  
  series_list_gp <<- apply(block_df_ind1, 1, function(row) {
    
    list(
      id = as.character(row["drilldown"]),
      name = "Gram Panchayat",
      data = list_parse
      (data_frame(
        # name = filter(gp_df_ind1, block == as.character(row["drilldown"]))$gram_parishad_name,
        # name = gp_df_ind1[,gram_parishad_name,block == as.character(row["drilldown"])],
        name = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$gram_parishad_name,
        y = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$value,
        drilldown = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$block_gp
        # y = round(filter(gp_df_ind1, block == as.character(row["drilldown"]))$value,digits = 2),
        # drilldown = filter(gp_df_ind1, block == as.character(row["drilldown"]))$block_gp
      ))
    )
  })
  
  
  series_list_gs <<- apply(gp_df_ind1, 1, function(row) {
    # print("a")
    # print("name")
    # print(row)
    
    list(
      id = as.character(row["block_gp"]),
      name = "Gram Sansad",
      data = list_parse
      (data_frame(
        name = setDT(df_ind)[block_parishad == as.character(row["block"]) & gram_parishad == as.character(row["gp"])]$gram_sansad_name,
        # name = filter(df_ind, block_parishad == row["block"],gram_parishad == row["gp"])$gram_sansad_name,
        # y = unlist
        # (filter(df_ind,block_parishad == row["block"], gram_parishad == row["gp"])[,which(colnames(df_ind)== ind_name_gs)])
        y=as.integer(unlist(setDT(df_ind)[block_parishad == as.character(row["block"]) & gram_parishad == as.character(row["gp"]),
                                          which(colnames(df_ind)== ind_name_gs), with = FALSE]))
        
      ))
    )
  })
  
  
  series_list_gp <- series_list_gp  
  
  
  highchart() %>%
    hc_chart(
      type = "column",
      events = list(
        drilldown = 
          JS('function(e) {
             console.log(e.seriesOptions);
             this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
             
} '),
        drillup = 
          JS('function(e) {
             console.log(e.seriesOptions);
             this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
} ')
        
          )) %>%  
    
    
    # hc_title(text = "Panchayat Samiti") %>%
    hc_title(text = "Panchayat Samiti",
             # margin = 20,
             align = "centre",
             style = list(color = "#2171b5", useHTML = TRUE)) %>%
    
    hc_xAxis(type = "category"
             
    ) %>%
    
    hc_yAxis(title = list(text = "")
             # ,
             # plotLines = list(
             #   list(label = list(
             #     text = ""
             #     # paste0("District Average"," = ",round(mean(block_df_ind$y),digits = 0))
             #   ),
             #   color = "#00ffff",
             #   width = 3.0
             #   ,
             # #   value = mean(block_df_ind1$y)
             #   ))
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
      data = list_parse(block_df_ind1),
      # colorByPoint = TRUE,
      color = "#2171b5"
      
    ) %>%
    
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = c(series_list_gp,series_list_gs),
      colorByPoint = TRUE
    )
  
  
})  
  
output$reportedExcretaToiletPan <- renderHighchart({
  
  df_ind_block <- getIndBlock()
  df_ind_block <- df_ind_block[!duplicated(df_ind_block[,c("block_parishad_name")]),]
  # print("DF IND BLK")
  
  df_ind_gp <- getIndGP()
  df_ind_gp <- df_ind_gp[!duplicated(df_ind_gp[,c("block_parishad_name","gram_parishad_name")]),]
  # print("DF IND GP")
  
  df_ind <- getIndGS()
  # print("DF_INDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD")
  df_ind <- df_ind[!duplicated(df_ind[,c("block_parishad_name","gram_parishad_name","gram_sansad_name")]),]
  # print("DF IND GS")
  # print(str(df_ind))
  
  ind_name <- paste("mean","(","i",34,",","na.rm=TRUE",")",sep = "")
  
  ind_name_gs <- paste0("i",34)
  summ_name <- paste0('mean_', "i",34)
  
  #### PRINT #######################################################
  # print(ind_name_gs)
  df_ind_block$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_block$gram_parishad_name))
  df_ind_gp$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_gp$gram_parishad_name))
  df_ind$gram_parishad <- tolower(gsub("[[:blank:]]", "", df_ind$gram_parishad_name))
  
  df_ind_block$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_block$block_parishad_name))
  df_ind_gp$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind_gp$block_parishad_name))
  df_ind$block_parishad <- tolower(gsub("[[:blank:]]", "", df_ind$block_parishad_name))
  
  
  # {{{{{{  FOR BLOCK LEVEL GRAPH }}}}}}
  
  by_block <- 
    df_ind_block %>% group_by(block_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  colnames(by_block)<- c("name","y")
  by_block$y <- as.integer(by_block$y )
  by_block$drilldown <- tolower(gsub("[[:blank:]]", "", by_block$name))
  block_df_ind1 <<- as.data.frame(by_block)
  
  # print("BLOCK DFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF (FOR GRAPH) COLUMNS: NAME,Y, DRILLDOWN ")
  # print(str(block_df_ind1))
  
  
  # {{{{{{{{{{{{{{{{{{ FOR GP LEVEL GRAPH }}}}}}}}}}}}}}}}}}
  by_gp <- 
    df_ind_gp %>% group_by(block_parishad_name, gram_parishad_name)%>% 
    summarise_(.dots = setNames(ind_name, summ_name))
  
  colnames(by_gp)<- c("block_parishad_name","gram_parishad_name","value")
  
  by_gp$drilldown <- tolower(gsub("[[:blank:]]", "", paste0(by_gp$block_parishad_name,by_gp$gram_parishad_name)))
  by_gp$block <- tolower(gsub("[[:blank:]]", "", by_gp$block_parishad_name))
  by_gp$gp <- tolower(gsub("[[:blank:]]", "", by_gp$gram_parishad_name))
  by_gp$block_gp <- tolower(gsub("[[:blank:]]", "", paste0(by_gp$block_parishad_name,by_gp$gram_parishad_name)))
  
  gp_df_ind1 <<- as.data.frame(by_gp)
  
  # print(str(gp_df_ind1))
  # print(mean(block_df_ind1$y))
  
  series_list_gp <<- apply(block_df_ind1, 1, function(row) {
    
    list(
      id = as.character(row["drilldown"]),
      name = "Gram Panchayat",
      data = list_parse
      (data_frame(
        # name = filter(gp_df_ind1, block == as.character(row["drilldown"]))$gram_parishad_name,
        # name = gp_df_ind1[,gram_parishad_name,block == as.character(row["drilldown"])],
        name = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$gram_parishad_name,
        y = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$value,
        drilldown = setDT(gp_df_ind1)[block == as.character(row["drilldown"])]$block_gp
        # y = round(filter(gp_df_ind1, block == as.character(row["drilldown"]))$value,digits = 2),
        # drilldown = filter(gp_df_ind1, block == as.character(row["drilldown"]))$block_gp
      ))
    )
  })
  
  
  series_list_gs <<- apply(gp_df_ind1, 1, function(row) {
    # print("a")
    # print("name")
    # print(row)
    
    list(
      id = as.character(row["block_gp"]),
      name = "Gram Sansad",
      data = list_parse
      (data_frame(
        name = setDT(df_ind)[block_parishad == as.character(row["block"]) & gram_parishad == as.character(row["gp"])]$gram_sansad_name,
        # name = filter(df_ind, block_parishad == row["block"],gram_parishad == row["gp"])$gram_sansad_name,
        # y = unlist
        # (filter(df_ind,block_parishad == row["block"], gram_parishad == row["gp"])[,which(colnames(df_ind)== ind_name_gs)])
        y=as.integer(unlist(setDT(df_ind)[block_parishad == as.character(row["block"]) & gram_parishad == as.character(row["gp"]),
                                          which(colnames(df_ind)== ind_name_gs), with = FALSE]))
        
      ))
    )
  })
  
  
  series_list_gp <- series_list_gp  
  
  
  highchart() %>%
    hc_chart(
      type = "column",
      events = list(
        drilldown = 
          JS('function(e) {
             console.log(e.seriesOptions);
             this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
             
} '),
        drillup = 
          JS('function(e) {
             console.log(e.seriesOptions);
             this.setTitle({text: e.seriesOptions.name || e.seriesOptions.id })
} ')
        
          )) %>%  
    
    
    # hc_title(text = "Panchayat Samiti") %>%
    hc_title(text = "Panchayat Samiti",
             # margin = 20,
             align = "centre",
             style = list(color = "#2171b5", useHTML = TRUE)) %>%
    
    hc_xAxis(type = "category"
             
    ) %>%
    
    hc_yAxis(title = list(text = "")
             # ,
             # plotLines = list(
             #   list(label = list(
             #     text = ""
             #     # paste0("District Average"," = ",round(mean(block_df_ind$y),digits = 0))
             #   ),
             #   color = "#00ffff",
             #   width = 3.0
             #   ,
             # #   value = mean(block_df_ind1$y)
             #   ))
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
      data = list_parse(block_df_ind1),
      # colorByPoint = TRUE,
      color = "#2171b5"
      
    ) %>%
    
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = c(series_list_gp,series_list_gs),
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
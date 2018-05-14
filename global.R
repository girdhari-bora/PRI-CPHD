library(shiny)
library(shinydashboard)
library(dplyr)
library(RColorBrewer)
library(dygraphs)
# library(xts)
library(highcharter)
library(dplyr)
library(DT)
# library(data.table)
library(shinyjs)
library(htmltools)
library(apputils)
# library(collapsibleTree)
# library(data.table)
# library(dplyr)
# require("httr")
library(jsonlite)
library(httr)
# library(rdrop2)
library(dplyr)
library(shinycssloaders)
library(RMariaDB)
library(DBI)
library(RMySQL)
library(dbConnect)
library(dplyr)
library(pool)
library(shiny)
library(dbplyr)
library(tools)
library(stringr)
library(parallel)
library(doParallel)
library(data.table)

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
registerDoParallel(cl)
# lapply(dbListConnections(MySQL()), dbDisconnect)

#change in global
options(spinner.color="#0dc5c1")

  # token <- readRDS("./Data/droptoken.rds")
  # get_data_df <- drop_read_csv("CPHD/get_data_df.csv", dtoken = token)
  # get_data_df %>% mutate_if(is.factor, as.character) -> get_data_df
  # unique(get_data_df$gram_sansad_name)

# 
#   set_config( config( ssl_verifypeer = 0L ))
#   get_data <- GET("https://spreadcreativity.org/master_pri/master_pri_data.php")
#   get_data_text <- content(get_data, "text")
#   get_data_json <- fromJSON(get_data_text, flatten = TRUE)
#   get_data_df_new <- as.data.frame(get_data_json[["indicatordata"]])
#   get_data_df_new %>% mutate_if(is.factor, as.character) -> get_data_df

my_db<-dbPool(
  RMySQL::MySQL(),
  user= 'primaster',
  password='pri@2017',
  dbname='spreadcr_pri',
  host='88.85.67.178'
)

village <- my_db %>% tbl("village")
gram_sansad <- my_db %>% tbl("gram_sansad")
gram_panchayat <- my_db %>% tbl("gram_panchayat")
block_parishad <- my_db %>% tbl("block_parishad")
jila_parishad <- my_db %>% tbl("jila_parishad")
block <- my_db %>% tbl("block")
district <- my_db %>% tbl("district")
indicator_data_entry <- my_db %>% tbl("indicator_data_entry")

data_all <- indicator_data_entry %>% 
  left_join(gram_sansad, by = c("gram_sansad_id" = "id")) %>% 
  left_join(gram_panchayat, by = c("gram_panchayat_id.y" = "id")) %>% 
  left_join(block_parishad, by = c("block_parishad_id" = "id"))%>% 
  left_join(jila_parishad, by = c("jila_parishad_id" = "id"))%>% 
  select(c(134,125,118),c(1:110),c(113:114)) %>% collect()

get_data_df_new <- as.data.frame(data_all)  

get_data_df_new %>% mutate_if(is.factor, as.character) -> get_data_df_new
get_data_df_new[,4:113] <- sapply(get_data_df_new[,4:113],as.integer)
get_data_df_new$month <- as.integer(get_data_df_new$month)

get_data_df_new <- get_data_df_new[!is.na(get_data_df_new["year"]),]
get_data_df_new <- get_data_df_new[!is.na(get_data_df_new["month"]),]
# get_data_df_new[is.na(get_data_df_new)] <- ""

get_data_df <- get_data_df_new

get_data_df <- filter(get_data_df, get_data_df$block_parishad_name != "Test Block 1", 
                        get_data_df$block_parishad_name != "Test Block 2", 
                      get_data_df$block_parishad_name != "TEST BLOCK PARISAD")

get_data_df_final <- get_data_df %>% 
  mutate(year_month_date = ifelse( month>3 , paste(matrix(unlist(strsplit(get_data_df$year, "-")),ncol=2, byrow = TRUE)[,1],
                                                   month,"01",sep = "-")
                                   , paste(as.integer(matrix(unlist(strsplit(get_data_df$year, "-")),ncol=2, byrow = TRUE)[,1])+1,
                                           month,"01",sep = "-"))
  )
get_data_df_final$year_month_date[as.Date(get_data_df_final$year_month_date) == as.Date("2019-1-01")] <- "2018-1-01"
get_data_df_final$year_month_date[as.Date(get_data_df_final$year_month_date) == as.Date("2019-2-01")] <- "2018-2-01"
get_data_df_final$year_month_date[as.Date(get_data_df_final$year_month_date) == as.Date("2019-3-01")] <- "2018-3-01"

get_data_df <- get_data_df_final

get_data_df <- filter(get_data_df,get_data_df$i1_add!=0,get_data_df$i1_add!="",
                      get_data_df$i2_add!=0,get_data_df$i2_add!="",
                      get_data_df$i18_add!=0,get_data_df$i18_add!="",
                      get_data_df$i36_add!=0,get_data_df$i36_add!="")


  
# get_data_df_new <- get_data_df
  
 #   print("hello I am here2")  
 # }

 # }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 


# get_data_df <- read.csv("./Data/get_data_df.csv", 
#                        header = TRUE, stringsAsFactors = FALSE ,check.names = TRUE)
indicators_master <- read.csv("./Data/Indicators PRI.csv", 
                        header = TRUE, stringsAsFactors = FALSE ,check.names = TRUE)

choices_block <- c("Select All",unique(get_data_df$block_parishad_name))
choices_panchayat <- c("Select All", unique(get_data_df$gram_parishad_name))
choices_sansad <- c("Select All", unique(get_data_df$gram_sansad_name))




series_list_gp1 <<- vector("list",0)
series_list_gs1 <<- vector("list",0)
series_list_gp <<- vector("list",0)
series_list_gs <<- vector("list",0)

# max(get_data_df_final$year_month_date)

block_df_ind1 <<- data.frame() 
#   # setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("block", "ind_value"))
gp_df_ind1 <<- data.frame() 
# # <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("gp", "ind_value"))
gs_df_ind1 <<- data.frame() 
# # <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("gs", "ind_value"))


################################ IND GS ########################

get_data_df_gram_sabha <- get_data_df

# get_data_df_gram_sabha[,8:59] <- sapply(get_data_df_gram_sabha[,8:59],as.integer)

# Indicators_PRI <- indicators_master
#NEW CODE BLOCK

# get_data_df_gram_sabha <- filter(get_data_df_gram_sabha,get_data_df_gram_sabha$i1_add!=0,get_data_df_gram_sabha$i1_add!="",
#                                  get_data_df_gram_sabha$i2_add!=0,get_data_df_gram_sabha$i2_add!="",
#                                  get_data_df_gram_sabha$i18_add!=0,get_data_df_gram_sabha$i18_add!="",
#                                  get_data_df_gram_sabha$i36_add!=0,get_data_df_gram_sabha$i36_add!="")


get_data_df_gs_ind <-
  get_data_df_gram_sabha %>%
  group_by(gram_sansad_name) %>%
  mutate(
    
    i1 =   round((mean(i1_add,na.rm = TRUE)/mean(i2_add,na.rm = TRUE)),digits = 2),
    #i1 = ifelse(is.finite(i1),i1,""),
    
    i2 =   round((sum(i3_add,na.rm = TRUE) / mean(i1_add,na.rm = TRUE))*100  ,digits = 2),
    
    # i3 =   round((i4_add/i1_add)*100  ,digits = 2),
    i3 =   round((sum(i4_add,na.rm = TRUE) / mean(i1_add,na.rm = TRUE))*100  ,digits = 2),
    
    i4 =   round(mean(i5_add,na.rm = TRUE),digits = 2),
    
    i5 =   round((sum(i6_add,na.rm = TRUE)/sum(i5_add,na.rm = TRUE))*100  ,digits = 2),
    
    #i6 =   round((    i7_add/  i8_add)*100  ,digits = 0),
    i6 =   round((sum(i7_add,na.rm = TRUE) /sum(i8_add,na.rm = TRUE))*100  ,digits = 2),
    
    
    # i7 =   round((    i9_add/  i8_add)*100  ,digits = 0),
    i7 =   round((sum(i9_add, na.rm = TRUE)/sum(i8_add,na.rm = TRUE))*100  ,digits = 2),
    
    # i8 =   round((    i10_add/  i8_add)*100  ,digits = 0),
    i8 =   round((sum(  i10_add, na.rm = TRUE)/sum(  i8_add,na.rm = TRUE))*100  ,digits = 2),
    
    # i9 =   round((    i11_add/(  i8_add-  i10_add))*100  ,digits = 0)
    i9 =   round((sum(i11_add, na.rm = TRUE)/
                    (sum(i8_add,na.rm = TRUE) - sum(i10_add,na.rm = TRUE)))*100  ,digits = 2),
    
    # ,i10 = round((    i12_add/  i3_add)*100  ,digits = 0)
    i10 =   round((sum(i12_add, na.rm = TRUE)/sum(i3_add,na.rm = TRUE))*100  ,digits = 2),
    
    i11 = round((sum(i13_add, na.rm = TRUE) / mean(i1_add,na.rm = TRUE))*100  ,digits = 2),
    
    i12 = round((sum(i14_add, na.rm = TRUE)/ mean(i1_add,na.rm = TRUE))*100, digits = 2),
    
    # i13 = round((    i15_add/  i3_add)*100  ,digits = 0),
    i13 = round((sum(i15_add, na.rm = TRUE)/sum(i3_add,na.rm = TRUE))*100  ,digits = 2),
    
    # i14 = round((i16_add/  i4_add)*100  ,digits = 0),
    i14 = round((sum(i16_add, na.rm = TRUE) /sum(i4_add,na.rm = TRUE))*100, digits = 2),
    
    i15 = round(sum(i17_add,na.rm = TRUE),digits = 2),
    
    ###################################################################################################
    ################ CHILD DEVELOPMENT ################################################################
    ###################################################################################################
    
    i16 = round(i18_add/sum(i4_add,na.rm = TRUE)*100  ,digits = 2),
    
    i17 = round((sum(i19_add, na.rm= TRUE) /
                   (sum(i8_add,na.rm = TRUE) - sum(i10_add,na.rm = TRUE)))*100,digits = 2),
    
    i18 =   round(( sum(i20_add,na.rm = TRUE)/ sum(i11_add,na.rm = TRUE))*100  ,digits = 2),
    # i19 =   round(( i21_add/  i4_add)*100  ,digits = 0),
    i19 =   round((sum(i21_add,na.rm = TRUE)/ sum(i4_add,na.rm = TRUE))*100  ,digits = 2),
    
    # i20 =   round(( i22_add/  i4_add)*100  ,digits = 0),
    i20 =   round((sum(i22_add,na.rm = TRUE)/ sum(i4_add,na.rm = TRUE))*100  ,digits = 2),
    
    # i21 =   round(( i23_add/  i22_add)*100  ,digits = 0),
    i21 =   round((sum(i23_add,na.rm = TRUE)/ sum(i22_add,na.rm = TRUE))*100  ,digits = 2),
    
    ###################################################################################################
    ################ PANCHAYAT AND RURAL DEVELOPMENT ##################################################
    ###################################################################################################
    
    i23 =   round(( sum(i25_add,na.rm = TRUE)/  mean(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i24 =   round(( sum(i26_add,na.rm = TRUE)/  mean(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i25 =   round(( sum(i27_add,na.rm = TRUE)/  mean(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i26 =   round(( sum(i28_add,na.rm = TRUE)/  mean(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i27 =  round(100 - (i23+i24+i25+i26), digits = 2),
    
    
    i29 =   round(( sum(i30_add,na.rm = TRUE)/  mean(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i30 =   round(( sum(i31_add,na.rm = TRUE)/  mean(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i31 =   round(( sum(i32_add,na.rm = TRUE)/  mean(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i32 =  round(100 - (i29+i30+i31), digits = 2),
    
    i33 =   round(( sum(i33_add,na.rm = TRUE)/  mean(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i34 =   round(( sum(i34_add,na.rm = TRUE)/  mean(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i35 =   round(( sum(i35_add,na.rm = TRUE)/  mean(i2_add,na.rm = TRUE))*100  ,digits = 2),
    
    i36 =   round((sum(i37_add,na.rm = TRUE)/ mean(i36_add,na.rm = TRUE))*100  ,digits = 2),
    
    i38 =   round(( sum(i39_add,na.rm = TRUE)/  mean(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i39 =   round(( sum(i40_add,na.rm = TRUE)/  mean(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i40 =   round(( sum(i41_add,na.rm = TRUE)/  mean(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i41 =   round(( sum(i42_add,na.rm = TRUE)/  mean(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i42 =   round(( sum(i43_add,na.rm = TRUE)/  mean(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i43 =  round(100 - (i38+i39+i40+i41+i42), digits = 2),
    
    
    i45 =   round((sum(i45_add,na.rm = TRUE)/
                     (  sum(i45_add,na.rm = TRUE) +   sum(i46_add,na.rm = TRUE) +   sum(i47_add,na.rm = TRUE) +   sum(i48_add,na.rm = TRUE) 
                        +   sum(i49_add,na.rm = TRUE) +   sum(i50_add,na.rm = TRUE) +   sum(i51_add,na.rm = TRUE) 
                        +   sum(i52_add,na.rm = TRUE)))*100  ,digits = 2),
    
    i46 =   round((sum(i46_add,na.rm = TRUE)/
                     (  sum(i45_add,na.rm = TRUE) +   sum(i46_add,na.rm = TRUE) +   sum(i47_add,na.rm = TRUE) +   sum(i48_add,na.rm = TRUE) 
                        +   sum(i49_add,na.rm = TRUE) +   sum(i50_add,na.rm = TRUE) +   sum(i51_add,na.rm = TRUE) 
                        +   sum(i52_add,na.rm = TRUE)))*100  ,digits = 2),
    i47 =   round((sum(i47_add,na.rm = TRUE)/
                     (  sum(i45_add,na.rm = TRUE) +   sum(i46_add,na.rm = TRUE) +   sum(i47_add,na.rm = TRUE) +   sum(i48_add,na.rm = TRUE) 
                        +   sum(i49_add,na.rm = TRUE) +   sum(i50_add,na.rm = TRUE) +   sum(i51_add,na.rm = TRUE) 
                        +   sum(i52_add,na.rm = TRUE)))*100  ,digits = 2),
    i48 =   round((sum(i48_add,na.rm = TRUE)/
                     (  sum(i45_add,na.rm = TRUE) +   sum(i46_add,na.rm = TRUE) +   sum(i47_add,na.rm = TRUE) +   sum(i48_add,na.rm = TRUE) 
                        +   sum(i49_add,na.rm = TRUE) +   sum(i50_add,na.rm = TRUE) +   sum(i51_add,na.rm = TRUE) 
                        +   sum(i52_add,na.rm = TRUE)))*100  ,digits = 2),
    i49 =   round((sum(i49_add,na.rm = TRUE)/
                     (  sum(i45_add,na.rm = TRUE) +   sum(i46_add,na.rm = TRUE) +   sum(i47_add,na.rm = TRUE) +   sum(i48_add,na.rm = TRUE) 
                        +   sum(i49_add,na.rm = TRUE) +   sum(i50_add,na.rm = TRUE) +   sum(i51_add,na.rm = TRUE) 
                        +   sum(i52_add,na.rm = TRUE)))*100  ,digits = 2),
    i50 =   round((sum(i50_add,na.rm = TRUE)/
                     (  sum(i45_add,na.rm = TRUE) +   sum(i46_add,na.rm = TRUE) +   sum(i47_add,na.rm = TRUE) +   sum(i48_add,na.rm = TRUE) 
                        +   sum(i49_add,na.rm = TRUE) +   sum(i50_add,na.rm = TRUE) +   sum(i51_add,na.rm = TRUE) 
                        +   sum(i52_add,na.rm = TRUE)))*100  ,digits = 2),
    i51 =   round((sum(i51_add,na.rm = TRUE)/
                     (  sum(i45_add,na.rm = TRUE) +   sum(i46_add,na.rm = TRUE) +   sum(i47_add,na.rm = TRUE) +   sum(i48_add,na.rm = TRUE) 
                        +   sum(i49_add,na.rm = TRUE) +   sum(i50_add,na.rm = TRUE) +   sum(i51_add,na.rm = TRUE) 
                        +   sum(i52_add,na.rm = TRUE)))*100  ,digits = 2),
    i52 =   round((sum(i52_add,na.rm = TRUE)/
                     (  sum(i45_add,na.rm = TRUE) +   sum(i46_add,na.rm = TRUE) +   sum(i47_add,na.rm = TRUE) +   sum(i48_add,na.rm = TRUE) 
                        +   sum(i49_add,na.rm = TRUE) +   sum(i50_add,na.rm = TRUE) +   sum(i51_add,na.rm = TRUE) 
                        +   sum(i52_add,na.rm = TRUE)))*100  ,digits = 2)
    
  )

################################################################

################################ IND BLOCK ########################

get_data_df_block <- get_data_df

# get_data_df_block[,8:59] <- sapply(get_data_df_block[,8:59],as.integer)

# Indicators_PRI <- indicators_master
#NEW CODE BLOCK

# get_data_df_block <- filter(get_data_df_block,get_data_df_block$i1_add!=0,get_data_df_block$i1_add!="",
#                             get_data_df_block$i2_add!=0,get_data_df_block$i2_add!="",
#                             get_data_df_block$i18_add!=0,get_data_df_block$i18_add!="",
#                             get_data_df_block$i36_add!=0,get_data_df_block$i36_add!="")


get_data_df_block_ind <-
  get_data_df_block %>%
  group_by(block_parishad_name) %>%
  mutate(
    i1 =   round((mean(i1_add,na.rm = TRUE)/mean(i2_add,na.rm = TRUE)),digits = 2),
    #i1 = ifelse(is.finite(i1),i1,""),
    
    i2 =   round((sum(i3_add,na.rm = TRUE) / sum(i1_add,na.rm = TRUE))*100  ,digits = 2),
    
    # i3 =   round((i4_add/i1_add)*100  ,digits = 2),
    i3 =   round((sum(i4_add,na.rm = TRUE) / sum(i1_add,na.rm = TRUE))*100  ,digits = 2),
    
    i4 =   round(mean(i5_add,na.rm = TRUE),digits = 2),
    
    i5 =   round((sum(i6_add,na.rm = TRUE)/sum(i5_add,na.rm = TRUE))*100  ,digits = 2),
    
    #i6 =   round((    i7_add/  i8_add)*100  ,digits = 0),
    i6 =   round((sum(i7_add,na.rm = TRUE) /sum(i8_add,na.rm = TRUE))*100  ,digits = 2),
    
    
    # i7 =   round((    i9_add/  i8_add)*100  ,digits = 0),
    i7 =   round((sum(i9_add, na.rm = TRUE)/sum(i8_add,na.rm = TRUE))*100  ,digits = 2),
    
    # i8 =   round((    i10_add/  i8_add)*100  ,digits = 0),
    i8 =   round((sum(  i10_add, na.rm = TRUE)/sum(  i8_add,na.rm = TRUE))*100  ,digits = 2),
    
    # i9 =   round((    i11_add/(  i8_add-  i10_add))*100  ,digits = 0)
    i9 =   round((sum(i11_add, na.rm = TRUE)/
                    (sum(i8_add,na.rm = TRUE) - sum(i10_add,na.rm = TRUE)))*100  ,digits = 2),
    
    # ,i10 = round((    i12_add/  i3_add)*100  ,digits = 0)
    i10 =   round((sum(i12_add, na.rm = TRUE)/sum(i3_add,na.rm = TRUE))*100  ,digits = 2),
    
    i11 = round((sum(i13_add, na.rm = TRUE) / mean(i1_add,na.rm = TRUE))*100  ,digits = 2),
    
    i12 = round((sum(i14_add, na.rm = TRUE)/ mean(i1_add,na.rm = TRUE))*100, digits = 2),
    
    # i13 = round((    i15_add/  i3_add)*100  ,digits = 0),
    i13 = round((sum(i15_add, na.rm = TRUE)/sum(i3_add,na.rm = TRUE))*100  ,digits = 2),
    
    # i14 = round((i16_add/  i4_add)*100  ,digits = 0),
    i14 = round((sum(i16_add, na.rm = TRUE) /sum(i4_add,na.rm = TRUE))*100, digits = 2),
    
    i15 = round(sum(i17_add,na.rm = TRUE),digits = 2),
    
    ###################################################################################################
    ################ CHILD DEVELOPMENT ################################################################
    ###################################################################################################
    
    i16 = round(i18_add/sum(i4_add,na.rm = TRUE)*100  ,digits = 2),
    
    i17 = round((sum(i19_add, na.rm= TRUE) /
                   (sum(i8_add,na.rm = TRUE) - sum(i10_add,na.rm = TRUE)))*100,digits = 2),
    
    i18 =   round(( sum(i20_add,na.rm = TRUE)/ sum(i11_add,na.rm = TRUE))*100  ,digits = 2),
    # i19 =   round(( i21_add/  i4_add)*100  ,digits = 0),
    i19 =   round((sum(i21_add,na.rm = TRUE)/ sum(i4_add,na.rm = TRUE))*100  ,digits = 2),
    
    # i20 =   round(( i22_add/  i4_add)*100  ,digits = 0),
    i20 =   round((sum(i22_add,na.rm = TRUE)/ sum(i4_add,na.rm = TRUE))*100  ,digits = 2),
    
    # i21 =   round(( i23_add/  i22_add)*100  ,digits = 0),
    i21 =   round((sum(i23_add,na.rm = TRUE)/ sum(i22_add,na.rm = TRUE))*100  ,digits = 2),
    
    ###################################################################################################
    ################ PANCHAYAT AND RURAL DEVELOPMENT ##################################################
    ###################################################################################################
    
    i23 =   round(( sum(i25_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i24 =   round(( sum(i26_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i25 =   round(( sum(i27_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i26 =   round(( sum(i28_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i27 =  round(100 - (i23+i24+i25+i26), digits = 2),
    
    
    i29 =   round(( sum(i30_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i30 =   round(( sum(i31_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i31 =   round(( sum(i32_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i32 =  round(100 - (i29+i30+i31), digits = 2),
    
    i33 =   round(( sum(i33_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i34 =   round(( sum(i34_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i35 =   round(( sum(i35_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    
    i36 =   round((sum(i37_add,na.rm = TRUE)/ sum(i36_add, na.rm = TRUE))*100  ,digits = 2),
    
    i38 =   round(( sum(i39_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i39 =   round(( sum(i40_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i40 =   round(( sum(i41_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i41 =   round(( sum(i42_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i42 =   round(( sum(i43_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i43 =   round(100 - (i38+i39+i40+i41+i42), digits = 2),
    
    
    i45 =   round((sum(i45_add,na.rm = TRUE)/
                     (  sum(i45_add,na.rm = TRUE) +   sum(i46_add,na.rm = TRUE) +   sum(i47_add,na.rm = TRUE) +   sum(i48_add,na.rm = TRUE) 
                        +   sum(i49_add,na.rm = TRUE) +   sum(i50_add,na.rm = TRUE) +   sum(i51_add,na.rm = TRUE) 
                        +   sum(i52_add,na.rm = TRUE)))*100  ,digits = 2),
    
    i46 =   round((sum(i46_add,na.rm = TRUE)/
                     (  sum(i45_add,na.rm = TRUE) +   sum(i46_add,na.rm = TRUE) +   sum(i47_add,na.rm = TRUE) +   sum(i48_add,na.rm = TRUE) 
                        +   sum(i49_add,na.rm = TRUE) +   sum(i50_add,na.rm = TRUE) +   sum(i51_add,na.rm = TRUE) 
                        +   sum(i52_add,na.rm = TRUE)))*100  ,digits = 2),
    i47 =   round((sum(i47_add,na.rm = TRUE)/
                     (  sum(i45_add,na.rm = TRUE) +   sum(i46_add,na.rm = TRUE) +   sum(i47_add,na.rm = TRUE) +   sum(i48_add,na.rm = TRUE) 
                        +   sum(i49_add,na.rm = TRUE) +   sum(i50_add,na.rm = TRUE) +   sum(i51_add,na.rm = TRUE) 
                        +   sum(i52_add,na.rm = TRUE)))*100  ,digits = 2),
    i48 =   round((sum(i48_add,na.rm = TRUE)/
                     (  sum(i45_add,na.rm = TRUE) +   sum(i46_add,na.rm = TRUE) +   sum(i47_add,na.rm = TRUE) +   sum(i48_add,na.rm = TRUE) 
                        +   sum(i49_add,na.rm = TRUE) +   sum(i50_add,na.rm = TRUE) +   sum(i51_add,na.rm = TRUE) 
                        +   sum(i52_add,na.rm = TRUE)))*100  ,digits = 2),
    i49 =   round((sum(i49_add,na.rm = TRUE)/
                     (  sum(i45_add,na.rm = TRUE) +   sum(i46_add,na.rm = TRUE) +   sum(i47_add,na.rm = TRUE) +   sum(i48_add,na.rm = TRUE) 
                        +   sum(i49_add,na.rm = TRUE) +   sum(i50_add,na.rm = TRUE) +   sum(i51_add,na.rm = TRUE) 
                        +   sum(i52_add,na.rm = TRUE)))*100  ,digits = 2),
    i50 =   round((sum(i50_add,na.rm = TRUE)/
                     (  sum(i45_add,na.rm = TRUE) +   sum(i46_add,na.rm = TRUE) +   sum(i47_add,na.rm = TRUE) +   sum(i48_add,na.rm = TRUE) 
                        +   sum(i49_add,na.rm = TRUE) +   sum(i50_add,na.rm = TRUE) +   sum(i51_add,na.rm = TRUE) 
                        +   sum(i52_add,na.rm = TRUE)))*100  ,digits = 2),
    i51 =   round((sum(i51_add,na.rm = TRUE)/
                     (  sum(i45_add,na.rm = TRUE) +   sum(i46_add,na.rm = TRUE) +   sum(i47_add,na.rm = TRUE) +   sum(i48_add,na.rm = TRUE) 
                        +   sum(i49_add,na.rm = TRUE) +   sum(i50_add,na.rm = TRUE) +   sum(i51_add,na.rm = TRUE) 
                        +   sum(i52_add,na.rm = TRUE)))*100  ,digits = 2),
    i52 =   round((sum(i52_add,na.rm = TRUE)/
                     (  sum(i45_add,na.rm = TRUE) +   sum(i46_add,na.rm = TRUE) +   sum(i47_add,na.rm = TRUE) +   sum(i48_add,na.rm = TRUE) 
                        +   sum(i49_add,na.rm = TRUE) +   sum(i50_add,na.rm = TRUE) +   sum(i51_add,na.rm = TRUE) 
                        +   sum(i52_add,na.rm = TRUE)))*100  ,digits = 2)
    
  )

###############################################################################################

################################ IND GP ########################

get_data_df_gp <- get_data_df

# get_data_df_gp[,8:59] <- sapply(get_data_df_gp[,8:59],as.integer)

# Indicators_PRI <- indicators_master
#NEW CODE BLOCK

# get_data_df_gp <- filter(get_data_df_gp,get_data_df_gp$i1_add!=0,get_data_df_gp$i1_add!="",
#                          get_data_df_gp$i2_add!=0,get_data_df_gp$i2_add!="",
#                          get_data_df_gp$i18_add!=0,get_data_df_gp$i18_add!="",
#                          get_data_df_gp$i36_add!=0,get_data_df_gp$i36_add!="")


get_data_df_gp_ind <-
  get_data_df_gp %>%
  group_by(gram_parishad_name) %>%
  mutate(
    
    i1 =   round((mean(i1_add,na.rm = TRUE)/mean(i2_add,na.rm = TRUE)),digits = 2),
    #i1 = ifelse(is.finite(i1),i1,""),
    
    i2 =   round((sum(i3_add,na.rm = TRUE) / sum(i1_add,na.rm = TRUE))*100  ,digits = 2),
    
    # i3 =   round((i4_add/i1_add)*100  ,digits = 2),
    i3 =   round((sum(i4_add,na.rm = TRUE) / sum(i1_add,na.rm = TRUE))*100  ,digits = 2),
    
    i4 =   round(mean(i5_add,na.rm = TRUE),digits = 2),
    
    i5 =   round((sum(i6_add,na.rm = TRUE)/sum(i5_add,na.rm = TRUE))*100  ,digits = 2),
    
    #i6 =   round((    i7_add/  i8_add)*100  ,digits = 0),
    i6 =   round((sum(i7_add,na.rm = TRUE) /sum(i8_add,na.rm = TRUE))*100  ,digits = 2),
    
    
    # i7 =   round((    i9_add/  i8_add)*100  ,digits = 0),
    i7 =   round((sum(i9_add, na.rm = TRUE)/sum(i8_add,na.rm = TRUE))*100  ,digits = 2),
    
    # i8 =   round((    i10_add/  i8_add)*100  ,digits = 0),
    i8 =   round((sum(  i10_add, na.rm = TRUE)/sum(  i8_add,na.rm = TRUE))*100  ,digits = 2),
    
    # i9 =   round((    i11_add/(  i8_add-  i10_add))*100  ,digits = 0)
    i9 =   round((sum(i11_add, na.rm = TRUE)/
                    (sum(i8_add,na.rm = TRUE) - sum(i10_add,na.rm = TRUE)))*100  ,digits = 2),
    
    # ,i10 = round((    i12_add/  i3_add)*100  ,digits = 0)
    i10 =   round((sum(i12_add, na.rm = TRUE)/sum(i3_add,na.rm = TRUE))*100  ,digits = 2),
    
    i11 = round((sum(i13_add, na.rm = TRUE) / mean(i1_add,na.rm = TRUE))*100  ,digits = 2),
    
    i12 = round((sum(i14_add, na.rm = TRUE)/ mean(i1_add,na.rm = TRUE))*100, digits = 2),
    
    # i13 = round((    i15_add/  i3_add)*100  ,digits = 0),
    i13 = round((sum(i15_add, na.rm = TRUE)/sum(i3_add,na.rm = TRUE))*100  ,digits = 2),
    
    # i14 = round((i16_add/  i4_add)*100  ,digits = 0),
    i14 = round((sum(i16_add, na.rm = TRUE) /sum(i4_add,na.rm = TRUE))*100, digits = 2),
    
    i15 = round(sum(i17_add,na.rm = TRUE),digits = 2),
    
    ###################################################################################################
    ################ CHILD DEVELOPMENT ################################################################
    ###################################################################################################
    
    i16 = round(i18_add/sum(i4_add,na.rm = TRUE)*100  ,digits = 2),
    
    i17 = round((sum(i19_add, na.rm= TRUE) /
                   (sum(i8_add,na.rm = TRUE) - sum(i10_add,na.rm = TRUE)))*100,digits = 2),
    
    i18 =   round(( sum(i20_add,na.rm = TRUE)/ sum(i11_add,na.rm = TRUE))*100  ,digits = 2),
    # i19 =   round(( i21_add/  i4_add)*100  ,digits = 0),
    i19 =   round((sum(i21_add,na.rm = TRUE)/ sum(i4_add,na.rm = TRUE))*100  ,digits = 2),
    
    # i20 =   round(( i22_add/  i4_add)*100  ,digits = 0),
    i20 =   round((sum(i22_add,na.rm = TRUE)/ sum(i4_add,na.rm = TRUE))*100  ,digits = 2),
    
    # i21 =   round(( i23_add/  i22_add)*100  ,digits = 0),
    i21 =   round((sum(i23_add,na.rm = TRUE)/ sum(i22_add,na.rm = TRUE))*100  ,digits = 2),
    
    ###################################################################################################
    ################ PANCHAYAT AND RURAL DEVELOPMENT ##################################################
    ###################################################################################################
    
    i23 =   round(( sum(i25_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i24 =   round(( sum(i26_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i25 =   round(( sum(i27_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i26 =   round(( sum(i28_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i27 =  round(100 - (i23+i24+i25+i26), digits = 2),
    
    
    i29 =   round(( sum(i30_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i30 =   round(( sum(i31_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i31 =   round(( sum(i32_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i32 =  round(100 - (i29+i30+i31), digits = 2),
    
    i33 =   round(( sum(i33_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i34 =   round(( sum(i34_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i35 =   round(( sum(i35_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    
    i36 =   round((sum(i37_add,na.rm = TRUE)/  i36_add)*100  ,digits = 2),
    
    i38 =   round(( sum(i39_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i39 =   round(( sum(i40_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i40 =   round(( sum(i41_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i41 =   round(( sum(i42_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i42 =   round(( sum(i43_add,na.rm = TRUE)/  sum(i2_add,na.rm = TRUE))*100  ,digits = 2),
    i43 =   round(100 - (i38+i39+i40+i41+i42), digits = 2),
    
    
    i45 =   round((sum(i45_add,na.rm = TRUE)/
                     (  sum(i45_add,na.rm = TRUE) +   sum(i46_add,na.rm = TRUE) +   sum(i47_add,na.rm = TRUE) +   sum(i48_add,na.rm = TRUE) 
                        +   sum(i49_add,na.rm = TRUE) +   sum(i50_add,na.rm = TRUE) +   sum(i51_add,na.rm = TRUE) 
                        +   sum(i52_add,na.rm = TRUE)))*100  ,digits = 2),
    
    i46 =   round((sum(i46_add,na.rm = TRUE)/
                     (  sum(i45_add,na.rm = TRUE) +   sum(i46_add,na.rm = TRUE) +   sum(i47_add,na.rm = TRUE) +   sum(i48_add,na.rm = TRUE) 
                        +   sum(i49_add,na.rm = TRUE) +   sum(i50_add,na.rm = TRUE) +   sum(i51_add,na.rm = TRUE) 
                        +   sum(i52_add,na.rm = TRUE)))*100  ,digits = 2),
    i47 =   round((sum(i47_add,na.rm = TRUE)/
                     (  sum(i45_add,na.rm = TRUE) +   sum(i46_add,na.rm = TRUE) +   sum(i47_add,na.rm = TRUE) +   sum(i48_add,na.rm = TRUE) 
                        +   sum(i49_add,na.rm = TRUE) +   sum(i50_add,na.rm = TRUE) +   sum(i51_add,na.rm = TRUE) 
                        +   sum(i52_add,na.rm = TRUE)))*100  ,digits = 2),
    i48 =   round((sum(i48_add,na.rm = TRUE)/
                     (  sum(i45_add,na.rm = TRUE) +   sum(i46_add,na.rm = TRUE) +   sum(i47_add,na.rm = TRUE) +   sum(i48_add,na.rm = TRUE) 
                        +   sum(i49_add,na.rm = TRUE) +   sum(i50_add,na.rm = TRUE) +   sum(i51_add,na.rm = TRUE) 
                        +   sum(i52_add,na.rm = TRUE)))*100  ,digits = 2),
    i49 =   round((sum(i49_add,na.rm = TRUE)/
                     (  sum(i45_add,na.rm = TRUE) +   sum(i46_add,na.rm = TRUE) +   sum(i47_add,na.rm = TRUE) +   sum(i48_add,na.rm = TRUE) 
                        +   sum(i49_add,na.rm = TRUE) +   sum(i50_add,na.rm = TRUE) +   sum(i51_add,na.rm = TRUE) 
                        +   sum(i52_add,na.rm = TRUE)))*100  ,digits = 2),
    i50 =   round((sum(i50_add,na.rm = TRUE)/
                     (  sum(i45_add,na.rm = TRUE) +   sum(i46_add,na.rm = TRUE) +   sum(i47_add,na.rm = TRUE) +   sum(i48_add,na.rm = TRUE) 
                        +   sum(i49_add,na.rm = TRUE) +   sum(i50_add,na.rm = TRUE) +   sum(i51_add,na.rm = TRUE) 
                        +   sum(i52_add,na.rm = TRUE)))*100  ,digits = 2),
    i51 =   round((sum(i51_add,na.rm = TRUE)/
                     (  sum(i45_add,na.rm = TRUE) +   sum(i46_add,na.rm = TRUE) +   sum(i47_add,na.rm = TRUE) +   sum(i48_add,na.rm = TRUE) 
                        +   sum(i49_add,na.rm = TRUE) +   sum(i50_add,na.rm = TRUE) +   sum(i51_add,na.rm = TRUE) 
                        +   sum(i52_add,na.rm = TRUE)))*100  ,digits = 2),
    i52 =   round((sum(i52_add,na.rm = TRUE)/
                     (  sum(i45_add,na.rm = TRUE) +   sum(i46_add,na.rm = TRUE) +   sum(i47_add,na.rm = TRUE) +   sum(i48_add,na.rm = TRUE) 
                        +   sum(i49_add,na.rm = TRUE) +   sum(i50_add,na.rm = TRUE) +   sum(i51_add,na.rm = TRUE) 
                        +   sum(i52_add,na.rm = TRUE)))*100  ,digits = 2)
    
    )

##############################################################################################




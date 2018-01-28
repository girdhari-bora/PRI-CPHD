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

#change in global

  
  token <- readRDS("./Data/droptoken.rds")
  get_data_df <- drop_read_csv("CPHD/get_data_df.csv", dtoken = token)
  get_data_df %>% mutate_if(is.factor, as.character) -> get_data_df
  # unique(get_data_df$gram_sansad_name)


  set_config( config( ssl_verifypeer = 0L ))
  get_data <- GET("https://spreadcreativity.org/master_pri/master_pri_data.php")
  get_data_text <- content(get_data, "text")
  get_data_json <- fromJSON(get_data_text, flatten = TRUE)
  get_data_df_new <- as.data.frame(get_data_json[["indicatordata"]])


# tryCatch({

if (nrow(get_data_df_new) > nrow(get_data_df))
{
  # print("hello I am here1")
  tryCatch({
  drop_delete("get_data_df.csv", path = "CPHD", dtoken = token)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  # write.csv(data, fileName, row.names = TRUE, quote = TRUE)
  
  write.csv(get_data_df_new, "get_data_df.csv")
  drop_upload("get_data_df.csv", path = "CPHD", dtoken = token)
  
  get_data_df <- get_data_df_new
  
  print("hello I am here2")  
}

# }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 


# get_data_df <- read.csv("./Data/get_data_df.csv", 
#                        header = TRUE, stringsAsFactors = FALSE ,check.names = TRUE)
indicators_master <- read.csv("./Data/Indicators PRI.csv", 
                        header = TRUE, stringsAsFactors = FALSE ,check.names = TRUE)

choices_block <- c(unique(get_data_df$block_parishad_name))
choices_panchayat <- c("Select All", unique(get_data_df$gram_parishad_name))
choices_sansad <- c("Select All", unique(get_data_df$gram_sansad_name))

get_data_df_final <- get_data_df %>% 
  mutate(year_month_date = ifelse( month >3 , paste(matrix(unlist(strsplit(get_data_df$year, "-")),ncol=2, byrow = TRUE)[,1],
                                                    month,"01",sep = "-")
                                   , paste(as.integer(matrix(unlist(strsplit(get_data_df$year, "-")),ncol=2, byrow = TRUE)[,1])+1,
                                           month,"01",sep = "-"))
  )

max(get_data_df_final$year_month_date)



# token <- readRDS("./Data/droptoken.rds")
# 
# get_data_df <- drop_read_csv("CPHD/get_data_df.csv", dtoken = token)
# 
# set_config( config( ssl_verifypeer = 0L ))
# get_data <- GET("https://spreadcreativity.org/master_pri/master_pri_data.php")
# get_data_text <- content(get_data, "text")
# get_data_json <- fromJSON(get_data_text, flatten = TRUE)
# get_data_df_new <- as.data.frame(get_data_json[["indicatordata"]])
# 
# if (nrow(get_data_df_new) > nrow(get_data_df))
# {
#   drop_delete("get_data_df.csv", path = "CPHD", dtoken = token)
#   drop_upload("get_data_df.csv", path = "CPHD", dtoken = token)
#   get_data_df <- get_data_df_new
# }



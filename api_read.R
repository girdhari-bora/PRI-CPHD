# install.packages("httr")
# install.packages("jsonlite")

#Require the package so you can use it
require("httr")
require("jsonlite")
library(httr)
library(rdrop2)
library(dplyr)


token <- readRDS("./Data/droptoken.rds")

get_data_df <- drop_read_csv("CPHD/get_data_df.csv", dtoken = token)

get_data_df %>% mutate_if(is.factor, as.character) -> get_data_df

unique(get_data_df$gram_sansad_name)


set_config( config( ssl_verifypeer = 0L ))
get_data <- GET("https://spreadcreativity.org/master_pri/master_pri_data.php")
get_data_text <- content(get_data, "text")
get_data_json <- fromJSON(get_data_text, flatten = TRUE)
get_data_df_new <- as.data.frame(get_data_json[["indicatordata"]])

if (nrow(get_data_df_new) > nrow(get_data_df))
  {
    drop_delete("get_data_df.csv", path = "CPHD", dtoken = token)
    drop_upload("get_data_df.csv", path = "CPHD", dtoken = token)
    get_data_df <- get_data_df_new
  }

get_data_df_final <- get_data_df %>% 
  mutate(year_month_date = paste(matrix(unlist(strsplit(get_data_df$year, "-")),ncol=2, byrow = TRUE)[,1],
                                 month,"01",sep = "-")
  )

max(as.Date(get_data_df_final$year_month_date))







choices_month <- format(seq.Date(from = as.Date("2017-11-01"), by = "month", length.out = 2), "%B-%Y")
choices_month

years <- unique(get_data_df$year)
years <- sort(as.integer(matrix(unlist(strsplit(unlist(years), "-")),ncol=2, byrow = TRUE)[,1]))
years

year_month <- unique(select(get_data_df, year, month))


get_data_df_final <- unique(select(get_data_df, year, month)) %>% mutate(year_month_date =
                        paste(matrix(unlist(strsplit(unlist(unique(select(get_data_df, year, month))$year), "-")),ncol=2, byrow = TRUE)[,1],
                              month,"01",sep = "-")
                              )
year_month1 <- get_data_df %>% 
                  mutate(year_month_date = ifelse( month >3 , paste(matrix(unlist(strsplit(get_data_df$year, "-")),ncol=2, byrow = TRUE)[,1],
                                                                           month,"01",sep = "-")
                                                   , paste(as.integer(matrix(unlist(strsplit(get_data_df$year, "-")),ncol=2, byrow = TRUE)[,1])+1,
                                                           month,"01",sep = "-"))
)

max(as.Date(year_month1$year_month_date))


# token <- drop_auth()
# saveRDS(token, file = "droptoken.rds")

# Retrieve Dropbox account information
# drop_acc(dtoken = token) %>% data.frame()
# Dropbox directory listing
    # write.csv(get_data_df, "get_data_df.csv")
    # drop_upload("get_data_df.csv")
# drop_dir(dtoken = token, '/CPHD')
# Create folders on Dropbox
  # drop_create('drop_test')
  # # or provide the full path where it needs to be created
  # drop_create('public/drop_test')
# Upload a file into Dropbox
  # write.csv(mtcars, 'mtcars.csv')
  # drop_upload('mtcars.csv')
  ## or upload to a specific folder
  # drop_upload('mtcars.csv', path = "drop_test")
# Download a file
  # drop_download('mtcars.csv')
  # or add path if file is not in root
  # downloaded_file <- drop_download("CPHD/get_data_df.csv", dtoken = token )
# downloaded_file <- drop_read_csv("CPHD/get_data_df.csv", dtoken = token)




#Health and Family Welfare
    Indicators_PRI$value[1] <- round(mean(get_data_df$i1_add/get_data_df$i2_add, na.rm = TRUE),digits = 0)
    Indicators_PRI$value[2] <- round(mean((get_data_df$i3_add/get_data_df$i1_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$value[3] <- round(mean((get_data_df$i4_add/get_data_df$i1_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$value[4] <- round(mean(get_data_df$i5_add, na.rm = TRUE),digits = 0) 
    Indicators_PRI$value[5] <- round(mean((get_data_df$i6_add/get_data_df$i5_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$value[6] <- round(mean((get_data_df$i7_add/get_data_df$i8_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$value[7] <- round(mean((get_data_df$i9_add/get_data_df$i8_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$value[8] <- round(mean((get_data_df$i10_add/get_data_df$i8_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$value[9] <- round(mean((get_data_df$i11_add/(get_data_df$i8_add-get_data_df$i10_add))*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$value[10] <- round(mean((get_data_df$i12_add/get_data_df$i3_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$value[11] <- round(mean((get_data_df$i13_add/get_data_df$i1_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$value[12] <- round(mean((get_data_df$i14_add/get_data_df$i1_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$value[13] <- round(mean((get_data_df$i15_add/get_data_df$i3_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$value[14] <- round(mean((get_data_df$i16_add/get_data_df$i4_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$value[15] <- round(sum(get_data_df$i17_add, na.rm = TRUE),digits = 0) 

  #Child Development
    Indicators_PRI$value[16] <- round(mean((get_data_df$i18_add/get_data_df$i4_add)*100, na.rm = TRUE),digits = 0)
    # Indicators_PRI$value[17] <- round(mean((get_data_df$i19_add/get_data_df$i8_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$value[18] <- round(mean((get_data_df$i20_add/get_data_df$i1_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$value[19] <- round(mean((get_data_df$i21_add/get_data_df$i4_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$value[20] <- round(mean((get_data_df$i22_add/get_data_df$i4_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$value[21] <- round(mean((get_data_df$i23_add/get_data_df$i22_add)*100, na.rm = TRUE),digits = 0) 
    
  #Panchayat and Rural Development
    Indicators_PRI$value[23] <- round(mean((get_data_df$i25_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$value[24] <- round(mean((get_data_df$i26_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$value[25] <- round(mean((get_data_df$i27_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$value[26] <- round(mean((get_data_df$i28_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$value[27] <- round(mean(((get_data_df$i2_add- (get_data_df$i25_add + 
                                              get_data_df$i26_add + 
                                              get_data_df$i27_add + 
                                              get_data_df$i28_add ))/
                                             get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    
    Indicators_PRI$value[29] <- round(mean((get_data_df$i30_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$value[30] <- round(mean((get_data_df$i31_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$value[31] <- round(mean((get_data_df$i32_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$value[32] <- round(mean(((get_data_df$i2_add- (get_data_df$i30_add + 
                                                                    get_data_df$i31_add + 
                                                                    get_data_df$i32_add 
                                                                    ))/
                                              get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    
    Indicators_PRI$value[33] <- round(mean((get_data_df$i33_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$value[34] <- round(mean((get_data_df$i34_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$value[35] <- round(mean((get_data_df$i35_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    
    # Indicators_PRI$value[17] <- round(mean((get_data_df$i19_add/get_data_df$i8_add)*100, na.rm = TRUE),digits = 0) 
    Indicators_PRI$value[36] <- round(mean((get_data_df$i37_add/get_data_df$i36_add)*100, na.rm = TRUE),digits = 0)
    # Indicators_PRI$value[37] <- round(mean((get_data_df$i38_add/get_data_df$i36_add)*100, na.rm = TRUE),digits = 0)
    
    Indicators_PRI$value[38] <- round(mean((get_data_df$i39_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$value[39] <- round(mean((get_data_df$i40_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$value[40] <- round(mean((get_data_df$i41_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$value[41] <- round(mean((get_data_df$i42_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$value[42] <- round(mean((get_data_df$i43_add/get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$value[43] <- round(mean(((get_data_df$i2_add- (  get_data_df$i39_add + 
                                                                    get_data_df$i40_add + 
                                                                    get_data_df$i41_add +
                                                                    get_data_df$i42_add + 
                                                                    get_data_df$i43_add 
                                                                         ))/
                                              get_data_df$i2_add)*100, na.rm = TRUE),digits = 0)
    
    denominator_45_to_52 <- (get_data_df$i45_add + get_data_df$i46_add + get_data_df$i47_add + get_data_df$i48_add + get_data_df$i49_add 
                             + get_data_df$i50_add + get_data_df$i51_add + get_data_df$i52_add)
    
    Indicators_PRI$value[45] <- round(mean((get_data_df$i45_add/denominator_45_to_52)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$value[46] <- round(mean((get_data_df$i46_add/denominator_45_to_52)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$value[47] <- round(mean((get_data_df$i47_add/denominator_45_to_52)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$value[48] <- round(mean((get_data_df$i48_add/denominator_45_to_52)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$value[49] <- round(mean((get_data_df$i49_add/denominator_45_to_52)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$value[50] <- round(mean((get_data_df$i50_add/denominator_45_to_52)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$value[51] <- round(mean((get_data_df$i51_add/denominator_45_to_52)*100, na.rm = TRUE),digits = 0)
    Indicators_PRI$value[52] <- round(mean((get_data_df$i52_add/denominator_45_to_52)*100, na.rm = TRUE),digits = 0)
    


  
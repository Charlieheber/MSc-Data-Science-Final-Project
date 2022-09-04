
# get whp data -----------------------------------------
hazard_data_file_loc <- paste0(here::here(), "/data/hazard data/")

whp_2020_cls_conus_df <- data.table::fread(paste0(hazard_data_file_loc, "whp_cls_conus.csv"))
whp_2020_cls_conus_df$V1 <- NULL

# connect to sql server --------------------------------
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "laptop-qq5m45ki\\hazusplussrvr",
                 Database = "MSc Masters Project")

# test query
dbGetQuery(con,'
  SELECT * FROM [MSc Masters Project].[dbo].[whp_2020_cls_conus]
')

# write table
dbWriteTable(con, DBI::Id(schema = "dbo", table = "whp_2020_cls_conus_v2"), whp_2020_cls_conus_df, append = TRUE, row.names = F)

library(BASGRA)
file_weather   <- 'data-raw/weather_00_Saerheim_format_bioforsk.txt'
parcol         <- 13  
file_params    <- 'data-raw/parameters.txt'
df_params      <- read.table(file_params,header=T,sep="\t",row.names=1)
run_params     <- df_params[,parcol]
site_harvest   <- 'data-raw/site_harvest.csv'
site_fertilize <- 'data-raw/site_fertilize.csv'
site_ndep      <- 'data-raw/site_ndep.csv'
start_date     <- "2000-04-22"
end_date       <- "2000-12-31"
output         <- run_BASGRA(file_weather, run_params, site_harvest, site_fertilize, site_ndep, start_date, end_date)

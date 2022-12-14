#' Read in the BASGRA weather file  
#' @details Read in the BASGRA weather file .
#' @export
#' @useDynLib BASGRA
read_weather          <- function(y = year_start,
                                  d = doy_start,
                                  n = NDAYS,
                                  df_weather) {
  row_start             <- 1
  while( df_weather[row_start,]$YR  < y ) { row_start <- row_start+1 }
  while( df_weather[row_start,]$doy < d ) { row_start <- row_start+1 }
  df_weather_sim        <- df_weather[row_start:(row_start+n-1),]
  NMAXDAYS              <- as.integer(10000)
  NWEATHER              <- as.integer(9)
  matrix_weather        <- matrix( 0., nrow=NMAXDAYS, ncol=NWEATHER )
  matrix_weather[1:n,1] <- df_weather_sim$YR
  matrix_weather[1:n,2] <- df_weather_sim$doy
  matrix_weather[1:n,3] <- df_weather_sim$GR
  matrix_weather[1:n,4] <- df_weather_sim$T
  matrix_weather[1:n,5] <- df_weather_sim$T
  matrix_weather[1:n,6] <- exp(17.27*df_weather_sim$T/(df_weather_sim$T+239)) *
                               0.6108 * df_weather_sim$RH / 100
  matrix_weather[1:n,7] <- df_weather_sim$RAINI
  matrix_weather[1:n,8] <- df_weather_sim$WNI
  matrix_weather[1:n,9] <- 350

  return(matrix_weather)
}


      

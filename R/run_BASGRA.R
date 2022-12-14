#' Run the grass model BASGRA 
#' @details This is a wrapper function to run BASGRA.
#' @export
#' @useDynLib BASGRA
run_BASGRA <- function(file_weather, run_params, site_harvest, site_fertilize, site_ndep, start_date, end_date){

    start_date  <- as.POSIXlt(start_date, tz = "UTC")
    end_date    <- as.POSIXlt(end_date, tz = "UTC")
    start_year  <- lubridate::year(start_date)
    start_doy   <- lubridate::yday(start_date)
    end_year    <- lubridate::year(end_date)

    NDAYS <- as.integer(length(seq(start_date,end_date,"days")))
    year_start  <- as.integer(start_year)
    doy_start   <- as.integer(lubridate::yday(start_date))
    
    df_weather     <- read.table( file_weather, header=TRUE )
    matrix_weather  <- read_weather(year_start,doy_start,NDAYS,df_weather)

    outputNames <- c(
        "Time"      , "year"     , "doy"      , "DAVTMP"    , "CLV"      , "CLVD"     ,
        "YIELD"     , "CRES"     , "CRT"      , "CST"       , "CSTUB"    , "DRYSTOR"  ,
        "Fdepth"    , "LAI"      , "LT50"     , "O2"        , "PHEN"     , "ROOTD"    ,
        "Sdepth"    , "TANAER"   , "TILG"     , "TILV"      , "WAL"      , "WAPL"     ,
        "WAPS"      , "WAS"      , "WETSTOR"  , "DM"        , "RES"      , "PHENCR"     , 
        "NELLVG"    , "NELLVM"    , "SLA"      , "TILTOT"    , "FRTILG"   , "TILG1"  ,
        "TILG2"   , "RDRT"     , "VERN"     ,
        "CLITT"      , "CSOMF", "CSOMS"   , "NLITT"       , "NSOMF",
        "NSOMS"      , "NMIN" , "NPP"    , "RplantAer"   ,"Rsoil"   , "NemissionN2O",
        "NemissionNO", "Nfert", "Ndep"    , "RWA"         ,
        "NSH"        , "GNSH" , "DNSH"    , "HARVNSH"     ,  "NCSH" ,
        "NCGSH"      , "NCDSH", "NCHARVSH",
        "fNgrowth","RGRTV","FSPOT","RESNOR","TV2TIL","NSHNOR","KNMAX","KN",    # 63:70
        "DMLV"       , "DMST"             , "NSH_DMSH"    ,                    # 71:73
        "Nfert_TOT"  , "YIELD_TOT"        , "DM_MAX"      ,                    # 74:76
        "F_PROTEIN"  , "F_ASH"            ,                                    # 77:78
        "F_WALL_DM"  , "F_WALL_DMSH"      , "F_WALL_LV"   , "F_WALL_ST",       # 79:82
        "F_DIGEST_DM", "F_DIGEST_DMSH"    ,                                    # 83:84
        "F_DIGEST_LV", "F_DIGEST_ST"      , "F_DIGEST_WALL",                   # 85:87
        "RDRS"       , "Precipitation"    , "Nleaching"   , "NSHmob",          # 88:91
        "NSHmobsoil" , "Nfixation"        , "Nupt"        , "Nmineralisation", # 92:95
        "NSOURCE"    , "NSINK"            ,                                    # 96:97
        "NRT"        , "NCRT"             ,                                    # 98:99
        "rNLITT"     , "rNSOMF"           ,                                    # 100:101
        "DAYL"       , "EVAP"             , "TRAN"                             # 102:104
    )
    
    outputUnits <- c(
        "(y)"       , "(y)"      , "(d)"      , "(degC)"    , "(g C m-2)", "(g C m-2)",  #  1: 6
        "(g C m-2)", "(g C m-2)", "(g C m-2)", "(g C m-2)" , "(g C m-2)", "(mm)"     ,  #  7:12
        "(m)"       , "(m2 m-2)" , "(degC)"   , "(mol m-2)" , "(-)"      , "(m)"      ,  # 13:18
        "(m)"       , "(d)"      , "(m-2)"    , "(m-2)"     , "(mm)"     , "(mm)"     ,  # 19:24
        "(mm)"      , "(mm)"     , "(mm)"     , "(g DM m-2)", "(g g-1)"  , "(m d-1)"  ,  # 25:30
        "(tiller-1)", "(d-1)"    , "(m2 g-1)" , "(m-2)"     , "(-)"      , "(-)"      ,  # 31:36
        "(-)"       , "(d-1)"    , "(-)"      ,                                          # 37:39
        "(g C m-2)"    , "(g C m-2)"    , "(g C m-2)"    , "(g N m-2)"    , "(g N m-2)", # 40:44
        "(g N m-2)"    , "(g N m-2)"    , "(g C m-2 d-1)", "(g N m-2 d-1)",              # 45:48
        "(g C m-2 d-1)", "(g N m-2 d-1)",                                                # 49:50
        "(g N m-2 d-1)", "(g N m-2 d-1)", "(g N m-2 d-1)", "(-)"          ,              # 51:54
        "(g N m-2)"    , "(g N m-2 d-1)", "(g N m-2 d-1)", "(g N m-2 d-1)", "(-)"      , # 55:59
        "(-)"          , "(-)"          , "(-)"          ,                               # 60:62
        "(-)", "(d-1)", "(-)", "(-)", "(d-1)", "(-)", "(m2 m-2)", "(m2 m-2)",            # 63:70
        "(g DM m-2)"   , "(g DM m-2)"   , "(g N g-1 DM)"  ,                              # 71:73
        "(g N m-2)"    , "(g DM m-2)"   , "(g DM m-2)"    ,                              # 74:76
        "(g g-1 DM)"   , "(g g-1 DM)"   ,                                                # 77:78
        "(g g-1 DM)"   , "(g g-1 DM)"   , "(g g-1 DM)"    , "(g g-1 DM)"  ,              # 79:82
        "(-)"          , "(-)"          ,                                                # 83:84
        "(-)"          , "(-)"          , "(-)"           ,                              # 85:87
        "(d-1)"        , "(mm d-1)"     , "(g N m-2 d-1)" , "(g N m-2 d-1)",             # 88:91
        "(g N m-2 d-1)", "(g N m-2 d-1)", "(g N m-2 d-1)" , "(g N m-2 d-1)",             # 92:95
        "(g N m-2 d-1)", "(g N m-2 d-1)",                                                # 96:97
        "(g N m-2)"    , "(g N g-1 C)"  ,                                                # 98:99
        "(g N m-2)"    , "(g N g-1 C)"  ,                                                # 100:101
        "(d d-1)"      , "(mm d-1)"     , "(mm d-1)"                                     # 102:104
    )
    
    NOUT <- as.integer( length(outputNames) )

    calendar_fert     <- matrix( 0, nrow=300, ncol=3 )
    f_days <- as.matrix(utils::read.table(site_fertilize, header = TRUE, sep = ","))
    calendar_fert[1:nrow(f_days),] <- f_days
    calendar_fert[,3] <- calendar_fert[,3] * 1000/10000       # convert kg N ha-1 into g / m2
     

    n_days <- as.matrix(utils::read.table(site_ndep, header = TRUE, sep = ","))
    calendar_Ndep     <- matrix( 0, nrow=300, ncol=3 )
    calendar_Ndep[1:nrow(n_days),] <- n_days
    calendar_Ndep[,3] <- calendar_Ndep[,3] *  1000/(10000*365)  # kg N ha-1 y-1 convert into g / m2 /day 

    days_harvest      <- matrix(as.integer(-1), nrow= 300, ncol = 2)
    # read in harvest days
    h_days <- as.matrix(utils::read.table(site_harvest, header = TRUE, sep = ","))
    days_harvest[1:nrow(h_days),] <- h_days[,1:2]
   
    output <- .Fortran('BASGRA',
                       run_params,
                       matrix_weather,
                       calendar_fert,
                       calendar_Ndep,
                       days_harvest,
                       NDAYS,
                       NOUT,
                       matrix(0, NDAYS, NOUT))[[8]]
    output <- data.frame(output)
    names(output) <- outputNames
    return(output)
}

# Retrieve sample tracking data from Google Sheets

library(httr)
library(readxl)
library(dplyr)
library(yaml)

# Make sure Bison Connect is your default, and log out for good measure any personal gmail

get_tracking_data <- function(get_tracking.config = "10_load_data/cfg/tracking_config.yaml"){
  
  config.args <- yaml.load_file(get_tracking.config)
  
  set_config(config(ssl_verifypeer = 0L))
  # 
  timeZone <- c("UTC-5")

  read_excel(./10_load_data/raw/config.args)
      dfState <- gs_read(glpfTitle,ws=States[i],range="A4:P100")
    names(dfState) <- c("Site","SiteID","Date","Time", "MediumCode","HydrologicCondition",
                        "SampleTypeCode","NWISRecordNumber",
                        "Glyphosate","Neonics","ToxCast","Metabolomics","Transcriptomics","Passive",
                        "Comments","GlyphosateComments")
    
    dfState <- dfState[which(!is.na(dfState$Date)),]
    dfState$SampleTypeCode <- as.character(dfState$SampleTypeCode)
    dfState$NWISRecordNumber <- as.character(dfState$NWISRecordNumber)
    if(class(dfState$Time)[1] %in% c("character","integer")){
      dfState$Time <- as.integer(dfState$Time)
      dfState$Time <- substr(as.POSIXct(sprintf("%04.0f", dfState$Time), format='%H%M'), 12, 16)
        } else dfState$Time <- format(strptime(dfState$Time, format="%H:%M"), format = "%H:%M")
    dfState$Time  <- as.character(dfState$Time)
    dfState$pdate <- paste(dfState$Date,dfState$Time)
    dfState$pdate <- as.POSIXct(dfState$pdate,format='%m/%d/%Y %H:%M',tz=timeZone[i])
    dfState$pdate <- as.POSIXct(format(as.POSIXct(dfState$pdate),tz="GMT",usetz=TRUE),tz="GMT")
    dfState$Date <- as.Date(dfState$Date,format='%m/%d/%Y' )
    # 
    dfState$State <- States[i]
    dfState <- subset(dfState, select=c(Site:Time,pdate,MediumCode:State))
    dfState <- subset(dfState, select=c(State,Site:GlyphosateComments))
    
    if(i == 1) {
      df <- dfState
    } else {
      df <- bind_rows(df,dfState)
    }
  }

  df$NWISRecordNumber <- gsub(" ", "", df$NWISRecordNumber)
  df$NWISRecordNumber <- gsub("(DB04)", "", df$NWISRecordNumber, fixed=TRUE)
  df$NWISRecordNumber <- gsub("(DB01)", "", df$NWISRecordNumber, fixed=TRUE)
  df$NWISRecordNumber <- gsub("(DB01", "", df$NWISRecordNumber, fixed=TRUE)
  df$NWISRecordNumber <- gsub("q", "", df$NWISRecordNumber)
  df$NWISRecordNumber <- gsub("(DB4)", "", df$NWISRecordNumber, fixed=TRUE)
  df$NWISRecordNumber <- gsub("(DB3)", "", df$NWISRecordNumber, fixed=TRUE)
  df$NWISRecordNumber <- zeroPad(df$NWISRecordNumber, 8)
  
  return(df)
  
}


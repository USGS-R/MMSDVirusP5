# Retrieve pharm samples from MMSD Phase V virus project
# Include Underwood Creek, 16th St., and Jones Island Influent

# library(dataRetrieval)
library(tidyr)
library(dplyr)
library(WQReview)
library(reshape2)
library(stringr)
         

#Retrieve data for selected sites and dates
get_NWIS <- function(siteInfo,parmInfo,dates){

  ## define sites for data retrieval ##
  sites <- siteInfo$site_no #Use all sites in list for now

  qw.data <- readNWISodbc(DSN="NWISWI",
                          env.db = "01",
                          qa.db = "03",
                          STAIDS = c("04087088","04087142","430125087540400"),
                          dl.parms="All",
                          parm.group.check=TRUE,
                          begin.date = dates[1],
                          end.date = dates[2],
                          projectCd = NULL)
  
  dfWide <- qw.data[[1]]
  idVars <- names(dfWide)[c(1:8,142:166)]
  df <- melt(dfWide,id=idVars)
  values <- str_split_fixed(df$value," ",n=2)
  df$remark_cd <- values[,2]
  df$value <- as.numeric(values[,1])

  surrogates <- df[unique(grep("sur",df$variable)),"CASN"]

  df$occur <-  ifelse(df$remark_cd != "<",1,0)
  df$occur <- ifelse(is.na(df$occur),TRUE,df$occur)

  # df_sub <- df %>%
  #   select(pdate = ActivityStartDateTime, FullsiteID = MonitoringLocationIdentifier, ActivityIdentifier,
  #          CharacteristicName,pCode=USGSPCode,value = ResultMeasureValue,remark_cd,units = ResultMeasure.MeasureUnitCode,
  #          ActivityTypeCode, HydrologicCondition, HydrologicEvent,ResultValueTypeName,
  #          lab.comments = ResultLaboratoryCommentText,
  #          detection.type = DetectionQuantitationLimitTypeName,
  #          detection.limit = DetectionQuantitationLimitMeasure.MeasureValue,
  #          detection.units = DetectionQuantitationLimitMeasure.MeasureUnitCode) %>%
  #   mutate(SiteID = gsub(pattern = "USGS-","",FullsiteID),
  #          NWISRecordNumber = sapply(strsplit(ActivityIdentifier,"\\."), function(x) x[3])) %>%
  #   select(-ActivityIdentifier, -FullsiteID)

  #dfqwd <- read.delim("./10_load_data/raw/mmsdp5dataNoHeaders.txt",stringsAsFactors = FALSE)
  
  #parms <- sub("R","",names(dfqwd)[grep("R",names(dfqwd))])
  
  
  # headers <- c("STAID","DATES","EDATE")
  # for(i in 1:length(parms)){
  #   columnNames <- c(headers,paste0("P",parms[i]),paste0("R",parms[i]))
  #   dftemp <- dfqwd[,columnNames]
  #   dftemp$pcode <- paste0("P",parms[i])
  #   names(dftemp) <- c(headers,"value","remark_cd","pcode")
  #   if (i == 1) {df <- dftemp
  #   }else{
  #     df <- rbind(df, dftemp)
  #   }
  # }

  return(df)
  
}


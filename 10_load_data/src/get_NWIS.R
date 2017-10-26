# Retrieve pharm samples from MMSD Phase V virus project
# Include Underwood Creek, 16th St., and Jones Island Influent

# library(dataRetrieval)
# library(tidyr)
# library(dplyr)

         

#Retrieve data for selected sites and dates
get_NWIS <- function(siteInfo,parmInfo,dates){

  ## define sites for data retrieval ##
  sites <- siteInfo$site_no #Use all sites in list for now

  df <- readWQPdata(siteNumbers = paste0("USGS-",sites), 
                      startDate = dates[1],
                      endDate = dates[2])

  df$remark_cd <- ""
  df$remark_cd[grep("^Not Detected$",df$ResultDetectionConditionText)] <- "<"

  df$occur <-  df$remark_cd != "<"
  df$occur <- ifelse(is.na(df$occur),TRUE,df$occur)
  
  df_sub <- df %>%
    select(pdate = ActivityStartDateTime, FullsiteID = MonitoringLocationIdentifier, ActivityIdentifier,
           CharacteristicName,pCode=USGSPCode,value = ResultMeasureValue,remark_cd,units = ResultMeasure.MeasureUnitCode,
           ActivityTypeCode, HydrologicCondition, HydrologicEvent,ResultValueTypeName,
           lab.comments = ResultLaboratoryCommentText,
           detection.type = DetectionQuantitationLimitTypeName,
           detection.limit = DetectionQuantitationLimitMeasure.MeasureValue,
           detection.units = DetectionQuantitationLimitMeasure.MeasureUnitCode) %>%
    mutate(SiteID = gsub(pattern = "USGS-","",FullsiteID),
           NWISRecordNumber = sapply(strsplit(ActivityIdentifier,"\\."), function(x) x[3])) %>%
    select(-ActivityIdentifier, -FullsiteID)
  

  return(df_sub)
  
}


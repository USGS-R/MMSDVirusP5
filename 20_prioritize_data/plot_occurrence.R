# Exploratory graphics for prioritizing samples to help Pharm3 sample selection
library(ggplot2)
library(dplyr)


plot_occurrence <- function(NWIS,parmInfo) {
  df <- NWIS[-grep("sur",NWIS$variable),]
  
  df$value2 <- ifelse(df$remark_cd == "<",0,df$value)
  NWISSummary <- df %>% group_by(STATION_NM,SAMPLE_END_DT) %>%
    summarise(occurrence = mean(occur),
              concSum = sum(value2))
  df <- NWIS
    parms <- parmInfo[which(parmInfo$analyteType == "analyte"),"pcode"]
    parmNames <- parmInfo[which(parmInfo$analyteType == "analyte"),""]

    df <- df[which(df$pCode %in% parms),]
    sites <- unique(df$SiteID)
    df$occur <- ifelse(df$remark_cd == "<" , 0,1)
    
    plotdf <- df %>% group_by(SiteID,pdate) %>%
      summarise(occur = mean(occur),
                count = sum(occur),
                concSum = sum(value,na.rm=TRUE))
    sites <- unique(plotdf$SiteID)
    plotdf <- arrange(plotdf,pdate)
    
    plotdf$colors <- ifelse(plotdf$SiteID == sites[1],"blue","forestgreen")
    
    ggplot(plotdf, aes(x=row_number(plotdf$SiteID),y=occur)) +
      geom_col(fill = plotdf$colors)# + 
#      facet_grid(SiteID ~ .)
    
    ggplot(plotdf, aes(x=plotdf$pdate,y=occur)) +
      geom_point(color = plotdf$colors)# + 
    ggplot(plotdf, aes(x=plotdf$pdate,y=concSum)) +
      geom_point(aes(color = plotdf$SiteID),size=3) + 
      scale_colour_manual(values=c("orange","blue"))

    unique(df$pCode)
plotdf

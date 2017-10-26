# Exploratory graphics for prioritizing samples to help Pharm3 sample selection
library(ggplot2)
plot_occurrence <- function(NWIS,parmInfo) {
    df <- NWIS
    parms <- parmInfo[which(parmInfo$analyteType == "analyte"),"pcode"]
    df <- df[which(df$pCode %in% parms),]
    sites <- unique(df$SiteID)
    df$occur <- ifelse(df$remark_cd == "<" , 0,1)
    df %>% group_by(SiteID,pdate) %>%
      summarise(occur = mean(occur)) %>%
    ggplot(aes(x=pdate,y=occur)) +
      geom_bar() + 
      facet_grid(SiteID ~ .)

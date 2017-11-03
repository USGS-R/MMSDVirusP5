library(ggplot2)

df <- NWIS[-grep("sur",NWIS$variable),]

df$value2 <- ifelse(df$remark_cd == "<",0,df$value)
plotdf <- df %>% group_by(STATION_NM,SITE_NO,SAMPLE_START_DT,RECORD_NO) %>%
  summarise(occurrence = mean(occur),
            count = sum(occur),
            concSum = sum(value2,na.rm=TRUE))
sites <- unique(plotdf$STATION_NM)
plotdf <- arrange(plotdf,SAMPLE_START_DT)
siteColors <- c("forestgreen","blue","brown")
names(siteColors) <- sites

plotdf$colors <- siteColors[plotdf$STATION_NM]

ggplot(plotdf, aes(x=plotdf$SAMPLE_START_DT,y=occurrence)) +
  geom_point(color = plotdf$colors)# + 

plotdf <- arrange(plotdf,desc(count),SAMPLE_START_DT,STATION_NM)

write.csv(plotdf,file="MMSDPharmPrioritiesNov12017.csv",row.names = FALSE)

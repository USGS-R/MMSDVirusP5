



dfqwd <- read.delim("./10_load_data/raw/mmsdp5dataNoHeaders.txt",stringsAsFactors = FALSE)
parmNames <- read.delim("./10_load_data/raw/mmsdp5data.txt.parnames.txt",
                        stringsAsFactors = FALSE,header = FALSE,skip = 3)
names(parmNames) <- "parm"
parmNames <- parmNames[-which(substr(parmNames$parm,start = 1,stop=1) == "R"),]
grep("sur",parmNames,ignore.case = TRUE)

parmNames <- substr(parmNames,1,5)

parms <- sub("R","",names(dfqwd)[grep("R",names(dfqwd))])


headers <- c("STAID","DATES","EDATE")
for(i in 1:length(parms)){
  columnNames <- c(headers,paste0("P",parms[i]),paste0("R",parms[i]))
  dftemp <- dfqwd[,columnNames]
  dftemp$pcode <- paste0("P",parms[i])
  names(dftemp) <- c(headers,"value","remark_cd","pcode")
  if (i == 1) {df <- dftemp
  }else{
    df <- rbind(df, dftemp)
  }
}

dfsort <- arrange(df,STAID,DATES)
write.csv(dfsort,file="dfsort.csv")

df$occur <- 0
df[which(df$remark_cd %in% "<"),"occur"] <- 1
value2

naRows <- which(is.na(df$value))
df <- df[-naRows,]

df %>% group_by(STAID,DATES) %>%
  summarise(count = sum(occur,na.rm=TRUE),
            occur = mean(occur,na.rm=TRUE))
  

plotdf <- df %>% group_by(STAID,DATES) %>%
  summarise(count = sum(occur,na.rm=TRUE),
            occur = mean(occur,na.rm=TRUE),
            concSum = sum(value,na.rm=TRUE))

ggplot(plotdf, aes(x=plotdf$pdate,y=occur)) +
  geom_point(color = plotdf$STAID)# + 
ggplot(plotdf, aes(x=plotdf$pdate,y=concSum)) +
  geom_point(aes(color = plotdf$SiteID),size=3) + 
  scale_colour_manual(values=c("orange","blue"))




# Exploratory graphics for prioritizing samples to help Pharm3 sample selection
library(ggplot2)
library(dplyr)


plot_occurrence <- function(NWIS,parmInfo) {
  df <- NWIS
  parms <- parmInfo[which(parmInfo$analyteType == "analyte"),"pcode"]
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
  
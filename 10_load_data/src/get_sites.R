# Read site names and USGS station IDs
library(dplyr)
library(dataRetrieval)
library(readxl)

get_sites <- function(sitesFile) {
  
#  sitesFile <- "./10_load_data/raw/sites.csv"
  sites <- read.csv(sitesFile,colClasses = c("character","character"))
  from_NWIS <- readNWISsite(sites$STAID)
  dfSites <- left_join(from_NWIS, sites, by=c("site_no"="STAID"))
  
  return(dfSites)
}


#Compile information about the parameters of interest
get_parms <- function(parmFile,mdlFile) {
# parmFile <-   "./10_load_data/raw/pharm2ParmInfo.txt"
# LIMSFile <- "./10_load_data/raw/pharm2InfoFromLIMS.xls"
# mdlFile <-  "./10_load_data/raw/pharm2DetectionLimits.txt"

  parmInfo <- read.delim(parmFile,stringsAsFactors = FALSE,colClasses = "character")
  LIMSInfo <- read_excel(LIMSFile); names(LIMSInfo) <- make.names(names(LIMSInfo))
  parmInfo <- full_join(parmInfo, LIMSInfo, by=c("pcode" = "Parameter.Code"))

  #Separate analytes and surrogates into separate dataframe
  parmInfo$analyteType <- "analyte"
  surrogateRows <- grep("surrogate",parmInfo$Parameter.Name)
  parmInfo[surrogateRows,"analyteType"] <- "surrogate"
  surrogateInfo <- parmInfo[surrogateRows,]
  
  #Orlistat is not on the current LIMS list
  parmInfo[which(is.na(parmInfo$Parameter.Name)),"Parameter.Name"] <-
    parmInfo[which(is.na(parmInfo$Parameter.Name)),"pharmName"]  
  
  # mdlInfo <- read.delim(mdlFile,stringsAsFactors = FALSE)
  # mdlInfo[which(mdlInfo$Pharmaceutical == "Metoprolol"),"CASRN"] <- parmInfo[which(parmInfo$pharmName == "Metoprolol"),"CASRN"]
  # mdlInfo[which(mdlInfo$Pharmaceutical == "Methyl-1H-benzotriazole"),"CASRN"] <- parmInfo[which(parmInfo$pharmName == "Methyl-1H-benzotriazole"),"CASRN"]
  # mdlInfo[which(mdlInfo$Pharmaceutical == "Bupropion"),"CASRN"] <- parmInfo[which(parmInfo$pharmName == "Bupropion"),"CASRN"]
  
  return(parmInfo)
}


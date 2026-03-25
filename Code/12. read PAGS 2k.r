

# Read PAGES Data ---------------------------------------------------------

## Read Data
## https://lipdverse.org/Pages2kTemperature/2_1_2/
library(lipdR)

## Transfer to Time Series
load("Z:/2023/Input data/PAGE 2K v2.1.2/Pages2kTemperature2_1_2.RData")
PAGES.TS = extractTs(D)
PAGES.TS_2 = filterTs(PAGES.TS,"paleoData_useInGlobalTemperatureAnalysis == TRUE")

# Annual Transformation and Check -----------------------------------------------------------

## read data
properties = data.frame()
i = 529

## Read Each Series
for(i in 1:length(PAGES.TS_2)) {
  
  ## Data
  Temp = PAGES.TS_2[[i]]
  
  ## Properties
  properties[i,c("pages2kID")] =  Temp$paleoData_pages2kID
  properties[i,c("Lon")] =  Temp$geo_longitude
  properties[i,c("Lat")] =  Temp$geo_latitude
  properties[i,c("elevation")] =  Temp$geo_elevation
  properties[i,c("name")] =  Temp$geo_siteName
  properties[i,c("archive")] =  Temp$archiveType
  properties[i,c("proxy")] = Temp$paleoData_proxy
  properties[i,c("variable")] =  Temp$paleoData_variableName
  properties[i,c("units")] =  Temp$paleoData_units
  properties[i,c("direction")] =  Temp$interpretation1_interpDirection
  properties[i,c("seasonality")] =  Temp$interpretation1_seasonality
  properties[i,c("Resolution_Max")] =  Temp$hasResolution_hasMaxValue
  properties[i,c("Resolution_median")] =  Temp$hasResolution_hasMedianValue
  properties[i,c("Reference1_doi")] =  Temp$pub1_doi
  properties[i,c("Reference2_doi")] =  Temp$pub2_doi
  
  ## Time Series
  Data.series = data.frame("year" = Temp$year, 
                           "value" = Temp$paleoData_values)
  Data.series = Data.series[order(Data.series$year),]
  
  ## Climate Direction
  if(properties[i,c("direction")] == "negative"){
    Data.series$value = - Data.series$value
  }
  
  ## Remove rows with NAs
  Data.series = Data.series[complete.cases(Data.series), ]
  Data.series = Data.series[which(Data.series$year >= 1), ]
  
  ## Annual Resolution
  A.Data.series = Data.series %>%
    dplyr::mutate(year = floor(year)) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(value = mean(value))
  
  ## Scaling to numeric values
  Data.series2 = A.Data.series
  Data.series2$value = as.numeric(scale(Data.series2$value))
  
  ## Period
  Ava.years = Data.series2$year
  properties[i,"Start.year"] = min(Ava.years)
  properties[i,"End.year"] = max(Ava.years)
  
  # Check : Cover 1959-1990
  Cover.years = length(which(c(1959:2000) %in% Ava.years))
  if (Cover.years < 30) {
    properties[i, "Remove.Missing.30"] = TRUE
  } else {
    properties[i, "Remove.Missing.30"] = FALSE
  }
  
  ## Write Time Series
  write.csv(Data.series,paste0("Output Data/PAGES 2k v.2.0.0/Raw_Series/",properties$pages2kID[i],".csv"))
  write.csv(Data.series2,paste0("Output Data/PAGES 2k v.2.0.0/Annual.Processded Data/",properties$pages2kID[i],".csv"))
  
  ## Print
  print(i)
}

## Remove Two Checks
properties1 = properties %>%
  dplyr::filter(Remove.Missing.30 != TRUE)

## Remove Resolution > 1 year
properties1 = properties1 %>%
  dplyr::filter(Resolution_median <= 1)

## Write
write.csv(properties,"Output Data/PAGES 2k v.2.0.0/Properties/All.properties.csv",row.names = FALSE)
write.csv(properties1,"Output Data/PAGES 2k v.2.0.0/Properties/properties.select.csv",row.names = FALSE)

# Correlations with CGR ---------------------------------------------------

## CGR
Annual.CGR=read.csv("Output Data/CGR/CGR.csv")
properties1=read.csv("Output Data/PAGES 2k v.2.0.0/Properties/properties.select.csv")
ge = 1

## Read Data
for (ge in 1:NROW(properties1)) {
  
  ## read chronology
  raw.chro = read.csv(
    file = paste0(
      "Output Data/PAGES 2k v.2.0.0/Annual.Processded Data/",
      properties1$pages2kID[ge],
      ".csv"
    ),
    row.names = 1
  )
  
  ## Pass-filter
  raw.chro$value = pass.filter.NA.mean(
    a = raw.chro$value,
    W = 10,
    type = "high",
    method = c("Butterworth")
  )
  
  ## Write
  write.csv(
    raw.chro,
    paste0(
      "Output Data/PAGES 2k v.2.0.0/High_pass_PAGES/",
      properties1$pages2kID[ge],
      ".csv"
    )
  )
  
  ## Correlation in current year
  Total.data = merge(raw.chro, Annual.CGR[, c("year", "Total.Obs1")])
  properties1[ge, c("A.Cor.0", "A.Cor.p.0")] = cor25.1(Total.data$value, Total.data$Total.Obs1)
  
  ## Correlation in preceding year
  raw.chro1 = raw.chro
  raw.chro1$year = raw.chro1$year + 1
  Total.data = merge(raw.chro1, Annual.CGR[, c("year", "Total.Obs1")])
  properties1[ge, c("A.Cor.1", "A.Cor.p.1")] = cor25.1(Total.data$value, Total.data$Total.Obs1)
  
  ## Select Max Correlation
  Cor.max = which.max(abs(properties1[ge, c("A.Cor.0", "A.Cor.1")]))
  properties1[ge, c("Max.Cor.year")] = Cor.max - 1
  if(Cor.max == 1) {
    properties1[ge, c("Max.Cor.CGR", "Max.Cor.CGR.p")] = properties1[ge, c("A.Cor.0", "A.Cor.p.0")]
  }
  if(Cor.max == 2) {
    properties1[ge, c("Max.Cor.CGR", "Max.Cor.CGR.p")] = properties1[ge, c("A.Cor.1", "A.Cor.p.1")]
  }
  
}

## Select significance level < 0.05
properties2 = properties1 %>%
  dplyr::filter(Max.Cor.CGR.p < 0.05)

## Write
write.csv(properties1,"Output Data/PAGES 2k v.2.0.0/Properties/All.properties.select.csv",row.names = FALSE)
write.csv(properties2,"Output Data/PAGES 2k v.2.0.0/Properties/Cor.properties.select.csv",row.names = FALSE)

# End -------------------------------------------------------------

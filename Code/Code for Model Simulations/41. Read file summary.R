

# Summary for CMIP6 -----------------------------------------------

## Functions
CMIP6_properties = function(CMIP.name) {
  
  proper=strsplit(CMIP.name,"_")[[1]]
  
  stats.model1 = data.frame()
  stats.model1[1,"File.name"] = CMIP.name
  
  stats.model1[1,c("Output.variable","nc.variable","relam","model")]= proper[c(1,1,2,3)]
  stats.model1[1,c("scenario","varivant","resolution")]= proper[4:6]
  
  temp=strsplit(proper[7],split=c(".n"))[[1]]
  A.date =as.numeric(strsplit(temp,split=c("-"))[[1]])
  stats.model1[1,c("startdate","enddate")]=A.date
  stats.model1[1,c("startyear","endyear")]=c(year(ymd(A.date[1]*100+1)),year(ymd(A.date[2]*100+1)))
  stats.model1[1,c("startmonth","endmonth")]=c(month(ymd(A.date[1]*100+1)),month(ymd(A.date[2]*100+1)))
  
  int <- interval(ymd(A.date[1]*100+1),ymd(A.date[2]*100+1))
  stats.model1[1,"totalmonth"]=time_length(int, "month")+1

  return(stats.model1)
}

## Parameters
PMIP.file = data.frame()
Dir.path = "Z:/2022/Science9_SM/Input Data/PMIIP_past1000/past1000_CMIP6/"
Ava.models = list.files(Dir.path)
i = 1; j = 2

## Read Files
for(i in 1:length(Ava.models)){
  file.names = list.files(paste0(Dir.path,Ava.models[i]))
  for(j in 1:length(file.names)){
    stats.temp = CMIP6_properties(CMIP.name = file.names[j])
    stats.temp$path = paste0(Dir.path,Ava.models[i],"/",file.names[j])
    PMIP.file = rbind(PMIP.file,stats.temp)
  }
}

## Selections of files
PMIP.CMIP6 = PMIP.file%>%dplyr::filter(Output.variable %in% c("tas","nbp"))
write.csv(PMIP.CMIP6,"Output Data/PMIIP_past1000/PMIP.CMIP6.csv")

# Summary for CMIP5 -----------------------------------------------

## Functions
CMIP5_properties = function(CMIP.name) {
  
  proper=strsplit(CMIP.name,"_")[[1]]
  
  stats.model1 = data.frame()
  stats.model1[1,"File.name"] = CMIP.name
  
  stats.model1[1,c("Output.variable","nc.variable","relam","model")]= proper[c(1,1,2,3)]
  stats.model1[1,c("scenario","varivant")]= proper[4:5]
  stats.model1[1,c("resolution")]= NA
  
  temp=strsplit(proper[6],split=c(".n"))[[1]]
  A.date =as.numeric(strsplit(temp,split=c("-"))[[1]])
  stats.model1[1,c("startdate","enddate")]=A.date
  stats.model1[1,c("startyear","endyear")]=c(year(ymd(A.date[1]*100+1)),year(ymd(A.date[2]*100+1)))
  stats.model1[1,c("startmonth","endmonth")]=c(month(ymd(A.date[1]*100+1)),month(ymd(A.date[2]*100+1)))
  
  int <- interval(ymd(A.date[1]*100+1),ymd(A.date[2]*100+1))
  stats.model1[1,"totalmonth"]=time_length(int, "month")+1
  
  return(stats.model1)
}

## Parameters
PMIP.file = data.frame()
Dir.path = "Z:/2022/Science9_SM/Input Data/PMIIP_past1000/past1000_CMIP5/"
Ava.models = list.files(Dir.path)
i = 1; j = 2

## Read Files
for(i in 1:length(Ava.models)){
  file.names = list.files(paste0(Dir.path,Ava.models[i]))
  for(j in 1:length(file.names)){
    stats.temp = CMIP5_properties(CMIP.name = file.names[j])
    stats.temp$path = paste0(Dir.path,Ava.models[i],"/",file.names[j])
    PMIP.file = rbind(PMIP.file,stats.temp)
  }
}

## Selections of files
PMIP.CMIP5 = PMIP.file%>%dplyr::filter(Output.variable %in% c("tas","nbp"))
write.csv(PMIP.CMIP5,"Output Data/PMIIP_past1000/PMIP.CMIP5.csv")


# Summary for CESM ------------------------------------------------

## Functions
CMIP5_properties = function(CMIP.name) {
  
  proper=unlist(strsplit(CMIP.name,split = ".", fixed = TRUE))
  
  stats.model1 = data.frame()
  stats.model1[1,"File.name"] = CMIP.name
  
  stats.model1[1,c("Output.variable","nc.variable","relam","model")]= c(NA,proper[8],"Atom","CESM1")
  stats.model1[1,c("scenario","varivant")]= c("past1000",proper[5])
  stats.model1[1,c("resolution")]= NA
  
  temp=strsplit(proper[9],split=c(".n"))[[1]]
  A.date =as.numeric(strsplit(temp,split=c("-"))[[1]])
  stats.model1[1,c("startdate","enddate")]=A.date
  stats.model1[1,c("startyear","endyear")]=c(year(ymd(A.date[1]*100+1)),year(ymd(A.date[2]*100+1)))
  stats.model1[1,c("startmonth","endmonth")]=c(month(ymd(A.date[1]*100+1)),month(ymd(A.date[2]*100+1)))
  
  int <- interval(ymd(A.date[1]*100+1),ymd(A.date[2]*100+1))
  stats.model1[1,"totalmonth"]=time_length(int, "month")+1
  
  return(stats.model1)
}

## Parameters
PMIP.file = data.frame()
i = 2

## NBP models
Ava.files = list.files("Z:/2022/Science9_SM/Input Data/PMIIP_past1000/past1000_CESM/nbp/")
for(i in 1:length(Ava.files)){
  stats.temp = CMIP5_properties(CMIP.name = Ava.files[i])
  stats.temp$Output.variable = "nbp"
  stats.temp$path = paste0("Z:/2022/Science9_SM/Input Data/PMIIP_past1000/past1000_CESM/nbp/",Ava.files[i])
  PMIP.file = rbind(PMIP.file,stats.temp)
}

## TS models
Ava.files = list.files("Z:/2022/Science9_SM/Input Data/PMIIP_past1000/past1000_CESM/TS/")
for(i in 1:length(Ava.files)){
  stats.temp = CMIP5_properties(CMIP.name = Ava.files[i])
  stats.temp$Output.variable = "tas"
  stats.temp$path = paste0("Z:/2022/Science9_SM/Input Data/PMIIP_past1000/past1000_CESM/TS/",Ava.files[i])
  PMIP.file = rbind(PMIP.file,stats.temp)
}

## Selections of files
PMIP.CESM1 = PMIP.file%>%dplyr::filter(Output.variable %in% c("tas","nbp"))
write.csv(PMIP.CESM1,"Output Data/PMIIP_past1000/PMIP.CESM1.csv")


# Summary for all data --------------------------------------------

## All models Data
PMIP.CMIP6 = read.csv("Output Data/PMIIP_past1000/PMIP.CMIP6.csv",row.names = 1)
PMIP.CMIP5 = read.csv("Output Data/PMIIP_past1000/PMIP.CMIP5.csv",row.names = 1)
PMIP.CESM1 = read.csv("Output Data/PMIIP_past1000/PMIP.CESM1.csv",row.names = 1)

## Data.frame
PMIP.models = rbind(PMIP.CMIP6,PMIP.CMIP5,PMIP.CESM1)

## Data.frame
PMIP.model = PMIP.models %>%
  dplyr::group_by(model,varivant,Output.variable) %>%
  dplyr::summarise(N.month = sum(totalmonth)) %>%
  pivot_wider(names_from = Output.variable, values_from = N.month) %>%
  dplyr::filter(!is.na(tas) & !is.na(nbp))

## Write
write.csv(PMIP.models,"Output Data/PMIIP_past1000/PMIP.models.csv")
write.csv(PMIP.model,"Output Data/PMIIP_past1000/PMIP.model.csv")

# End -------------------------------------------------------------



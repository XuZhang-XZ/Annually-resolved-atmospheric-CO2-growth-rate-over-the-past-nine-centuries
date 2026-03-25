

# Annual Values ------------------------------------------------------------

## Land Area
Noah=nc_open("Z:/2023/Input data/CRU TS v4.07/cru_ts4.07.1901.2022.tmp.dat.nc")
Land.mask=ncvar_get(Noah, "tmp")
Land.mask = Land.mask[,,1440]
Land.mask[which(!is.na(Land.mask))] = 1
Land.mask[which(is.na(Land.mask))] = 0
Land.lon=ncvar_get(Noah, "lon")
Land.lat=ncvar_get(Noah, "lat")
nc_close(Noah)

## Change
Land.lon = Land.lon[c(361:720,1:360)]
Land.lon[which(Land.lon<0)] = Land.lon[which(Land.lon<0)]+360
Land.mask = Land.mask[c(361:720,1:360),]

## Change land mask
o.lon = seq(1,359,by = 2)
o.lat = seq(-89,89,by = 2)
Land.mask1 = conser(x = Land.lon, y = Land.lat, z = Land.mask, xo = o.lon, yo = o.lat)
Land.mask1[which(Land.mask1<0.5)] = NA

## All models Data
PMIP.models = read.csv("Output Data/PMIIP_past1000/PMIP.models.csv",row.names = 1)
PMIP.model = read.csv("Output Data/PMIIP_past1000/PMIP.model.csv",row.names = 1)

## Data
Sim.Series = data.frame()
i = 1

## Read Global Data
for(i in 1:NROW(PMIP.model)){

  ## Read Temperature
  out.name = paste0("Z:/2022/Science9_SM/Output Data/PMIIP_past1000/Gridded Data/",PMIP.model$model[i],"_",PMIP.model$varivant[i],"_","tas",".nc")
  Noah=nc_open(out.name)
  i.lon = ncvar_get(Noah,"longitude")
  i.lat = ncvar_get(Noah,"latitude")
  i.time = ncvar_get(Noah,"time")
  i.date.tas = ymd(19700101) + i.time
  i.tas = ncvar_get(Noah,"tas")
  nc_close(Noah)
  
  ## Read NBP
  out.name = paste0("Z:/2022/Science9_SM/Output Data/PMIIP_past1000/Gridded Data/",PMIP.model$model[i],"_",PMIP.model$varivant[i],"_","nbp",".nc")
  Noah=nc_open(out.name)
  i.lon = ncvar_get(Noah,"longitude")
  i.lat = ncvar_get(Noah,"latitude")
  i.time = ncvar_get(Noah,"time")
  i.date.nbp = ymd(19700101) + i.time
  i.nbp = ncvar_get(Noah,"nbp")
  nc_close(Noah)

  # ## Test Date
  # i.date[13968]
  # which(!c(1000:2000)%in%unique(Sim.Series$year))
  # c(1000:2000)[110]

  ## Change Resolution
  i.lat = i.lat[90:1]
  i.tas = i.tas[,90:1,]
  i.nbp = i.nbp[,90:1,]
  
  ## Tropical Temperature
  i.tas_Tropic = i.tas
  i.tas_Tropic[,which(i.lat<(-23)|i.lat > 23),] = NA
  
  ## Land Area
  A.land.mask = array(Land.mask1,dim(i.nbp))
  i.nbp[which(is.na(A.land.mask))] = NA
  
  # ## Test
  # image.plot(i.tas[,,1])
  # image.plot(i.nbp[,,1])
  
  ## Spatial Mean values
  A.Weight = array(rep(cos(i.lat * pi / 180), each = length(i.lon)), c(length(i.lon), length(i.lat)))
  Sim.Series_tas = data.frame(
    "year" = year(i.date.tas),
    "month" = month(i.date.tas),
    "model" = PMIP.model$model[i],
    "varivant" = PMIP.model$varivant[i],
    "type" = "tas_tropical",
    "value" = areal.mean(z = i.tas_Tropic, A.Weight = A.Weight)
  )
  Sim.Series_nbp = data.frame(
    "year" = year(i.date.nbp),
    "month" = month(i.date.nbp),
    "model" = PMIP.model$model[i],
    "varivant" = PMIP.model$varivant[i],
    "type" = "nbp",
    "value" = areal.mean(z = i.nbp, A.Weight = A.Weight) * 10e9
  )
  
  ## Change Unit
  if(PMIP.model$model[i] != "CESM1"){   
    conversion = 1.3425 * 1e+8 * 1e+6 * 31536000 / (1e12) /  10e9 ## From kg/m^2/s to GtC/year
    Sim.Series_nbp$value = Sim.Series_nbp$value * conversion
  }
  if(PMIP.model$model[i] == "CESM1"){   
    conversion = 1 / 1000 * 1.3425 * 1e+8 * 1e+6 * 31536000 / (1e12) /  10e9 ## From gC/m^2/s to GtC/year
    Sim.Series_nbp$value = Sim.Series_nbp$value * conversion
  }
  
  ## From GtC/year to ppm/year
  Sim.Series_nbp$value = Sim.Series_nbp$value/2.214
  
  ## Combine
  Sim.Series_temp = bind_rows(Sim.Series_tas, Sim.Series_nbp) %>%
    dplyr::group_by(year, model, varivant, type) %>%
    dplyr::summarise(value = mean(value))  %>%
    dplyr::filter(year %in% c(1000:2000))
  
  ## Check
  if (NROW(Sim.Series_temp) != 2002) {
    print(paste0("Error:", i))
  }
  
  ## Combine
  Sim.Series = rbind(Sim.Series,Sim.Series_temp)
  
  ## Print
  print(i)
  
}

## Write
out.name = "Z:/2022/Science9_SM/Output Data/PMIIP_past1000/past1000_summary/Sim.series.csv"
write.csv(Sim.Series,out.name)

# Correlation and Variance -----------------------------------------------------

## Data
Total.summary.sd = read.csv("Output Data/Reconstructed CGR/SD/Total.summary.sd.csv")
Total.summary.sd1 = subset(Total.summary.sd,year %in% c(1959:2000))
a2.mean = mean(Total.summary.sd1$Total.Obs)
a2.sd = sd(Total.summary.sd1$Total.Obs)

## Data
Total_Tropic = read.csv("Output Data/Reconstructed CGR/SD/Total_Tropic.csv")
Total_Tropic1 = subset(Total_Tropic,year %in% c(1959:2000))
a2.mean_tas = mean(Total_Tropic1$Tropical.T)
a2.sd_tas = sd(Total_Tropic1$Tropical.T)

## Series
PMIP.model = read.csv("Output Data/PMIIP_past1000/PMIP.model.csv",row.names = 1)
Sim.Series = read.csv("Z:/2022/Science9_SM/Output Data/PMIIP_past1000/past1000_summary/Sim.series.csv",row.name = 1)

## Pass filter
Sim.Series = Sim.Series %>%
  dplyr::group_by(model, varivant, type) %>%
  dplyr::reframe(year = year, Raw_value = value, value_10 = pass.filter.NA(value, W = 10, type = "high"))

## Summary
CMIP_Summary = data.frame()
Models_NBP_Var = data.frame() 
i = 13

## Correlation and Sensitivity
for(i in 1:NROW(PMIP.model)) {
  
  ## Data
  temp =  Sim.Series %>% 
    dplyr::filter(model == PMIP.model$model[i] & varivant == PMIP.model$varivant[i])
  Sim.Series_Sel = temp %>% 
    pivot_wider(id_cols = year,names_from = type,values_from = value_10)
  Sim.Series_RSel = temp %>% 
    pivot_wider(id_cols = year,names_from = type,values_from = Raw_value,names_prefix = "Raw_")
  Sim.Series_Sel = merge(Sim.Series_Sel,Sim.Series_RSel,all.x = TRUE)
  Sim.Series_Sel[,"model"] = PMIP.model$model[i]
  Sim.Series_Sel[,"varivant"] = PMIP.model$varivant[i]
  j = 16
  
  # ## Test
  # plot(Sim.Series_Sel$Raw_nbp)
  # plot(Sim.Series_Sel$Raw_tas_tropical,type = "l")
  
  ## NBP Var
  Sim.Series_Sel1 = subset(Sim.Series_Sel,year %in% c(1959:2000))
  Models_NBP_Var[i,"model"] = PMIP.model$model[i]
  Models_NBP_Var[i,"varivant"] = PMIP.model$varivant[i]
  Models_NBP_Var[i,"NBP_var"] = var(Sim.Series_Sel1$nbp,na.rm = TRUE)
  
  ## Adjust mean and sd for NBP
  Sim.Series_Sel1 = subset(Sim.Series_Sel,year %in% c(1959:2000))
  a1.mean = mean(Sim.Series_Sel1$nbp)
  a1.sd = sd(Sim.Series_Sel1$nbp)
  Sim.Series_Sel$nbp = (Sim.Series_Sel$nbp - a1.mean)/a1.sd * a2.sd + a2.mean

  ## Adjust mean and sd for Raw_tas_tropical
  Sim.Series_Sel1 = subset(Sim.Series_Sel,year %in% c(1959:2000))
  a1.mean = mean(Sim.Series_Sel1$Raw_tas_tropical)
  a1.sd = sd(Sim.Series_Sel1$Raw_tas_tropical)
  Sim.Series_Sel$Raw_tas_tropical = (Sim.Series_Sel$Raw_tas_tropical - a1.mean)/a1.sd * a2.sd_tas + a2.mean_tas
  
  ## Prediction
  Sim.Series_Sel1 = subset(Sim.Series_Sel,year %in% c(1959:2000))
  kk1 = lm(nbp ~ tas_tropical, data = Sim.Series_Sel1)
  Sim.Series_Sel$Pre_nbp = predict(kk1,Sim.Series_Sel)
  
  ## 31-years Correlation
  for(j in 16:(NROW(Sim.Series_Sel) - 15)){
    
    ## Select Data
    Sel_data = Sim.Series_Sel[c((j - 15):(j + 15)),]
    
    ## Correlation
    temp = cor.test(Sel_data$tas_tropical,Sel_data$nbp,method = c("pearson"))
    Sim.Series_Sel[j,c("Cor")] = -temp$estimate
    Sim.Series_Sel[j,c("Cor_p")] = temp$p.value
    
    ## Correlation for prediction
    temp = cor.test(Sel_data$Pre_nbp,Sel_data$nbp,method = c("pearson"))
    Sim.Series_Sel[j,c("Cor_pre")] = temp$estimate
    Sim.Series_Sel[j,c("Cor_pre_p")] = temp$p.value
    Sim.Series_Sel[j,c("CE_pre")] = NSE(obs = Sel_data$nbp, pre = Sel_data$Pre_nbp)
    Sim.Series_Sel[j,c("RE_pre")] = RE(obs = Sel_data$nbp, pre = Sel_data$Pre_nbp, calibration = Sim.Series_Sel1$nbp)
    
    ## Sensitivity
    kk1 = lm(nbp ~ tas_tropical, data = Sel_data)
    Sim.Series_Sel[j, c("Sen")] = kk1$coefficients[2]
    
    ## SD
    Sim.Series_Sel[j, "Var_tas_tropical"] = var(Sel_data$tas_tropical,na.rm = T)
    Sim.Series_Sel[j, "Var_nbp"] = var(Sel_data$nbp,na.rm = T)
  }
  
  # ## Test
  # plot(Sim.Series_Sel$Var_nbp)
  # plot(Sim.Series_Sel$Raw_tas_tropical,type = "l")
  
  ## Combine
  CMIP_Summary = rbind(CMIP_Summary,Sim.Series_Sel)
  
}

## Write
write.csv(Models_NBP_Var,"Z:/2022/Science9_SM/Output Data/PMIIP_past1000/past1000_summary/Models_NBP_Var.csv")
write.csv(CMIP_Summary,"Z:/2022/Science9_SM/Output Data/PMIIP_past1000/past1000_summary/CMIP_Summary.csv")

## Plot Correlation
p1 = ggplot() +
  geom_path(data = CMIP_Summary,
            aes(x = year, y = Cor, color = model))

## Plot Sensitivity
p1 = ggplot() +
  geom_path(data = CMIP_Summary,
            aes(x = year, y = Sen, color = model))

## Plot SD
p1 = ggplot() +
  geom_path(data = CMIP_Summary,
            aes(x = year, y = Var_nbp, color = model))

## Plot RE
p1 = ggplot() +
  geom_path(data = CMIP_Summary,
            aes(x = year, y = RE_pre, color = model))

## Examine
plot(Sel_data$year,Sel_data$nbp,type = "l")
lines(Sel_data$year,Sel_data$Pre_nbp, col = "blue")
cor.test(Sel_data$Pre_nbp,Sel_data$nbp,method = c("pearson"))
RE(obs = Sel_data$nbp, pre = Sel_data$Pre_nbp, calibration = Sim.Series_Sel1$nbp)

# End -------------------------------------------------------------


# 
# ## Parameters
# Statis.model = data.frame()
# Cor.T.CGR = data.frame()
# i = 18
# 
# ## Read Global Data
# for(i in 1:NROW(PMIP.model)){
#   
#   ## Read Data
#   out.name = paste0("Output Data/PMIIP_past1000/Statis/",PMIP.model$model[i],"_",PMIP.model$varivant[i],".txt")
#   Sim.Series = read.csv(out.name, row.names = 1)
#   
#   ## Combine
#   Statis.model = rbind(Statis.model,Sim.Series)
#   
#   ## Exceedance Probability
#   CGR.1990 = Sim.Series$Var_nbp[which(Sim.Series$year == 1985)]
#   CGR.1500_1850 = Sim.Series$Var_nbp[which(Sim.Series$year %in% c(1515:1835))]
#   CGR.1000_1500 = Sim.Series$Var_nbp[which(Sim.Series$year %in% c(1015:1485))]
#   Cor.T.CGR[i, "Exceed.1500_1850"] = length(which(CGR.1500_1850 > CGR.1990)) / length(CGR.1500_1850)
#   Cor.T.CGR[i, "Exceed.1000_1500"] = length(which(CGR.1000_1500 > CGR.1990)) / length(CGR.1000_1500)
#   
#   ## Correlation between SD and T
#   temp = cor.test(Sim.Series$Var_TroT,Sim.Series$Var_nbp)
#   Cor.T.CGR[i,"Model"] = PMIP.model$model[i]
#   Cor.T.CGR[i,"varivant"] = PMIP.model$varivant[i]
#   Cor.T.CGR[i,"X_text"] = paste0(PMIP.model$model[i],"_",PMIP.model$varivant[i])
#   Cor.T.CGR[i,"Cor_T_CGR"] = temp$estimate
#   Cor.T.CGR[i,"Cor_T_CGR_p"] = temp$p.value
# }
# 
# ## Write
# write.csv(Statis.model,"Output Data/PMIIP_past1000/Statis/Statis.model.csv")
# write.csv(Cor.T.CGR,"Output Data/PMIIP_past1000/Statis/Cor.T.CGR.csv")
# 
# ## Summary
# Statis.model = Statis.model %>%
#   dplyr::filter(Model != "CESM1")
# Cor.T.CGR = Cor.T.CGR %>%
#   dplyr::filter(Model != "CESM1")
# 
# ## Properties
# Statis.model_summary = Statis.model %>%
#   dplyr::group_by(year) %>%
#   dplyr::summarise(across(!c(Model,varivant), list(
#     mean = ~ mean(.x, na.rm = TRUE),
#     median = ~ median(.x, na.rm = TRUE),
#     upper_95 = ~ quantile(.x, probs = 0.975, na.rm = TRUE),
#     lower_95 = ~ quantile(.x, probs = 0.025, na.rm = TRUE)
#   )))
# 
# ## Plot Correlation
# plot(Statis.model_summary$year,Statis.model_summary$Cor_tas_nbp_mean,type = "l",ylim = c(-1,0))
# lines(Statis.model_summary$year,Statis.model_summary$Cor_tas_nbp_upper_95,col = "red")
# lines(Statis.model_summary$year,Statis.model_summary$Cor_tas_nbp_lower_95,col = "red")
# 
# ## Plot Correlation for prediction
# plot(Statis.model_summary$year,Statis.model_summary$NSE_pre_nbp_mean,type = "l",ylim = c(0,1))
# lines(Statis.model_summary$year,Statis.model_summary$NSE_pre_nbp_upper_95,col = "red")
# lines(Statis.model_summary$year,Statis.model_summary$NSE_pre_nbp_lower_95,col = "red")
# 
# ## Plot Var
# plot(Statis.model_summary$year,Statis.model_summary$Var_nbp_mean,type = "l",ylim = c(0,1))
# lines(Statis.model_summary$year,Statis.model_summary$Var_nbp_upper_95,col = "red")
# lines(Statis.model_summary$year,Statis.model_summary$Var_nbp_lower_95,col = "red")
# 
# ## Plot Var Median
# plot(Statis.model_summary$year,Statis.model_summary$Var_nbp_median,type = "l",ylim = c(0,1))
# lines(Statis.model_summary$year,Statis.model_summary$Var_nbp_upper_95,col = "red")
# lines(Statis.model_summary$year,Statis.model_summary$Var_nbp_lower_95,col = "red")
# 
# 
# ## Plot Correlation
# p1 = ggplot()+
#   geom_path(data = Statis.model,
#             aes(x = year, y = Cor_tas_nbp, color = Model))
# p1
# p1 = ggplot()+
#   geom_path(data = Statis.model,
#             aes(x = year, y = NSE_pre_nbp, color = Model))
# p1
# 
# ## Plot Var
# p1 = ggplot()+
#   geom_path(data = Statis.model,
#             aes(x = year, y = Var_nbp, color = Model))
# p1
# 
# ## Plot Cor T and CGR
# p1 = ggplot()+
#   geom_col(data = Cor.T.CGR,
#             aes(x = X_text, y =Cor_T_CGR))
# p1
# 
# ## Plot Exceedance Probability
# p1 = ggplot()+
#   geom_col(data = Cor.T.CGR,
#            aes(x = X_text, y = Exceed.1500_1850))
# p1
# p1 = ggplot()+
#   geom_col(data = Cor.T.CGR,
#            aes(x = X_text, y = Exceed.1000_1500))
# p1

# ## Tas
# Tas.old = read.csv("Output Data/PMIIP_past1000/MIROC-ES2L_tas_tropical.T.csv",row.names = 1)
# Tas.new = read.csv("Output Data/PMIIP_past1000/Statis/MIROC-ES2L_r1i1p1f2.txt",row.names = 1)
# 
# ## Plot
# plot(Tas.old$year,Tas.old$Tropical.tas,type = "l",xlim = c(1900,2000))
# lines(Tas.new$year,Tas.new$Tropcal.tas,col = "red")
# 
# plot(Tas.old$year,Tas.old$Tropical.tas,type = "l",xlim = c(1700,1800))
# lines(Tas.new$year,Tas.new$Tropcal.tas,col = "red")



#     
#   
# p1  
# 
#   }
#   
#     
#   ## 31-years Correlation
#   for(j in c((1000+15):(2000-15))){
#     
#     ## Select Data
#     A.years = c(c(j-15):(j+15))
#     Sel_data = Sim.Series[which(Sim.Series$year%in%A.years),]
#     
#     ## Correlation
#     temp = cor.test(Sel_data$Tropcal.tas_10,Sel_data$Global.nbp_10,method = c("pearson"))
#     Sim.Series[which(Sim.Series$year == j), "Cor_tas_nbp"] = -temp$estimate
#     Sim.Series[which(Sim.Series$year == j), "Cor_tas_nbp_p"] = temp$p.value
#   }
#   
#   ## 31-years Variance
#   for(j in c((1000+15):(2000-15))){
#     
#     ## Select Data
#     A.years = c(c(j-15):(j+15))
#     Sel_data = Sim.Series[which(Sim.Series$year%in%A.years),]
#     
#     ## SD
#     Sim.Series[which(Sim.Series$year == j), "Var_nbp"] = var(Sel_data$Global.nbp_10,na.rm = T)
#     Sim.Series[which(Sim.Series$year == j), "Var_TroT"] = var(Sel_data$Tropcal.tas_10,na.rm = T)
#   }
#   
#   
# }
# 
# 
#   
# 
# Sim.Series$Tropcal.tas_10 = pass.filter.NA(Sim.Series$Tropcal.tas,W = 10,type = "high")
# Sim.Series$Global.nbp_10 = pass.filter.NA(Sim.Series$Global.nbp,W = 10,type = "high")
# 
# 
# 
# 
# 
# ## Parameters
# Total.summary=read.csv("Output Data/Reconstructed CGR/Ensemble Mean/Total.summary.csv")
# i = 14
# 
# ## Read Global Data
# for(i in 1:NROW(PMIP.model)){
#   
#   ## Read Data
#   out.name = paste0("Output Data/PMIIP_past1000/Global Average Series/",PMIP.model$model[i],"_",PMIP.model$varivant[i],".txt")
#   Sim.Series = read.csv(out.name,row.names = 1)%>%
#     dplyr::arrange(year,month)
# 
# 
#   ## Annual Values
#   Sim.Series = Sim.Series%>%
#     dplyr::group_by(year)%>%
#     dplyr::summarise(across(!(month),mean))
#   
#   ## Pass filter
#   Sim.Series$Tropcal.tas_10 = pass.filter.NA(Sim.Series$Tropcal.tas,W = 10,type = "high")
#   Sim.Series$Global.nbp_10 = pass.filter.NA(Sim.Series$Global.nbp,W = 10,type = "high")
#   
#   ## Change Mean Series
#   Sim.Series_mean = mean(Sim.Series$Global.nbp_10[which( Sim.Series$year%in%c(1959:2000))])
#   Sim.Series_sd = sd(Sim.Series$Global.nbp_10[which( Sim.Series$year%in%c(1959:2000))])
#   Obs_mean = mean(Total.summary$Total.Obs[which(Total.summary$year%in%c(1959:2000))])
#   Obs_sd = sd(Total.summary$Total.Obs[which(Total.summary$year%in%c(1959:2000))])
#   Sim.Series$Global.nbp_10 = (Sim.Series$Global.nbp_10- Sim.Series_mean)/Sim.Series_sd * Obs_sd + Obs_mean
#   
#   # ## Test Monthly
#   # plot(density(Sim.Series$Global.nbp,na.rm = TRUE))
#   # lines(density(Sim.Series$Global.nbp[which(Sim.Series$year%in%c(1900))],na.rm = TRUE))
#   # mean(Sim.Series$Global.nbp[which(Sim.Series$year%in%c(1900))])
#   # mean(Sim.Series$Global.nbp[which(Sim.Series$year%in%c(1903))])
#   # mean(Sim.Series$Global.nbp[which(Sim.Series$year%in%c(1904))])
# 
#   ## Remove Means
#   Sim.Series$Tropcal.tas_10 = Sim.Series$Tropcal.tas_10-mean(Sim.Series$Tropcal.tas_10,na.rm = TRUE)
#   Sim.Series$Global.nbp_10 = Sim.Series$Global.nbp_10 -mean(Sim.Series$Global.nbp_10 ,na.rm = TRUE)
#   
#   ## 31-years Correlation
#   j = 1950
#   for(j in c((1000+15):(2000-15))){
#     
#     ## Select Data
#     A.years = c(c(j-15):(j+15))
#     Sel_data = Sim.Series[which(Sim.Series$year%in%A.years),]
#     
#     ## Correlation
#     temp = cor.test(Sel_data$Tropcal.tas_10,Sel_data$Global.nbp_10,method = c("pearson"))
#     Sim.Series[which(Sim.Series$year == j), "Cor_tas_nbp"] = -temp$estimate
#     Sim.Series[which(Sim.Series$year == j), "Cor_tas_nbp_p"] = temp$p.value
#   }
#   
#   ## 31-years Variance
#   for(j in c((1000+15):(2000-15))){
#     
#     ## Select Data
#     A.years = c(c(j-15):(j+15))
#     Sel_data = Sim.Series[which(Sim.Series$year%in%A.years),]
# 
#     ## SD
#     Sim.Series[which(Sim.Series$year == j), "Var_nbp"] = var(Sel_data$Global.nbp_10,na.rm = T)
#     Sim.Series[which(Sim.Series$year == j), "Var_TroT"] = var(Sel_data$Tropcal.tas_10,na.rm = T)
#   }
#   
#   ## Prediction Calibration
#   A.years = c(1959:2000)
#   Sel_data = Sim.Series[which(Sim.Series$year%in%A.years),]
#   kk1 = lm(Global.nbp_10~Tropcal.tas_10, data = Sel_data)
#   Sim.Series$Pre_Global.nbp_10 = predict(kk1,Sim.Series)
# 
#   ## Prediction Performance
#   for(j in c((1000+15):(2000-15))){
#     
#     ## Select Data
#     A.years = c(c(j-15):(j+15))
#     Sel_data = Sim.Series[which(Sim.Series$year%in%A.years),]
#     
#     ## Correlation
#     temp = cor.test(Sel_data$Global.nbp_10,Sel_data$Pre_Global.nbp_10,method = c("pearson"))
#     Sim.Series[which(Sim.Series$year == j), "Cor_pre_nbp"] = temp$estimate
#     Sim.Series[which(Sim.Series$year == j), "Cor_pre_nbp_p"] = temp$p.value
#     temp = NSE(Sel_data$Global.nbp_10,Sel_data$Pre_Global.nbp_10)
#     Sim.Series[which(Sim.Series$year == j), "NSE_pre_nbp"] = temp
#   }
# 
#   ## Write
#   Sim.Series$Model = PMIP.model$model[i]
#   Sim.Series$varivant = PMIP.model$varivant[i]
#   out.name = paste0("Output Data/PMIIP_past1000/Statis/",PMIP.model$model[i],"_",PMIP.model$varivant[i],".txt")
#   write.csv(Sim.Series,out.name)
#   
#   ## Print
#   print(i)
# }
#  
# ## Test Time Series
# plot(Sel_data$year,scale(Sel_data$Tropcal.tas_10),type = "l",xlim = c(1900,2000))
# lines(Sel_data$year,-scale(Sel_data$Global.nbp_10),col = "red")
# 
# ## Test Comparison
# plot(Sel_data$Tropcal.tas_10,Sel_data$Global.nbp_10)
# cor.test(Sel_data$Tropcal.tas_10,Sel_data$Global.nbp_10)
# 
# ## Test Time Series
# plot(Sim.Series$year,scale(Sim.Series$Tropcal.tas_10),type = "l",xlim = c(1900,2000))
# lines(Sim.Series$year,-scale(Sim.Series$Global.nbp_10),col = "red")
# 
# ## Test Correlation
# plot(Sim.Series$year,Sim.Series$Cor_tas_nbp,type = "l",xlim = c(1000,2020))
# 
# ## Test Performance
# plot(Sim.Series$year,Sim.Series$NSE_pre_nbp,type = "l",xlim = c(1000,2020))
# 
# ## Test SD of nbp
# plot(Sim.Series$year,Sim.Series$Var_nbp,type = "l",xlim = c(1000,2020))
# plot(Sim.Series$year,Sim.Series$Tropcal.tas,type = "l",xlim = c(1000,2020))


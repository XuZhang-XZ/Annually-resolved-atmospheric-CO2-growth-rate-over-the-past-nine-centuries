

# Analyses for IVI2 ---------------------------------------------------------------

## VSSI
His_VSSI = read.table(
  "Z:/2023/Input data/Volcanic Forcing/Gao_2008_Volcanic forcing of climate/IVI2TotalInjection_501-2000Version2.txt",
  fill = NA,
  skip = 11,
  header = TRUE
)

## Filter
His_VSSI_Sel = His_VSSI %>%
  dplyr::mutate(Global = Global * 0.75 * 32 / 96) %>% ## Change Unit To Tg S
  dplyr::filter(Year %in% c(1100:2000) & Global >= 7.5) ## Larger than 1991 magnitude

## Read CGR Reconstruction
Total.summary=read.csv("Output Data/Reconstructed CGR/SD/Total.summary.sd.csv")
All.ensemble=read.csv("Output Data/Reconstructed CGR/Ensemble Mean/All.ensemble.csv")

## Observed Variance
sel.years.i = which(Total.summary$year%in%c((1959+15):(2020-15)))
for(i in sel.years.i) {
  Total.summary[i,"Obs.CGR.Var"]=var(Total.summary$Total.Obs[c((i-15):(i+15))])
}

## Replace with 1991-1992
temp = Total.summary$Total.Obs[which(Total.summary$year %in% c(1991:1992))]
All.ensemble1 = All.ensemble
All.ensemble1[which(Total.summary$year %in% (His_VSSI_Sel$Year + 0)),] = temp[1]
All.ensemble1[which(Total.summary$year %in% (His_VSSI_Sel$Year + 1)),] = temp[2]

## Reconstructed Variance
sel.years.i = which(Total.summary$year%in%c((1100+15):(2006-15)))
Analysis.CGR=data.frame("year"=Total.summary$year)
for(en.ge in 1:1000){
  for(i in sel.years.i) {
    Analysis.CGR[i,en.ge+1]=var(All.ensemble1[c((i-15):(i+15)),en.ge])
  }
  print(en.ge)
}

## Summary
Total.summary_1 = Total.summary
Total.summary_1$HR.SD.median=apply(Analysis.CGR[,-1],1,quantile,probs=0.5,na.rm=TRUE)
Total.summary_1$HR.SD.0.025=apply(Analysis.CGR[,-1],1,quantile,probs=0.025,na.rm=TRUE)
Total.summary_1$HR.SD.0.975=apply(Analysis.CGR[,-1],1,quantile,probs=0.975,na.rm=TRUE)

## Plot
plot(Total.summary$year,Total.summary$HR.SD.median,type = "l",xlim = c(1000,2000),ylim = c(0,0.45))
lines(Total.summary_1$year,Total.summary_1$HR.SD.median,type = "l",col = "red")

# Plot IVI2 --------------------------------------------------------------------

## Data
Total.summary$Type = "Reconstruction"
Total.summary_1$Type = "Sensitivity Test"
plot.data = rbind(Total.summary,Total.summary_1)

## Plot
p1 = ggplot() +
  geom_ribbon(data = plot.data,alpha = 0.2,color = NA,
              aes(x = year, ymin = HR.SD.0.025, ymax = HR.SD.0.975, fill = Type))+
  geom_path(data = plot.data,
            aes(x = year, y = HR.SD.median, color = Type))+
  geom_point(data = His_VSSI_Sel,shape = 8,color = "purple",
             aes(x = Year, y = 0.02)) +
  scale_color_discrete(breaks = c("Reconstruction","Sensitivity Test")) +
  scale_fill_discrete(breaks = c("Reconstruction","Sensitivity Test")) +
  scale_x_continuous(name = "Year",breaks = seq(1000,2000,100),sec.axis =  sec_axis(~ .)) +
  scale_y_continuous(name = "CGR Variance",sec.axis =  sec_axis(~ .)) +
  coord_cartesian(xlim = c(1100,2006), ylim = c(0,0.4),expand = F)+
  theme_figure1 +
  theme(
    plot.margin = margin(10, 12, 5, 15, "pt"),
    legend.position = "inside",
    legend.position.inside = c(0.7, 0.85),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.background = element_blank()
  )

# Analyses for eVolv2k ------------------------------------------------------------

## Read VSSI
His_VSSI = read.table("Z:/2025/Input Data/eVolv2k_version4/Sigl-Toohey_2024_eVolv2k_v4.tab",skip = 48,sep = "\t", header = TRUE)

## Select
His_VSSI = His_VSSI[ ,c(1,8)]
names(His_VSSI) = c("year","VSSI")

## Years
His_VSSI_Sel = His_VSSI %>%
  dplyr::filter(year %in% c(1100:2000) & VSSI >= 9)

## Add 1991
His_VSSI_Sel = His_VSSI_Sel %>% 
  dplyr::add_row(year = 1991, VSSI = 9)

## Read CGR Reconstruction
Total.summary=read.csv("Output Data/Reconstructed CGR/SD/Total.summary.sd.csv")
All.ensemble=read.csv("Output Data/Reconstructed CGR/Ensemble Mean/All.ensemble.csv")

## Observed Variance
sel.years.i = which(Total.summary$year%in%c((1959+15):(2020-15)))
for(i in sel.years.i) {
  Total.summary[i,"Obs.CGR.Var"]=var(Total.summary$Total.Obs[c((i-15):(i+15))])
}

## Replace with 1991-1992
temp = Total.summary$Total.Obs[which(Total.summary$year %in% c(1991:1992))]
All.ensemble1 = All.ensemble
All.ensemble1[which(Total.summary$year %in% (His_VSSI_Sel$year + 0)),] = temp[1]
All.ensemble1[which(Total.summary$year %in% (His_VSSI_Sel$year + 1)),] = temp[2]

## Reconstructed Variance
sel.years.i = which(Total.summary$year%in%c((1100+15):(2006-15)))
Analysis.CGR=data.frame("year"=Total.summary$year)
for(en.ge in 1:1000){
  for(i in sel.years.i) {
    Analysis.CGR[i,en.ge+1]=var(All.ensemble1[c((i-15):(i+15)),en.ge])
  }
  print(en.ge)
}

## Summary
Total.summary_1 = Total.summary
Total.summary_1$HR.SD.median=apply(Analysis.CGR[,-1],1,quantile,probs=0.5,na.rm=TRUE)
Total.summary_1$HR.SD.0.025=apply(Analysis.CGR[,-1],1,quantile,probs=0.025,na.rm=TRUE)
Total.summary_1$HR.SD.0.975=apply(Analysis.CGR[,-1],1,quantile,probs=0.975,na.rm=TRUE)

## Plot
plot(Total.summary$year,Total.summary$HR.SD.median,type = "l",xlim = c(1000,2000),ylim = c(0,0.45))
lines(Total.summary_1$year,Total.summary_1$HR.SD.median,type = "l",col = "red")

# Plot eVolv2k --------------------------------------------------------------------

## Data
Total.summary$Type = "Reconstruction"
Total.summary_1$Type = "Sensitivity Test"
plot.data = rbind(Total.summary,Total.summary_1)

## Plot
p2 = ggplot() +
  geom_ribbon(data = plot.data,alpha = 0.2,color = NA,
              aes(x = year, ymin = HR.SD.0.025, ymax = HR.SD.0.975, fill = Type))+
  geom_path(data = plot.data,
            aes(x = year, y = HR.SD.median, color = Type))+
  geom_point(data = His_VSSI_Sel,shape = 8,color = "purple",
             aes(x = year, y = 0.02)) +
  scale_color_discrete(breaks = c("Reconstruction","Sensitivity Test")) +
  scale_fill_discrete(breaks = c("Reconstruction","Sensitivity Test")) +
  scale_x_continuous(name = "Year",breaks = seq(1000,2000,100),sec.axis =  sec_axis(~ .)) +
  scale_y_continuous(name = "CGR Variance",sec.axis =  sec_axis(~ .)) +
  coord_cartesian(xlim = c(1100,2006), ylim = c(0,0.4),expand = F)+
  theme_figure1 +
  theme(
    plot.margin = margin(10, 12, 5, 15, "pt"),
    legend.position = "inside",
    legend.position.inside = c(0.7, 0.85),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.background = element_blank()
  )

# Saving ------------------------------------------------------------------

## Sum
p.total = plot_grid(p1, p2, ncol = 1, labels = "auto")

## Saving
ggsave(
  plot = p.total,
  filename = "Figures/Analysis/Test for eruptions.tiff",
  width = 17,
  height = 17,
  unit = "cm"
)

# End ---------------------------------------------------------------------




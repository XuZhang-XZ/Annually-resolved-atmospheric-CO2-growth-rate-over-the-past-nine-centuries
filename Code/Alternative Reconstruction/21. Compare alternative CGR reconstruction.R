


# Data --------------------------------------------------------------------

## Data
Total.summary1=read.csv("Output Data/Reconstructed CGR/SD/Total.summary.sd.csv")
Total.summary2=read.csv("Output Data/Reconstructed CGR/SD/Total.summary.sd_Cor predictors.csv")
Total.summary3=read.csv("Output Data/Reconstructed CGR/SD/Total.summary.sd_MLR predictors.csv")
Total.summary4=read.csv("Output Data/Reconstructed CGR/SD/Total.summary.sd_Low predictors.csv")
Total.summary5=read.csv("Output Data/Reconstructed CGR/SD/Total.summary.sd_Cor_S.csv")

## Plot Data
plot.data = data.frame(
  "year" = Total.summary1$year,
  
  "CGR_All" = Total.summary1$Pre.median,
  "CGR_Cor" = Total.summary2$Pre.median,
  "CGR_MLR" = Total.summary3$Pre.median,
  "CGR_Low" = Total.summary4$Pre.median,
  "CGR_CoS" = Total.summary5$Pre.median,
  
  "CGR_Var_All" = Total.summary1$HR.SD.median,
  "CGR_Var_All_1" = Total.summary1$HR.SD.0.025,
  "CGR_Var_All_2" = Total.summary1$HR.SD.0.975,

  "CGR_Var_Cor" = Total.summary2$HR.SD.median,
  "CGR_Var_Cor_1" = Total.summary2$HR.SD.0.025,
  "CGR_Var_Cor_2" = Total.summary2$HR.SD.0.975,

  "CGR_Var_MLR" = Total.summary3$HR.SD.median,
  "CGR_Var_MLR_1" = Total.summary3$HR.SD.0.025,
  "CGR_Var_MLR_2" = Total.summary3$HR.SD.0.975,

  "CGR_Var_Low" = Total.summary4$HR.SD.median,
  "CGR_Var_Low_1" = Total.summary4$HR.SD.0.025,
  "CGR_Var_Low_2" = Total.summary4$HR.SD.0.975,
  
  "CGR_Var_CoS" = Total.summary5$HR.SD.median,
  "CGR_Var_CoS_1" = Total.summary5$HR.SD.0.025,
  "CGR_Var_CoS_2" = Total.summary5$HR.SD.0.975
)

## Calculate Correlation
sel.years.i = which(plot.data$year%in%c(1201:2000))
i = 1200
for(i in sel.years.i) {
  Temp = plot.data[c((i-100):i),]
  plot.data[i,"All_Cor"]= cor25.1(Temp$CGR_All,Temp$CGR_Cor)[1]
  plot.data[i,"All_MLR"]= cor25.1(Temp$CGR_All,Temp$CGR_MLR)[1]
  plot.data[i,"All_Low"]= cor25.1(Temp$CGR_All,Temp$CGR_Low)[1]
  plot.data[i,"All_CoS"]= cor25.1(Temp$CGR_All,Temp$CGR_CoS)[1]
}

## Plot
plot(plot.data$year,plot.data$All_Cor,ylim = c(0,1))
lines(plot.data$year,plot.data$All_MLR,col = "red")
lines(plot.data$year,plot.data$All_Low,col = "red")
lines(plot.data$year,plot.data$All_CoS,col = "red")

## Plot
plot(plot.data$year,plot.data$CGR_Var_All, type = "l")
lines(plot.data$year,plot.data$CGR_Var_Cor,col = "blue")
lines(plot.data$year,plot.data$CGR_Var_CoS,col = "red")

# Plot --------------------------------------------------------------------

## Data
plot.data1 = pivot_longer(plot.data,cols = c("CGR_Var_All","CGR_Var_Cor","CGR_Var_MLR","CGR_Var_Low","CGR_Var_CoS"))
colors7=brewer.pal(n=9,"Set1")[c(1,2,4,7,3)]

## Correlation
p2 = ggplot() +
  geom_ribbon(data = plot.data, fill = colors7[1],alpha = 0.2,color = NA,
              aes(x = year, ymin = CGR_Var_All_1, ymax = CGR_Var_All_2)) +
  geom_ribbon(data = plot.data, fill = colors7[2],alpha = 0.2,color = NA,
              aes(x = year, ymin = CGR_Var_Cor_1, ymax = CGR_Var_Cor_2)) +
  geom_ribbon(data = plot.data, fill = colors7[3],alpha = 0.2,color = NA,
              aes(x = year, ymin = CGR_Var_MLR_1, ymax = CGR_Var_MLR_2)) +
  geom_ribbon(data = plot.data, fill = colors7[4],alpha = 0.2,color = NA,
              aes(x = year, ymin = CGR_Var_Low_1, ymax = CGR_Var_Low_2)) +
  geom_ribbon(data = plot.data, fill = colors7[5],alpha = 0.2,color = NA,
              aes(x = year, ymin = CGR_Var_CoS_1, ymax = CGR_Var_CoS_2)) +
  geom_path(data = plot.data1,
            aes(x = year, y = value, color = name)) +
  scale_color_manual(
    values = colors7[c(1:5)],
    breaks = c("CGR_Var_All","CGR_Var_Cor","CGR_Var_CoS","CGR_Var_MLR","CGR_Var_Low"),
    labels = c("Final proxies (82)", "Correlated proxies_1 (114)", "Correlated proxies_2 (125)","Mid- and low-latitude proxies (73)","Low-latitude proxies (56)")
  ) +
  scale_x_continuous(breaks = seq(0,2000,100),name = "Year") +
  scale_y_continuous(breaks = c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4),name = "CGR variance")+
  coord_cartesian(xlim = c(1100,2000), ylim = c(0.00,0.4),expand = FALSE) +
  P_Xu_F1 +
  theme(
    legend.position = "inside",
    legend.justification.inside = c(0.99,0.99),
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.background = element_blank(),
    plot.margin = margin(10, 15, 10, 10, "pt")
  )

## Saving
ggsave(
  plot = p2,
  filename = "Figures/Performance/Alternative Reconstruction_Predictors.tiff",
  width = 8,
  height = 5
)

# End ---------------------------------------------------------------------




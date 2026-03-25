

# Read Data --------------------------------------------------------------------

## Read Data
Total.summary1=read.csv("Output Data/Reconstructed CGR/SD/Total.summary.sd.csv")
Total.summary2=read.csv("Output Data/Reconstructed CGR/SD/Total.summary.sd_RF.csv")

## Data for plotting
plot.data = data.frame(
  "year" = Total.summary1$year,
  
  "CGR_All" = Total.summary1$Pre.median,
  "CGR_RF" = Total.summary2$Pre.median,

  "CGR_Var_All" = Total.summary1$HR.SD.median,
  "CGR_Var_All_1" = Total.summary1$HR.SD.0.025,
  "CGR_Var_All_2" = Total.summary1$HR.SD.0.975,
  
  "CGR_Var_RF" = Total.summary2$HR.SD.median,
  "CGR_Var_RF_1" = Total.summary2$HR.SD.0.025,
  "CGR_Var_RF_2" = Total.summary2$HR.SD.0.975
)

## Calculate Correlation
sel.years.i = which(plot.data$year%in%c(1201:2000))
i = 1200
for(i in sel.years.i) {
  Temp = plot.data[c((i-100):i),]
  plot.data[i,"Cor_All_RF"]= cor25.1(Temp$CGR_All,Temp$CGR_RF)[1]
}

## Plot
plot(plot.data$year,plot.data$Cor_All_RF,ylim = c(0,1))

# Plot --------------------------------------------------------------------

## Correlation
p1 = ggplot() +
  geom_path(data = plot.data,
            aes(x = year, y = Cor_All_RF)) +
  scale_x_continuous(breaks = seq(0,2000,100),name = "Year") +
  scale_y_continuous(breaks = seq(0,1,0.2),name = "Correlation") +
  coord_cartesian(xlim = c(1100,2000), ylim = c(0,1),expand = FALSE) +
  P_Xu_F1 +
  theme(
    legend.position = "inside",
    legend.justification.inside = c(0.99,0.99),
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.background = element_blank(),
    plot.margin = ggplot2::margin(10, 15, 10, 10, "pt")
  )

## Data
plot.data1 = pivot_longer(plot.data,cols = c("CGR_Var_All","CGR_Var_RF"))
colors7 = brewer.pal(n = 9, "Set1")[c(1, 3)]

## Correlation
p2 = ggplot() +
  geom_ribbon(data = plot.data, fill = colors7[1],alpha = 0.2,color = NA,
              aes(x = year, ymin = CGR_Var_All_1, ymax = CGR_Var_All_2)) +
  geom_ribbon(data = plot.data, fill = colors7[2],alpha = 0.2,color = NA,
              aes(x = year, ymin = CGR_Var_RF_1, ymax = CGR_Var_RF_2)) +
  geom_path(data = plot.data1,
            aes(x = year, y = value, color = name)) +
  scale_color_manual(
    values = colors7[c(1:2)],
    breaks = c("CGR_Var_All","CGR_Var_RF"),
    labels = c("PCR", "Random Forests")
  ) +
  scale_x_continuous(breaks = seq(0,2000,100),name = "Year") +
  scale_y_continuous(breaks = c(0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45),name = "CGR variance") +
  coord_cartesian(xlim = c(1100,2000), ylim = c(0.00,0.45),expand = FALSE) +
  P_Xu_F1 +
  theme(
    legend.position = "inside",
    legend.justification.inside = c(0.99,0.99),
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.background = element_blank(),
    plot.margin = ggplot2::margin(10, 15, 10, 10, "pt")
  )

## Total
p.total = plot_grid(p1,p2,nrow = 1,align = "h",labels = "auto")

## Saving
ggsave(
  plot = p.total,
  filename = "Figures/Performance/Alternative Reconstruction_RF.tiff",
  width = 21,
  height = 10,
  unit = "cm"
)

# End ---------------------------------------------------------------------




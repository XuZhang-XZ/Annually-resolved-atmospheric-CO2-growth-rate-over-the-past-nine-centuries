

# Plot CGR Time Series ------------------------------------------------------------

## Read Data
Total.summary=read.csv("Output Data/Reconstructed CGR/Ensemble Mean/Total.summary.csv")
colors_temp2 = brewer.pal(n=9,"Set1")

## Plot
p1=ggplot()+
  geom_ribbon(data=Total.summary,
              aes(x=year,ymin=Pre.0.025,ymax=Pre.0.975),fill="gray",alpha=1)+
  geom_path(data = Total.summary,
            aes(x = year, y = Pre.median),color = colors_temp2[1],linewidth = 0.3)+
  geom_hline(yintercept = 0,color = "blue", linetype = "dashed",linewidth=0.3)+
  scale_x_continuous(limits=c(1100,2006),breaks = seq(1000,2000,100),expand = c(0,0),
                     sec.axis = sec_axis( trans=~.+0,breaks = seq(1000,2000,100)))+
  scale_y_continuous(name = "CGR (ppm/year)",
                     sec.axis = sec_axis( trans=~.+0,breaks = c(-1,-0.5,0,0.5,1,1.5), name = "CGR (ppm/year)")
  ) +
  coord_cartesian(ylim = c(-1.5,1.5),expand = F)+
  P_Xu_F1+
  theme(
    panel.border = element_blank(),
    axis.line.x.top = element_line(linewidth = 0.3, color = "black"),
    axis.title.x.top = element_blank(),
    axis.text.x.top = element_blank(),
    axis.ticks.length.x.top = unit(0,"cm"),
    axis.line.y = element_line(linewidth = 0.3, color = "black"),
    legend.position = "none",
    plot.margin = margin(5, 13, 0, 13, "pt"),
    axis.title.x.bottom = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.ticks.length.x.bottom = unit(0,"cm"),
    axis.title.y.left = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    axis.title.y.right = element_text(color = "black"),
    axis.text.y.right = element_text(color = "black"),
    axis.ticks.y.right = element_line(color = "black")
  )

# Plot CGR Variance ---------------------------------------------------------

## Data
Total.summary.sd = read.csv("Output Data/Reconstructed CGR/SD/Total.summary.sd.csv")
Total.summary.sd$CGR.Var.1970.2000 = Total.summary.sd$HR.SD.median[which(Total.summary.sd$year == 1985)]
Total.summary.sd$Obs.CGR.Var.1976.2006 = Total.summary.sd$Obs.CGR.Var[which(Total.summary.sd$year == 1991)]

## Output for Source Data
Total.summary.sd1 = Total.summary.sd[, c(
  "year",
  "Pre.median",
  "Pre.0.025",
  "Pre.0.975",
  "HR.SD.median",
  "HR.SD.0.025",
  "HR.SD.0.025"
)]
Total.summary.sd1 = subset(Total.summary.sd1, year >= 1100)
write_xlsx(Total.summary.sd1,"Output Data/Source Data/Total.summary.sd1.xlsx")

## Plot Data
All.CGR.plot = pivot_longer(data = Total.summary.sd, cols = c("HR.SD.median","Obs.CGR.Var","Obs.CGR.Var.1976.2006"))
colors_temp2 = brewer.pal(n=9,"Set1")

## Plot
p2 = ggplot() +
  geom_ribbon(data=Total.summary.sd,
              aes(x=year,ymin=HR.SD.0.025,ymax=HR.SD.0.975),fill=colors_temp2[2],alpha=0.3)+
  geom_path(data=All.CGR.plot,size = 0.5,
            aes(x=year,y=value,color = name))+
  scale_color_manual(values = colors_temp2[c(2,3,4,5)],
                     breaks = c("HR.SD.median","Obs.CGR.Var","CGR.Var.1970.2000","Obs.CGR.Var.1976.2006"),
                     labels = c("Reconstructed CGR", "Observed CGR", "Reconstructed CGR (1970-2000)", "Observed CGR (1976-2006)"))+
  scale_x_continuous(limits=c(1100,2006),
                     breaks = seq(1000,2000,100),
                     expand = c(0,0),
                     sec.axis = sec_axis( trans=~.+0,breaks = seq(1000,2000,100)))+
  scale_y_continuous(name = "CGR Variance",
                     breaks = seq(0.05,0.35,0.05),
                     sec.axis = sec_axis( trans=~.+0, name="CGR Variance")
  ) +
  coord_cartesian(ylim = c(0.01,0.4),expand = F)+
  P_Xu_F1+
  theme(
    panel.border = element_blank(),
    axis.line.y = element_line(linewidth = 0.3, color = "black"),
    legend.position = "inside",
    legend.justification.inside = c(0.7,0.8),
    plot.margin = margin(0, 13, 0, 13, "pt"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.line.x = element_blank(),
    axis.ticks.length.x = unit(0,"cm"),
    axis.title.y.left = element_text(color = "black"),
    axis.text.y.left = element_text(color = "black"),
    axis.ticks.y.left = element_line(color = "black"),
    axis.title.y.right = element_blank(),
    axis.text.y.right = element_blank(),
    axis.ticks.y.right = element_blank()
  )

# Plot CO2 from ice core ------------------------------------------------

## Read Data
CO2.ice.points = read.csv("Output Data/Reconstructed CGR/SD/CO2.ice.points.csv")

## 1000-1800
CO2.ice.points1 = CO2.ice.points%>%dplyr::filter(year<=1800)

## 1800-2000
CO2.ice.points2 = CO2.ice.points%>%dplyr::filter(year>=1800)
CO2.ice.points2$CO2_Change =  (CO2.ice.points2$CO2 - 281) /8+281

## Polygon
figure.e = data.frame("x" = c(2006,1800,1800,2006),
                      "y" = c(279,279,291.5,291.5))

## Plot
p3 = ggplot()+
  geom_path(data = figure.e, aes(x = x, y = y),
            color = "black",linetype = "longdash",linewidth = 0.3)+
  geom_point(data=CO2.ice.points1,
            aes(x=year,y=CO2, color = Type),size = 1)+
  geom_path(data=CO2.ice.points1,
             aes(x=year,y=CO2, color = Type),size = 0.5)+
  geom_point(data=CO2.ice.points2,
             aes(x=year,y=CO2_Change, color = Type),size = 1)+
  geom_path(data=CO2.ice.points2,
            aes(x=year,y=CO2_Change, color = Type),size = 0.5)+
  scale_color_manual(values = colors_temp2[c(5,7)],breaks = c("WAIS","Law Dome"),
                     labels = c("WAIS","Law Dome"))+
  scale_x_continuous(breaks = seq(1000,2000,100),
                     expand = c(0,0),
                     sec.axis = sec_axis( trans=~.+0,breaks = seq(1000,2000,100)))+
  scale_y_continuous(name = expression(CO[2]*" Concentration (ppm)"),
                     sec.axis = sec_axis( trans=~(.-280)*8+280, 
                                          breaks = c(280,300,320,340,360),
                                          name=expression(CO[2]*" Concentration (ppm)")))+
  coord_cartesian(xlim = c(1100,2006),ylim = c(269,292),expand = 0)+
  P_Xu_F1+
  theme(
    panel.border = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_line(linewidth = 0.3, color = "black"),
    legend.position = "inside",
    legend.position.inside = c(0.7,0.8),
    plot.margin = margin(0, 13, 0, 13, "pt"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.length.x = unit(0,"cm"),
    axis.title.y.left = element_text(color = "black"),
    axis.text.y.left = element_text(color = "black"),
    axis.ticks.y.left = element_line(color = "black"),
    axis.title.y.right = element_text(color = "black",size = 10),
    axis.text.y.right = element_text(color = "black"),
    axis.ticks.y.right = element_line(color = "black")
  )

# Plot Tropical Temperature -----------------------------------------------------

## Read Data
Total_Tropic = read.csv("Output Data/Reconstructed CGR/SD/Total_Tropic.csv")
Total_Tropic$Tropical.T_Rec[which(Total_Tropic$year %in% c(1981:2020))] = NA
Tropical.T1 = pivot_longer(data = Total_Tropic, cols = c("Tropical.T_Rec","Tropical.T"))

## Write Source Data
Total_Tropic_S = Total_Tropic[, c("year", "Tropical.T_Rec", "Tropical.T")]
Total_Tropic_S = subset(Total_Tropic_S, year >= 1100)
write_xlsx(Total_Tropic_S,"Output Data/Source Data/Total_Tropic_S.xlsx")

## Plot
p4 = ggplot()+
  geom_ribbon(data=Total_Tropic,
              aes(x=year,ymin=Lower.T,ymax=Upper.T),fill=colors_temp2[1],alpha=0.1)+
  geom_path(data = Tropical.T1, aes(x = year, y = value, color = name),linewidth = 0.8)+
  scale_color_manual(values = colors_temp2[c(1,2)],breaks = c("Tropical.T_Rec","Tropical.T"),
                     labels = c("Synthesized proxies ","Observations"))+
  scale_x_continuous(
    limits = c(1100, 2006),
    breaks = seq(1100, 2000, 50),
    labels = c("1100"," ","1200"," ","1300"," ","1400"," ","1500"," ","1600"," ","1700"," ","1800"," ","1900"," ","2000"),
    expand = c(0, 0),
    name = "Year",
    sec.axis = sec_axis(transform =  ~ . + 0, breaks = seq(1000, 2000, 100))
  ) +
  scale_y_continuous(
    name = "Temperature Anomaly (°C)",
    sec.axis = sec_axis(transform =  ~ ., name = "Temperature Anomaly (°C)",breaks = c(-1,-0.5,0,0.5))
  ) +
  coord_cartesian(xlim = c(1100,2006),ylim = c(-1,0.6),expand = 0)+
  P_Xu_F1+
  theme(
    panel.border = element_blank(),
    axis.line.x.bottom = element_line(linewidth = 0.3, color = "black"),
    axis.line.y = element_line(linewidth = 0.3, color = "black"),
    legend.position = "inside",
    legend.position.inside = c(0.7,0.8),
    plot.margin = margin(0, 13, 5, 13, "pt"),
    axis.title.x.top = element_blank(),
    axis.text.x.top = element_blank(),
    axis.line.x.top = element_blank(),
    axis.ticks.length.x.top = unit(0,"cm"),
    axis.title.y.left = element_blank(),
    axis.text.y.left = element_blank(),
    axis.ticks.y.left = element_blank(),
    axis.title.y.right = element_text(color = "black"),
    axis.text.y.right = element_text(color = "black"),
    axis.ticks.y.right = element_line(color = "black")
  )

# Saving Fig. 2 ----------------------------------------------------------

## Data
p.upper=plot_grid(p1,p2,p3,p4,
                  labels = c("a","b","c","e"),
                  align = "v",
                  ncol = 1,
                  rel_heights = c(0.9,1.3,1,1.1))

## Plot
ggsave(
  plot = p.upper,
  filename = "Figures/Final Figures/Figure 2_temp.tiff",
  dpi = 450,
  width = 9,
  height = 11
)
ggsave(
  plot = p.upper,
  filename = "Figures/Final Figures/Figure 2_temp.pdf",
  device = cairo_pdf,
  dpi = 450,
  width = 10,
  height = 12
)

# Scatter plot Temperature and CGR -----------------------------------------------------

## Read Data
Total.summary.sd = read.csv("Output Data/Reconstructed CGR/SD/Total.summary.sd.csv")
Total_Tropic = read.csv("Output Data/Reconstructed CGR/SD/Total_Tropic.csv")
WAIS_CO2 = read.csv("Output Data/Reconstructed CGR/SD/CO2.ice.points.csv") %>%
  dplyr::filter(Type == "Law Dome") %>%
  dplyr::mutate(year = floor(year))

## Summary
Total = merge(Total.summary.sd, Total_Tropic,all.x = T)
Total = merge(Total,WAIS_CO2,all.x = T)
colors_temp2 = brewer.pal(n=9,"Set1")

## Plot Data
P_Data = Total[,c("year","HR.SD.median","Tropical.T_Rec","CO2")]

## Replace
P_Data[which(P_Data$year %in% c(1974:2020)),c("HR.SD.median")] = Total[which(Total$year %in% c(1974:2020)),c("Obs.CGR.Var")]
P_Data[which(P_Data$year %in% c(1981:2020)),c("Tropical.T_Rec")] =  Total[which(Total$year %in% c(1981:2020)),c("Tropical.T")]
P_Data$Type = NA
P_Data$Type[which(P_Data$year < 1980)] = "1100-1979"
P_Data$Type[which(P_Data$year >= 1980)] = "1980-2020"

## Plot
p_1 = ggplot() +
  geom_point(data = P_Data, 
             aes(x = Tropical.T_Rec, y = CO2, color = Type)) +
  stat_ellipse(
    data = P_Data,
    aes(x = Tropical.T_Rec, y = CO2, color = Type),
    show.legend = F,
    linetype = 2,
    lwd = 1.2
  ) +
  labs(x = "Tropical temperature (°C)", y = expression(CO[2]*" concentration (ppm)")) +
  theme_figure1 +
  theme(
    plot.margin = margin(8, 12, 10, 15, "pt"),
    legend.position = "inside",
    legend.justification.inside = c(0.91, 0.01),
    legend.title = element_blank(),
    legend.background = element_blank()
  )

## Plot
p_2 = ggplot() +
  geom_point(data = P_Data, 
             aes(x = Tropical.T_Rec, y = HR.SD.median, color = Type)) +
  stat_ellipse(data = P_Data, 
               aes(x = Tropical.T_Rec, y = HR.SD.median, color = Type),
               show.legend = F,
               linetype = 2,
               lwd = 1.2)+
  labs(x = "Tropical temperature (°C)", y = "CGR variance") +
  theme_figure1+
  theme(
    plot.margin = margin(8, 12, 10, 15, "pt"),
    legend.position = "inside",
    legend.justification.inside = c(0.91,0.01),
    legend.title = element_blank(),
    legend.background = element_blank()
  )

## Summary
p_total = plot_grid(p_1, p_2, nrow = 1, labels = "auto")

## Saving
ggsave(
  plot = p_total,
  filename = "Figures/Analysis/CGR and T.tiff",
  width = 9,
  height = 4.5
)

# Plot CGR Variance against SOI ----------------------------------------------------

## Colors
colors7=brewer.pal(n=11,"Set1")

## Data
Total.summary.sd = read.csv("Output Data/Reconstructed CGR/SD/Total.summary.sd.csv")

## SOI Data
soi_data = read_SPSL_SOI(file.name = "Input Data/SOI index/soi_3dp.dat.txt",Title1 = "SOI",NA.value = "-99.990")
soi_data$year = year(soi_data$Date)
soi_data$month = month(soi_data$Date)

## From previous July to December
soi_data$year[soi_data$month%in%c(7:12)] = soi_data$year[soi_data$month%in%c(7:12)]+1
soi_data = soi_data[soi_data$year%in%c(1867:2020),]
soi_Yearly = aggregate(cbind(SOI) ~ year, data = soi_data,mean)
soi_Yearly = subset(soi_Yearly, year %in% c(1959:2020))

## Moving Average
soi_Yearly$SOI_A = stats::filter(soi_Yearly$SOI, filter = rep(1/31,31), side = 2)

## SOI_Yearly
soi_Yearly1 = subset(soi_Yearly, year %in% c(1961:1980))
soi_Yearly2 = subset(soi_Yearly, year %in% c(1981:2000))
soi_Yearly3 = subset(soi_Yearly, year %in% c(2001:2020))
length(which(soi_Yearly1$SOI < (-1)))
length(which(soi_Yearly2$SOI < (-1)))
length(which(soi_Yearly3$SOI < (-1)))

## Plot
plot(Total.summary.sd$year, scale(Total.summary.sd$Obs.CGR.Var), type = "l",xlim = c(1900,2050))
lines(soi_Yearly$year, -scale(soi_Yearly$SOI_A), type = "l", col = "red")
plot(soi_Yearly$year,soi_Yearly$SOI, type = "l")
plot(soi_Yearly$year,soi_Yearly$SOI_A, type = "l")

## Adjustment for two y axes
plot.data0 = merge(Total.summary.sd, soi_Yearly, all.x = TRUE, by = "year")
lm(Obs.CGR.Var~SOI_A, data = plot.data0)
plot.data0$SOI_A = plot.data0$SOI_A* (-0.242) + 0.136
plot.data1 = pivot_longer(plot.data0, cols=c("Obs.CGR.Var","SOI_A"))

## Compare with land sink
p_Sink = ggplot() +
  geom_path(data = plot.data1,
            linewidth = 0.5,
            aes(x = year, y = value, color = name)) +
  scale_color_manual(
    values=  colors7[1:2],
    breaks = c("Obs.CGR.Var","SOI_A"),
    labels = c("CGR Variance", "SOI"),
    guide = guide_legend(nrow = 2)
  ) +
  scale_x_continuous(name = "Year",breaks = seq(1940,2020,10),sec.axis =  sec_axis(~ .)) +
  scale_y_continuous(name = "CGR Variance",breaks = seq(0.1,0.3,0.05),
                     sec.axis = sec_axis( trans= ~ (. - 0.136) / (-0.242), name="SOI"))+
  coord_cartesian(xlim = c(1970,2010), ylim = c(0.1,0.3),expand = F) +
  theme_figure1 +
  theme(
    plot.margin = margin(8, 12, 0, 15, "pt"),
    legend.position = "inside",
    legend.justification.inside = c(0.2, 0.97),
    legend.title = element_blank(),
    legend.background = element_blank(),
    axis.title.y.right = element_text(color = colors7[2]),
    axis.ticks.y.right = element_line(linewidth = 0.3, color = colors7[2]),
    axis.ticks.length.y.right = unit(4, "pt"),
    axis.text.y.right = element_text(color = colors7[2]),
    axis.title.y.left = element_text(color = colors7[1]),
    axis.ticks.y.left = element_line(linewidth = 0.3, color = colors7[1]),
    axis.ticks.length.y.left = unit(4, "pt"),
    axis.text.y.left = element_text(color = colors7[1])
  )

## Saving
ggsave(
  plot = p_Sink,
  filename = "Figures/Analysis/CGR variance and SOI.tiff",
  width = 15,
  height = 10,
  unit = "cm"
)

# End -------------------------------------------------------------


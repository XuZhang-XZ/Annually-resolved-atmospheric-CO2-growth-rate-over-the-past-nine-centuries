

# read global CGR ----------------------------------------------------------------

## Read
CGR=read.table("Input Data/Carbon Dioxide/co2_gr_gl.txt",header = TRUE)
CGR = CGR[, c(1, 2, 3)]
names(CGR) = c("year", "CGR", "Uncer")

## De-trend and high-pass-filtered series
CGR[, "Detrend.CGR"] = detrend.linear(CGR[, "CGR"])
CGR[, "CGR.high"] = pass.filter.NA.mean(
  a = CGR[, "CGR"],
  W = 10,
  type = "high",
  method = c("Butterworth")
)
write.csv(CGR, "Output Data/CGR/Raw.CGR.csv", row.names = FALSE)

## Remove years impacted by volcanic eruptions
CGR1 = data.frame(
  "year" = CGR$year,
  "Obs" = CGR$CGR.high,
  "Total.Obs" = CGR$CGR.high,
  "Total.Obs1" = CGR$CGR.high
)
Volcanio.year = c(1991,1992)
CGR1$Obs[which(CGR1$year%in%Volcanio.year)]=NA

## Remove years after 2000
CGR1$Obs[which(CGR1$year%in%c(2001:2020))]=NA
CGR1$Total.Obs1[which(CGR1$year%in%Volcanio.year)]=NA
write.csv(CGR1,"Output Data/CGR/CGR.csv",row.names = FALSE)

# Plot ------------------------------------------------------------

## Data
colors7 = brewer.pal(n=7,"Set1")

## Observation
p1=ggplot()+
  geom_path(data=CGR,aes(x=year,y=CGR),color = "black")+
  geom_vline(xintercept = c(1963,1982,1991),linetype = "longdash", col = "red",linewidth = 0.4)+
  scale_x_continuous(sec.axis =  sec_axis(~.))+
  scale_y_continuous(sec.axis =  sec_axis(~.), name = "CGR (ppm/year)",
                     breaks = seq(-1,4,1),
                     limits = c(-1,4))+
  coord_cartesian(xlim = c(1957,2020),ylim = c(-0.8,4), expand = F)+
  theme_figure1+
  theme(
    plot.margin = margin(5, 15, 0, 5, "pt"),
    axis.line.x.bottom = element_blank(),
    axis.title.x.bottom = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    axis.ticks.length.x.bottom = unit(0, "pt")
  )

## Filtered Series
temp.data=pivot_longer(data=CGR,cols=c("Detrend.CGR","CGR.high"))
Erup=data.frame("xmin"=1990.5,"xmax"=1992.5,"ymin"=-5,"ymax"=5)
p2=ggplot()+
  geom_ribbon(data=CGR,aes(x=year,ymin=CGR.high-2*CGR.sd,ymax=CGR.high+2*CGR.sd),fill = "gray40",alpha = 0.5)+
  geom_path(data=temp.data,aes(x=year,y=value,color=name))+
  geom_rect(data=Erup,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill="red",alpha=0.2)+
  scale_color_discrete(breaks = c("Detrend.CGR","CGR.high"),labels=c("De-trended","10-yr high-pass filtered"))+
  P_Xu_F1+
  ylab(expression(CGR[A]*" (ppm/year)"))+
  coord_cartesian(xlim = c(1957,2020),ylim = c(-1.2,1.2), expand = F)+
  theme(legend.position = c(0.17,0.8))


## NINO 3.4
NINO34=read_SPSL(file.name = "Input Data/NINO 3.4/NINO 3.4 PSL.txt", Title1 = "NINO34")
NINO34$year = year(NINO34$Date)
NINO34$month = month(NINO34$Date)

## Adjustment for NINO 3.4 from previous July to December
NINO34$year[which(NINO34$month%in%c(7:12))]=NINO34$year[which(NINO34$month%in%c(7:12))]+1

## Annual Values and high-pass filtered
Annual.NINO34=aggregate(NINO34~year,data=NINO34,mean,na.rm=TRUE)
Annual.NINO34=Annual.NINO34[-which(Annual.NINO34$year==1870|Annual.NINO34$year==2021),]
Annual.NINO34$NINO34.10=pass.filter.NA(a=Annual.NINO34$NINO34,W=10,type="high",method = c("Butterworth"))

## Total Data
Total = merge(CGR, Annual.NINO34)

## Adjustment for two y axes. The coefficients are derived from linear regression in the following line
kk1 = lm(CGR.high~NINO34.10, data = Total)
Total$NINO34.10 = Total$NINO34.10*0.36926 +0.03571
temp.data=pivot_longer(data=Total,cols=c("CGR.high","NINO34.10"))

## Plot
p3 = ggplot()+
  geom_path(data = temp.data, aes(x = year, y = value, color = name)) +
  geom_vline(xintercept = c(1963,1982,1991),linetype = "longdash", col = "red",linewidth = 0.4)+
  scale_color_manual(breaks = c("CGR.high","NINO34.10"),labels=c("CGR","NINO 3.4"), values = c("black","blue"))+
  scale_x_continuous(sec.axis =  sec_axis(~.))+
  scale_y_continuous(sec.axis =  sec_axis(~(.-0.03571)/0.36926, name = "NINO 3.4 (°C)"), name = "CGR (ppm/year)")+
  coord_cartesian(xlim = c(1957,2020), ylim = c(-1.2,1.2), expand = F)+
  theme_figure1+
  theme(
    plot.margin = margin(0, 15, 0, 5, "pt"),
    legend.position = "inside",
    legend.justification.inside = c(1,0),
    legend.direction = "horizontal",
    axis.title.y.right = element_text(color = "blue"),
    axis.text.y.right = element_text(color = "blue"),
    axis.ticks.length.y.right = unit(4, "pt"),
    axis.ticks.y.right = element_line(color = "blue"),
    axis.line.x.top = element_line(linewidth = 0.3, color = "black"),
    axis.line.x.bottom = element_blank(),
    axis.title.x.bottom = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    axis.ticks.length.x.bottom = unit(0, "pt")
  )

## Tropical Temperature
Noah=nc_open("Z:/2023/Input data/HadCRUT5/HadCRUT.5.0.1.0.analysis.anomalies.ensemble_mean.nc")
Tmp.data=ncvar_get(Noah,"tas_mean")
i.lon=ncvar_get(Noah, "longitude")
i.lat=ncvar_get(Noah, "latitude")
i.time=ncvar_get(Noah, "time")
Tmp.time1=i.time+ymd(18500101)
Tmp.time1 = ymd(year(Tmp.time1)*10000+month(Tmp.time1)*100+1)
nc_close(Noah)

## Area-weighted Series
Tmp.data[,which(i.lat>(23)|i.lat<(-23)),] = NA
A.Weight = array(rep(cos(i.lat*pi/180),each = 72),c(72,36))
Tropical.T.data = data.frame("year" = year(Tmp.time1),
                             "month" = month(Tmp.time1),
                             "Tropical.T" = areal.mean(z = Tmp.data,A.Weight = A.Weight))
Annual.Tropical.T=aggregate(Tropical.T~year,data=Tropical.T.data,mean,na.rm=TRUE)
Annual.Tropical.T$Tropical.T.10=pass.filter.NA(a=Annual.Tropical.T$Tropical.T,W=10,type="high",method = c("Butterworth"))
write.csv(Annual.Tropical.T,"Output Data/Reconstructed CGR/SD/Annual.Tropical.T.csv",row.names = F)

## Adjustment for two y axes. The coefficients are derived from linear regression
Total = merge(CGR, Annual.Tropical.T)
kk1 = lm(CGR.high~Tropical.T.10, data = Total)
Total$Tropical.T.10 = Total$Tropical.T.10*2.0415+0.1373
temp.data=pivot_longer(data=Total,cols=c("CGR.high","Tropical.T.10"))

## Plot
p4 = ggplot()+
  geom_path(data=temp.data,aes(x=year,y=value,color=name))+
  geom_vline(xintercept = c(1963,1982,1991),linetype = "longdash", col = "red",linewidth = 0.4)+
  scale_color_manual(breaks = c("CGR.high","Tropical.T.10"),labels=c("CGR","Tropical Temperature"), values = c("black",colors7[3]))+
  scale_x_continuous(sec.axis =  sec_axis(~.),breaks = seq(1960,2020,10),name = "Year")+
  scale_y_continuous(sec.axis =  sec_axis(~.*0.58, name = "Temperature (°C)"), name = "CGR (ppm/year)",
                     limits = c(-1.5,1.5),breaks = seq(-1.5,1.5,0.5))+
  coord_cartesian(xlim = c(1957,2020), ylim = c(-1,1.7), expand = F)+
  theme_figure1+
  theme(
    plot.margin = margin(0, 15, 5, 5, "pt"),
    legend.position = "inside",
    legend.justification.inside = c(1,0),
    legend.direction = "horizontal",
    axis.title.y.right = element_text(color = colors7[3]),
    axis.text.y.right = element_text(color = colors7[3]),
    axis.ticks.length.y.right = unit(4, "pt"),
    axis.ticks.y.right = element_line(color = colors7[3]),
    axis.line.x.top = element_line(linewidth = 0.3, color = "black")
  )

## Saving
p.total = plot_grid(
  p1,
  p3,
  p4,
  nrow = 3,
  align = "v",
  labels = "auto",
  rel_heights = c(1, 1, 1.2)
)
ggsave(
  plot = p.total,
  filename = "Figures/Performance/Series.tiff",
  width = 8,
  height = 7
)
ggsave(
  plot = p.total,
  filename = "Figures/Performance/Series.pdf",
  device = cairo_pdf,
  width = 8,
  height = 7
)

# Compare Multiple Filtering Methods --------------------------------------------------

## Read Data
CGR=read.table("Input Data/Carbon Dioxide/co2_gr_gl.txt",header = TRUE)
CGR = CGR[, c(1, 2, 3)]
names(CGR) = c("year", "CGR", "Raw.sd")

## De-trend
CGR = CGR %>%
  dplyr::mutate(
    CGR_detrend = detrend.linear(CGR),
    CGR_filter_10 = pass.filter.NA.mean(
      a = CGR,
      W = 10,
      type = "high",
      method = c("Butterworth")
    ),
    CGR_filter_20 = pass.filter.NA.mean(
      a = CGR,
      W = 20,
      type = "high",
      method = c("Butterworth")
    )
  )

## Plot Data
plot.data = pivot_longer(CGR, cols = c("CGR_detrend","CGR_filter_10","CGR_filter_20"))

## Plot Data
p2=ggplot()+
  geom_path(data=plot.data,
            aes(x=year,y=value,color=name))+
  scale_color_discrete(breaks = c("CGR_detrend","CGR_filter_10","CGR_filter_20"),
                       labels=c("Linear detrending","10-yr high-pass","20-yr high-pass"))+
  scale_x_continuous(breaks = seq(1950,2020,10),
                     name = "Year",
                     sec.axis =  sec_axis(~ .))+
  scale_y_continuous(
    name = "CGR (ppm/year)",
    breaks = round(seq(-1.2,1.5,0.3),1),
    sec.axis =  sec_axis( ~ .)
  ) +
  coord_cartesian(xlim = c(1955,2022), ylim = c(-1.2,1.5),expand = FALSE) +
  theme_figure1+
  theme(
    legend.position = "inside",
    legend.justification.inside = c(0.02, 0.98),
    legend.background = element_rect(fill = "white"),
    legend.title = element_blank(),
    plot.margin = margin(7, 15, 10, 7, "pt")
  )

## Saving
ggsave("Figures/Performance/Series.tiff",
       p2,
       width=7,
       height=4)
ggsave("Figures/Performance/Series.pdf",
       p2,
       device = cairo_pdf,
       width=7,
       height=4)

# End -------------------------------------------------------------

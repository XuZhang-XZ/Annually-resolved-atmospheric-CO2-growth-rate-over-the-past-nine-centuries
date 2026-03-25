

# Plot Locations of Proxies ---------------------------------------------------

## Read Data
properties2 = read.csv("Output Data/PAGES 2k v.2.0.0/Properties/properties.Obs.Select.csv")
colors7=brewer.pal(n=11,"RdYlBu")[11:1]
table(properties2$archive)

## Plot locations and correlation
world_map1 <-   ggplot() +
  geom_sf(
    data = con.boundary,
    color = "gray30",
    fill = NA,
    linewidth = 0.3
  ) +
  geom_point(data = properties2,
             size = 2.5,
             aes(
               x = Lon,
               y = Lat,
               color = Select.cor,
               shape = archive
             )) +
  scale_color_stepsn(
    colours = colors7,
    limits = c(-0.6, 0.6),
    breaks = seq(-0.6, 0.6, 0.1),
    labels = c(-0.6, " ", -0.4, " ", -0.2, " ", 0, " ", 0.2, " ", 0.4, "", 0.6),
    oob = scales::squish
  ) +
  scale_shape_manual(
    breaks = c("tree", "coral", "lake sediment", "glacier ice"),
    labels = c("tree-ring", "coral", "lake sediment", "ice core"),
    values = c(16, 15, 18, 17)
  ) +
  guides(color = "none") +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 80),expand = F)+
  scale_x_continuous(breaks = seq(-120,120,60),
                     labels = c("120°W","60°W","0°","60°E","120°E"),
                     sec.axis =  sec_axis(~ .))+
  scale_y_continuous(
    breaks = seq(-60, 60, 30),
    labels = c("-60°S", "-30°S", "0°", "30°S", "60°S"),
    sec.axis =  sec_axis( ~ .)
  ) +
  theme_figure1+
  theme(
    panel.border = element_rect(size = unit(0.4, "pt"),fill = NA, color = "black"),
    legend.position = "inside",
    legend.justification.inside = c(0.1, 0.01),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "white"),
    legend.title = element_blank(),
    plot.margin = margin(5, 15, 10, 5, "pt"),
    axis.title.x.bottom = element_blank(),
    axis.title.y.left = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.line.y.left = element_blank()
  )

## Proxy types
legend1 <-   ggplot() +
  geom_point(
    data = properties2,
    size = 2,
    aes(x = Lon, y =Lat, shape=archive)
  ) +
  scale_shape_manual(breaks = c("tree","coral","lake sediment","glacier ice"),values = c(16,15,18,17))+
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white"),
    legend.key = element_blank()
  )
legend1 = cowplot::get_plot_component(legend1, 'guide-box-top', return_all = TRUE)
legend1 = plot_grid(legend1)

## Colors
legend2 <-   ggplot() +
  geom_point(
    data = properties2,
    size = 2,
    aes(x = Lon, y =Lat, color = Select.cor)
  ) +
  scale_color_stepsn(
    colours = colors7,
    limits = c(-0.6, 0.6),
    breaks = seq(-0.6, 0.6, 0.1),
    labels = c(-0.6," ",-0.4," ",-0.2," ",0," ",0.2, " ",0.4, "",0.6),
    name = "Correlation with CGR",
    oob = scales::squish
  ) +
  theme(
    legend.position = "top",
    legend.title = element_text(angle = 90, hjust = 0.5),
    legend.title.position = "right",
    legend.background = element_rect(fill = "white"),
    legend.direction = "vertical",
    legend.key.width = unit(6, "pt"),
    legend.key.height = unit(34.5, "pt")
  )
legend2 = cowplot::get_plot_component(legend2, 'guide-box-top', return_all = TRUE)
legend2 = plot_grid(legend2)

# Plot Number of Chronologies ---------------------------------------------------

## Number of Chronologies
properties2 = read.csv("Output Data/PAGES 2k v.2.0.0/Properties/properties.Obs.Select.csv")
predictors = read.csv("Output Data/Predictors/PAGS2k.csv")
Name.Archive=unique(properties2$archive)
Number=data.frame()
for(i in 1:length(Name.Archive)){
  predictors1=predictors[,c(1,which(properties2$archive==Name.Archive[i])+1)]
  Number1=data.frame("year"=predictors$year,
                     "Num"=apply(predictors1,1,function(a) {length(which(!is.na(a)))})-1,
                     "Archive"=Name.Archive[i])
  Number=rbind(Number,Number1)
}

## Write
write_xlsx(Number,"Output Data/Source Data/Number.xlsx")

## Plot
p_Number = ggplot() +
  geom_area(data = Number, aes(x = year, y = Num, fill = Archive)) +
  scale_x_continuous(
    breaks = seq(1100, 2000, 200),
    sec.axis =  sec_axis( ~ .)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    name = "Number",
    sec.axis =  sec_axis( ~ .)
  ) +
  coord_cartesian(xlim = c(1100,2000), ylim = c(0,100),expand = FALSE) +
  scale_fill_manual(
    values = brewer.pal(n=9,"Set2")[c(1,2,3,4)],
    breaks = c("tree", "coral", "lake sediment", "glacier ice"),
    labels = c("tree-ring", "coral", "lake sediment", "ice core")
  ) +
  theme_figure1+
  theme(
    plot.margin = margin(5, 12, 10, 8, "pt"),
    axis.title.x.bottom = element_blank(),
    legend.position = "inside",
    legend.justification.inside = c(0.05, 0.95)
  )

## Proxies during 1950-2020
p11=ggplot()+
  geom_area(data=Number,
            aes(x=year,y=Num,fill=Archive))+
  geom_vline(xintercept = c(2000,2005,2010))+
  geom_hline(yintercept = c(10,20))+
  scale_x_continuous(limits = c(1900,2030),expand = c(0.0,0.0),breaks = seq(800,2000,200))+
  scale_y_continuous(limits = c(0,101),expand = c(0.0,0.01))+
  ylab("Number")+
  scale_fill_discrete()+
  P_Xu_F1 +
  theme(
    axis.title.x = element_blank(),
    plot.margin = margin(10, 15, 12, 5, "pt"),
    legend.position = c(0.3, 0.75)
  )

# Plot Series of NBP and SOI ---------------------------------------------------

## Colors
colors_p2 = c(
  rgb(0,0,255, maxColorValue = 255), ## Blue
  rgb(0,0,0, maxColorValue = 255), ## Black
  rgb(255,0,0, maxColorValue = 255), ## Red
  "#1C9E77"
)

## Read Data
Total = read.csv("Output Data/Reconstructed CGR/Plot.Total.csv",row.names = 1)

## Write
Total1 = Total[, c("year", "Pre.median", "Total.Obs", "NBP","SOI")]
write_xlsx(Total1,"Output Data/Source Data/Total1.xlsx")

## Change Direction
Total$NBP = Total$NBP * (-1)

## Compare with Land Sink
temp = Total[, c("year", "Pre.median", "Total.Obs", "NBP")]
temp.long = pivot_longer(temp, cols = c(2:NCOL(temp)))

## Compare with land sink
p_Sink = ggplot() +
  geom_ribbon(
    data = Total,
    aes(x = year, ymin = Pre.0.025, ymax = Pre.0.975),
    fill = "gray70",
    alpha = 0.5
  ) +
  geom_path(data=temp.long,linewidth = 0.5,
            aes(x=year,y=value,color=name))+
  geom_vline(xintercept = c(1963,1982,1991),linetype = "longdash", col = "red",linewidth = 0.3)+
  scale_color_manual(
    values=  colors_p2[1:4],
    breaks = c("Total.Obs","Pre.median","NBP","SOI_1"),
    labels = c("Observed CGR", "Reconstructed CGR",  "NBP", "SOI"),
    guide = guide_legend(nrow = 1)
  ) +
  scale_x_continuous(name = "Year",breaks = seq(1800,2020,20),sec.axis =  sec_axis(~ .)) +
  scale_y_continuous(name = "CGR (ppm/year)",sec.axis = sec_axis( trans=~(.)*(-1), name="NBP (ppm/year)"))+
  coord_cartesian(xlim = c(1867,2006), ylim = c(-1.1,1.5),expand = F)+
  theme_figure1+
  theme(
    plot.margin = margin(8, 12, 0, 15, "pt"),
    legend.position = "inside",
    legend.justification.inside = c(0.15, 0.99),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.background = element_blank(),
    axis.title.x.bottom = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    axis.ticks.length.x.bottom = unit(0, "pt"),
    axis.text.x.bottom = element_blank(),
    axis.line.x.bottom = element_blank(),
    axis.title.y.right = element_text(color = colors_p2[3]),
    axis.ticks.y.right = element_line(linewidth = 0.3, color = colors_p2[3]),
    axis.ticks.length.y.right = unit(4, "pt"),
    axis.text.y.right = element_text(color = colors_p2[3])
  )

## Comparison with SOI
temp=Total[,c("year","Pre.median","Total.Obs","SOI_1")]
temp.long=pivot_longer(temp,cols=c(2:4))

## Plot
p_SOI = ggplot() +
  geom_ribbon(data=Total,
              aes(x=year,ymin=Pre.0.025,ymax=Pre.0.975),fill="gray70",alpha=0.5)+
  geom_path(data=temp.long,linewidth = 0.5,
            aes(x=year,y=value,color=name))+
  geom_vline(xintercept = c(1963,1982,1991),linetype = "longdash", col = "red",linewidth = 0.3)+
  scale_color_manual(
    values=  colors_p2[1:4],
    breaks = c("Total.Obs","Pre.median","NBP","SOI_1"),
    labels = c("Observed CGR", "Reconstructed CGR",  "NBP", "SOI"),
    guide = guide_legend(nrow = 1)
  ) +
  scale_x_continuous(name = "Year",breaks = seq(1800,2020,10),sec.axis =  sec_axis(~ .)) +
  scale_y_continuous(name = "CGR (ppm/year)",breaks = c(-1,-0.5,0,0.5,1),
                     sec.axis = sec_axis( trans=~(.)/0.5, name="SOI",breaks = c(-2,-1,0,1,2)))+
  coord_cartesian(xlim = c(1867,2006), ylim = c(-1,1.5),expand = F)+
  theme_figure1+
  theme(
    plot.margin = margin(0, 12, 5, 15, "pt"),
    legend.position = "inside",
    legend.justification.inside = c(0.15, 0.99),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.background = element_blank(),
    axis.title.y.right = element_text(color = colors_p2[4]),
    axis.ticks.y.right = element_line(linewidth = 0.3, color = colors_p2[4]),
    axis.ticks.length.y.right = unit(4, "pt"),
    axis.text.y.right = element_text(color = colors_p2[4])
  )

# Saving Fig. 1 ------------------------------------------------------------------

## Top
p.top = plot_grid(world_map1,legend2,p_Number, nrow=1,rel_widths = c(6,1,3),labels = c("a","","b"))

## Middle
p.bottom = plot_grid(p_Sink,p_SOI,nrow=2, align = "v",rel_heights = c(2,2.4),labels = c("c","d"))

## Total
p.total = plot_grid(p.top,
                    p.bottom,
                    nrow = 2 ,
                    rel_heights = c(2.8, 2 + 2.4))

## Saving
ggsave(plot = p.total,
       filename = "Figures/Final Figures/Figure 1_temp.tiff",
       width=10,
       height=2.8+2+2.4)
ggsave(plot = p.total,
       filename = "Figures/Final Figures/Figure 1_temp.pdf",
       device = cairo_pdf,
       width=10,
       height=2.8+2+2.4)

# Plot Metrics ----------------------------------------------------

## Read Data
Total.summary=read.csv("Output Data/Reconstructed CGR/Ensemble Mean/Total.summary.csv")

## Plot
p1=ggplot()+
  geom_ribbon(data=Total.summary,
              aes(x=year,ymin=CRE.0.025,ymax=CRE.0.975),fill="gray",alpha=0.7)+
  geom_path(data=Total.summary,
            aes(x=year,y=CRE.median),color="red")+
  scale_x_continuous(breaks = seq(1100,2000,100))+
  coord_cartesian(xlim = c(1100,2005),ylim=c(-0.1,1),expand = F)+
  ylab("RE (Calibration)")+
  theme_bw()+
  theme(
    plot.margin = margin(7, 15, 7, 7, "pt")
  )

p2=ggplot()+
  geom_ribbon(data=Total.summary,
              aes(x=year,ymin=Cr.0.025,ymax=Cr.0.975),fill="gray",alpha=0.7)+
  geom_path(data=Total.summary,
            aes(x=year,y=Cr.median),color="red")+
  scale_x_continuous(breaks = seq(1100,2000,100))+
  coord_cartesian(xlim = c(1100,2005),ylim=c(-0.1,1),expand = F)+
  ylab("r (Calibration)")+
  theme_bw()+
  theme(
    plot.margin = margin(7, 15, 7, 7, "pt")
  )

p3=ggplot()+
  geom_ribbon(data=Total.summary,
              aes(x=year,ymin=VCE.0.025,ymax=VCE.0.975),fill="gray",alpha=0.7)+
  geom_path(data=Total.summary,
            aes(x=year,y=VCE.median),color="red")+
  scale_x_continuous(breaks = seq(1100,2000,100))+
  coord_cartesian(xlim = c(1100,2005),ylim=c(-0.1,1),expand = F)+
  ylab("RE (Validation)")+
  theme_bw()+
  theme(
    plot.margin = margin(7, 15, 7, 7, "pt")
  )

p4=ggplot()+
  geom_ribbon(data=Total.summary,
              aes(x=year,ymin=Vr.0.025,ymax=Vr.0.975),fill="gray",alpha=0.7)+
  geom_path(data=Total.summary,
            aes(x=year,y=Vr.median),color="red")+
  scale_x_continuous(breaks = seq(1100,2000,100))+
  coord_cartesian(xlim = c(1100,2005),ylim=c(-0.1,1),expand = F)+
  ylab("r (Validation)")+
  theme_bw()+
  theme(
    plot.margin = margin(7, 15, 7, 7, "pt")
  )

## Saving
p.total=plot_grid(p1,p2,p3,p4,
                  labels = c("auto"),
                  nrow=2)
ggsave("Figures/Performance/Metrics.tiff",
       p.total,
       width=8,
       height=6)


# 500 vs 1000 ensemble members -----------------------------------------------------

## Read Data
All.ensemble=read.csv("Output Data/Reconstructed CGR/Ensemble Mean/All.ensemble.csv")
Total.summary=read.csv("Output Data/Reconstructed CGR/Ensemble Mean/Total.summary.csv")
colors3 = brewer.pal(n=7,"Set1")

## Ensemble Median
Ensemble.median=data.frame("year"=Total.summary$year,
                           "Median.500"=apply(All.ensemble[,1:500],1,median,na.rm=TRUE),
                           "Median.1000"=apply(All.ensemble[,1:1000],1,median,na.rm=TRUE))
Ensemble.median1=pivot_longer(Ensemble.median,cols=c(2:3))
Ensemble.median1$name=factor(Ensemble.median1$name,levels=c("Median.500","Median.1000"))

## Plot
p1=ggplot()+
  geom_path(data=Ensemble.median1,
            aes(x=year,y=value,color=name))+
  scale_color_manual(
    values=colors3[c(1,2)],
    breaks = c("Median.500","Median.1000"),
    labels = c("500 members","1000 members")
  ) +
  scale_x_continuous(limits = c(1100,2010),breaks = seq(1100,2000,100),expand = c(0,0))+
  scale_y_continuous(limits = c(-1.5,1.5),expand = c(0,0),name = "CGR (ppm/year)")+ ## expression(CGR[A]*" (ppm/year)")
  theme_bw()+
  theme(
    plot.margin = margin(7, 15, 7, 7, "pt"),
    legend.position = "inside",
    legend.position.inside = c(0.3, 0.92),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.background = element_blank()
  )

p2=ggplot()+
  geom_path(data=Ensemble.median,
            aes(x=year,y=Median.1000-Median.500))+
  scale_x_continuous(limits = c(1100,2010),breaks = seq(1100,2000,100),expand = c(0,0))+
  scale_y_continuous(limits = c(-1,1),expand = c(0,0),name = "Difference in CGR (ppm/year)")+ ## expression(CGR[A]*" (ppm/year)")
  theme_bw()+
  theme(
    plot.margin = margin(7, 15, 7, 7, "pt"),
  )

## Calculate width of 95% Range
range=function(x) {
  a1=quantile(x,probs = 0.025,na.rm=TRUE)
  a2=quantile(x,probs = 0.975,na.rm=TRUE)
  return(a2-a1)
}
Ensemble.median=data.frame("year"=Total.summary$year,
                           "Median.500"=apply(All.ensemble[,1:500],1,range),
                           "Median.1000"=apply(All.ensemble[,1:1000],1,range))
Ensemble.median1=pivot_longer(Ensemble.median,cols=c(2:3))
Ensemble.median1$name=factor(Ensemble.median1$name,levels=c("Median.500","Median.1000"))

## Plot
p3=ggplot()+
  geom_path(data=Ensemble.median1,
            aes(x=year,y=value,color=name))+
  scale_color_manual(
    values=colors3[c(1,2)],
    breaks = c("Median.500","Median.1000"),
    labels = c("500 members","1000 members")
  ) +
  scale_x_continuous(limits = c(1100,2010),breaks = seq(1100,2000,100),expand = c(0,0))+
  scale_y_continuous(limits = c(0.7,1.5),expand = c(0,0),name = "95% width (ppm/year)")+ ## expression(CGR[A]*" (ppm/year)")
  theme_bw()+
  theme(
    plot.margin = margin(7, 15, 7, 7, "pt"),
    legend.position = "inside",
    legend.position.inside = c(0.7, 0.92),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.background = element_blank()
  )

p4=ggplot()+
  geom_path(data=Ensemble.median,
            aes(x=year,y=Median.1000-Median.500))+
  scale_x_continuous(limits = c(1100,2010),breaks = seq(1100,2000,100),expand = c(0,0))+
  scale_y_continuous(limits = c(-1,1),expand = c(0,0),name = "Difference in 95% width (ppm/year)")+ ## expression(CGR[A]*" (ppm/year)")
  theme_bw()+
  theme(
    plot.margin = margin(7, 15, 7, 7, "pt"),
  )

## Saving
p.total=plot_grid(p1,p2,p3,p4,
                  labels = c("auto"),
                  nrow=2)
ggsave("Figures/Performance/Number of ensemble.tiff",
       p.total,
       width=10,
       height=7)

# End -------------------------------------------------------------

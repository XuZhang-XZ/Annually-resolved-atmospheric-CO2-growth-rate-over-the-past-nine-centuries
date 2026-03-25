

# Correlations --------------------------------------------------------

## Read Data
mesh.Cor = read.csv("Output Data/Figures/Cor.PDSI/mesh.Cor_3.csv", row.names = 1)
color.Cor <- brewer.pal(n = 11, "RdYlBu")[11:1]

## Selection based on P - value
i.lon1 = seq(-179.75,179.75,2.5)
i.lat1 = seq(-89.75,89.75,2.5)
mesh.Cor.p = mesh.Cor[mesh.Cor$lon%in%i.lon1&mesh.Cor$lat%in%i.lat1,]

## Plot Correlation for Pre
p11 = ggplot() + 
  geom_raster(data = mesh.Cor,
              aes(x = lon, y = lat, fill = Cor.Pre),
              interpolate = FALSE)  +
  geom_point(
    data = mesh.Cor.p %>% dplyr::filter(Cor.Pre.p < 0.05),
    aes(x = lon, y = lat),
    shape = 16,
    size = 0.3,
    alpha = 1 ,
    color = rgb(0, 0, 0, maxColorValue = 255)
  )  +
  geom_polygon(
    data = con.boundary1,
    color = "gray50",
    fill = NA,
    linewidth = 0.1,
    aes(x = long, y = lat, group = group)
  ) +
  scale_fill_stepsn(
    colours = color.Cor,
    limits = c(-0.6, 0.6),
    breaks = seq(-0.6, 0.6, 0.1),
    labels = c(-0.6," ",-0.4," ",-0.2," ",0," ",0.2, " ",0.4, "",0.6),
    na.value = NA
  ) +
  scale_x_continuous(limits = c(-180.3,180.3),expand = c(0,0),
                     breaks = seq(-180,180,90),labels = c("180°W","90°W","0","90°E","180°E"),
                     sec.axis =  sec_axis(~ .))+
  scale_y_continuous(limits = c(-61,85),expand = c(0,0),
                     breaks = c(-60,0,60),labels = c("60°S","0","60°N"),
                     sec.axis =  sec_axis(~ .))+
  coord_quickmap(ylim = c(-60,90),xlim = c(-180,180)) +
  theme_figure1+
  theme(
    plot.margin = margin(5, 5, 5, 5, "pt"),
    legend.position = "none",
    legend.position.inside = c(1.01,1.03),
    legend.justification.inside = c(0,1),
    legend.key.width = unit(4.8,"pt"),
    legend.key.height = unit(32,"pt"),
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.background = element_blank(),
    axis.ticks.length.x.bottom = unit(-2,"pt"),
    axis.ticks.length.y.left = unit(-2,"pt"),
    axis.text.x.bottom = element_blank(),
    axis.text.y.left = element_blank(),
    axis.title.x.bottom = element_blank(),
    axis.title.y.left = element_blank()
  )

## Plot Correlation for Tmp
p12 = ggplot() + 
  geom_raster(data = mesh.Cor,
              aes(x = lon, y = lat, fill = Cor.Tmp),
              interpolate = FALSE)  +
  geom_point(
    data = mesh.Cor.p %>% dplyr::filter(Cor.Tmp.p < 0.05),
    aes(x = lon, y = lat),
    shape = 16,
    size = 0.3,
    alpha = 1 ,
    color = rgb(0, 0, 0, maxColorValue = 255)
  )  +
  geom_polygon(
    data = con.boundary1,
    color = "gray50",
    fill = NA,
    linewidth = 0.1,
    aes(x = long, y = lat, group = group)
  ) +
  scale_fill_stepsn(
    colours = color.Cor,
    limits = c(-0.6, 0.6),
    breaks = seq(-0.6, 0.6, 0.1),
    labels = c(-0.6," ",-0.4," ",-0.2," ",0," ",0.2, " ",0.4, "",0.6),
    na.value = NA
  ) +
  scale_x_continuous(limits = c(-180.3,180.3),expand = c(0,0),
                     breaks = seq(-180,180,90),labels = c("180°W","90°W","0","90°E","180°E"),
                     sec.axis =  sec_axis(~ .))+
  scale_y_continuous(limits = c(-61,85),expand = c(0,0),
                     breaks = c(-60,0,60),labels = c("60°S","0","60°N"),
                     sec.axis =  sec_axis(~ .))+
  coord_quickmap(ylim = c(-60,90),xlim = c(-180,180)) +
  theme_figure1+
  theme(
    plot.margin = margin(5, 5, 5, 5, "pt"),
    legend.position = "none",
    legend.position.inside = c(1.01,1.03),
    legend.justification.inside = c(0,1),
    legend.key.width = unit(4.8,"pt"),
    legend.key.height = unit(32,"pt"),
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.background = element_blank(),
    axis.ticks.length.x.bottom = unit(-2,"pt"),
    axis.ticks.length.y.left = unit(-2,"pt"),
    axis.text.x.bottom = element_blank(),
    axis.text.y.left = element_blank(),
    axis.title.x.bottom = element_blank(),
    axis.title.y.left = element_blank()
  )

## Plot Correlation for PDSI
p13 = ggplot() + 
  geom_raster(data = mesh.Cor,
              aes(x = lon, y = lat, fill = Cor.TWS),
              interpolate = FALSE)  +
  geom_point(
    data = mesh.Cor.p %>% dplyr::filter(Cor.TWS.p < 0.05),
    aes(x = lon, y = lat),
    shape = 16,
    size = 0.3,
    alpha = 1 ,
    color = rgb(0, 0, 0, maxColorValue = 255)
  )  +
  geom_polygon(
    data = con.boundary1,
    color = "gray50",
    fill = NA,
    linewidth = 0.1,
    aes(x = long, y = lat, group = group)
  ) +
  scale_fill_stepsn(
    colours = color.Cor,
    limits = c(-0.6, 0.6),
    breaks = seq(-0.6, 0.6, 0.1),
    labels = c(-0.6," ",-0.4," ",-0.2," ",0," ",0.2, " ",0.4, "",0.6),
    na.value = NA
  ) +
  scale_x_continuous(limits = c(-180.3,180.3),expand = c(0,0),
                     breaks = seq(-180,180,90),labels = c("180°W","90°W","0","90°E","180°E"),
                     sec.axis =  sec_axis(~ .))+
  scale_y_continuous(limits = c(-61,85),expand = c(0,0),
                     breaks = c(-60,0,60),labels = c("60°S","0","60°N"),
                     sec.axis =  sec_axis(~ .))+
  coord_quickmap(ylim = c(-60,90),xlim = c(-180,180)) +
  theme_figure1+
  theme(
    plot.margin = margin(5, 5, 5, 5, "pt"),
    legend.position = "none",
    legend.position.inside = c(1.01,1.03),
    legend.justification.inside = c(0,1),
    legend.key.width = unit(4.8,"pt"),
    legend.key.height = unit(32,"pt"),
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.background = element_blank(),
    axis.ticks.length.x.bottom = unit(-2,"pt"),
    axis.ticks.length.y.left = unit(-2,"pt"),
    axis.text.x.bottom = element_blank(),
    axis.text.y.left = element_blank(),
    axis.title.x.bottom = element_blank(),
    axis.title.y.left = element_blank()
  )

## Legend
legend1 <-   ggplot() +
  geom_raster(data = mesh.Cor,
              aes(x = lon, y = lat, fill = Cor.TWS),
              interpolate = FALSE)  +
  geom_point(
    data = mesh.Cor.p %>% dplyr::filter(Cor.TWS.p < 0.05),
    aes(x = lon, y = lat),
    shape = 16,
    size = 0.3,
    alpha = 1 ,
    color = rgb(0, 0, 0, maxColorValue = 255)
  )  +
  scale_fill_stepsn(
    colours = color.Cor,
    limits = c(-0.6, 0.6),
    breaks = seq(-0.6, 0.6, 0.1),
    labels = round(seq(-0.6, 0.6, 0.1),1),
    name = "Correlation with CGR",
    na.value = NA
  ) +
  theme(
    plot.margin = margin(2, 2, 2, 2, "pt"),
    legend.position = "top",
    legend.key.width = unit(66,"pt"),
    legend.key.height = unit(5,"pt"),
    legend.direction = "horizontal",
    legend.title = element_text(hjust = 0.5,size = 10),
    legend.title.position = "bottom",
    legend.background = element_blank()
  )
legend_Cor = plot_grid(cowplot::get_plot_component(legend1, 'guide-box-top', return_all = TRUE))

## Total
p_temp = plot_grid(p11,
                   p12,
                   p13,
                   legend_Cor,
                   labels = c("a","b","c"," "),
                   ncol = 1,
                   rel_heights = c(2.1, 2.1, 2.1, 0.5))

# Plot Cor with land covers ---------------------------------------

## Read Data
PDSI.cor = read.csv("Output Data/Figures/Cor.PDSI/PDSI.cor.csv",row.names = 1)
PDSI.cor = PDSI.cor %>% dplyr::filter(Variable == "PDSI")

## Write Source Data
PDSI.cor_S = PDSI.cor[, c("Type", "LC.cover.name", "Cor","p.Cor")]
write_xlsx(PDSI.cor_S,"Output Data/Source Data/PDSI.cor_S.xlsx")

## Land Cover Names
New.LC.name = c(
  "Tropical forests",
  "Tropical shrublands",
  "Tropical savannas",
  "Tropical grasslands",
  "Extra-tropical forests",
  "Extra-tropical shrublands",
  "Extra-tropical savannas",
  "Extra-tropical grasslands"
)

## Adjustment for ploting P-values
PDSI.cor$Cor1 = PDSI.cor$Cor
PDSI.cor$Cor1[PDSI.cor$Cor>0] = PDSI.cor$Cor[PDSI.cor$Cor>0]+0.018
PDSI.cor$Cor1[PDSI.cor$Cor<=0] = PDSI.cor$Cor[PDSI.cor$Cor<=0]-0.018
PDSI.cor$Cor1[PDSI.cor$p.Cor>=0.05] = 10

## Data for plotting Text
PDSI.text = PDSI.cor[PDSI.cor$Type == "Reconstructed CGR (1901-2006)",]

## Add Top two blank rows
PDSI.cor2 = data.frame("Type" = "Reconstructed CGR (1901-2006)",
                       "Variable" = "PDSI",
                       "Cor" = 0,
                       "p.Cor" = 2,
                       "LC.cover" = c(998),
                       "LC.cover.name" = c(998),
                       "Cor1" = 2)
PDSI.cor3 = rbind(PDSI.cor,PDSI.cor2)
PDSI.cor3$LC.cover.name = factor(PDSI.cor3$LC.cover.name, levels = c(New.LC.name,998))

## Plot
Typess = c("Reconstructed CGR (1901-2006)","Reconstructed CGR (1901-1958)","Reconstructed CGR (1959-2006)","Observed CGR (1959-2006)")
PDSI.cor3 = PDSI.cor3[which(PDSI.cor3$Type%in%Typess),]
PDSI.cor3$Type = factor(PDSI.cor3$Type, levels = Typess)
colors_P21=brewer.pal(n=8,"Set2")[c(1:4)]
labels_Xu = c("Reconstructed CGR (1901-2006)","Reconstructed CGR (1901-1958)","Reconstructed CGR (1959-2006)","Observed CGR (1959-2006)")

## Plot
p_Land = ggplot()+
  geom_col(data = PDSI.cor3,
           aes(x = Cor,y = LC.cover.name,fill = Type),
           width = 0.45,color = NA,
           position = position_dodge(width = 0.47,preserve = "single"))+
  geom_point(data = PDSI.cor3,
             shape = 8,show.legend = F,size = 1,
             aes(x = Cor1,y = LC.cover.name,color = Type),
             position = position_dodge(width = 0.47,preserve = "total"))+
  geom_text(data = PDSI.text,
            size = 3.3,
            hjust = 0,
            aes(x = 0.2,y = LC.cover.name,label = LC.cover.name))+
  geom_path(data = data.frame("x" = c(0, 0), "y" = c(0.4, 8.4)),
            aes(x = x, y = y),
            linewidth = unit(0.3, "pt")) +
  geom_hline(yintercept = length(New.LC.name) + 0.4, linewidth = unit(0.3, "pt")) +
  scale_fill_manual(breaks = Typess,
                    labels = labels_Xu,
                    values = colors_P21[c(1:4)],
                    guide = guide_legend(ncol = 1))+
  scale_color_manual(breaks = Typess,
                     labels = labels_Xu,
                     values = colors_P21[c(1:4)])+
  scale_x_continuous(limits = c(-0.8,0.7),
                     expand = c(0,0),
                     breaks = seq(-1,1,0.25),
                     name = expression("Correlation between scPDSI and CGR"))+
  scale_y_discrete(breaks = c(New.LC.name,998),
                   labels = c(New.LC.name,NA))+
  P_Xu_F1 +
  theme(plot.margin = margin(5, 13, 10, 4, "pt"),
        legend.position = "inside",
        legend.justification.inside = c(0.05,0.99),
        legend.key.size = unit(15,"pt"),
        axis.ticks.length.y = unit(-2,"pt"),
        axis.title.x = element_text(vjust = -0.5, size = 10),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

# Saving ----------------------------------------------------------

## Total
p.total = plot_grid(
  p_temp,
  p_Land,
  labels = c("", "d"),
  nrow = 1,
  rel_widths = c(5, 3.7)
)

## Saving
ggsave(
  plot = p.total,
  filename = "Figures/Final Figures/Figure 3_temp.tiff",
  width = 5 + 3.7,
  height = 2.1 * 3 + 0.5
)
ggsave(
  plot = p.total,
  filename = "Figures/Final Figures/Figure 3_temp.pdf",
  device = cairo_pdf,
  width = 5 + 3.7,
  height = 2.1 * 3 + 0.5
)

# End ---------------------------------------------------------------------



# PDF of CGR SD ---------------------------------------------------

## Read Data
Total.summary.sd = read.csv("Output Data/Reconstructed CGR/SD/Total.summary.sd.csv")
Var_Rec_1985 = Total.summary.sd$HR.SD.median[which(Total.summary.sd$year == 1985)]
Var_Obs_1991 = Total.summary.sd$Obs.CGR.Var[which(Total.summary.sd$year == 1991)] ## 1976-2006

## Historical Data
Total.summary.sd = Total.summary.sd %>% dplyr::filter(year %in% c(1100:1985))

## Exceedance Probability
Ex.Number_1 = length(which(Total.summary.sd$HR.SD.median > Var_Rec_1985))
Ex.Number_2 = length(which(Total.summary.sd$HR.SD.median > Var_Obs_1991))
Ex.Number_1 / NROW(Total.summary.sd)
Ex.Number_2 / NROW(Total.summary.sd)

## Plot
p1 = ggplot()+
  geom_histogram(
    data = Total.summary.sd,
    binwidth = 0.005,
    color = "#e9ecef",
    fill = "#69b3a2",
    alpha = 0.6,
    position = 'identity',
    aes(x = HR.SD.median)
  ) +
  geom_vline(xintercept = Var_Obs_1991, color = "blue")+
  annotate("text", x = Inf, y = Inf,hjust = 2, vjust = 2, label = "0%")+
  scale_x_continuous(limits = c(0,0.3),breaks = c(0,0.1,0.2,0.3),expand = c(0,0), name = "CGR Variance")+
  scale_y_continuous(limits = c(0,120),expand = c(0,0), name = "Count")+
  theme_figure1+
  theme(
    plot.margin = margin(10, 15, 10, 10, "pt"),
    axis.ticks = element_line(linewidth = 0.3)
  )

## Saving
ggsave(plot = p1,
       filename = "Figures/Performance/Exceedance Probability.tiff",
       width=7,
       height= 5)

# End -------------------------------------------------------------

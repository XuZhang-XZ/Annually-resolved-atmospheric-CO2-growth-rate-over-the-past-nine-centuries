

# Plot Correlation --------------------------------------------------------

## Data
CMIP_Summary = read.csv("Z:/2022/Science9_SM/Output Data/PMIIP_past1000/past1000_summary/CMIP_Summary.csv",row.names = 1)
colors7=brewer.pal(n=9,"Set1")

## Select
CMIP_Summary1 =  CMIP_Summary %>% 
  dplyr::filter(!model %in% "CESM1")

## Correlation Summary
CMIP_Summary_Sel =  CMIP_Summary %>% 
  dplyr::filter(!model %in% "CESM1") %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(across(!c(model,varivant), list(
    mean = ~ mean(.x, na.rm = TRUE),
    median = ~ median(.x, na.rm = TRUE),
    upper_95 = ~ quantile(.x, probs = 0.975, na.rm = TRUE),
    lower_95 = ~ quantile(.x, probs = 0.025, na.rm = TRUE)
  )))

## Test
plot(CMIP_Summary_Sel$year,CMIP_Summary_Sel$RE_pre_mean,type = "l",ylim = c(0,1))
plot(CMIP_Summary_Sel$year,CMIP_Summary_Sel$CE_pre_mean,type = "l",ylim = c(0,1))

## Plot Correlation for CESM
p1 = ggplot()+
  geom_path(data = CMIP_Summary %>% dplyr::filter(model %in% "CESM1"),
            color = "gray50",
            aes(x = year, y = Cor)) +
  geom_hline(yintercept = 0.769, color = "blue", linetype = "dashed", linewidth = 0.8) +
  scale_x_continuous(
    name = "Year",
    limits = c(1100, 2000),
    breaks = seq(1000, 2000, 100),
    expand = c(0, 0),
    sec.axis =  sec_axis( ~ .)
  ) +
  scale_y_continuous(
    name = "Correlation",
    limits = c(0, 1),
    expand = c(0, 0),
    sec.axis =  sec_axis( ~ .)
  ) +
  theme_figure1+
  theme(
    plot.margin = margin(7, 15, 7, 7, "pt"),
    legend.position = "none"
  )
  
## Saving
ggsave(plot = p1,
       filename = "Figures/CMIP6/CESM Cor.tiff",
       width=8,
       height= 5)


## Plot Correlation Summary
p1 = ggplot()+
  geom_path(data = CMIP_Summary1,color = "gray60",
            aes(x = year, y = Cor, group = model)) +
  geom_path(data = CMIP_Summary_Sel,color = colors7[1],
            aes(x = year, y = Cor_median)) +
  geom_ribbon(data = CMIP_Summary_Sel,color = NA,fill = colors7[1], alpha = 0.2,
              aes(x = year, ymin = Cor_lower_95, ymax = Cor_upper_95)) +
  scale_x_continuous(
    name = "Year",
    breaks = seq(1100, 2000, 100),
    sec.axis =  sec_axis( ~ .)
  ) +
  scale_y_continuous(
    name = "Correlation",
    sec.axis =  sec_axis( ~ .)
  ) +
  coord_cartesian(xlim = c(1100,2000),ylim = c(0,1),expand = F)+
  theme_figure1+
  theme(
    plot.margin = margin(7, 15, 7, 7, "pt"),
    legend.position = "none"
  )

## Plot Sensitivity
p2 = ggplot()+
  geom_path(data = CMIP_Summary1,color = "gray60",
            aes(x = year, y = Sen, group = model)) +
  geom_path(data = CMIP_Summary_Sel,color = colors7[1],
            aes(x = year, y = Sen_median)) +
  geom_ribbon(data = CMIP_Summary_Sel,color = NA,fill = colors7[1], alpha = 0.2,
              aes(x = year, ymin = Sen_lower_95, ymax = Sen_upper_95)) +
  scale_x_continuous(
    name = "Year",
    breaks = seq(1000, 2000, 100),
    sec.axis =  sec_axis( ~ .)
  ) +
  scale_y_continuous(
    name = "Sensitivity (ppm/year/°C)",
    sec.axis =  sec_axis( ~ .)
  ) +
  coord_cartesian(xlim = c(1100,2000),ylim = c(-6,2),expand = F)+
  theme_figure1+
  theme(
    plot.margin = margin(7, 15, 7, 7, "pt"),
    legend.position = "none"
  )

## Plot prediction
p3 = ggplot()+
  geom_path(data = CMIP_Summary_Sel,color = colors7[1],
            aes(x = year, y = Cor_pre_median)) +
  geom_ribbon(data = CMIP_Summary_Sel,color = NA,fill = colors7[1], alpha = 0.2,
              aes(x = year, ymin = Cor_pre_lower_95, ymax = Cor_pre_upper_95)) +
  scale_x_continuous(
    name = "Year",
    breaks = seq(1000, 2000, 100),
    sec.axis =  sec_axis( ~ .)
  ) +
  scale_y_continuous(
    name = "Correlation",
    sec.axis =  sec_axis( ~ .)
  ) +
  coord_cartesian(xlim = c(1100,2000),ylim = c(0,1),expand = F)+
  theme_figure1+
  theme(
    plot.margin = margin(7, 15, 7, 7, "pt"),
    legend.position = "none"
  )

## Total
p.total = plot_grid(p1, p2, p3, align = "v", nrow = 3, labels = "auto")

## Saving
ggsave(plot = p.total,
       filename = "Figures/CMIP6/Correlation stationarity.tiff",
       width=8,
       height= 8)


# Compare SD --------------------------------------------------------------

## Data
CMIP_Summary = read.csv("Z:/2022/Science9_SM/Output Data/PMIIP_past1000/past1000_summary/CMIP_Summary.csv",row.names = 1)
colors7=brewer.pal(n=9,"Set1")

## Correlation Summary
CMIP_Summary_Sel =  CMIP_Summary %>% 
  dplyr::filter(!model %in% "CESM1") %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(across(!c(model,varivant), list(
    mean = ~ mean(.x, na.rm = TRUE),
    median = ~ median(.x, na.rm = TRUE),
    upper_95 = ~ quantile(.x, probs = 0.975, na.rm = TRUE),
    lower_95 = ~ quantile(.x, probs = 0.025, na.rm = TRUE)
  )))

## Data
Total.summary.sd = read.csv("Output Data/Reconstructed CGR/SD/Total.summary.sd.csv")
colors_temp2 = brewer.pal(n=9,"Set1")

## Add
CMIP_Summary_SD = CMIP_Summary_Sel %>%
  dplyr::mutate("Type" = "Simulation") %>%
  dplyr::add_row(Type = "Reconstruction",year = Total.summary.sd$year,Var_nbp_median = Total.summary.sd$HR.SD.median)

## Plot Variance
p3 = ggplot()+
  geom_path(data = CMIP_Summary_SD, 
            aes(x = year, y = Var_nbp_median, color = Type)) +
  # geom_vline(xintercept = c(1220,1290,1760,1840)) +
  geom_ribbon(
    data = Total.summary.sd,
    color = NA,
    fill = brewer.pal(n = 9, "Set1")[2],
    alpha = 0.2,
    aes(x = year, ymin = HR.SD.0.025, ymax = HR.SD.0.975)
  ) +
  geom_ribbon(
    data = CMIP_Summary_Sel,
    color = NA,
    fill = brewer.pal(n = 9, "Set1")[1],
    alpha = 0.2,
    aes(x = year, ymin = Var_nbp_lower_95, ymax = Var_nbp_upper_95)
  ) +
  scale_color_manual(
    breaks = c("Reconstruction", "Simulation"),
    labels = c("CGR reconstruction", "NBP simulation"),
    values = brewer.pal(n = 9, "Set1")[c(2,1)]
  ) +
  scale_x_continuous(
    name = "Year",
    breaks = seq(1100, 2000, 100),
    sec.axis =  sec_axis( ~ .)
  ) +
  scale_y_continuous(
    name = "CGR and NBP Variance",
    sec.axis =  sec_axis( ~ .)
  ) +
  coord_cartesian(xlim = c(1100,2000),ylim = c(0,0.4),expand = F)+
  theme_figure1+
  theme(
    plot.margin = margin(7, 15, 7, 7, "pt"),
    legend.position = "inside",
    legend.justification.inside = c(0.6,0.99),
    legend.background = element_rect(fill = "white")
  )

## Saving
ggsave(
  plot = p3,
  filename = "Figures/CMIP6/Compare CGR Variance.tiff",
  width = 8,
  height = 5
)


# Compare NBP SD in individual models -------------------------------------

## Data
CMIP_Summary = read.csv("Z:/2022/Science9_SM/Output Data/PMIIP_past1000/past1000_summary/CMIP_Summary.csv",row.names = 1)
colors7=brewer.pal(n=9,"Set1")

## Correlation Summary
CMIP_Summary_Sel =  CMIP_Summary %>% 
  dplyr::filter(!model %in% "CESM1")

## Plot Variance
p3 = ggplot()+
  geom_path(data = CMIP_Summary_Sel, 
            aes(x = year, y = Var_nbp, color = model)) +
  scale_x_continuous(
    name = "Year",
    breaks = seq(1100, 2000, 100),
    sec.axis =  sec_axis( ~ .)
  ) +
  scale_y_continuous(
    name = "CGR and NBP Variance",
    sec.axis =  sec_axis( ~ .)
  ) +
  coord_cartesian(xlim = c(1100,2000),ylim = c(0,0.4),expand = F)+
  theme_figure1+
  theme(
    plot.margin = margin(7, 15, 7, 7, "pt"),
    legend.position = "inside",
    legend.justification.inside = c(0.6,0.8),
    legend.background = element_rect(fill = "white")
  )

# Compare Variance of NBP variance in models ----------------------------------------------------

## Models
Models_NBP_Var = read.csv("Z:/2022/Science9_SM/Output Data/PMIIP_past1000/past1000_summary/Models_NBP_Var.csv",row.names = 1)

## Name
Models_NBP_Var$model[Models_NBP_Var$model == "CESM1"] = "CESM-LME"

## Data
Total.summary.sd = read.csv("Output Data/Reconstructed CGR/SD/Total.summary.sd.csv")
Total.summary.sd1 = subset(Total.summary.sd,year %in% c(1959:2000))
a2.mean = mean(Total.summary.sd1$Total.Obs)
a2.var = var(Total.summary.sd1$Total.Obs)

## Combine
Models_NBP_Var = Models_NBP_Var %>%
  add_row(model = "Observation", varivant = NA, NBP_var = a2.var) %>%
  dplyr::mutate(Model_name = paste0(model,"_",varivant))

## Name
Models_NBP_Var$Model_name[Models_NBP_Var$model != "CESM-LME"] = Models_NBP_Var$model[Models_NBP_Var$model != "CESM-LME"]
Models_NBP_Var$Model_name = factor(
  Models_NBP_Var$Model_name,
  levels = c(
    paste0("CESM-LME_", 1:13),
    "IPSL-CM5A-LR",
    "MIROC-ESM",
    "MPI-ESM-P",
    "INM-CM4-8",
    "MIROC-ES2L",
    "Observation"
  )
)

## Plot
p1 = ggplot() +
  geom_col(data = Models_NBP_Var,fill="#69b3a2", color="#e9ecef",width = 0.7,
           aes(x = Model_name, y = NBP_var)) +
  scale_y_continuous(breaks = seq(0,1,0.1),expand = c(0,0),name = "NBP variance")+
  coord_cartesian(ylim = c(0,0.5))+
  theme_figure1+
  theme(
    legend.position = "inside",
    legend.justification = c(0.04,1),
    legend.direction = "vertical",
    plot.margin = margin(t = 10, r = 10, b = 5, l = 12, unit = "pt"),
    axis.title.x.bottom = element_blank(),
    axis.text.x.bottom = element_text(color = "black",size = 9,angle = 45,hjust = 1),
    axis.text.y.left = element_text(color = "black",size = 8)
  )

## Saving
ggsave(
  plot = p1,
  filename = "Figures/CMIP6/CGR Variance_Models.tiff",
  width = 8,
  height = 6
)

# End ---------------------------------------------------------------------



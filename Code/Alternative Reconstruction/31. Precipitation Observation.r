
# Read Precipitation Observation ----------------------------------------------------

## Library
library(precintcon)

## Read Precipitation Data
Pre.data = readLines("Z:/2025/Input Data/CRU TS v4.08 Observation/pre.2406262226.clean.dtb")
Read.n = which(nchar(Pre.data) == 70)

## Data Format
All_pre = data.frame()
All_spi = data.frame()
Station.S = data.frame()
i = 1
j = 3

## Read Individual Observations
for(i in Read.n) {
  
  ## Data
  Pre.read = Pre.data[i]
  WMO.code = substr(Pre.read, 1, 7)
  WMO.lat = as.numeric(substr(Pre.read, 9, 13)) / 100
  WMO.lon = as.numeric(substr(Pre.read, 15, 20)) / 100
  WMO.alt = substr(Pre.read, 22, 25)
  WMO.name = substr(Pre.read, 27, 46)
  WMO.country = substr(Pre.read, 48, 60)
  WMO.Start = as.numeric(substr(Pre.read, 62, 65))
  WMO.End = as.numeric(substr(Pre.read, 67, 70))
  n.year = WMO.End - WMO.Start
  
  ## Select Time Periods
  if(WMO.Start > 1911 | WMO.End < 1990) {
    next
  }

  ## Read Data
  R.data = data.frame()
  for (j in (i + 2):(i + 2 + n.year - 1)) {
    Pre.read = Pre.data[j]
    R.year = substr(Pre.read,1,4)
    R.value = sapply(1:12,function(x) {substr(Pre.read,4+x*5-4,4+x*5)})
    R.data_1 = data.frame("year" = R.year,
                          "month" = 1:12,
                          "precipitation" = R.value)
    R.data = rbind(R.data,R.data_1)
  }
  
  ## Summary
  R.data$precipitation[which(R.data$precipitation == "-9999")] = NA
  
  ## Unit
  R.data = R.data %>%
    dplyr::mutate(year = as.numeric(year),
                  precipitation = as.numeric(precipitation) / 10)

  ## To SPI
  pre.data = as.precintcon.monthly(R.data)
  R_spi = spi(pre.data,period = 6)
  
  ## Format
  R_pre = as_tibble(R.data)
  R_spi = as_tibble(R_spi)
  
  ## Combine
  R_pre$Code = WMO.code
  R_spi$Code = WMO.code
  
  ## Missing
  n.missing = length(which(is.na(R_spi$spi)))
  All.value = NROW(R_spi)
  if(n.missing > 0.05*All.value) {
    next
  }
  
  ## Station Summary
  Station.S_1 = data.frame("Code" = WMO.code,
                           "Read.n" = i,
                           "Country" = WMO.country,
                           "Name" = WMO.name,
                           "Lon" = WMO.lon,
                           "Lat" = WMO.lat,
                           "Alt" = WMO.alt,
                           "Start" = WMO.Start,
                           "End" = WMO.End)
  Station.S = rbind(Station.S,Station.S_1)
  
  ## Combine
  All_pre = All_pre %>%
    dplyr::bind_rows(R_pre)
  All_spi = All_spi %>%
    dplyr::bind_rows(R_spi)
  
  ## Print
  print(i)
  
}

## Write
write.csv(Station.S,"Z:/2022/Science9_SM/Output Data/Observation/Station.S_spi.csv")
write.csv(All_pre,"Z:/2022/Science9_SM/Output Data/Observation/All_pre.csv")
write.csv(All_spi,"Z:/2022/Science9_SM/Output Data/Observation/All_spi.csv")

# Analyses for Precipitation Variance ----------------------------------------------------------------

## Read
Station.S = read.csv("Z:/2022/Science9_SM/Output Data/Observation/Station.S_spi.csv",row.names = 1)
Pre.Summary = read.csv("Z:/2022/Science9_SM/Output Data/Observation/All_pre.csv",row.names = 1)

## Annual Value
Pre.Summary = Pre.Summary %>%
  dplyr::rename(value = precipitation) %>%
  dplyr::filter(month %in% c(1:12)) %>%
  dplyr::group_by(Code, year) %>%
  dplyr::summarise(value = mean(value))

## High-pass
Pre.Summary1 = Pre.Summary %>%
  dplyr::group_by(Code) %>%
  dplyr::reframe(year = year,
                 value = pass.filter.NA.mean(
                   a = value,
                   W = 10,
                   type = "high",
                   method = c("Butterworth")
                 ))

## Significance
Sig.F.test = function(x, y) {
  
  if(length(which(!is.na(x))) < 10 | length(which(!is.na(y))) < 10) {
    r1 = NA
    return(r1)
  }
  
  temp = var.test(x, y, alternative = "two.sided")
  r1 = temp$p.value
  r2 = temp$estimate
  return(r1)
}

## Variability
Pre_Var = Pre.Summary1 %>%
  dplyr::group_by(Code) %>%
  dplyr::summarise(
    Var_1 = sd(value[which(year %in% c(1911:1940))], na.rm = TRUE),
    Var_2 = sd(value[which(year %in% c(1961:1990))], na.rm = TRUE),
    Var_p = Sig.F.test(value[which(year %in% c(1911:1940))], value[which(year %in% c(1961:1990))])
  ) %>%
  dplyr::mutate(Var_Diff = Var_2 - Var_1,
                Var_Perc = (Var_2 - Var_1) / Var_1 * 100) %>%
  dplyr::filter(Var_Perc < 200 & Var_Perc > (-200)) %>%
  dplyr::filter(!is.na(Var_p))

## Combine
Pre_Var = merge(Pre_Var,Station.S, by = "Code")

## Summary
Pre_Var$Var_Type = NA
Pre_Var$Var_Type[which(Pre_Var$Var_Diff > 0 & Pre_Var$Var_p < 0.05)] = "SI"
Pre_Var$Var_Type[which(Pre_Var$Var_Diff < 0 & Pre_Var$Var_p < 0.05)] = "SD"
Pre_Var$Var_Type[which(Pre_Var$Var_Diff > 0 & Pre_Var$Var_p > 0.05)] = "NI"
Pre_Var$Var_Type[which(Pre_Var$Var_Diff < 0 & Pre_Var$Var_p > 0.05)] = "ND"

# Spatial Interpolation ------------------------------------------------------------

## Land Cover
Noah=nc_open("Z:/2024/Input Data/GRACE/LAND_MASK.CRI.nc")
Land.c=ncvar_get(Noah,"land_mask")
i.lon=ncvar_get(Noah, "lon")
i.lat=ncvar_get(Noah, "lat")
nc_close(Noah)

## Land Coverage
Land.coverage = expand.grid(Lon = i.lon, Lat = i.lat) %>%
  dplyr::mutate(Weight = as.numeric(Land.c)) %>%
  dplyr::filter(Weight == 1) %>%
  dplyr::filter(Lat > (-50) & Lat < 50)

## Grids
Land.coverage$Lon[which(Land.coverage > 180)] = Land.coverage$Lon[which(Land.coverage > 180)] - 360
i = 1

## Examines distance
for(i in 1:NROW(Land.coverage)) {
  
  ## Target
  T.Lon = Land.coverage$Lon[i]
  T.Lat = Land.coverage$Lat[i]
  Dis = 111*cos(T.Lat*pi/180)
  Land.coverage[i,"Dist.less.200"] = FALSE
  
  ## Stations
  Pre_Var_T = Pre_Var %>%
    dplyr::filter(Lon > (T.Lon - 5) &
                    Lon < (T.Lon + 5) &
                    Lat > (T.Lat - 500 / Dis) &
                    Lat < (T.Lat + 500 / Dis))
  
  ## Check
  if(NROW(Pre_Var_T) < 1) {
    Land.coverage[i,"Dist.less.200"] = T
  }
  
  ## Locations
  for (k in 1:NROW(Pre_Var_T)) {
    Pre_Var_T[k, "distance"] = distanceme(
      lon1 = T.Lon,
      lat1 = T.Lat,
      lon2 = Pre_Var_T$Lon[k],
      lat2 = Pre_Var_T$Lat[k]
    )
  }
  
  ## Check
  Pre_Var_T1 = Pre_Var_T %>%
    dplyr::filter(distance < 200)
  if (NROW(Pre_Var_T1) < 1) {
    Land.coverage[i, "Dist.less.200"] = T
  }
  
  ## Print
  print(i)
  
}

## Select Distance < 200
Land.coverage_S = Land.coverage %>% dplyr::filter(Dist.less.200 == FALSE)
Pre_Var_S = Pre_Var

## Interpolation
Temp =  interp::interp(
  x = Pre_Var_S$Lon,
  y = Pre_Var_S$Lat,
  z = Pre_Var_S$Var_Perc,
  xo = Land.coverage_S$Lon,
  yo = Land.coverage_S$Lat,
  duplicate = "strip",
  method = "linear",
  output = "points"
)

## Data.frame
Re.data = as.data.frame(Temp) %>%
  dplyr::filter(!is.na(z)) %>%
  dplyr::mutate(area = cos(y * pi /180) * 111 * 111 * 0.5 * 0.5 / 10^6)

## Test
if(1 == 0) {
  ggplot() +
    geom_point(data = Re.data, 
               aes(x = x, y = y, color = z)) +
    scale_color_stepsn(
      colours = brewer.pal(n=11,"RdYlBu")[11:1],
      limits = c(-100, 100),
      breaks = seq(-100, 100, 20),
      na.value = NA
    )
}

## Write
write.csv(Pre_Var_S,"Z:/2022/Science9_SM/Output Data/Observation/Pre_Var_S.csv")
write.csv(Re.data,"Z:/2022/Science9_SM/Output Data/Observation/Re.data.csv")

# Summary -----------------------------------------------------------------

## Weather Stations with increased variability
Pre_Var_S_1 = Pre_Var_S %>%
  dplyr::filter(Lat < 50 & Lat > (-50))
length(which(Pre_Var_S_1$Var_Perc > 0)) / NROW(Pre_Var_S_1)

## Grid Areas with increased variability
sum(Re.data$area[which(Re.data$z > 0)]) / sum(Re.data$area)

## Global mean
weighted.mean(Re.data$z, weight = Re.data$area,na.rm = TRUE)
plot(density(Re.data$z,na.rm = TRUE))
abline(v = 0)

## Select Australia
Pre_Var_S_1 = Pre_Var_S %>%
  dplyr::filter(Country == "AUSTRALIA    ")
length(which(Pre_Var_S_1$Var_Perc > 0)) / NROW(Pre_Var_S_1)
mean(Pre_Var_S_1$Var_Perc)

## Select Australia
Pre_Var_S_1 = Pre_Var_S %>%
  dplyr::filter(Country == "USA          ")
length(which(Pre_Var_S_1$Var_Perc > 0)) / NROW(Pre_Var_S_1)
mean(Pre_Var_S_1$Var_Perc)


# Plot Results --------------------------------------------------------------------

## Read
Pre_Var_S = read.csv("Z:/2022/Science9_SM/Output Data/Observation/Pre_Var_S.csv",row.names = 1)
Re.data = read.csv("Z:/2022/Science9_SM/Output Data/Observation/Re.data.csv",row.names = 1)

## Weather Stations
Pre_Var_S_1 = Pre_Var_S %>%
  dplyr::filter(Lat < 50 & Lat > (-50))

## Data
color.Cor<- brewer.pal(n=11,"RdYlGn")[11:1]

## Plot Meteorological Stations
p11 = ggplot() + 
  geom_sf(
    data = con.boundary,
    color = "gray30",
    fill = NA,
    linewidth = 0.3
  ) +
  geom_point(
    data = Pre_Var_S_1,
    aes(x = Lon, y = Lat, color = Var_Perc),
    shape = 16,
    size = 0.3,
    alpha = 1
  )  +
  scale_color_stepsn(
    colours = color.Cor,
    limits = c(-100, 100),
    breaks = seq(-100, 100, 20),
    name = "Changed precipitation variability (%)",
    na.value = NA
  ) +
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
    legend.justification.inside = c(0.9,0.01),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "white"),
    legend.title = element_text(hjust = 0.5),
    legend.title.position = "top",
    legend.key.width = unit(40,"pt"),
    legend.key.height = unit(5,"pt"),
    plot.margin = margin(7, 15, 10, 7, "pt"),
    axis.line.x.bottom = element_blank(),
    axis.line.y.left = element_blank(),
    axis.title.x.bottom = element_blank(),
    axis.title.y.left = element_blank()
  )

## Data
Re.data_A = Pre_Var_S_1 %>%
  dplyr::add_row(Var_Perc = seq(-79,161,20)) %>%
  dplyr::mutate(Group = cut(Var_Perc,seq(-80,160,20),labels = seq(-80,140,20))) %>%
  dplyr::filter(!is.na(Group)) %>%
  dplyr::group_by(Group) %>%
  dplyr::summarise(Area = length(Var_Perc))

## Mean
mean(Pre_Var_S_1$Var_Perc)

## Plot Hist gram
p12 = ggplot() +
  geom_vline(xintercept = 0, color = "gray10", linetype = "dashed")+
  geom_bar(data = Re.data_A,
           color = "#e9ecef",
           fill = "#69b3a2",
           just = 0,
           width = 1,
           aes(x = Group, y = Area),
           stat = 'identity') +
  annotate("text",x = Inf, y = Inf, hjust = 1.5, vjust = 1.5, label ="11.8%") +
  scale_x_discrete(
    name = "Changed precipitation variability (%)"
  ) +
  scale_y_continuous(breaks = seq(0, 1000, 200),
                     limits = c(0, 1000),
                     expand = c(0,0),
                     name = "Number of stations") +
  theme_figure1 +
  theme(plot.margin = margin(7, 15, 10, 7, "pt"))


## Plot Grid Results
p21 = ggplot() + 
  geom_sf(
    data = con.boundary,
    color = "gray30",
    fill = NA,
    linewidth = 0.3
  ) +
  geom_tile(data = Re.data,
              aes(x = x, y = y, fill = z))  +
  scale_fill_stepsn(
    name = "Changed precipitation variability (%)",
    colours = color.Cor,
    limits = c(-100, 100),
    breaks = seq(-100, 100, 20),
    na.value = NA
  ) +
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
    legend.justification.inside = c(0.9,0.01),
    legend.direction = "horizontal",
    legend.background = element_rect(fill = "white"),
    legend.title = element_text(hjust = 0.5),
    legend.title.position = "top",
    legend.key.width = unit(40,"pt"),
    legend.key.height = unit(5,"pt"),
    plot.margin = margin(7, 15, 10, 7, "pt"),
    axis.line.x.bottom = element_blank(),
    axis.line.y.left = element_blank(),
    axis.title.x.bottom = element_blank(),
    axis.title.y.left = element_blank()
  )

## Data
Re.data_A = Re.data %>%
  dplyr::add_row(z = seq(-79,161,20)) %>%
  dplyr::mutate(Group = cut(z,seq(-80,160,20),labels = seq(-80,140,20))) %>%
  dplyr::filter(!is.na(Group)) %>%
  dplyr::group_by(Group) %>%
  dplyr::summarise(Area = sum(area,na.rm = TRUE))

## Mean
weighted.mean(Re.data$z, w = Re.data$area)

## Plot Hist garea## Plot Hist gram
p22 = ggplot() +
  geom_bar(data = Re.data_A,
           color = "#e9ecef",
           fill = "#69b3a2",
           just = 0,
           width = 1,
           aes(x = Group, y = Area),
           stat = 'identity') +
  annotate("text",x = Inf, y = Inf, hjust = 1.5, vjust = 1.5, label ="10.4%") +
  scale_x_discrete(
    name = "Changed precipitation variability (%)"
  ) +
  scale_y_continuous(breaks = seq(0, 20, 4),
                     limits = c(0,20),
                     expand = c(0,0),
                     name = bquote("Land area ("*10^6*" "*km^2*")")) +
  theme_figure1 +
  theme(plot.margin = margin(7, 15, 10, 7, "pt"))


# Saving ------------------------------------------------------------------

## Top
p.top = plot_grid(
  p11,
  p12,
  labels = c("a", "b"),
  rel_widths = c(6, 4)
)

## Middle
p.bottom = plot_grid(
  p21,
  p22,
  labels = c("c", "d"),
  rel_widths = c(6, 4)
)

## Total
p.total=plot_grid(p.top,
                  p.bottom,
                  nrow =2)

## Saving
ggsave(
  plot = p.total,
  filename = "Figures/Analysis/Changed precipitation variability.pdf",
  device = cairo_pdf,
  width = 10,
  height = 3 + 3
)

# End ---------------------------------------------------------------------


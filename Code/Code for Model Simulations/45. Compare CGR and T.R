

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


## Plot
plot(CMIP_Summary_Sel$Var_tas_tropical_median,CMIP_Summary_Sel$Var_nbp_median)



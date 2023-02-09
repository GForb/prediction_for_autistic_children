library(tidyverse)
library(here)
library(ggplot2)
library(patchwork)

load(file.path(derived_data, "gui.Rdata"))

gui_data |> 
  select(-ID, where(~is.character(.))) |> 
  pivot_longer(everything())
  

# Obtaining a tibble of summaries ----
data_long <- gui_data |> 
  select(where(~!is.character(.)), -ID, -wave) |> 
  pivot_longer(everything())

summary_statistics <- data_long |> 
  group_by(name) |> 
  summarise(n = sum(!is.na(value)),
            mean = mean(value,na.rm = TRUE), 
            sd = sd(value, na.rm = TRUE),
            median = median(value, na.rm =TRUE),
            p25 = quantile (value, probs = 0.25, na.rm = TRUE),
            p75 = quantile (value, probs = 0.75, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE)
  )

for (i in 6:23) {
  tmp <- ggplot(gui_data, aes(x = 1.5, y = gui_data[,i], na.rm = T)) + 
    labs(x = as.character(names(gui_data[i])), y = "Value") +
    ggdist::stat_halfeye(
      adjust = .5, 
      width = .6, 
      .width = 0, 
      justification = -.3, 
      point_colour = NA) + 
    geom_boxplot(
      width = .25, 
      outlier.shape = NA
    ) +
    geom_point(
      size = 1.3,
      alpha = .3,
      position = position_jitter(
        seed = 1, width = .1
      )
    ) + 
    coord_cartesian(xlim = c(1.2, NA), clip = "off")
  print(tmp)
  par(mfrow = c(5, 5))
  print("-----------------------------")
}

gui_data_wave1 <- gui_data |> 
  filter(wave == 1)
gui_data_wave2 <- gui_data |> 
  filter(wave == 2)
gui_data_wave3 <- gui_data |> 
  filter(wave == 3)

for (j in 6:23) {
  print(paste("Wave 1 variable:", colnames(gui_data_wave1[j])))
  print(paste("Minimum value of variable in wave 1:", min(gui_data_wave1[,j], na.rm = T)))
  print(paste("Maximum value of variable in wave 1:", max(gui_data_wave1[,j], na.rm =T)))
  print(paste("Median value of variable in wave 1:", median(gui_data_wave1[,j], na.rm =T)))
  print(paste("Mean of variable in wave 1:", mean(gui_data_wave1[,j], na.rm =T)))
  tmp1 <- ggplot(gui_data_wave1, aes(x = 1.5, y = gui_data_wave1[,j], na.rm = T)) + 
    labs(x = as.character(names(gui_data_wave1[j])), y = "Wave 1 value") +
    ggdist::stat_halfeye(
      adjust = .5, 
      width = .6, 
      .width = 0, 
      justification = -.3, 
      point_colour = NA) + 
    geom_boxplot(
      width = .25, 
      outlier.shape = NA
    ) +
    geom_point(
      size = 1.3,
      alpha = .3,
      position = position_jitter(
        seed = 1, width = .1
      )
    ) + 
    coord_cartesian(xlim = c(1.2, NA), clip = "off")
  print(tmp1)
  print("-----------------------------")
}

for (k in 6:23) {
  print(paste("Wave 2 variable:", colnames(gui_data_wave2[k])))
  print(paste("Minimum value of variable in wave 2:", min(gui_data_wave2[,k], na.rm = T)))
  print(paste("Maximum value of variable in wave 2:", max(gui_data_wave2[,k], na.rm =T)))
  print(paste("Median value of variable in wave 2:", median(gui_data_wave2[,k], na.rm =T)))
  print(paste("Mean of variable in wave 2:", mean(gui_data_wave2[,k], na.rm =T)))
  tmp2 <- ggplot(gui_data_wave2, aes(x = 1.5, y = gui_data_wave2[,k], na.rm = T)) + 
    labs(x = as.character(names(gui_data_wave2[k])), y = "Wave 2 value") +
    ggdist::stat_halfeye(
      adjust = .5, 
      width = .6, 
      .width = 0, 
      justification = -.3, 
      point_colour = NA) + 
    geom_boxplot(
      width = .25, 
      outlier.shape = NA
    ) +
    geom_point(
      size = 1.3,
      alpha = .3,
      position = position_jitter(
        seed = 1, width = .1
      )
    ) + 
    coord_cartesian(xlim = c(1.2, NA), clip = "off")
  print(tmp2)
  print("-----------------------------")
}
for (l in 6:23) {
  print(paste("Wave 3 variable:", colnames(gui_data_wave3[l])))
  print(paste("Minimum value of variable in wave 3:", min(gui_data_wave3[,l], na.rm = T)))
  print(paste("Maximum value of variable in wave 3:", max(gui_data_wave3[,l], na.rm =T)))
  print(paste("Median value of variable in wave 3:", median(gui_data_wave3[,l], na.rm =T)))
  print(paste("Mean of variable in wave 3:", mean(gui_data_wave3[,l], na.rm =T)))
  tmp3 <- ggplot(gui_data_wave3, aes(x = 1.5, y = gui_data_wave3[,l], na.rm = T)) + 
    labs(x = as.character(names(gui_data_wave3[l])), y = "Wave 3 value") +
    ggdist::stat_halfeye(
      adjust = .5, 
      width = .6, 
      .width = 0, 
      justification = -.3, 
      point_colour = NA) + 
    geom_boxplot(
      width = .25, 
      outlier.shape = NA
    ) +
    geom_point(
      size = 1.3,
      alpha = .3,
      position = position_jitter(
        seed = 1, width = .1
      )
    ) + 
    coord_cartesian(xlim = c(1.2, NA), clip = "off")
  print(tmp3)
  print("-----------------------------")
}

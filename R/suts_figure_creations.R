## Scripts for figures for the SUTS talk

library(tidyverse)
library(rEDM)
library(epiforecast)
library(cowplot)
library(here)

source(here("R/helper_data_fxns.R")) ## Loading and using the epi data
source(here("R/helper_forecast_fxns.R")) ## Important functions for forecasting
source(here("R/helper_pred_parsing_fxns.R")) ## Taking predictions and parsing for outputting
source(here("R/main_forecast_fxns.R")) ## Main functions automating the forecasts


ili <- load_all_fluview("nat")
ili <- ili %>% group_by(date) %>%
  filter(release_date == max(release_date), date >= "2010-08-30")

ili %>%
  ggplot(aes(date, wili)) +
    geom_line() +
    labs(x = "", y = "Influenza Prevalence") -> ili_plot

ili_plot
save_plot("suts_figs/ili_plot.png", ili_plot, base_height = 3, base_aspect_ratio = 2)

season_data <- ili %>% group_by(season) %>%
  filter(week %in% c(20, 40)) %>%
  summarize(min_date = min(date), max_date = max(date))

ili  %>%
  ggplot(aes(date, wili)) +
  geom_rect(data = season_data, aes(xmin = min_date, xmax = max_date, ymin = -Inf, ymax = Inf, group = season), inherit.aes = FALSE, alpha=.1)+
  geom_rect(data = data_frame(min = as.Date("2017-10-01"), max=as.Date("2018-01-14")), aes(xmin = min, xmax = max, ymin = -Inf, ymax = Inf), alpha = .1, inherit.aes=FALSE) +
  geom_line() +
  labs(x = "", y = "Influenza Prevalence")

save_plot("suts_figs/season_plot.png", season_plot, base_height = 3, base_aspect_ratio = 2)


load("suts_figs/daily_specific_humidities_us_2009.Rdata")


ili %>%
  filter(year %in% c(2013, 2014)) %>%
  ungroup() %>%
  mutate(humidity = rep(colMeans(matrix(daily_specific_humidities, 7)), length.out = 105)) -> hum_ili_data

hum_ili_data %>%
  ggplot(aes(date, wili)) +
    geom_line() +
    labs(x = "", y = "Influenza Prevalence")-> p1

hum_ili_data %>%
  ggplot(aes(date, humidity)) +
    geom_line(lty = 2) +
    labs(x = "", y = "Humidity")+
    scale_y_continuous(position = "right") -> p2



aligned_plots <- align_plots(p1, p2, align="hv", axis="tblr")
combined <- ggdraw(aligned_plots[[1]]) +draw_plot(aligned_plots[[2]])
save_plot("suts_figs/ili_alone_hum_plot.png", aligned_plots[[1]], base_height = 3, base_aspect_ratio = 1.8)
save_plot("suts_figs/ili_hum_plot.png", combined, base_height = 3, base_aspect_ratio = 1.8)


ili %>% filter(year %in% c(2015,2016,2017)) %>%
  ggplot(aes(date,wili)) +
  geom_line() +
  coord_cartesian(xlim = as.Date(c("2015-09-15", "2017-05-15"))) +
  # geom_vline(xintercept = as.Date("2016-01-05"), lty=2) +
  # geom_vline(xintercept = as.Date("2017-01-07"), lty=2) +
  labs(x = "", y = "Influenza Prevalence")-> school_cal_plot
save_plot("suts_figs/school_cal_plot.png", school_cal_plot, base_height = 3, base_aspect_ratio = 1.8)


all_ili <- load_all_fluview("nat")
all_ili <- all_ili %>% group_by(date) %>%
  filter(release_date == max(release_date))



plot_trick_seasons <- function(all_ili, flu_season, max_date){
  ili_dat <- all_ili %>%
    filter(season == flu_season) %>%
    mutate(before_date = date < max_date)
  last_true <- match(FALSE, ili_dat$before_date) - 1
  ili_dat <- ili_dat[c(1:last_true, last_true:nrow(ili_dat)),]
  ili_dat$before_date[last_true+1] <- FALSE

  ili_dat %>%
    ggplot(aes(date, wili, color = before_date)) +
      labs(x="", y = "Influenza Prevalence") +
      scale_color_manual(values = c("white", "black"), guide=FALSE) +
      geom_line(size=1) -> before_peak_plot


  ili_dat %>%
    ggplot(aes(date, wili, color = before_date)) +
      labs(x="", y = "Influenza Prevalence")+
      scale_color_manual(values = c("blue", "black"), guide=FALSE) +
      geom_line(size=1) -> full_season_plot

  return(list(before_peak_plot, full_season_plot))
}

save_trick_plots <- function(plots, lettering){
  data_frame(filename = c(paste0("suts_figs/trick_flu_", lettering, "1.png"),
                          paste0("suts_figs/trick_flu_", lettering, "2.png")),
              plot = plots) %>% pmap(.f = save_plot, base_height = 3, base_aspect_ratio = 1.8)
}



# Trick plot 2 - goes up
plot_trick_seasons(all_ili, 1999, "1999-12-20") %>%  save_trick_plots(lettering = "a")

# Trick plot 3 - goes down then up
plot_trick_seasons(all_ili, 2002, "2002-12-25") %>% save_trick_plots(lettering = "b")

# Extra tricky plot 4 - 2009!
plot_trick_seasons(all_ili, 2008, "2009-04-15") %>% save_trick_plots(lettering="c")

## Plot the 2018 data -- Can leave as a question mark, and then intro the flusight website!
## Flu near you, and what not

all_ili %>%
  filter(season == 2017) %>%
  ggplot(aes(date, wili)) +
    geom_line(size=1) + labs(x="", y = "Influenza Prevalence")+
    coord_cartesian(xlim = as.Date(c("2017-09-01", "2018-03-01"))) -> curr_seas_plot
save_plot("suts_figs/curr_seas_plot.png", curr_seas_plot, base_height = 3, base_aspect_ratio = 1.8)



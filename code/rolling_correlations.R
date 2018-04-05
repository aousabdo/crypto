# this tutorial is taken from this source: 
# http://www.business-science.io/timeseries-analysis/2017/07/30/tidy-timeseries-analysis-pt-3.html

library(tidyquant)  # Loads tidyverse, tidyquant, financial pkgs, xts/zoo
library(cranlogs)   # For inspecting package downloads over time
library(corrr)      # Tidy correlation tables and correlation plotting
library(cowplot)    # Multiple plots with plot_grid()

# tidyverse packages (see my laptop stickers from first post) ;)
pkgs <- c(
  "tidyr", "lubridate", "dplyr", 
  "broom", "tidyquant", "ggplot2", "purrr", 
  "stringr", "knitr"
)

# Get the downloads for the individual packages
tidyverse_downloads <- cran_downloads(
  packages = pkgs, 
  from     = "2017-01-01", 
  to       = "2017-06-30") %>%
  tibble::as_tibble() %>%
  group_by(package)

# Visualize the package downloads
tidyverse_downloads %>%
  ggplot(aes(x = date, y = count, color = package)) +
  # Data
  geom_point(alpha = 0.5) +
  facet_wrap(~ package, ncol = 3, scale = "free_y") +
  # Aesthetics
  labs(title = "tidyverse packages: Daily downloads", x = "",
       subtitle = "2017-01-01 through 2017-06-30",
       caption = "Downloads data courtesy of cranlogs package") +
  scale_color_tq() +
  theme_tq() +
  theme(legend.position="none")


# Get data for total CRAN downloads
all_downloads <- cran_downloads(from = "2017-01-01", to = "2017-06-30") %>%
  tibble::as_tibble()

# Visualize the downloads
all_downloads %>%
  ggplot(aes(x = date, y = count)) +
  # Data
  geom_point(alpha = 0.5, color = palette_light()[[1]], size = 2) +
  # Aesthetics
  labs(title = "Total CRAN Packages: Daily downloads", x = "",
       subtitle = "2017-01-01 through 2017-06-30",
       caption = "Downloads data courtesy of cranlogs package") +
  scale_y_continuous(labels = scales::comma) +
  theme_tq() +
  theme(legend.position="none")

# Correlation table
tidyverse_static_correlations <- tidyverse_downloads %>%
  # Data wrangling
  spread(key = package, value = count) %>%
  left_join(all_downloads, by = "date") %>%
  rename(all_cran = count) %>%
  select(-date) %>%
  # Correlation and formating
  correlate() 

# Pretty printing
tidyverse_static_correlations %>%
  shave(upper = F)

# Network plot
gg_all <- tidyverse_static_correlations %>%
  network_plot(colours = c(palette_light()[[2]], "white", palette_light()[[4]]), legend = TRUE) +
  labs(
    title = "Correlations of tidyverse Package Downloads to Total CRAN Downloads",
    subtitle = "Looking at January through June, tidyquant is a clear outlier"
  ) +
  expand_limits(x = c(-0.75, 0.25), y = c(-0.4, 0.4)) +
  theme_tq() +
  theme(legend.position = "bottom")
gg_all

# Get rolling correlations
tidyverse_rolling_corr <- tidyverse_downloads %>%
  # Data wrangling
  left_join(all_downloads, by = "date") %>%
  select(date, package, count.x, count.y) %>%
  # Mutation
  tq_mutate_xy(
    x          = count.x,
    y          = count.y,
    mutate_fun = runCor, 
    # runCor args
    n          = 30,
    use        = "pairwise.complete.obs",
    # tq_mutate args
    col_rename = "rolling_corr"
  )

# Join static correlations with rolling correlations
tidyverse_static_correlations <- tidyverse_static_correlations %>%
  select(rowname, all_cran) %>%
  rename(package = rowname)

tidyverse_rolling_corr <- tidyverse_rolling_corr %>%
  left_join(tidyverse_static_correlations, by = "package") %>%
  rename(static_corr = all_cran)

# Plot
tidyverse_rolling_corr %>%
  ggplot(aes(x = date, color = package)) +
  # Data
  geom_line(aes(y = static_corr), color = "red") +
  geom_point(aes(y = rolling_corr), alpha = 0.5) +
  facet_wrap(~ package, ncol = 3, scales = "free_y") +
  # Aesthetics
  scale_color_tq() +
  labs(
    title = "tidyverse: 30-Day Rolling Download Correlations, Package vs Total CRAN",
    subtitle = "Relationships are dynamic vs static correlation (red line)",
    x = "", y = "Correlation"
  ) +
  theme_tq() +
  theme(legend.position="none")



# Redrawing Network Plot from April through June
gg_subset <- tidyverse_downloads %>%
  # Filter by date >= April 1, 2017
  filter(date >= ymd("2017-04-01")) %>%
  # Data wrangling
  spread(key = package, value = count) %>%
  left_join(all_downloads, by = "date") %>%
  rename(all_cran = count) %>%
  select(-date) %>%
  # Correlation and formating
  correlate() %>%
  # Network Plot
  network_plot(colours = c(palette_light()[[2]], "white", palette_light()[[4]]), legend = TRUE) +
  labs(
    title = "April through June (Last 3 Months)",
    subtitle = "tidyquant correlation is increasing"
  ) +
  expand_limits(x = c(-0.75, 0.25), y = c(-0.4, 0.4)) +
  theme_tq() +
  theme(legend.position = "bottom")

# Modify the January through June network plot (previous plot)
gg_all <- gg_all +
  labs(
    title = "January through June (Last 6 months)",
    subtitle = "tidyquant is an outlier"
  )

# Format cowplot
cow_net_plots <- plot_grid(gg_all, gg_subset, ncol = 2)
title <- ggdraw() + 
  draw_label(label = 'tidyquant is getting "tidy"-er',
             fontface = 'bold', size = 18)
cow_out <- plot_grid(title, cow_net_plots, ncol=1, rel_heights=c(0.1, 1))
cow_out

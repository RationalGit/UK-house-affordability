library(svglite)
library(sp)
library(sf)
library(ggplot2)
library(mapproj)
library(broom)
library(dplyr)
library(viridis)
library(httr)
library(scales)
library(purrr)
library(tidyverse)
library(rjson)
library(gganimate)
library(gifski)
library(png)
library(data.table)
library(viridis)
library(rgdal)

# setwd to data folder


spdf_EW_data <- readRDS(file = "spdf_EW_data.rds")
setwd("..")
setwd("./plots/")

scatter_regions_p <- data_LA %>%
  ggplot(aes(x = reorder(Region.name, -Income, mean, na.rm = T), y = Income)) +
  
# to_Match <- c("2011", "2012", "2014", "2016")
# data.working <- cbind(data[, c(1:4)], data[, grep(paste(to_Match,collapse="|"),  colnames(data))])
# data.working <- data.working %>%
#   merge(x = . , y = prices_year_df, by.x="MSOA.code", by.y="MSOA code") 
# 
# colnames(data.working)
# max(data.working[,"Year.ending.Sep.2018"], na.rm = TRUE)
# # Distribution of the house prices in the year to december 2018
separate_DF <- long.data %>% separate(Quarter, c("Month", "Year"))

theme_set(theme_bw())

p <- function(df,
              x_var = "Price",
              bins = 200,
              anim_by = "Quarter",
              y_var_name = "house price",
              fill_colour = 'skyblue',
              x_lim = 2.5e6
              ){ 
  anim_by <- sym(anim_by)            
  df %>% 
  ggplot(aes(x = get(x_var))) + 
  geom_histogram(bins = bins, fill= fill_colour, color='white') +
  labs(
    ylab = paste("Number of regions with", y_var_name, "in range", sep = " "), 
    caption = "Data: ONS | Creator: Tom Webber"
  ) +
  # displays as you require
  scale_x_continuous(labels = dollar_format(suffix = "", prefix = "£"), name="House price", limits = c(0, x_lim), minor_breaks = seq(0, 2.5e6, 1e5)) +
  scale_y_continuous(name = paste("Number of regions with that average", y_var_name, sep = " "), limits = c()) +
  # scale_y_continuous(name = "Number of regions with that average house price", trans = "pseudo_log")
  transition_states(!!anim_by, transition_length = 1, state_length = 1) +
  labs(title = paste("Median", y_var_name, "across England and Wales, year ending: {closest_state}", sep = " "))
  
}
a <- animate(p(long.data), 
             nframes = (2*30 + 2 * length(unique(long.data$Quarter))), 
             fps = 10,
             renderer = gifski_renderer(),
             end_pause = 30 
             )
anim_save("house_prices.gif", 
          a, 
          fps=10
          )

total_income_df <- separate_income_df[separate_income_df$'Income type' == 'Total',]

b <- animate(p(total_income_df, 
               x_var = "Income", 
               x_lim = 1.5e5,
               bins = 100,
               y_var_name = "Total Income", 
               fill_colour = 'seagreen4',
               anim_by = "Year"),
             nframes = (3*length(unique(total_income_df$Year))), 
             fps = 2,
             renderer = gifski_renderer()
) 
anim_save("Total incomes.gif", 
          b, 
          fps=2
)

max(total_income_df$Income, na.rm = TRUE)
save_names <- paste0("test_", 1:(length(colnames(data))-4))
column_names <- colnames(data[5:length(colnames(data))])
lapply(column_names, histogram_sequence, save_names)
histogram_sequence(column_names[1], save_names[1])


# Make the merge


head(spdf_EW_data)

# msoa.areas <- unique(spdf_fortified$id)
# df1 <- filter(spdf_fortified_4d, id %in% msoa.areas[1:1000],)
minimum.df <- colSums(is.na(spdf_EW_data))
minimum.df <- colSums(is.na(data))
min(minimum.df[4:length(minimum.df)])
save.image <- TRUE

require(scales)
p <- function(df){
  ggplot() +
  geom_polygon(data = df, 
               aes(fill = Year.ending.Dec.2016, x = long/10000, y = lat/10000, group = group) , 
               size=0, 
               alpha=0.9) +
  theme_void() +
  scale_fill_viridis(trans = "log", 
                     breaks=c(50000, 100000,250000, 500000,1000000,2500000), 
                     name="House price",
                     labels = comma) + 
                     # guide = guide_legend(keyheight = unit(2, units = "mm"), 
                     #                      keywidthv= unit(8, units = "mm"), 
                     #                      label.position = "bottom", 
                     #                      title.position = 'top', 
                     #                      nrow=1) ) +
  scale_y_continuous(limits = c(0, NA), labels = comma) +
  labs(
    title = "Median house price \nper region in the UK",
    # subtitle = "", 
    caption = "Data: ONS | Creator: Tom Webber"
  ) +
  theme(
    text = element_text(color = "#22211d"), 
    plot.background = element_rect(fill = "#f5f5f2", 
                                   color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", 
                                    color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", 
                                     color = NA),
    #legend.position = 'right',
    plot.title = element_text(size= 16, 
                              hjust=0.01, 
                              color = "#4e4d47", 
                              margin = margin(b = -0.1, 
                                              t = 0.4, 
                                              l = 2, 
                                              unit = "cm")),
    plot.subtitle = element_text(size= 13, 
                                 hjust=0.01, 
                                 color = "#4e4d47", 
                                 margin = margin(b = -0.1, 
                                                 t = 0.43, 
                                                 l = 2, 
                                                 unit = "cm")),
    plot.caption = element_text(size=10, 
                                color = "#4e4d47", 
                                margin = margin(b = 0.3, 
                                                r=-99, 
                                                unit = "cm") ),
    legend.position = c(0.0, 0.0)
  ) +
  coord_map()
}

## Map projection: https://medium.com/@amy.degenaro/introduction-to-digital-cartography-geojson-and-d3-js-c27f066aa84
## Saving in function

print(spdf_EW_data$id[1])
graph.title <- paste("test_", "12_2016_3", sep = "_")
ggsave(
  file=paste(gsub("/", "x", graph.title), ".png", sep=""),
  scale = 0.5,
  plot=p(spdf_EW_data), 
  limitsize = FALSE)
print(paste(graph.title, "saved", sep = " "))
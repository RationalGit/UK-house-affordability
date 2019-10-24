library(svglite)
library(geojsonio)
library(sp)
library(ggplot2)
library(mapproj)
library(broom)
library(dplyr)
library(viridis)
library(httr)
library(scales)
library(purrr)
library(tidyverse)
library(jsonlite)
library(rjson)
library(gganimate)
library(gifski)
library(png)
library(data.table)
library(viridis)

## Up to date regions downloaded as shapefile: https://borders.ukdataservice.ac.uk/easy_download_data.html?data=infuse_uk_2011
## Converted to geoJSON using: https://mapshaper.org/
## Other details available at: https://census.ukdataservice.ac.uk/use-data/guides/boundary-data.aspx
##Scot data from: https://statistics.gov.scot/slice?dataset=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fhouse-sales-prices&http%3A%2F%2Fpurl.org%2Flinked-data%2Fcube%23measureType=http%3A%2F%2Fstatistics.gov.scot%2Fdef%2Fmeasure-properties%2Fmedian

## Read in the geoJSON file downloaded from ONS using the geojsonio library:
## spdf = spatial dataframe, EW = England & Wales

## https://blog.exploratory.io/making-maps-for-uk-countries-and-local-authorities-areas-in-r-b7d222939597

spdf_GB <- geojson_read("G:/Tom Webber/Programming/Projects/20190801 UK years to home ownership/infuse_msoa_lyr_2011.json", method = "local", what = "sp")

## For testing, I subsetted a small segment of the data (MSOAs with ID's matching 01xxx or 02xxx)
# spdf1 <- spdf_EW[ substr(spdf_EW@data$msoa01cd,5,6) %in% c("01", "02") , ]

## Fortify the data (convert spatial data into a dataframe object) and keep track of the MSOA code (Takes a while for the whole dataset)
# spdf_fortified_1 <- tidy(spdf1, region = "msoa01cd")
spdf_fortified <- tidy(spdf_EW, region = "msoa01cd")
spdf_GB_fort <- tidy(spdf_GB, region = "geo_code")
to_Match_1 <- c('^[E]*[0-9]','^[W]*[0-9]')
spdf_EW_fort <- spdf_GB_fort[grep(paste(to_Match_1,collapse="|"), spdf_GB_fort[["id"]]), ]
## Save an object to a file so I don't have to do that again
# Desktop
setwd("G:/Tom Webber/Programming/Projects/20190801 UK years to home ownership")

saveRDS(spdf_EW_fort, file = "spdf_EW_fort.rds")
saveRDS(spdf_GB_fort, file = "spdf_GB_fort.rds")

# Restore the object
spdf_EW_fort <- readRDS(file = "spdf_EW_fort.rds")
spdf_GB_fort <- readRDS(file = "spdf_GB_fort.rds")

# read data
# House price data found here: https://www.ons.gov.uk/peoplepopulationandcommunity/housing/datasets/medianhousepricebymsoaquarterlyrollingyearhpssadataset02a
# Downloaded and unzipped
data <- read.csv("C:/Users/tomwe/Dropbox (Personal)/Training/Coding/20190801 UK years to home ownership/MSOA_MedianHousePrice_UK_.csv", skip = 5, header=T, na.strings = ":")
# Desktop: 
data <- read.csv("G:/Tom Webber/Dropbox (Personal)/Training/Coding/20190801 UK years to home ownership/MSOA_MedianHousePrice_UK_.csv", skip = 5, header=T, na.strings = ":")

clean <- function(ttt){
  as.numeric( gsub('[^a-zA-Z0-9.]', '', ttt))
}

col2cvt <- 5:97
data[,col2cvt] <- sapply(data[col2cvt], clean)


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
require(scales)
colnames(data) <- gsub(".*\\.([[:alpha:]]{3})\\.([[:digit:]]{4})", "\\1 \\2", colnames(data))

long.data <- data %>%
  gather(Quarter, Price, 5:97)
separate_DF <- long.data %>% separate(Quarter, c("Month", "Year"))
names(long.data[5])
long.data[,"Quarter"] <- as.Date(paste('01', long.data[,"Quarter"]), format='%d %b %Y')

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
anim_save("house_prices_3.gif", 
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

spdf_EW_data <- spdf_EW_fort %>%
  merge(x = . , y = data.working, by.x="id", by.y="MSOA.code") 
spdf_EW_data <- arrange(spdf_EW_data, id, order)

setwd("G:/Tom Webber/Dropbox (Personal)/Training/Coding/20190801 UK years to home ownership/")
setwd("C:/Users/Tomwe/Dropbox (Personal)/Training/Coding/20190801 UK years to home ownership/")
saveRDS(spdf_EW_data, file = "spdf_EW_data.rds")
# Restore the object
spdf_EW_data <- readRDS(file = "spdf_EW_data.rds")

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
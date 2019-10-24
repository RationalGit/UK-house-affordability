setwd("C:/Users/tomwe/Dropbox (Personal)/Training/Coding/20190801 UK years to home ownership/LA data/LA data")
# read data
# House price data found here: https://www.ons.gov.uk/peoplepopulationandcommunity/housing/datasets/medianhousepricebymsoaquarterlyrollingyearhpssadataset02a
library(ggthemes)
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
library(RColorBrewer)
library(extrafont)

clean <- function(ttt){
  as.numeric( gsub('[^a-zA-Z0-9.]', '', ttt))
}

data_LA_I <- read.csv("C:/Users/tomwe/Dropbox (Personal)/Training/Coding/20190801 UK years to home ownership/LA data/LA_I_UK.csv", skip = 6, header=T, na.strings = ":", check.names = F)
data_LA_HP <- read.csv("C:/Users/tomwe/Dropbox (Personal)/Training/Coding/20190801 UK years to home ownership/LA data/LA_HP_UK.csv", skip = 6, header=T, na.strings = ":", check.names = F)
data_LA_Ratio <- read.csv("C:/Users/tomwe/Dropbox (Personal)/Training/Coding/20190801 UK years to home ownership/LA data/LA_HP_I_ratio_UK.csv", skip = 6, header=T, na.strings = ":", check.names = F)
names(data_LA_HP) <- names(data_LA_I)
df_gather <- function(df, col_name){
  col2cvt <- 5:21
  df[,col2cvt] <- sapply(df[col2cvt], clean)
  df <- df %>%
  gather(Year, !!col_name, 5:21)
  make.names(df)
  return(df)
}
data_LA_I <- df_gather(data_LA_I[1:348,], 'Income')
data_LA_HP <- df_gather(data_LA_HP[1:348,], "House.Price")
data_LA_Ratio <- df_gather(data_LA_Ratio[1:348,], "Ratio")

data_LA <- cbind(data_LA_I, data_LA_HP[,6], data_LA_Ratio[,6])
names(data_LA)[7:8] <- c("House.price", "Ratio")
names(data_LA) <- make.names(names(data_LA))
data_LA$Year <- as.numeric(data_LA$Year)
sapply(data_LA, class)

data_LA <- data.table(data_LA)
data_LA[,list(mean=mean(Income, na.rm = T)),by=Region.name]
data_LA$median.income <- round(ave(data_LA$Income, as.factor(data_LA$Region.name), as.factor(data_LA$Year), FUN = function(x) median(x,na.rm = T)))
data_LA$median.HP <- round(ave(data_LA$House.price, as.factor(data_LA$Region.name), as.factor(data_LA$Year), FUN = function(x) median(x,na.rm = T)))
data_LA$median.Ratio <- round(ave(data_LA$Ratio, as.factor(data_LA$Region.name), as.factor(data_LA$Year), FUN = function(x) median(x,na.rm = T)))

setwd("C:/Users/tomwe/Dropbox (Personal)/Training/Coding/20190801 UK years to home ownership/LA data/")
saveRDS(data_LA, file = "data_LA.rds")
data_LA <- readRDS(file = "data_LA.rds")

col_names <- c("Income", "House.price", "Ratio")
y_labs <- c("income", "house price", "house price/income ratio")
names(data_LA)
get_palette = colorRampPalette(rainbow(9, start = 0, end = 0.8))
#get_palette = colorRampPalette(viridis(10, begin = 0.1, end = 0.95, option = "magma"))

for(i in 1:3){
  col_name = sym(col_names[i]) #https://stackoverflow.com/questions/22309285/how-to-use-a-variable-to-specify-column-name-in-ggplot/53168593#53168593
  y_lab = y_labs[i]
  data_LA %>%
    ggplot(aes(x = Year, y = median)) +
    ## geom_line(aes(colour = reorder(Region.name, -Income, mean, na.rm = T)), show.legend = TRUE) +
    geom_smooth(aes(y = !!col_name, colour = reorder(Region.name, -Income, mean, na.rm = T), fill = reorder(Region.name, -Income, mean, na.rm = T))) +
    scale_colour_manual(name = "Regions", values = c(viridis(5, end = 0.88), viridis(5, end = 0.84, option = "magma"))) +
    scale_fill_viridis(name = "Regions", discrete = TRUE) +
    # facet_wrap(facets = vars(reorder(Region.name, -Income, mean, na.rm = T))) +
    labs(y = paste("Median", y_lab, "per local authority", sep = " "), 
         x = "Year", 
         title = paste("Median", y_lab, "across local authorities, 2002-2018", sep = " "),
         subtitle = paste("Regions ranked by median", y_lab, "in 2002", sep = " "), 
         caption = "Data: ONS | Creator: Tom Webber",
         fill = "Regions",
         colour = "Regions") +
    scale_y_continuous(limits = c(0, NA), label=scales::comma) +
    scale_x_continuous(breaks = seq(2002, 2018, 2)) +
    theme_classic(base_family = "Calibri") +
    theme(legend.position = "right", 
          text = element_text(size = rel(4)),
          axis.title = element_text(size = rel(0.9)),
          legend.text=element_text(size = rel(2.5)),
          plot.title = element_text(size = rel(4)),
          plot.subtitle = element_text(size = rel(2.5),
                                       colour = "grey40"),
          plot.caption = element_text(size = rel(2.5),
                                      colour = "grey40")) +
    ggsave(paste0("Median_", make.names(y_lab), "_LA_2", ".png"),
           width = 9,
           height = 5,
           dpi = 220)
}
## could mark the ddx = 0 'gradient minima' to show onset of 'slowdown' affected by 2008

require(splines)

model_df <- data_LA[data_LA$Region.name=="London",]
data_LA_1 <- tapply(data_LA$Year, data_LA$Region.name, data_LA$Income, FUN = function(x){predict(lm(data = x, Income~ns(Year,3)), Year)})
length(data_LA$Region.name)
model1 <- predict(lm(Income~ns(Year,3),data=model_df))

# require(tidyverse)
# df <- data.frame(x1 = seq(1, 100, 10),
#                  x2 = (1:10)^2,
#                  y =  seq(1, 20, 2))
# 
# pred_df <- df %>% 
#   gather(var, val, -y) %>% 
#   nest(-var) %>% 
#   mutate(model = map(data, ~glm(y~val, data = .)), 
#          predicted = map(model, predict)) %>% 
#   unnest(data, predicted)

pred_data_LA <- data_LA %>%
  nest(-Region.name) %>%
  mutate(model = map(data, ~lm(Income~ns(Year, 3), data = ., na.action=na.exclude)),
         Predicted = map(model, predict)) %>%
  unnest(data, Predicted) 
diff_data_LA <- pred_data_LA %>%
  select(Region.name, Predicted, Year) %>%
  group_by(Region.name) %>%
  distinct() %>%
  filter(!is.na(Predicted)) %>%
  mutate(dIncome = c(NA, diff(Predicted)/diff(Year)),
         m_dIncome = (min(dIncome, na.rm = T) == dIncome | max(dIncome, na.rm = T) == dIncome))

data_LA_ <- inner_join(pred_data_LA, diff_data_LA)
data_LA_$m_dIncome[data_LA_$m_dIncome == 'FALSE'] <- NA

# p1 <- pred_df %>% 
#   ggplot(aes(x = val, group = var))+
#   geom_point(aes(y = y))+
#   geom_line(aes(y = predicted))
# p1
# 
# df2 <- df %>% 
#   gather(var, val, -y) %>% 
#   nest(-var) %>% 
#   mutate(model = map(data, ~glm(y~val, data = .)), 
#          predicted = map(model, predict)) %>%
#   mutate(intercept = map(model, ~summary(.x)$coefficients[[1]]),
#          slope = map(model, ~summary(.x)$coefficients[[2]]))

X <- data.frame(Year=seq(2002,2018,1) ) # make an ordered sequence
Y <- predict(model,newdata=X) # calculate predictions for that sequence
plot(X$Year,Y,type="l",main="Original fit") #check

dY <- diff(Y)/diff(X$Year)  # the derivative of your function
dX <- rowMeans(embed(X$Year,2)) # centers the X values for plotting
plot(dX,dY,type="l",main="Derivative") #check
custom_breaks <- c("London", "South East", "East", "West Midlands", "East Midlands", "South West", "North West", "Yorkshire and The Humber", "North East", "Wales")

p1 <- data_LA_ %>%
  ggplot(aes(x = Year, y = dIncome, color =  reorder(Region.name, -Income, mean, na.rm = T))) +
  ## Jointed line
  # geom_line(aes(colour = reorder(Region.name, -Income, mean, na.rm = T)), show.legend = TRUE) +
  ## Single colour scale
  geom_smooth(size = 0.9) +
  scale_colour_manual(name = "Regions", values = c(viridis(5, begin = 0.04, end = 0.92), viridis(5, end = 0.84, option = "magma"))) +
  ## Colour scale repeated across two line styles
  # geom_smooth(aes(linetype = reorder(Region.name, -Income, mean, na.rm = T)), size = 0.9) +
  # scale_linetype_manual(name = "Regions", values = rep(c('solid', 'longdash'),each = 5), breaks = custom_breaks) +
  # scale_colour_manual(name = "Regions", values = rep(viridis(5, begin = 0.1, end = 0.95), 2), breaks = custom_breaks) +
  geom_point(data = subset(data_LA_, m_dIncome == "TRUE"), size = 3, shape = 1) +
  labs(title = "Per-region annual median income growth, 2002-2018", 
       subtitle = "Regions ranked by median income in 2002", 
       caption = "Data: ONS | Creator: Tom Webber",
       y = "Annual median income growth per local authority", 
       x = "Year", 
       colour = "Regions") +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(breaks = seq(2002, 2018, 2)) +
  theme_light(base_family = "Calibri") +
  theme(legend.position = "right", 
        text = element_text(size = rel(4)),
        legend.text=element_text(size = rel(2.5)),
        plot.title = element_text(size = rel(4)),
        plot.subtitle = element_text(size = rel(2.5),
                                     colour = "grey40"),
        plot.caption = element_text(size = rel(2.5),
                                    colour = "grey40"),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank())

p1

ggsave("Median_Income_Growth_LA_all_1.png",
       plot = p1,
       width = 8*1.25,
       height = 5*1.25,
       dpi = 220)

p2 <- p1 +
  geom_smooth(size = 1.5) +
  facet_wrap(vars(reorder(Region.name, -Income, mean, na.rm = T)), nrow = 2) +
  scale_x_continuous(breaks = seq(2002, 2018, 4)) +
  theme(legend.position = "none",
        text = element_text(size = rel(4)), 
        strip.text.x = element_text(size = rel(3), 
                                    face = "bold"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1))
 
p2
ggsave("Median_Income_Growth_LA_facet_1.png",
       plot = p2,
       width = 9,
       height = 5,
       dpi = 220)
n = 2
violin_regions_Income <- data_LA %>%
  ggplot(aes(x = reorder(Region.name, -Income, mean, na.rm = T), y = Income)) +
  geom_violin(aes(fill = median.income), trim = FALSE) +
  scale_fill_gradient(low="white",high = "black") +
  labs(y = paste("Median", "income", "per local authority", sep = " "), 
       x = "Regions", 
       subtitle = "Regions ranked by median income in 2002", 
       caption = "Data: ONS | Creator: Tom Webber",
       fill = "Median for each region") +
  theme_light(base_family = "Calibri") +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1),
        legend.position = "right", 
        text = element_text(size = rel(n^2*3.75)),
        legend.text = element_text(size = rel(n^2*2)),
        legend.title = element_text(size = rel(n^2*2.5)),
        axis.title = element_text(size = rel(n^2*0.9)),
        plot.title = element_text(size = rel(n^2*2.5)),
        plot.subtitle = element_text(size = rel(n^2*2.5),
                                     colour = "grey40"),
        plot.caption = element_text(size = rel(n^2*2.5),
                                    colour = "grey40")) +
  scale_y_continuous(limits = c(0, NA), label=scales::comma) +
  transition_states(Year, transition_length = 2, state_length = 1) +
  labs(title = paste("Median", "income", "across England and Wales, {closest_state}", sep = " ")) +
  theme(plot.title = element_text(size = rel(n^2*3.5)))

violin_regions_HP
anim_save("LA_Ratio_big2.gif",
          animate(violin_regions_Ratio,
                  width = 500*n,
                  height = 400*n,
                  end_pause = 10,
                  nframes = (2*10 + 2*length(unique(data_LA$Year))), 
                  renderer = gifski_renderer()
          ),
          scale = 2
          
)
# ggplot: faceted linear regressions for the different regions (10 regions)

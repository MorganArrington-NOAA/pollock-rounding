# Packages
library(FSA)
library(flextable)
library(grid)
library(cowplot)
library(tidyverse)
library(ggplotify)

# Load data
df1 <- read.csv(file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Rounding conversation SIDT/pollock-rounding/Data/model_predictions.csv")

# Rounding schemes
df1_rounded <- df1 %>%
  mutate(nearest_int = round(prediction), round_down = floor(prediction), round_up = ceiling(prediction))

##################  
# Nearest integer (status quo)
bias1 <- ageBias(reference~nearest_int,data=df1_rounded,
                nref.lab="Reference Age",ref.lab="Predicted Age")

plot(bias1)

pdf(file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Rounding conversation SIDT/pollock-rounding/Output/bland_altman_nearest_integer.pdf", height = 5, width = 5.5)
plot(bias1)
dev.off()

ap1 <- agePrecision(reference~nearest_int,data=df1_rounded)

## Make table df
table_df1 <- data.frame(ASD = ap1$ASD, ACV = ap1$ACV, APE = ap1$APE, PercAgree = ap1$PercAgree)

## format values properly
table_df1$ASD <- round(table_df1$ASD, digits = 3)
table_df1$ACV <- round(table_df1$ACV, digits = 3)
table_df1$APE <- round(table_df1$APE, digits = 3)
table_df1$PercAgree <- round(table_df1$PercAgree, digits = 3)

## plot table using flextable package
ft1 <- flextable(table_df1) %>% 
  align(align = "center", part = "all") %>% 
  line_spacing(space = 2, part = "all") %>% 
  padding(padding = 6, part = "header")

ft_raster1 <- flextable::as_raster(ft1)

gflextable1 <- ggplot() + 
  theme_void() + 
  annotation_custom(rasterGrob(ft_raster1), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

pdf(file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Rounding conversation SIDT/pollock-rounding/Output/precision_metrics_nearest_integer.pdf", height = 1, width = 5.5)
gflextable1
dev.off()

##################
# Round down - for survey samples, more closely aligned with traditional age reading  protocol
bias2 <- ageBias(reference~round_down,data=df1_rounded,
                 nref.lab="Reference Age",ref.lab="Predicted Age")

plot(bias2)

pdf(file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Rounding conversation SIDT/pollock-rounding/Output/bland_altman_round_down.pdf", height = 5, width = 5.5)
plot(bias2)
dev.off()

ap2 <- agePrecision(reference~round_down,data=df1_rounded)

## Make table df
table_df2 <- data.frame(ASD = ap2$ASD, ACV = ap2$ACV, APE = ap2$APE, PercAgree = ap2$PercAgree)

## format values properly
table_df2$ASD <- round(table_df2$ASD, digits = 3)
table_df2$ACV <- round(table_df2$ACV, digits = 3)
table_df2$APE <- round(table_df2$APE, digits = 3)
table_df2$PercAgree <- round(table_df2$PercAgree, digits = 3)

## plot table using flextable package
ft2 <- flextable(table_df2) %>% 
  align(align = "center", part = "all") %>% 
  line_spacing(space = 2, part = "all") %>% 
  padding(padding = 6, part = "header")

ft_raster2 <- flextable::as_raster(ft2)

gflextable2 <- ggplot() + 
  theme_void() + 
  annotation_custom(rasterGrob(ft_raster2), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

pdf(file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Rounding conversation SIDT/pollock-rounding/Output/precision_metrics_round_down.pdf", height = 1, width = 5.5)
gflextable2
dev.off()

##################
# Round up - just to see
bias3 <- ageBias(reference~round_up,data=df1_rounded,
                 nref.lab="Reference Age",ref.lab="Predicted Age")

plot(bias3)

pdf(file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Rounding conversation SIDT/pollock-rounding/Output/bland_altman_round_up.pdf", height = 5, width = 5.5)
plot(bias3)
dev.off()

ap3 <- agePrecision(reference~round_up,data=df1_rounded)

## Make table df
table_df3 <- data.frame(ASD = ap3$ASD, ACV = ap3$ACV, APE = ap3$APE, PercAgree = ap3$PercAgree)

## format values properly
table_df3$ASD <- round(table_df3$ASD, digits = 3)
table_df3$ACV <- round(table_df3$ACV, digits = 3)
table_df3$APE <- round(table_df3$APE, digits = 3)
table_df3$PercAgree <- round(table_df3$PercAgree, digits = 3)

## plot table using flextable package
ft3 <- flextable(table_df3) %>% 
  align(align = "center", part = "all") %>% 
  line_spacing(space = 2, part = "all") %>% 
  padding(padding = 6, part = "header")

ft_raster3 <- flextable::as_raster(ft3)

gflextable3 <- ggplot() + 
  theme_void() + 
  annotation_custom(rasterGrob(ft_raster3), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)

pdf(file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Rounding conversation SIDT/pollock-rounding/Output/precision_metrics_round_up.pdf", height = 1, width = 5.5)
gflextable3
dev.off()
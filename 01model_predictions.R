# Packages
library(pls)
library(mdatools)
library(tidyverse)
library(prospectr)
library(RColorBrewer)
library(gridExtra)
library(flextable)
library(plotly)
library(FSA)

# Load data
load("Data/dat_sg.rda") #from Walleye Pollock, explore-pollock-spectra-offset script. SG preprocessing, PCA for outlier removal.

# Filter all by spectra collected between 6/2021 and 5/2022
## Filter out all of 2022, and 2021 if month >= 6
dat_sg_rm <- dat_sg %>%
  filter(scan_year == "2021" & scan_month %in% c(6,7,8,9,10,11,12))

dat_sg_filt <- anti_join(dat_sg, dat_sg_rm)

dat_sg_rm <- dat_sg_filt%>%
  filter(instrument_name == "MPAII_AFSC" & scan_year == "2022" & scan_month %in% c(1,2,3,4,5,6))

dat_sg_filt <- anti_join(dat_sg_filt, dat_sg_rm)

summary(dat_sg_filt$scan_year)

#Clean up
names(dat_sg_filt) <- sub('X','', names(dat_sg_filt)) #remove X in front of wavenumbers

dat_sg_rm <- dat_sg_filt[is.na(dat_sg_filt$final_age),] #any rows that don't have a final age (n = 7)

dat_sg_filt <- anti_join(dat_sg_filt, dat_sg_rm) #filter out rows with no final_age

# Look at spectra
## First need to pivot_longer to get column of absorbance
sg_dat_L<-dat_sg_filt%>%tidyr::pivot_longer(.,cols=c(42:ncol(dat_sg_filt)),names_to="wavenumber",values_to="absorbance", names_prefix = "X") 

sg_dat_L$wavenumber <- as.numeric(sg_dat_L$wavenumber)

# SG spectra plot
selected_wn <- vip[which(vip > .5),]
names(selected_wn) <- sub('X','', names(selected_wn)) #remove X in front of wavenumbers
selected_wn <- as.numeric(names(selected_wn))

`%notin%` <- Negate(`%in%`)

wn_dat_plot <- sg_dat_L[which(sg_dat_L$wavenumber %notin% selected_wn),]

sp <- ggplot(sg_dat_L[seq(1,12696138, by=30),])+
  geom_line(aes(x = wavenumber, y = absorbance, group = filename, color = as.factor(collection_year)))+
  scale_x_reverse()+
  theme_classic()+
  # guides(color = "none")+
  geom_vline(xintercept = wn_dat_plot$wavenumber, color = alpha("grey", 0.2))
#facet_wrap(~collection_year, ncol = 4)

ggplotly(sp)

# PCA
m1 <- pca(dat_sg_filt[,42:length(dat_sg_filt)], ncomp = 7, scale= F, center = T)

# Plot PC1 vs. PC2
s <- ggplot(dat_sg_filt, aes(label = filename))+
  geom_point(aes(m1$res$cal$scores[,1], m1$res$cal$scores[,2], color = as.factor(collection_year)))+
  #stat_ellipse(aes(m1$res$cal$scores[,1], m1$res$cal$scores[,2], color = Year))+
  labs(title="Scores",
       caption="Source: walleye pollock 2014-2021",
       x="Comp 1",
       y="Comp 2",
       color = "Year")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50")+
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50")+
  theme(panel.border = element_rect(fill = NA, color = "grey50"), panel.background = element_blank(), legend.key = element_rect(fill = "white"))

ggplotly(s)

# PC 2 and PC 3
s2 <- ggplot(dat_sg_filt, aes(label = filename))+
  geom_point(aes(m1$res$cal$scores[,2], m1$res$cal$scores[,3], color = as.factor(collection_year)))+
  #stat_ellipse(aes(m1$res$cal$scores[,1], m1$res$cal$scores[,2], color = Year))+
  labs(title="Scores",
       caption="Source: walleye pollocl 2014-2021",
       x="Comp 2",
       y="Comp 3",
       color = "Year")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50")+
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50")+
  theme(panel.border = element_rect(fill = NA, color = "grey50"), panel.background = element_blank(), legend.key = element_rect(fill = "white"))

ggplotly(s2)

# PC 3 and PC 4
s3 <- ggplot(dat_sg_filt, aes(label = filename))+
  geom_point(aes(m1$res$cal$scores[,3], m1$res$cal$scores[,4], color = as.factor(collection_year)))+
  #stat_ellipse(aes(m1$res$cal$scores[,1], m1$res$cal$scores[,2], color = Year))+
  labs(title="Scores",
       caption="Source: walleye pollocl 2014-2021",
       x="Comp 3",
       y="Comp 4",
       color = "Year")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50")+
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50")+
  theme(panel.border = element_rect(fill = NA, color = "grey50"), panel.background = element_blank(), legend.key = element_rect(fill = "white"))

ggplotly(s3)

# PC 4 and PC 6
s4 <- ggplot(dat_sg_filt, aes(label = filename))+
  geom_point(aes(m1$res$cal$scores[,4], m1$res$cal$scores[,6], color = as.factor(collection_year)))+
  #stat_ellipse(aes(m1$res$cal$scores[,1], m1$res$cal$scores[,2], color = Year))+
  labs(title="Scores",
       caption="Source: walleye pollock 2014-2021",
       x="Comp 4 (0.14%)",
       y="Comp 6 (0.08%)",
       color = "Year")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50")+
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50")+
  theme(panel.border = element_rect(fill = NA, color = "grey50"), panel.background = element_blank(), legend.key = element_rect(fill = "white"))

ggplotly(s4)

# Look at loadings 
plotLoadings(m1, 4:6, type = "l")

wavenumber_vec <- unique(sg_dat_L$wavenumber)

ggplot()+
  geom_line(aes(x = wavenumber_vec, y = m1$loadings[,1]), color = "blue")+
  geom_line(aes(x = wavenumber_vec, y = m1$loadings[,2]), color = "red")+
  labs(x = "wavenumber", y = "loadings")+
  scale_x_reverse()+
  theme_classic()


# PLS for outlier removal
# PLS
m1 = mdatools::pls(dat_sg[,c(42:946)], final_age, 10, scale = F, center = T, cv = 20)

# Plot Residuals
plotXYResiduals(m1, show.labels = TRUE, labels = "indices")

# Remove outlier
c <- categorize(m1)
index_outliers <- as.numeric(which(c == "outlier"))

dat_filt <- dat_2014[-c(index_outliers),]

# PLS
m1 = mdatools::pls(dat_filt[,c(42:946)], dat_filt$final_age, 10, scale = F, center = T, cv = 20)

# Plot Residuals
plotXYResiduals(m1, show.labels = TRUE, labels = "indices")

# Remove outlier
c <- categorize(m1)
index_outliers <- as.numeric(which(c == "outlier"))

dat_filt <- dat_filt[-c(index_outliers),]

# PLS
m1 = mdatools::pls(dat_filt[,c(42:946)], dat_filt$final_age, 10, scale = F, center = T, cv = 20)

# Plot Residuals
plotXYResiduals(m1, show.labels = TRUE, labels = "indices")

# Remove outlier
c <- categorize(m1)
index_outliers <- as.numeric(which(c == "outlier"))

dat_filt <- dat_filt[-c(index_outliers),]

# PLS
m1 = mdatools::pls(dat_filt[,c(42:946)], dat_filt$final_age, 10, scale = F, center = T, cv = 20)

# Plot Residuals
plotXYResiduals(m1, show.labels = TRUE, labels = "indices")

# Remove outlier
c <- categorize(m1)
index_outliers <- as.numeric(which(c == "outlier"))

dat_filt <- dat_filt[-c(index_outliers),]

# PLS
m1 = mdatools::pls(dat_filt[,c(42:946)], dat_filt$final_age, 10, scale = F, center = T, cv = 20)

# Plot Residuals
plotXYResiduals(m1, show.labels = TRUE, labels = "indices")
## No more!

# Date frame of outliers to double check in model
outliers <- anti_join(dat_sg, dat_filt)

# PLS
m2 = mdatools::pls(dat_filt[,c(42:946)], dat_filt$final_age, 10, x.test = outliers[,c(42:946)], y.test = outliers$final_age, scale = F, center = T, cv = 20)

# Plot Residuals
plotXYResiduals(m2, show.labels = TRUE, labels = "indices")

# Outliers to add back to data
c <- categorize(m2, m2$testres)
index_outliers <- as.numeric(which(c == "outlier"))

outliers_2_add_back <- outliers[-c(index_outliers),]

# Rejoin outliers to 2014
dat_fin <- data.frame(rbind(dat_filt, outliers_2_add_back))

# function for rsq
rsq <- function (x, y) cor(x, y) ^ 2


### Optimize

# VIP Scores
x_1 <- mdatools::pls(dat_fin[, c(42:946)], dat_fin$final_age,  x.test = dat_fin[, c(42:946)], y.test = dat_fin$final_age, scale = F, center = T, ncomp = 10, cv = 20) #when I have more time to run this, change to ncomp = 5 and cv = 1

comp <- x_1$cvres$ncomp.selected #allow each to use optimal ncomp
vip <- vipscores(x_1, ncomp = comp)

# Optimized model
x <- mdatools::pls(dat_fin[, c(42:946)], dat_fin$final_age,  x.test = dat_fin[, c(42:946)], y.test = dat_fin$final_age, scale = F, center = T, ncomp = 10, cv = 20, exclcols = (vip < 0.5)) #when I have more time to run this, change to ncomp = 5 and cv = 1

comp <- x$cvres$ncomp.selected

#Make dataframe
df1 <- cbind.data.frame(reference = x$cvres$y.ref, prediction = x$cvres$y.pred[,comp,1], residual = abs(x$cvres$y.pred[,comp,1] - x$cvres$y.ref), category = as.vector(categorize(x, x$testres)), z = x$calres$ydecomp$Q[, comp], f = attr(x$calres$xdecomp$T2, "Nu")[[comp]] * x$calres$xdecomp$T2[, comp] / attr(x$calres$xdecomp$T2, "u0")[[comp]] + attr(x$calres$xdecomp$Q, "Nu")[[comp]] * x$calres$xdecomp$Q[, comp] / attr(x$calres$xdecomp$Q, "u0")[[comp]] , Z_scale = x$calres$ydecomp$Q[, comp]/attr(x$calres$ydecomp$Q, "u0")[[comp]], f_scale = (attr(x$calres$xdecomp$T2, "Nu")[[comp]] * x$calres$xdecomp$T2[, comp] / attr(x$calres$xdecomp$T2, "u0")[[comp]] + attr(x$calres$xdecomp$Q, "Nu")[[comp]] * x$calres$xdecomp$Q[, comp] / attr(x$calres$xdecomp$Q, "u0")[[comp]])/(attr(x$calres$xdecomp$T2, "Nu")[[comp]] + attr(x$calres$xdecomp$Q, "Nu")[[comp]]))

df2 <- pls.getLimitsCoordinates(Qlim = x$Qlim, T2lim = x$T2lim, x$Zlim, ncomp = comp, norm = TRUE, nobj = x$limParams$Q$moments$nobj, log = FALSE)
#use x to select preds from matrix

## How to plot

dat_plot <- df1 %>% 
  group_by(reference) %>%
  mutate(rounded_pred = round(prediction), avg_prediction = mean(rounded_pred), count_ref = n())

dat_RMSEP <- sqrt(sum(dat_plot$residual^2)/length(dat_plot$residual))

dat_r2 <- rsq(dat_2014_plot$reference, dat_plot$rounded_pred)

dat_dist <- dat_plot %>%
  ungroup()%>%
  summarise(extreme = length(category[which(category == "extreme")])/length(category), outlier = length(category[which(category == "outlier")])/length(category))

# # Extra just in case want to try to plot Q and T2 with limits
# dat_2015_lim <- bind_rows(lapply(preds_list_2015,"[[", 2), .id = "column_label")
Iter <- 1
cols <- brewer.pal(8, "BuPu")

p0 <- ggplot(dat_plot)+
  geom_bin2d(aes(reference, rounded_pred), binwidth = c(1, 1))+
  scale_fill_gradientn(colors = cols, trans = "log", breaks = c(1,20,400,8000), labels = c(1,20,400,8000))+
  #geom_line(aes(reference, avg_prediction, color = iteration), size = 1.5)+
  scale_color_manual(values = rep("grey15", Iter), guide = "none")+
  geom_abline(intercept = 0, slope = 1, linetype = 2)+
  labs(title = "2014-2021", y = "predicted age (years)", x = "reference age (years)", fill = "log(counts)")+
  scale_y_continuous(limits = c(-1,20), breaks = c(0,5,10,15,20), expand = c(0,0))+
  scale_x_continuous(limits = c(-1,25), breaks = c(0,5,10,15,20), expand = c(0,0))+
  annotate(geom = 'text', label = paste0("r^2==", round(dat_2014_r2, digits = 3)), x = 0, y = 19, hjust = "left", parse = TRUE, size = 2.5)+
  annotate("text", x=0, y = 18, label = paste0("RMSECV==", round(dat_2014_RMSEP, digits = 3)), hjust = "left", parse = TRUE, size = 2.5)+
  annotate("text", x=0, y = 16, label = paste0("extreme=", round(dat_2014_dist$extreme, digits = 3)*100, "%"), hjust = "left", parse = F, size = 2.5)+
  annotate("text", x=0, y = 15, label = paste0("outlier=", round(dat_2014_dist$outlier, digits = 3)*100, "%"), hjust = "left", parse = F, size = 2.5)+
  theme_classic()

p0

ggplot()+
  geom_point(data = df1, aes(f_scale, Z_scale, color = category)) + #this is uggo but don't feel like making a separate vector right now
  geom_line(data = data.frame(df2_14$extremes), aes(fE, zE), color = "blue")+
  geom_line(data = data.frame(df2_14$outliers), aes(fO, zO), color = "red")+
  labs(x = "Full X-distance, f/f0",
       y = "Y-distance, z/z0")+
  theme_classic()
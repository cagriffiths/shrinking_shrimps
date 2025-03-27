###
# code and data to recreate the figures in Griffiths et al. 2025
# Shrinking shrimp - investigating the weight loss of northern shrimp Pandalus borealis following boiling
# code is only provided for Figures 3 and 4, and Figures S1 and S2
# Figures 1 and 2 in the main MS were made by a colleague in GIS and Powerpoint, respectively

### 
# packages
require(tidyverse)
require(viridis)
require(readxl)
require(patchwork)

###
# load data
data = read_excel("data/Weight loss data.xlsx", sheet = 1)
head(data)

###
# manipulate data for plotting
dat = data.frame(matrix(NA, ncol=9, nrow=NROW(data)*3))
colnames(dat) = c('trip', 'quarter', 'year', 'percent_change', 'test', 'start_temperature', 'temperature_change', 'salinity', 'boiling_duration')

dat$trip = rep(data$trip,3)
dat$quarter = rep(data$quarter,3)
dat$year = rep(data$year,3)
dat$percent_change[1:23] = as.numeric(data$delta_boiled)
dat$percent_change[24:46] = as.numeric(data$delta_boiled_to_packed)
dat$percent_change[47:69] = as.numeric(data$delta_packed)
dat$start_temperature = rep(data$boiling_start_temperature,3)
dat$temperature_change = rep(data$delta_boiling_temperature,3)
dat$salinity = rep(data$water_salinity,3)
dat$boiling_duration = rep(data$boiling_duration,3)

dat$test[1:23] = 'Capture to boiled'
dat$test[24:46] = 'Boiled to packing'
dat$test[47:69] = 'Capture to packing'

dat$sample_size[1:23] = 'n = 21'
dat$sample_size[24:46] = 'n = 21'
dat$sample_size[47:69] = 'n = 23'

###
# Figure 3
dat$test = factor(dat$test, levels = c("Capture to boiled", "Boiled to packing", "Capture to packing")) 

fig3 = ggplot(data = dat, aes(x = test, y = percent_change, fill=as.factor(year),group=as.factor(test)))+
  geom_boxplot(color="grey40", outlier.shape = NA, alpha=0.4, staplewidth = 0.5, fill = 'grey50')+
  geom_point(shape=21,position=position_jitterdodge(), size = 4)+
  scale_fill_manual(values=c("#7F3C8D","#11A579","#3969AC"), name = 'Year:')+
  geom_hline(yintercept = 0, color = 'grey40', linetype = 'dashed', linewidth = 1.2)+
  theme_bw()+
  xlab('')+
  ylab('Weight loss (% decrease)')+
  theme(legend.position = 'top', text = element_text(size = 18))+
  scale_y_continuous(breaks = seq(-4,16,2), limits=c(-4,16))+
  geom_label(stat = "identity", aes(label = sample_size), y = -4, vjust = 0, fill = "white", nudge_y = 0.1, hjust=0.5)

###
# Figure 4
fig4 = ggplot(data = dat, aes(x = quarter, y = percent_change, fill=as.factor(year),group=as.factor(quarter)))+
  geom_boxplot(color="grey40", outlier.shape = NA, alpha=0.4, staplewidth = 0.5, fill = 'grey50')+
  geom_point(shape=21,position=position_jitterdodge(), size = 4)+
  scale_fill_manual(values=c("#7F3C8D","#11A579","#3969AC"), name = 'Year:')+
  geom_hline(yintercept = 0, color = 'grey40', linetype = 'dashed', linewidth = 1.2)+
  theme_bw()+
  xlab('Quarter')+
  ylab('Weight loss (% decrease)')+
  theme(legend.position = 'top', text = element_text(size = 16))+
  scale_y_continuous(breaks = seq(-4,16,2), limits=c(-4,16))+
  facet_wrap(~test)

###
# Figure S1
figS1 = ggplot(data = dat, aes(x = as.factor(year), y = percent_change, fill=as.factor(quarter),group=as.factor(year)))+
  geom_boxplot(color="grey40", outlier.shape = NA, alpha=0.4, staplewidth = 0.5, fill = 'grey50')+
  geom_point(shape=21,position=position_jitterdodge(), size = 4)+
  scale_fill_viridis(option = 'viridis', discrete=T, name = 'Quarter:')+
  geom_hline(yintercept = 0, color = 'grey40', linetype = 'dashed', linewidth = 1.2)+
  theme_bw()+
  xlab('Year')+
  ylab('Weight loss (% decrease)')+
  theme(legend.position = 'top', text = element_text(size = 16))+
  scale_y_continuous(breaks = seq(-4,16,2), limits=c(-4,16))+
  facet_wrap(~test)

###
# Figure S2
# build correlation plots by comparison

# Capture to Boiling
B = filter(dat, test %in% c('Capture to boiled'))

b1 = ggplot(data = B, aes(x = start_temperature,y = percent_change))+
  geom_point(shape=21, color='grey20',fill = 'grey80', size = 4, alpha = 0.6)+
  geom_smooth(method = 'lm', se = F)+
  theme_bw()+
  xlab('Temperature')+
  ylab('Weight loss')+
  theme(legend.position = '', text = element_text(size = 14))

b2 = ggplot(data = B, aes(x = temperature_change,y = percent_change))+
  geom_point(shape=21, color='grey20',fill = 'grey80', size = 4, alpha = 0.6)+
  geom_smooth(method = 'lm', se = F)+
  theme_bw()+
  xlab('Temperature change')+
  ylab('Weight loss')+
  theme(legend.position = '', text = element_text(size = 14))

b3 = ggplot(data = B, aes(x = salinity,y = percent_change))+
  geom_point(shape=21, color='grey20',fill = 'grey80', size = 4, alpha = 0.6)+
  geom_smooth(method = 'lm', se = F)+
  theme_bw()+
  xlab('Salinity')+
  ylab('Weight loss')+
  theme(legend.position = '', text = element_text(size = 14))

b4 = ggplot(data = B, aes(x = boiling_duration,y = percent_change))+
  geom_point(shape=21, color='grey20',fill = 'grey80', size = 4, alpha = 0.6)+
  geom_smooth(method = 'lm', se = F)+
  theme_bw()+
  xlab('Boiling duration')+
  ylab('Weight loss')+
  theme(legend.position = '', text = element_text(size = 14))

b = b1/b2/b3/b4 + plot_layout(guides = 'collect')

# Boiling to packing
P = filter(dat, test %in% c('Boiled to packing'))

p1 = ggplot(data = P, aes(x = start_temperature,y = percent_change))+
  geom_point(shape=21, color='grey20',fill = 'black', size = 4, alpha = 0.6)+
  geom_smooth(method = 'lm', se = F)+
  theme_bw()+
  xlab('Temperature')+
  ylab('Weight loss')+
  theme(legend.position = '', text = element_text(size = 14))

p2 = ggplot(data = P, aes(x = temperature_change,y = percent_change))+
  geom_point(shape=21, color='grey20',fill = 'black', size = 4, alpha = 0.6)+
  geom_smooth(method = 'lm', se = F)+
  theme_bw()+
  xlab('Temperature change')+
  ylab('Weight loss')+
  theme(legend.position = '', text = element_text(size = 14))

p3 = ggplot(data = P, aes(x = salinity,y = percent_change))+
  geom_point(shape=21, color='grey20',fill = 'black', size = 4, alpha = 0.6)+
  geom_smooth(method = 'lm', se = F)+
  theme_bw()+
  xlab('Salinity')+
  ylab('Weight loss')+
  theme(legend.position = '', text = element_text(size = 14))

p4 = ggplot(data = P, aes(x = boiling_duration,y = percent_change))+
  geom_point(shape=21, color='grey20',fill = 'black', size = 4, alpha = 0.6)+
  geom_smooth(method = 'lm', se = F)+
  theme_bw()+
  xlab('Boiling duration')+
  ylab('Weight loss')+
  theme(legend.position = '', text = element_text(size = 14))

p = p1/p2/p3/p4 + plot_layout(guides = 'collect')

# Capture to packing
C = filter(dat, test %in% c('Capture to packing'))

c1 = ggplot(data = C, aes(x = start_temperature,y = percent_change))+
  geom_point(shape=21, color='grey20',fill = 'grey30', size = 4, alpha = 0.6)+
  geom_smooth(method = 'lm', se = F)+
  theme_bw()+
  xlab('Temperature')+
  ylab('Weight loss')+
  theme(legend.position = '', text = element_text(size = 14))

c2 = ggplot(data = C, aes(x = temperature_change,y = percent_change))+
  geom_point(shape=21, color='grey20',fill = 'grey30', size = 4, alpha = 0.6)+
  geom_smooth(method = 'lm', se = F)+
  theme_bw()+
  xlab('Temperature change')+
  ylab('Weight loss')+
  theme(legend.position = '', text = element_text(size = 14))

c3 = ggplot(data = C, aes(x = salinity,y = percent_change))+
  geom_point(shape=21, color='grey20',fill = 'grey30', size = 4, alpha = 0.6)+
  geom_smooth(method = 'lm', se = F)+
  theme_bw()+
  xlab('Salinity')+
  ylab('Weight loss')+
  theme(legend.position = '', text = element_text(size = 14))

c4 = ggplot(data = C, aes(x = boiling_duration,y = percent_change))+
  geom_point(shape=21, color='grey20',fill = 'grey30', size = 4, alpha = 0.6)+
  geom_smooth(method = 'lm', se = F)+
  theme_bw()+
  xlab('Boiling duration')+
  ylab('Weight loss')+
  theme(legend.position = '', text = element_text(size = 14))

c = c1/c2/c3/c4 + plot_layout(guides = 'collect')

# merge using patchwork
figS2 = b|p|c + plot_layout(guides = 'collect')

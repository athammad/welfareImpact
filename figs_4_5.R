library(sf)
library(raster)
library(tidyverse)
library("rnaturalearth")
library("rnaturalearthdata")

listone <- FBS

# fig 4

est <- do.call(rbind, list(multiPSdry.Rwi$coefficients,multiPSdry.Awi$coefficients,multiPSdryExtra.Rwi$coefficients,multiPSdryExtra.Awi$coefficients,multiPSwet.Rwi$coefficients,multiPSwet.Awi$coefficients,multiPSwetExtra.Rwi$coefficients,multiPSwetExtra.Awi$coefficients))

est <- as.data.frame(est)
est$`(Intercept)`<- NULL
est$Type <-  c(rep(c("Dry"), 4), rep(c("Wet"), 4))
est$Extreme <-  rep(c(rep(c("Severe event"), 2), rep(c("Extreme event"), 2)), 2)
est <- reshape2::melt(est, 5:6)
est$Period <- c(rep("1901-1928", 8), rep("1929-1958", 8), rep("1959 - 1988", 8), rep("1989 - 2018", 8))
est$Estimate <- "Estimate"
est$Outcome <-  rep(c(rep(c("RWI"), 1), rep(c("AWE"), 1)), 4)
est$variable<- NULL

lower <- do.call(rbind, list((multiPSdry.Rwi$coefficients - (1.96 * as.vector(summary(multiPSdry.Rwi)$coefficients[,2]))), (multiPSdry.Awi$coefficients - (1.96 * as.vector(summary(multiPSdry.Awi)$coefficients[,2]))), (multiPSdryExtra.Rwi$coefficients - (1.96 * as.vector(summary(multiPSdryExtra.Rwi)$coefficients[,2]))), (multiPSdryExtra.Awi$coefficients - (1.96 * as.vector(summary(multiPSdryExtra.Awi)$coefficients[,2]))), (multiPSwet.Rwi$coefficients - (1.96 * as.vector(summary(multiPSwet.Rwi)$coefficients[,2]))), (multiPSwet.Awi$coefficients - (1.96 * as.vector(summary(multiPSwet.Awi)$coefficients[,2]))), (multiPSwetExtra.Rwi$coefficients - (1.96 * as.vector(summary(multiPSwetExtra.Rwi)$coefficients[,2]))), (multiPSwetExtra.Awi$coefficients - (1.96 * as.vector(summary(multiPSwetExtra.Awi)$coefficients[,2])))))

lower <- as.data.frame(lower)
lower$`(Intercept)`<- NULL
lower$Type <-  c(rep(c("Dry"), 4), rep(c("Wet"), 4))
lower$Extreme <-  rep(c(rep(c("Severe event"), 2), rep(c("Extreme event"), 2)), 2)
lower <- reshape2::melt(lower, 5:6)
lower$Period <- c(rep("1901-1928", 8), rep("1929-1958", 8), rep("1959 - 1988", 8), rep("1989 - 2018", 8))
lower$estimate <- "Lower"
lower$Outcome <-  rep(c(rep(c("RWI"), 1), rep(c("AWE"), 1)), 4)
lower$variable<- NULL

upper <- do.call(rbind, list((multiPSdry.Rwi$coefficients + (1.96 * as.vector(summary(multiPSdry.Rwi)$coefficients[,2]))), (multiPSdry.Awi$coefficients + (1.96 * as.vector(summary(multiPSdry.Awi)$coefficients[,2]))), (multiPSdryExtra.Rwi$coefficients + (1.96 * as.vector(summary(multiPSdryExtra.Rwi)$coefficients[,2]))), (multiPSdryExtra.Awi$coefficients + (1.96 * as.vector(summary(multiPSdryExtra.Awi)$coefficients[,2]))), (multiPSwet.Rwi$coefficients + (1.96 * as.vector(summary(multiPSwet.Rwi)$coefficients[,2]))), (multiPSwet.Awi$coefficients + (1.96 * as.vector(summary(multiPSwet.Awi)$coefficients[,2]))), (multiPSwetExtra.Rwi$coefficients + (1.96 * as.vector(summary(multiPSwetExtra.Rwi)$coefficients[,2]))), (multiPSwetExtra.Awi$coefficients + (1.96 * as.vector(summary(multiPSwetExtra.Awi)$coefficients[,2])))))

upper <- as.data.frame(upper)
upper$`(Intercept)`<- NULL
upper$Type <-  c(rep(c("Dry"), 4), rep(c("Wet"), 4))
upper$Extreme <-  rep(c(rep(c("Severe event"), 2), rep(c("Extreme event"), 2)), 2)
upper <- reshape2::melt(upper, 5:6)
upper$Period <- c(rep("1901-1928", 8), rep("1929-1958", 8), rep("1959 - 1988", 8), rep("1989 - 2018", 8))
upper$estimate <- "Upper"
upper$Outcome <-  rep(c(rep(c("RWI"), 1), rep(c("AWE"), 1)), 4)
upper$variable<- NULL

coeffs <- data.frame(est, upper$value, lower$value)

#

est_one <- do.call(rbind, list(onePSdry.Rwi$coefficients,onePSdry.Awi$coefficients,onePSdryExtra.Rwi$coefficients,onePSdryExtra.Awi$coefficients,onePSwet.Rwi$coefficients,onePSwet.Awi$coefficients,onePSwetExtra.Rwi$coefficients,onePSwetExtra.Awi$coefficients))

est_one <- as.data.frame(est_one)
est_one$`(Intercept)`<- NULL
est_one$Type <-  c(rep(c("Dry"), 4), rep(c("Wet"), 4))
est_one$Extreme <-  rep(c(rep(c("Severe event"), 2), rep(c("Extreme event"), 2)), 2)
est_one <- reshape2::melt(est_one, 2:3)
est_one$estimate <- "Estimate"
est_one$Outcome <-  rep(c(rep(c("RWI"), 1), rep(c("AWE"), 1)), 4)
est_one$variable<- NULL

lower_one <- do.call(rbind, list((onePSdry.Rwi$coefficients - (1.96 * as.vector(summary(onePSdry.Rwi)$coefficients[,2]))), (onePSdry.Awi$coefficients - (1.96 * as.vector(summary(onePSdry.Awi)$coefficients[,2]))), (onePSdryExtra.Rwi$coefficients - (1.96 * as.vector(summary(onePSdryExtra.Rwi)$coefficients[,2]))), (onePSdryExtra.Awi$coefficients - (1.96 * as.vector(summary(onePSdryExtra.Awi)$coefficients[,2]))), (onePSwet.Rwi$coefficients - (1.96 * as.vector(summary(onePSwet.Rwi)$coefficients[,2]))), (onePSwet.Awi$coefficients - (1.96 * as.vector(summary(onePSwet.Awi)$coefficients[,2]))), (onePSwetExtra.Rwi$coefficients - (1.96 * as.vector(summary(onePSwetExtra.Rwi)$coefficients[,2]))), (onePSwetExtra.Awi$coefficients - (1.96 * as.vector(summary(onePSwetExtra.Awi)$coefficients[,2])))))

lower_one <- as.data.frame(lower_one)
lower_one$`(Intercept)`<- NULL
lower_one$Type <-  c(rep(c("Dry"), 4), rep(c("Wet"), 4))
lower_one$Extreme <-  rep(c(rep(c("Severe event"), 2), rep(c("Extreme event"), 2)), 2)
lower_one <- reshape2::melt(lower_one, 2:3)
lower_one$estimate <- "Lower"
lower_one$Outcome <-  rep(c(rep(c("RWI"), 1), rep(c("AWE"), 1)), 4)
lower_one$variable<- NULL

upper_one <- do.call(rbind, list((onePSdry.Rwi$coefficients + (1.96 * as.vector(summary(onePSdry.Rwi)$coefficients[,2]))), (onePSdry.Awi$coefficients + (1.96 * as.vector(summary(onePSdry.Awi)$coefficients[,2]))), (onePSdryExtra.Rwi$coefficients + (1.96 * as.vector(summary(onePSdryExtra.Rwi)$coefficients[,2]))), (onePSdryExtra.Awi$coefficients + (1.96 * as.vector(summary(onePSdryExtra.Awi)$coefficients[,2]))), (onePSwet.Rwi$coefficients + (1.96 * as.vector(summary(onePSwet.Rwi)$coefficients[,2]))), (onePSwet.Awi$coefficients + (1.96 * as.vector(summary(onePSwet.Awi)$coefficients[,2]))), (onePSwetExtra.Rwi$coefficients + (1.96 * as.vector(summary(onePSwetExtra.Rwi)$coefficients[,2]))), (onePSwetExtra.Awi$coefficients + (1.96 * as.vector(summary(onePSwetExtra.Awi)$coefficients[,2])))))

upper_one <- as.data.frame(upper_one)
upper_one$`(Intercept)`<- NULL
upper_one$Type <-  c(rep(c("Dry"), 4), rep(c("Wet"), 4))
upper_one$Extreme <-  rep(c(rep(c("Severe event"), 2), rep(c("Extreme event"), 2)), 2)
upper_one <- reshape2::melt(upper_one, 2:3)
upper_one$estimate <- "upper_one"
upper_one$Outcome <-  rep(c(rep(c("RWI"), 1), rep(c("AWE"), 1)), 4)
upper_one$variable<- NULL

coeffs_one <- data.frame(est_one, upper_one$value, lower_one$value)

#


a <- ggplot() + geom_line(data=coeffs %>% filter(Outcome=="AWE"), aes(Period, value, colour = interaction(Type, Extreme), group = interaction(Type, Extreme))) +   theme_classic() +
  geom_ribbon(data=coeffs %>% filter(Outcome=="AWE"), aes(x=Period, y=value, ymin = lower.value, ymax = upper.value, fill = interaction(Type, Extreme), group = interaction(Type, Extreme)), alpha = 0.15)+
  geom_hline(yintercept = 0)+
  ylab("Time-heterogeneous marginal effect of an hydrological event \non the AWE (local  PPP per-capita GDP, 2011 USD)")+
  scale_colour_manual(name="", values = c("darkblue", "darkred", "turquoise", "firebrick1"), labels=c("Dry extreme event", "Wet extreme event", "Dry severe event", "Wet severe event"))+
  scale_fill_manual(name="", values = c("darkblue", "darkred", "turquoise", "firebrick1"), labels=c("Dry extreme event", "Wet extreme event", "Dry severe event", "Wet severe event"))+theme(legend.position = "bottom", legend.direction = "horizontal")

b <- ggplot() + geom_pointrange(data=coeffs_one %>% filter(Outcome=="AWE"), aes(y=value, ymin=lower_one.value, ymax=upper_one.value, colour = interaction(Type, Extreme), x = interaction(Type, Extreme)))+   theme_classic() +   scale_colour_manual(name="", values = c("darkblue", "darkred", "turquoise", "firebrick1"), labels=c("Dry extreme event", "Wet extreme event", "Dry severe event", "Wet severe event"))+ geom_hline(yintercept = 0) + scale_x_discrete(labels=c("Dry extreme event", "Wet extreme event", "Dry severe event", "Wet severe event"))+ylab("Average marginal effect of an hydrological event \non the AWE(local  PPP per-capita GDP, 2011 USD)")+
  xlab("Event type")


cowplot::plot_grid(cowplot::plot_grid(a+ theme(legend.position = "none"), b + theme(legend.position = "none"), labels="AUTO"), get_legend(a), ncol=1, rel_heights = c(1, 0.1))

ggsave("fig4.png", last_plot(), device="png", height=4, scale=1.4)


#

coeffs <- coeffs %>% filter(Outcome=="AWE")

coeffs$notsig <- ifelse(sign(coeffs$upper.value) != sign(coeffs$lower.value), 1, 0)

coeffs$value <- ifelse(coeffs$notsig==1, 0, coeffs$value )

listone$impact_droughts <- listone$spei_1_28_count_dry * coeffs$value[coeffs$Period=="1901-1928" & coeffs$Type=="Dry" & coeffs$Extreme=="Severe event"] + listone$spei_29_58_count_dry * coeffs$value[coeffs$Period=="1929-1958" & coeffs$Type=="Dry" & coeffs$Extreme=="Severe event"] + listone$spei_59_88_count_dry * coeffs$value[coeffs$Period=="1959 - 1988" & coeffs$Type=="Dry" & coeffs$Extreme=="Severe event"] + listone$spei_89_18_count_dry * coeffs$value[coeffs$Period=="1989 - 2018" & coeffs$Type=="Dry" & coeffs$Extreme=="Severe event"]

listone$impact_floods <- listone$spei_1_28_count_wet * coeffs$value[coeffs$Period=="1901-1928" & coeffs$Type=="Wet" & coeffs$Extreme=="Severe event"] + listone$spei_29_58_count_wet * coeffs$value[coeffs$Period=="1929-1958" & coeffs$Type=="Wet" & coeffs$Extreme=="Severe event"] + listone$spei_59_88_count_wet * coeffs$value[coeffs$Period=="1959 - 1988" & coeffs$Type=="Wet" & coeffs$Extreme=="Severe event"] + listone$spei_89_18_count_wet * coeffs$value[coeffs$Period=="1989 - 2018" & coeffs$Type=="Wet" & coeffs$Extreme=="Severe event"]

listone$impact_droughts_extra <- listone$spei_1_28_count_dry * coeffs$value[coeffs$Period=="1901-1928" & coeffs$Type=="Dry" & coeffs$Extreme=="Extreme event"] + listone$spei_29_58_count_dry * coeffs$value[coeffs$Period=="1929-1958" & coeffs$Type=="Dry" & coeffs$Extreme=="Extreme event"] + listone$spei_59_88_count_dry * coeffs$value[coeffs$Period=="1959 - 1988" & coeffs$Type=="Dry" & coeffs$Extreme=="Extreme event"] + listone$spei_89_18_count_dry * coeffs$value[coeffs$Period=="1989 - 2018" & coeffs$Type=="Dry" & coeffs$Extreme=="Extreme event"]

listone$impact_floods_extra <- listone$spei_1_28_count_wet * coeffs$value[coeffs$Period=="1901-1928" & coeffs$Type=="Wet" & coeffs$Extreme=="Extreme event"] + listone$spei_29_58_count_wet * coeffs$value[coeffs$Period=="1929-1958" & coeffs$Type=="Wet" & coeffs$Extreme=="Extreme event"] + listone$spei_59_88_count_wet * coeffs$value[coeffs$Period=="1959 - 1988" & coeffs$Type=="Wet" & coeffs$Extreme=="Extreme event"] + listone$spei_89_18_count_wet * coeffs$value[coeffs$Period=="1989 - 2018" & coeffs$Type=="Wet" & coeffs$Extreme=="Extreme event"]

#

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

a <- ggplot()+
  geom_point(data = listone, 
             aes(x = X, y = Y, colour = (impact_droughts)), size=0.0005) +
  scale_colour_gradient("Cumulative 2011 PPP USD/capita", low = 'yellow', high = 'red',
                        na.value = NA)+
  geom_sf(data=world, fill=NA, size =.3)+
  theme_classic()+
  coord_sf(ylim=c(-35, 36), xlim=c(-20, 55))+
  ggtitle("Welfare change due to droughts \n(1901-2018)")+
  theme(legend.position = "bottom", legend.direction = "horizontal", axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  guides(colour = guide_colourbar(barwidth = 10, barheight = .5))+
  xlab("")+
  ylab("")

b <- ggplot()+
  geom_point(data = listone, 
             aes(x = X, y = Y, colour = -(impact_floods)), size=0.0005) +
  scale_colour_gradient("Cumulative 2011 PPP USD/capita", low = 'turquoise', high = 'darkblue',
                        na.value = NA)+
  geom_sf(data=world, fill=NA, size =.3)+
  theme_classic()+
  coord_sf(ylim=c(-35, 36), xlim=c(-20, 55))+
  ggtitle("Welfare change due to floods \n(1901-2018)")+
  theme(legend.position = "bottom", legend.direction = "horizontal", axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  guides(colour = guide_colourbar(barwidth = 10, barheight = .5))+
  xlab("")+
  ylab("")

c <- ggplot()+
  geom_point(data = listone, 
             aes(x = X, y = Y, colour = -(impact_droughts_extra)), size=0.0005) +
  scale_colour_gradient("Cumulative 2011 PPP USD/capita", low = 'yellow', high = 'red',
                        na.value = NA)+
  geom_sf(data=world, fill=NA, size =.3)+
  theme_classic()+
  coord_sf(ylim=c(-35, 36), xlim=c(-20, 55))+
  ggtitle("Welfare change due to extreme \ndroughts (1901-2018)")+
  theme(legend.position = "bottom", legend.direction = "horizontal", axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  guides(colour = guide_colourbar(barwidth = 10, barheight = .5))+
  xlab("")+
  ylab("")

d <- ggplot()+
  geom_point(data = listone, 
             aes(x = X, y = Y, colour = -(impact_floods_extra)), size=0.0005) +
  scale_colour_gradient("Cumulative 2011 PPP USD/capita", low = 'turquoise', high = 'darkblue',
                        na.value = NA)+
  geom_sf(data=world, fill=NA, size =.3)+
  theme_classic()+
  coord_sf(ylim=c(-35, 36), xlim=c(-20, 55))+
  ggtitle("Welfare change due to extreme \nfloods (1901-2018)")+
  theme(legend.position = "bottom", legend.direction = "horizontal", axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  guides(colour = guide_colourbar(barwidth = 10, barheight = .5))+
  xlab("")+
  ylab("")

library(cowplot)

fig5 <- plot_grid(a, b, c, d, nrow=2, labels="AUTO")

ggsave("fig5.png", fig5, device="png", scale=1.75)
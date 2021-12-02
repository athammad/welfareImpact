rm(list=ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, WeightIt, survey,cobalt,jtools,olsrr,ggplot2,ggpubr,modelsummary, stargazer)


### Setting the WD (TO BE CHECKED)
setwd("./SSA_Data")
### Create folders for images etc (TO BE CHECKED)
output_dir <- file.path(getwd(), "Plots")
if (!dir.exists(output_dir)){
  dir.create(output_dir)
} else {
  print("Dir already exists!")
}

###############################################################################

FBS<-fread("data_080921.csv")
#FBS<-FBS[sample(.N,500)]
FBS$V1<-NULL

#---- NA CHECK ---#
sapply(FBS, function(x) sum(is.na(x)))
anyNA(FBS)

#---- CONVERT VARS---#
colList<-c("diam1","diam2","gold","petroleum","gems")
FBS<-FBS[,(colList):=lapply(.SD, function(x) ifelse(x==0,0,1)),.SDcols=colList]
colList<-c("diam1","diam2","gold","petroleum","gems","agroclimaticzone","iso3c")
FBS<-FBS[,(colList):=lapply(.SD,as.factor),.SDcols=colList]

lapply(FBS, class)
#store and remove iso3c 
#SSAiso<-FBS$iso3c
#FBS$iso3c<-NULL
#store and remove error
#errorFB<-FBS$error
#FBS$error<-NULL

names(FBS)
timeINV<-c("pop", "city_tt", "capdist", "distnearestcountry", "distownborders", 
           "diam1", "diam2", "gold", "petroleum", "gems", "agroclimaticzone", 
           "malariasuitability", "altitude", "urbpix", "SP.URB.TOTL.IN.ZS", 
           "AG.SRF.TOTL.K2", "AG.LND.ARBL.HA.PC", "EG.ELC.ACCS.ZS", "SE.PRM.NENR", 
           "ER.H2O.INTR.PC", "IC.BUS.EASE.XQ", "IC.REG.DURS", "IC.LGL.CRED.XQ", 
           "TX.VAL.FUEL.ZS.UN", "TX.VAL.MMTL.ZS.UN","forest",
           "iso3c")



#timeINV<-names(DATA)[2:28]
################################################################################
#

#                             ## count_dry ####   

#
################################################################################

FormulaListDRY<-list(
  as.formula(
    paste("spei_1_28_count_dry ~ ", 
          paste(c(timeINV,"pre_1_28","temp_1_28","wet_1_28"), collapse= "+"))),
  
  as.formula(paste("spei_29_58_count_dry ~", 
                   paste(c(timeINV,"pre_29_58","temp_29_58","wet_29_58",
                           "spei_1_28_count_dry","pre_1_28","temp_1_28","wet_1_28"), collapse= "+"))),
  
  as.formula(paste("spei_59_88_count_dry ~ ",
                   paste(c(timeINV,"pre_59_88","temp_59_88","wet_59_88",
                           "spei_1_28_count_dry","pre_1_28","temp_1_28","wet_1_28",
                           "spei_29_58_count_dry","pre_29_58","temp_29_58","wet_29_58"), collapse= "+"))),
  
  as.formula(paste("spei_89_18_count_dry ~ ",
                   paste(c(timeINV,"pre_89_18","temp_89_18","wet_89_18",
                           "spei_1_28_count_dry","pre_1_28","temp_1_28","wet_1_28",
                           "spei_29_58_count_dry","pre_29_58","temp_29_58","wet_29_58",
                           "spei_59_88_count_dry","pre_59_88","temp_59_88","wet_59_88"), collapse= "+"))) 
  
)


#check for imbalance of actual data for each time point
# if the treatment is continuous, instead of producing mean differences, bal.tab() 
# will produce correlations between the covariates and the treatment. 
# The corresponding entries in the output will be "Corr.Un", "Corr.Adj", 
# and "R.Threshold" (and accordingly for the balance tally and maximum imbalance tables).


#===========================================================
#NOTE: Molto dellâunbalance iniziale eâ causato dalle timeINV variables
#===========================================================

set.seed(1989) 
psDRY<-weightitMSM(formula.list =FormulaListDRY,
                   data=FBS,
                   method = "ps",verbose = T)
# No matter which method is selected, weightitMSM() estimates separate weights 
# for each time period and then takes the product of the weights for each individual
# psDRY arrive at the final estimated weights.
#psDRY$estimand
gc()


library(survey)
sDesign <- svydesign(~1, weights = psDRY$weights,
                     data = FBS)


onePSdry.Rwi <- svyglm(rwi ~ I(spei_1_28_count_dry +spei_29_58_count_dry+
                                 spei_59_88_count_dry+
                                 spei_89_18_count_dry)
                       , design = sDesign)

multiPSdry.Rwi <- svyglm(rwi ~ spei_1_28_count_dry +spei_29_58_count_dry+
                           spei_59_88_count_dry+
                           spei_89_18_count_dry
                         , design = sDesign)

onePSdry.Awi <- svyglm(awi ~ I(spei_1_28_count_dry +spei_29_58_count_dry+
                                 spei_59_88_count_dry+
                                 spei_89_18_count_dry)
                       , design = sDesign)

multiPSdry.Awi <- svyglm(awi ~ spei_1_28_count_dry +spei_29_58_count_dry+
                           spei_59_88_count_dry+
                           spei_89_18_count_dry
                         , design = sDesign)




################################################################################
#

#                                  ## count_dry_extra ####   

#
################################################################################


FormulaListDRYEXTRA<-list(
  as.formula(
    paste("spei_1_28_count_dry_extra ~ ", 
          paste(c(timeINV,"pre_1_28","temp_1_28","wet_1_28"), collapse= "+"))),
  
  as.formula(paste("spei_29_58_count_dry_extra ~", 
                   paste(c(timeINV,"pre_29_58","temp_29_58","wet_29_58",
                           "spei_1_28_count_dry_extra","pre_1_28","temp_1_28","wet_1_28"), collapse= "+"))),
  
  as.formula(paste("spei_59_88_count_dry_extra ~ ",
                   paste(c(timeINV,"pre_59_88","temp_59_88","wet_59_88",
                           "spei_1_28_count_dry_extra","pre_1_28","temp_1_28","wet_1_28",
                           "spei_29_58_count_dry_extra","pre_29_58","temp_29_58","wet_29_58"), collapse= "+"))),
  
  as.formula(paste("spei_89_18_count_dry_extra ~ ",
                   paste(c(timeINV,"pre_89_18","temp_89_18","wet_89_18",
                           "spei_1_28_count_dry_extra","pre_1_28","temp_1_28","wet_1_28",
                           "spei_29_58_count_dry_extra","pre_29_58","temp_29_58","wet_29_58",
                           "spei_59_88_count_dry_extra","pre_59_88","temp_59_88","wet_59_88"), collapse= "+"))) 
  
)


set.seed(1989) 
psDRYEXTRA<-weightitMSM(formula.list =FormulaListDRYEXTRA,
                        data=FBS,
                        method = "ps",verbose = T, use.kernel=T)

library(survey)
sDesign <- svydesign(~1, weights = psDRYEXTRA$weights,
                     data = FBS)
multiPSdryExtra.Rwi <- svyglm(rwi ~ spei_1_28_count_dry_extra +spei_29_58_count_dry_extra+
                           spei_59_88_count_dry_extra+
                           spei_89_18_count_dry_extra
                         , design = sDesign)

multiPSdryExtra.Awi <- svyglm(awi ~ spei_1_28_count_dry_extra +spei_29_58_count_dry_extra+
                           spei_59_88_count_dry_extra+
                           spei_89_18_count_dry_extra
                         , design = sDesign)


onePSdryExtra.Rwi <- svyglm(rwi ~ I(spei_1_28_count_dry_extra +spei_29_58_count_dry_extra+
                                 spei_59_88_count_dry_extra+
                                 spei_89_18_count_dry_extra)
                       , design = sDesign)


onePSdryExtra.Awi <- svyglm(awi ~ I(spei_1_28_count_dry_extra +spei_29_58_count_dry_extra+
                                 spei_59_88_count_dry_extra+
                                 spei_89_18_count_dry_extra)
                       , design = sDesign)


################################################################################
#

#                                ## count_wet #### 

#
################################################################################

FormulaListWET<-list(
  as.formula(
    paste("spei_1_28_count_wet ~ ", 
          paste(c(timeINV,"pre_1_28","temp_1_28","wet_1_28"), collapse= "+"))),
  
  as.formula(paste("spei_29_58_count_wet ~", 
                   paste(c(timeINV,"pre_29_58","temp_29_58","wet_29_58",
                           "spei_1_28_count_wet","pre_1_28","temp_1_28","wet_1_28"), collapse= "+"))),
  
  as.formula(paste("spei_59_88_count_wet ~ ",
                   paste(c(timeINV,"pre_59_88","temp_59_88","wet_59_88",
                           "spei_1_28_count_wet","pre_1_28","temp_1_28","wet_1_28",
                           "spei_29_58_count_wet","pre_29_58","temp_29_58","wet_29_58"), collapse= "+"))),
  
  as.formula(paste("spei_89_18_count_wet ~ ",
                   paste(c(timeINV,"pre_89_18","temp_89_18","wet_89_18",
                           "spei_1_28_count_wet","pre_1_28","temp_1_28","wet_1_28",
                           "spei_29_58_count_wet","pre_29_58","temp_29_58","wet_29_58",
                           "spei_59_88_count_wet","pre_59_88","temp_59_88","wet_59_88"), collapse= "+"))) 
  
)


gc()
set.seed(1989) 
psWET<-weightitMSM(formula.list =FormulaListWET,
                   data=FBS,
                   method = "ps",verbose = T)
library(survey)
sDesign <- svydesign(~1, weights = psWET$weights,
                     data = FBS)
multiPSwet.Rwi <- svyglm(rwi ~ spei_1_28_count_wet +spei_29_58_count_wet+
                           spei_59_88_count_wet+
                           spei_89_18_count_wet
                         , design = sDesign)

multiPSwet.Awi <- svyglm(awi ~ spei_1_28_count_wet +spei_29_58_count_wet+
                           spei_59_88_count_wet+
                           spei_89_18_count_wet
                         , design = sDesign)

onePSwet.Rwi <- svyglm(rwi ~ I(spei_1_28_count_wet +spei_29_58_count_wet+
                                 spei_59_88_count_wet+
                                 spei_89_18_count_wet)
                       , design = sDesign)


onePSwet.Awi <- svyglm(awi ~ I(spei_1_28_count_wet +spei_29_58_count_wet+
                                 spei_59_88_count_wet+
                                 spei_89_18_count_wet)
                       , design = sDesign)

FormulaListWETextra<-list(
  as.formula(
    paste("spei_1_28_count_wet_extra ~ ", 
          paste(c(timeINV,"pre_1_28","temp_1_28","wet_1_28"), collapse= "+"))),
  
  as.formula(paste("spei_29_58_count_wet_extra ~", 
                   paste(c(timeINV,"pre_29_58","temp_29_58","wet_29_58",
                           "spei_1_28_count_wet_extra","pre_1_28","temp_1_28","wet_1_28"), collapse= "+"))),
  
  as.formula(paste("spei_59_88_count_wet_extra ~ ",
                   paste(c(timeINV,"pre_59_88","temp_59_88","wet_59_88",
                           "spei_1_28_count_wet_extra","pre_1_28","temp_1_28","wet_1_28",
                           "spei_29_58_count_wet_extra","pre_29_58","temp_29_58","wet_29_58"), collapse= "+"))),
  
  as.formula(paste("spei_89_18_count_wet_extra ~ ",
                   paste(c(timeINV,"pre_89_18","temp_89_18","wet_89_18",
                           "spei_1_28_count_wet_extra","pre_1_28","temp_1_28","wet_1_28",
                           "spei_29_58_count_wet_extra","pre_29_58","temp_29_58","wet_29_58",
                           "spei_59_88_count_wet_extra","pre_59_88","temp_59_88","wet_59_88"), collapse= "+"))) 
  
)

gc()
set.seed(1989) 
psWETextra<-weightitMSM(formula.list =FormulaListWETextra,
                   data=FBS,
                   method = "ps",verbose = T)
# No matter which method is selected, weightitMSM() estimates separate weights 
# for each time period and then takes the product of the weights for each individual
# psDRY arrive at the final estimated weights.
gc()


library(survey)
sDesign <- svydesign(~1, weights = psWETextra$weights,
                     data = FBS)
multiPSwetExtra.Rwi <- svyglm(rwi ~ spei_1_28_count_wet_extra +spei_29_58_count_wet_extra+
                           spei_59_88_count_wet_extra+
                           spei_89_18_count_wet_extra
                         , design = sDesign)


multiPSwetExtra.Awi <- svyglm(awi ~ spei_1_28_count_wet_extra +spei_29_58_count_wet_extra+
                                spei_59_88_count_wet_extra+
                                spei_89_18_count_wet_extra
                         , design = sDesign)


onePSwetExtra.Rwi <- svyglm(rwi ~ I(spei_1_28_count_wet_extra +spei_29_58_count_wet_extra+
                                 spei_59_88_count_wet_extra+
                                 spei_89_18_count_wet_extra)
                       , design = sDesign)


onePSwetExtra.Awi <- svyglm(awi ~ I(spei_1_28_count_wet_extra +spei_29_58_count_wet_extra+
                                 spei_59_88_count_wet_extra+
                                 spei_89_18_count_wet_extra)
                       , design = sDesign)

save.image("envPsOLS101121_GF.RData")

source("figs_4_5.R", echo=T)

################################

library(stargazer)

names(multiPSdry.Rwi$coefficients) <- names(multiPSdry.Awi$coefficients) <- names(multiPSdryExtra.Rwi$coefficients) <-names(multiPSdryExtra.Awi$coefficients) <-names(multiPSwet.Rwi$coefficients) <- names(multiPSwet.Awi$coefficients) <- names(multiPSwetExtra.Rwi$coefficients) <-names(multiPSwetExtra.Awi$coefficients) <- c("Intercept", "# events (1901 - 1928)",  "# events (1929 - 1958)",  "# events (1959 - 1988)",  "# events (1989 - 2018)")

stargazer(multiPSdry.Rwi, multiPSdry.Awi, multiPSdryExtra.Rwi, multiPSdryExtra.Awi, multiPSwet.Rwi, multiPSwet.Awi, multiPSwetExtra.Rwi, multiPSwetExtra.Awi, align=TRUE, dep.var.labels=c("RWI (dry)","AWE (dry)", "RWI (dry extreme)","AWE (dry extreme)", "RWI (wet)","AWE (wet)", "RWI (wet extreme)","AWE (wet extreme)", no.space=TRUE), out="table3.tex")

names(onePSdry.Rwi$coefficients) <- names(onePSdry.Awi$coefficients) <- names(onePSdryExtra.Rwi$coefficients) <-names(onePSdryExtra.Awi$coefficients) <-names(onePSwet.Rwi$coefficients) <- names(onePSwet.Awi$coefficients) <- names(onePSwetExtra.Rwi$coefficients) <-names(onePSwetExtra.Awi$coefficients) <- c("Intercept", "Cumulative # events")

stargazer(onePSdry.Rwi, onePSdry.Awi, onePSdryExtra.Rwi, onePSdryExtra.Awi, onePSwet.Rwi, onePSwet.Awi, onePSwetExtra.Rwi, onePSwetExtra.Awi, align=TRUE, dep.var.labels=c("RWI (dry)","AWE (dry)", "RWI (dry extreme)","AWE (dry extreme)", "RWI (wet)","AWE (wet)", "RWI (wet extreme)","AWE (wet extreme)", no.space=TRUE,  font.size = "small", column.sep.width = "-15pt"), out="table2.tex")

###################

gc()
PreBalanceDRY<-bal.tab(FormulaListDRY,data=FBS,r.threshold = .05, disp.ks = TRUE, which.time = .none)$Balanced.correlations

pstBalancePSDRY<-bal.tab(psDRY, r.threshold = .05, disp.ks = TRUE, which.time = .none)$Balanced.correlations

psDRY$covs.list$spei_1_28_count_dry$iso3c<-NULL
psDRY$covs.list$spei_29_58_count_dry$iso3c<-NULL
psDRY$covs.list$spei_59_88_count_dry$iso3c<-NULL
psDRY$covs.list$spei_89_18_count_dry$iso3c<-NULL
psDRY$covs.list$spei_1_28_count_dry$agroclimaticzone <-NULL
psDRY$covs.list$spei_29_58_count_dry$agroclimaticzone<-NULL
psDRY$covs.list$spei_59_88_count_dry$agroclimaticzone<-NULL
psDRY$covs.list$spei_89_18_count_dry$agroclimaticzone<-NULL

myvars<-names(psDRY$covs.list$spei_89_18_count_dry)

newLables<-cbind.data.frame(old=myvars,new=1:length(myvars))

plotBalanceDRY<-love.plot(psDRY, stats = c("cor"), thresholds = c(cor = .05), 
                          abs = TRUE,
                          line = FALSE,addl.list=myvars,data=FBS,
                          var.names=newLables,title=" ",labels=" "
)+labs( subtitle = NULL,x = NULL)+theme(text = element_text(size=8))

##

gc()
PreBalanceDRYextra<-bal.tab(FormulaListDRYextra,data=FBS,r.threshold = .05, disp.ks = TRUE, which.time = .none)$Balanced.correlations

pstBalancePSDRYextra<-bal.tab(psDRYEXTRA, r.threshold = .05, disp.ks = TRUE, which.time = .none)$Balanced.correlations

psDRYEXTRA$covs.list$spei_1_28_count_dry_extra$iso3c<-NULL
psDRYEXTRA$covs.list$spei_29_58_count_dry_extra$iso3c<-NULL
psDRYEXTRA$covs.list$spei_59_88_count_dry_extra$iso3c<-NULL
psDRYEXTRA$covs.list$spei_89_18_count_dry_extra$iso3c<-NULL
psDRYEXTRA$covs.list$spei_1_28_count_dry_extra$agroclimaticzone <-NULL
psDRYEXTRA$covs.list$spei_29_58_count_dry_extra$agroclimaticzone<-NULL
psDRYEXTRA$covs.list$spei_59_88_count_dry_extra$agroclimaticzone<-NULL
psDRYEXTRA$covs.list$spei_89_18_count_dry_extra$agroclimaticzone<-NULL

myvars<-names(psDRYEXTRA$covs.list$spei_89_18_count_dry_extra)

newLables<-cbind.data.frame(old=myvars,new=1:length(myvars))

plotBalanceDRYextra<-love.plot(psDRYEXTRA, stats = c("cor"), thresholds = c(cor = .05), 
                          abs = TRUE,
                          line = FALSE,addl.list=myvars,data=FBS,
                          var.names=newLables,title=" ",labels=" "
)+labs( subtitle = NULL,x = NULL)+theme(text = element_text(size=8))


##

gc()
PreBalanceWET<-bal.tab(FormulaListWET,data=FBS,r.threshold = .05, disp.ks = TRUE, which.time = .none)$Balanced.correlations

pstBalancePSWET<-bal.tab(psWET, r.threshold = .05, disp.ks = TRUE, which.time = .none)$Balanced.correlations

psWET$covs.list$spei_1_28_count_wet$iso3c<-NULL
psWET$covs.list$spei_29_58_count_wet$iso3c<-NULL
psWET$covs.list$spei_59_88_count_wet$iso3c<-NULL
psWET$covs.list$spei_89_18_count_wet$iso3c<-NULL
psWET$covs.list$spei_1_28_count_wet$agroclimaticzone <-NULL
psWET$covs.list$spei_29_58_count_wet$agroclimaticzone<-NULL
psWET$covs.list$spei_59_88_count_wet$agroclimaticzone<-NULL
psWET$covs.list$spei_89_18_count_wet$agroclimaticzone<-NULL

myvars<-names(psWET$covs.list$spei_89_18_count_wet)

newLables<-cbind.data.frame(old=myvars,new=1:length(myvars))

plotBalanceWET<-love.plot(psWET, stats = c("cor"), thresholds = c(cor = .05), 
                          abs = TRUE,
                          line = FALSE,addl.list=myvars,data=FBS,
                          var.names=newLables,title=" ",labels=" "
)+labs( subtitle = NULL,x = NULL)+theme(text = element_text(size=8))

##

gc()
PreBalanceWETextra<-bal.tab(FormulaListWETextra,data=FBS,r.threshold = .05, disp.ks = TRUE, which.time = .none)$Balanced.correlations

pstBalancePSWETextra<-bal.tab(psWETextra, r.threshold = .05, disp.ks = TRUE, which.time = .none)$Balanced.correlations

psWETextra$covs.list$spei_1_28_count_wet_extra$iso3c<-NULL
psWETextra$covs.list$spei_29_58_count_wet_extra$iso3c<-NULL
psWETextra$covs.list$spei_59_88_count_wet_extra$iso3c<-NULL
psWETextra$covs.list$spei_89_18_count_wet_extra$iso3c<-NULL
psWETextra$covs.list$spei_1_28_count_wet_extra$agroclimaticzone <-NULL
psWETextra$covs.list$spei_29_58_count_wet_extra$agroclimaticzone<-NULL
psWETextra$covs.list$spei_59_88_count_wet_extra$agroclimaticzone<-NULL
psWETextra$covs.list$spei_89_18_count_wet_extra$agroclimaticzone<-NULL

myvars<-names(psWETextra$covs.list$spei_89_18_count_wet_extra)

newLables<-cbind.data.frame(old=myvars,new=1:length(myvars))

plotBalanceWETextra<-love.plot(psWETextra, stats = c("cor"), thresholds = c(cor = .05), 
                          abs = TRUE,
                          line = FALSE,addl.list=myvars,data=FBS,
                          var.names=newLables,title=" ",labels=" "
)+labs( subtitle = NULL,x = NULL)+theme(text = element_text(size=8))

#

#----------------------------------------------------
# Balance Tables
#----------------------------------------------------

TableBalancePRE<-list(PreBalanceDRY,PreBalanceDRYextra,
                      PreBalanceWET,PreBalanceWETextra
)
TableBalancePRE<-do.call("cbind", TableBalancePRE)
names(TableBalancePRE)<-c("Dry","Dry-Extra","Wet","Wet-Extra")

TableBalancePOST<-list(pstBalancePSDRY,pstBalancePSDRYextra,
                       pstBalancePSWET,pstBalancePSWETextra
)
TableBalancePOST<-do.call("cbind", TableBalancePOST)
names(TableBalancePOST)<-c("Dry","Dry-Extra","Wet","Wet-Extra")

xtable::xtable(TableBalancePRE)
xtable::xtable(TableBalancePOST)

#----------------------------------------------------
# Balance Plots
#----------------------------------------------------
PlotsBalance<-list(plotBalanceDRY,plotBalanceDRYextra,
                   plotBalanceWET, plotBalanceWETextra)
library(ggpubr)
figure <- ggarrange(plotlist=PlotsBalance,
                    labels = c("A", "B", "C","D"),
                    ncol = 2, nrow = 2,common.legend = TRUE, legend = "top")

figureBalance<-annotate_figure(figure,
                top = text_grob("Covariate Balance\n Max Across Time Points",
                                color = "black", face = "bold", size = 10),
                bottom = text_grob("Absolute Treatment-Covarites Correlations",
                                color = "black", face = "plain", size = 9)
                )

ggsave("./Plots/figureBalanceOLS.png", width = 20, height = 20, units = "cm")




###### DENSITY PLOTS #####

legendName=c("# events (1901 - 1928)","# events (1929 - 1958)","# events (1959 - 1988)","# events (1989 - 2018)")
plot(density(FBS$spei_1_28_count_dry),col=1,main = "Count of Dry Events")
lines(density(FBS$spei_29_58_count_dry),col=2)
lines(density(FBS$spei_59_88_count_dry),col=3)
lines(density(FBS$spei_89_18_count_dry),col=4)
legend("topright",        # Add legend to plot
       legendName,
       col = 1:4,
       lty = 1)

ggsave("./Plots/densDry.png", width = 20, height = 13, units = "cm")

plot(density(FBS$spei_1_28_count_dry_extra),col=1,main = "Count of Dry-Extra Events")
lines(density(FBS$spei_29_58_count_dry_extra),col=2)
lines(density(FBS$spei_59_88_count_dry_extra),col=3)
lines(density(FBS$spei_89_18_count_dry_extra),col=4)
legend("topright",        # Add legend to plot
       c("1_28", "29_58", "59_88","89_18"),
       col = 1:4,
       lty = 1)


ggsave("./Plots/densDryEX.png", width = 20, height = 13, units = "cm")


plot(density(FBS$spei_1_28_count_wet),col=1,main = "Count of Wet Events")
lines(density(FBS$spei_29_58_count_wet),col=2)
lines(density(FBS$spei_59_88_count_wet),col=3)
lines(density(FBS$spei_89_18_count_wet),col=4)
legend("topright",        # Add legend to plot
       c("1_28", "29_58", "59_88","89_18"),
       col = 1:4,
       lty = 1)



ggsave("./Plots/densWet.png", width = 20, height = 13, units = "cm")


plot(density(FBS$spei_1_28_count_wet_extra),col=1,main = "Count of Wet-Extra Events")
lines(density(FBS$spei_29_58_count_wet_extra),col=2)
lines(density(FBS$spei_59_88_count_wet_extra),col=3)
lines(density(FBS$spei_89_18_count_wet_extra),col=4)
legend("topright",        # Add legend to plot
       c("1_28", "29_58", "59_88","89_18"),
       col = 1:4,
       lty = 1)

ggsave("./Plots/densWetEX.png", width = 20, height = 13, units = "cm")

###################################################################

#Decile analysis

FBS[,dryTreat:=spei_1_28_count_dry +spei_29_58_count_dry+
      spei_59_88_count_dry+spei_89_18_count_dry]

FBS[,dryExTreat:=spei_1_28_count_dry_extra +spei_29_58_count_dry_extra+
      spei_59_88_count_dry_extra+spei_89_18_count_dry_extra]


FBS[,wetTreat:=spei_1_28_count_wet +spei_29_58_count_wet+
      spei_59_88_count_wet+spei_89_18_count_wet]

FBS[,wetExTreat:=spei_1_28_count_wet_extra +spei_29_58_count_wet_extra+
      spei_59_88_count_wet_extra+spei_89_18_count_wet_extra]


FBS$DryCut<-quantcut(FBS$dryTreat, seq(0, 1, by = 0.1))
FBS$DryExCut<-quantcut(FBS$dryExTreat, seq(0, 1, by = 0.1))
FBS$WetCut<-quantcut(FBS$wetTreat, seq(0, 1, by = 0.1))
FBS$WetExCut<-quantcut(FBS$wetExTreat, seq(0, 1, by = 0.1))
anyNA(FBS$WetCut)

table(FBS$WetCut)

#### DRY
sDesign <- svydesign(~1, weights = psDRY$weights,
                     data = FBS)

quantAwiDry <- svyglm(awi ~ DryCut
                   , design = sDesign)

summary(quantAwiDry)

effect_plot(quantAwiDry, pred =DryCut, interval = TRUE,int.type = "confidence",
            cat.geom="line",y.label = "AWE",
            x.label = "Quantiles of # Dry Events")+theme_bw()   

ggsave("Qdry.png", width = 20, height = 13, units = "cm")

####### DRY EX

sDesign <- svydesign(~1, weights = psDRYEXTRA$weights,
                     data = FBS)

quantAwiDryEx <- svyglm(awi ~ DryExCut
                      , design = sDesign)

summary(quantAwiDryEx)

effect_plot(quantAwiDryEx, pred =DryExCut, interval = TRUE,int.type = "confidence",
            cat.geom="line",y.label = "AWE",
            x.label = "Quantiles of # Extreme Dry Events")+theme_bw()   

ggsave("QdryEx.png", width = 20, height = 13, units = "cm")

####### WET

sDesign <- svydesign(~1, weights = psWET$weights,
                     data = FBS)

quantAwiWet <- svyglm(awi ~ WetCut
                      , design = sDesign)

summary(quantAwiWet)

effect_plot(quantAwiWet, pred =WetCut, interval = TRUE,int.type = "confidence",
            cat.geom="line",y.label = "AWE",
            x.label = "Quantiles of # Wet Events")+theme_bw()   


ggsave("Qwet.png", width = 20, height = 13, units = "cm")
##### WET EX
sDesign <- svydesign(~1, weights = psWETextra$weights,
                     data = FBS)

quantAwiWetEx<- svyglm(awi ~ WetExCut
                      , design = sDesign)

summary(quantAwiWetEx)

effect_plot(quantAwiWetEx, pred =WetExCut, interval = TRUE,int.type = "confidence",
            cat.geom="line",y.label = "AWE",
            x.label = "Quantiles of # Extreme Wet Events")+theme_bw()   

ggsave("QwetEx.png", width = 20, height = 13, units = "cm")




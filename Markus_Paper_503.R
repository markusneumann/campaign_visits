### MARKUS NEUMANN
### PLSC 503 - Multivariate Analysis for Political Research
### Paper: Count Models and Campaign Visits


#Set working directory to whichever folder the files are in
setwd("C:/Users/Markus/OneDrive/2_Zorn/Paper")

#R version 3.2.5 (2016-04-14) -- "Very, Very Secure Dishes"

library(stargazer) #exporting regression tables to LaTeX
library(ggplot2) #graphics
library(cowplot) #graphics
library(RColorBrewer) #colour palettes for ggplot
library(pscl) #hurdle and zero inflated count models
library(MASS) #negative binomial
library(reshape2) #reshape data (used for ggplot)
library(xtable) #exporting tables to LaTeX

#Set random seed for consistent results
set.seed(123)



##########
# STATES #
##########

##State-level campaign visits data
#I already assembled this data for my M.A. thesis
#Since it's not really pertinent, I decided it's best to spare you the 600 lines of code
load("States.rdata")
States$StateAbb <- state.abb[match(States$StateName,state.name)]
States$StateAbb[9] <- "DC"

#With Senate variable
m1.ls <- lm(Total12 ~ VoteDiff08 + EVotes2012 + VoteDiff08*EVotes2012 + Class3Senator, data=States)
m1.log0001 <- lm(log(Total12+0.001) ~ VoteDiff08 + EVotes2012 + VoteDiff08*EVotes2012 + Class3Senator, data=States)
m1.log05 <- lm(log(Total12+0.5) ~ VoteDiff08 + EVotes2012 + VoteDiff08*EVotes2012 + Class3Senator, data=States)
m1.log1 <- lm(log(Total12+1) ~ VoteDiff08 + EVotes2012 + VoteDiff08*EVotes2012 + Class3Senator, data=States)
m1.quad <- lm(Total12^2 ~ VoteDiff08 + EVotes2012 + VoteDiff08*EVotes2012 + Class3Senator, data=States)
m1.sq <- lm(sqrt(Total12) ~ VoteDiff08 + EVotes2012 + VoteDiff08*EVotes2012 + Class3Senator, data=States)
m1.pois <- glm(Total12 ~ VoteDiff08 + EVotes2012 + VoteDiff08*EVotes2012 + Class3Senator, data=States, family="poisson")
m1.qpois <- glm(Total12 ~ VoteDiff08 + EVotes2012 + VoteDiff08*EVotes2012 + Class3Senator, data=States, family="quasipoisson")
m1.nb <- glm.nb(Total12 ~ VoteDiff08 + EVotes2012 + VoteDiff08*EVotes2012 + Class3Senator, data=States)
m1.hurdle <- hurdle(Total12 ~ VoteDiff08 + EVotes2012 + VoteDiff08*EVotes2012 + Class3Senator, data=States, dist = "negbin")
m1.zero <- zeroinfl(Total12 ~ VoteDiff08 + EVotes2012 + VoteDiff08*EVotes2012 + Class3Senator, data=States, dist = "negbin")
m1.zero.pois <- zeroinfl(Total12 ~ VoteDiff08 + EVotes2012 + VoteDiff08*EVotes2012 + Class3Senator, data=States, dist = "poisson")

#Stargazer
stargazer(m1.ls,m1.log0001,m1.log05,m1.log1,m1.sq,
          digits=2,
          dep.var.labels = c("Visits","log(Visits+0.001)","log(Visits+0.5)","log(Visits+1)","sqrt(Visits)"),
          keep.stat = c("rsq","adj.rsq"),
          title="OLS count models - 2012 state level effects")

stargazer(m1.pois,m1.qpois,m1.nb,m1.hurdle,m1.zero,
          digits=2,
          dep.var.labels = "Visits",
          keep.stat = c("ll"),
          title="Poisson and negative binomial count models - 2012 state level effects")

#in-sample predictions for model 1
predictions <- data.frame(State=States$StateAbb,
                          actual=States$Total12, 
                          ls=m1.ls$fitted.values,
                          log0001=exp(m1.log0001$fitted.values),
                          log05=exp(m1.log05$fitted.values),
                          log1=exp(m1.log1$fitted.values),
                          sqrt=(m1.sq$fitted.values)^2,
                          pois=m1.pois$fitted.values,
                          nb=m1.nb$fitted.values,
                          hurdle=m1.hurdle$fitted.values,
                          zero=m1.zero$fitted.values)

print(xtable(predictions, digits=0, caption="Actual versus predicted number of campaign visits"), include.rownames=FALSE)

#...for when y is zero
predictions_zero <- predictions[predictions$actual==0,2:ncol(predictions)]
predictions_zero <- melt(predictions_zero)
g_pred_zero <- ggplot(predictions_zero, aes(factor(variable), value)) + geom_boxplot() + labs(x="Model", y="Value", title="States: Fitted values for y=0")
g_pred_zero

#...for when y is not zero
predictions_nzero <- predictions[predictions$actual!=0,2:ncol(predictions)]
predictions_nzero <- melt(predictions_nzero)
g_pred_nzero <- ggplot(predictions_nzero, aes(factor(variable), value)) + geom_boxplot() + ylim(0,200) + labs(x="Model", y="Value", title="States: Fitted values for y!=0")
g_pred_nzero

#for all y
predictions_all <- predictions[,2:ncol(predictions)]
predictions_all <- melt(predictions_all)
g_pred_all <- ggplot(predictions_all, aes(factor(variable), value)) + geom_boxplot() + ylim(0,200) + labs(x="Model", y="Value", title="States: Fitted values")
g_pred_all

#plot y=0 and y!=0 together and save the pdf
g_pred <- plot_grid(g_pred_zero,g_pred_nzero)
g_pred
ggsave("Figures/g_pred.pdf", plot=g_pred, height=6, width=12)


#residuals - states
resi_m1 <- data.frame(res.ls=m1.ls$residuals,
                      res.log0001=m1.log0001$residuals,
                      res.log05=m1.log05$residuals,
                      res.log1=m1.log1$residuals,
                      res.sqrt=m1.sq$residuals,
                      res.pois=m1.pois$residuals,
                      res.qpois=m1.qpois$residuals,
                      res.nb=m1.nb$residuals,
                      res.hurdle=m1.hurdle$residuals,
                      res.zero=m1.zero$residuals)

#save residual plots
g_r_1 <- ggplot(resi_m1, aes(res.ls)) + geom_density() + labs(x="Untransformed") + theme(legend.position="none")
g_r_2 <- ggplot(resi_m1, aes(res.log0001)) + geom_density() + labs(x="Log-Transformation (+0.001)") + theme(legend.position="none")
g_r_3 <- ggplot(resi_m1, aes(res.log05)) + geom_density() + labs(x="Log-Transformation (+0.5)") + theme(legend.position="none")
g_r_4 <- ggplot(resi_m1, aes(res.log1)) + geom_density() + labs(x="Log-Transformation (+1)") + theme(legend.position="none")
g_r_5 <- ggplot(resi_m1, aes(res.sqrt)) + geom_density() + labs(x="Square root-transformation") + theme(legend.position="none")
g_r_6 <- ggplot(resi_m1, aes(res.pois)) + geom_density() + labs(x="Poisson") + theme(legend.position="none")
g_r_7 <- ggplot(resi_m1, aes(res.qpois)) + geom_density() + labs(x="Quasi-Poisson") + theme(legend.position="none")
g_r_8 <- ggplot(resi_m1, aes(res.nb)) + geom_density() + labs(x="Negative Binomial") + theme(legend.position="none")
g_r_9 <- ggplot(resi_m1, aes(res.hurdle)) + geom_density() + labs(x="Hurdle") + theme(legend.position="none")
g_r_10 <- ggplot(resi_m1, aes(res.zero)) + geom_density() + labs(x="Zero inflation") + theme(legend.position="none")

#put them all together
resplot <- plot_grid(g_r_1,g_r_2,g_r_3,g_r_4,g_r_5,g_r_6,g_r_7,g_r_8,g_r_9,g_r_10, ncol = 2)
resplot
ggsave(filename="Figures/g_resplot.pdf", plot=resplot, height=10, width=8)


############
# COUNTIES #
############

##County-level campaign visits data
load("Counties.rdata")

#Democrats 2012
m2.ls <- lm(DEM12~VoteDem12PCT+VoteDiff12+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2012+evanrate, data=Counties)
m2.log0001 <- lm(log(DEM12+0.001)~VoteDem12PCT+VoteDiff12+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2012+evanrate, data=Counties)
m2.log05 <- lm(log(DEM12+0.5)~VoteDem12PCT+VoteDiff12+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2012+evanrate, data=Counties)
m2.log1 <- lm(log(DEM12+1)~VoteDem12PCT+VoteDiff12+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2012+evanrate, data=Counties)
m2.sq <- lm(sqrt(DEM12)~VoteDem12PCT+VoteDiff12+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2012+evanrate, data=Counties)
m2.pois <- glm(DEM12~VoteDem12PCT+VoteDiff12+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2012+evanrate, data=Counties, family="poisson")
m2.qpois <- glm(DEM12~VoteDem12PCT+VoteDiff12+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2012+evanrate, data=Counties, family="quasipoisson")
m2.nb <- glm.nb(DEM12~VoteDem12PCT+VoteDiff12+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2012+evanrate, data=Counties)
m2.hurdle <- hurdle(DEM12~VoteDem12PCT+VoteDiff12+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2012+evanrate, data=Counties, dist = "negbin")
m2.zero <- zeroinfl(DEM12~VoteDem12PCT+VoteDiff12+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2012+evanrate, data=Counties, dist = "negbin")

#Stargazer
stargazer(m2.ls,m2.log0001,m2.log05,m2.log1,m2.sq,
          digits=2,
          dep.var.labels = c("Visits","log(Visits+0.001)","log(Visits+0.5)","log(Visits+1)","sqrt(Visits)"),
          keep.stat = c("rsq","adj.rsq"),
          title="OLS count models - 2012 county level effects")

stargazer(m2.pois,m2.qpois,m2.nb,m2.hurdle,m2.zero,
          digits=2,
          dep.var.labels = "Visits",
          keep.stat = c("ll"),
          title="Poisson and negative binomial count models - 2012 county level effects")


#in-sample predictions for model 2
predictions2 <- data.frame(actual=m2.nb$y, 
                          ls=m2.ls$fitted.values,
                          log0001=m2.log0001$residuals,
                          log05=m2.log05$residuals,
                          log1=m2.log1$residuals,
                          sq=(m2.sq$fitted.values)^2,
                          pois=m2.pois$fitted.values,
                          nb=m2.nb$fitted.values,
                          hurdle=m2.hurdle$fitted.values,
                          zero=m2.zero$fitted.values)

#...for when y is zero
predictions2_zero <- predictions2[predictions2$actual==0,1:ncol(predictions2)]
predictions2_zero <- melt(predictions2_zero)
g_pred2_zero <- ggplot(predictions2_zero, aes(factor(variable), value)) + geom_boxplot() + ylim(0,5) + labs(x="Model", y="Value", title="Counties: Fitted values for y=0")
g_pred2_zero

#...for when y is zero
predictions2_nzero <- predictions2[predictions2$actual!=0,1:ncol(predictions2)]
predictions2_nzero <- melt(predictions2_nzero)
g_pred2_nzero <- ggplot(predictions2_nzero, aes(factor(variable), value)) + geom_boxplot() + ylim(0,10) + labs(x="Model", y="Value", title="Counties: Fitted values for y!=0")
g_pred2_nzero

#for all y
predictions2_all <- melt(predictions2)
g_pred2_all <- ggplot(predictions2_all, aes(factor(variable), value)) + geom_boxplot() + ylim(0,10) + labs(x="Model", y="Value", title="Counties: Fitted values")
g_pred2_all

#plot y=0 and y!=0 together and save the pdf
g_pred2 <- plot_grid(g_pred2_zero,g_pred2_nzero)
g_pred2
ggsave("Figures/g_pred2.pdf", plot=g_pred2, height=6, width=12)



#FIGURES

#Frequency Plot

#States part of the plot
States_Total_04_08_12 <- melt(subset(States, select=c(Total12,Total08,Total04)))
States_Parties_04_08_12 <- melt(subset(States, select=c(DEM12,REP12,DEM08,REP08,DEM04,REP04)))

a <- data.frame(apply(subset(States, select=c(DEM12,REP12,DEM08,REP08,DEM04,REP04)),2,sum))
names(a) <- "value"
a$variable <- c("d1","d2","d3","d4","d5","d6") #these just need to be in descending order

g_bar_States_Visits <- ggplot(a, aes(x=0, y=value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme(legend.position="none",axis.ticks = element_blank(), axis.text.x = element_blank()) + 
  labs(x=NULL, y=NULL, title="Total Visits")
g_bar_States_0 <- ggplot(States_Parties_04_08_12, aes(value, fill=variable)) + geom_bar(position="dodge") + xlim(-0.5,0.5) + theme(legend.position="none",axis.ticks = element_blank(), axis.text.x = element_blank()) + labs(x=NULL, y=NULL, title="Zeros (States)")#zeroes
g_bar_States <- ggplot(States_Parties_04_08_12, aes(value, color=variable)) + geom_density() + xlim(1,100) + labs(title="Visit Number Frequency (States)") + theme(legend.position="none")

#Counties part of the plot
Counties_Parties_04_08_12 <- melt(subset(Counties, select=c(DEM12,REP12,DEM08,REP08,DEM04,REP04)))

a <- data.frame(apply(subset(Counties, select=c(DEM12,REP12,DEM08,REP08,DEM04,REP04)),2,sum))
names(a) <- "value"
a$variable <- c("d1","d2","d3","d4","d5","d6") #these just need to be in descending order

g_bar_Counties_Visits <- ggplot(a, aes(x=0, y=value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") + 
  theme(legend.position="left",axis.ticks = element_blank(), axis.text.x = element_blank()) + 
  labs(x=NULL, y=NULL, title="Total Visits")
g_bar_Counties_0 <- ggplot(Counties_Parties_04_08_12, aes(value, fill=variable)) + geom_bar(position="dodge") + xlim(-0.5,0.5) + theme(legend.position="none",axis.ticks = element_blank(), axis.text.x = element_blank()) + labs(x=NULL, y=NULL, title="Zeros (Counties)")#zeroes
g_bar_Counties <- ggplot(Counties_Parties_04_08_12, aes(value, fill=variable)) + geom_bar(position="dodge") + xlim(0,10) + ylim(0,150) + labs(title="Visit Number Frequency (Counties)") + theme(legend.position="bottom")

abc <- plot_grid(g_bar_States_Visits,g_bar_States_0,g_bar_Counties_0, ncol = 3)
def <- plot_grid(abc,g_bar_States,g_bar_Counties, ncol = 1)

#Save the plot
ggsave(filename="Figures/g_frequencies.pdf", plot=def, height=12, width=8)



#Transformations distributions
states_transformed <- data.frame(untrans=States$Total12,logged0001=log(States$Total12+0.001),logged05=log(States$Total12+0.5),logged1=log(States$Total12+1),sqrted=sqrt(States$Total12))

g_distr_transf <- ggplot(states_transformed, aes(untrans)) + geom_density() + xlim(0,200) + labs(x="Untransformed") + theme(legend.position="none")
g_distr_logged0001 <- ggplot(states_transformed, aes(logged0001)) + geom_density() + xlim(0,6) + labs(x="Log-transformed (+0.001)") + theme(legend.position="none")
g_distr_logged05 <- ggplot(states_transformed, aes(logged05)) + geom_density() + xlim(0,6) + labs(x="Log-transformed (+0.5)") + theme(legend.position="none")
g_distr_logged1 <- ggplot(states_transformed, aes(logged1)) + geom_density() + xlim(0,6) + labs(x="Log-transformed (+1)") + theme(legend.position="none")
g_distr_sqrted <- ggplot(states_transformed, aes(sqrted)) + geom_density() + xlim(0,15) + labs(x="Square-root-transformed") + theme(legend.position="none")
g_distr_transformed <- plot_grid(g_distr_transf,g_distr_logged0001,g_distr_logged1,g_distr_sqrted,ncol=2)
g_distr_transformed
ggsave(filename="Figures/g_distr_transformed.pdf", plot=g_distr_transformed, height=8, width=12)

#Residuals counties
resi_m1 <- data.frame(res.ls=m2.ls$residuals,
                      res.log0001=m2.log0001$residuals,
                      res.log05=m2.log05$residuals,
                      res.log1=m2.log1$residuals,
                      res.sqrt=m2.sq$residuals,
                      res.pois=m2.pois$residuals,
                      res.qpois=m2.qpois$residuals,
                      res.nb=m2.nb$residuals,
                      res.hurdle=m2.hurdle$residuals,
                      res.zero=m2.zero$residuals)

g_r_1 <- ggplot(resi_m1, aes(res.ls)) + geom_density() + labs(x="Untransformed") + theme(legend.position="none")
g_r_2 <- ggplot(resi_m1, aes(res.log0001)) + geom_density() + labs(x="Log-Transformation (+0.001)") + theme(legend.position="none")
g_r_3 <- ggplot(resi_m1, aes(res.log05)) + geom_density() + labs(x="Log-Transformation (+0.5)") + theme(legend.position="none")
g_r_4 <- ggplot(resi_m1, aes(res.log1)) + geom_density() + labs(x="Log-Transformation (+1)") + theme(legend.position="none")
g_r_5 <- ggplot(resi_m1, aes(res.sqrt)) + geom_density() + labs(x="Square root-transformation") + theme(legend.position="none")
g_r_6 <- ggplot(resi_m1, aes(res.pois)) + geom_density() + labs(x="Poisson") + theme(legend.position="none")
g_r_7 <- ggplot(resi_m1, aes(res.qpois)) + geom_density() + labs(x="Quasi-Poisson") + theme(legend.position="none")
g_r_8 <- ggplot(resi_m1, aes(res.nb)) + geom_density() + labs(x="Negative Binomial") + theme(legend.position="none")
g_r_9 <- ggplot(resi_m1, aes(res.hurdle)) + geom_density() + labs(x="Hurdle") + theme(legend.position="none")
g_r_10 <- ggplot(resi_m1, aes(res.zero)) + geom_density() + labs(x="Zero inflation") + theme(legend.position="none")

resplot2 <- plot_grid(g_r_1,g_r_2,g_r_3,g_r_4,g_r_5,g_r_6,g_r_7,g_r_8,g_r_9,g_r_10, ncol = 2)
resplot2
ggsave(filename="Figures/g_resplot2.pdf", plot=resplot2, height=10, width=8)



###Alternative models

#States
#2008
m1.ls <- lm(Total08 ~ VoteDiff04 + EVotes2004_2008 + VoteDiff04*EVotes2004_2008 + Class1Senator, data=States)
m1.log0001 <- lm(log(Total08+0.001) ~ VoteDiff04 + EVotes2004_2008 + VoteDiff04*EVotes2004_2008 + Class1Senator, data=States)
m1.log05 <- lm(log(Total08+0.5) ~ VoteDiff04 + EVotes2004_2008 + VoteDiff04*EVotes2004_2008 + Class1Senator, data=States)
m1.log1 <- lm(log(Total08+1) ~ VoteDiff04 + EVotes2004_2008 + VoteDiff04*EVotes2004_2008 + Class1Senator, data=States)
m1.quad <- lm(Total08^2 ~ VoteDiff04 + EVotes2004_2008 + VoteDiff04*EVotes2004_2008 + Class1Senator, data=States)
m1.sq <- lm(sqrt(Total08) ~ VoteDiff04 + EVotes2004_2008 + VoteDiff04*EVotes2004_2008 + Class1Senator, data=States)
m1.pois <- glm(Total08 ~ VoteDiff04 + EVotes2004_2008 + VoteDiff04*EVotes2004_2008 + Class1Senator, data=States, family="poisson")
m1.qpois <- glm(Total08 ~ VoteDiff04 + EVotes2004_2008 + VoteDiff04*EVotes2004_2008 + Class1Senator, data=States, family="quasipoisson")
m1.nb <- glm.nb(Total08 ~ VoteDiff04 + EVotes2004_2008 + VoteDiff04*EVotes2004_2008 + Class1Senator, data=States)
m1.hurdle <- hurdle(Total08 ~ VoteDiff04 + EVotes2004_2008 + VoteDiff04*EVotes2004_2008 + Class1Senator, data=States, dist = "negbin")
m1.zero <- zeroinfl(Total08 ~ VoteDiff04 + EVotes2004_2008 + VoteDiff04*EVotes2004_2008 + Class1Senator, data=States, dist = "negbin")
m1.zero.pois <- zeroinfl(Total08 ~ VoteDiff04 + EVotes2004_2008 + VoteDiff04*EVotes2004_2008 + Class1Senator, data=States, dist = "poisson")

#2004
m1.ls <- lm(Total04 ~ VoteDiff00 + EVotes2004_2008 + VoteDiff00*EVotes2004_2008 + Class2Senator, data=States)
m1.log0001 <- lm(log(Total04+0.001) ~ VoteDiff00 + EVotes2004_2008 + VoteDiff00*EVotes2004_2008 + Class2Senator, data=States)
m1.log05 <- lm(log(Total04+0.5) ~ VoteDiff00 + EVotes2004_2008 + VoteDiff00*EVotes2004_2008 + Class2Senator, data=States)
m1.log1 <- lm(log(Total04+1) ~ VoteDiff00 + EVotes2004_2008 + VoteDiff00*EVotes2004_2008 + Class2Senator, data=States)
m1.quad <- lm(Total04^2 ~ VoteDiff00 + EVotes2004_2008 + VoteDiff00*EVotes2004_2008 + Class2Senator, data=States)
m1.sq <- lm(sqrt(Total04) ~ VoteDiff00 + EVotes2004_2008 + VoteDiff00*EVotes2004_2008 + Class2Senator, data=States)
m1.pois <- glm(Total04 ~ VoteDiff00 + EVotes2004_2008 + VoteDiff00*EVotes2004_2008 + Class2Senator, data=States, family="poisson")
m1.qpois <- glm(Total04 ~ VoteDiff00 + EVotes2004_2008 + VoteDiff00*EVotes2004_2008 + Class2Senator, data=States, family="quasipoisson")
m1.nb <- glm.nb(Total04 ~ VoteDiff00 + EVotes2004_2008 + VoteDiff00*EVotes2004_2008 + Class2Senator, data=States)
m1.hurdle <- hurdle(Total04 ~ VoteDiff00 + EVotes2004_2008 + VoteDiff00*EVotes2004_2008 + Class2Senator, data=States, dist = "negbin")
m1.zero <- zeroinfl(Total04 ~ VoteDiff00 + EVotes2004_2008 + VoteDiff00*EVotes2004_2008 + Class2Senator, data=States, dist = "negbin")
m1.zero.pois <- zeroinfl(Total04 ~ VoteDiff00 + EVotes2004_2008 + VoteDiff00*EVotes2004_2008 + Class2Senator, data=States, dist = "poisson")

#Counties

#Republicans 2012
m2.ls <- lm(REP12~VoteRep12PCT+VoteDiff12+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2012+evanrate, data=Counties)
m2.log0001 <- lm(log(REP12+0.001)~VoteRep12PCT+VoteDiff12+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2012+evanrate, data=Counties)
m2.log05 <- lm(log(REP12+0.5)~VoteRep12PCT+VoteDiff12+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2012+evanrate, data=Counties)
m2.log1 <- lm(log(REP12+1)~VoteRep12PCT+VoteDiff12+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2012+evanrate, data=Counties)
m2.sq <- lm(sqrt(REP12)~VoteRep12PCT+VoteDiff12+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2012+evanrate, data=Counties)
m2.pois <- glm(REP12~VoteRep12PCT+VoteDiff12+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2012+evanrate, data=Counties, family="poisson")
m2.qpois <- glm(REP12~VoteRep12PCT+VoteDiff12+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2012+evanrate, data=Counties, family="quasipoisson")
m2.nb <- glm.nb(REP12~VoteRep12PCT+VoteDiff12+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2012+evanrate, data=Counties)
m2.hurdle <- hurdle(REP12~VoteRep12PCT+VoteDiff12+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2012+evanrate, data=Counties, dist = "negbin")
m2.zero <- zeroinfl(REP12~VoteRep12PCT+VoteDiff12+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2012+evanrate, data=Counties, dist = "negbin")

#Democrats 2008
m2.ls <- lm(DEM08~VoteDem08PCT+VoteDiff08+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2008+evanrate, data=Counties)
m2.log0001 <- lm(log(DEM08+0.001)~VoteDem08PCT+VoteDiff08+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2008+evanrate, data=Counties)
m2.log05 <- lm(log(DEM08+0.5)~VoteDem08PCT+VoteDiff08+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2008+evanrate, data=Counties)
m2.log1 <- lm(log(DEM08+1)~VoteDem08PCT+VoteDiff08+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2008+evanrate, data=Counties)
m2.sq <- lm(sqrt(DEM08)~VoteDem08PCT+VoteDiff08+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2008+evanrate, data=Counties)
m2.pois <- glm(DEM08~VoteDem08PCT+VoteDiff08+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2008+evanrate, data=Counties, family="poisson")
m2.qpois <- glm(DEM08~VoteDem08PCT+VoteDiff08+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2008+evanrate, data=Counties, family="quasipoisson")
m2.nb <- glm.nb(DEM08~VoteDem08PCT+VoteDiff08+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2008+evanrate, data=Counties)
m2.hurdle <- hurdle(DEM08~VoteDem08PCT+VoteDiff08+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2008+evanrate, data=Counties, dist = "negbin")
m2.zero <- zeroinfl(DEM08~VoteDem08PCT+VoteDiff08+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2008+evanrate, data=Counties, dist = "negbin")

#Republicans 2008
m2.ls <- lm(REP08~VoteRep08PCT+VoteDiff08+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2008+evanrate, data=Counties)
m2.log0001 <- lm(log(REP08+0.001)~VoteRep08PCT+VoteDiff08+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2008+evanrate, data=Counties)
m2.log05 <- lm(log(REP08+0.5)~VoteRep08PCT+VoteDiff08+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2008+evanrate, data=Counties)
m2.log1 <- lm(log(REP08+1)~VoteRep08PCT+VoteDiff08+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2008+evanrate, data=Counties)
m2.sq <- lm(sqrt(REP08)~VoteRep08PCT+VoteDiff08+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2008+evanrate, data=Counties)
m2.pois <- glm(REP08~VoteRep08PCT+VoteDiff08+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2008+evanrate, data=Counties, family="poisson")
m2.qpois <- glm(REP08~VoteRep08PCT+VoteDiff08+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2008+evanrate, data=Counties, family="quasipoisson")
m2.nb <- glm.nb(REP08~VoteRep08PCT+VoteDiff08+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2008+evanrate, data=Counties)
m2.hurdle <- hurdle(REP08~VoteRep08PCT+VoteDiff08+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2008+evanrate, data=Counties, dist = "negbin")
m2.zero <- zeroinfl(REP08~VoteRep08PCT+VoteDiff08+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2008+evanrate, data=Counties, dist = "negbin")

#Democrats 2004
m2.ls <- lm(REP04~VoteDem04PCT+VoteDiff04+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2004+evanrate, data=Counties)
m2.log0001 <- lm(log(REP04+0.001)~VoteDem04PCT+VoteDiff04+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2004+evanrate, data=Counties)
m2.log05 <- lm(log(REP04+0.5)~VoteDem04PCT+VoteDiff04+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2004+evanrate, data=Counties)
m2.log1 <- lm(log(REP04+1)~VoteDem04PCT+VoteDiff04+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2004+evanrate, data=Counties)
m2.sq <- lm(sqrt(REP04)~VoteDem04PCT+VoteDiff04+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2004+evanrate, data=Counties)
m2.pois <- glm(REP04~VoteDem04PCT+VoteDiff04+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2004+evanrate, data=Counties, family="poisson")
m2.qpois <- glm(REP04~VoteDem04PCT+VoteDiff04+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2004+evanrate, data=Counties, family="quasipoisson")
m2.nb <- glm.nb(REP04~VoteDem04PCT+VoteDiff04+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2004+evanrate, data=Counties)
m2.hurdle <- hurdle(REP04~VoteDem04PCT+VoteDiff04+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2004+evanrate, data=Counties, dist = "negbin")
m2.zero <- zeroinfl(REP04~VoteDem04PCT+VoteDiff04+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2004+evanrate, data=Counties, dist = "negbin")

#Republicans 2004
m2.ls <- lm(REP04~VoteRep04PCT+VoteDiff04+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2004+evanrate, data=Counties)
m2.log0001 <- lm(log(REP04+0.001)~VoteRep04PCT+VoteDiff04+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2004+evanrate, data=Counties)
m2.log05 <- lm(log(REP04+0.5)~VoteRep04PCT+VoteDiff04+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2004+evanrate, data=Counties)
m2.log1 <- lm(log(REP04+1)~VoteRep04PCT+VoteDiff04+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2004+evanrate, data=Counties)
m2.sq <- lm(sqrt(REP04)~VoteRep04PCT+VoteDiff04+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2004+evanrate, data=Counties)
m2.pois <- glm(REP04~VoteRep04PCT+VoteDiff04+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2004+evanrate, data=Counties, family="poisson")
m2.qpois <- glm(REP04~VoteRep04PCT+VoteDiff04+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2004+evanrate, data=Counties, family="quasipoisson")
m2.nb <- glm.nb(REP04~VoteRep04PCT+VoteDiff04+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2004+evanrate, data=Counties)
m2.hurdle <- hurdle(REP04~VoteRep04PCT+VoteDiff04+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2004+evanrate, data=Counties, dist = "negbin")
m2.zero <- zeroinfl(REP04~VoteRep04PCT+VoteDiff04+Pop2010+MedianAge+Female+White+Black+Latino+ForeignBorn+MedHInc+BachelorDegree+Poverty+PopArea+Unemployment2004+evanrate, data=Counties, dist = "negbin")


##############################
#    Game-theoretical models #
##############################

#Make table
table1 <- matrix(NA,5,3)

#
##Proportional to Electoral Votes
###

table1[1,1] <- cor(States$Total04,States$EVotes2004_2008,method = "pearson") #2004
table1[1,2] <- cor(States$Total08,States$EVotes2004_2008,method = "pearson") #2008
table1[1,3] <- cor(States$Total12,States$EVotes2012,method = "pearson") #2012

#
##Proportional to Population
###

table1[2,1] <- cor(States$Total04,States$Pop2010,method = "pearson") #2004
table1[2,2] <- cor(States$Total08,States$Pop2010,method = "pearson") #2008
table1[2,3] <- cor(States$Total12,States$Pop2010,method = "pearson") #2012

#
##Brams & Davis 1974  ##
###(Population to the power of 3/2)

table1[3,1] <- cor(States$Total04,(States$Pop2010)^1.5,method = "pearson") #2004
table1[3,2] <- cor(States$Total08,(States$Pop2010)^1.5,method = "pearson") #2008
table1[3,3] <- cor(States$Total12,(States$Pop2010)^1.5,method = "pearson") #2012

#
##Lake 1979
###(Electoral Votes to the power of 1.72)

table1[4,1] <- cor(States$Total04,(States$EVotes2004_2008)^1.72,method = "pearson") #2004
table1[4,2] <- cor(States$Total08,(States$EVotes2004_2008)^1.72,method = "pearson") #2008
table1[4,3] <- cor(States$Total12,(States$EVotes2012)^1.5,method = "pearson") #2012

#
##Bartels 1986 
###(Electoral Votes to the power of 1.64)

table1[5,1] <- cor(States$Total04,(States$EVotes2004_2008)^1.64,method = "pearson") #2004
table1[5,2] <- cor(States$Total08,(States$EVotes2004_2008)^1.64,method = "pearson") #2008
table1[5,3] <- cor(States$Total12,(States$EVotes2012)^1.64,method = "pearson") #2012

#Format the table:
table1 <- as.data.frame(table1)

names(table1) <- c("2004","2008","2012")
row.names(table1) <- c("Electoral Votes","Population","Brams & Davis","Lake","Bartels")

#LaTeX output
xtable(table1, caption="Correlation (Pearson's R) between resource distribution models and actual distribution")
rm(table1)

# Replication materials
# Ruettenauer + Best (2021): Environmental Inequality and Residential Sorting in Germany
# Run main analyses

rm(list = ls())

library(plyr)
library(dplyr)
library(texreg)
library(spdep)
library(sf)
library(plm)
library(feisr)
library(ggplot2)
library(grid)
library(gridExtra)

# library(extrafont)
# loadfonts()



setwd("../02_Data")


### Load ###

load("EPER_INKAR_2017_reg.RData")
load("Eper_QueensNB_2017.RData")







#__________________________________________________________#
####################### Hausman Test ####################### 
#__________________________________________________________#


feis1.mod <- feis(formula=std_number_ind_perc ~ 
                    l_std_einkommensteuer +
                    W_l_std_einkommensteuer +
                    std_anteilarbeitsloseauslnder +
                    W_std_anteilarbeitsloseauslnder +
                    std_einwohner65jahreundlter + 
                    W_std_einwohner65jahreundlter + 
                    std_einwohner18juj +
                    W_std_einwohner18juj +
                    std_popdens +
                    W_std_popdens +
                    I(std_popdens^2) +
                    W_std_popdens_sq +
                  as.factor(reportingyear)
                  | std_gewerbesteuer + I(std_gewerbesteuer^2) ,
                  data=eper_inkar_final.df,
                  id = "kennziffer", robust = T, tol = 1e-36
)
summary(feis1.mod)
ht1 <- feistest(feis1.mod, robust = TRUE, type = "all")

summary(ht1)


feis2.mod <- feis(formula=std_einkommensteuer ~ 
                    l_std_number_ind_perc +
                    W_l_std_number_ind_perc +
                    std_anteilarbeitsloseauslnder +
                    W_std_anteilarbeitsloseauslnder +
                    std_anteilarbeitsloseauslnder +
                    W_std_anteilarbeitsloseauslnder +
                  std_einwohner65jahreundlter + 
                  W_std_einwohner65jahreundlter + 
                  std_einwohner18juj +
                  W_std_einwohner18juj +
                    std_popdens +
                    W_std_popdens +
                    I(std_popdens^2) +
                  as.factor(reportingyear)  
                  | std_gewerbesteuer + I(std_gewerbesteuer^2) ,
                  data=eper_inkar_final.df,
                  id = "kennziffer", robust = T, tol = 1e-36
)
summary(feis2.mod)
ht2 <- feistest(feis2.mod, robust = TRUE, type = "all")

summary(ht2)






#_____________________________________________________________#
####################### Regressions SLX ####################### 
#_____________________________________________________________#

### Define formulas

# Between models
btw_fm1 <- std_number_ind_perc ~ 
  std_einkommensteuer +
  W_std_einkommensteuer 

btw_fm2 <- std_number_ind_perc ~ 
  std_einkommensteuer +
  W_std_einkommensteuer +
  std_anteilarbeitsloseauslnder + 
  W_std_anteilarbeitsloseauslnder +
  std_gewerbesteuer + 
  W_std_gewerbesteuer +
  I(std_gewerbesteuer^2) + 
  W_std_gewerbesteuer_sq +
  std_einwohner65jahreundlter + 
  W_std_einwohner65jahreundlter + 
  std_einwohner18juj +
  W_std_einwohner18juj + 
  std_popdens +
  W_std_popdens +
  I(std_popdens^2) +
  W_std_popdens_sq 


# Selective siting
feislag1_fm <- as.formula(std_number_ind_perc ~ 
                            l_std_einkommensteuer +
                            W_l_std_einkommensteuer +
                            as.factor(reportingyear)
                          | std_gewerbesteuer + I(std_gewerbesteuer^2)  )


feislag2_fm <- as.formula(std_number_ind_perc ~ 
                            l_std_einkommensteuer +
                            W_l_std_einkommensteuer +
                            std_anteilarbeitsloseauslnder + 
                            W_std_anteilarbeitsloseauslnder +
                            std_einwohner65jahreundlter + 
                            W_std_einwohner65jahreundlter + 
                            std_einwohner18juj +
                            W_std_einwohner18juj +
                            std_popdens +
                            W_std_popdens +
                            I(std_popdens^2) +
                            W_std_popdens_sq +
                            as.factor(reportingyear)
                          | std_gewerbesteuer + I(std_gewerbesteuer^2) )
  
  



# Selective migration
feislag3_fm <- as.formula(std_einkommensteuer ~ 
                            l_std_number_ind_perc +
                            W_l_std_number_ind_perc +
                            as.factor(reportingyear)
                          | std_gewerbesteuer + I(std_gewerbesteuer^2) )


feislag4_fm <- as.formula(std_einkommensteuer ~ 
                            l_std_number_ind_perc +
                            W_l_std_number_ind_perc +
                            std_anteilarbeitsloseauslnder + 
                            W_std_anteilarbeitsloseauslnder +
                            std_einwohner65jahreundlter + 
                            W_std_einwohner65jahreundlter + 
                            std_einwohner18juj +
                            W_std_einwohner18juj +
                            std_popdens +
                            W_std_popdens +
                            I(std_popdens^2) +
                            W_std_popdens_sq +
                            as.factor(reportingyear)
                          | std_gewerbesteuer + I(std_gewerbesteuer^2) )




###########################
#### SLX Between Model #### 
###########################

btw_1.mod <- plm(formula = btw_fm1,
               data=eper_inkar_final.df,
               effect="individual", model="between",
               index=c("kennziffer", "reportingyear")
)

summary(btw_1.mod)


btw_2.mod <- plm(formula = btw_fm2,
               data=eper_inkar_final.df,
               effect="individual", model="between",
               index=c("kennziffer", "reportingyear")
)

summary(btw_2.mod)






###########################
#### Selective siting  #### 
###########################

### FEIS

feislag_1.mod <- feis(formula = feislag1_fm,
                  data=eper_inkar_final.df,
                  id = "kennziffer", robust = T, tol = 1e-36
)

summary(feislag_1.mod)



feislag_2.mod <- feis(formula = feislag2_fm,
                  data=eper_inkar_final.df,
                  id = "kennziffer", robust = T, tol = 1e-36
)

summary(feislag_2.mod)



##############################
#### Selective migration  #### 
##############################


### FEIS

feislag_3.mod <- feis(formula = feislag3_fm,
                  data=eper_inkar_final.df,
                  id = "kennziffer", robust = T, tol = 1e-36
)

summary(feislag_3.mod)



feislag_4.mod <- feis(formula = feislag4_fm,
                  data=eper_inkar_final.df,
                  id = "kennziffer", robust = T, tol = 1e-36
)

summary(feislag_4.mod)




#------------------------------------------------#
#-------------#### WEST GERMANY ####-------------#
#------------------------------------------------#

eper_inkar_final_west.df <- eper_inkar_final.df[eper_inkar_final.df$east==0,]


###########################
#### SLX Between Model #### 
###########################

btw_1.mod_west<-plm(formula = btw_fm1,
               data=eper_inkar_final_west.df,
               effect="individual", model="between",
               index=c("kennziffer", "reportingyear")
)

summary(btw_1.mod_west)


btw_2.mod_west<-plm(formula = btw_fm2,
               data=eper_inkar_final_west.df,
               effect="individual", model="between",
               index=c("kennziffer", "reportingyear")
)

summary(btw_2.mod_west)






###########################
#### Selective siting  #### 
###########################


### FEIS

feislag_1.mod_west<-feis(formula = feislag1_fm,
                    data=eper_inkar_final_west.df,
                    id = "kennziffer", robust = T, tol = 1e-36
)

summary(feislag_1.mod_west)




feislag_2.mod_west<-feis(formula = feislag2_fm,
                    data=eper_inkar_final_west.df,
                    id = "kennziffer", robust = T, tol = 1e-36
)

summary(feislag_2.mod_west)



##############################
#### Selective migration  #### 
##############################


### FEIS

feislag_3.mod_west<-feis(formula = feislag3_fm,
                    data=eper_inkar_final_west.df,
                    id = "kennziffer", robust = T, tol = 1e-36
)

summary(feislag_3.mod_west)



feislag_4.mod_west<-feis(formula = feislag4_fm,
                    data=eper_inkar_final_west.df,
                    id = "kennziffer", robust = T, tol = 1e-36
)

summary(feislag_4.mod_west)




#------------------------------------------------#
#-------------#### EAST GERMANY ####-------------#
#------------------------------------------------#

eper_inkar_final_east.df <- eper_inkar_final.df[eper_inkar_final.df$east==1,]



###########################
#### SLX Between Model #### 
###########################

btw_1.mod_east<-plm(formula = btw_fm1,
                    data=eper_inkar_final_east.df,
                    effect="individual", model="between",
                    index=c("kennziffer", "reportingyear")
)

summary(btw_1.mod_east)


btw_2.mod_east<-plm(formula = btw_fm2,
                    data=eper_inkar_final_east.df,
                    effect="individual", model="between",
                    index=c("kennziffer", "reportingyear")
)

summary(btw_2.mod_east)






###########################
#### Selective siting  #### 
###########################

### FEIS

feislag_1.mod_east<-feis(formula = feislag1_fm,
                         data=eper_inkar_final_east.df,
                         id = "kennziffer", robust = T, tol = 1e-36
)

summary(feislag_1.mod_east)




feislag_2.mod_east<-feis(formula = feislag2_fm,
                         data=eper_inkar_final_east.df,
                         id = "kennziffer", robust = T, tol = 1e-36
)

summary(feislag_2.mod_east)



##############################
#### Selective migration  #### 
##############################


### FEIS

feislag_3.mod_east<-feis(formula = feislag3_fm,
                         data=eper_inkar_final_east.df,
                         id = "kennziffer", robust = T, tol = 1e-36
)

summary(feislag_3.mod_east)



feislag_4.mod_east<-feis(formula = feislag4_fm,
                         data=eper_inkar_final_east.df,
                         id = "kennziffer", robust = T, tol = 1e-36
)

summary(feislag_4.mod_east)








#-------------------------------------------------#
#-------------#### Export Models ####-------------#
#-------------------------------------------------#


##########################
#### Table 1: between ####
##########################


### Between

tex_btw <- texreg(l=list(btw_1.mod, btw_2.mod, btw_1.mod_west, btw_2.mod_west, btw_1.mod_east, btw_2.mod_east), 
       #file="../03_Output/Mod_between.tex",
       digits = 3, leading.zero = TRUE,
       stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "\\dagger",
       label = "tab:between",
       caption = "SLX between estimator. Dep. var.: Number of industrial facilities",
       custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
       custom.coef.map = list('std_einkommensteuer' =  'Income tax revenue',
                              'W_std_einkommensteuer' =  'W Income tax revenue'),
       #custom.note = "%stars. Standard errors in parentheses",
       dcolumn = TRUE, caption.above = TRUE, use.packages=FALSE
)
# Customize
tex_btw <- gsub("D[{].[}][{].[}][{][[:digit:]]+\\.*[[:digit:]]*[}]", "D{.}{.}{3.4}", tex_btw)
 
head <- c("\\begin{table}[ht!]\n\\centering\n\\footnotesize\n{\\begin{threeparttable}")
tex_btw <- gsub("\n\\begin{table}", head, tex_btw, fixed = TRUE)

head2 <- c("\\hline \n  & \\multicolumn{2}{c}{Overall}  & \\multicolumn{2}{c}{West}  & \\multicolumn{2}{c}{East} \\\\ 
           \\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}")
tex_btw <- sub("\\hline", head2, tex_btw, fixed = TRUE)

cont <- paste0("\\hline
 Controls & \\multicolumn{1}{c}{No} & \\multicolumn{1}{c}{Yes} & \\multicolumn{1}{c}{No} & \\multicolumn{1}{c}{Yes} & \\multicolumn{1}{c}{No} & \\multicolumn{1}{c}{Yes} \\\\
 \\hline\n","R$^2$")
tex_btw <- sub("\\hline\nR$^2$", cont, tex_btw, fixed = TRUE)

bottom <- paste0("\\\\hline\n ",
                 "\\\\end{tabular}\n ",
                 "\\\\begin{tablenotes}\n ",
                 "\\\\item \\\\scriptsize{$^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$, $^{\\\\dagger}p<0.1$. Standardized coeffcients. Standard errors in parentheses. W is the spatially lagged coefficient. Controls: \\\\% aged 18 or younger, \\\\% aged 65 or above, population density, population density$^2$, \\\\% of foreigners, trade tax revenue per capita, trade tax revenue per capita$^2$ (all additionally included as spatial lag).}\n",
                 "\\\\end{tablenotes}\n",
                 "\\\\label{tab:between}\n",
                 "\\\\end{center}\n",
                 "\\\\end{threeparttable}\n",
                 "}\n", 
                 "\\\\end{table} \n")
l <- gregexpr("\\hline",tex_btw)
l <- l[[1]][length(l[[1]])]
tex_btw <- substr(tex_btw, 1, (l-2))
tex_btw <- sub("$", bottom, tex_btw, fixed = FALSE)


write.table(tex_btw, file = "../03_Output/Table1.tex",
            col.names = FALSE, row.names = FALSE, quote = FALSE)

htmlreg(l=list( btw_1.mod, btw_2.mod, btw_1.mod_west, btw_2.mod_west, btw_1.mod_east, btw_2.mod_east), 
       file="../03_Output/Table1.doc",
       digits = 3, leading.zero = TRUE,
       stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "&#8224;",
       label = "tab:between",
       caption = "SLX between estimator. Dep. var.: Number of industrial facilities",
       custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
       custom.coef.map = list('std_einkommensteuer' =  'Income tax revenue',
                              'W_std_einkommensteuer' =  'W Income tax revenue'),
       #custom.note = "%stars. Standard errors in parentheses",
       dcolumn = TRUE, caption.above = TRUE, use.packages=FALSE
)






#########################
#### Table 2: Siting ####
#########################

### Only FEIS
tex_sit2 <- texreg(l=list(feislag_2.mod, feislag_2.mod_west, feislag_2.mod_east), 
                  #file="../03_Output/Mod_fe_siting.tex",
                  digits = 3, leading.zero = TRUE,
                  stars = c(0.001, 0.01, 0.05, 0.1),
                  symbol = "\\dagger",
                  label = "tab:siting",
                  caption = "Fixed effects individual slopes (FEIS) estimator. Dep. var.: Number of industrial facilities",
                  custom.model.names = c("(1)", "(2)", "(3)"),
                  #groups=list("Census cell level"=1:6, "Community level"=7:nb),
                  custom.coef.map = list('l_std_einkommensteuer' =  'Income tax revenue$_{t-1}$',
                                         'W_l_std_einkommensteuer' =  'W Income tax revenue$_{t-1}$'),
                  custom.note = "%stars. Cluster robust standard errors in parentheses",
                  dcolumn = TRUE, caption.above = TRUE, use.packages=FALSE, include.rmse = FALSE
)

# Customize
tex_sit2 <- gsub("D[{].[}][{].[}][{][[:digit:]]+\\.*[[:digit:]]*[}]", "D{.}{.}{4.5}", tex_sit2)


head <- c("\\begin{table}[ht!]\n\\centering\n\\footnotesize\n{\\begin{threeparttable}")
tex_sit2 <- gsub("\n\\begin{table}", head, tex_sit2, fixed = TRUE)

head2 <- c("\\hline \n   & \\multicolumn{1}{c}{Overall} & \\multicolumn{1}{c}{West} & \\multicolumn{1}{c}{East}  \\\\
           \\cmidrule(lr){2-4}") 
tex_sit2 <- sub("\\hline", head2, tex_sit2, fixed = TRUE)

cont <- paste0("\\hline
 Controls & \\multicolumn{1}{c}{Yes} & \\multicolumn{1}{c}{Yes} & \\multicolumn{1}{c}{Yes}  \\\\
 \\hline\n","R$^2$")
tex_sit2 <- sub("\\hline\nR$^2$", cont, tex_sit2, fixed = TRUE)

bottom <- paste0("\\\\hline\n ",
                 "\\\\end{tabular}\n ",
                 "\\\\begin{tablenotes}\n ",
                 "\\\\item \\\\scriptsize{$^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$, $^{\\\\dagger}p<0.1$. Standardized coefficients. Cluster robust standard errors in parentheses. W is the spatially lagged coefficient. Controls: \\\\% aged 18 or younger, \\\\% aged 65 or above, population density, population density$^2$, \\\\% of foreigners, year dummies (except year, all additionally included as spatial lag). Slopes for FEIS: trade tax revenue per capita, trade tax revenue per capita$^2$.}\n",
                 "\\\\end{tablenotes}\n",
                 "\\\\label{tab:siting}\n",
                 "\\\\end{center}\n",
                 "\\\\end{threeparttable}\n",
                 "}\n", 
                 "\\\\end{table} \n")
l <- gregexpr("\\hline",tex_sit2)
l <- l[[1]][length(l[[1]])]
tex_sit2 <- substr(tex_sit2, 1, (l-2))
tex_sit2 <- sub("$", bottom, tex_sit2, fixed = FALSE)

tex_sit2 <- sub("kennziffer", "id", tex_sit2, fixed = TRUE)


write.table(tex_sit2, file = "../03_Output/Table2.tex",
            col.names = FALSE, row.names = FALSE, quote = FALSE)

htmlreg(l=list(feislag_2.mod, feislag_2.mod_west, feislag_2.mod_east), 
       file="../03_Output/Table2.doc",
       digits = 3, leading.zero = TRUE,
       stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = "&#8224;",
       label = "tab:siting",
       caption = "Fixed effects individual slopes (FEIS) estimator. Dep. var.: Number of industrial facilities",
       custom.model.names = c("(1)", "(2)", "(3)"),
       #groups=list("Census cell level"=1:6, "Community level"=7:nb),
       custom.coef.map = list('l_std_einkommensteuer' =  'Income tax revenue$_{t-1}$',
                              'W_l_std_einkommensteuer' =  'W Income tax revenue$_{t-1}$'),
       custom.note = "%stars. Cluster robust standard errors in parentheses",
       dcolumn = TRUE, caption.above = TRUE, use.packages=FALSE, include.rmse = FALSE
)








#______________________________________________________________#
####################### Regressions Rank ####################### 
#______________________________________________________________#

### Define formulas



# Selective migration

feisrank3_fm <- as.formula(std_einkommensteuer ~ 
                            l_std_rank +
                            as.factor(reportingyear)
                          | std_gewerbesteuer + I(std_gewerbesteuer^2) )



feisrank4_fm <- as.formula(std_einkommensteuer ~ 
                            l_std_rank +
                            std_anteilarbeitsloseauslnder + 
                            std_einwohner65jahreundlter + 
                            std_einwohner18juj +
                            std_popdens +
                            I(std_popdens^2) +
                            as.factor(reportingyear)
                          | std_gewerbesteuer + I(std_gewerbesteuer^2) )





##############################
#### Selective migration  #### 
##############################



### FEIS

feisrank_3.mod<-feis(formula = feisrank3_fm,
                    data=eper_inkar_final.df,
                    id = "kennziffer", robust = T, tol = 1e-36
)

summary(feisrank_3.mod)



feisrank_4.mod<-feis(formula = feisrank4_fm,
                    data=eper_inkar_final.df,
                    id = "kennziffer", robust = T, tol = 1e-36
)

summary(feisrank_4.mod)




#------------------------------------------------#
#-------------#### WEST GERMANY ####-------------#
#------------------------------------------------#

eper_inkar_final_west.df <- eper_inkar_final.df[eper_inkar_final.df$east==0,]



##############################
#### Selective migration  #### 
##############################


### FEIS

feisrank_3.mod_west<-feis(formula = feisrank3_fm,
                         data=eper_inkar_final_west.df,
                         id = "kennziffer", robust = T, tol = 1e-36
)

summary(feisrank_3.mod_west)



feisrank_4.mod_west<-feis(formula = feisrank4_fm,
                         data=eper_inkar_final_west.df,
                         id = "kennziffer", robust = T, tol = 1e-36
)

summary(feisrank_4.mod_west)




#------------------------------------------------#
#-------------#### EAST GERMANY ####-------------#
#------------------------------------------------#

eper_inkar_final_east.df <- eper_inkar_final.df[eper_inkar_final.df$east==1,]


##############################
#### Selective migration  #### 
##############################


### FEIS

feisrank_3.mod_east<-feis(formula = feisrank3_fm,
                         data=eper_inkar_final_east.df,
                         id = "kennziffer", robust = T, tol = 1e-36
)

summary(feisrank_3.mod_east)



feisrank_4.mod_east<-feis(formula = feisrank4_fm,
                         data=eper_inkar_final_east.df,
                         id = "kennziffer", robust = T, tol = 1e-36
)

summary(feisrank_4.mod_east)








#-------------------------------------------------#
#-------------#### Export Models ####-------------#
#-------------------------------------------------#


### Combine with previous FEIS estimation 


tex_comb <- texreg(l=list(feislag_4.mod, feislag_4.mod_west, feislag_4.mod_east, feisrank_4.mod, feisrank_4.mod_west, feisrank_4.mod_east), 
                   #file="../03_Output/Mod_fe_migration_rank.tex",
                   digits = 3, leading.zero = TRUE,
                   stars = c(0.001, 0.01, 0.05, 0.1),
                   symbol = "\\dagger",
                   label = "table:feis2",
                   caption = "Fixed effects individual slopes (FEIS) estimator Dep. var.: Income tax revenue",
                   custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
                   #groups=list("Census cell level"=1:6, "Community level"=7:nb),
                   custom.coef.map = list('l_std_number_ind_perc' =  'Number facilities$_{t-1}$',
                                          'W_l_std_number_ind_perc' =  'W Number facilities$_{t-1}$',
                                          'l_std_rank' =  'Relative rank$_{t-1}$'),
                   custom.note = "%stars. Cluster robust standard errors in parentheses",
                   dcolumn = TRUE, caption.above = TRUE, use.packages=FALSE, include.rmse = FALSE
)

# Customize
tex_comb <- gsub("D[{].[}][{].[}][{][[:digit:]]+\\.*[[:digit:]]*[}]", "D{.}{.}{3.4}", tex_comb)
tex_comb <- gsub("D[{].[}][{].[}][{][[:digit:]]+\\.*[[:digit:]]*[}]", "D{.}{.}{3.4}", tex_comb)


head <- c("\\begin{table}[ht!]\n\\centering\n\\footnotesize\n{\\begin{threeparttable}")
tex_comb <- gsub("\n\\begin{table}", head, tex_comb, fixed = TRUE)

head2 <- c("\\hline \n  &  \\multicolumn{1}{c}{Overall} & \\multicolumn{1}{c}{West} & \\multicolumn{1}{c}{East} & \\multicolumn{1}{c}{Overall} & \\multicolumn{1}{c}{West} & \\multicolumn{1}{c}{East}  \\\\ 
           \\cmidrule(lr){2-4} \\cmidrule(lr){5-7}") 
tex_comb <- sub("\\hline", head2, tex_comb, fixed = TRUE)

cont <- paste0("\\hline
 Controls & \\multicolumn{1}{c}{Yes} & \\multicolumn{1}{c}{Yes} & \\multicolumn{1}{c}{Yes} & \\multicolumn{1}{c}{Yes} & \\multicolumn{1}{c}{Yes} & \\multicolumn{1}{c}{Yes} \\\\
 \\hline\n","R$^2$")
tex_comb <- sub("\\hline\nR$^2$", cont, tex_comb, fixed = TRUE)

bottom <- paste0("\\\\hline\n ",
                 "\\\\end{tabular}\n ",
                 "\\\\begin{tablenotes}\n ",
                 "\\\\item \\\\scriptsize{$^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$, $^{\\\\dagger}p<0.1$. Standardized coefficients. Cluster robust standard errors in parentheses. W is the spatially lagged coefficient. Controls: \\\\% aged 18 or younger, \\\\% aged 65 or above, population density, population density$^2$, \\\\% of foreigners, year dummies (except year, all additionally included as spatial lag in models 1 - 3). Slopes for FEIS: trade tax revenue per capita, trade tax revenue per capita$^2$.}\n",
                 "\\\\end{tablenotes}\n",
                 "\\\\label{tab:ranking}\n",
                 "\\\\end{center}\n",
                 "\\\\end{threeparttable}\n",
                 "}\n", 
                 "\\\\end{table} \n")
l <- gregexpr("\\hline",tex_comb)
l <- l[[1]][length(l[[1]])]
tex_comb <- substr(tex_comb, 1, (l-2))
tex_comb <- sub("$", bottom, tex_comb, fixed = FALSE)

tex_comb <- sub("kennziffer", "id", tex_comb, fixed = TRUE)


write.table(tex_comb, file = "../03_Output/Table3.tex",
            col.names = FALSE, row.names = FALSE, quote = FALSE)

htmlreg(l=list(feislag_4.mod, feislag_4.mod_west, feislag_4.mod_east, feisrank_4.mod, feisrank_4.mod_west, feisrank_4.mod_east), 
        file="../03_Output/Table3.doc",
        digits = 3, leading.zero = TRUE,
        stars = c(0.001, 0.01, 0.05, 0.1),
        symbol = "&#8224;",
        label = "table:feis2",
        caption = "Fixed effects individual slopes (FEIS) estimator Dep. var.: Income tax revenue",
        custom.model.names = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)"),
        #groups=list("Census cell level"=1:6, "Community level"=7:nb),
        custom.coef.map = list('l_std_number_ind_perc' =  'Number facilities$_{t-1}$',
                               'W_l_std_number_ind_perc' =  'W Number facilities$_{t-1}$',
                               'l_std_rank' =  'Relative rank$_{t-1}$'),
        custom.note = "%stars. Cluster robust standard errors in parentheses",
        dcolumn = TRUE, caption.above = TRUE, use.packages=FALSE, include.rmse = FALSE
)







#___________________________________________________________________#
####################### Regressions Time path ####################### 
#___________________________________________________________________#



#########################
#### Define treshold ####
#########################

sd_within_ind <- mean(by(eper_inkar_final.df$number_ind_perc,
                         eper_inkar_final.df$kennziffer,
                         FUN= function(x) sd(x)))

tresh <- 0.9

###################################################
#### Identify different trajectories over time ####
###################################################

# Make ids numeric
eper_inkar_final.df$kennziffer <- as.numeric(as.character(eper_inkar_final.df$kennziffer))
eper_inkar_final.df$kennziffer <- as.numeric(as.character(eper_inkar_final.df$kennziffer))
eper_inkar_final.df$reportingyear <- as.numeric(as.character(eper_inkar_final.df$reportingyear))
eper_inkar_final.df$reportingyear <- as.numeric(as.character(eper_inkar_final.df$reportingyear))


summary(lm(number_ind_perc ~ as.factor(reportingyear), data.frame(eper_inkar_final.df)))


#### Communities that stay at zero all time

eper_inkar_final.df$nevertreat <- 0
eper_inkar_final.df$nevertreat[eper_inkar_final.df$number_ind_perc == 0] <- 1

T <- length(unique(eper_inkar_final.df$reportingyear))

eper_inkar_final.df$nevertreat <- ave(eper_inkar_final.df$nevertreat,
                                      eper_inkar_final.df$kennziffer,
                                      FUN = function(x) sum(x))

eper_inkar_final.df$nevertreat[eper_inkar_final.df$nevertreat < T] <- 0
eper_inkar_final.df$nevertreat[eper_inkar_final.df$nevertreat == T] <- 1

table(eper_inkar_final.df$nevertreat)


#### Decreases and increases

sd_within_ind <- mean(by(eper_inkar_final.df$number_ind_perc,
                         eper_inkar_final.df$kennziffer,
                         FUN= function(x) sd(x)))

### Gen facility increase
# sd_within_ind/1
eper_inkar_final.df <- eper_inkar_final.df[order(eper_inkar_final.df$kennziffer, eper_inkar_final.df$reportingyear), ]

eper_inkar_final.df$delta_number_ind_perc <- ave(eper_inkar_final.df$number_ind_perc,
                                                 eper_inkar_final.df$kennziffer,
                                                 FUN = function(x) x - dplyr::lag(x))

eper_inkar_final.df$number_ind_perc_inc <- 0
eper_inkar_final.df$number_ind_perc_inc[eper_inkar_final.df$delta_number_ind_perc>=(tresh)] <- 1
eper_inkar_final.df$number_ind_perc_inc[is.na(eper_inkar_final.df$delta_number_ind_perc)] <- NA
eper_inkar_final.df$number_ind_perc_dec <- 0
eper_inkar_final.df$number_ind_perc_dec[eper_inkar_final.df$delta_number_ind_perc<=-(tresh)] <- 1
eper_inkar_final.df$number_ind_perc_dec[is.na(eper_inkar_final.df$delta_number_ind_perc)] <- NA

#### Count number of instances
eper_inkar_final.df$inc_nr<- ave(eper_inkar_final.df$number_ind_perc_inc, 
                                 eper_inkar_final.df$kennziffer,
                                 FUN = function(x) c(0, cumsum(x[-1])))
eper_inkar_final.df$dec_nr<- ave(eper_inkar_final.df$number_ind_perc_dec, 
                                 eper_inkar_final.df$kennziffer,
                                 FUN = function(x) c(0, cumsum(x[-1])))  

eper_inkar_final.df$nr <- eper_inkar_final.df$inc_nr + eper_inkar_final.df$dec_nr


# for legacy reasons, rename
eper_inkar_final.df$number_ind_perc_inc3 <- eper_inkar_final.df$number_ind_perc_inc
eper_inkar_final.df$number_ind_perc_dec3 <- eper_inkar_final.df$number_ind_perc_dec


#### Count number of instances for new indicator
eper_inkar_final.df$inc_nr2 <- ave(eper_inkar_final.df$number_ind_perc_inc3, 
                                   eper_inkar_final.df$kennziffer,
                                   FUN = function(x) c(0, cumsum(x[-1])))
eper_inkar_final.df$dec_nr2 <- ave(eper_inkar_final.df$number_ind_perc_dec3, 
                                   eper_inkar_final.df$kennziffer,
                                   FUN = function(x) c(0, cumsum(x[-1])))  

eper_inkar_final.df$nr2 <- eper_inkar_final.df$inc_nr2 + eper_inkar_final.df$dec_nr2



#### indicator for fist increase and decrease

eper_inkar_final.df$inc_first <- 0
eper_inkar_final.df$inc_first[is.na(eper_inkar_final.df$number_ind_perc_inc)] <- NA
eper_inkar_final.df$inc_first[which(eper_inkar_final.df$number_ind_perc_inc3 == 1 &
                                      eper_inkar_final.df$inc_nr2 == 1)] <- 1

eper_inkar_final.df$dec_first <- 0
eper_inkar_final.df$dec_first[is.na(eper_inkar_final.df$number_ind_perc_dec)] <- NA
eper_inkar_final.df$dec_first[which(eper_inkar_final.df$number_ind_perc_dec3 == 1 &
                                      eper_inkar_final.df$dec_nr2 == 1)] <- 1




##############################
#### Generate time counts ####
##############################


### Time count function
time_count <- function(x, incidence = 1, first = FALSE, ...){
  ind <- which(x == incidence)
  l <- length(x)
  if(length(ind) == 0){
    res <- rep(0, times = l)
  }else{
    if(first == TRUE){
      beg <- 1
    }else{
      beg <- 0
    }
    st <- rep(0, times = (ind[1] - 1)) # zero till first incidence
    sp <- unlist(lapply(c(ind[-1], (l + 1)) - ind, FUN = function(x) c(beg:(x - (1 - beg)) )))
    res <- c(st, sp)
  }
  return(res)
}





#####################
#### Count years ####
#####################

tmp <- eper_inkar_final.df[, c("kennziffer", "reportingyear", "inc_first", "dec_first")]
tmp <- apply(tmp, 2, FUN = function(x) as.numeric(as.character(x)))
tmp <- data.frame(tmp)

### Increase
tmp$years_inc_ind <- ave(tmp$inc_first,
                         tmp$kennziffer,
                         FUN = function(x) time_count(x, 1))

# Years before
tmp$years_inc_ind_dbefore <- ave(tmp$inc_first,
                                 tmp$kennziffer,
                                 FUN = function(x) rev(time_count(rev(x), 1)))


### Decrease
tmp$years_dec_ind <- ave(tmp$dec_first,
                         tmp$kennziffer,
                         FUN = function(x) time_count(x, 1))

# Years before
tmp$years_dec_ind_dbefore <- ave(tmp$dec_first,
                                 tmp$kennziffer,
                                 FUN = function(x) rev(time_count(rev(x), 1)))



### Merge
count <- tmp[, c("kennziffer", "reportingyear", "years_inc_ind", "years_inc_ind_dbefore", "years_dec_ind", "years_dec_ind_dbefore")]
eper_inkar_final.df <- merge(eper_inkar_final.df, count, by = c("kennziffer", "reportingyear"))








#### Make dummys ####


for(i in c(1:(length(table(eper_inkar_final.df$years_inc_ind))-1))){
  tmp <- data.frame(rep(NA, nrow(eper_inkar_final.df)))
  tmp[which(!is.na(eper_inkar_final.df$years_inc_ind) & eper_inkar_final.df$years_inc_ind == i),] <- 1
  tmp[which(!is.na(eper_inkar_final.df$years_inc_ind) & eper_inkar_final.df$years_inc_ind != i),] <- 0
  names(tmp) <- paste("years_inc_ind", i, sep = "_")
  eper_inkar_final.df <- cbind(eper_inkar_final.df, tmp)
}
for(i in c(1:(length(table(eper_inkar_final.df$years_dec_ind))-1))){
  tmp <- data.frame(rep(NA, nrow(eper_inkar_final.df)))
  tmp[which(!is.na(eper_inkar_final.df$years_dec_ind) & eper_inkar_final.df$years_dec_ind == i),] <- 1
  tmp[which(!is.na(eper_inkar_final.df$years_dec_ind) & eper_inkar_final.df$years_dec_ind != i),] <- 0
  names(tmp) <- paste("years_dec_ind", i, sep = "_")
  eper_inkar_final.df <- cbind(eper_inkar_final.df, tmp)
}
for(i in c(1:(length(table(eper_inkar_final.df$years_inc_ind_dbefore))-1))){
  tmp <- data.frame(rep(NA, nrow(eper_inkar_final.df)))
  tmp[which(!is.na(eper_inkar_final.df$years_inc_ind_dbefore) & eper_inkar_final.df$years_inc_ind_dbefore == i),] <- 1
  tmp[which(!is.na(eper_inkar_final.df$years_inc_ind_dbefore) & eper_inkar_final.df$years_inc_ind_dbefore != i),] <- 0
  names(tmp) <- paste("years_inc_ind_dbefore", i, sep = "_")
  eper_inkar_final.df <- cbind(eper_inkar_final.df, tmp)
}
for(i in c(1:(length(table(eper_inkar_final.df$years_dec_ind_dbefore))-1))){
  tmp <- data.frame(rep(NA, nrow(eper_inkar_final.df)))
  tmp[which(!is.na(eper_inkar_final.df$years_dec_ind_dbefore) & eper_inkar_final.df$years_dec_ind_dbefore == i),] <- 1
  tmp[which(!is.na(eper_inkar_final.df$years_dec_ind_dbefore) & eper_inkar_final.df$years_dec_ind_dbefore != i),] <- 0
  names(tmp) <- paste("years_dec_ind_dbefore", i, sep = "_")
  eper_inkar_final.df <- cbind(eper_inkar_final.df, tmp)
}



#### Make spatial lags ####

vars<-c("inc_first", "dec_first", "years_inc_ind", "years_dec_ind",
        "years_inc_ind_dbefore_1", "years_inc_ind_dbefore_2", "years_inc_ind_dbefore_3",
        "years_inc_ind_dbefore_4", "years_inc_ind_dbefore_5", "years_inc_ind_dbefore_6",
        "years_inc_ind_dbefore_7" , "years_inc_ind_dbefore_8",       
        "years_inc_ind_1", "years_inc_ind_2", "years_inc_ind_3",
        "years_inc_ind_4", "years_inc_ind_5", "years_inc_ind_6",
        "years_inc_ind_7" , "years_inc_ind_8", 
        "years_dec_ind_dbefore_1", "years_dec_ind_dbefore_2", "years_dec_ind_dbefore_3", 
        "years_dec_ind_dbefore_4", "years_dec_ind_dbefore_5", "years_dec_ind_dbefore_6", 
        "years_dec_ind_dbefore_7", "years_dec_ind_dbefore_8", 
        "years_dec_ind_1", "years_dec_ind_2", "years_dec_ind_3", 
        "years_dec_ind_4", "years_dec_ind_5", "years_dec_ind_6", 
        "years_dec_ind_7", "years_dec_ind_8" 
)



### Gen WX
wx.df <- eper_inkar_final.df[, c("kennziffer", "reportingyear")]

wx.df <- cbind(wx.df, (matrix(NA, nrow=nrow(wx.df), ncol=length(vars))))
names(wx.df)[-c(1:2)] <- paste("W_", vars, sep="")



# Loop over for each year
for(y in unique(eper_inkar_final.df$reportingyear)){
  tmp.df <- eper_inkar_final.df[eper_inkar_final.df$reportingyear == y, ]
  rownames(tmp.df) <- as.character(tmp.df$kennziffer)
  ids <- tmp.df$kennziffer
  
  # # Subset lw to years ob
  W <- listw2mat(eper.lw)
  # keep <- which(rownames(W) %in% ids)
  # W <- W[keep, keep]
  # #tmp.lw <- mat2listw(W)
  
  # Gen WX
  for(i in vars){
    wx <- NA # start with NA to avoid using last vars vector (in case of error)
    x <- tmp.df[, which(names(tmp.df)==i), drop=F]
    # x <- x[match(rownames(W), rownames(x)), ,drop=F]
    # if(any(is.na(x))){ # Drop NAs (this should be entire cities, thus allow unbalanced)
    #   r <- which(!is.na(x))
    #   x <- x[r, , drop =F]
    #   W <- W[r, r]
    # }
    wx <-  W %*% as.vector(x[match(rownames(W), rownames(x)), ])
    oo <- match(wx.df$kennziffer[wx.df$reportingyear == y], as.numeric(rownames(wx)))
    wx <- wx[oo, ]
    wx.df[wx.df$reportingyear == y, which(names(wx.df)==paste("W_", i, sep=""))] <- wx
  }
  
}

### Bind SLX
eper_inkar_final.df <- merge(eper_inkar_final.df, wx.df, 
                             by = c("kennziffer", "reportingyear"))








#----------------------------------------------#
#-------------#### Regression ####-------------#
#----------------------------------------------#



##############################
#### Time Path Regression ####
##############################


feislag_3b.mod<-feis(formula=std_einkommensteuer ~ 
                       inc_first + 
                       I(as.factor(years_inc_ind)) + 
                       years_inc_ind_dbefore_1 + 
                       dec_first + 
                       I(as.factor(years_dec_ind)) + 
                       years_dec_ind_dbefore_1 + 
                       W_inc_first + 
                       W_years_inc_ind_1 + W_years_inc_ind_2 + W_years_inc_ind_3 + W_years_inc_ind_4 + W_years_inc_ind_5 + W_years_inc_ind_6 + W_years_inc_ind_7 + W_years_inc_ind_8 + # W_years_inc_ind_9 + 
                       W_years_inc_ind_dbefore_1 + 
                       W_dec_first + 
                       W_years_dec_ind_1 + W_years_dec_ind_2 + W_years_dec_ind_3 + W_years_dec_ind_4 + W_years_dec_ind_5 + W_years_dec_ind_6 + W_years_dec_ind_7 + W_years_dec_ind_8 + # W_years_dec_ind_9 + 
                       W_years_dec_ind_dbefore_1 + 
                       std_anteilarbeitsloseauslnder +
                       W_std_anteilarbeitsloseauslnder +
                       std_popdens +
                       W_std_popdens +
                       I(std_popdens^2) +
                       W_std_popdens_sq +
                       std_einwohner65jahreundlter + 
                       W_std_einwohner65jahreundlter + 
                       std_einwohner18juj +
                       W_std_einwohner18juj +
                       as.factor(reportingyear)
                     | std_gewerbesteuer + I(std_gewerbesteuer^2) ,
                     data=eper_inkar_final.df,
                     id = "kennziffer", robust = T, tol = 1e-36
                     
)

summary(feislag_3b.mod)





######################
#### West Germany ####
######################

eper_inkar_final.df_west <- eper_inkar_final.df[eper_inkar_final.df$east==0,]

feislag_3b_west.mod<-feis(formula=std_einkommensteuer ~ 
                            inc_first + 
                            I(as.factor(years_inc_ind)) + 
                            years_inc_ind_dbefore_1 + 
                            dec_first + 
                            I(as.factor(years_dec_ind)) + 
                            years_dec_ind_dbefore_1 + 
                            W_inc_first + 
                            W_years_inc_ind_1 + W_years_inc_ind_2 + W_years_inc_ind_3 + W_years_inc_ind_4 + W_years_inc_ind_5 + W_years_inc_ind_6 + W_years_inc_ind_7 + W_years_inc_ind_8 + # W_years_inc_ind_9 + 
                            W_years_inc_ind_dbefore_1 + 
                            W_dec_first + 
                            W_years_dec_ind_1 + W_years_dec_ind_2 + W_years_dec_ind_3 + W_years_dec_ind_4 + W_years_dec_ind_5 + W_years_dec_ind_6 + W_years_dec_ind_7 + W_years_dec_ind_8 + # W_years_dec_ind_9 + 
                            W_years_dec_ind_dbefore_1 + 
                            std_anteilarbeitsloseauslnder +
                            W_std_anteilarbeitsloseauslnder +
                            std_popdens +
                            W_std_popdens +
                            I(std_popdens^2) +
                            W_std_popdens_sq +
                            std_einwohner65jahreundlter + 
                            W_std_einwohner65jahreundlter + 
                            std_einwohner18juj +
                            W_std_einwohner18juj +
                            as.factor(reportingyear)
                          | std_gewerbesteuer + I(std_gewerbesteuer^2) ,
                          data=eper_inkar_final.df_west,
                          id = "kennziffer", robust = T, tol = 1e-36
                          
)

summary(feislag_3b_west.mod)





######################
#### East Germany ####
######################

eper_inkar_final.df_east <- eper_inkar_final.df[eper_inkar_final.df$east==1,]

feislag_3b_east.mod<-feis(formula=std_einkommensteuer ~ 
                            inc_first + 
                            I(as.factor(years_inc_ind)) + 
                            years_inc_ind_dbefore_1 + 
                            dec_first + 
                            I(as.factor(years_dec_ind)) + 
                            years_dec_ind_dbefore_1 + 
                            W_inc_first + 
                            W_years_inc_ind_1 + W_years_inc_ind_2 + W_years_inc_ind_3 + W_years_inc_ind_4 + W_years_inc_ind_5 + W_years_inc_ind_6 + W_years_inc_ind_7 + W_years_inc_ind_8 + # W_years_inc_ind_9 + 
                            W_years_inc_ind_dbefore_1 + 
                            W_dec_first + 
                            W_years_dec_ind_1 + W_years_dec_ind_2 + W_years_dec_ind_3 + W_years_dec_ind_4 + W_years_dec_ind_5 + W_years_dec_ind_6 + W_years_dec_ind_7 + W_years_dec_ind_8 + # W_years_dec_ind_9 + 
                            W_years_dec_ind_dbefore_1 + 
                            std_anteilarbeitsloseauslnder +
                            W_std_anteilarbeitsloseauslnder +
                            std_popdens +
                            W_std_popdens +
                            I(std_popdens^2) +
                            W_std_popdens_sq +
                            std_einwohner65jahreundlter + 
                            W_std_einwohner65jahreundlter + 
                            std_einwohner18juj +
                            W_std_einwohner18juj +
                            as.factor(reportingyear)
                          | std_gewerbesteuer + I(std_gewerbesteuer^2) ,
                          data=eper_inkar_final.df_east,
                          id = "kennziffer", robust = T, tol = 1e-36
                          
)

summary(feislag_3b_east.mod)





#----------------------------------------#
#-------------#### Plot ####-------------#
#----------------------------------------#



######################
#### West Germany ####
######################


#### FEIS ####


unicode_minus = function(x) sub('^-', '\U2212', format(x))

#### Increase ####

coef.df<-data.frame(matrix(NA, ncol=3, nrow=10))
names(coef.df)<-c("year", "coef", "se")
coef.df$year<-seq(-1,8)

modsum<-summary(feislag_3b_west.mod)$coefficients

# Paste values
coef.df[1,2:3]<-modsum[which(row.names(modsum)=="years_inc_ind_dbefore_1"),1:2]
coef.df[2,2:3]<-modsum[which(row.names(modsum)=="inc_first"),1:2]
coef.df[3,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_inc_ind))1"),1:2]
coef.df[4,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_inc_ind))2"),1:2]
coef.df[5,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_inc_ind))3"),1:2]
coef.df[6,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_inc_ind))4"),1:2]
coef.df[7,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_inc_ind))5"),1:2]
coef.df[8,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_inc_ind))6"),1:2]
coef.df[9,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_inc_ind))7"),1:2]
coef.df[10,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_inc_ind))8"),1:2]

# Plot
interval<- -qnorm((1-0.95)/2)  # 95% multiplier
coef.df$lwr=coef.df$coef-interval*coef.df$se
coef.df$upr=coef.df$coef+interval*coef.df$se


p1 <- ggplot(coef.df, aes(year, coef))+
  geom_point()+
  geom_line(data=coef.df)+
  geom_ribbon(data=coef.df,aes(ymin=lwr,ymax=upr),alpha=0.3)
p1<- p1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)+
  geom_vline(xintercept = -0.5, colour = gray(1/2), lty = 1)
p1<- p1 + scale_y_continuous(breaks=c(seq(-.3,0,.05), seq(.05,.3,.05)), labels = unicode_minus)
p1<- p1 + scale_x_continuous(limits=c(-1, 8), breaks=seq(-1,8))
p1<- p1 + labs(y="Change of Income Tax Revenue \n (standard deviation)",
               x="Years Since Increase",
               title = "a. Increase") 
p1<- p1 + theme_bw()
p1  <-  p1 + theme(text = element_text(size=16),
                   legend.position="bottom",
                   legend.key=element_blank(),
                   plot.title = element_text(hjust = 0.5, face  = "bold"),
                   axis.text.y=element_text(colour="black"),
                   axis.title.x=element_text(size=16, colour="black", margin = margin(t = 10, r = 0 , b = 0, l = 0), face = "bold"),
                   axis.title.y=element_text(size=16, colour="black", margin = margin(t = 0, r = 10 , b = 0, l = 0), face = "bold"),
                   axis.text.x = element_text(colour="black"),
                   #axis.text.y = element_text(size=16),
                   #axis.title.x = element_text(size=16),
                   #axis.title.y = element_text(size=16)
)
# p1<- p1 + ggtitle("West Germany")
p1



#### Decrease ####

coef.df<-data.frame(matrix(NA, ncol=3, nrow=10))
names(coef.df)<-c("year", "coef", "se")
coef.df$year<-seq(-1,8)

modsum<-summary(feislag_3b_west.mod)$coefficients

# Paste values
coef.df[1,2:3]<-modsum[which(row.names(modsum)=="years_dec_ind_dbefore_1"),1:2]
coef.df[2,2:3]<-modsum[which(row.names(modsum)=="dec_first"),1:2]
coef.df[3,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_dec_ind))1"),1:2]
coef.df[4,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_dec_ind))2"),1:2]
coef.df[5,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_dec_ind))3"),1:2]
coef.df[6,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_dec_ind))4"),1:2]
coef.df[7,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_dec_ind))5"),1:2]
coef.df[8,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_dec_ind))6"),1:2]
coef.df[9,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_dec_ind))7"),1:2]
coef.df[10,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_dec_ind))8"),1:2]


# Plot
interval<- -qnorm((1-0.95)/2)  # 95% multiplier
coef.df$lwr=coef.df$coef-interval*coef.df$se
coef.df$upr=coef.df$coef+interval*coef.df$se


p2 <- ggplot(coef.df, aes(year, coef))+
  geom_point()+
  geom_line(data=coef.df)+
  geom_ribbon(data=coef.df,aes(ymin=lwr,ymax=upr),alpha=0.3)
p2<- p2 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)+
  geom_vline(xintercept = -0.5, colour = gray(1/2), lty = 1)
p2<- p2 + scale_x_continuous(limits=c(-1, 8), breaks=seq(-1,8))
p2<- p2 + scale_y_continuous(breaks=c(seq(-.3,0,.05), seq(.05,.3,.05)), labels = unicode_minus)
p2<- p2 + labs(y="Change of Income Tax Revenue \n (standard deviation)",
               x="Years Since Decrease",
               title = "b. Decrease") 
p2<- p2 + theme_bw()
p2  <-  p2 + theme(text = element_text(size=16),
                   legend.position="bottom",
                   legend.key=element_blank(),
                   axis.text.y=element_text(colour="black"),
                   plot.title = element_text(hjust = 0.5, face  = "bold"),
                   axis.title.x=element_text(size=16, colour="black", margin = margin(t = 10, r = 0 , b = 0, l = 0), face = "bold"),
                   axis.title.y=element_text(size=16, colour="black", margin = margin(t = 0, r = 10 , b = 0, l = 0), face = "bold"),
                   axis.text.x = element_text(colour="black"),
                   #axis.text.y = element_text(size=16),
                   #axis.title.x = element_text(size=16),
                   #axis.title.y = element_text(size=16)
)
# p2<- p2 + ggtitle("West Germany")
p2



# Export combined
cairo_pdf(file="../03_Output/Figure3.pdf", width=14, height=5, bg = "white", 
          #family="CM Roman"
          )
par(mar=c(0,0,0,0))
par(mfrow=c(1,1),oma=c(0,0,0,0))
grid.arrange(p1, p2, ncol = 2)
dev.off()





######################
#### East Germany ####
######################


#### FEIS ####

#### Increase ####

coef.df<-data.frame(matrix(NA, ncol=3, nrow=10))
names(coef.df)<-c("year", "coef", "se")
coef.df$year<-seq(-1,8)

modsum<-summary(feislag_3b_east.mod)$coefficients

# Paste values
coef.df[1,2:3]<-modsum[which(row.names(modsum)=="years_inc_ind_dbefore_1"),1:2]
coef.df[2,2:3]<-modsum[which(row.names(modsum)=="inc_first"),1:2]
coef.df[3,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_inc_ind))1"),1:2]
coef.df[4,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_inc_ind))2"),1:2]
coef.df[5,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_inc_ind))3"),1:2]
coef.df[6,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_inc_ind))4"),1:2]
coef.df[7,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_inc_ind))5"),1:2]
coef.df[8,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_inc_ind))6"),1:2]
coef.df[9,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_inc_ind))7"),1:2]
coef.df[10,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_inc_ind))8"),1:2]

# Plot
interval<- -qnorm((1-0.95)/2)  # 95% multiplier
coef.df$lwr=coef.df$coef-interval*coef.df$se
coef.df$upr=coef.df$coef+interval*coef.df$se


p1 <- ggplot(coef.df, aes(year, coef))+
  geom_point()+
  geom_line(data=coef.df)+
  geom_ribbon(data=coef.df,aes(ymin=lwr,ymax=upr),alpha=0.3)
p1<- p1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)+
  geom_vline(xintercept = -0.5, colour = gray(1/2), lty = 1)
p1<- p1 + scale_x_continuous(limits=c(-1, 8), breaks=seq(-1,8))
p1<- p1 + scale_y_continuous(breaks=c(seq(-.3,0,.05), seq(.05,.3,.05)), labels = unicode_minus)
p1<- p1 + labs(y="Change of Income Tax Revenue \n (standard deviation)",
               x="Years Since Increase",
               title = "a. Increase") 
p1<- p1 + theme_bw()
p1  <-  p1 + theme(text = element_text(size=16),
                   legend.position="bottom",
                   legend.key=element_blank(),
                   plot.title = element_text(hjust = 0.5, face = "bold"),
                   axis.text.y=element_text(colour="black"),
                   axis.title.x=element_text(size=16, colour="black", margin = margin(t = 10, r = 0 , b = 0, l = 0), face = "bold"),
                   axis.title.y=element_text(size=16, colour="black", margin = margin(t = 0, r = 10 , b = 0, l = 0), face = "bold"),
                   axis.text.x = element_text(colour="black"),
                   #axis.text.y = element_text(size=16),
                   #axis.title.x = element_text(size=16),
                   #axis.title.y = element_text(size=16)
)
# p1<- p1 + ggtitle("East Germany")
p1





#### Decrease ####

coef.df<-data.frame(matrix(NA, ncol=3, nrow=10))
names(coef.df)<-c("year", "coef", "se")
coef.df$year<-seq(-1,8)

modsum<-summary(feislag_3b_east.mod)$coefficients

# Paste values
coef.df[1,2:3]<-modsum[which(row.names(modsum)=="years_dec_ind_dbefore_1"),1:2]
coef.df[2,2:3]<-modsum[which(row.names(modsum)=="dec_first"),1:2]
coef.df[3,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_dec_ind))1"),1:2]
coef.df[4,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_dec_ind))2"),1:2]
coef.df[5,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_dec_ind))3"),1:2]
coef.df[6,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_dec_ind))4"),1:2]
coef.df[7,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_dec_ind))5"),1:2]
coef.df[8,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_dec_ind))6"),1:2]
coef.df[9,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_dec_ind))7"),1:2]
coef.df[10,2:3]<-modsum[which(row.names(modsum)=="I(as.factor(years_dec_ind))8"),1:2]


# Plot
interval<- -qnorm((1-0.95)/2)  # 95% multiplier
coef.df$lwr=coef.df$coef-interval*coef.df$se
coef.df$upr=coef.df$coef+interval*coef.df$se


p2 <- ggplot(coef.df, aes(year, coef))+
  geom_point()+
  geom_line(data=coef.df)+
  geom_ribbon(data=coef.df,aes(ymin=lwr,ymax=upr),alpha=0.3)
p2<- p2 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)+
  geom_vline(xintercept = -0.5, colour = gray(1/2), lty = 1)
p2<- p2 + scale_x_continuous(limits=c(-1, 8), breaks=seq(-1,8))
p2<- p2 + scale_y_continuous(breaks=c(seq(-.3,0,.05), seq(.05,.3,.05)), labels = unicode_minus)
p2<- p2 + labs(y="Change of Income Tax Revenue \n (standard deviation)",
               x="Years Since Decrease",
               title = "b. Decrease") 
p2<- p2 + theme_bw()
p2  <-  p2 + theme(text = element_text(size=16),
                   legend.position="bottom",
                   legend.key=element_blank(),
                   plot.title = element_text(hjust = 0.5, face = "bold"),
                   axis.text.y=element_text(colour="black"),
                   axis.title.x=element_text(size=16, colour="black", margin = margin(t = 10, r = 0 , b = 0, l = 0), face = "bold"),
                   axis.title.y=element_text(size=16, colour="black", margin = margin(t = 0, r = 10 , b = 0, l = 0), face = "bold"),
                   axis.text.x = element_text(colour="black"),
                   #axis.text.y = element_text(size=16),
                   #axis.title.x = element_text(size=16),
                   #axis.title.y = element_text(size=16)
)
# p2<- p2 + ggtitle("East Germany")
p2




# Export combined
cairo_pdf(file="../03_Output/Figure4.pdf", width=14, height=5, bg = "white", 
          #family="CM Roman"
          )
par(mar=c(0,0,0,0))
par(mfrow=c(1,1),oma=c(0,0,0,0))
grid.arrange(p1, p2, ncol = 2)
dev.off()



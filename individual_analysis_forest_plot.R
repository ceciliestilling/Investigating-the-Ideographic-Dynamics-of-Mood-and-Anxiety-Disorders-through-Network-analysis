##### LIBRARIES #####
library(graphicalVAR)
library(qgraph)
library(tidyverse)
library(rmeta)
library(forestplot)

setwd("C:/Users/cecil/OneDrive/AU/Bachelorprojekt/Time-series angst og depression/R Data/R Data")


filenames <- list.files(pattern = "\\.csv", full.names = TRUE)

vars <- c("energetic","enthusiastic","content","irritable","restless",
          "worried","guilty","afraid","anhedonia","angry","hopeless",
          "down","positive","fatigue","tension","concentrate","accepted",
          "threatened","ruminate","avoid_act","reassure","procrast","avoid_people")

analyze <- function(filenames) {
  # Input is character string of a csv file.
  data <- read.csv(file = filenames, header = TRUE)
  data_symp <- data[,c(2,4:25,29)]
  
  
  # Encode time variable in a way R understands
  data_symp$start <- as.POSIXct(data_symp$start,
                                tryFormats = c("10/21/2014 17:55",
                                               "%m/%d/%Y %H:%M"),
                                optional = FALSE)
  
  # Extract days:
  data_symp$date <- as.Date(data_symp$start)
  
  lm_var1 <- lm(energetic ~ start, data = data_symp)
  #summary(lm_var1)
  #plot(lm_var1)
  
  lm_var2 <- lm(enthusiastic ~ start, data = data_symp)
  #summary(lm_var2)
  #plot(lm_var2)
  
  lm_var3 <- lm(content ~ start, data = data_symp)
  #summary(lm_var3)
  #plot(lm_var3)
  
  lm_var4 <- lm(irritable ~ start, data = data_symp)
  #summary(lm_var4)
  #plot(lm_var4)
  
  lm_var5 <- lm(restless ~ start, data = data_symp)
  #summary(lm_var5)
  #plot(lm_var5)
  
  lm_var6 <- lm(worried ~ start, data = data_symp)
  #summary(lm_var6)
  #plot(lm_var6)
  
  lm_var7 <- lm(guilty ~ start, data = data_symp)
  #summary(lm_var7)
  #plot(lm_var7)
  
  lm_var8 <- lm(afraid ~ start, data = data_symp)
  #summary(lm_var8)
  #plot(lm_var8)
  
  lm_var9 <- lm(anhedonia ~ start, data = data_symp)
  #summary(lm_var9)
  #plot(lm_var9)
  
  lm_var10 <- lm(angry ~ start, data = data_symp)
  #summary(lm_var10)
  #plot(lm_var10)
  
  lm_var11 <- lm(hopeless ~ start, data = data_symp)
  #summary(lm_var11)
  #plot(lm_var11)
  
  lm_var12 <- lm(down ~ start, data = data_symp)
  #summary(lm_var12)
  #plot(lm_var12)
  
  lm_var13 <- lm(positive ~ start, data = data_symp)
  #summary(lm_var13)
  #plot(lm_var13)
  
  lm_var14 <- lm(fatigue ~ start, data = data_symp)
  #summary(lm_var14)
  #plot(lm_var14)
  
  lm_var15 <- lm(tension ~ start, data = data_symp)
  #summary(lm_var15)
  #plot(lm_var15)
  
  lm_var16 <- lm(concentrate ~ start, data = data_symp)
  #summary(lm_var16)
  #plot(lm_var16)
  
  lm_var17 <- lm(accepted ~ start, data = data_symp)
  #summary(lm_var17)
  #plot(lm_var17)
  
  lm_var18 <- lm(threatened ~ start, data = data_symp)
  #summary(lm_var18)
  #plot(lm_var18)
  
  lm_var19 <- lm(ruminate ~ start, data = data_symp)
  #summary(lm_var19)
  #plot(lm_var19)
  
  lm_var20 <- lm(avoid_act ~ start, data = data_symp)
  #summary(lm_var20)
  #plot(lm_var20)
  
  lm_var21 <- lm(reassure ~ start, data = data_symp)
  #summary(lm_var21)
  #plot(lm_var21)
  
  lm_var22 <- lm(procrast ~ start, data = data_symp)
  #summary(lm_var22)
  #plot(lm_var22)
  
  #lm_var23 <- lm(hours ~ start, data = data_symp)
  #summary(lm_var23)
  #plot(lm_var23)
  
  #lm_var24 <- lm(difficult ~ start, data = data_symp)
  #summary(lm_var24)
  #plot(lm_var24)
  
  #lm_var25 <- lm(unsatisfy ~ start, data = data_symp)
  #summary(lm_var25)
  #plot(lm_var25)
  
  lm_var26 <- lm(avoid_people ~ start, data = data_symp)
  #summary(lm_var26)
  #plot(lm_var26)
  #detrend 
  data_symp$energetic[!is.na(data_symp$energetic)] <- residuals(lm_var1)
  data_symp$enthusiastic[!is.na(data_symp$enthusiastic)] <- residuals(lm_var2)
  data_symp$content[!is.na(data_symp$content)] <- residuals(lm_var3)
  data_symp$irritable[!is.na(data_symp$irritable)] <- residuals(lm_var4)
  data_symp$restless[!is.na(data_symp$restless)] <- residuals(lm_var5)
  data_symp$worried[!is.na(data_symp$worried)] <- residuals(lm_var6)
  data_symp$guilty[!is.na(data_symp$guilty)] <- residuals(lm_var7)
  data_symp$afraid[!is.na(data_symp$afraid)] <- residuals(lm_var8)
  data_symp$anhedonia[!is.na(data_symp$anhedonia)] <- residuals(lm_var9)
  data_symp$angry[!is.na(data_symp$angry)] <- residuals(lm_var10)
  data_symp$hopeless[!is.na(data_symp$hopeless)] <- residuals(lm_var11)
  data_symp$down[!is.na(data_symp$down)] <- residuals(lm_var12)
  data_symp$positive[!is.na(data_symp$positive)] <- residuals(lm_var13)
  data_symp$fatigue[!is.na(data_symp$fatigue)] <- residuals(lm_var14)
  data_symp$tension[!is.na(data_symp$tension)] <- residuals(lm_var15)
  data_symp$concentrate[!is.na(data_symp$concentrate)] <- residuals(lm_var16)
  data_symp$accepted[!is.na(data_symp$accepted)] <- residuals(lm_var17)
  data_symp$threatened[!is.na(data_symp$threatened)] <- residuals(lm_var18)
  data_symp$ruminate[!is.na(data_symp$ruminate)] <- residuals(lm_var19)
  data_symp$avoid_act[!is.na(data_symp$avoid_act)] <- residuals(lm_var20)
  data_symp$reassure[!is.na(data_symp$reassure)] <- residuals(lm_var21)
  data_symp$procrast[!is.na(data_symp$procrast)] <- residuals(lm_var22)
  #data_symp$hours[!is.na(data_symp$hours)] <- residuals(lm_var23)
  #data_symp$difficult[!is.na(data_symp$difficult)] <- residuals(lm_var24)
  #data_symp$unsatisfy[!is.na(data_symp$unsatisfy)] <- residuals(lm_var25)
  data_symp$avoid_people[!is.na(data_symp$avoid_people)] <- residuals(lm_var26)
  
  ##### SCALE DATA WITHOUT START AND DATE #####
  
  #remove NA
  data_symp_clean <- na.omit(data_symp)
  
  #selcting columns to exclude
  col_excl <- c("start", "date")
  
  # index vector of columns which must not be scaled
  index <- names(data_symp_clean) %in% col_excl
  
  #actually scaling the shit
  data_symp_clean_sc <- as.data.frame(scale(data_symp_clean[, !index], center = TRUE, scale = TRUE))
  
  #adding the columns again
  data_final <- data_symp_clean_sc
  data_final$start <- data_symp_clean$start
  data_final$date <- data_symp_clean$date
  
  ##### ANALYSIS #####
  vars <- c("energetic","enthusiastic","content","irritable","restless",
            "worried","guilty","afraid","anhedonia","angry","hopeless",
            "down","positive","fatigue","tension","concentrate","accepted",
            "threatened","ruminate","avoid_act","reassure","procrast","avoid_people")
  
  
  # Resample using spline (uncomment to use spline method, see https://osf.io/zefbc/ ):
  for (v in seq_along(vars)){
    data_final[[vars[v]]] <- (spline(data_final$start,data_final[[vars[v]]],
                                     nrow(data_final),method='fmm'))$y
  }
  
  
  
  #run analysis (excluding nights using dayvar = 'date'):
  res <- graphicalVAR(data_final, 
                      gamma = 0,
                      vars = vars,
                      nLambda = 10,
                      dayvar = "date")
  
  return(res)
  }


#worried
df_wrr_PCC <- data.frame(row.names = 23)
df_wrr_PDC <- data.frame(row.names = 23)

#ruminate
df_rmn_PCC <- data.frame(row.names = 23)
df_rmn_PDC <- data.frame(row.names = 23)

#down
df_dwn_PCC <- data.frame(row.names = 23)
df_dwn_PDC <- data.frame(row.names = 23)

#avoid_act
df_av_act_PCC <- data.frame(row.names = 23)
df_av_act_PDC <- data.frame(row.names = 23)

#hopeless
df_hpl_PCC <- data.frame(row.names = 23)
df_hpl_PDC <- data.frame(row.names = 23)

name <- c()


cent_cont <- c()
cent_temp <- c()

for(f in 1:3){
  
  filen <- filenames[f]
  res <- analyze(filenames = filen)
  name[f] <- substr(filenames[f],3,6)
  
  contemporaneous <- qgraph(res$PCC, fade = FALSE, 
                            labels = gsub("\\.","\n",vars),
                            layout = "spring", 
                            theme = "colorblind",
                            title = name[f],
                            height=5,
                            width=10,
                            filetype = "pdf",
                            filename = paste(name[f],"cont", sep = "_"))
  
  temporal <- qgraph(res$PDC, fade = FALSE, 
                     labels = gsub("\\.","\n",vars),
                     layout = "spring", 
                     theme = "colorblind",
                     title = name[f],
                     height=5,
                     width=10,
                     filetype = "pdf",
                     filename = paste(name[f],"temp", sep = "_"))
  
  path <- file.path("C://Users//cecil//OneDrive//AU//Bachelorprojekt//Time-series angst og depression//R Data//R Data//Plots//Centrality plots//Individual centrality plots",
                    paste(name[f],"_cent_cont", ".pdf", sep = ""))
  
  pdf(file=path)
  mytitle = paste(name[f])
  centralityPlot(contemporaneous,
                 include = c("Strength",
                             "Betweenness",
                             "Closeness"))
  
  
  dev.off()
  
  
  path2 <- file.path("C://Users//cecil//OneDrive//AU//Bachelorprojekt//Time-series angst og depression//R Data//R Data//Plots//Centrality plots//Individual centrality plots",
                     paste(name[f],"_cent_temp", ".pdf", sep = ""))
  pdf(path2)
  mytitle = paste(name[f])
  centralityPlot(temporal,
                 include = c("InStrength",
                             "OutStrength"))
  dev.off()
  
  df_wrr_PCC <- data.frame(df_wrr_PCC , name = res$PCC[,"worried"] )
  df_wrr_PDC <- data.frame(df_wrr_PDC , name = res$PDC[,"worried"] )
  
  df_rmn_PCC <- data.frame(df_rmn_PCC , name = res$PCC[,"ruminate"] )
  df_rmn_PDC <- data.frame(df_rmn_PDC , name = res$PDC[,"ruminate"] )
  
  df_dwn_PCC <- data.frame(df_dwn_PCC , name = res$PCC[,"down"] )
  df_dwn_PDC <- data.frame(df_dwn_PDC , name = res$PDC[,"down"] )
  
  df_av_act_PCC <- data.frame(df_av_act_PCC , name = res$PCC[,"avoid_act"] )
  df_av_act_PDC <- data.frame(df_av_act_PDC , name = res$PDC[,"avoid_act"] )
  
  df_hpl_PCC <- data.frame(df_hpl_PCC , name = res$PCC[,"hopeless"] )
  df_hpl_PDC <- data.frame(df_hpl_PDC , name = res$PDC[,"hopeless"] )
  
  names(df_wrr_PCC)[f] <- substr(filenames[f],3,6)
  names(df_wrr_PDC)[f] <- substr(filenames[f],3,6)
  
  names(df_rmn_PCC)[f] <- substr(filenames[f],3,6)
  names(df_rmn_PDC)[f] <- substr(filenames[f],3,6)
  
  names(df_dwn_PCC)[f] <- substr(filenames[f],3,6)
  names(df_dwn_PDC)[f] <- substr(filenames[f],3,6)
  
  names(df_av_act_PCC)[f] <- substr(filenames[f],3,6)
  names(df_av_act_PDC)[f] <- substr(filenames[f],3,6)
  
  names(df_hpl_PCC)[f] <- substr(filenames[f],3,6)
  names(df_hpl_PDC)[f] <- substr(filenames[f],3,6)
  
  
  if( (f %% 10) == 0){
    print(f)
  }
}


centrality(res$PCC)

##### FOREST PLOTS #####


##### FP - WORRIED #####

#remove participant with 0
df_wrr_PCC$P040 <- NULL
df_wrr_PCC$P163 <- NULL

#remove those participants from name too
wrr_name1 <- name[name!= "P040"]
wrr_name <- wrr_name1[wrr_name1!= "P163"]

wrr_name

wrr_sd_PDC <- c()
wrr_m_PDC <- c()
wrr_lower_PDC <- c()
wrr_upper_PDC <- c()


wrr_sd_PCC <- c()
wrr_m_PCC <- c()
wrr_lower_PCC <- c()
wrr_upper_PCC <- c()


for(i in 1:40){
  #PDC for worried
  wrr_PDC = df_wrr_PDC[,i]
  wrr_PDC[wrr_PDC==0] <- NA
  wrr_PDC <- wrr_PDC[!is.na(wrr_PDC)]
  wrr_m_PDC[i] <- mean(wrr_PDC)
  wrr_sd_PDC[i] <- sd(wrr_PDC)
  wrr_lower_PDC[i] <- wrr_m_PDC[i] - 1.96*wrr_sd_PDC[i]
  wrr_upper_PDC[i] <- wrr_m_PDC[i] + 1.96*wrr_sd_PDC[i]
  
  
  #PCC for worried
  wrr_PCC = df_wrr_PCC[,i]
  wrr_PCC[wrr_PCC==0] <- NA
  wrr_PCC <- wrr_PCC[!is.na(wrr_PCC)]
  wrr_m_PCC[i] <- mean(wrr_PCC)
  wrr_sd_PCC[i] <- sd(wrr_PCC)
  wrr_lower_PCC[i] <- wrr_m_PCC[i] - 1.96*wrr_sd_PCC[i]
  wrr_upper_PCC[i] <- wrr_m_PCC[i] + 1.96*wrr_sd_PCC[i]
}


wrr_f_plot_PDC <- data.frame("mean" = c(NA, wrr_m_PDC) , "lower" = c(NA, wrr_lower_PDC) , "upper" = c(NA, wrr_upper_PDC))
wrr_f_plot_PCC <- data.frame("mean" = c(NA, wrr_m_PCC) , "lower" = c(NA, wrr_lower_PCC) , "upper" = c(NA, wrr_upper_PCC))


wrr_tabletext20 <- cbind(c( "Participants:" , wrr_name[1:20] , NA , "Generalized"), 
                       c( "Mean", round(wrr_m_PCC[1:20], digits = 3), NA, 0.029),
                       c( "SD", round(wrr_sd_PCC[1:20], digits = 3), NA, 0.058),
                       c( "Lower", round(wrr_lower_PCC[1:20], digits = 3), NA, -0.084),
                       c( "Upper", round(wrr_upper_PCC[1:20], digits = 3), NA, 0.142))


wrr_tabletext40 <- cbind(c( "Participants:" , wrr_name[21:38] , NA , "Generalized"), 
                         c( "Mean", round(wrr_m_PCC[21:38], digits = 3), NA, 0.029),
                         c( "SD", round(wrr_sd_PCC[21:38], digits = 3), NA, 0.058),
                         c( "Lower", round(wrr_lower_PCC[21:38], digits = 3), NA, -0.084),
                         c( "Upper", round(wrr_upper_PCC[21:38], digits = 3), NA, 0.142)
                         )



#FP over contemporaneous



forestplot(wrr_tabletext20 , mean = c(NA,wrr_m_PCC[1:20],NA,0.02877048) ,
           lower = c(NA,wrr_lower_PCC[1:20],NA,-0.08442642) ,
           upper = c(NA,wrr_upper_PCC[1:20],NA,0.1419674),
           is.summary=c(TRUE,rep(FALSE,20),TRUE), 
           xlog=FALSE, title = "Worried", 
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue"),
           hrzl_lines = gpar(col="#444444"))



forestplot(wrr_tabletext40 , mean = c(NA,wrr_m_PCC[21:38],NA,0.02877048) ,
           lower = c(NA,wrr_lower_PCC[21:38],NA,-0.08442642) ,
           upper = c(NA,wrr_upper_PCC[21:38],NA,0.1419674),
           is.summary=c(TRUE,rep(FALSE,18),TRUE), 
           xlog=FALSE, title = "Worried", 
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue"),
           hrzl_lines = gpar(col="#444444"))

##### FP - RUMINATION #####

#remove participant with 0
df_rmn_PCC$P003 <- NULL
df_rmn_PCC$P009 <- NULL
df_rmn_PCC$P013 <- NULL
df_rmn_PCC$P019 <- NULL
df_rmn_PCC$P040 <- NULL
df_rmn_PCC$P037 <- NULL
df_rmn_PCC$P139 <- NULL
df_rmn_PCC$P163 <- NULL
df_rmn_PCC$P202 <- NULL
df_rmn_PCC$P204 <- NULL

#remove those participants from name too
rmn_name1 <- name[name!= "P003"]
rmn_name2 <- rmn_name1[rmn_name1!= "P009"]
rmn_name3 <- rmn_name2[rmn_name2!= "P013"]
rmn_name4 <- rmn_name3[rmn_name3!= "P019"]
rmn_name5 <- rmn_name4[rmn_name4!= "P040"]
rmn_name6 <- rmn_name5[rmn_name5!= "P037"]
rmn_name7 <- rmn_name6[rmn_name6!= "P139"]
rmn_name8 <- rmn_name7[rmn_name7!= "P163"]
rmn_name9 <- rmn_name8[rmn_name8!= "P202"]
rmn_name <- rmn_name9[rmn_name9!= "P204"]

rmn_name


rmn_sd_PDC <- c()
rmn_m_PDC <- c()
rmn_lower_PDC <- c()
rmn_upper_PDC <- c()

rmn_sd_PCC <- c()
rmn_m_PCC <- c()
rmn_lower_PCC <- c()
rmn_upper_PCC <- c()

for(i in 1:40){
  #PDC for rumination
  rmn_PDC = df_rmn_PDC[,i]
  rmn_PDC[rmn_PCC==0] <- NA
  rmn_PDC <- rmn_PDC[!is.na(rmn_PDC)]
  rmn_m_PDC[i] <- mean(rmn_PDC)
  rmn_sd_PDC[i] <- sd(rmn_PDC)
  rmn_lower_PDC[i] <- rmn_m_PDC[i] - 1.96*rmn_sd_PDC[i]
  rmn_upper_PDC[i] <- rmn_m_PDC[i] + 1.96*rmn_sd_PDC[i]
  
  #PCC for rumination
  rmn_PCC = df_rmn_PCC[,i]
  rmn_PCC[rmn_PCC==0] <- NA
  rmn_PCC <- rmn_PCC[!is.na(rmn_PCC)]
  rmn_m_PCC[i] <- mean(rmn_PCC)
  rmn_sd_PCC[i] <- sd(rmn_PCC)
  rmn_lower_PCC[i] <- rmn_m_PCC[i] - 1.96*rmn_sd_PCC[i]
  rmn_upper_PCC[i] <- rmn_m_PCC[i] + 1.96*rmn_sd_PCC[i]
}




rmn_f_plot_PDC <- data.frame("mean" = c(NA, rmn_m_PDC) , "lower" = c(NA, rmn_lower_PDC) , "upper" = c(NA, rmn_upper_PDC))
rmn_f_plot_PCC <- data.frame("mean" = c(NA, rmn_m_PCC) , "lower" = c(NA, rmn_lower_PCC) , "upper" = c(NA, rmn_upper_PCC))


rmn_tabletext15 <- cbind(c( "Participants:" , rmn_name[1:15] , NA , "Generalized"), 
                         c( "Mean", round(rmn_m_PCC[1:15], digits = 3), NA, 0.023),
                         c( "SD", round(rmn_sd_PCC[1:15], digits = 3), NA, 0.037),
                         c( "Lower", round(rmn_lower_PCC[1:15], digits = 3), NA, -0.051),
                         c( "Upper", round(rmn_upper_PCC[1:15], digits = 3), NA, 0.096))

rmn_tabletext30 <- cbind(c( "Participants:" , rmn_name[16:30] , NA , "Generalized"), 
                         c( "Mean", round(rmn_m_PCC[16:30], digits = 3), NA, 0.023),
                         c( "SD", round(rmn_sd_PCC[16:30], digits = 3), NA, 0.037),
                         c( "Lower", round(rmn_lower_PCC[16:30], digits = 3), NA, -0.051),
                         c( "Upper", round(rmn_upper_PCC[16:30], digits = 3), NA, 0.096))

#FP over contemporaneous



forestplot(rmn_tabletext15 , mean = c(NA,rmn_m_PCC[1:15],NA,0.02262946) ,
           lower = c(NA,rmn_lower_PCC[1:15],NA,-0.05076336) ,
           upper = c(NA,rmn_upper_PCC[1:15],NA,0.09602227),
           is.summary=c(TRUE,rep(FALSE,15),TRUE),
           xlog=FALSE, title = "Ruminate", 
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue"),
           hrzl_lines = gpar(col="#444444"))


forestplot(rmn_tabletext30 , mean = c(NA,rmn_m_PCC[16:30],NA,0.02262946) ,
           lower = c(NA,rmn_lower_PCC[16:30],NA,-0.05076336) ,
           upper = c(NA,rmn_upper_PCC[16:30],NA,0.09602227),
           is.summary=c(TRUE,rep(FALSE,15),TRUE),
           xlog=FALSE, title = "Ruminate", 
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue"),
           hrzl_lines = gpar(col="#444444"))




##### FP - DOWN #####

#remove participant with 0
df_dwn_PCC$P023 <- NULL

#remove those participants from name too
dwn_name <- name[name!= "P023"]

dwn_name

dwn_sd_PDC <- c()
dwn_m_PDC <- c()
dwn_lower_PDC <- c()
dwn_upper_PDC <- c()

dwn_sd_PCC <- c()
dwn_m_PCC <- c()
dwn_lower_PCC <- c()
dwn_upper_PCC <- c()

for(i in 1:40){
  #PDC for down
  dwn_PDC = df_dwn_PDC[,i]
  dwn_PDC[dwn_PDC==0] <- NA
  dwn_PDC <- dwn_PDC[!is.na(dwn_PDC)]
  dwn_m_PDC[i] <- mean(dwn_PDC)
  dwn_sd_PDC[i] <- sd(dwn_PDC)
  dwn_lower_PDC[i] <- dwn_m_PDC[i] - 1.96*dwn_sd_PDC[i]
  dwn_upper_PDC[i] <- dwn_m_PDC[i] + 1.96*dwn_sd_PDC[i]
  
  #PCC for down
  dwn_PCC = df_dwn_PCC[,i]
  dwn_PCC[dwn_PCC==0] <- NA
  dwn_PCC <- dwn_PCC[!is.na(dwn_PCC)]
  dwn_m_PCC[i] <- mean(dwn_PCC)
  dwn_sd_PCC[i] <- sd(dwn_PCC)
  dwn_lower_PCC[i] <- dwn_m_PCC[i] - 1.96*dwn_sd_PCC[i]
  dwn_upper_PCC[i] <- dwn_m_PCC[i] + 1.96*dwn_sd_PCC[i]
}




dwn_f_plot_PDC <- data.frame("mean" = c(NA, dwn_m_PDC) , "lower" = c(NA, dwn_lower_PDC) , "upper" = c(NA, dwn_upper_PDC))
dwn_f_plot_PCC <- data.frame("mean" = c(NA, dwn_m_PCC) , "lower" = c(NA, dwn_lower_PCC) , "upper" = c(NA, dwn_upper_PCC))



dwn_tabletext20 <- cbind(c( "Participants:" , dwn_name[1:20] , NA , "Generalized"), 
                         c( "Mean", round(dwn_m_PCC[1:20], digits = 3), NA, 0.033),
                         c( "SD", round(dwn_sd_PCC[1:20], digits = 3), NA, 0.067),
                         c( "Lower", round(dwn_lower_PCC[1:20], digits = 3), NA, -0.098),
                         c( "Upper", round(dwn_upper_PCC[1:20], digits = 3), NA, 0.165))


dwn_tabletext40 <- cbind(c( "Participants:" , dwn_name[21:40] , NA , "Generalized"), 
                         c( "Mean", round(dwn_m_PCC[21:40], digits = 3), NA, 0.033),
                         c( "SD", round(dwn_sd_PCC[21:40], digits = 3), NA, 0.067),
                         c( "Lower", round(dwn_lower_PCC[21:40], digits = 3), NA, -0.098),
                         c( "Upper", round(dwn_upper_PCC[21:40], digits = 3), NA, 0.165))

#FP over contemporaneous




forestplot(dwn_tabletext20 , mean = c(NA,dwn_m_PCC[1:20],NA,0.03329012) ,
           lower = c(NA,dwn_lower_PCC[1:20],NA,-0.09842987) ,
           upper = c(NA,dwn_upper_PCC[1:20],NA,0.1650101),
           is.summary=c(TRUE,rep(FALSE,20),TRUE),
           xlog=FALSE, title = "Down", 
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue"),
           hrzl_lines = gpar(col="#444444"))

forestplot(dwn_tabletext40 , mean = c(NA,dwn_m_PCC[21:40],NA,0.03329012) ,
           lower = c(NA,dwn_lower_PCC[21:40],NA,-0.09842987) ,
           upper = c(NA,dwn_upper_PCC[21:40],NA,0.1650101),
           is.summary=c(TRUE,rep(FALSE,20),TRUE),
           xlog=FALSE, title = "Down", 
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue"),
           hrzl_lines = gpar(col="#444444"))



##### FP - AVOID ACTIONS #####

#remove participant with 0
df_av_act_PCC$P006 <- NULL
df_av_act_PCC$P007 <- NULL
df_av_act_PCC$P204 <- NULL
df_av_act_PCC$P215 <- NULL

#remove those participants from name too
av_act_name1 <- name[name!= "P006"]
av_act_name2 <- av_act_name1[av_act_name1!= "P007"]
av_act_name3 <- av_act_name2[av_act_name2!= "P204"]
av_act_name <- av_act_name3[av_act_name3!= "P215"]
av_act_name

av_act_sd_PDC <- c()
av_act_m_PDC <- c()
av_act_lower_PDC <- c()
av_act_upper_PDC <- c()

av_act_sd_PCC <- c()
av_act_m_PCC <- c()
av_act_lower_PCC <- c()
av_act_upper_PCC <- c()

for(i in 1:40){
  #PDC for avoid action
  av_act_PDC = df_av_act_PDC[,i]
  av_act_PDC[av_act_PDC==0] <- NA
  av_act_PDC <- av_act_PDC[!is.na(av_act_PDC)]
  av_act_m_PDC[i] <- mean(av_act_PDC)
  av_act_sd_PDC[i] <- sd(av_act_PDC)
  av_act_lower_PDC[i] <- av_act_m_PDC[i] - 1.96*av_act_sd_PDC[i]
  av_act_upper_PDC[i] <- av_act_m_PDC[i] + 1.96*av_act_sd_PDC[i]
  
  #PCC for avoid action
  av_act_PCC = df_av_act_PCC[,i]
  av_act_PCC[av_act_PCC==0] <- NA
  av_act_PCC <- av_act_PCC[!is.na(av_act_PCC)]
  av_act_m_PCC[i] <- mean(av_act_PCC)
  av_act_sd_PCC[i] <- sd(av_act_PCC)
  av_act_lower_PCC[i] <- av_act_m_PCC[i] - 1.96*av_act_sd_PCC[i]
  av_act_upper_PCC[i] <- av_act_m_PCC[i] + 1.96*av_act_sd_PCC[i]
}




av_act_f_plot_PDC <- data.frame("mean" = c(NA, av_act_m_PDC) , "lower" = c(NA, av_act_lower_PDC) , "upper" = c(NA, av_act_upper_PDC))
av_act_f_plot_PCC <- data.frame("mean" = c(NA, av_act_m_PCC) , "lower" = c(NA, av_act_lower_PCC) , "upper" = c(NA, av_act_upper_PCC))


av_act_tabletext20 <- cbind(c( "Participants:" , av_act_name[1:20] , NA , "Generalized"), 
                            c( "Mean", round(av_act_m_PCC[1:20], digits = 3), NA, 0.026),
                            c( "SD", round(av_act_sd_PCC[1:20], digits = 3), NA, 0.089),
                            c( "Lower", round(av_act_lower_PCC[1:20], digits = 3), NA, -0.148),
                            c( "Upper", round(av_act_upper_PCC[1:20], digits = 3), NA, 0.200))


av_act_tabletext40 <- cbind(c( "Participants:" , av_act_name[21:37] , NA , "Generalized"), 
                            c( "Mean", round(av_act_m_PCC[21:37], digits = 3), NA, 0.026),
                            c( "SD", round(av_act_sd_PCC[21:37], digits = 3), NA, 0.089),
                            c( "Lower", round(av_act_lower_PCC[21:37], digits = 3), NA, -0.148),
                            c( "Upper", round(av_act_upper_PCC[21:37], digits = 3), NA, 0.200))

#FP over contemporaneous



forestplot(av_act_tabletext20 , mean = c(NA,av_act_m_PCC[1:20],NA,0.02600758) ,
           lower = c(NA,av_act_lower_PCC[1:20],NA,-0.1480614) ,
           upper = c(NA,av_act_upper_PCC[1:20],NA,0.2001705),
           is.summary=c(TRUE,rep(FALSE,20),TRUE),
           xlog=FALSE, title = "Avoid Action", 
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue"),
           hrzl_lines = gpar(col="#444444"))

forestplot(av_act_tabletext40 , mean = c(NA,av_act_m_PCC[21:37],NA,0.02600758) ,
           lower = c(NA,av_act_lower_PCC[21:37],NA,-0.1480614) ,
           upper = c(NA,av_act_upper_PCC[21:37],NA,0.2000766),
           is.summary=c(TRUE,rep(FALSE,17),TRUE),
           xlog=FALSE, title = "Avoid Action", 
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue"),
           hrzl_lines = gpar(col="#444444"))

##### FP - HOPLESS #####


#remove participant with 0
df_hpl_PCC$P007 <- NULL
df_hpl_PCC$P012 <- NULL
df_hpl_PCC$P048 <- NULL
df_hpl_PCC$P137 <- NULL
df_hpl_PCC$P139 <- NULL
df_hpl_PCC$P145 <- NULL

#remove those participants from name too
hpl_name1 <- name[name!= "P007"]
hpl_name2 <- hpl_name1[hpl_name1!= "P012"]
hpl_name3 <- hpl_name2[hpl_name2!= "P048"]
hpl_name4 <- hpl_name3[hpl_name3!= "P139"]
hpl_name5 <- hpl_name4[hpl_name4!= "P137"]
hpl_name <- hpl_name5[hpl_name5!= "P145"]

hpl_name

hpl_sd_PDC <- c()
hpl_m_PDC <- c()
hpl_lower_PDC <- c()
hpl_upper_PDC <- c()

hpl_sd_PCC <- c()
hpl_m_PCC <- c()
hpl_lower_PCC <- c()
hpl_upper_PCC <- c()

for(i in 1:40){
  #PDC for hopeless
  hpl_PDC = df_hpl_PDC[,i]
  hpl_PDC[hpl_PDC==0] <- NA
  hpl_PDC <- hpl_PDC[!is.na(hpl_PDC)]
  hpl_m_PDC[i] <- mean(hpl_PDC)
  hpl_sd_PDC[i] <- sd(hpl_PDC)
  hpl_lower_PDC[i] <- hpl_m_PDC[i] - 1.96*hpl_sd_PDC[i]
  hpl_upper_PDC[i] <- hpl_m_PDC[i] + 1.96*hpl_sd_PDC[i]
  
  #PCC for hopeless
  hpl_PCC = df_hpl_PCC[,i]
  hpl_PCC[hpl_PCC==0] <- NA
  hpl_PCC <- hpl_PCC[!is.na(hpl_PCC)]
  hpl_m_PCC[i] <- mean(hpl_PCC)
  hpl_sd_PCC[i] <- sd(hpl_PCC)
  hpl_lower_PCC[i] <- hpl_m_PCC[i] - 1.96*hpl_sd_PCC[i]
  hpl_upper_PCC[i] <- hpl_m_PCC[i] + 1.96*hpl_sd_PCC[i]
}




hpl_f_plot_PDC <- data.frame("mean" = c(NA, hpl_m_PDC) , "lower" = c(NA, hpl_lower_PDC) , "upper" = c(NA, hpl_upper_PDC))
hpl_f_plot_PCC <- data.frame("mean" = c(NA, hpl_m_PCC) , "lower" = c(NA, hpl_lower_PCC) , "upper" = c(NA, hpl_upper_PCC))



hpl_tabletext20 <- cbind(c( "Participants:" , hpl_name[1:20] , NA , "Generalized"), 
                         c( "Mean", round(hpl_m_PCC[1:20], digits = 3), NA, 0.033),
                         c( "SD", round(hpl_sd_PCC[1:20], digits = 3), NA, 0.057),
                         c( "Lower", round(hpl_lower_PCC[1:20], digits = 3), NA, -0.079),
                         c( "Upper", round(hpl_upper_PCC[1:20], digits = 3), NA, 0.144))


hpl_tabletext40 <- cbind(c( "Participants:" , hpl_name[21:35] , NA , "Generalized"), 
                         c( "Mean", round(hpl_m_PCC[21:35], digits = 3), NA, 0.033),
                         c( "SD", round(hpl_sd_PCC[21:35], digits = 3), NA, 0.057),
                         c( "Lower", round(hpl_lower_PCC[21:35], digits = 3), NA, -0.079),
                         c( "Upper", round(hpl_upper_PCC[21:35], digits = 3), NA, 0.144))

#FP over contemporaneous



forestplot(hpl_tabletext20 , mean = c(NA,hpl_m_PCC[1:20],NA,0.03272013) ,
           lower = c(NA,hpl_lower_PCC[1:20],NA,-0.07866474) ,
           upper = c(NA,hpl_upper_PCC[1:20],NA,0.144105),
           is.summary=c(TRUE,rep(FALSE,20),TRUE),
           xlog=FALSE, title = "Hopeless", 
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue"),
           hrzl_lines = gpar(col="#444444"))

forestplot(hpl_tabletext40 , mean = c(NA,hpl_m_PCC[21:35],NA,0.03272013) ,
           lower = c(NA,hpl_lower_PCC[21:35],NA,-0.07866474) ,
           upper = c(NA,hpl_upper_PCC[21:35],NA,0.144105),
           is.summary=c(TRUE,rep(FALSE,15),TRUE),
           xlog=FALSE, title = "Hopeless", 
           col=fpColors(box="royalblue",line="darkblue", summary="royalblue"),
           hrzl_lines = gpar(col="#444444"))





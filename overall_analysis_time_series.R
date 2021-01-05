##### OVERALL TIME SERIES ANALYSIS #####

##### GET READY #####
library(mlVAR)
library(tictoc)
library(qgraph)
library(bootnet)
library(lme4)

#working directory
setwd("C:/Users/cecil/OneDrive/AU/Bachelorprojekt/Time-series angst og depression/R Data/R Data/all data")


##### DATA PREP #####
data_all <-  read.csv("all_data_id.csv", header = TRUE)

#select only variables and remove the 3 variables which have only been registered once a day
data_all_symp <- data_all[,c(2,4,6:27,31)]

# Encode time variable in a way R understands
data_all_symp$start <- as.POSIXct(data_all_symp$start,
                              tryFormats = c("10/21/2014 17:55",
                                             "%m/%d/%Y %H:%M"),
                              optional = FALSE)

# Extract days:
data_all_symp$date <- as.Date(data_all_symp$start)


##### LINEARITY AND DETRENDING #####

#Check for linerarity for each variable
lm_var1 <- lm(energetic ~ start, data = data_all_symp)
#summary(lm_var1)
#plot(lm_var1)

lm_var2 <- lm(enthusiastic ~ start, data = data_all_symp)
#summary(lm_var2)
#plot(lm_var2)

lm_var3 <- lm(content ~ start, data = data_all_symp)
#summary(lm_var3)
#plot(lm_var3)

lm_var4 <- lm(irritable ~ start, data = data_all_symp)
#summary(lm_var4)
#plot(lm_var4)

lm_var5 <- lm(restless ~ start, data = data_all_symp)
#summary(lm_var5)
#plot(lm_var5)

lm_var6 <- lm(worried ~ start, data = data_all_symp)
#summary(lm_var6)
#plot(lm_var6)

lm_var7 <- lm(guilty ~ start, data = data_all_symp)
#summary(lm_var7)
#plot(lm_var7)

lm_var8 <- lm(afraid ~ start, data = data_all_symp)
#summary(lm_var8)
#plot(lm_var8)

lm_var9 <- lm(anhedonia ~ start, data = data_all_symp)
#summary(lm_var9)
#plot(lm_var9)

lm_var10 <- lm(angry ~ start, data = data_all_symp)
#summary(lm_var10)
#plot(lm_var10)

lm_var11 <- lm(hopeless ~ start, data = data_all_symp)
#summary(lm_var11)
#plot(lm_var11)

lm_var12 <- lm(down ~ start, data = data_all_symp)
#summary(lm_var12)
#plot(lm_var12)

lm_var13 <- lm(positive ~ start, data = data_all_symp)
#summary(lm_var13)
#plot(lm_var13)

lm_var14 <- lm(fatigue ~ start, data = data_all_symp)
#summary(lm_var14)
#plot(lm_var14)

lm_var15 <- lm(tension ~ start, data = data_all_symp)
#summary(lm_var15)
#plot(lm_var15)

lm_var16 <- lm(concentrate ~ start, data = data_all_symp)
#summary(lm_var16)
#plot(lm_var16)

lm_var17 <- lm(accepted ~ start, data = data_all_symp)
#summary(lm_var17)
#plot(lm_var17)

lm_var18 <- lm(threatened ~ start, data = data_all_symp)
#summary(lm_var18)
#plot(lm_var18)

lm_var19 <- lm(ruminate ~ start, data = data_all_symp)
#summary(lm_var19)
#plot(lm_var19)

lm_var20 <- lm(avoid_act ~ start, data = data_all_symp)
#summary(lm_var20)
#plot(lm_var20)

lm_var21 <- lm(reassure ~ start, data = data_all_symp)
#summary(lm_var21)
#plot(lm_var21)

lm_var22 <- lm(procrast ~ start, data = data_all_symp)
#summary(lm_var22)
#plot(lm_var22)

#lm_var23 <- lm(hours ~ start, data = data_all_symp)
#summary(lm_var23)
#plot(lm_var23)

#lm_var24 <- lm(difficult ~ start, data = data_all_symp)
#summary(lm_var24)
#plot(lm_var24)

#lm_var25 <- lm(unsatisfy ~ start, data = data_all_symp)
#summary(lm_var25)
#plot(lm_var25)

lm_var26 <- lm(avoid_people ~ start, data = data_all_symp)
#summary(lm_var26)
#plot(lm_var26)

#detrend 
data_all_symp$energetic[!is.na(data_all_symp$energetic)] <- residuals(lm_var1)
data_all_symp$enthusiastic[!is.na(data_all_symp$enthusiastic)] <- residuals(lm_var2)
data_all_symp$content[!is.na(data_all_symp$content)] <- residuals(lm_var3)
data_all_symp$irritable[!is.na(data_all_symp$irritable)] <- residuals(lm_var4)
data_all_symp$restless[!is.na(data_all_symp$restless)] <- residuals(lm_var5)
data_all_symp$worried[!is.na(data_all_symp$worried)] <- residuals(lm_var6)
data_all_symp$guilty[!is.na(data_all_symp$guilty)] <- residuals(lm_var7)
data_all_symp$afraid[!is.na(data_all_symp$afraid)] <- residuals(lm_var8)
data_all_symp$anhedonia[!is.na(data_all_symp$anhedonia)] <- residuals(lm_var9)
data_all_symp$angry[!is.na(data_all_symp$angry)] <- residuals(lm_var10)
data_all_symp$hopeless[!is.na(data_all_symp$hopeless)] <- residuals(lm_var11)
data_all_symp$down[!is.na(data_all_symp$down)] <- residuals(lm_var12)
data_all_symp$positive[!is.na(data_all_symp$positive)] <- residuals(lm_var13)
data_all_symp$fatigue[!is.na(data_all_symp$fatigue)] <- residuals(lm_var14)
data_all_symp$tension[!is.na(data_all_symp$tension)] <- residuals(lm_var15)
data_all_symp$concentrate[!is.na(data_all_symp$concentrate)] <- residuals(lm_var16)
data_all_symp$accepted[!is.na(data_all_symp$accepted)] <- residuals(lm_var17)
data_all_symp$threatened[!is.na(data_all_symp$threatened)] <- residuals(lm_var18)
data_all_symp$ruminate[!is.na(data_all_symp$ruminate)] <- residuals(lm_var19)
data_all_symp$avoid_act[!is.na(data_all_symp$avoid_act)] <- residuals(lm_var20)
data_all_symp$reassure[!is.na(data_all_symp$reassure)] <- residuals(lm_var21)
data_all_symp$procrast[!is.na(data_all_symp$procrast)] <- residuals(lm_var22)
#data_all_symp$hours[!is.na(data_all_symp$hours)] <- residuals(lm_var23)
#data_all_symp$difficult[!is.na(data_all_symp$difficult)] <- residuals(lm_var24)
#data_all_symp$unsatisfy[!is.na(data_all_symp$unsatisfy)] <- residuals(lm_var25)
data_all_symp$avoid_people[!is.na(data_all_symp$avoid_people)] <- residuals(lm_var26)

##### SCALE #####
#remove NA
data_all_symp_clean <- na.omit(data_all_symp)

#selcting columns to exclude 
col_excl <- c("start", "date", "id")

# index vector of columns which must not be scaled
index <- names(data_all_symp_clean) %in% col_excl

#actually scaling the shit
data_all_symp_clean_sc <- as.data.frame(scale(data_all_symp_clean[, !index], center = TRUE, scale = TRUE))

#adding the columns again
data_all_final <- data_all_symp_clean_sc
data_all_final$start <- data_all_symp_clean$start
data_all_final$date <- data_all_symp_clean$date
data_all_final$id <- data_all_symp_clean$id


##### ANALYSIS #####
vars <- c("energetic","enthusiastic","content","irritable","restless",
          "worried","guilty","afraid","anhedonia","angry","hopeless",
          "down","positive","fatigue","tension","concentrate","accepted",
          "threatened","ruminate","avoid_act","reassure","procrast","avoid_people")


Res <- mlVAR(data_all_final,vars = vars, idvar = "id", dayvar = "date")
Res$output
# Fixed effects contemporaneous network:
plot1 <- plot(Res, "contemporaneous", nonsig = "hide")

summary(plot1)

# Fixed effects temporal network:
plot2 <- plot(Res, "temporal", nonsig = "hide")

summary(plot2)

# Fixed between-subjects network:
plot3 <- plot(Res, "between", nonsig = "hide")

summary(plot3)




##### CENTRALITY ANALYSIS #####

res_temp <- getNet(Res, "temporal")
res_cont <- getNet(Res, "contemporaneous")
res_betw <- getNet(Res, "between")

res_cont
print(Res)

#Computing centrality indices temporal
#The centrality function can be used to compute centrality measures:
cent_temp <- centrality(res_temp)

# Node strength (degree):
cent_temp$OutDegree # Or InDegree, it's the same in unweighted networks
cent_temp$InDegree

# Closeness - Closeness centrality scores each node based on their 'closeness' to all other nodes in the network.
cent_temp$Closeness

# Betweenness - Betweenness centrality measures the number of times a node lies on the shortest path between other nodes
cent_temp$Betweenness


#The centralityPlot function can be used to plot centrality indices. These are standardized to z-scores by default (centered and divided by the standard deviations):
centralityPlot(res_temp, include = c("InStrength", "OutStrength"))


#Computing centrality indices contemporaneous
#The centrality function can be used to compute centrality measures:
cent_cont <- centrality(res_cont)

# Node strength (degree):
cent_cont$OutDegree # Or InDegree, it's the same in unweighted networks

# Closeness - Closeness centrality scores each node based on their 'closeness' to all other nodes in the network.
cent_cont$Closeness

# Betweenness - Betweenness centrality measures the number of times a node lies on the shortest path between other nodes
cent_cont$Betweenness

cent_cont$ShortestPaths

#The centralityPlot function can be used to plot centrality indices. These are standardized to z-scores by default (centered and divided by the standard deviations):
centralityPlot(res_cont, include = c("Strength", "Betweenness", "Closeness"))

##### GENERAL #####
x <- res_cont
y <- res_temp
z <- res_betw
gen_m_gen_c <- mean(x)
gen_sd_gen_c <- sd(x)
gen_lower_gen_c <- gen_m_gen_c - 1.96*gen_sd_gen_c
gen_upper_gen_c <- gen_m_gen_c + 1.96*gen_sd_gen_c
#gen_m_gen_c 0.02333974
#gen_sd_gen_c 0.06324719
#gen_lower_gen_c -0.1006248
#gen_upper_gen_c 0.1473042
gen_m_gen_t <- mean(y)
gen_m_gen_b <- mean(z)

##### WORRIED #####
#worried mean, SD, upper, and lower for contemporaneous
v <- res_cont[,6]

wrr_m_gen_c <- mean(v)
wrr_sd_gen_c <- sd(v)
wrr_lower_gen_c <- wrr_m_gen_c - 1.96*wrr_sd_gen_c
wrr_upper_gen_c <- wrr_m_gen_c + 1.96*wrr_sd_gen_c
#wrr_m_gen_c 0.02877048
#wrr_sd_gen_c 0.05775352
#wrr_lower_gen_c -0.08442642
#wrr_upper_gen_c 0.1419674


#worried mean, SD, upper, and lower for temporal
f <- res_temp[,6]

wrr_m_gen_t <- mean(f)
wrr_sd_gen_t <- sd(f)
wrr_lower_gen_t <- wrr_m_gen_t - 1.96*wrr_sd_gen_t
wrr_upper_gen_t <- wrr_m_gen_t + 1.96*wrr_sd_gen_t
#wrr_m_gen_t 0.01413877
#wrr_sd_gen_t 0.05285028
#wrr_lower_gen_t -0.08944778
#wrr_upper_gen_t 0.1177253

##### RUMINATE #####
#ruminate mean, SD, upper, and lower for contemporaneous
a <- res_cont[,19]

rmn_m_gen_c <- mean(a)
rmn_sd_gen_c <- sd(a)
rmn_lower_gen_c <- rmn_m_gen_c - 1.96*rmn_sd_gen_c
rmn_upper_gen_c <- rmn_m_gen_c + 1.96*rmn_sd_gen_c
#rmn_m_gen_c 0.02262946
#rmn_sd_gen_c 0.03744531
#rmn_lower_gen_c -0.05076336
#rmn_upper_gen_c 0.09602227


#ruminate mean, SD, upper, and lower for temporal
b <- res_temp[,19]

rmn_m_gen_t <- mean(b)
rmn_sd_gen_t <- sd(b)
rmn_lower_gen_t <- rmn_m_gen_t - 1.96*rmn_sd_gen_t
rmn_upper_gen_t <- rmn_m_gen_t + 1.96*rmn_sd_gen_t
#rmn_m_gen_t 0.01624612
#rmn_sd_gen_t 0.04666996
#rmn_lower_gen_t -0.075227
#rmn_upper_gen_t 0.1077192


##### DOWN #####
#down mean, SD, upper, and lower for contemporaneous
c <- res_cont[,12]

dwn_m_gen_c <- mean(c)
dwn_sd_gen_c <- sd(c)
dwn_lower_gen_c <- dwn_m_gen_c - 1.96*dwn_sd_gen_c
dwn_upper_gen_c <- dwn_m_gen_c + 1.96*dwn_sd_gen_c
#dwn_m_gen_c 0.03329012
#dwn_sd_gen_c 0.06720407
#dwn_lower_gen_c -0.09842987
#dwn_upper_gen_c 0.1650101


#down mean, SD, upper, and lower for temporal
d <- res_temp[,12]

dwn_m_gen_t <- mean(d)
dwn_sd_gen_t <- sd(d)
dwn_lower_gen_t <- dwn_m_gen_t - 1.96*dwn_sd_gen_t
dwn_upper_gen_t <- dwn_m_gen_t + 1.96*dwn_sd_gen_t
#dwn_m_gen_t 0.01858957
#dwn_sd_gen_t 0.0471063
#dwn_lower_gen_t -0.07373878
#dwn_upper_gen_t 0.1109179

##### AVOID ACT #####
#avoid act mean, SD, upper, and lower for contemporaneous
e <- res_cont[,20]

av_act_m_gen_c <- mean(e)
av_act_sd_gen_c <- sd(e)
av_act_lower_gen_c <- av_act_m_gen_c - 1.96*av_act_sd_gen_c
av_act_upper_gen_c <- av_act_m_gen_c + 1.96*av_act_sd_gen_c
#av_act_m_gen_c 0.02600758
#av_act_sd_gen_c 0.08881071
#av_act_lower_gen_c -0.1480614
#av_act_upper_gen_c 0.2000766


#avoid act mean, SD, upper, and lower for temporal
g <- res_temp[,20]

av_act_m_gen_t <- mean(g)
av_act_sd_gen_t <- sd(g)
av_act_lower_gen_t <- av_act_m_gen_t - 1.96*av_act_sd_gen_t
av_act_upper_gen_t <- av_act_m_gen_t + 1.96*av_act_sd_gen_t
#av_act_m_gen_t 0.01937198
#av_act_sd_gen_t 0.04516439
#av_act_lower_gen_t -0.06915023
#av_act_upper_gen_t 0.1078942

##### HOPELESS #####
#hopeless act mean, SD, upper, and lower for contemporaneous
h <- res_cont[,11]

hpl_m_gen_c <- mean(h)
hpl_sd_gen_c <- sd(h)
hpl_lower_gen_c <- hpl_m_gen_c - 1.96*hpl_sd_gen_c
hpl_upper_gen_c <- hpl_m_gen_c + 1.96*hpl_sd_gen_c
#hpl_m_gen_c 0.03272013
#hpl_sd_gen_c 0.05682902
#hpl_lower_gen_c -0.07866474
#hpl_upper_gen_c 0.144105


#hopeless act mean, SD, upper, and lower for temporal
i <- res_temp[,11]

hpl_m_gen_t <- mean(i)
hpl_sd_gen_t <- sd(i)
hpl_lower_gen_t <- hpl_m_gen_t - 1.96*hpl_sd_gen_t
hpl_upper_gen_t <- hpl_m_gen_t + 1.96*hpl_sd_gen_t
#hpl_m_gen_t 0.01690591
#hpl_sd_gen_t 0.04328707
#hpl_lower_gen_t -0.06793675
#hpl_upper_gen_t 0.1017486











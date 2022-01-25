setwd("C:\\Users\\Ilhem\\Documents\\physics_toolbox")
##############################################################
########### Import Library ###################################
library(readr)
library(dplyr)
library(ggplot2)
library(Rcpp)
library(RcppRoll)
library(groupdata2)
library(checkmate)
library(plyr)
library(tidyr)
library(magrittr)


#############################################################
###################### function definition ##################

extract_feature_long = function(data,threeshold_mean,threeshold_sd,threeshold_t){
  if("...11" %in% colnames(data))
  {
    data = data %>% select(-c(...11))
  }
  
  data= data %>% mutate(gN= abs((gFx * gFx) + (gFy * gFy) + (gFz * gFz) - 0.8 ))
  data= data %>% mutate(gNm = roll_meanl(gN, 10))
  data= data %>% mutate(gNsd = roll_sdl(gN, 10))
  dataCopy= data %>% filter(gNm <= threeshold_mean | gNsd <= threeshold_sd)
  data= data %>% filter(gNm > threeshold_mean & gNsd > threeshold_sd)
  data= data %>% mutate(tm= lag(time)) %>% mutate(dt = time - tm) %>% select(-c(tm))
  data = data %>% mutate(switch= if_else(dt > threeshold_t, 1, 0))
  data= data %>% mutate(switch= if_else(is.na(dt),1,switch))
  vector= rep(1, sum(data$switch))
  data= group(data, n=vector, method='l_starts',col_name = 'pid', starts_col = 'switch')
  data = data %>% mutate(pid= as.numeric(pid), pid= if_else(switch== 1, 0, pid))
  dataCopy= dataCopy %>% mutate(dt= 0)
  dataCopy= dataCopy %>% mutate(pid= 0)
  dataCopy= dataCopy %>% mutate(switch= 0)
  data_res= bind_rows(data, dataCopy)
  return(data_res)
}

extract_feature_large= function(data,threeshold_mean,threeshold_sd,threeshold_t){
  X_long=extract_feature_long(data,threeshold_mean,threeshold_sd,threeshold_t)
  X_long= X_long %>% drop_na()
  dataCopy= X_long %>% group_by(pid) %>% filter(sum(dt) > 1 & sum(dt) <3 ) %>% 
    mutate(localtime= time - min(time)) %>% mutate(tbin= floor(localtime * 2))
  
  dataCopy = dataCopy %>% group_by(pid, tbin) %>%  summarise(
    gFx_mean= mean(gFx),
    gFy_mean= mean(gFy),
    gFz_mean= mean(gFz),
    ax_mean= mean(ax),
    ay_mean= mean(ay),
    az_mean= mean(az),
    wx_mean= mean(wx),
    wy_mean= mean(wy),
    wz_mean= mean(wz)
  ) %>% pivot_wider(
    pid, tbin,
    values_from = c(
      gFx_mean,gFy_mean,gFz_mean,ax_mean,ay_mean,az_mean,wx_mean,wy_mean,wz_mean),
    values_fill = 0)
  
  return(dataCopy)
}


#####################main ##########################################
data = read_csv("x_move2.csv")
X_long=extract_feature_long(data,0.45,0.2,0.5)
str(X_long)
X_large= extract_feature_large(data,0.45,0.2,0.5)

str(X_large,give.attr = F)
ggplot(X_long %>% filter(pid>0)) + geom_line(aes(x=time,y=gNsd,color=as.factor(pid),group=pid)) + geom_point(data=X_long %>% filter(pid==00),aes(x=time,y=gNsd),color="black") 

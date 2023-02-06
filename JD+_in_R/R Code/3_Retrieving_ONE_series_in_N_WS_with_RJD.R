# GOAL : see numerical impact of manual intervention 

# compare 2 (or more) workspaces
# - one as reference "..._ref"
# _ one modified "_mod"

# more complete code to be found on github (write to)

options(stringAsFactors=FALSE)
# install.packages("RJDemetra", type="source", INSTALL_opts = "--no-multiarch" )
# A LANCER en 1er (sinon pas de java 8)
library(RJDemetra)
# dplyr sinon LAG ne marche pas
# attention : masque compute
library(dplyr)
library(xlsx)
library(ggplot2)
library(zoo) # for "as.date" on TS object


######### Auxiliary functions to compute growth rates
tx_cr<-function(v){
  w<-(v-lag(v))/lag(v)*100
  return(w)}
tx_12<-function(v){
  w<-(v-lag(v,12))/lag(v,12)*100
  return(w)}


######### PARAMETRES
# SERIES choice
serie_a_exp<-"RF3030" 

# PATHS to used workspaces
path_ref<-"Workspaces/industrie.xml"
path_mod<-"Workspaces/industrie_modified.xml"


# list of paths to workspaces
list_paths_ws <-c(path_ref,path_mod)
list_paths_ws
# list of suffixes
list_suf<-c("ref","mod")
list_suf
# named list
names(list_paths_ws)<-list_suf
list_paths_ws

####################################################################################
####### Retrieve components of ONE SERIES in all the Workspaces of the list
for(i in seq_along(list_paths_ws)){
  print(i)
  suffixe<-paste0("_",list_suf[i])
  print(suffixe)
  # chemins WS
  ch<- list_paths_ws[i]
  ch
  # LOAD des WS UN par UN et cumul  
  ws <- load_workspace(normalizePath(file.path(ch)))
  RJDemetra::compute(ws) #attention : compute est masquee par dplyr : preciser package
  trm<-get_object(ws, 1)
  series<- get_all_objects(trm)
  sa<- get_object(trm, which(names(series)==serie_a_exp)) #### CHOIX serie dans WS####
  model_sa<- get_model(sa,ws)
  # SERIES Main
  tsa<-model_sa$final$series
  head(tsa)
  tail(tsa)
  # de l'TS object vers un data frame
  d<-data.frame(date=as.Date(time(tsa)))
  head(d)
  tail(d)
  df<-as.data.frame(tsa)
  df<-cbind(d,df)
  tail(df)
  # on stocke la derniere obs realisee
  last_obs<-df[nrow(df),1]
  assign(paste0("last_obs",suffixe),last_obs)
  # afficher la derniere obs realisee
  print(paste0("dernier point (non prevu) du ws :", df[nrow(df),1]))
  ##TX CR SERIES MAIN Y et SA
  df_tx<-sapply(df[,c(2,3)],tx_cr)
  colnames(df_tx)<-paste0("tx_",colnames(df_tx))
  df<-data.frame(df,df_tx)
  head(df)
  tail(df)
  
  ########
  # series du PRE PROCESSING 
  pre<-model_sa$regarima$model$effects
  # data frame
  d<-data.frame(date=as.Date(time(pre)))
  head(d)
  tail(d)
  df_pre<-as.data.frame(pre)
  df_pre<-cbind(d,df_pre)
  head(df_pre)
  tail(df_pre)
  df_pre<-df_pre[,-c(4,5)]# on enleve effet de paques et fetes mobiles 
  #  AJUSTEMENT y_lin : prendre l exponenetielle en cas de modele multiplicatif 
  m<-model_sa$regarima$model$spec_rslt
  schema<-data.frame(Log=m[,3])
  ifelse(schema,df_pre[,2]<-exp(df_pre[,2]),df_pre[,2]<-df_pre[,2])
  # la variable "tde" trading day effect est renommee "cal"
  colnames(df_pre)[3]<-"cal"  
   # ajout d'un coeff saiso pur calcule (le D10 peut etre faux )
  # ifelse(schema,df_pre$sp<-df_pre$s/df_pre$cal,df_pre$sp<-df_pre$s-df_pre$cal)
  head(df_pre)
  tail(df_pre)

  # merge composantes finales et predj
  df_full<-merge(df,df_pre,by="date",ALL=TRUE)
  tail(df_full)
  # pour chaque WS de la boucle 
  # collage des suffixes correspondant au WS : renommage des variables (sauf la date [-1])
  suffixe
  names(df_full)[-1]<-paste0(names(df_full)[-1],suffixe)
  head(df_full)

  # cumul a chaque boucle = ajout d un ws 
  # intialisation 
  if(i==1){df_rea<-d}
  df_rea<-merge(df_rea,df_full,by="date", all=TRUE)
  # outliers par workspace 
  df_out<-as.data.frame(model_sa$regarima$regression.coefficients)
  assign(paste0("df_out",suffixe),df_out)
  # regresseurs cjo par workspace en data frame (il faut les couper..sinon 2030)
  cal_regs<-model_sa$regarima$specification$regression$userdef$variables$serie
  d1<-data.frame(date=as.Date(time(cal_regs)))
  df_cal_regs<-as.data.frame(cal_regs)
  df_cal_regs<-cbind(d1,df_cal_regs)
  last_obs
  df_cal_regs<-df_cal_regs[df_cal_regs$date<=last_obs,]
  tail(df_cal_regs)
  assign(paste0("cal_regs",suffixe),cal_regs)
}
# end of loop
# known values (not forcasted)
tail(df_rea)


# raw data
df_G<-df_rea[df_rea$date>="2014-01-01",]
head(df_G)
ggplot(df_G)+
  ggtitle(paste0("Raw_",serie_a_exp))+
  geom_line(aes(x=date,y=y_ref),color="black")+
  geom_line(aes(x=date,y=y_mod), color="red")+
  scale_x_date(name="date",date_breaks = "1 year",date_labels = "%Y")+
  scale_y_continuous(name=serie_a_exp)

# LIN
df_G<-df_rea[df_rea$date>="2017-01-01",]
ggplot(df_G)+
  ggtitle(paste0("LIN_",serie_a_exp))+
  geom_line(aes(x=date,y=y_lin_ref),color="black")+
  geom_line(aes(x=date,y=y_lin_mod), color="red")+
  scale_x_date(name="date",date_breaks = "1 year",date_labels = "%Y")+
  scale_y_continuous(name=serie_a_exp)

## SA
df_G<-df_rea[df_rea$date>="2012-01-01",]
ggplot(df_G)+
  ggtitle(paste0("SA_",serie_a_exp))+
  geom_line(aes(x=date,y=sa_ref),color="black")+
  geom_line(aes(x=date,y=sa_mod), color="red")+
  scale_x_date(name="date",date_breaks = "1 year",date_labels = "%Y")+
  scale_y_continuous(name=serie_a_exp)


df_G<-df_rea[df_rea$date>="2016-01-01",]
ggplot(df_G)+
  ggtitle(paste0("SA_growth_rate_",serie_a_exp))+
  geom_line(aes(x=date,y=tx_sa_ref),color="black")+
  geom_line(aes(x=date,y=tx_sa_mod), color="red")+
  scale_x_date(name="date",date_breaks = "1 year",date_labels = "%Y")+
  scale_y_continuous(name=serie_a_exp)

colnames(df_rea)



# ####### VISUALIZE data in an excel file
# generate names (independetly of chosen suffixes)
list_suf
raw<-paste0("y_",list_suf)
raw
lin<-paste0("y_lin_",list_suf)
lin
sa <-paste0("sa_",list_suf)
sa
s<-paste0("s_",list_suf)
s
cal<-paste0("cal_",list_suf)
cal
t<-paste0("t_",list_suf)
t
vars<-c(raw,lin,sa,s,cal,t)
vars
df_A<-df_rea[,c("date",vars)]
colnames(df_A)
df3<-df_A[df_A$date>="2017-01-01",]
# transposition
head(df3)
t_df3<-as.data.frame(t(df3))
colnames(t_df3)<-df3$date
t_df3<-t_df3[-1,]
path_out11<-paste0(serie_a_exp,"_N_WS.xlsx")
path_out11
write.xlsx(t_df3,path_out11)

 


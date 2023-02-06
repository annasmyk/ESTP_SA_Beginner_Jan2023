#############
### Seasonal adjustment in R 
# here no link with JD+ workspaces or GUI

# 1 basic SA (x13 et Tramo seats)
# adjusting series (pre def spec) 
# output orga
# 2 customizing parameters 
# 3 Quality assessment 

# see M202101_R_Tools_for_JDemetra.pdf for plenty of details

# see RJDemetra documentation in R !!

options(stringasfactors = FALSE)
# install.packages("RJDemetra")
library(RJDemetra)
library(ggplot2)
library(zoo) #att : indispensable for "as.date on TS object
library(lubridate)

# My data
ipi<-read.csv2("Data/IPI_nace4.csv")
head(ipi)
# import en CHR...?
str(ipi)
# Dates
# FORMAT %Y : 4 chiffres %y 2 chiffres
ipi$date<-as.Date(ipi$DATE,format="%d/%m/%Y")
# Formatage sans objet TS
ipi[,-1]<-sapply(ipi[,-1],as.numeric)
ipi[1:5,1:10]
nrow(ipi)
last_obs<-ipi[nrow(ipi),1]
last_obs
str(ipi)
# ggplot2 
ggplot(ipi,aes(date,RF3030))+geom_line()

# create a TS object from a data frame
# pay attention to start at the right date
raw_series<-ts(ipi[,"RF3030"],frequency=12,start=c(1990,1),end=c(2018,6))
raw_series


# SA with X13 or Tramo-Seats
model_sa<-RJDemetra::x13(raw_series, spec ="RSA5c") # attention v2 or V3
model_sa_ts<-tramoseats(raw_series, spec ="RSAfull")

## demo model_sa= sa_item navigation: model_sa$

# preadjustment only : REGARIMA part 
myreg <- regarima_x13(raw_series,spec ="RG5c")
summary(myreg)

# model_sa (result of estimation) = whole output in a list of lists
# 3 parts : series / parameters / diagnostics / 

# final components
model_sa$final$series
# their forecasts y_f sa_f s_f t_f i_f
model_sa$final$forecasts


# pre proc
model_sa$regarima$model$effects
# model residuals forecast
model_sa$regarima$forecast

#  AJUSTEMENTS y_lin
# y_lin correction (displayed in log if multiplicative model)
# read if log transformed
to_log<-model_sa$regarima$model$spec_rslt[,3]
# correct the series 
ts_pre<-model_sa$regarima$model$effects
ifelse(to_log,ts_pre[,1]<-exp(ts_pre[,1]),ts_pre[,1])
head(ts_pre)

################ DATA VISUALIZATION 
# monthplot of the raw series 
ts_y<-model_sa$final$series[,1]
monthplot(ts_y,xlab="mois",main="raw series")

# for class 'final' : 2 types 
# syntax min 
plot(model_sa, type_chart = "sa-trend", first_date = c(2015, 1))
plot(model_sa, type= "cal-seas-irr",first_date = c(2015, 1))

layort(matrix(1:6, 3, 2));plot(model_sa$regarima,ask = FALSE)

model_sa$decomposition$si_ratio

## S3 method for class 'decomposition_X11'
# TEMPLATE
# plot(x, first_date, last_date, caption = "S-I ratio", ylim, ...)
# Plotting SI ratios  
plot(model_sa$decomposition, first_date = c(2015,1))

####### DIAGNOSTICS (add visu resultats ?)
# preadjustment 


# regression variables and coefficients 
# (all deterministicinterventions : automatic or used-defined)
# trading days, easter, outliers...
model_sa$regarima$regression.coefficients

# arima coeffs (modele explicite with noms des coeffs,quick exp)
model_sa$regarima$arima.coefficients

# stats on residuals
model_sa$regarima$residuals.stat$tests

# forecasts quality (voir or est "ort of sample")
# has to be defined in user-defined output

# Decomposition : stats M (ref to Lothian et Mory)
model_sa$decomposition$mstats


model_sa$decomposition$s_filter
model_sa$decomposition$t_filter

# Final Diagnostics
# test for seasonality presence (X-11)
model_sa$diagnostics$combined_test
# testing for residual seasonality and residual trading days 
# on seasonnally adjusted series (sa) and irregular component (i)
model_sa$diagnostics$residuals_test
# X-11 Variance decomposition
model_sa$diagnostics$variance_decomposition 

################################################################
####NOT in the paper
# direct functions : to access parameters but not results 
# s_estimate returns a data.frame with the estimate variables
s_estimate(model_sa)
# s_transform returns a data.frame with the transform variables
s_transform(model_sa) 
# s_usrdef returns a data.frame with the user-defined regressors 
# (outliers and variables) model specification, indicating if those variables are included in the model and if coefficients are pre-specified
s_userdef(model_sa)
# s_preort returns a data.frame with the pre-specified outliers
# 
# s_preVar returns a list with the information on the user-defined variables, including: series - the time series and description - data.frame with the variable type and coefficients
# 
# s_td returns a data.frame with the trading.days variables
s_td(model_sa) 
# s_easter returns a data.frame with the easter variables
 
# s_out returns a data.frame with the outliers detection variables
# 
# s_arima returns a data.frame with the arima variables
s_arima(model_sa) 
# s_arimaCoef returns a data.frame with the user-specified ARMA coefficients
# 
# s_fcst returns a data.frame with the forecast horizon
s_fcst(model_sa)
# s_span returns a data.frame with the span variables
s_span(model_sa)
# s_x11 returns a data.frame with the x11 variables
s_x11(model_sa) 
# s_seats returns a data.frame with the seats variables
# 

############################################################################
# # Customizing parameters 
# - defining a new spec 
# - changing estimation span (only preadj not decomp)
# - adding outliers 
# - defining arima model 
# - adding user defined td regressors
# - decomp filters for x13

# frame here = preadj + decomp (rather than preadj seul)

# defining and saving a spec 
# get the spec corresponding to a previous adjustment 
# here spec_1 will correspond to RSA5c, as specified above when "model_sa" has been defined
spec_1<- RJDemetra::x13_spec(model_sa)

raw_series

# changing estimation span, imposing additive model and adding user defined outliers 
# first create a new spec modifying the previous one 
spec_2<- RJDemetra::x13_spec(spec_1, estimate.from = "2004-01-01",
                  usrdef.outliersEnabled = TRUE,
                             usrdef.outliersType = c("LS", "AO"),
                             usrdef.outliersDate = c("2008-10-01", "2016-01-01"),
                             transform.function = "None") # additive model
# here the reg-arima model will be estimated from  "2004-01-01" 
# the decomposition will be run on the whole span 

# new sa processing
model_sa_2<-RJDemetra::x13(raw_series,spec_2)

# changing the arima model, starting from spec_1
# we force a (1,d,1)(0,D,1), 
# the regular coefficients are fixed : ar(1)  is "arima.p=-0.8" and ma(1) is "arima.q=-0.6"
# the seasonal ar order = 0, the seasonal ma(1) is "arima.bq=1 is not fixed by the user ("Undefined")
spec_3 <- x13_spec(spec_1, automdl.enabled = FALSE,
                   arima.p = 1, arima.q = 1,
                   arima.bp = 0, arima.bq = 1,
                   arima.coefEnabled = TRUE,
                   arima.coef = c(-0.8, -0.6, 0), # 0 stands for not fixed 
                   arima.coefType = c(rep("Fixed", 2), "Undefined"))
# new sa processing
model_sa_3<-RJDemetra::x13(raw_series,spec_3)
# to see the parameters in the specification
s_arimaCoef(spec_3)
# argument can be spec or model
s_arimaCoef(model_sa_3)
# to access broader arima parameters (not only coeffs)
s_arima(model_sa_3)
# argument can be spec or model
s_arima(model_sa_3)
# to access the estimation results for the arima model 
model_sa_3$regarima$arima.coefficient

####################################################
# defining user defined trading days 
spec_4 <- x13_spec(spec_1,
tradingdays.option = "UserDefined",
tradingdays.test ="None",
usrdef.varEnabled = TRUE,
# the user defined variable will be assigned to the calendar component
usrdef.varType="Calendar",
usrdef.var=td_reg) # regressors have to be a single or multiple TS 
# new sa processing
model_sa_4<-RJDemetra::x13(raw_series,spec_4)
# accessing parameters fonction s_
# accessing the results 
model_sa_4$regarima$regression.coefficients
model_sa_4$regarima$regression.coefficients
####################################################
# user defined intervention variable  
spec_5 <- x13_spec(spec_1,
                   usrdef.varEnabled = TRUE,
                   # the user defined variable will be assigned to the trend component
                   usrdef.varType="Trend",
                   usrdef.var=x ) # x has to to be a single or multiple TS 
# new sa processing
model_sa_5<-x13(raw_series,spec_5)
# accessing parameters fonction s_
# accessing the results 
model_sa_5$regarima$regression.coefficients


# customizing decomposition filters
# Henderson moving average (ma) length for trend
# Seasonal moving average (ma) length for seasonal component's extraction
# starting from spec_1
spec_6 <- x13_spec(spec_1,x11.trendma = 23,x11.seasonalma = "S3X9")
# new sa processing
model_sa_6<-x13(raw_series,spec_5)
# to access the actual parameters 
# s_decomp ?


# a specification can be saved and loaded back like any R object through a Rdata file 
# save_spec function  
save_spec(spec_3, file.path("Z:/Anna_SMYK/Doc_R_Tools","spec_3_user_def_arima.RData"))
# load_spec function
new_spec<-load_spec(file="spec_3_user_def_arima.RData")

############################ USER DEFINED output

##User defined output 
# lists of output items in a character vector 
user_defined_variables("X13-ARIMA") 
#or 
# user_defined_variables("TRAMO-SEATS")
# adding user defined output
my_output<-user_defined_variables("X13-ARIMA")[c(28,174,258,259,260)]
# run the sa function with user defined output  
model_sa_out<-x13(raw_series, spec ="RSA5c",userdefined=my_output)
# retrieve the series : list of TS objects
# additional_series <-model_sa_out$user_defined[[1],[2]]
str(model_sa$user_defined)
additionnal_diagnostics<-model_sa_out[[5]]
additionnal_diagnostics





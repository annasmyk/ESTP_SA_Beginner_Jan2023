### rjd3 suite....building calendar regressors
# to install 
install.packages("RProtoBuf","remotes")
library(RProtoBuf)
library(remotes)
remotes::install_github("palatej/rjd3toolkit", INSTALL_opts = "--no-multiarch")
remotes::install_github("palatej/rjd3modelling", INSTALL_opts = "--no-multiarch")

library(rjd3toolkit)
library(rjd3modelling)


# French calendar
fr_cal <- calendar.new()
calendar.holiday(fr_cal, "NEWYEAR")
calendar.holiday(fr_cal, "EASTERMONDAY")
calendar.holiday(fr_cal, "MAYDAY")
calendar.fixedday(fr_cal, month = 5, day = 8,
                  start = "1982-01-01")
# calendar.holiday(fr_cal, "WHITMONDAY") # Equivalent to:
calendar.easter(fr_cal, offset = 61)

calendar.fixedday(fr_cal, month = 7, day = 14)
# calendar.holiday(fr_cal, "ASSUMPTION")
calendar.easter(fr_cal, offset = 61)
calendar.holiday(fr_cal, "ALLSAINTSDAY")
calendar.holiday(fr_cal, "ARMISTICE")
calendar.holiday(fr_cal, "CHRISTMAS")


### Creation of a associated regressors
#s <- ts(0, start = 1990, end = c(2018, 6), frequency = 12)
# Trading-days regressors (each day has a different effect, Sunday as contrasts)
td_reg <- htd(calendar=fr_cal,start=c(1990,1), frequency=12, length=492,
              groups = c(1, 2, 3, 4, 5, 6, 0), contrasts=TRUE, meanCorrection = TRUE,
              holiday=7)
td_reg
# Working-days regressors (Monday = ... = Friday; Saturday = Sunday = contrasts)
wd_reg <- htd(fr_cal, s = s, groups = c(1, 1, 1, 1, 1, 0, 0))
# Monday = ... = Friday; Saturday; Sunday = contrasts
wd_reg <- htd(fr_cal, s = s, groups = c(1, 1, 1, 1, 1, 2, 0))
wd_reg
# Monday = ... = Wednesday; Thursday; Friday = contrasts
wd_reg2 <- htd(fr_cal, s = s, groups = c(1, 1, 1, 2, 0, 1, 1))
wd_reg2
str(wd_reg2)


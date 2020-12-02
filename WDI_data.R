
library(WDI)


?WDI

WDI_ind <- WDIsearch()


View(WDI_ind)

pop_ind <- WDIsearch('Population')
View(pop_ind)

WDI(start =  2020)



GDP2020 <- WDI(indicator = '6.0.GDP_current', start = 2009)

x <- WDI(indicator = c("BG.KAC.FNEI.GD.PP.ZS"), start=2015,, end =2016)


x <- WDIbulk()
y <- e
WDIcache()
?WDIcache

?WDIbulk

library(installr)

updateR()

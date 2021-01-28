# Sesión 3, elsoc, enero 2021

library(foreign) 
library(dplyr)

# Para tablas de calidad
library(texreg)
library(xtable)

library(plm) # Paquete para datos de panel

rm(list=ls())

# Base de datos
load("/Users/luismaldonado/Dropbox/Sol3026_3063/2020/Clases/Clase 9/Ejercicio/Panel.RData")

Panel[,1:4]
Panel[,1:7]
Panel

# Cross-sectional OLS para T=4
##################################
ols1 <- lm(wage~marr, data=Panel, subset=time==4)
summary(ols1)

# Pooled OLS
###############
ols2 <- lm(wage~marr, data=Panel)
summary(ols2)

# Primeras diferencias 1
############################
summary(lm(dwage~dmarr,data=Panel))  


# Primeras diferencias 2
###########################
fd1 <- plm(wage~-1 + marr, data = Panel, model = "fd")
summary(fd1)

fd2 <- plm(wage~ marr, data = Panel, model = "fd")
summary(fd2)

# Efectos fijos 1
#####################
summary(lm(wwage~wmarr,data=Panel)) 

fe1 <- plm(wage~marr, data = Panel, model = "within")
summary(fe1)

# Efectos fijos 2
#####################
fe2 <- lm(wage~marr + factor(id), data=Panel)
summary(fe2)

fe3 <- lm(wage~-1 + marr + factor(id), data=Panel)
summary(fe3)


# ------------- Cluster estandar errors ------------- #
library(clubSandwich)

# Stata cluster SE
a <- lm(wage~marr, data=Panel)
summary(a)

coef_test(a,vcov="CR2",cluster=Panel$id)






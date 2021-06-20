library(plyr)
# PD: sí, el título dice 'seasonal' :v
########### IMPUT ###########
setwd('C:\\Users\\Kevin\\Desktop\\UNALM\\SextoCiclo\\Est_Climat\\Trabajo_final')
PISCO_t <- read.csv('PISCO_t.csv', sep = ';')
PISCO_pp <- read.csv('PISCO_pp.csv', sep = ';')

ERA5_t <- read.csv('ERA5_t.csv', sep = ';')
ERA5_pp <- read.csv('ERA5_pp.csv', sep = ';')

CMIP6_t <- read.csv('CMIP6_t.csv', sep = ';')
CMIP6_pp <- read.csv('CMIP6_pp.csv', sep = ';')

########### PROCESS ############

#### PISCO vs CMIP6
# Acoplando a un mismo DataFrame
dF <- data.frame(PISCO_t$x, CMIP6_t$x, CMIP6_t$m, CMIP6_t$Q)
colnames(dF) <- c('PISCO', 'CMIP6', 'm', 'Q')

dF2 <- data.frame(PISCO_pp$x, CMIP6_pp$x, CMIP6_pp$m, CMIP6_pp$Q)
colnames(dF2) <- c('PISCO', 'CMIP6', 'm', 'Q')

#### ERA5 vs CMIP6
# Acoplando a un mismo DataFrame
dF3 <- data.frame(ERA5_t$x, CMIP6_t$x, CMIP6_t$m, CMIP6_t$Q)
colnames(dF3) <- c('ERA5', 'CMIP6', 'm', 'Q')

dF4 <- data.frame(ERA5_pp$x, CMIP6_pp$x, CMIP6_pp$m, CMIP6_pp$Q)
colnames(dF4) <- c('ERA5', 'CMIP6', 'm', 'Q')

###### Parámetros mensuales ######

n = 132/12*12/4

### Indicadores
PISCO_CMIP6_t <- data.frame(x=seq(1:4), BIAS = 1:4, RMSE= 1:4, r_pearson= 1:4)
PISCO_CMIP6_pp <- data.frame(x=seq(1:4), BIAS = 1:4, RMSE= 1:4, r_pearson= 1:4)
ERA5_CMIP6_t <- data.frame(x=seq(1:4), BIAS = 1:4, RMSE= 1:4, r_pearson= 1:4)
ERA5_CMIP6_pp <- data.frame(x=seq(1:4), BIAS = 1:4, RMSE= 1:4, r_pearson= 1:4)

i=1
while(i<=4) {
  # BIAS
  PISCO_CMIP6_t[i,2] <- (1/n)*sum(dF[dF$Q==i,2]-dF[dF$Q==i,1])
  PISCO_CMIP6_pp[i,2] <- (1/n)*sum(dF2[dF2$Q==i,2]-dF2[dF2$Q==i,1])
  ERA5_CMIP6_t[i,2] <- (1/n)*sum(dF3[dF3$Q==i,2]-dF3[dF3$Q==i,1])
  ERA5_CMIP6_pp[i,2] <- (1/n)*sum(dF4[dF4$Q==i,2]-dF4[dF4$Q==i,1])
  
  # RMSE
  PISCO_CMIP6_t[i,3] <- (sum((dF[dF$Q==i,2]-dF[dF$Q==i,1])^2)/n)^0.5
  PISCO_CMIP6_pp[i,3] <- (sum((dF2[dF2$Q==i,2]-dF2[dF2$Q==i,1])^2)/n)^0.5
  ERA5_CMIP6_t[i,3] <- (sum((dF3[dF3$Q==i,2]-dF3[dF3$Q==i,1])^2)/n)^0.5
  ERA5_CMIP6_pp[i,3] <- (sum((dF4[dF4$Q==i,2]-dF4[dF4$Q==i,1])^2)/n)^0.5
  
  # r-Pearson
  sum_x <- dF[dF$Q==i,1]-mean(dF[dF$Q==i,1])
  sum_x2 <- dF2[dF2$Q==i,1]-mean(dF2[dF2$Q==i,1])
  sum_x3 <- dF3[dF3$Q==i,1]-mean(dF3[dF3$Q==i,1])
  sum_x4 <- dF4[dF4$Q==i,1]-mean(dF4[dF4$Q==i,1])
  
  sum_y <- dF[dF$Q==i,2]-mean(dF[dF$Q==i,2])
  sum_y2 <- dF2[dF2$Q==i,2]-mean(dF2[dF2$Q==i,2])
  sum_y3 <- dF3[dF3$Q==i,2]-mean(dF3[dF3$Q==i,2])
  sum_y4 <- dF4[dF4$Q==i,2]-mean(dF4[dF4$Q==i,2])
  
  PISCO_CMIP6_t[i,4] <- sum(sum_x*sum_y)/(sum((sum_x)^2)*sum((sum_y)^2))^0.5
  PISCO_CMIP6_pp[i,4] <- sum(sum_x2*sum_y2)/(sum((sum_x2)^2)*sum((sum_y2)^2))^0.5
  ERA5_CMIP6_t[i,4] <- sum(sum_x3*sum_y3)/(sum((sum_x3)^2)*sum((sum_y3)^2))^0.5
  ERA5_CMIP6_pp[i,4] <- sum(sum_x4*sum_y4)/(sum((sum_x4)^2)*sum((sum_y4)^2))^0.5
  
  i=i+1
}

########### OUTPUT ##########
# write.table(PISCO_CMIP6_t, 'PISCO_CMIP6_t_s.csv', sep=',')
# write.table(PISCO_CMIP6_pp, 'PISCO_CMIP6_pp_s.csv', sep=',')
# write.table(ERA5_CMIP6_t, 'ERA5_CMIP6_t_s.csv', sep=',')
# write.table(ERA5_CMIP6_pp, 'ERA5_CMIP6_pp_s.csv', sep=',')
library(plyr)
library(ggplot2)

######### IMPUT #########
setwd('C:\\Users\\Kevin\\Desktop\\UNALM\\SextoCiclo\\Est_Climat\\Trabajo_final')

######### PROCESS #########
# Trimestres
gg <- c('DJF', 'MAM', 'JJA', 'SON')

# Dataframe con promedios estacionales
dF_t <- data.frame(
  x = seq(1,4),
  time = gg,
  PISCO = c(12.67445, 11.91668, 11.77202, 12.94362),
  ERA5 = c(6.215866, 5.027859, 4.660130, 5.997622),
  CMIP6 = c(23.01856, 21.05748, 19.02296, 19.89798)
)

dF_pp <- data.frame(
  x = seq(1,4),
  time = gg,
  PISCO = c(92.184355, 17.817710, 8.698974, 56.246754),
  ERA5 = c(228.43030, 83.14128, 68.22130, 176.04416),
  CMIP6 = c(80.33866, 31.81504, 14.99438, 37.74268)
)

### Indicadores
Rel_t_Pisco_CMIP6 <- data.frame(x=seq(1:4), BIAS = 1:4, RMSE= 1:4, r_pearson= 1:4)

i=1
while(i<=4) {
  # BIAS
  Rel_t_Pisco_CMIP6[i,2] <- (1/1)*sum(dF_t[dF_t$x==i,5]-dF_t[dF_t$x==i,3])
  
  # RMSE
  Rel_t_Pisco_CMIP6[i,3] <- (sum((dF_t[dF_t$x==i,5]-dF_t[dF_t$x==i,3])^2)/1)^0.5
  
  # r-Pearson
  sum_x <- dF_t[dF_t$x==i,3]-mean(dF_t[dF_t$x==i,3])
  sum_y <- dF_t[dF_t$x==i,5]-mean(dF_t[dF_t$x==i,5])
  Rel_t_Pisco_CMIP6[i,4] <- sum(sum_x*sum_y)/(sum((sum_x)^2)*sum((sum_y)^2))^0.5
  
  i=i+1
}
######### OUTPUT #########


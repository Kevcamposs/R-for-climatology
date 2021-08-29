library(tidyverse)
library(lubridate)
library(tidyquant)
library(dplyr)
###### Input #######
df <- read.table('Data_pp_diurnal.csv', sep = ',', header = TRUE) # Lectura de datos

###### Process #######
df <- df %>% select(Fecha, X12, X18) %>% rename(S1 = X12, S2 = X18) # estación 12 y 18
df$Fecha <- as.Date(df$Fecha, '%m/%d/%Y', '%Y-%m-%d') # convertir columna de fechas a Date object

S1 <- data.frame(Date = df$Fecha, x = df$S1)
S2 <- data.frame(Date = df$Fecha, x = df$S2)


### S1
# Agregando columna de trimestre y año
S1 <- S1 %>%
  mutate(Y = year(S1$Date),
         M = month(S1$Date))

S1_max <- data.frame(Year = numeric(),
                       Month = numeric(),
                       x = numeric())

for (i in unique(S1$Y)) {
  wk <- data.frame(Year = numeric(),
                   Month = numeric(),
                   x = numeric())
  
  for (k in unique(S1$M)) {
    x <- subset(S1, subset = (Y == i & M == k))[2]
    
    x1 <- apply(x, 2, max, na.rm = TRUE)
    
    XD <- data.frame(Year = i, Month = k, x = x1)
    
    wk <- merge(wk, XD, by = c("Year", "Month", "x"), all = T)}
  
  S1_max <- merge(S1_max, wk, by = c("Year", "Month", "x"), all = T)
}

S1_max <- S1_max %>% pivot_table(
  .rows = ~Year,
  .columns = ~Month,
  .values = ~x
)

attr(S1_max,"variable.labels")<- NULL

### S2
# Agregando columna de trimestre y año
S2 <- S2 %>%
  mutate(Y = year(S2$Date),
         M = month(S2$Date))

S2_max <- data.frame(Year = numeric(),
                     Month = numeric(),
                     x = numeric())

for (i in unique(S2$Y)) {
  wk <- data.frame(Year = numeric(),
                   Month = numeric(),
                   x = numeric())
  
  for (k in unique(S2$M)) {
    x <- subset(S2, subset = (Y == i & M == k))[2]
    
    x1 <- apply(x, 2, max, na.rm = TRUE)
    
    XD <- data.frame(Year = i, Month = k, x = x1)
    
    wk <- merge(wk, XD, by = c("Year", "Month", "x"), all = T)}
  
  S2_max <- merge(S2_max, wk, by = c("Year", "Month", "x"), all = T)
}

S2_max <- S2_max %>% pivot_table(
  .rows = ~Year,
  .columns = ~Month,
  .values = ~x
)

attr(S2_max,"variable.labels")<- NULL

###### Output #######
write.table(S1_max, "S1_max.csv", sep = ";")
write.table(S2_max, "S2_max.csv", sep = ";")
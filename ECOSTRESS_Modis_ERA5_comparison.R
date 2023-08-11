#1.0 R Environment ----

library(dplyr)
library(tidyverse)
library(lubridate)

modis_lst <- read.csv("/Users/maciejjurzyk/Downloads/GRASSAT-Calosc-MOD11A1-061-results.csv")
modis_lai <- read.csv("/Users/maciejjurzyk/Downloads/GRASSAT-Calosc-MCD15A2H-061-results.csv")
ecostress_lst <- read.csv("/Users/maciejjurzyk/Downloads/GRASSAT-Calosc-ECO2LSTE-001-results.csv")
ecostress_et <- read.csv("/Users/maciejjurzyk/Downloads/GRASSAT-Calosc-ECO3ETPTJPL-001-results.csv")

###1.1 MODIS ---- 

e2 <- modis_lst
e3 <- e2%>% mutate(e2$First_10_Characters <- substr(e2$Date, 1, 10))
e3 %>% glimpse()
e4 <- e3 %>%
  mutate(
    Date = ymd_hms(Date),      
    Time = format(Date, "%H:%M:%S")  
  )

e4 %>% glimpse()

e5 <- e4 %>% mutate(day=ymd(e4$'e2$First_10_Characters <- substr(e2$Date, 1, 10)'))

e5 %>% glimpse()

e5%>% hms(e5$Time)

et<- e5 %>% mutate(godz=hms(e5$Time))

e5%>% glimpse() # full success! 

####1.1.1. 0 into NA conversion  ---- 

e6 <- e5 %>%
  mutate_all(.funs = ~replace(., . == 0, NA))

e6 %>% glimpse()


####1.1.2 K into Celsius degree conversion ---- 

modlst <- e6 %>%  mutate(TempCday=MOD11A1_061_LST_Day_1km -273.15)

modlst1 <- modlst %>%  mutate(TempCnight=MOD11A1_061_LST_Night_1km  -273.15)

modlst1%>% glimpse()


lst_modis <- modlst1


####1.1.3 MODSI LAI 


l1 <- modis_lai

l2 <- l1%>% mutate(l1$First_10_Characters <- substr(l1$Date, 1, 10))

l2 %>% glimpse()

l3 <- l2 %>%
  mutate(
    Date = ymd_hms(Date),      
    Time = format(Date, "%H:%M:%S")  
  )

l3 %>% glimpse()

l4 <- l3 %>% mutate(day=ymd(l3$`l1$First_10_Characters <- substr(l1$Date, 1, 10)` ))

l4 %>% glimpse()

lai_modis <- l4


####1.1.1. 0 into NA conversion  ---- 

e6 <- e5 %>%
  mutate_all(.funs = ~replace(., . == 0, NA))

e6 %>% glimpse()




#### 1.1.4 GGPLOT----

## Temp Day


modlst1 %>% ggplot(mapping = aes(x=day,y=TempCday))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Category)+
  theme_minimal()+
  scale_y_continuous(limits = c(-20, 40))+
  labs(
    title = "Temperatura powierzchni mierzona w nocy na poszczególnych łąkach GRASSAT w Wielkopolsce",
    x = "Data",
    y = "Temperatura (°C)",
    caption = "Na podstawie danych MODIS"
  )

##1.2 ERA5 ----


ERA5 <- read.csv("/Users/maciejjurzyk/Downloads/TEMP2M_Recznie - Arkusz1-5.csv")

ERA5 %>% glimpse()

ERA51 <- ERA5%>% mutate(ERA5$First_10_Characters <- substr(ERA5$Date, 1, 10))

ERA52 <- ERA51 %>%
  mutate(
    Date = ymd_hms(Date)
  ) %>%
  select(-Date)

ERA52 %>% glimpse()

ERA53 <- ERA52 %>% mutate(day=ymd(ERA52$`ERA5$First_10_Characters <- substr(ERA5$Date, 1, 10)`))

ERA53%>% glimpse()


###1.2.1 ERA5 Temperature to numeric ----

ERA5 <- ERA53%>%
  mutate(Temp = as.numeric(gsub(",", ".", Temp)))

ERA5 %>% glimpse() # full success! 


#### 1.2.2 GGPLOT----

## ERA5


ERA5 %>% ggplot(mapping = aes(x=day,y=Temp))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Category)+
  theme_minimal()+
  scale_y_continuous(limits = c(-20, 40))+
  labs(
    title = "Temperatura powierzchni mierzona w nocy na poszczególnych łąkach GRASSAT w Wielkopolsce",
    x = "Data",
    y = "Temperatura (°C)",
    caption = "Na podstawie danych ERA5")


## 1.3 ECOSTRESS LST ----

###1.31 LST ECOSTRESS  Data  ---- 


l1 <- ecostress_lst

l2 <- l1%>% mutate(l1$First_10_Characters <- substr(l1$Date, 1, 10))

l2 %>% glimpse()

l3 <- l2 %>%
  mutate(
    Date = ymd_hms(Date),      
    Time = format(Date, "%H:%M:%S")  
  ) %>%
  select(-Date)

l3 %>% glimpse()

l <- l3 %>% mutate(day=ymd(l3$'l1$First_10_Characters'))

l %>% glimpse()


l %>% hms(l$Time)

lst1<- l %>% mutate(godz=hms(l$Time))

lst1%>% glimpse() # full success! 

####1.3.1.1 0 into NA conversion  ---- 

lst3 <- lst1 %>%
  mutate_all(.funs = ~replace(., . == 0, NA))


####1.3.1.2 K into Celsius degree conversion ---- 

lstC <- lst3 %>%  mutate(TempC=ECO2LSTE_001_SDS_LST  -273.15)

lstC %>% glimpse()

lstC %>% ggplot(mapping = aes(x=day,y=TempC))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Category)+
  theme_minimal()+
  scale_y_continuous(limits = c(-20, 40))+
  labs(
    title = "Temperatura powierzchni mierzona w ciągu dnia na poszczególnych łąkach GRASSAT w Wielkopolsce",
    x = "Data",
    y = "Temperatura (°C)",
    caption = "Na podstawie danych NASA ECOSTRESS"
  )

lstC %>% glimpse()


ECO1 <- lstC %>%
  select(
    Category,
    ID,
    Latitude,
    Longitude,
    Date = day,
    LST_ECOSTRESS = TempC,
    Time=godz)

ECO %>% glimpse()

ECO <- lstC %>%
  select(
    Category,
    ID,
    Latitude,
    Longitude,
    Date = day,
    LST_ECOSTRESS_11 = TempC,
    Time=godz
  ) %>% 
  filter(grepl("11H", Time)) # (12:00:00 - 12:59:59)

library(data.table)

ECO10_13 <- lstC %>%
  select(
    Category,
    ID,
    Latitude,
    Longitude,
    Date = day,
    LST_ECOSTRESS_11 = TempC,
    Time = godz
  ) %>% 
  filter(
    grepl("11H|12H|13H", Time)
  )

ECO10_13 %>% glimpse()

 ECO %>% ggplot(mapping = aes(x=Date,y=LST_ECOSTRESS))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~Category)+
  theme_minimal()+
  scale_y_continuous(limits = c(-20, 40))+
  labs(
    title = "Temperatura powierzchni mierzona w ciągu dnia na poszczególnych łąkach GRASSAT w Wielkopolsce",
    x = "Data",
    y = "Temperatura (°C)",
    caption = "Na podstawie danych NASA ECOSTRESS")

 ECO %>% glimpse()

    
    ECO_11_all <- ECO %>%
      full_join(ECO1, by = c("Category", "Date"))
    
    ECO_11_all %>% glimpse()
    
    
    ECO_10_13_all <- ECO10_13 %>%
      full_join(ECO1, by = c("Category", "Date"))
    
    ECO_11_all %>% glimpse()
    
## 1.4 ECOSTRESS ET ----
    
###1.31 ET ECOSTRESS  Data  ---- 
    
    
t1 <- ecostress_et
    
    t2 <- t1%>% mutate(t1$First_10_Characters <- substr(t1$Date, 1, 10))
    
    t2 %>% glimpse()
    
    t3 <- t2 %>%
      mutate(
        Date = ymd_hms(Date),      
        Time = format(Date, "%H:%M:%S")  
      ) %>%
      select(-Date)
    
    t3 %>% glimpse()
    
    t <- t3 %>% mutate(day=ymd(t3$'t1$First_10_Characters'))
    
    t %>% glimpse()
    
    
    t %>% hms(t$Time)
    
    et1<- t %>% mutate(godz=hms(t$Time))
    
    et1%>% glimpse() # full success! 
    
    
  
#2.0 Joining tables ----

##2.1 MODIS LST  + MODIS LAI ----
    
lst_modis %>% glimpse()
lai_modis %>% glimpse()

lst_modis %>% view()



modis_full <- lst_modis %>%
left_join(lai_modis, by = c("Category", "day"))

modis_full %>% view()


##2.2 ERA5 ----
ERA5_full <- ERA5 %>%
  left_join(modis_full, by = c("Category", "day"))


ERA5_full %>% view()


MODIS_ERA <- ERA5_full %>%
  select(
    Category,
    ID,
    Latitude,
    Longitude,
    Date = day,
    ERA5 = Temp,
    LST_MODIS_Ta_day = TempCday,
    LST_MODIS_Ta_night = TempCnight,
    MODIS_LAI = MCD15A2H_061_Lai_500m,
  )

MODIS_ERA %>% view()

ggplot(MODIS_ERA, aes(x = Date)) +
  geom_smooth(aes(y = LST_MODIS_Ta_day, color = "LST_MODIS_Ta_day")) +
  geom_smooth(aes(y = ERA5, color = "ERA5"))+
  theme_minimal()+
  facet_wrap(~Category)


lstC %>% glimpse()


ecs <- lstC %>% 
  select(
  Category,
  ID,
  Latitude,
  Longitude,
  Date=day,
  LST_ECOSTRESS_TA =ECO2LSTE_001_SDS_LST,
  Time=godz
)

## 2.3 Ecostress LST ----

library(lubridate)

library(dplyr)

library()

ECO10_13 %>% glimpse()

ecs %>% glimpse()

ecs_11 <- ecs %>% 
  filter(grepl("11H", Time))

ecs_11 %>% glimpse()

MODIS_ERA %>% glimpse()

All_data <- MODIS_ERA %>%
  left_join(ecs, by = c("Category", "Date"))

All_data_11 <- MODIS_ERA %>%
  left_join(ecs_11, by = c("Category", "Date"))



All_data_10_13 <- MODIS_ERA %>%
  left_join(ECO10_13, by = c("Category", "Date"))


All_data_10_13 %>% glimpse()


merged_data <- MODIS_ERA %>% 
  left_join(ecs, by = c("Category", "Date"))

## 2.4 Ecostress ET data ----

et1 %>% glimpse()

et_data <- et1 %>%
  select(
    Category,
    ID,
    Latitude,
    Longitude,
    Date=day,
    ET_ECOSTRESS_canopy=ECO3ETPTJPL_001_EVAPOTRANSPIRATION_PT_JPL_ETcanopy,
    ET_ECOSTRESS_daily=ECO3ETPTJPL_001_EVAPOTRANSPIRATION_PT_JPL_ETdaily,
    Time=godz)

et_data %>% glimpse()

merged_data %>% glimpse()


#3.0 Tidying data ----

merged_data %>% glimpse()

merged_data <- merged_data %>%
  mutate(LST_ECOSTRESS_TS = LST_ECOSTRESS_TA - 273.15) %>%
  select(
    Category,
    ID=ID.x,
    Latitude=Latitude.x,
    Longitude=Longitude.x,
    Date,
    ERA5_Ta=ERA5,
    LST_MODIS_Ts_day=LST_MODIS_Ta_day,
    LST_MODIS_Ts_night=LST_MODIS_Ta_day,
    MODIS_LAI,
    LST_ECOSTRESS_TS=LST_ECOSTRESS_TS,
    Time)

merged_data %>% view()

merged_data_11 <- All_data_11 %>%
  mutate(LST_ECOSTRESS_TA = LST_ECOSTRESS_TA - 273.15) %>%
  select(
    Category,
    ID = ID.x,
    Latitude = Latitude.x,
    Longitude = Longitude.x,
    Date,
    ERA5,
    LST_MODIS_Ts_day=LST_MODIS_Ta_day,
    LST_MODIS_Ts_night=LST_MODIS_Ta_day,
    MODIS_LAI,
    LST_ECOSTRESS_TS=LST_ECOSTRESS_TA,
    Time
  )

merged_data_11 %>% glimpse()



merged_data <- All_data %>%
  mutate(LST_ECOSTRESS_TA = LST_ECOSTRESS_TA - 273.15) %>%
  select(
    Category,
    ID = ID.x,
    Latitude = Latitude.x,
    Longitude = Longitude.x,
    Date,
    ERA5_Ta=ERA5,
    LST_MODIS_Ts_day=LST_MODIS_Ta_day,
    LST_MODIS_Ts_night=LST_MODIS_Ta_day,
    MODIS_LAI,
    LST_ECOSTRESS_Ts=LST_ECOSTRESS_TA,
    Time
  )


#4.0 Final results ====

ggplot(merged_data_11, aes(x = Date)) +
  geom_smooth(aes(y = LST_MODIS_Ts_day, color = "LST_MODIS_Ts_day")) +
  geom_point(aes(y = LST_ECOSTRESS_TS, color = "LST_ECOSTRESS_TS"))+
  geom_smooth(aes(y = ERA5, color = "ERA5"))+
  theme_minimal()+
  facet_wrap(~Category)+
  labs(title = "Porównanie Temperatur Modis, ECOSTRESS (tylko o godzinie 11:00 - 11:59) i ERA5 dla obszaru GRASSAT ", x = "Data", y = "Temperatura (°C)", color="Sensor")+
  theme_minimal()

merged_data_11 %>% glimpse()

merged_data %>% glimpse()

ggplot(merged_data, aes(x = Date)) +
  geom_smooth(aes(y = LST_MODIS_Ts_day, color = "LST_MODIS_Ts_day")) +
  geom_smooth(aes(y = LST_ECOSTRESS_Ts, color = "LST_ECOSTRESS_Ts")) +
  geom_point(aes(y = LST_ECOSTRESS_Ts, color = "LST_ECOSTRESS_Ts"))+
  geom_smooth(aes(y = ERA5_Ta, color = "ERA5_Ta"))+
  theme_minimal()+
  facet_wrap(~Category)+
  labs(title = "Porownanie Temperatur Modis, ECOSTRESS i ERA5 dla obszaru GRASSAT ",x = "Data",y = "Temperatura (°C)",color="Sensor")+
  theme_minimal()+
  scale_y_continuous(limits = c(-20, 40))


merged_data %>% view()

getwd()

write.csv2(merged_data, file="Temperatur(MODIS_ECOSTRESS_ERA5.csv")

#5.0 Vegetation periods ---- 

merged_data %>% glimpse()



# 1 Swath in 2022
period_1 <- merged_data %>%
  filter(between(Date, as.Date("2022-01-01"), as.Date("2022-05-31")))


ggplot(period_1, aes(x = Date)) +
  geom_smooth(aes(y = LST_MODIS_Ts_day, color = "LST_MODIS_Ts_day")) +
  geom_smooth(aes(y = LST_ECOSTRESS_TS, color = "LST_ECOSTRESS_TS")) +
  geom_point(aes(y = LST_ECOSTRESS_TS, color = "LST_ECOSTRESS_TS"))+
  geom_smooth(aes(y = ERA5_Ta, color = "ERA5_Ta"))+
  theme_minimal()+
  facet_wrap(~Category)+
  labs(title = "Porownanie Temperatur Modis, ECOSTRESS i ERA5 dla pierwszego pokosu w 2022r. (1.01.2022-31.05.2022) ",x = "Data",y = "Temperatura (°C)",color="Sensor")+
  theme_minimal()+
  scale_y_continuous(limits = c(-20, 40))

# 2 Swath in 2022 
period_1_a <- merged_data %>%
  filter(between(Date, as.Date("2022-06-01"), as.Date("2022-06-30")))


ggplot(period_1_a, aes(x = Date)) +
  geom_smooth(aes(y = LST_MODIS_Ts_day, color = "LST_MODIS_Ts_day")) +
  geom_smooth(aes(y = LST_ECOSTRESS_TS, color = "LST_ECOSTRESS_TS")) +
  geom_point(aes(y = LST_ECOSTRESS_TS, color = "LST_ECOSTRESS_TS"))+
  geom_smooth(aes(y = ERA5_Ta, color = "ERA5_Ta"))+
  theme_minimal()+
  facet_wrap(~Category)+
  labs(title = "Porownanie Temperatur Modis, ECOSTRESS i ERA5 dla drugiego pokosu w 2022r. (1.06.2022-30.06.2022) ",x = "Data",y = "Temperatura (°C)",color="Sensor")+
  theme_minimal()+
  scale_y_continuous(limits = c(-20, 40))

# 3 Swath in 2022
period_1_b <- merged_data %>%
  filter(between(Date, as.Date("2022-07-01"), as.Date("2022-12-31")))

ggplot(period_1_b, aes(x = Date)) +
  geom_smooth(aes(y = LST_MODIS_Ts_day, color = "LST_MODIS_Ts_day")) +
  geom_smooth(aes(y = LST_ECOSTRESS_TS, color = "LST_ECOSTRESS_TS")) +
  geom_point(aes(y = LST_ECOSTRESS_TS, color = "LST_ECOSTRESS_TS"))+
  geom_smooth(aes(y = ERA5_Ta, color = "ERA5_Ta"))+
  theme_minimal()+
  facet_wrap(~Category)+
  labs(title = "Porownanie Temperatur Modis, ECOSTRESS i ERA5 dla trzeciego pokosu w 2022r. (1.07.2022-31.12.2022) ",x = "Data",y = "Temperatura (°C)",color="Sensor")+
  theme_minimal()+
  scale_y_continuous(limits = c(-20, 40))+
scale_x_date(
    date_breaks = "1 month",  # Podział co 1 miesiąc
    date_labels = "%b"        # Format etykiet miesięcy
  )


#6.0  Statistics ----

merged_data %>% glimpse()

cleaned_data <- merged_data %>%
  filter(LST_ECOSTRESS_TS >= -25) %>% 
  filter(LST_ECOSTRESS_TS <= 50)

summary(cleaned_data[, c("ERA5_Ta", "LST_MODIS_Ts_day", "LST_ECOSTRESS_TS")])


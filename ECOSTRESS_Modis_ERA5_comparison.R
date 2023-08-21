#0.0 About the script ----

#In this project I will compare data from NASA sensors: Modis and ECOSTRESS and from ECMWF: ERA5.

#Comparison of temperature data from NASA sensors: ECOSTRESS, MODIS and ECMWF: ERA5
#in the area of meadow development under the GRASSAT programme
#Prepared by: MSc. Eng. Maciej Jurzyk, Institute of Geodesy and Cartography, August 2023
#Data: Temperature data obtained for the points was used to perform the analysis
#located in a random location at the Meadow measurement stations of the program
#GRASSAT. Instrument data used:
#  - Ts (surface temperature) NASA ECOSTRESS with a spatial resolution of 70m.
#The instrument is located on the International Space Station ISS and takes measurements in various
#hours during the day.
#- Ts (surface temperature) MODIS Terra, a satellite in a heliocentric orbit.
#A measurement with a spatial resolution of 1000 m was obtained, measurement around 11:00
#- Ta (atmospheric temperature at 2m height) from ERA5 Satellite with 11km spatial resolution

#1.0 R Environment ----

library(dplyr)
library(tidyverse)
library(lubridate)

#All data from Appears by nasa 


modis_lst <- read.csv("/Users/maciejjurzyk/Downloads/GRASSAT-Calosc-MOD11A1-061-results.csv")
modis_lai <- read.csv("/Users/maciejjurzyk/Downloads/GRASSAT-Calosc-MCD15A2H-061-results.csv")
ecostress_lst <- read.csv("/Users/maciejjurzyk/Downloads/GRASSAT-Calosc-ECO2LSTE-001-results.csv")
ecostress_et <- read.csv("/Users/maciejjurzyk/Downloads/GRASSAT-Calosc-ECO3ETPTJPL-001-results.csv")


#To start working on the tables, we need to do some cleaning and reformatting

# Let's start with - 

###1.1 MODIS ---- 

# Date and Time format - Time is not necessary for that table and can cause minor problems

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

# Let's present some graphs 

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

# Filtering data 

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

# More filtering 

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

#4.1 Final normalized  table 

write.csv2(merged_data, file="Temperatur(MODIS_ECOSTRESS_ERA5.csv")

#5.0 Vegetation periods ---- 

merged_data %>% glimpse()



# 5.1 First Swath in 2022 ----
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



cleaned_data %>%  glimpse()



# 7.0  Cumulative temperature sum----

merged_data %>% view()

##7.1 2020 ---- 

sum_data2020 <- merged_data   %>%
  group_by(Category, year = lubridate::year(Date)) %>% 
  ungroup() %>% 
  filter(year==2020)

sum_data2020 %>% glimpse()

sum_data2020_1 <- sum_data2020 %>%
  arrange(Category, Date) %>%
  group_by(Category, year = lubridate::year(Date)) %>%
  mutate(skumulowany_ERA5 = cumsum(ERA5_Ta)) %>%
  ungroup()


sum_data_2020_2 <- sum_data2020_1  %>%
  arrange(Category, Date) %>%
  group_by(Category, year = lubridate::year(Date)) %>%
  mutate(skumulowany_MODIS = cumsum(ifelse(is.na(LST_MODIS_Ts_day), 0, LST_MODIS_Ts_day))) %>%
  ungroup()


sum_data_2020_2 %>% glimpse()


##7.2 2021 ---- 

sum_data2021 <- merged_data  %>%
  group_by(Category, year = lubridate::year(Date)) %>% 
  ungroup() %>% 
  filter(year==2021)

sum_data2021 %>% glimpse()

sum_data2021_1 <- sum_data2021 %>%
  arrange(Category, Date) %>%
  group_by(Category, year = lubridate::year(Date)) %>%
  mutate(skumulowany_ERA5 = cumsum(ERA5_Ta)) %>%
  ungroup()


sum_data_2021_2 <- sum_data2021_1  %>%
  arrange(Category, Date) %>%
  group_by(Category, year = lubridate::year(Date)) %>%
  mutate(skumulowany_MODIS = cumsum(ifelse(is.na(LST_MODIS_Ts_day), 0, LST_MODIS_Ts_day))) %>%
  ungroup()


sum_data_2021_2 %>% glimpse()

##7.3 2022 ---- 

sum_data2022 <- merged_data  %>%
  group_by(Category, year = lubridate::year(Date)) %>% 
  ungroup() %>% 
  filter(year==2022)

sum_data2022 %>% glimpse()

sum_data2022_1 <- sum_data2022 %>%
  arrange(Category, Date) %>%
  group_by(Category, year = lubridate::year(Date)) %>%
  mutate(skumulowany_ERA5 = cumsum(ERA5_Ta)) %>%
  ungroup()


sum_data_2022_2 <- sum_data2022_1  %>%
  arrange(Category, Date) %>%
  group_by(Category, year = lubridate::year(Date)) %>%
  mutate(skumulowany_MODIS = cumsum(ifelse(is.na(LST_MODIS_Ts_day), 0, LST_MODIS_Ts_day))) %>%
  ungroup()


### 7.3.1 ECOSTRESS Data - dosen't make any sense :/ ----
sum_data_2022_3 <- sum_data_2022_2  %>%
  arrange(Category, Date) %>%
  group_by(Category, year = lubridate::year(Date)) %>%
  mutate(skumulowany_ECOSTRESS = cumsum(ifelse(is.na(LST_ECOSTRESS_TS), 0, LST_ECOSTRESS_TS))) %>%
  ungroup()


sum_data_2022_3 %>% view()


##7.4 GGPLOT ---- 


ggplot(sum_data_2022_2, aes(x = Date)) +
  geom_smooth(aes(y = skumulowany_ERA5, color = "skumulowany_ERA5")) +
  geom_smooth(aes(y = skumulowany_MODIS, color = "skumulowany_MODIS")) +
  labs(title = "Wzrost skumulowanych temperatur",
       x = "Data",
       y = "Wartość skumulowana") +
  scale_color_manual(values = c("skumulowany_ERA5" = "blue", "skumulowany_MODIS" = "red")) +
  facet_wrap(~Category)+
  theme_minimal()

sum_data_2022_2 %>% glimpse()


## 7.5 Creating one graph - Adding DOY, day, month colun, rbinding 3 tables into 1 ----


doy_data2020  <- sum_data_2020_2 %>%
  mutate(month = month(Date), day = day(Date), DOY = yday(Date))

doy_data2021 <- sum_data_2021_2 %>%
  mutate(month = month(Date), day = day(Date), DOY = yday(Date))

doy_data2022 <- sum_data_2022_2 %>%
  mutate(month = month(Date), day = day(Date), DOY = yday(Date))

combined_data <- rbind(doy_data2020, doy_data2021, doy_data2022)

combined_data %>% view()


### 7.5.1 GGPLOT ----


ggplot(combined_data, aes(x = DOY, y = skumulowany_ERA5, color = factor(year))) +
  geom_smooth() +
  labs(title = "Skumulowana temperatura w poszczególnych latach",
       x = "Numer dnia w roku (DOY)",
       y = "Skumulowana temperatura") +
  scale_color_discrete(name = "Rok") +
  scale_x_continuous(breaks = seq(1, max(combined_data$DOY), by = 14)) + 
  facet_grid(~Category)+
  theme_minimal()

## Done! 

### 7.5.2 Selecting only specific categories e.g (G1 i G2) ----

selected_categories <- c("Brody 1", "Kaczmarek 1", "T 1")
filtered_data <- combined_data %>% filter(Category %in% selected_categories)

# GGPLOT MODIS 

ggplot(filtered_data, aes(x = DOY, y = skumulowany_MODIS,color = factor(year))) +
  geom_smooth() +
  labs(title = "Skumulowana temperatura w poszczegolnych latach na polach pomiarowych GRASSAT",
       x = "Dzien roku",
       y = "Skumulowana temperatura",
       caption= "Na podstawie danych MODIS") +
  scale_color_discrete(name = "Rok") +
  scale_x_continuous(breaks = seq(1, max(filtered_data$DOY), by = 50)) + 
  scale_y_continuous(limits = c(0, 5000)) +
  facet_grid(~Category)+
  theme_minimal()

# GGPLOT ERA5

ggplot(filtered_data, aes(x = DOY, y = skumulowany_ERA5,color = factor(year))) +
  geom_smooth() +
  labs(title = "Skumulowana temperatura w poszczegolnych latach na polach pomiarowych GRASSAT",
       x = "Dzien roku",
       y = "Skumulowana temperatura",
       caption= "Na podstawie danych MODIS") +
  scale_color_discrete(name = "Rok") +
  scale_x_continuous(breaks = seq(1, max(filtered_data$DOY), by = 50)) + 
  scale_y_continuous(limits = c(0, 5000)) +
  facet_grid(~Category)+
  theme_minimal()

# combined_data final CSV

# 8.0  Cumulative ts_ta ----

merged_data %>% view()

##8.1 2020 ---- 

TS_TA2020 <- merged_data   %>%
  group_by(Category, year = lubridate::year(Date)) %>% 
  ungroup() %>% 
  filter(year==2020)


TS_TA2020_1 <- TS_TA2020 %>%
  mutate(ts_ta= LST_MODIS_Ts_day - ERA5_Ta)


TS_TA2020_2 <- TS_TA2020_1  %>%
  arrange(Category, Date) %>%
  group_by(Category, year = lubridate::year(Date)) %>%
  mutate(skumulowany_TS_TA = cumsum(ifelse(is.na(ts_ta), 0, ts_ta))) %>%
  ungroup()

TS_TA2020_2 %>% glimpse()


##8.2 2021 ---- 

TS_TA2021 <- merged_data   %>%
  group_by(Category, year = lubridate::year(Date)) %>% 
  ungroup() %>% 
  filter(year==2021)


TS_TA2021_1 <- TS_TA2021 %>%
  mutate(ts_ta= LST_MODIS_Ts_day - ERA5_Ta)


TS_TA2021_2 <- TS_TA2021_1  %>%
  arrange(Category, Date) %>%
  group_by(Category, year = lubridate::year(Date)) %>%
  mutate(skumulowany_TS_TA = cumsum(ifelse(is.na(ts_ta), 0, ts_ta))) %>%
  ungroup()

##8.3 2022 ---- 

TS_TA2022 <- merged_data   %>%
  group_by(Category, year = lubridate::year(Date)) %>% 
  ungroup() %>% 
  filter(year==2022)


TS_TA2022_1 <- TS_TA2022 %>%
  mutate(ts_ta= LST_MODIS_Ts_day - ERA5_Ta)


TS_TA2022_2 <- TS_TA2022_1  %>%
  arrange(Category, Date) %>%
  group_by(Category, year = lubridate::year(Date)) %>%
  mutate(skumulowany_TS_TA = cumsum(ifelse(is.na(ts_ta), 0, ts_ta))) %>%
  ungroup()


TS_TA2022_2 %>% view()


##8.4 GGPLOT ---- 


ggplot(TS_TA2022_2, aes(x = Date)) +
  geom_smooth(aes(y = skumulowany_TS_TA, color = "skumulowany_TS_TA")) +
  labs(title = "Wzrost skumulowanych ts_ta",
       x = "Data",
       y = "Wartość skumulowana") +
  facet_wrap(~Category)+
  theme_minimal()


## 8.5 Creating one graph - Adding DOY, day, month colun, rbinding 3 tables into 1 ----


doy_tsta2020  <- TS_TA2022_2 %>%
  mutate(month = month(Date), day = day(Date), DOY = yday(Date))

doy_tsta2021 <- TS_TA2021_2 %>%
  mutate(month = month(Date), day = day(Date), DOY = yday(Date))

doy_tsta2022 <- TS_TA2020_2 %>%
  mutate(month = month(Date), day = day(Date), DOY = yday(Date))

combinedtsta_data <- rbind(doy_tsta2022, doy_tsta2021, doy_tsta2020)

combinedtsta_data %>% view()


### 8.5.1 GGPLOT ----


ggplot(combinedtsta_data, aes(x = DOY, y = skumulowany_TS_TA, color = factor(year))) +
  geom_smooth() +
  labs(title = "Skumulowany TS-TA w poszczególnych latach",
       x = "Numer dnia w roku (DOY)",
       y = "Skumulowana temperatura") +
  scale_color_discrete(name = "Rok") +
  scale_x_continuous(breaks = seq(1, max(combined_data$DOY), by = 14)) + 
  facet_grid(~Category)+
  theme_minimal()

## Done! 

### 8.5.2 Selecting only specific categories e.g (G1 i G2) ----

selected_categories <- c("Brody 1", "Kaczmarek 1")
filtered_datatsta <- combinedtsta_data %>% filter(Category %in% selected_categories)



# GGPLOT MODIS 

ggplot(filtered_datatsta, aes(x = DOY, y = skumulowany_TS_TA,color = factor(year))) +
  geom_smooth() +
  labs(title = "Skumulowany TS-TA w poszczegolnych latach na polach pomiarowych GRASSAT",
       x = "Dzien roku",
       y = "Skumulowana TS_TA",
       caption= "Na podstawie danych MODIS i ERA5") +
  scale_color_discrete(name = "Rok") +
  scale_x_continuous(breaks = seq(1, max(filtered_data$DOY), by = 50)) + 
  facet_grid(~Category)+
  theme_minimal()


# combinedtsta_data - final table






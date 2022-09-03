library(dplyr)
library(ggplot2)

setwd("C:\\Users\\erica\\Documents\\R")


db <- "healthcare-dataset-stroke-data.csv"

health <- read.csv(db, sep = ",", header = TRUE)

# para ver el numero de filas y columnas.
dim(health)

# estructura del DF
str(health)

class(health)
typeof(health)

# nombres de las columnas
names(health)

# acceso a los datos
health[3:6, ]

# para ver si tiene datos vacios
any(is.na(health))

# filtra los valores de bmi con N/A
filter <- health$bmi == "N/A"
health <- health[filter == FALSE, ]

# filtra los valores de la columna fumadores con el valor Unknown
filter <- health$smoking_status == "Unknown"
health <- health[filter == FALSE, ]

health$hypertension[health$hypertension == 0] <- "No"
health$hypertension[health$hypertension == 1] <- "Yes"


#renombrar smoking_status
health$smoking_status[health$smoking_status =="formerly smoked"] <- 1
health$smoking_status[health$smoking_status =="never smoked"] <- 0
health$smoking_status[health$smoking_status =="smokes"] <- 1


health$age_group <- cut(health$age, breaks = seq(0, 100, 10))

health$bmi <- as.integer(health$bmi)
health$bmi_group <- cut(health$bmi, breaks = seq(0,100,10))

#hipertension x infarto
strokes_by_hypetension <- health %>%
  select(hypertension,stroke) %>%
  group_by(hypertension) %>%
  summarise(target = sum(stroke == 1))

#edad x infarto
strokes_by_age <- health %>%
    filter(health$hypertension == "Yes") %>%
    group_by(age_group) %>%
    summarise(target = sum(stroke == 1))

#bmi x edad
bmi_by_age <- health %>%
  group_by(bmi_group) %>%
  summarise(target = sum(stroke == 1))


#infartos x fumar
strokes_by_smoke <- health %>%
  select(smoking_status,stroke) %>%
  filter(stroke == 1) %>%
  group_by(smoking_status) %>%
  summarise(target = sum(stroke ==1))

#enfermedades x edad
disease_by_age <- health %>%
  group_by(age_group) %>%
  summarise(target = sum(heart_disease ==1))

#enfermedades x fumar
disease_by_smoke <- health %>%
  select(smoking_status,heart_disease) %>%
  group_by(smoking_status) %>%
  summarise(target = sum(heart_disease ==1))

#enfermedades x peso

disease_by_bmi <- health %>%
  



  
ggplot(strokes_by_age, aes(x = age_group, y = target)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(title = "Strokes by Age", x = "Age", y = "Strokes")

ggplot(bmi_by_age, aes(x = bmi_group, y = target)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Strokes by bmi", x = "Age", y = "Strokes")

ggplot(strokes_by_smoke, aes(x = smoking_status, y = target)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Strokes by smoke", x = "Age", y = "Strokes")


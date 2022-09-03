library(dplyr)
library(ggplot2)

# Cambiar WD.
setwd("C:\\Users\\katak\\Desktop\\R")

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

# Renombrar hipertensión.
health$hypertension[health$hypertension == 0] <- "No"
health$hypertension[health$hypertension == 1] <- "Yes"

# Renombrar diabetes
health$avg_glucose_status[health$avg_glucose_level < 127] <- "Normal"
health$avg_glucose_status[health$avg_glucose_level >= 127] <- "Diabetes"

# renombrar smoking_status
health$smoking_status[health$smoking_status == "formerly smoked"] <- 1
health$smoking_status[health$smoking_status == "never smoked"] <- 0
health$smoking_status[health$smoking_status == "smokes"] <- 1


health$age_group <- cut(health$age, breaks = seq(30, 110, 10))
health$bmi_group <- cut(as.numeric(health$bmi), breaks = seq(15, 60, 5.5))

strokes_by_hypetension <- health %>%
    select(hypertension, stroke) %>%
    group_by(hypertension) %>%
    summarise(target = sum(stroke == 1))

strokes_by_age <- health %>%
    group_by(age_group) %>%
    summarise(target = sum(stroke == 1))

strokes_by_bmi <- health %>%
    group_by(bmi_group) %>%
    summarise(target = sum(stroke == 1))

strokes_by_smoke <- health %>%
    select(smoking_status, stroke) %>%
    filter(stroke == 1) %>%
    group_by(smoking_status) %>%
    summarise(target = sum(stroke == 1))

strokes_by_glucose <- health %>%
    select(avg_glucose_level, stroke) %>%
    filter(stroke == 1) %>%
    group_by(avg_glucose_level) %>%
    summarise(target = sum(stroke == 1))

# strokes by gender
strokes_by_gender <- health %>%
    select(gender, stroke) %>%
    filter(stroke == 1) %>%
    group_by(gender) %>%
    summarise(target = sum(stroke == 1))

# enfermedades x edad
disease_by_age <- health %>%
    group_by(age_group) %>%
    summarise(target = sum(heart_disease == 1))

# enfermedades x fumar
disease_by_smoke <- health %>%
    select(smoking_status, heart_disease) %>%
    group_by(smoking_status) %>%
    summarise(target = sum(heart_disease == 1))

# enfermedades x peso
disease_by_bmi <- health %>%
    group_by(bmi_group) %>%
    summarise(target = sum(heart_disease == 1))

ggplot(strokes_by_age, aes(x = age_group, y = target)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(title = "Infartos por grupo etario", x = "Edad", y = "Infartos")

ggplot(strokes_by_bmi, aes(x = bmi_group, y = target)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(title = "Infartos por IMC", x = "IMC", y = "Infartos")

ggplot(strokes_by_smoke, aes(x = smoking_status, y = target)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(title = "Strokes by smoke", x = "Age", y = "Strokes")

ggplot(strokes_by_gender, aes(x = gender, y = target)) +
    geom_bar(stat = "identity", fill = "blue") +
    labs(title = "Infartos por genero", x = "Genero", y = "Infartos")


# strokes_by_gender <- health %>%
#     filter(health$hypertension == 1) %>%
#     group_by(health$gender) %>%
#     summarise(target = sum(stroke))

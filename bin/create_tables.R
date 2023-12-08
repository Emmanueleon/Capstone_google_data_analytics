# Exportación y limpieza de base de datos

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Preparación del ambiente
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
## Limpieza del ambiente 
rm(list = ls())

## Cargar los paquetes a utilizar
library(tidyverse)
library(DataExplorer)
library(ggpubr)

## Carga de objeto
daily_activity <- read.csv("../data/dailyActivity_merged.csv")
daily_calories <- read_csv("../data/dailyCalories_merged.csv")
daily_sleep <- read_csv("../data/minuteSleep_merged.csv")
daily_heart_rate <- read_csv("../data/heartrate_seconds_merged.csv")
weight_loss_info <- read_csv("../data/weightLogInfo_merged.csv")



# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Tablas de dato conjuntas
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



## Base datos 

## Actividad diaria
# se compone de 15 variables ( 1 de tipo caracter y 14 numéricas) y 940 observaciones.

daily_activity |> 
  glimpse()

daily_calories |> 
  glimpse()


daily_sleep |> 
  glimpse()



## Creación de tabla evaluación 
daily_activity_report <- daily_activity |> 
  select(Id:TrackerDistance) |> 
  rename("ActivityDay"=ActivityDate) |> 
  inner_join(daily_calories, by= c("Id", "ActivityDay")) |> 
  #inner_join(daily_sleep, by= c("Id", "ActivityDay")) |> 
  as_tibble() |> 
  
  ## Unifrom names
  rename_all(~ str_replace_all(., "(?<=[a-z])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])", "_")) |> 
  rename_all(tolower) |>
  
  ## Change date info and factor variables 
  mutate(
    activity_day = mdy(activity_day), 
    weekday= weekdays(activity_day),
    weekday_type = case_when(!weekday %in% c("Saturday", "Sunday")~"Weekday", 
                             weekday %in% c("Saturday", "Sunday")~"Weekend"),
    across(c(id, weekday, weekday_type), factor))




  
  

# Cambio del nombre de la base de datos `ActivityDate` para que corresponda con las otras bases
# Sin embargo, notamos algunas inconsistencias con el tipo de datos que contiene. 
# Por ejemplo notamos que la columna Id es de tipo numérico y otras columnas contienen 
# fechas y tiempo. Por lo que es necesario hacer el parseo de esas variables. 


# Resumen actividad física 
## ¿Cuál es el día que tiene una mayor cantidad de pasos
daily_activity_report |> 
  group_by(weekday) |> 
  summarise(avg_total_steps = mean(total_steps), 
            avg_total_distance =  mean(total_distance), 
            avg_tracker_distance = mean(total_distance), 
            avg_total_calories = mean(calories), 
  ) |> 
  arrange(desc(avg_total_calories))


# De acuerdo con los resultados observamos que en promedio 
#   * El Sábado es el día con mayor número de pasos y distancia total. Seguido de los días Martes y Lunes, días de inicio de semana. 
#     Por último y de manera esperada el día Domingo es el que menor actividad registra en términos de pasos y distancia registrada. 
#     Es posible que el día sábado sea el más activo entre nuestros usuarios debido a que parte de nuestro target son personas que cuidan su salud física,
#     por lo que suponemos que son personas que hacen ejercicio el sábado. 
#   * En promedio el día que tiene una mayor ingesta de calorías es el día Martes y los Sábados, mientras que los días Domingo y Jueves son los días que
#     menos ingesta de caloría registran. 


# ¿Existe relación entre el número de pasos, la ingesta de calorías y el peso corporal de acuerdo con el IMC?

## Preparar la tabla de pérdida de peso 
weight_loss_tbl <- weight_loss_info |> 
  rename("activity_day" = Date) |> 
  ## Unifrom names
  rename_all(~ str_replace_all(., "(?<=[a-z])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])", "_")) |> 
  rename_all(tolower) |> 
  
  ## Change date info and factor variables 
  mutate(bmi_classification= case_when(bmi < 18.5~"uw", 
                                       between(bmi, 18.5,24.99)~"np",
                                       between(bmi, 25,29.99)~"sp",
                                       bmi >= 30~"ob",
                                       TRUE~"Check"), 
         activity_day_time = mdy_hms(activity_day),
         activity_day = date(activity_day_time), 
         across(c(id, bmi_classification), factor))|> 
  select(id, weight_kg, bmi, is_manual_report, bmi_classification, activity_day)



# Asociación entre la 
daily_activity_report_imc <- daily_activity_report |> 
  inner_join(weight_loss_tbl, by= c("id", "activity_day")) |> 
  print()


## Resumen por BMI
daily_activity_report_imc |> 
  summarize(no = n(), 
            mean_calories = mean(calories), 
            mean_steps = mean(total_steps), 
            .by = bmi_classification)


daily_activity_report_imc |> 
  filter(!bmi_classification == "ob") |> 
  ggplot(aes(x = bmi_classification, y = total_steps, fill = bmi_classification)) +
  geom_boxplot(alpha= 0.8) +
  geom_jitter( color="black", size = 2.5, alpha = 0.6) +
  scale_fill_manual(values = c("#708d81", "#8d0801"))+
  labs(
    x = "BMI Classification",
    y = "Total Steps",
    title = "Daily Activity Report by BMI Classification",
    subtitle = "Excluding 'obese' category ") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size= 12, hjust = 1),
    legend.position = "none"
    )
  


daily_activity_report_imc |> 
  filter(!bmi_classification == "ob") |> 
  ggplot(aes(x = bmi_classification, y = calories, fill = bmi_classification)) +
  geom_boxplot(alpha= 0.8) +
  geom_jitter( color="black", size = 2.5, alpha = 0.6) +
  scale_fill_manual(values = c("#708d81", "#8d0801"))+
  labs(
    x = "BMI Classification",
    y = "Total Steps",
    title = "Daily Activity Report by BMI Classification",
    subtitle = "Excluding 'obese' category ") +
  theme_classic() +
  theme(
    axis.text.x = element_text(size= 12, hjust = 1),
    legend.position = "none"
  )













## Debido a que es un gran conjunto de datos tomamos una muestra representativa para fines prácticos del caso de estudio. 

daily_activity_report %>%
  ggplot(aes(x = total_steps, y = calories)) +
  geom_jitter(alpha = 0.6, color = "#1f78b4", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  stat_cor(label.x = 0.9, label.y = 0.9, label.sep = "\n", aes(color = "blue")) +
  labs(
    x = "Total Steps",
    y = "Calories Burned",
    title = "Relationship between Total Steps and Calories Burned",
    caption = "Source: Your Data Source"
  ) +
  theme_minimal()

## Activity summaries 
summary_stats <- daily_activity_tbl |> 
  group_by(week_day) |> 
  summarise(avg_steps= mean(TotalSteps), 
            avg_distance= mean(TotalDistance), .groups = "drop") |> 
  print()

## Número de mediciones 
### Notamos que existieron 6 Ids que contenían menos del 90% de los registros, por lo que los eliminamos.  
daily_activity_tbl |> 
  group_by(weekday_type) |> 
  summarise(no=length(total_steps), 
            mean=mean(total_steps,na.rm=TRUE),
            sd=sd(total_steps,na.rm=TRUE))
arrange(desc(max))
filter(no > 27)


daily_activity_tbl <- 
  
  shapiro.test(daily_activity_tbl$Total_steps)
shapiro.test(daily_activity_tbl$Total_distance)


plot_bar(daily_activity_tbl)



ggplot(daily_activity_tbl, aes(x = ActivityDate, y = Total_steps, color=Id)) +
  geom_line() +
  labs(title = "Daily Activity",
       x = "Activity Date",
       y = "Total Steps")

view(daily_activity_tbl)




## Valores de sueño 
daily_sleep |> 
  rename("ActivityDay"=date) |> 
  rename_all(~ str_replace_all(., "(?<=[a-z])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])", "_")) |> 
  rename_all(tolower) |>
  
  ## Change date info and factor variables 
  mutate(
    activity_day = mdy_hms(activity_day), 
    day = date(activity_day)) |>
  group_by(id, day) |> 
  summarise(total_sleep_minutes = sum(value), .groups = "drop") |> 
  mutate(total_sleep_hours = as.period(duration(minutes = total_sleep_minutes)))    

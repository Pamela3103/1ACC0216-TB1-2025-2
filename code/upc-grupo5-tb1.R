#------------------------------------------------
# Cargar datos 
#-------------------------------------------------
hotel_data <-read.csv("C:\\Users\\pamela\\1ACC0216-TB1-2025-2\\data\\hotel_bookings_raw.csv", header=TRUE, sep=",")
View(hotel_data)

#------------------------------------------------
# Inspeccionar datos 
#-------------------------------------------------
str(hotel_data)

#------------------------------------------------
# Transformaciones iniciales
#------------------------------------------------

# Convertir negativos a 0
hotel_data$adr[hotel_data$adr < 0] <- 0

# Reemplazar valores "NULL" por NA
hotel_data[hotel_data == "NULL"] <- NA

# Convertir variables categóricas a factor
cols_factor <- c("hotel", "meal", "country", "market_segment", 
                 "distribution_channel", "reserved_room_type", 
                 "assigned_room_type", "deposit_type", 
                 "customer_type", "reservation_status")
hotel_data[cols_factor] <- lapply(hotel_data[cols_factor], as.factor)

# Convertir fecha al tipo Date
hotel_data$reservation_status_date <- as.Date(hotel_data$reservation_status_date)

# Verificar duplicados
sum(duplicated(hotel_data)) # Hay duplicados pero se mantienen

#------------------------------------------------
# Resumen estadístico de las variables 
#------------------------------------------------
summary(hotel_data)

#------------------------------------------------
# Identificación de valores faltantes
#------------------------------------------------
colSums(is.na(hotel_data))

#------------------------------------------------
# Tratamiento de Datos Faltantes
#------------------------------------------------

# 1. Imputar valores faltantes en 'children' con la moda
moda_children <- as.numeric(names(sort(table(hotel_data$children), decreasing = TRUE))[1])
hotel_data$children[is.na(hotel_data$children)] <- moda_children

# 2. Imputar valores faltantes en 'country' con la moda
moda_country <- names(sort(table(hotel_data$country), decreasing = TRUE)[1])
hotel_data$country[is.na(hotel_data$country)] <- moda_country

# 3. Reemplazar valores faltantes en 'agent' y 'company' por "0"
hotel_data$agent[hotel_data$agent == "NULL" | is.na(hotel_data$agent)] <- "0"
hotel_data$company[hotel_data$company == "NULL" | is.na(hotel_data$company)] <- "0"

# Verificar nuevamente los valores faltantes
colSums(is.na(hotel_data))


#------------------------------------------------
# Detección de Outliers
#------------------------------------------------

# Primer grupo de variables
vars_1 <- c("lead_time", "adr", "stays_in_week_nights")

# Configurar para mostrar 3 gráficos
par(mfrow=c(1,3))

# Crear boxplots
for (var in vars_1) {
  boxplot(hotel_data[[var]], main=paste("Boxplot de", var), col="lightblue")
}

# Restaurar configuración
par(mfrow=c(1,1))


# Segundo grupo de variables
vars_2 <- c("stays_in_weekend_nights", "previous_cancellations", "previous_bookings_not_canceled", "days_in_waiting_list")

# Configurar para mostrar graficos
par(mfrow=c(2,2))

# Crear boxplots
for (var in vars_2) {
  boxplot(hotel_data[[var]], main=paste("Boxplot de", var), col="lightblue")
}

# Restaurar configuración
par(mfrow=c(1,1))

#------------------------------------------------
# Tratamiento de outliers 
#-------------------------------------------------

# Variables a winsorizar
numeric_vars <- c("lead_time", "adr", "stays_in_weekend_nights",
                  "stays_in_week_nights", "previous_cancellations",
                  "previous_bookings_not_canceled", "days_in_waiting_list")

# Hacer copia del dataset
hotel_wins <- hotel_data

# Inicializar vector para registrar cuántos valores se modificaron
modificados <- numeric(length(numeric_vars))
names(modificados) <- numeric_vars

# Loop: calcular límites IQR y winsorizar cada variable
for (v in numeric_vars) {
  x_orig <- hotel_data[[v]]
  # Evitar errores si toda la variable es NA o tiene var=0
  if (all(is.na(x_orig))) {
    modificados[v] <- NA
    next
  }
  # Calcular Q1, Q3 e IQR (ignorando NA)
  q1 <- quantile(x_orig, 0.25, na.rm = TRUE)
  q3 <- quantile(x_orig, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  limite_inferior <- q1 - 1.5 * iqr
  limite_superior <- q3 + 1.5 * iqr
  
  # Copiar la columna y reemplazar por los límites cuando corresponda
  x_new <- x_orig
  # Condiciones solo donde no es NA
  idx_low <- which(!is.na(x_orig) & x_orig < limite_inferior)
  idx_high <- which(!is.na(x_orig) & x_orig > limite_superior)
  
  if (length(idx_low) > 0) x_new[idx_low] <- limite_inferior
  if (length(idx_high) > 0) x_new[idx_high] <- limite_superior
  
  # Guardar en el dataset winsorizado
  hotel_wins[[v]] <- x_new
  
  # Contar modificaciones (comparando valores diferentes, ignorando NA)
  modificados[v] <- sum((x_orig != x_new) & !is.na(x_orig))
}

# Mostrar resumen de cuántos valores se modificaron por variable
print("Número de valores modificados por variable (winsorización IQR):")
print(modificados)

# Guardar el dataset winsorizado
write.csv(hotel_wins, "hotel_data_winsorizado_IQR.csv", row.names = FALSE)
cat("✅ Archivo 'hotel_data_winsorizado_IQR.csv' guardado.\n")

#------------------------------------------------
# Visualización de datos (dataset-winsorizado)
#-------------------------------------------------

library(ggplot2)
library(dplyr)

# Usar el dataset winsorizado
hotel_data_win <-read.csv("C:\\Users\\pamela\\1ACC0216-TB1-2025-2\\code\\hotel_data_winsorizado_IQR.csv", header=TRUE, sep=",")

# PREGUNTA 1. ¿Cuántas reservas se realizan por tipo de hotel? ¿Qué tipo de hotel prefiere la gente?
ggplot(hotel_data_win, aes(x = hotel, fill = hotel)) +
  geom_bar() +
  labs(
    title = "Cantidad de reservas por tipo de hotel",
    x = "Tipo de hotel",
    y = "Número de reservas"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# PREGUNTA 2. ¿Está aumentando la demanda con el tiempo?
hotel_data_win %>%
  group_by(arrival_date_year) %>%
  summarise(reservas = n()) %>%
  ggplot(aes(x = arrival_date_year, y = reservas)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "darkblue", size = 3) +
  labs(
    title = "Evolución de la demanda de reservas por año",
    x = "Año de llegada",
    y = "Número de reservas"
  ) +
  theme_minimal()

# pregunta 3. ¿Cuáles son las temporadas de reservas (alta, media, baja)?
# Ordenar meses cronológicamente
hotel_data_win$arrival_date_month <- factor(
  hotel_data_win$arrival_date_month,
  levels = month.name
)

ggplot(hotel_data_win, aes(x = arrival_date_month, fill = hotel)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Reservas por mes y tipo de hotel",
    x = "Mes de llegada",
    y = "Número de reservas",
    fill = "Tipo de hotel"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# PREGUNTA 4. ¿Cuál es la duración promedio de las estancias por tipo de hotel?
hotel_data_win %>%
  mutate(total_stay = stays_in_week_nights + stays_in_weekend_nights) %>%
  group_by(hotel) %>%
  summarise(promedio_estancia = mean(total_stay)) %>%
  ggplot(aes(x = hotel, y = promedio_estancia, fill = hotel)) +
  geom_col() +
  labs(
    title = "Duración promedio de las estancias por tipo de hotel",
    x = "Tipo de hotel",
    y = "Promedio de noches"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# PREGUNTA 5. ¿Cuántas reservas incluyen niños y/o bebés?
hotel_data_win %>%
  mutate(con_ninos = ifelse(children > 0 | babies > 0, "Sí", "No")) %>%
  ggplot(aes(x = con_ninos, fill = con_ninos)) +
  geom_bar() +
  labs(
    title = "Reservas que incluyen niños y/o bebés",
    x = "Incluye niños o bebés",
    y = "Número de reservas"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# PREGUNTA 6. ¿Es importante contar con espacios de estacionamiento?
hotel_data_win %>%
  group_by(required_car_parking_spaces) %>%
  summarise(reservas = n()) %>%
  ggplot(aes(x = factor(required_car_parking_spaces), y = reservas, fill = factor(required_car_parking_spaces))) +
  geom_col() +
  labs(
    title = "Reservas según número de espacios de estacionamiento requeridos",
    x = "Espacios de estacionamiento",
    y = "Número de reservas",
    fill = "Espacios"
  ) +
  theme_minimal()

# PREGUNTA 7. ¿En qué meses del año se producen más cancelaciones de reservas?
hotel_data_win %>%
filter(is_canceled == 1) %>%
  group_by(arrival_date_month) %>%
  summarise(cancelaciones = n()) %>%
  ggplot(aes(x = arrival_date_month, y = cancelaciones, fill = arrival_date_month)) +
  geom_col() +
  labs(
    title = "Cancelaciones de reservas por mes",
    x = "Mes de llegada",
    y = "Número de cancelaciones"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# PREGUNTA 8. ¿Cuál es la relación entre el tiempo de anticipación de la reserva y las cancelaciones?
ggplot(hotel_data_win, aes(x = lead_time, y = is_canceled)) +
  geom_jitter(alpha = 0.3, color = "darkred") +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  labs(
    title = "Relación entre tiempo de reserva y cancelación",
    x = "Días de anticipación (lead_time)",
    y = "Cancelación (1 = Sí, 0 = No)"
  ) +
  theme_minimal()

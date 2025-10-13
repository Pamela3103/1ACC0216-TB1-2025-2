# 1ACC0216-TB1-2025-2
## Objetivo del trabajo
Realizar un análisis exploratorio de un conjunto de datos (EDA) para encontrar patrones de comportamiento, generando visualizaciones, preparando los datos y extrayendo conclusiones iniciales utilizando R/RStudio como herramienta de software.

## Alumnos participantes
- Rivera Contreras, Pamela Ivonne
- Zavala Arteaga, Gabriel Aldo

## Conjunto de datos
El dataset analizado se denomina **Hotel booking demand**. Su versión original se obtuvo de [Kaggle](https://www.kaggle.com/), pero para esta evaluación se ha modificado incorporando ruido en los datos, como valores faltantes (NA) y datos atípicos (outliers).  

Este dataset recopila información de reservas de un hotel urbano y otro tipo resort, incluyendo datos como:  
- Fecha de la reserva  
- Duración de la estadía  
- Cantidad de espacios de estacionamiento disponibles  
- Cantidad de huéspedes (adultos, niños, bebés)  
- Otros datos relevantes de la reserva  

El dataset original proviene del documento: *Hotel booking demand datasets*.  
El dataset modificado se puede descargar desde [AQUI](ruta/al/archivo.pdf).

## Estructura del repositorio
El repositorio contiene dos carpetas principales:  
- **data**: contiene el dataset original y el dataset final preparado para análisis.  
- **code**: contiene los scripts en R utilizados para la carga, inspección, preprocesamiento y visualización del dataset.

## Conclusiones
- El **City Hotel** concentra la mayoría de las reservas, con picos entre mayo y agosto.  
- El **Resort Hotel** presenta estancias más largas pero menor volumen de huéspedes.  
- El año **2016** registró la mayor demanda total, con una ligera disminución en **2017**.  
- La mayoría de las reservas no incluye niños ni requiere estacionamiento, reflejando un perfil **adulto y urbano**.  
- Las **cancelaciones** son más frecuentes en los meses de mayor actividad y se relacionan con el **tiempo de anticipación**.  
- Se recomienda fortalecer la **gestión operativa en temporada alta**, implementar **políticas de cancelación estratégicas**, **diferenciar las ofertas** entre hoteles y diseñar **campañas orientadas a adultos**.

## Licencia
MIT License – ver [LICENSE](LICENSE)

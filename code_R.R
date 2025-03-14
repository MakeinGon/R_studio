  
  install.packages("readxl")  
  
  library(readxl)
  
  
  Datos <- read_excel("E:/UNAS_MAKEIN/R/Precios_Automoviles.xlsx")
  
  
  View(Datos)  
  head(Datos)  
  
  ####################################################################################################################################################################################################################################################################################
  # 1.Transmisión → (Manual, Automática, Semiautomática)   
  # 1.Transmisión → (Manual, Automática, Semiautomática)   
  # 📌 Cargar librerías necesarias
  library(readxl)
  library(ggplot2)
  
  # 📌 Crear tabla de frecuencia para Transmisión
  tablafrec1 <- table(Transmision = factor(Datos$Transmision))
  
  # 📌 Transformar en un data frame y agregar frecuencias acumuladas
  tablafrec2 <- as.data.frame(tablafrec1)
  tablafrec2$hi <- round(prop.table(tablafrec2$Freq), 3)
  tablafrec2$hiPorc <- round(tablafrec2$hi * 100, 3)
  
  # 📌 Agregar fila del total
  total_fila <- data.frame(Transmision = "Total", 
                           Freq = sum(tablafrec2$Freq), 
                           hi = sum(tablafrec2$hi), 
                           hiPorc = sum(tablafrec2$hiPorc))
  
  # 📌 Unir la tabla con la fila del total
  tablafrec2 <- rbind(tablafrec2, total_fila)
  
  # 📌 Mostrar la tabla
  print(tablafrec2)
  
  # 📌 Gráfico de barras con ggplot2
  ggplot(tablafrec2[tablafrec2$Transmision != "Total", ], aes(x = Transmision, y = Freq, fill = Transmision)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(hiPorc, "%")), vjust = -0.5) +  
    labs(title = "Distribución de Tipos de Transmisión", x = "Tipo de Transmisión", y = "Frecuencia") +
    theme_minimal()
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #######################################################################################################################################################################################################################################################################################################################################################################
  # 2. Tipo de Combustible → (Gasolina, Diésel, Híbrido, Eléctrico)
  
  # 📌 Cargar librerías necesarias
  library(readxl)
  library(ggplot2)
  
  # 📌 Extraer la variable "Tipo de Combustible"
  combustible <- factor(Datos$Tipo_combustible)
  
  # 📌 Crear la tabla de frecuencias
  tablafrec1 <- table(Combustible = combustible)
  
  # 📌 Transformar la tabla para incluir frecuencias acumuladas y relativas
  tablafrec2 <- transform(as.data.frame(tablafrec1),
                          Fi = cumsum(Freq),  # Frecuencia Acumulada
                          hi = round(prop.table(Freq), 3),  # Frecuencia Relativa
                          Hi = round(cumsum(prop.table(Freq)), 3),  # Frecuencia Relativa Acumulada
                          hiPorc = round(prop.table(Freq) * 100, 3),  # Frecuencia Relativa Porcentual
                          HiPorc = round(cumsum(prop.table(Freq) * 100), 3))  # Frecuencia Relativa Porcentual Acumulada
  
  # 📌 Agregar la fila de Total
  total_fila <- data.frame(Combustible = "Total",
                           Freq = sum(tablafrec2$Freq),
                           Fi = NA,  # No tiene sentido acumulado para total
                           hi = sum(tablafrec2$hi),
                           Hi = NA,
                           hiPorc = sum(tablafrec2$hiPorc),
                           HiPorc = NA)
  
  # 📌 Unir la tabla con la fila de Total
  tablafrec2 <- rbind(tablafrec2, total_fila)
  
  # 📌 Mostrar la tabla de frecuencia con el formato solicitado
  print(tablafrec2, row.names = FALSE)
  
  # 📌 Función para crear el gráfico circular
  graficar_combustible <- function(tabla) {
    # Filtrar la fila de "Total"
    tabla <- tabla[tabla$Combustible != "Total", ]
    
    # Crear el gráfico de pastel (Pie Chart)
    ggplot(tabla, aes(x = "", y = Freq, fill = Combustible)) +
      geom_bar(stat = "identity", width = 1, color = "black") +
      coord_polar("y", start = 0) +
      geom_text(aes(label = paste0(round(hiPorc, 2), "%")), position = position_stack(vjust = 0.5), size = 4) +
      labs(title = "Distribución de Tipos de Combustible") +
      theme_void() +
      theme(legend.title = element_blank())
  }
  
  # 📌 Generar el gráfico circular
  graficar_combustible(tablafrec2)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #######################################################################################################################################################################################################################################################################################################################################################################
  #3. VARIABLE MARCA
  
  # 📌 Cargar librerías necesarias
  library(readxl)
  library(ggplot2)
  
  # 📌 Leer los datos
  
  # 📌 Extraer la variable "Marca"
  marca <- factor(Datos$Marca)
  
  # 📌 Crear la tabla de frecuencias
  tablafrec1 <- table(Marca = marca)
  
  # 📌 Transformar la tabla para incluir frecuencias acumuladas y relativas
  tablafrec2 <- transform(as.data.frame(tablafrec1),
                          Fi = cumsum(Freq),  # Frecuencia Acumulada
                          hi = round(prop.table(Freq), 3),  # Frecuencia Relativa
                          Hi = round(cumsum(prop.table(Freq)), 3),  # Frecuencia Relativa Acumulada
                          hiPorc = round(prop.table(Freq) * 100, 3),  # Frecuencia Relativa Porcentual
                          HiPorc = round(cumsum(prop.table(Freq) * 100), 3))  # Frecuencia Relativa Porcentual Acumulada
  
  # 📌 Agregar la fila de Total
  total_fila <- data.frame(Marca = "Total",
                           Freq = sum(tablafrec2$Freq),
                           Fi = NA,  # No tiene sentido acumulado para total
                           hi = sum(tablafrec2$hi),
                           Hi = NA,
                           hiPorc = sum(tablafrec2$hiPorc),
                           HiPorc = NA)
  
  # 📌 Unir la tabla con la fila de Total
  tablafrec2 <- rbind(tablafrec2, total_fila)
  
  # 📌 Mostrar la tabla de frecuencia con el formato solicitado
  print(tablafrec2, row.names = FALSE)
  
  # 📌 Cálculo de la Moda (Marca más vendida)
  calcular_moda_marca <- function(tabla) {
    # Filtrar la fila de "Total"
    tabla <- tabla[tabla$Marca != "Total", ]
    
    # Obtener la marca con la mayor frecuencia
    moda <- as.character(tabla$Marca[which.max(tabla$Freq)])
    
    # 📌 Mostrar la moda con el nombre correcto
    cat("\n📊 La Marca Más Vendida es:", moda, "\n")
  }
  
  # 📌 Función para crear el gráfico circular
  graficar_marca <- function(tabla) {
    # Filtrar la fila de "Total"
    tabla <- tabla[tabla$Marca != "Total", ]
    
    # Crear el gráfico de pastel (Pie Chart)
    ggplot(tabla, aes(x = "", y = Freq, fill = Marca)) +
      geom_bar(stat = "identity", width = 1, color = "black") +
      coord_polar("y", start = 0) +
      geom_text(aes(label = paste0(round(hiPorc, 2), "%")), position = position_stack(vjust = 0.5), size = 4) +
      labs(title = "Distribución de Marcas de Autos") +
      theme_void() +
      theme(legend.title = element_blank())
  }
  
  # 📌 Ejecutar el cálculo de la moda
  calcular_moda_marca(tablafrec2)
  
  # 📌 Generar el gráfico circular
  graficar_marca(tablafrec2)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #######################################################################################################################################################################################################################################################################################################################################################################
  #4. Modelo → (Corolla, Focus, X5, C-Class)
  
  
  # 📌 Cargar librerías necesarias
  library(readxl)
  library(ggplot2)
  
  # 📌 Extraer la variable "Modelo"
  modelo <- factor(Datos$Modelo)
  
  # 📌 Crear la tabla de frecuencias
  tablafrec1 <- table(Modelo = modelo)
  
  # 📌 Transformar la tabla para incluir frecuencias acumuladas y relativas
  tablafrec2 <- transform(as.data.frame(tablafrec1),
                          Fi = cumsum(Freq),  # Frecuencia Acumulada
                          hi = round(prop.table(Freq), 3),  # Frecuencia Relativa
                          Hi = round(cumsum(prop.table(Freq)), 3),  # Frecuencia Relativa Acumulada
                          hiPorc = round(prop.table(Freq) * 100, 3),  # Frecuencia Relativa Porcentual
                          HiPorc = round(cumsum(prop.table(Freq) * 100), 3))  # Frecuencia Relativa Porcentual Acumulada
  
  # 📌 Agregar la fila de Total
  total_fila <- data.frame(Modelo = "Total",
                           Freq = sum(tablafrec2$Freq),
                           Fi = NA,  # No tiene sentido acumulado para total
                           hi = sum(tablafrec2$hi),
                           Hi = NA,
                           hiPorc = sum(tablafrec2$hiPorc),
                           HiPorc = NA)
  
  # 📌 Unir la tabla con la fila de Total
  tablafrec2 <- rbind(tablafrec2, total_fila)
  
  # 📌 Mostrar la tabla de frecuencia con el formato solicitado
  print(tablafrec2, row.names = FALSE)
  
  # 📌 Cálculo de la Moda (Modelo más vendido)
  calcular_moda_modelo <- function(tabla) {
    # Filtrar la fila de "Total"
    tabla <- tabla[tabla$Modelo != "Total", ]
    
    # Obtener el modelo con la mayor frecuencia
    moda <- as.character(tabla$Modelo[which.max(tabla$Freq)])
    
    # 📌 Mostrar la moda con el nombre correcto
    cat("\n📊 El Modelo Más Vendido es:", moda, "\n")
  }
  
  # 📌 Función para crear el gráfico de barras
  graficar_modelo <- function(tabla) {
    # Filtrar la fila de "Total"
    tabla <- tabla[tabla$Modelo != "Total", ]
    
    # Crear el gráfico de barras
    ggplot(tabla, aes(x = reorder(Modelo, -Freq), y = Freq, fill = Modelo)) +
      geom_bar(stat = "identity", color = "black") +
      geom_text(aes(label = Freq), vjust = -0.5, size = 4) +
      labs(title = "Distribución de Modelos de Autos", x = "Modelo", y = "Frecuencia") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  # 📌 Ejecutar el cálculo de la moda
  calcular_moda_modelo(tablafrec2)
  
  # 📌 Generar el gráfico de barras
  graficar_modelo(tablafrec2)
  
  
  
  
  
  
  

  
  
  
  
  
  
  
  
    
    
    ##############################################################################################################################################################################################################################################################################
    #5. Número de Propietarios → (Ejemplo: 1, 2, 3, 4)
    
    library(readxl)
    library(ggplot2)
    
    # 📌 Crear tabla de frecuencia para Número de Propietarios
    tablafrec1 <- table(Propietarios = factor(Datos$Numero_propietarios))
    
    # 📌 Transformar en un data frame y calcular frecuencias acumuladas
    tablafrec2 <- as.data.frame(tablafrec1)
    tablafrec2$Fi <- cumsum(tablafrec2$Freq)
    tablafrec2$hi <- round(prop.table(tablafrec2$Freq), 3)
    tablafrec2$Hi <- round(cumsum(prop.table(tablafrec2$hi)), 3)
    tablafrec2$hiPorc <- round(tablafrec2$hi * 100, 3)
    tablafrec2$HiPorc <- round(tablafrec2$Hi * 100, 3)
    
    # 📌 Agregar fila de Total
    total_fila <- data.frame(Propietarios = "Total", 
                             Freq = sum(tablafrec2$Freq), 
                             Fi = NA,  # No tiene sentido acumulado para total
                             hi = sum(tablafrec2$hi), 
                             Hi = NA,  
                             hiPorc = sum(tablafrec2$hiPorc), 
                             HiPorc = NA)
    
    # 📌 Unir la tabla con la fila del Total
    tablafrec2 <- rbind(tablafrec2, total_fila)
    
    # 📌 Mostrar la tabla mejorada
    print(tablafrec2)
    
    # 📌 Cálculo de la Moda (Número de Propietarios más común)
    calcular_moda_propietarios <- function(tabla) {
      # Filtrar la fila de "Total"
      tabla <- tabla[tabla$Propietarios != "Total", ]
      
      # Obtener el número de propietarios con la mayor frecuencia
      moda <- as.character(tabla$Propietarios[which.max(tabla$Freq)])
      
      # 📌 Mostrar la moda
      cat("\n📊 La Cantidad de Propietarios Más Común es:", moda, "\n")
    }
    
    # 📌 Función para calcular medidas estadísticas completas
    calcular_estadisticas_completas <- function(tabla) {
      # Filtrar la fila de "Total"
      tabla <- tabla[tabla$Propietarios != "Total", ]
      
      # Convertir la columna de propietarios a numérica
      tabla$Propietarios <- as.numeric(as.character(tabla$Propietarios))
      
      N <- sum(tabla$Freq)  # Total de observaciones
      
      # 📌 Media Aritmética
      media_aritmetica <- sum(tabla$Propietarios * tabla$Freq) / N
      
      # 📌 Media Armónica
      media_armonica <- N / sum(tabla$Freq / tabla$Propietarios)
      
      # 📌 Media Geométrica
      media_geometrica <- exp(sum(tabla$Freq * log(tabla$Propietarios)) / N)
      
      # 📌 Mediana
      N_2 <- N / 2
      clase_mediana_idx <- which(tabla$Fi >= N_2)[1]
      mediana <- tabla$Propietarios[clase_mediana_idx]
      
      # 📌 Moda (Número de propietarios más común)
      moda <- as.character(tabla$Propietarios[which.max(tabla$Freq)])
      
      # 📌 Varianza y Desviación Estándar
      varianza <- sum(tabla$Freq * (tabla$Propietarios - media_aritmetica)^2) / (N - 1)
      desviacion_estandar <- sqrt(varianza)
      
      # 📌 Coeficiente de Variación (%)
      coef_variacion <- (desviacion_estandar / media_aritmetica) * 100
      
      # 📌 Mostrar resultados con los valores de la imagen
      cat("\n📊 Medidas Estadísticas para Número de Propietarios\n")
      cat("-------------------------------------------------------------\n")
      cat("Media Aritmética: ", round(media_aritmetica, 2), "\n")
      cat("Mediana: ", mediana, "\n")
      cat("Moda: ", moda, "\n")
      cat("Media Armónica: ", round(media_armonica, 2), "\n")
      cat("Media Geométrica: ", round(media_geometrica, 2), "\n")
      cat("Varianza: ", round(varianza, 2), "\n")
      cat("Desviación Estándar: ", round(desviacion_estandar, 2), "\n")
      cat("Coeficiente de Variación (%): ", round(coef_variacion, 2), "%\n")
    }
    
    # 📌 Gráfico de Barras para Número de Propietarios
    graficar_propietarios_barras <- function(tabla) {
      # Filtrar la fila de "Total"
      tabla <- tabla[tabla$Propietarios != "Total", ]
      
      ggplot(tabla, aes(x = factor(Propietarios), y = Freq, fill = factor(Propietarios))) +
        geom_bar(stat = "identity", color = "black") +
        geom_text(aes(label = Freq), vjust = -0.5, size = 4) +
        labs(title = "Distribución de Número de Propietarios", x = "Número de Propietarios", y = "Frecuencia") +
        theme_minimal()
    }
    
    # 📌 Gráfico Circular para Número de Propietarios
    graficar_propietarios_pie <- function(tabla) {
      # Filtrar la fila de "Total"
      tabla <- tabla[tabla$Propietarios != "Total", ]
      
      ggplot(tabla, aes(x = "", y = Freq, fill = factor(Propietarios))) +
        geom_bar(stat = "identity", width = 1, color = "black") +
        coord_polar("y", start = 0) +
        geom_text(aes(label = paste0(round(hiPorc, 2), "%")), position = position_stack(vjust = 0.5), size = 4) +
        labs(title = "Distribución de Número de Propietarios") +
        theme_void() +
        theme(legend.title = element_blank())
    }
    
    # 📌 Ejecutar el cálculo de la moda
    calcular_moda_propietarios(tablafrec2)
    
    # 📌 Ejecutar el cálculo de medidas estadísticas completas
    calcular_estadisticas_completas(tablafrec2)
    
    # 📌 Generar el gráfico de barras
    graficar_propietarios_barras(tablafrec2)
    
    # 📌 Generar el gráfico circular
    graficar_propietarios_pie(tablafrec2)
    
    
  
  
  
  
    
    
    ############################################################################################################################################################################################################################################################################
    # 6. Año de Fabricación → (Ejemplo: 2005, 2018, 2023)
    
    library(readxl)
    library(ggplot2)
    
    # 📌 Extraer la variable "Año de Fabricación"
    anio_fabricacion <- factor(Datos$Anio)
    
    # 📌 Crear la tabla de frecuencias
    tablafrec1 <- table(Anios = anio_fabricacion)
    
    # 📌 Transformar la tabla para incluir frecuencias acumuladas y relativas
    tablafrec2 <- transform(as.data.frame(tablafrec1),
                            Fi = cumsum(Freq),  # Frecuencia Acumulada
                            hi = round(prop.table(Freq), 3),  # Frecuencia Relativa
                            Hi = round(cumsum(prop.table(Freq)), 3),  # Frecuencia Relativa Acumulada
                            hiPorc = round(prop.table(Freq) * 100, 3),  # Frecuencia Relativa Porcentual
                            HiPorc = round(cumsum(prop.table(Freq) * 100), 3))  # Frecuencia Relativa Porcentual Acumulada
    
    # 📌 Agregar la fila de Total
    total_fila <- data.frame(Anios = "Total",
                             Freq = sum(tablafrec2$Freq),
                             Fi = NA,  # No tiene sentido acumulado para total
                             hi = sum(tablafrec2$hi),
                             Hi = NA,
                             hiPorc = sum(tablafrec2$hiPorc),
                             HiPorc = NA)
    
    # 📌 Unir la tabla con la fila de Total
    tablafrec2 <- rbind(tablafrec2, total_fila)
    
    # 📌 Mostrar la tabla de frecuencia con el formato solicitado
    print(tablafrec2, row.names = FALSE)
    
    # 📌 Función para calcular medidas estadísticas
    calcular_estadisticas_anio <- function(tabla) {
      # Filtrar la fila de "Total"
      tabla <- tabla[tabla$Anios != "Total", ]
      
      # Convertir la columna de años a numérica
      tabla$Anios <- as.numeric(as.character(tabla$Anios))
      
      N <- sum(tabla$Freq)  # Total de observaciones
      
      # 📌 Media Aritmética
      media_aritmetica <- sum(tabla$Anios * tabla$Freq) / N
      
      # 📌 Media Armónica
      media_armonica <- N / sum(tabla$Freq / tabla$Anios)
      
      # 📌 Media Geométrica
      media_geometrica <- exp(sum(tabla$Freq * log(tabla$Anios)) / N)
      
      # 📌 Mediana
      N_2 <- N / 2
      clase_mediana_idx <- which(tabla$Fi >= N_2)[1]
      mediana <- tabla$Anios[clase_mediana_idx]
      
      # 📌 Varianza y Desviación Estándar
      varianza <- sum(tabla$Freq * (tabla$Anios - media_aritmetica)^2) / (N - 1)
      desviacion_estandar <- sqrt(varianza)
      
      # 📌 Coeficiente de Variación (%)
      coef_variacion <- (desviacion_estandar / media_aritmetica) * 100
      
      # 📌 Mostrar resultados
      cat("\n📊 Medidas Estadísticas para Año de Fabricación\n")
      cat("-------------------------------------------------------------\n")
      cat("Media Aritmética: ", round(media_aritmetica, 2), "\n")
      cat("Mediana: ", mediana, "\n")
      cat("Media Armónica: ", round(media_armonica, 2), "\n")
      cat("Media Geométrica: ", round(media_geometrica, 2), "\n")
      cat("Varianza: ", round(varianza, 2), "\n")
      cat("Desviación Estándar: ", round(desviacion_estandar, 2), "\n")
      cat("Coeficiente de Variación (%): ", round(coef_variacion, 2), "%\n")
    }
    
    # 📌 Ejecutar el cálculo de medidas estadísticas
    calcular_estadisticas_anio(tablafrec2)
    
    # 📌 Gráfico de Barras para Año de Fabricación
    ggplot(tablafrec2[tablafrec2$Anios != "Total", ], aes(x = Anios, y = Freq, fill = Anios)) +
      geom_bar(stat = "identity", color = "black") +
      geom_text(aes(label = Freq), vjust = -0.5, size = 4) +  
      labs(title = "Distribución del Año de Fabricación de los Automóviles",
           x = "Año de Fabricación",
           y = "Frecuencia") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas del eje X
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ##########################################################################################################################################################################################################################################################################
    #7. Número de Puertas → (Ejemplo: 2, 3, 4, 5)
    
    
    
    # 📌 Cargar librerías necesarias
    library(readxl)
    library(ggplot2)
    
    # 📌 Leer los datos
    Datos <- read_excel("E:/UNAS_MAKEIN/R/Precios_Automoviles.xlsx")
    
    # 📌 Extraer la variable "Puertas"
    puertas <- factor(Datos$Puertas)
    
    # 📌 Crear la tabla de frecuencias
    tablafrec1 <- table(Puertas = puertas)
    
    # 📌 Transformar la tabla para incluir frecuencias acumuladas y relativas
    tablafrec2 <- transform(as.data.frame(tablafrec1),
                            Fi = cumsum(Freq),  # Frecuencia Acumulada
                            hi = round(prop.table(Freq), 3),  # Frecuencia Relativa
                            Hi = round(cumsum(prop.table(Freq)), 3),  # Frecuencia Relativa Acumulada
                            hiPorc = round(prop.table(Freq) * 100, 3),  # Frecuencia Relativa Porcentual
                            HiPorc = round(cumsum(prop.table(Freq) * 100), 3))  # Frecuencia Relativa Porcentual Acumulada
    
    # 📌 Agregar la fila de Total
    total_fila <- data.frame(Puertas = "Total",
                             Freq = sum(tablafrec2$Freq),
                             Fi = NA,  # No tiene sentido acumulado para total
                             hi = sum(tablafrec2$hi),
                             Hi = NA,
                             hiPorc = sum(tablafrec2$hiPorc),
                             HiPorc = NA)
    
    # 📌 Unir la tabla con la fila de Total
    tablafrec2 <- rbind(tablafrec2, total_fila)
    
    # 📌 Mostrar la tabla de frecuencia con el formato solicitado
    print(tablafrec2, row.names = FALSE)
    
    # 📌 Cálculo de la Moda (Cantidad de puertas más común)
    calcular_moda_puertas <- function(tabla) {
      # Filtrar la fila de "Total"
      tabla <- tabla[tabla$Puertas != "Total", ]
      
      # Obtener la cantidad de puertas con la mayor frecuencia
      moda <- as.character(tabla$Puertas[which.max(tabla$Freq)])
      
      # 📌 Mostrar la moda con el nombre correcto
      cat("\n📊 La Cantidad de Puertas Más Común es:", moda, "puertas \n")
    }
    
    # 📌 Función para calcular medidas estadísticas completas
    calcular_estadisticas_puertas <- function(tabla) {
      # Filtrar la fila de "Total"
      tabla <- tabla[tabla$Puertas != "Total", ]
      
      # Convertir la columna de puertas a numérica
      tabla$Puertas <- as.numeric(as.character(tabla$Puertas))
      
      N <- sum(tabla$Freq)  # Total de observaciones
      
      # 📌 Media Aritmética
      media_aritmetica <- sum(tabla$Puertas * tabla$Freq) / N
      
      # 📌 Media Armónica
      media_armonica <- N / sum(tabla$Freq / tabla$Puertas)
      
      # 📌 Media Geométrica
      media_geometrica <- exp(sum(tabla$Freq * log(tabla$Puertas)) / N)
      
      # 📌 Mediana
      N_2 <- N / 2
      clase_mediana_idx <- which(tabla$Fi >= N_2)[1]
      mediana <- tabla$Puertas[clase_mediana_idx]
      
      # 📌 Varianza y Desviación Estándar
      varianza <- sum(tabla$Freq * (tabla$Puertas - media_aritmetica)^2) / (N - 1)
      desviacion_estandar <- sqrt(varianza)
      
      # 📌 Coeficiente de Variación (%)
      coef_variacion <- (desviacion_estandar / media_aritmetica) * 100
      
      # 📌 Mostrar resultados con los valores de la imagen
      cat("\n📊 Medidas Estadísticas para Número de Puertas\n")
      cat("-------------------------------------------------------------\n")
      cat("Media Aritmética: ", round(media_aritmetica, 2), "\n")
      cat("Mediana: ", mediana, "\n")
      cat("Moda: ", as.character(tabla$Puertas[which.max(tabla$Freq)]), "\n")
      cat("Media Armónica: ", round(media_armonica, 2), "\n")
      cat("Media Geométrica: ", round(media_geometrica, 2), "\n")
      cat("Varianza: ", round(varianza, 2), "\n")
      cat("Desviación Estándar: ", round(desviacion_estandar, 2), "\n")
      cat("Coeficiente de Variación (%): ", round(coef_variacion, 2), "%\n")
    }
    
    # 📌 Función para crear el gráfico circular
    graficar_puertas <- function(tabla) {
      # Filtrar la fila de "Total"
      tabla <- tabla[tabla$Puertas != "Total", ]
      
      # Crear el gráfico de pastel (Pie Chart)
      ggplot(tabla, aes(x = "", y = Freq, fill = Puertas)) +
        geom_bar(stat = "identity", width = 1, color = "black") +
        coord_polar("y", start = 0) +
        geom_text(aes(label = paste0(round(hiPorc, 2), "%")), position = position_stack(vjust = 0.5), size = 4) +
        labs(title = "Distribución de Número de Puertas") +
        theme_void() +
        theme(legend.title = element_blank())
    }
    
    # 📌 Ejecutar el cálculo de la moda
    calcular_moda_puertas(tablafrec2)
    
    # 📌 Ejecutar el cálculo de medidas estadísticas
    calcular_estadisticas_puertas(tablafrec2)
    
    # 📌 Generar el gráfico circular
    graficar_puertas(tablafrec2)
    
    
    
    
    
    
    
  
  
  
  
  
  
  
  
  ##################################################################################################################################################################################################################
  #8.Precio → (Ejemplo: $5000, $15,000, $30,000)

  
  
  # 📌 Cargar librerías necesarias
  library(readxl)
  library(ggplot2)
  
  # 📌 Extraer la variable Precio
  precio <- Datos$Precio
  
  # 📌 Función para calcular el número óptimo de intervalos usando la Regla de Sturges
  calcular_numero_intervalos <- function(datos) {
    n <- length(datos)
    k <- round(1 + 3.322 * log10(n))  
    return(k)
  }
  
  # 📌 Función para calcular la tabla de frecuencias
  calcular_tabla_frecuencias <- function(datos) {
    num_intervalos <- calcular_numero_intervalos(datos)
    rango <- max(datos) - min(datos)
    ancho_intervalo <- rango / num_intervalos
    
    intervalos <- seq(from = min(datos), to = max(datos), by = ancho_intervalo)
    frecuencias <- hist(datos, breaks = intervalos, plot = FALSE)$counts
    
    diferencia <- length(datos) - sum(frecuencias)
    if (diferencia != 0) {
      frecuencias[length(frecuencias)] <- frecuencias[length(frecuencias)] + diferencia
    }
    
    marcas_clase <- (intervalos[-length(intervalos)] + intervalos[-1]) / 2
    Fi <- cumsum(frecuencias)  
    hi <- frecuencias / length(datos)  
    Hi <- cumsum(hi)  
    hi_porcentaje <- hi * 100  
    Hi_porcentaje <- Hi * 100  
    
    return(list(intervalos = intervalos, marcas_clase = marcas_clase, frecuencias = frecuencias, 
                Fi = Fi, hi = hi, Hi = Hi, hi_porcentaje = hi_porcentaje, Hi_porcentaje = Hi_porcentaje))
  }
  
  # 📌 Función para mostrar la tabla de frecuencias con el nombre correcto
  mostrar_tabla_frecuencias <- function(tabla) {
    cat("\n📊 Tabla de Frecuencias para Precio:\n")
    cat("+-------------------+---------+------+------+------+------+------+------+\n")
    cat("| [Li – Ls]         |   Xi    | fi   |  Fi  |  hi  |  Hi  | hi%  |  Hi% |\n")
    cat("+-------------------+---------+------+------+------+------+------+------+\n")
    
    for (i in 1:length(tabla$frecuencias)) {
      cat(sprintf("| %5.2f - %5.2f | %7.2f | %4d | %4d | %4.2f | %4.2f | %3d%% | %3d%% |\n",
                  tabla$intervalos[i], tabla$intervalos[i+1], tabla$marcas_clase[i], 
                  tabla$frecuencias[i], tabla$Fi[i], tabla$hi[i], tabla$Hi[i], 
                  round(tabla$hi_porcentaje[i]), round(tabla$Hi_porcentaje[i])))
    }
    
    cat("+---------------------+----------+------+-------+-----+-----+----+----+\n")
    cat(sprintf("| Total:                         |%4d |        | 1.00 |     | 100%% |     |\n",
                sum(tabla$frecuencias)))
    cat("+---------------------+----------+------+-------+-----+-----+----+----+\n")
  }
  
  # 📌 Función para calcular Medidas de Tendencia Central y Dispersión
  calcular_estadisticas <- function(tabla) {
    N <- sum(tabla$frecuencias)  
    
    media_aritmetica <- sum(tabla$marcas_clase * tabla$frecuencias) / N
    media_armonica <- N / sum(tabla$frecuencias / tabla$marcas_clase)
    media_geometrica <- exp(sum(tabla$frecuencias * log(tabla$marcas_clase)) / N)
    
    N_2 <- N / 2
    clase_mediana_idx <- which(tabla$Fi >= N_2)[1]
    L_mediana <- tabla$intervalos[clase_mediana_idx]
    F_anterior <- ifelse(clase_mediana_idx == 1, 0, tabla$Fi[clase_mediana_idx - 1])
    f_mediana <- tabla$frecuencias[clase_mediana_idx]
    h <- mean(diff(tabla$marcas_clase))
    mediana <- L_mediana + (((N_2 - F_anterior) / f_mediana) * h)
    
    clase_moda_idx <- which.max(tabla$frecuencias)
    L_moda <- tabla$intervalos[clase_moda_idx]
    f_moda <- tabla$frecuencias[clase_moda_idx]
    f_anterior <- ifelse(clase_moda_idx == 1, 0, tabla$frecuencias[clase_moda_idx - 1])
    f_siguiente <- ifelse(clase_moda_idx == length(tabla$frecuencias), 0, tabla$frecuencias[clase_moda_idx + 1])
    d1 <- f_moda - f_anterior
    d2 <- f_moda - f_siguiente
    moda <- L_moda + ((d1 / (d1 + d2)) * h)
    
    varianza <- sum(tabla$frecuencias * (tabla$marcas_clase - media_aritmetica)^2) / (N - 1)
    desviacion_estandar <- sqrt(varianza)
    coef_variacion <- (desviacion_estandar / media_aritmetica) * 100
    
    cat("\n📊 Medidas de Tendencia Central y Dispersión para Precio\n")
    cat("-------------------------------------------------------------\n")
    cat("Media Aritmética: ", round(media_aritmetica, 2), "\n")
    cat("Media Armónica: ", round(media_armonica, 2), "\n")
    cat("Media Geométrica: ", round(media_geometrica, 2), "\n")
    cat("Mediana: ", round(mediana, 2), "\n")
    cat("Moda: ", round(moda, 2), "\n")
    cat("Varianza: ", round(varianza, 2), "\n")
    cat("Desviación Estándar: ", round(desviacion_estandar, 2), "\n")
    cat("Coeficiente de Variación (%): ", round(coef_variacion, 2), "%\n")
  }
  
  # 📌 Función para graficar la distribución del Precio con un gráfico de barras
  graficar_precio_barras <- function(tabla) {
    intervalos_str <- apply(cbind(tabla$intervalos[-length(tabla$intervalos)], tabla$intervalos[-1]), 1, 
                            function(x) sprintf("[%5.2f – %5.2f)", x[1], x[2]))
    
    df <- data.frame(intervalos = factor(intervalos_str, levels = intervalos_str), frecuencias = tabla$frecuencias)
    
    ggplot(df, aes(x = intervalos, y = frecuencias, fill = intervalos)) +
      geom_bar(stat = "identity", color = "black", fill = "skyblue") +
      geom_text(aes(label = frecuencias), vjust = -0.5, size = 4) +
      labs(title = "Distribución del Precio", x = "Intervalos de Precio", y = "Frecuencia") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  # 📌 Función Principal para ejecutar todo el análisis
  main <- function() {
    tabla <- calcular_tabla_frecuencias(precio)
    mostrar_tabla_frecuencias(tabla)  
    calcular_estadisticas(tabla)
    graficar_precio_barras(tabla)  
  }
  
  # 📌 Ejecutar la función principal
  main()
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ################################################################################################################################################################################################################################################################################################
  #(9).Kilometraje → (Ejemplo: 15,000 km, 75,000 km, 230,000 km)
  
  # 📌 Cargar librerías necesarias
  library(readxl)
  library(ggplot2)
  

  # 📌 Extraer la variable Kilometraje
  kilometraje <- Datos$Kilometraje
  
  # 📌 Función para calcular el número óptimo de intervalos usando la Regla de Sturges
  calcular_numero_intervalos <- function(datos) {
    n <- length(datos)
    k <- round(1 + 3.322 * log10(n))  
    return(k)
  }
  
  # 📌 Función para calcular la tabla de frecuencias
  calcular_tabla_frecuencias <- function(datos) {
    num_intervalos <- calcular_numero_intervalos(datos)
    rango <- max(datos) - min(datos)
    ancho_intervalo <- rango / num_intervalos
    
    intervalos <- seq(from = min(datos), to = max(datos), by = ancho_intervalo)
    frecuencias <- hist(datos, breaks = intervalos, plot = FALSE)$counts
    
    diferencia <- length(datos) - sum(frecuencias)
    if (diferencia != 0) {
      frecuencias[length(frecuencias)] <- frecuencias[length(frecuencias)] + diferencia
    }
    
    marcas_clase <- (intervalos[-length(intervalos)] + intervalos[-1]) / 2
    Fi <- cumsum(frecuencias)  
    hi <- frecuencias / length(datos)  
    Hi <- cumsum(hi)  
    hi_porcentaje <- hi * 100  
    Hi_porcentaje <- Hi * 100  
    
    return(list(intervalos = intervalos, marcas_clase = marcas_clase, frecuencias = frecuencias, 
                Fi = Fi, hi = hi, Hi = Hi, hi_porcentaje = hi_porcentaje, Hi_porcentaje = Hi_porcentaje))
  }
  
  # 📌 Función para mostrar la tabla de frecuencias con el nombre corregido
  mostrar_tabla_frecuencias <- function(tabla) {
    cat("\n📊 Tabla de Frecuencias para Kilometraje:\n")
    cat("+-------------------+---------+------+------+------+------+------+------+\n")
    cat("| [Li – Ls]         |   Xi    | fi   |  Fi  |  hi  |  Hi  | hi%  |  Hi% |\n")
    cat("+-------------------+---------+------+------+------+------+------+------+\n")
    
    for (i in 1:length(tabla$frecuencias)) {
      cat(sprintf("| %5.2f - %5.2f | %7.2f | %4d | %4d | %4.2f | %4.2f | %3d%% | %3d%% |\n",
                  tabla$intervalos[i], tabla$intervalos[i+1], tabla$marcas_clase[i], 
                  tabla$frecuencias[i], tabla$Fi[i], tabla$hi[i], tabla$Hi[i], 
                  round(tabla$hi_porcentaje[i]), round(tabla$Hi_porcentaje[i])))
    }
    
    cat("+---------------------+----------+------+-------+-----+-----+----+----+\n")
    cat(sprintf("| Total:                         |%4d |        | 1.00 |     | 100%% |     |\n",
                sum(tabla$frecuencias)))
    cat("+---------------------+----------+------+-------+-----+-----+----+----+\n")
  }
  
  # 📌 Función para calcular Medidas de Tendencia Central y Dispersión
  calcular_estadisticas <- function(tabla) {
    N <- sum(tabla$frecuencias)  
    
    media_aritmetica <- sum(tabla$marcas_clase * tabla$frecuencias) / N
    media_armonica <- N / sum(tabla$frecuencias / tabla$marcas_clase)
    media_geometrica <- exp(sum(tabla$frecuencias * log(tabla$marcas_clase)) / N)
    
    N_2 <- N / 2
    clase_mediana_idx <- which(tabla$Fi >= N_2)[1]
    L_mediana <- tabla$intervalos[clase_mediana_idx]
    F_anterior <- ifelse(clase_mediana_idx == 1, 0, tabla$Fi[clase_mediana_idx - 1])
    f_mediana <- tabla$frecuencias[clase_mediana_idx]
    h <- mean(diff(tabla$marcas_clase))
    mediana <- L_mediana + (((N_2 - F_anterior) / f_mediana) * h)
    
    clase_moda_idx <- which.max(tabla$frecuencias)
    L_moda <- tabla$intervalos[clase_moda_idx]
    f_moda <- tabla$frecuencias[clase_moda_idx]
    f_anterior <- ifelse(clase_moda_idx == 1, 0, tabla$frecuencias[clase_moda_idx - 1])
    f_siguiente <- ifelse(clase_moda_idx == length(tabla$frecuencias), 0, tabla$frecuencias[clase_moda_idx + 1])
    d1 <- f_moda - f_anterior
    d2 <- f_moda - f_siguiente
    moda <- L_moda + ((d1 / (d1 + d2)) * h)
    
    varianza <- sum(tabla$frecuencias * (tabla$marcas_clase - media_aritmetica)^2) / (N - 1)
    desviacion_estandar <- sqrt(varianza)
    coef_variacion <- (desviacion_estandar / media_aritmetica) * 100
    
    cat("\n📊 Medidas de Tendencia Central y Dispersión para Kilometraje\n")
    cat("-------------------------------------------------------------\n")
    cat("Media Aritmética: ", round(media_aritmetica, 2), "\n")
    cat("Media Armónica: ", round(media_armonica, 2), "\n")
    cat("Media Geométrica: ", round(media_geometrica, 2), "\n")
    cat("Mediana: ", round(mediana, 2), "\n")
    cat("Moda: ", round(moda, 2), "\n")
    cat("Varianza: ", round(varianza, 2), "\n")
    cat("Desviación Estándar: ", round(desviacion_estandar, 2), "\n")
    cat("Coeficiente de Variación (%): ", round(coef_variacion, 2), "%\n")
  }
  
  # 📌 Función para graficar la distribución del Kilometraje con un gráfico de barras
  graficar_kilometraje_barras <- function(tabla) {
    intervalos_str <- apply(cbind(tabla$intervalos[-length(tabla$intervalos)], tabla$intervalos[-1]), 1, 
                            function(x) sprintf("[%5.2f – %5.2f)", x[1], x[2]))
    
    df <- data.frame(intervalos = factor(intervalos_str, levels = intervalos_str), frecuencias = tabla$frecuencias)
    
    ggplot(df, aes(x = intervalos, y = frecuencias, fill = intervalos)) +
      geom_bar(stat = "identity", color = "black", fill = "skyblue") +
      geom_text(aes(label = frecuencias), vjust = -0.5, size = 4) +
      labs(title = "Distribución del Kilometraje", x = "Intervalos de Kilometraje", y = "Frecuencia") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  # 📌 Función Principal para ejecutar todo el análisis
  main <- function() {
    tabla <- calcular_tabla_frecuencias(kilometraje)
    mostrar_tabla_frecuencias(tabla)  
    calcular_estadisticas(tabla)
    graficar_kilometraje_barras(tabla)  
  }
  
  # 📌 Ejecutar la función principal
  main()
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ######################################################################################################################################################################################################################################################################################################################################################################
  #10. Tamaño del Motor → (Ejemplo: 1.6L, 2.0L, 3.5L)
  #VARIABLE tamaño moto
  
  # 📌 Cargar librerías necesarias
  library(readxl)
  library(ggplot2)
  
  
  # 📌 Extraer la variable "Tamanio_motor"
  tamanio_motor <- Datos$Tamanio_motor
  
  # 📌 Función para calcular el número óptimo de intervalos usando la Regla de Sturges
  calcular_numero_intervalos <- function(datos) {
    n <- length(datos)
    k <- round(1 + 3.322 * log10(n))  
    return(k)
  }
  
  # 📌 Función para calcular la tabla de frecuencias con intervalos
  calcular_tabla_frecuencias_motor <- function(datos) {
    num_intervalos <- calcular_numero_intervalos(datos)
    rango <- max(datos) - min(datos)
    ancho_intervalo <- rango / num_intervalos
    
    intervalos <- seq(from = min(datos), to = max(datos), by = ancho_intervalo)
    frecuencias <- hist(datos, breaks = intervalos, plot = FALSE)$counts
    
    diferencia <- length(datos) - sum(frecuencias)
    if (diferencia != 0) {
      frecuencias[length(frecuencias)] <- frecuencias[length(frecuencias)] + diferencia
    }
    
    marcas_clase <- (intervalos[-length(intervalos)] + intervalos[-1]) / 2
    Fi <- cumsum(frecuencias)  
    hi <- frecuencias / length(datos)  
    Hi <- cumsum(hi)  
    hi_porcentaje <- hi * 100  
    Hi_porcentaje <- Hi * 100  
    
    return(list(intervalos = intervalos, marcas_clase = marcas_clase, frecuencias = frecuencias, 
                Fi = Fi, hi = hi, Hi = Hi, hi_porcentaje = hi_porcentaje, Hi_porcentaje = Hi_porcentaje))
  }
  
  # 📌 Función para mostrar la tabla de frecuencias
  mostrar_tabla_frecuencias_motor <- function(tabla) {
    cat("\n📊 Tabla de Frecuencias para Tamaño del Motor:\n")
    cat("+-------------------+---------+------+------+------+------+------+------+\n")
    cat("| [Li – Ls]         |   Xi    | fi   |  Fi  |  hi  |  Hi  | hi%  |  Hi% |\n")
    cat("+-------------------+---------+------+------+------+------+------+------+\n")
    
    for (i in 1:length(tabla$frecuencias)) {
      cat(sprintf("| %5.2f - %5.2f | %7.2f | %4d | %4d | %4.2f | %4.2f | %3d%% | %3d%% |\n",
                  tabla$intervalos[i], tabla$intervalos[i+1], tabla$marcas_clase[i], 
                  tabla$frecuencias[i], tabla$Fi[i], tabla$hi[i], tabla$Hi[i], 
                  round(tabla$hi_porcentaje[i]), round(tabla$Hi_porcentaje[i])))
    }
    
    cat("+---------------------+----------+------+-------+-----+-----+----+----+\n")
    cat(sprintf("| Total:                         |%4d |        | 1.00 |     | 100%% |     |\n",
                sum(tabla$frecuencias)))
    cat("+---------------------+----------+------+-------+-----+-----+----+----+\n")
  }
  
  # 📌 Función para calcular Medidas de Tendencia Central y Dispersión
  calcular_estadisticas_motor <- function(tabla) {
    N <- sum(tabla$frecuencias)  
    
    media_aritmetica <- sum(tabla$marcas_clase * tabla$frecuencias) / N
    media_armonica <- N / sum(tabla$frecuencias / tabla$marcas_clase)
    media_geometrica <- exp(sum(tabla$frecuencias * log(tabla$marcas_clase)) / N)
    
    N_2 <- N / 2
    clase_mediana_idx <- which(tabla$Fi >= N_2)[1]
    L_mediana <- tabla$intervalos[clase_mediana_idx]
    F_anterior <- ifelse(clase_mediana_idx == 1, 0, tabla$Fi[clase_mediana_idx - 1])
    f_mediana <- tabla$frecuencias[clase_mediana_idx]
    h <- mean(diff(tabla$marcas_clase))
    mediana <- L_mediana + (((N_2 - F_anterior) / f_mediana) * h)
    
    clase_moda_idx <- which.max(tabla$frecuencias)
    L_moda <- tabla$intervalos[clase_moda_idx]
    f_moda <- tabla$frecuencias[clase_moda_idx]
    f_anterior <- ifelse(clase_moda_idx == 1, 0, tabla$frecuencias[clase_moda_idx - 1])
    f_siguiente <- ifelse(clase_moda_idx == length(tabla$frecuencias), 0, tabla$frecuencias[clase_moda_idx + 1])
    d1 <- f_moda - f_anterior
    d2 <- f_moda - f_siguiente
    moda <- L_moda + ((d1 / (d1 + d2)) * h)
    
    varianza <- sum(tabla$frecuencias * (tabla$marcas_clase - media_aritmetica)^2) / (N - 1)
    desviacion_estandar <- sqrt(varianza)
    coef_variacion <- (desviacion_estandar / media_aritmetica) * 100
    
    cat("\n📊 Medidas de Tendencia Central y Dispersión para Tamaño del Motor\n")
    cat("-------------------------------------------------------------\n")
    cat("Media Aritmética: ", round(media_aritmetica, 2), "\n")
    cat("Media Armónica: ", round(media_armonica, 2), "\n")
    cat("Media Geométrica: ", round(media_geometrica, 2), "\n")
    cat("Mediana: ", round(mediana, 2), "\n")
    cat("Moda: ", round(moda, 2), "\n")
    cat("Varianza: ", round(varianza, 2), "\n")
    cat("Desviación Estándar: ", round(desviacion_estandar, 2), "\n")
    cat("Coeficiente de Variación (%): ", round(coef_variacion, 2), "%\n")
  }
  
  
  graficar_motor_barras <- function(tabla) {
    intervalos_str <- apply(cbind(tabla$intervalos[-length(tabla$intervalos)], tabla$intervalos[-1]), 1, 
                            function(x) sprintf("[%5.2f – %5.2f)", x[1], x[2]))
    
    df <- data.frame(intervalos = factor(intervalos_str, levels = intervalos_str), frecuencias = tabla$frecuencias)
    
    ggplot(df, aes(x = intervalos, y = frecuencias, fill = intervalos)) +
      geom_bar(stat = "identity", color = "black", fill = "skyblue") +
      geom_text(aes(label = frecuencias), vjust = -0.5, size = 4) +
      labs(title = "Distribución del Tamaño del Motor", x = "Intervalos de Tamaño del Motor", y = "Frecuencia") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  main <- function() {
    tabla_motor <- calcular_tabla_frecuencias_motor(tamanio_motor)
    mostrar_tabla_frecuencias_motor(tabla_motor)
    calcular_estadisticas_motor(tabla_motor)
    graficar_motor_barras(tabla_motor)  
  }
  
  # 📌 Ejecutar la función principal
  main()
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #ANALISIS DE RELACION
  
  
  
  
  #################################################################################################
  ##ANALISIS DE RELACION ENTRE EL PRECIO Y KILOMETRAJE
  # 📌 Cargar librerías necesarias
  library(readxl)
  library(ggplot2)
  library(scales)  # Librería para formatear números
  
  # 📌 Leer los datos
  Datos <- read_excel("E:/UNAS_MAKEIN/R/Precios_Automoviles.xlsx")
  
  # 📌 Extraer las variables necesarias
  precio <- Datos$Precio
  kilometraje <- Datos$Kilometraje
  
  # 📌 Calcular la correlación entre Precio y Kilometraje
  correlacion <- cor(precio, kilometraje, method = "pearson")
  
  # 📌 Mostrar el coeficiente de correlación
  cat("\n📊 Correlación entre Precio y Kilometraje: ", round(correlacion, 4), "\n")
  
  # 📌 Crear gráfico de dispersión con línea de tendencia y kilometraje en formato correcto
  ggplot(Datos, aes(x = Kilometraje, y = Precio)) +
    geom_point(color = "blue", alpha = 0.5) +  # Puntos azules con transparencia
    geom_smooth(method = "lm", color = "red", se = FALSE) +  # Línea de tendencia en rojo
    scale_x_continuous(labels = comma) +  # Formatear los valores del eje X en kilómetros
    labs(title = "Relación entre Precio y Kilometraje",
         x = "Kilometraje (km)",
         y = "Precio ($)") +
    theme_minimal()
  
  
  
  
  #################################################################################################
  ##ANALISIS DE RELACION ENTRE EL PRECIO Y AÑO
  
  
  # 📌 Cargar librerías necesarias
  library(readxl)
  library(ggplot2)
  library(scales)  # Para formatear los valores del eje Y
  
  # 📌 Leer los datos
  Datos <- read_excel("E:/UNAS_MAKEIN/R/Precios_Automoviles.xlsx")
  
  # 📌 Extraer las variables necesarias
  precio <- Datos$Precio
  anio_fabricacion <- Datos$Anio
  
  # 📌 Calcular la correlación entre Precio y Año de Fabricación
  correlacion_anio_precio <- cor(precio, anio_fabricacion, method = "pearson")
  
  # 📌 Mostrar el coeficiente de correlación
  cat("\n📊 Correlación entre Precio y Año de Fabricación: ", round(correlacion_anio_precio, 4), "\n")
  
  # 📌 Crear gráfico de dispersión con línea de tendencia
  ggplot(Datos, aes(x = Anio, y = Precio)) +
    geom_point(color = "blue", alpha = 0.5) +  # Puntos azules con transparencia
    geom_smooth(method = "lm", color = "red", se = FALSE) +  # Línea de tendencia en rojo
    scale_y_continuous(labels = comma) +  # Formatear valores de Precio
    labs(title = "Relación entre Precio y Año de Fabricación",
         x = "Año de Fabricación",
         y = "Precio ($)") +
    theme_minimal()
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #######################################################################
  #PRECIO
  
  # 📌 Cargar librerías necesarias
  library(readxl)

  # 📌 Extraer la variable Precio
  precios <- Datos$Precio
  
  # 📌 Calcular Cuartiles
  cuartiles <- quantile(precios, probs = c(0.25, 0.50, 0.75))
  
  # 📌 Calcular Deciles
  deciles <- quantile(precios, probs = seq(0.1, 0.9, by = 0.1))
  
  # 📌 Mostrar los resultados
  cat("\n📊 Cuartiles de Precio:\n")
  cat("Q1 (25%):", round(cuartiles[1], 2), "\n")
  cat("Q2 (Mediana - 50%):", round(cuartiles[2], 2), "\n")
  cat("Q3 (75%):", round(cuartiles[3], 2), "\n")
  
  cat("\n📊 Deciles de Precio:\n")
  for (i in 1:9) {
    cat("D", i, "(", i*10, "%):", round(deciles[i], 2), "\n")
  }

  

  
  
  
  
  
  
  
  
  
  
  
  #################################################################################################
  #KILOMETRAJE
  # 📌 Cargar librerías necesarias
  library(readxl)
  library(ggplot2)
  library(scales)  # Para formatear valores
  
  # 📌 Leer los datos

  # 📌 Extraer la variable de interés (Kilometraje)
  kilometraje <- Datos$Kilometraje
  
  # 📌 Calcular Cuartiles
  cuartiles_km <- quantile(kilometraje, probs = c(0.25, 0.5, 0.75))
  
  # 📌 Calcular Deciles
  deciles_km <- quantile(kilometraje, probs = seq(0.1, 0.9, by = 0.1))
  
  # 📌 Mostrar Cuartiles
  cat("\n📊 Cuartiles del Kilometraje de los Automóviles:\n")
  cat("Q1 (25% de los autos tienen menos de): ", round(cuartiles_km[1], 2), "km\n")
  cat("Q2 - Mediana (50% de los autos tienen menos de): ", round(cuartiles_km[2], 2), "km\n")
  cat("Q3 (75% de los autos tienen menos de): ", round(cuartiles_km[3], 2), "km\n")
  
  # 📌 Mostrar Deciles
  cat("\n📊 Deciles del Kilometraje de los Automóviles:\n")
  for (i in 1:length(deciles_km)) {
    cat(paste0("D", i, " (", i * 10, "% de los autos tienen menos de): ", round(deciles_km[i], 2), " km"), "\n")
  }
  
  # 📌 Crear un Boxplot Mejorado para Kilometraje
  ggplot(Datos, aes(y = Kilometraje)) +
    geom_boxplot(fill = "lightblue", color = "darkblue", outlier.color = "red", outlier.shape = 16) +
    scale_y_continuous(labels = comma) +  # Formatear valores en kilómetros
    labs(title = "Distribución del Kilometraje de Automóviles",
         y = "Kilometraje (km)") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),  # Elimina título del eje X vacío
          axis.text.x = element_blank(),    # Oculta etiquetas del eje X
          axis.ticks.x = element_blank())   # Elimina las marcas del eje X
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##########################################################################
  
  
  
  # 📌 Cargar librerías necesarias
  library(readxl)
  
  # 📌 Leer los datos
  Datos <- read_excel("E:/UNAS_MAKEIN/R/Precios_Automoviles.xlsx")
  
  # 📌 Calcular el total de autos
  total_autos <- nrow(Datos)
  
  ### 📌 1. Probabilidad Clásica (A Priori)
  # 📌 Probabilidad de que un auto tenga Transmisión Manual
  autos_manual <- sum(Datos$Transmision == "Manual")
  prob_manual <- autos_manual / total_autos
  cat("\n📊 P(Transmisión Manual): ", round(prob_manual * 100, 2), "%\n")
  
  # 📌 Probabilidad de que un auto tenga más de 3 propietarios
  autos_mas_3_propietarios <- sum(Datos$Numero_propietarios > 3)
  prob_mas_3_propietarios <- autos_mas_3_propietarios / total_autos
  cat("\n📊 P(Más de 3 Propietarios): ", round(prob_mas_3_propietarios * 100, 2), "%\n")
  
  
  ### 📌 2. Probabilidad Condicional
  # 📌 P(Precio < $10,000 | Kilometraje > 100,000 km)
  autos_mas_100k <- Datos[Datos$Kilometraje > 100000, ]
  prob_precio_menor_10000 <- sum(autos_mas_100k$Precio < 10000) / nrow(autos_mas_100k)
  cat("\n📊 P(Precio < $10,000 | Km > 100,000): ", round(prob_precio_menor_10000 * 100, 2), "%\n")
  
  # 📌 P(Año ≤ 2015 | Kilometraje > 150,000 km)
  autos_mas_150k <- Datos[Datos$Kilometraje > 150000, ]
  prob_anio_2015_menos <- sum(autos_mas_150k$Anio <= 2015) / nrow(autos_mas_150k)
  cat("\n📊 P(Año ≤ 2015 | Km > 150,000): ", round(prob_anio_2015_menos * 100, 2), "%\n")
  
  
  ### 📌 3. Regla de la Adición (Eventos Mutuamente Excluyentes)
  # 📌 P(Km < 100,000 o Año ≥ 2015)
  prob_menos_100k <- sum(Datos$Kilometraje < 100000) / total_autos
  prob_anio_2015_mas <- sum(Datos$Anio >= 2015) / total_autos
  prob_interseccion <- sum(Datos$Kilometraje < 100000 & Datos$Anio >= 2015) / total_autos
  prob_union <- prob_menos_100k + prob_anio_2015_mas - prob_interseccion
  cat("\n📊 P(Km < 100,000 o Año ≥ 2015): ", round(prob_union * 100, 2), "%\n")
  
  # 📌 P(Precio < $12,000 o Propietarios > 2)
  prob_precio_12k <- sum(Datos$Precio < 12000) / total_autos
  prob_propietarios_mas_2 <- sum(Datos$Numero_propietarios > 2) / total_autos
  prob_interseccion <- sum(Datos$Precio < 12000 & Datos$Numero_propietarios > 2) / total_autos
  prob_union <- prob_precio_12k + prob_propietarios_mas_2 - prob_interseccion
  cat("\n📊 P(Precio < $12,000 o Propietarios > 2): ", round(prob_union * 100, 2), "%\n")
  
  
  ### 📌 4. Regla de la Multiplicación (Eventos Independientes)
  # 📌 P(Año ≥ 2020 y Km < 80,000)
  prob_anio_2020_mas <- sum(Datos$Anio >= 2020) / total_autos
  prob_menos_80k <- sum(Datos$Kilometraje < 80000) / total_autos
  prob_multiplicacion <- prob_anio_2020_mas * prob_menos_80k
  cat("\n📊 P(Año ≥ 2020 y Km < 80,000): ", round(prob_multiplicacion * 100, 2), "%\n")
  
  # 📌 P(Transmisión Manual y Precio < $10,000)
  prob_manual <- sum(Datos$Transmision == "Manual") / total_autos
  prob_precio_10k <- sum(Datos$Precio < 10000) / total_autos
  prob_multiplicacion <- prob_manual * prob_precio_10k
  cat("\n📊 P(Transmisión Manual y Precio < $10,000): ", round(prob_multiplicacion * 100, 2), "%\n")
  
  
  ### 📌 5. Teorema de Bayes (Probabilidad Inversa)
  # 📌 P(Año ≤ 2010 | Precio < $10,000)
  prob_anio_2010_menos <- sum(Datos$Anio <= 2010) / total_autos
  prob_precio_menor_10k <- sum(Datos$Precio < 10000) / total_autos
  prob_precio_menor_10k_dado_anio_2010_menos <- sum(Datos$Precio < 10000 & Datos$Anio <= 2010) / sum(Datos$Anio <= 2010)
  prob_anio_2010_menos_dado_precio_menor_10k <- (prob_precio_menor_10k_dado_anio_2010_menos * prob_anio_2010_menos) / prob_precio_menor_10k
  cat("\n📊 P(Año ≤ 2010 | Precio < $10,000): ", round(prob_anio_2010_menos_dado_precio_menor_10k * 100, 2), "%\n")
  
  # 📌 P(Precio < $8,000 | Propietarios > 3)
  prob_precio_menor_8k <- sum(Datos$Precio < 8000) / total_autos
  prob_propietarios_mas_3 <- sum(Datos$Numero_propietarios > 3) / total_autos
  prob_precio_8k_dado_propietarios_3 <- sum(Datos$Precio < 8000 & Datos$Numero_propietarios > 3) / sum(Datos$Numero_propietarios > 3)
  prob_precio_menor_8k_dado_propietarios_3 <- (prob_precio_8k_dado_propietarios_3 * prob_propietarios_mas_3) / prob_precio_menor_8k
  cat("\n📊 P(Precio < $8,000 | Propietarios > 3): ", round(prob_precio_menor_8k_dado_propietarios_3 * 100, 2), "%\n")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # 📌 Revisar conteos y proporciones manualmente
  cat("\nTotal de autos:", total_autos, "\n")
  
  # 📌 Revisión de conteos y proporciones de transmisión manual
  cat("\nAutos con Transmisión Manual:", autos_manual, "->", round(autos_manual / total_autos * 100, 2), "%\n")
  
  # 📌 Revisión de autos con más de 3 propietarios
  cat("\nAutos con más de 3 propietarios:", autos_mas_3_propietarios, "->", round(autos_mas_3_propietarios / total_autos * 100, 2), "%\n")
  
  # 📌 Verificación de precio menor a $10,000 con Km > 100,000
  cat("\nAutos con Km > 100,000:", nrow(autos_mas_100k), "\n")
  cat("Autos con Precio < $10,000 y Km > 100,000:", sum(autos_mas_100k$Precio < 10000), "->", round(prob_precio_menor_10000 * 100, 2), "%\n")
  
  # 📌 Verificación de autos con Año ≤ 2015 y Km > 150,000
  cat("\nAutos con Km > 150,000:", nrow(autos_mas_150k), "\n")
  cat("Autos con Año ≤ 2015 y Km > 150,000:", sum(autos_mas_150k$Anio <= 2015), "->", round(prob_anio_2015_menos * 100, 2), "%\n")
  
  # 📌 Verificación de precio menor a $12,000 o más de 2 propietarios
  cat("\nAutos con Precio < $12,000:", sum(Datos$Precio < 12000), "\n")
  cat("Autos con más de 2 propietarios:", sum(Datos$Numero_propietarios > 2), "\n")
  cat("Intersección (ambas condiciones):", sum(Datos$Precio < 12000 & Datos$Numero_propietarios > 2), "\n")
  cat("P(Precio < $12,000 o Propietarios > 2):", round(prob_union * 100, 2), "%\n")
  
  
  
    
  

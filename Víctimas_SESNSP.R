#Se instalan los paquetes los cuales contienen las librerías de interés.
packages<-c("foreign", "plyr", "dplyr", "readxl","openxlsx", "grid", "readr", "tidyverse")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
lapply(packages, require, character.only=TRUE)

# Libreria utilizada
library(data.table)#Importar y exportar datos
library(foreign) # Lee datos almacenados en un formato externo
library(plyr) #Se usa para dividir, aplicar y combinar datos
library(dplyr)# Para filtrar, transformar y agrupar datos de manera eficiente.
library(grid) #Proporciona herramientas para diseñar y personalizar gráficos.
library(readr) #Lee archivos de texto como CSV
library(tidyverse)# Diseñada para análisis de datos 
library(shiny) #Para la creación de app interactiva
library(ggplot2) #Gráficos bonitos
library(plotly) #Gráficos interactivos
library(DT)#Ayuda a crear gráficos interactivos
#Se define el directorio de trabajo con la ubicación de la base de datos.

# Se define el directorio
getwd()
ruta1 <- "D:/PRUEBA_INMUJERES/Víctimas_SESNSP/"
victimas<-fread(paste0(ruta1,"IDVFC_NM_2024.csv"),encoding="Latin-1")
#Se renombran algunas variables
names(victimas)[4] <- "Bien_jurid_afect"
names(victimas)[5] <- "Tipo_delito"
names(victimas)[6] <- "Subtipo_delito"
names(victimas)[9] <- "Rango_edad"
#Se hace el tratamiento de la base de datos para que sea manejable y fácil de manipular, 
#Se van a convertir las columnas "Enero" a "Diciembre" en una sola columna además, se agregará una columna en donde se mencione su respectivo mes en R.

victimas_1 <- melt(victimas, id.vars = c("Año", "Clave_Ent", "Entidad", "Bien_jurid_afect", "Tipo_delito", "Subtipo_delito", "Modalidad", "Sexo", "Rango_edad"), 
                   variable.name = "Mes", value.name = "Víctimas")
#Nota:Se revisó y validó que el resultado de esta nueva base aplicara el cambio sin 
#alterar o modificar la asignación de los datos. Con esa base de dato se trabajará 
#para realizar la visualización de datos desde la app de Shiny en R.

#Se convierten a factor las siguientes variables: Año, Entidad, Bien_jurid_afect,
#Tipo_delito, Subtipo_delito, Modalidad, Sexo, Rango_edad, Mes.

#Convertir a factor

factores <- c("Año", "Entidad", "Bien_jurid_afect", "Tipo_delito", "Subtipo_delito", "Modalidad", "Sexo", "Rango_edad", "Mes")
victimas_1[, (factores) := lapply(.SD, as.factor), .SDcols = factores]
# Se imprimen los primeros registros de la nueva base
head(victimas_1)
#Se exporta el resultado
#write.xlsx(victimas_1, "D:/PRUEBA_INMUJERES/victimas_2015_2024.xlsx", overwrite = TRUE)

#Se revisa la estructura de los datos

str(victimas_1)  # Estructura de los datos
summary(victimas_1)  # Resumen estadístico
colnames(victimas_1)  # Nombres de columnas
###########Dashboard#################################################### 

# UI
ui <- fluidPage(
  #Encabezado de la app
  tags$head(
    tags$style(HTML("
      /* Estilos CSS */
      /* Estilo para la pestaña 'Inicio' */
      .nav-tabs > li[data-value='inicio'] > a {
        background-color: #51F0FD;
        color: #fff;
        font-weight: bold;
      }

      body {
        background-color: #F2F2F2;
        font-family: Arial, sans-serif;
      }

      .header {
        background-color: #620930;
        color: #fff;
        padding: 10px;
        font-size: 36px;
        text-align: center;
      }

      .content {
        margin: 20px;
      }

      .logo {
        text-align: left;
        margin-bottom: 10px;
      }

      .title {
        text-align: center;
        margin-bottom: 20px;
        font-size: 24px;
        font-weight: bold;
      }

      .data-upload-form {
        max-width: 500px;
        margin: 0 auto;
      }

      .data-upload-form .form-group {
        margin-bottom: 20px;
      }

      .data-upload-form .btn-primary {
        width: 100%;
      }

      .data-table {
        margin-top: 20px;
      }

      /* Animación de las opciones del menú */
      .nav-tabs > li > a {
        transition: font-size 0.5s;
      }

      .nav-tabs > li > a:focus {
        font-size: 110%;
        background-color: #AB8735;
        color: #010101;
      }


      /* Estilo para la imagen de presentación */
      .image-presentation {
        max-width: 90%;
        margin: 0 auto;
      }
      
     /* Estilos para la pestaña 'Resumen de Datos' */
      #resumen_datos {
        background-color: #F6F6F6;
        padding: 20px;
      }
      
      /* Estilos para la pestaña 'Gráficos básicos' */
      #graficos_basicos {
        background-color: #F2F9FF;
        padding: 20px;
      }
      
    "))
  ),
  tags$div(class = "header",
           tags$div(class = "logo",
                    style = "text-align: center;",
                    img(src = "Logo.png", height = "100px")),
           tags$h1("Víctimas en México según el SESNSP")),
  
  sidebarLayout(
    sidebarPanel(
      #Se empieza con el menú desplegable
      selectInput("mes", "Selecciona Mes:", choices = c("Todos", levels(victimas_1$Mes))),
      selectInput("entidad", "Selecciona Entidad:", choices = c("Todas", levels(victimas_1$Entidad))),
      selectInput("bien_jurid_afect", "Selecciona Bien jurídico afectado:", choices = c("Todos", levels(victimas_1$Bien_jurid_afect))),
      selectInput("tipo_delito", "Selecciona Tipo de delito:", choices = c("Todos", levels(victimas_1$Tipo_delito))),
      selectInput("sexo", "Selecciona Sexo:", choices = c("Todos", levels(victimas_1$Sexo))),
      selectInput("rango_edad", "Selecciona Rango de edad:", choices = c("Todos", levels(victimas_1$Rango_edad)))
    ),
    
    mainPanel(
      tabsetPanel(
        #Se agregan los paneles a la app
        #Panel de inicio
        tabPanel("Inicio", value = "inicio",
                 tags$div(class = "content",
                          tags$div(
                            style = "background-color: #EAF2F8; border: 2px solid #135091; border-radius: 10px; padding: 20px;",
                            tags$h3(class = "title", "Bienvenido a la aplicación de Shiny"),
                            tags$p("Aquí encontrarás una variedad de funcionalidades para analizar y visualizar datos."),
                            tags$p("A continuación, se presenta una descripción de las funcionalidades disponibles:"),
                            tags$ul(
                              tags$li("Tabulados dinámicos con base en el filtrado de las variables de interés."),
                              tags$li("Resumen estadístico de los datos."),
                              tags$li("Visualizaciones interactivas: Genera visualizaciones interactivas de los datos."),
                              tags$li("Contacto: Si tienes alguna pregunta o necesitas ayuda, no dudes en contactarme")
                            ),
                            tags$p("¡Te invitamos a probar todas las funcionalidades y explorar tus datos de manera interactiva!"),
                            tags$p(
                              tags$b("NOTA:"), #negrillas
                              "Se sugiere cargar los archivos por año para no saturar la aplicación ya que el rendimiento de la 
                          aplicación mejora y el usuario puede manejar los datos de manera más eficiente.")
                          ))),
        
        #Panel de tabulado interactivo
        tabPanel("Tabla", DTOutput("tabla")),
        
        #Panel de resumen de los datos
        tabPanel("Resumen", verbatimTextOutput("resumen")),
        
        #Panel de gráficas
        tabPanel("Gráficos", plotOutput("grafico1"),
                             plotlyOutput("grafico2"),
                             plotlyOutput("grafico3"),
                             plotlyOutput("grafico4"),
                             plotlyOutput("grafico5")),
        
        #Panel mapa de calor
        tabPanel("Mapa_calor", plotlyOutput("map_calor")),
        
        #Panel de contacto
        tabPanel("Contacto", value = "contacto",
                 fluidPage(
                   div(style = "text-align: center; color: #078698 ",  # Se agrega el estilo CSS "text-align: center;"
                       titlePanel("Contacto")
                   ),
                   sidebarLayout(
                     sidebarPanel(
                       style = "background-color: #48A6A7; padding: 10px; border-radius: 7px; color: #FFFFFF",
                       h4("Información de contacto"),
                       p("Si tienes alguna pregunta o necesitas ayuda, no dudes en contactarme")
                     ),
                     mainPanel(
                       style = "background-color: #AB8735; padding: 10px; border-radius: 7px; color: #FFFFFF",
                       class = "text-center",  # Se agrega la clase CSS "text-center"
                       h4("Correo electrónico"),
                       tags$div(
                         style = "display: inline-block;",  # Agregamos el estilo CSS "display: inline-block;"
                         p(a("martha.aguilar@cimat.mx", href = "mailto:Martha Aguilar Jiménez", style = "color: #FFFFFF; text-decoration: underline;")),
                         h4("Elaboró"),
                         p("Martha Aguilar Jiménez"),
                       )))))))))

# Servidor
server <- function(input, output, sesion){
  
  datos_filtrados <- reactive({
    df <- victimas_1
    if (input$mes != "Todos")df <- df[df$Mes == input$mes,]
    if (input$entidad != "Todas")df <- df[df$Entidad == input$entidad,]
    if (input$bien_jurid_afect != "Todos")df <- df[df$Bien_jurid_afect == input$bien_jurid_afect,]
    if (input$tipo_delito != "Todos")df <- df[df$Tipo_delito == input$tipo_delito,]
    if (input$sexo != "Todos")df <- df[df$Sexo == input$sexo,]
    if (input$rango_edad != "Todos") df <- df[df$Rango_edad == input$rango_edad,]
    df
  })
  
  #Se muestran los datos de manera dinámica
  output$tabla <- renderDT({
  df <- datos_filtrados()
    datatable(datos_filtrados(), options = list(pageLength = 10))
  })
  #Se muestra un resumen de los datos
  output$resumen <- renderPrint({
    summary(datos_filtrados())
  })
  
  #Gráficos
  #Gráfico 1, se realiza el primer gráfico que es un gráfico circular y muestra
  #la distribución de las víctimas según su sexo.
  
  output$grafico1 <- renderPlot({
    df <- datos_filtrados()
    
    #Se agrupan los datos por sexo
    datos_sexo <- datos_filtrados() %>% group_by(Sexo) %>% summarise(victimas_t = sum(Víctimas, na.rm = TRUE))
    ggplot(datos_sexo, aes(x = "", y = victimas_t, fill = Sexo)) +
      geom_col(color = "black") +
      geom_text(aes(label = victimas_t),
                position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c("#2973B2", "#FF6FB5",
                                   "#A59D84"))+
      labs(title = "Víctimas según su sexo") +
      #theme(plot.title = element_text(face = "bold",size = 50)) +
      theme_minimal() +
      theme_void()
  })
  
  #Gráfico 2, es un gráfico de barras que muestra la distribución de las víctimas por entidad federativa.
  #Este gráfico es interactivo, se usó ploty para su realización.
  output$grafico2 <- renderPlotly({
    df <- datos_filtrados()
    if (nrow(df) == 0) {
      return(plotly_empty())  # Si no hay datos, devuelve un gráfico vacío
    }
    #Primero hay que agrupar los datos para el gráfico que se desea realizar.
    datos_ent <- datos_filtrados() %>% group_by(Entidad) %>% summarise(victimas_t = sum(Víctimas, na.rm = TRUE))
    #Se realiza la gráfica
    
    gg2 <- ggplot(datos_ent, aes(x = Entidad, y = victimas_t)) +
      geom_col(position = "dodge") +
      scale_x_discrete(drop = FALSE) +
      geom_col(fill = "#982B1C", color = "white")  +
      labs(title = "Víctimas por Entidad", y = "Número de Víctimas", x = "Entidad") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8)
      ) 
    ggplotly(gg2)  # Hace el gráfico interactivo
  })
  
  #Gráfico 3. Es un gráfico de línea que muestra el comportamiento de las
  #víctimas por mes y sexo, el gráfico tmabién fue hecho con la librería ploty.
  output$grafico3 <- renderPlotly({
    df <- datos_filtrados()
    if (nrow(df) == 0) {
      return(plotly_empty())  # Si no hay datos, devuelve un gráfico vacío
    }
    #Primero hay que agrupar los datos para el gráfico que se desea realizar.
    datos_mes <- datos_filtrados() %>% group_by(Mes,Sexo) %>% summarise(victimas_t = sum(Víctimas, na.rm = TRUE))
    
    #Se realiza la gráfica
    
    gg3 <- ggplot(datos_mes, aes(x = Mes, y = victimas_t, group = Sexo, colour = Sexo)) +
      geom_line(size=1.4) +
      geom_point( size=1.5, shape=21, fill="white") +
      #Caracterización a la gráfica
      labs(title = "Víctimas por mes y sexo",y = "Número de Víctimas", x = "Mes") +
      #Tamaño de título
      theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold"))+
      theme(axis.text.x = element_text(angle=90, size=10))
    ggplotly(gg3)  # Hace el gráfico interactivo
  })

  #Gráfico 4, es un gráfico de barras que muestra la distribución de las víctimas según su rango de edad y sexo.
  #Este gráfico es interactivo.
  output$grafico4 <- renderPlotly({
    df <- datos_filtrados()
    if (nrow(df) == 0) {
      return(plotly_empty())  # Si no hay datos, devuelve un gráfico vacío
    }
    #Primero hay que agrupar los datos para el gráfico que se desea realizar.
    datos_edad <- datos_filtrados() %>% group_by(Rango_edad,Sexo) %>% summarise(victimas_t = sum(Víctimas, na.rm = TRUE))
    #Se realiza la gráfica
    
    gg4 <- ggplot(datos_edad, aes(x = Rango_edad, y = victimas_t, fill = Sexo)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Rango de Edad", y = "Total de Víctimas", title ="Víctimas por grupo de edad y Sexo") +
      theme_minimal() +
      theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold")) +
      theme(axis.text.x = element_text(angle=90, size=10)) +
      scale_fill_manual(values = c("#2973B2", "#FF6FB5",
                                   "#A59D84"))
      ggplotly(gg4) # Hace el gráfico interactivo
  })
  
  #Gráfico 5, es un gráfico de barras que muestra la distribución de las víctimas por tipo de delito.
  #Este gráfico es interactivo, se usó ploty para su realización.
  output$grafico5 <- renderPlotly({
    df <- datos_filtrados()
    if (nrow(df) == 0) {
      return(plotly_empty())  # Si no hay datos, devuelve un gráfico vacío
    }
    #Primero hay que agrupar los datos para el gráfico que se desea realizar.
    datos_del <- datos_filtrados() %>% group_by(Tipo_delito) %>% summarise(victimas_t = sum(Víctimas, na.rm = TRUE))
    #Se realiza la gráfica
    
    gg5 <- ggplot(datos_del, aes(x = Tipo_delito, y = victimas_t)) +
      geom_bar(stat = "identity", fill = "#B5828C", color = "#4B164C") +
      coord_flip() +# Barras horizontales
      labs(title = "Víctimas según el tipo de delito", y = "Número de víctimas", x = "Tipo de delito") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8)
      )    
    ggplotly(gg5)  # Hace el gráfico interactivo
  })
  
  #Mapa de calor, es un gráfico de heat map que muestra el total de víctimas por entidad y mes.
  #Este gráfico es interactivo, se usó ploty para su realización.
  output$map_calor <- renderPlotly({
    df <- datos_filtrados()
    if (nrow(df) == 0) {
      return(plotly_empty())  # Si no hay datos, devuelve un gráfico vacío
    }
    #Primero hay que agrupar los datos para el gráfico que se desea realizar.
    datos_map <- datos_filtrados() %>% group_by(Mes,Entidad) %>% summarise(victimas_t = sum(Víctimas, na.rm = TRUE))
    #Se realiza la gráfica
    
    gg6 <- ggplot(datos_map, aes(Mes, Entidad)) +
      labs(title = "Víctimas por grupo de edad y entidad", size= 0.5)+ 
      theme(plot.title = element_text(size = 12, face = "bold"))+
      labs(caption = "Fuente: SESNSP")+
      theme(plot.caption = element_text(size = 8))+
      xlab("Mes")+ 
      ylab("Entidad") +
      geom_tile(aes(fill = victimas_t)) + #Opacidad
      geom_text(size=2.1,aes(label = victimas_t)) + #leyenda
      scale_fill_gradient(low = "white", high = "#16a596") +
      theme(axis.text.x = element_text(angle=90, size=10))
    plot(gg6)
    ggplotly(gg6)  # Hace el gráfico interactivo
  })
  
}

# Ejecutar aplicación
shinyApp(ui = ui, server = server)

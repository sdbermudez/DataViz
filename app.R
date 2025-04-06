library(tidyverse)
library(Amelia)
library(janitor)
library(magrittr)
library(ggplot2)
library(plotly)
library(dplyr)
library(pastecs)
library(readr)
library(mice)
library(shiny)
library(leaflet)
library(sf)
library(DT)
library(rsconnect)
library(quarto)
library(lubridate)

#Importacion de los datos
#setwd("C:\\Users\\SAMUEL\\OneDrive\\Documentos\\2025\\Quinto semestre\\Visualizacion de datos\\Proyecto")
datos <- read_csv("base_de_datos.csv")

clean_data <- function(datos){
  datos %<>% clean_names
  nombres <- list(
    date_disclosed = "fecha_divulgada",
    project_name = "nombre_proyecto",
    document_type = "tipo_documento",
    project_number = "numero_proyecto",
    project_url = "url_proyecto",
    product_line = "linea_producto",
    company_name = "nombre_empresa",
    country = "pais",
    ifc_country_code = "codigo_pais_ifc",
    industry = "industria",
    environmental_category = "categoria_ambiental",
    department = "departamento",
    status = "estado",
    projected_board_date = "fecha_estimada_junta",
    ifc_approval_date = "fecha_aprobacion_ifc",
    ifc_signed_date = "fecha_firma_ifc",
    ifc_invested_date = "fecha_inversion_ifc",
    ifc_investment_for_risk_management_million_usd = "inversion_ifc_gestion_riesgo_millones_usd",
    ifc_investment_for_guarantee_million_usd = "inversion_ifc_garantia_millones_usd",
    ifc_investment_for_loan_million_usd = "inversion_ifc_prestamo_millones_usd",
    ifc_investment_for_equity_million_usd = "inversion_ifc_capital_millones_usd",
    total_ifc_investment_as_approved_by_board_million_usd = "total_inversion_ifc_aprobada_junta_millones_usd",
    wb_country_code = "codigo_pais_bm",
    as_of_date = "fecha_corte"
  )
  
  names(datos) <- unname(nombres[names(datos)]) #colocar los nombres en español
  
  datos <- datos %>% distinct()
  
  
  datos <- datos %>% select(-c(fecha_corte,fecha_estimada_junta,fecha_aprobacion_ifc,fecha_firma_ifc,fecha_inversion_ifc))
  
  datos <- datos %>%
    mutate(across(contains("millones"), ~replace_na(., 0)))
  
  # Convertir a formato de fecha las columnas relacionadas con fechas
  datos<- datos %>%
    mutate(across(contains("fecha"), as.Date, format = "%m/%d/%Y"))  # Ajustar formato según sea necesario
  
  # Convertir los códigos (Country Code, Project Number, etc.) a factores o caracteres
  datos <- datos %>%
    mutate(across(contains("codigo") | contains("numero"), as.factor))  
  
}

choices<- c("total inversion ifc", "categoria ambiental", "industria", "pais", "estado")
parejas <- c(1,2,3,4)
ui <- navbarPage("Dashboard IFC",
                 tabPanel("MissMap",
                          sidebarLayout(
                            sidebarPanel(
                              actionButton("limpiar", "Limpiar Datos")
                              ),
                            mainPanel(
                              plotOutput("missmap_plot")
                            )
                          )),
                 
                 tabPanel("Analisis univariado",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("Variable","Variable", choices=choices)
                            ),
                            mainPanel(
                              plotlyOutput("grafico")
                            )
                          )
                   
                 ),
                 
                 
                 tabPanel("Analisis Bivariado",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("Variables", "Pareja", choices=parejas),
                              helpText("Guía de grupos:"),
                              
                              HTML("
                                    <ul>
                                      <li><strong>1</strong>: Industria Vs. Inversión total</li>
                                      <li><strong>2</strong>: País Vs. Inversión total</li>
                                      <li><strong>3</strong>: Proyectos divulgados por año</li>
                                      <li><strong>4</strong>: 10 años con mas proyectos</li>
                                    </ul>
                                  ")
                              
                            ), 
                            mainPanel(
                              plotlyOutput("bivariado")
                            )
                            
                          )
                   
                 ),
                 
                 tabPanel("Inversion, categoria ambiental, pais",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("pais_seleccionado", 
                                          "Selecciona un país:", 
                                          choices = NULL)  # Se llenará dinámicamente
                              
                            ),
                            mainPanel(plotlyOutput("grafico_inversion"))
                            )
                          
                  ),

                 tabPanel("Industria, pais, Inversion",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("pais_seleccionado1", "Seleccione un país", choices = NULL),
                              selectInput("industria_seleccionada", "Seleccione una industria", choices = NULL),
                              
                              
                            ),
                            mainPanel(plotlyOutput("grafico_industria_categoria")
                                      # Se llenará dinámicamente)
                            )
                          )
                   
                 ),

                 tabPanel("Datos",
                          DTOutput("tablaDatos")
                 )
                 
)
server <- function(input, output, session) {
  
  datos_nuevos <- reactiveVal(datos)
  
  output$missmap_plot <- renderPlot({
    missmap(datos_nuevos(), main = "Missmap de los datos")
  })
  
  observeEvent(input$limpiar, {
    datos_limpios <- clean_data(datos)  # Aplicas tu función
    datos_nuevos(datos_limpios)
  })
  
  output$grafico <- renderPlotly({
    if (input$Variable == choices[1]){
      print("Fuck mister")
      return(
      ggplotly(
        ggplot(datos_nuevos(), aes(x = total_inversion_ifc_aprobada_junta_millones_usd)) +
          geom_histogram(binwidth = 50, fill = "lightblue",color = "darkblue", alpha = 0.7) +
          labs(title = "Distribución de la Inversión Total Aprobada por la Junta",
               x = "Inversión IFC aprobada (millones USD)",
               y = "Frecuencia") +
          theme_minimal()
      )
      )
    }
    if (input$Variable == choices[2]){
      return(ggplotly(ggplot(datos_nuevos(), aes(x = categoria_ambiental)) +
                        geom_bar(fill = "lightblue") +
                        labs(title = "Distribución de Proyectos por Categoría Ambiental",
                             x = "Categoría Ambiental",
                             y = "Cantidad de Proyectos") +
                        theme_minimal()))
    }
    if (input$Variable == choices[3]){
     return( 
       ggplotly(
         ggplot(datos_nuevos(), aes(x = industria)) +
           geom_bar(fill = "lightblue") +
           labs(title = "Cantidad de proyectos por industria",
                x = "Industria",
                y = "Cantidad de proyectos") + theme_minimal()+
           theme(axis.text.x = element_text(angle = 90, hjust = 1))
       ))
    }
    if (input$Variable == choices[4]){
      top_paises <- datos_nuevos() %>%
        count(pais, sort = TRUE) %>%
        top_n(10, n)
      
      # Filtrar la base de datos para solo incluir estos países
      datos_top_paises <- datos_nuevos() %>%
        filter(pais %in% top_paises$pais)
      
      # Graficar solo los 10 países con más proyectos
      return(
        ggplotly(
          ggplot(datos_top_paises, aes(x = reorder(pais, -table(pais)[pais]))) +
            geom_bar(fill = "lightblue") +
            labs(title = "Top 10 países con más proyectos",
                 x = "País",
                 y = "Cantidad de proyectos") + theme_minimal()+
            theme(axis.text.x = element_text(angle = 90, hjust = 1)
        )
      )
      )
    }
    if (input$Variable == choices[5]){
      return(
        ggplotly(
          ggplot(datos_nuevos(), aes(x = estado)) +
            geom_bar(fill = "lightblue") +
            labs(title = "Distribución del estado de los proyectos",
                 x = "Estado",
                 y = "Cantidad de proyectos") + theme_minimal()+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        )
      )
    }
  })
  
  output$bivariado <- renderPlotly({
    if(input$Variables == 1){
      return(
        ggplotly(
          ggplot(datos_nuevos(), aes(x = industria, y = total_inversion_ifc_aprobada_junta_millones_usd)) +
            geom_boxplot(fill = "lightblue", color = "black", outlier.color = "blue", outlier.shape = 16) +
            labs(title = "Distribución de Inversión IFC por Industria",
                 x = "Industria",
                 y = "Inversión IFC aprobada (millones USD)") + theme_minimal()+
            theme(axis.text.x = element_text(angle = 90, hjust = 1))
        )
      )
    }
    if(input$Variables == 2){
      top_paises <- datos_nuevos() %>%
        group_by(pais) %>%
        summarise(total_inversion = sum(total_inversion_ifc_aprobada_junta_millones_usd, na.rm = TRUE)) %>%
        arrange(desc(total_inversion)) %>%
        top_n(10, total_inversion)
      return(ggplotly(
        ggplot(top_paises, aes(x = reorder(pais, -total_inversion), y = total_inversion)) +
          geom_bar(stat = "identity", fill = "lightblue", alpha = 0.7) +
          labs(title = "Top 10 Países con Mayor Inversión IFC Aprobada",
               x = "Pais",
               y = "Inversión IFC aprobada (millones USD)") + theme_minimal()+
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
      ))
    }
    if(input$Variables == 3){
      # Trabajamos sobre una copia del dataset reactivo
      df <- datos_nuevos()
      
      # Procesamos
      df$fecha_divulgada <- as.Date(df$fecha_divulgada)
      df$anio_divulgacion <- lubridate::year(df$fecha_divulgada)
      
      proyectos_por_anio <- df %>%
        group_by(anio_divulgacion) %>%
        summarise(cantidad_proyectos = n()) %>%
        arrange(anio_divulgacion)
      
      return(ggplotly(
        ggplot(proyectos_por_anio, aes(x = anio_divulgacion, y = cantidad_proyectos)) +
          geom_line(color = "lightblue", size = 1) +
          geom_point(color = "black", size = 2) +
          labs(title = "Evolución de la cantidad de proyectos divulgados por año",
               x = "Año",
               y = "Cantidad de proyectos") +
          theme_minimal() 
      ))
    }
    
    if(input$Variables == 4){
      # Copiamos el dataset reactivo
      df <- datos_nuevos()
      
      # Aseguramos que la fecha esté bien y generamos la columna de año
      df$fecha_divulgada <- as.Date(df$fecha_divulgada)
      df$anio_divulgacion <- lubridate::year(df$fecha_divulgada)
      
      # Calculamos los 10 años con más proyectos
      top_anios <- df %>%
        group_by(anio_divulgacion) %>%
        summarise(cantidad_proyectos = n()) %>%
        arrange(desc(cantidad_proyectos)) %>%
        head(10)
      
      # Graficamos con ggplotly
      return(ggplotly(
        ggplot(top_anios, aes(x = reorder(as.factor(anio_divulgacion), cantidad_proyectos), y = cantidad_proyectos)) +
          geom_bar(stat = "identity", fill = "lightblue", alpha = 0.7) +
          labs(title = "Top 10 Años con Más Proyectos Aprobados",
               x = "Año",
               y = "Cantidad de Proyectos") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          coord_flip()
      ))
    }
  })
  
  
  # Determinar top 10 países
  observe({
    df <- datos_nuevos()
    req(df)
    req("pais" %in% names(df))  # Asegura que existe la columna
    
    top_regiones <- df %>%
      count(pais, sort = TRUE) %>%
      top_n(10) %>%
      pull(pais)
    
    updateSelectInput(session, "pais_seleccionado",
                      choices = top_regiones,
                      selected = top_regiones[1])
  })
  
  output$grafico_inversion <- renderPlotly({
    req(input$pais_seleccionado)
    df <- datos_nuevos()
    # Filtrar y limpiar datos
    datos_filtrados <- df %>%
      filter(pais == input$pais_seleccionado) %>%
      mutate(categoria_ambiental = as.factor(categoria_ambiental))
      
    
    p <- ggplot(datos_filtrados, aes(x = categoria_ambiental,
                                     y = total_inversion_ifc_aprobada_junta_millones_usd,
                                     fill = categoria_ambiental,
                                     text = paste("Inversión:", round(total_inversion_ifc_aprobada_junta_millones_usd, 1), "M USD"))) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
      scale_fill_brewer(palette = "Pastel1") +
      labs(title = paste("Inversión por Categoría Ambiental en", input$pais_seleccionado),
           x = "Categoría Ambiental",
           y = "Inversión Total Aprobada (millones USD)",
           fill = "Categoría Ambiental") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  observe({
    df <- datos_nuevos()
    
    req(nrow(df) > 0)
    req("pais" %in% names(df), "industria" %in% names(df))
    
    # Asegura que no hay NA
    top_paises <- df %>%
      filter(!is.na(pais)) %>%
      count(pais, sort = TRUE) %>%
      top_n(10) %>%
      pull(pais)
    
    if (length(top_paises) > 0) {
      updateSelectInput(session, "pais_seleccionado1",
                        choices = top_paises,
                        selected = top_paises[1])
    }
    
    industrias_disponibles <- df %>%
      filter(!is.na(industria)) %>%
      distinct(industria) %>%
      arrange(industria) %>%
      pull(industria)
    
    if (length(industrias_disponibles) > 0) {
      updateSelectInput(session, "industria_seleccionada",
                        choices = industrias_disponibles,
                        selected = industrias_disponibles[1])
    }
  })
  
  
  output$grafico_industria_categoria <- renderPlotly({
    req(input$pais_seleccionado1, input$industria_seleccionada)
    df <- datos_nuevos()
    
    datos_filtrados <- df %>%
      filter(pais == input$pais_seleccionado1,
             industria == input$industria_seleccionada) %>%
      mutate(categoria_ambiental = as.factor(categoria_ambiental))
    
    p <- ggplot(datos_filtrados, aes(x = categoria_ambiental,
                                     y = total_inversion_ifc_aprobada_junta_millones_usd,
                                     fill = categoria_ambiental,
                                     text = paste0("Categoría: ", categoria_ambiental, "<br>",
                                                   "Inversión: ", round(total_inversion_ifc_aprobada_junta_millones_usd, 1), " M USD"))) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
      scale_fill_brewer(palette = "Pastel1") +
      labs(title = paste("Inversión por Categoría Ambiental en", input$industria_seleccionada, "(", input$pais_seleccionado, ")"),
           x = "Categoría Ambiental",
           y = "Inversión Total Aprobada (millones USD)",
           fill = "Categoría Ambiental") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  output$tablaDatos <- renderDT({
    df<- datos_nuevos()
    datatable(df)
  })
  
  
  }


shinyApp(ui = ui, server = server)

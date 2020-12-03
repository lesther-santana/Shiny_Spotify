library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(spotifyr)
library(magick)
library(shinythemes)
library(ggridges)
library(htmltools)
library(plotly)


Sys.setenv(SPOTIFY_CLIENT_ID = '*')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '*')

access_token <- get_spotify_access_token()




ui <- navbarPage(span(icon("spotify"), "Another App Using Spotify API"),
                 
                 
                 theme = shinytheme("cosmo"),
                 
                 tabPanel(h4("Ver por Artistas"),
                          fluidRow(
                                  column(width = 3,
                                         div(wellPanel(textInput("artista_escrito",
                                                                 label= h3(icon("searchengin"),strong("Buscador")),
                                                                 placeholder="Escribe aquí..."),
                                                       actionButton("boton_buscar","Buscar!"))),
                                          div(uiOutput("resultado"))),
                                   
                                   column(width = 9,
                                                 div(id="contenedor_artistas")
                                          )
                                   )
                          ),
                 
                 tabPanel(h4("Ver por Canciones")),
                 
                 tabPanel(h4("INFORMACION"),
                          uiOutput("infoUI")),
                 
                 useShinydashboard()
                 )


server <- function(input, output, session) {
  
  
  showModal(
    modalDialog(
      title = h1("Bienvenido", style = "text-align: center"),
      p("Esta aplicacion hecha con Shiny te permite ver los atributos que Spotify recopila sobre los artistas disponibles en su plataforma."),
      p("La descripcion de cada atributo se encuentra en el tab \"INFORMACION\"."),
      p("Finalmente, el limite de artistas que puedes comparar son 2. Una vez alcanzado el limite, para poder comparar otros debes refrescar la pagina.",
        "Estamos trabajando para que en un futuro lejano no tengas que hacerlo y para que puedas comparar canciones!"),
      footer = h1(" "),
      easyClose = TRUE)
  ) 
  
  
  
  
  updateTextInput(session, "artista_escrito", value = "")
  
  ########## data ################
  
  data_c <- reactiveVal()
  
  generos <- reactiveVal()
  
  nombre <- reactiveVal()
  
  nombres_monstrando <- c()
  
  id_mostrando <- c()
  
  botones <- c()
  
  lista <- list()
  
  plot_count <- 0

  output$infoUI <- renderUI({
    
    div(em("Fuente:", a(href="https://developer.spotify.com/discover/", "https://developer.spotify.com/discover/")),
        fluidRow(h1("Mood:"),
                 column(width = 3,
                        wellPanel(style = "border-color: #2c3e50",
                                  h2(icon("bolt"),strong("Energy")),
                                  helpText("Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity.",
                                           "Typically, energetic tracks feel fast, loud, and noisy.",
                                           "For example, death metal has high energy, while a Bach prelude scores low on the scale."))),
                 column(width = 3,
                        wellPanel(style = "border-color: #2c3e50",
                                  h2(icon("child"),strong("Danceability")),
                                  helpText("Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity.",
                                           "A value of 0.0 is least danceable and 1.0 is most danceable."))),
                 column(width = 3,
                        wellPanel(style = "border-color: #2c3e50",
                                  h2(icon("smile-beam"),strong("Valence")),
                                  helpText("A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track.",
                                           "Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry)."))),
                 column(width = 3,
                        wellPanel(style = "border-color: #2c3e50",
                                  h2(icon("heartbeat"),strong("Tempo")),
                                  helpText("The overall estimated tempo of a track in beats per minute (BPM).",
                                           "In musical terminology, tempo is the speed or pace of a given piece and derives directly from the average beat duration.")))),
        fluidRow(h1("Properties:"),
                 column(width = 3,
                        wellPanel(style = "border-color: #2c3e50",
                                  h2(icon("microphone-alt"),strong("Speechiness")),
                                  helpText("Speechiness detects the presence of spoken words in a track. The more exclusively speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value.",
                                           "Values above 0.66 describe tracks that are probably made entirely of spoken words.",
                                           "Values between 0.33 and 0.66 describe tracks that may contain both music and speech, either in sections or layered, including such cases as rap music.",
                                           "Values below 0.33 most likely represent music and other non-speech-like tracks.")))
        ))
    
  })
  
  
  
  observeEvent(input$boton_buscar,{
  
    
    
    temp_artista <- isolate(input$artista_escrito)
    
    ########## Condicion para verificar que el buscador no este vacio ########################33
    
    if (!isTruthy(temp_artista)) {
      showModal(modalDialog(
        title = h1("Wey!", style = "text-align: center"),
        tags$img(src = "exclamation-triangle-solid.svg", 
                 width = "100px", 
                 height ="100px",
                 style = "display: block; margin-left: auto; margin-right: auto;  width: 50%; color:red"),
        br(),
        tags$p("Se te olvido escribir.",
               style = "display: block; text-align: center; margin: auto; font-size: 20px"),
        footer = h1(" "),
        easyClose = TRUE)) 
    } 
    
    
    else {
      
      withProgress({
        
        
        #### Data ##############################
        
        temp_data <- get_artist_audio_features(temp_artista, include_groups = c("album", "single"))
        data_c(temp_data)
        
        temp_artista_id <- temp_data %>%
          distinct(artist_id)

        temp_artista_nombre <- temp_data %>%
          distinct(artist_name)
        nombre(temp_artista_nombre)
        
        
        
        ##### para imaagen ######################
        
        artista_lista<-  temp_artista_id %>%
          get_artist()
        
        temp_generos <- sapply(artista_lista["genres"], paste, collapse=", ")
        generos(temp_generos)
        
        lista_imagenes <- artista_lista$images
        
        imagen <- lista_imagenes %>%
          arrange(desc(height)) %>%
          head(1) %>%
          select(url)
        
        image_write(image_read(imagen$url), path = "www/foto.png")
        
        
        
      }, 
      
      message = "Buscando artista en Spotify!", 
      detail = "Esto puede que tarde un poco....")
      
      
      output$resultado <- renderUI({
        
        wellPanel(tags$div(id="resultado_mostrado",
                                    selectizeInput("choices",
                                                   label = span(icon("poll-h"),"Resultado"),
                                                   choices = temp_artista_nombre$artist_name,
                                                   selected = head(temp_artista_nombre$artist_name,1),
                                                   multiple = TRUE,
                                                   width = "50%"),
                                    
                                    div(style = "height: 200px", renderImage({list(src="www/foto.png", height=200, contentType = 'image/png')}, deleteFile = FALSE)),
                                    br(),
                                    actionButton("confirmar", label = "Confirmar", icon = icon("thumbs-up"), class ="btn-sm")))
        
      })  
    }
    
    
  })
  
  
  
  ################# EVENTO Añadir #############
  
  observeEvent(input$confirmar,{
    
    # removeUI(selector = "#resultado_mostrado")
    
    temp_nombre <- nombre()
    
    temp_data <- data_c()

    canciones <- temp_data %>%
      select(track_id) %>%
      count()
    
    temp_generos <-  generos()
    

    
    
    

    if (plot_count == 2){
      
      showModal(
        modalDialog(
          title = h1("Limite de artistas alcandazo!", style = "text-align: center"),
          tags$img(src = "exclamation-triangle-solid.svg",
                   width = "100px",
                   height ="100px",
                   style = "display: block; margin-left: auto; margin-right: auto;  width: 50%; color:red"),
          br(),
          tags$p("Dale a Reolad en tu explorador.",
                 style = "display: block; text-align: center; margin: auto; font-size: 20px"),
          footer = h1(" "),
          easyClose = TRUE)
      )
      
    } else if(temp_nombre %in% nombres_monstrando) {
      
      showModal(
        modalDialog(
          title = h1("Busca otro artista!", style = "text-align: center"),
          tags$img(src = "exclamation-triangle-solid.svg",
                   width = "100px",
                   height ="100px",
                   style = "display: block; margin-left: auto; margin-right: auto;  width: 50%; color:red"),
          br(),
          tags$p("Este ya lo estas viendo.",
                 style = "display: block; text-align: center; margin: auto; font-size: 20px"),
          footer = h1(" "),
          easyClose = TRUE)
      )
      
      
    } else {
      
        
        
        ###### GRAFICOS ########################
        
        ## duracion ###
        
        temp_g_duracion <- temp_data %>%
          mutate(duration_ms = duration_ms /1000)%>%
          ggplot(aes(x = duration_ms))+
          geom_histogram(binwidth = 15, fill="#69b3a2", color="#e9ecef")+
          labs(y="Cantidad de canciones", x="Duración en segundos")+
          theme_ridges(font_size = 10)+
          theme(axis.title.y = element_text(size = 8, face = "bold", hjust = 1),
                axis.title.x = element_text(size = 8, face = "bold"))
        g_duracion <- ggplotly(temp_g_duracion, height=245)
        
        ## energy
        
        temp_g_energy <- temp_data %>%
          ggplot(aes(x = energy))+
          geom_histogram(breaks=seq(0,1,by=0.1), fill="#69b3a2", color="#e9ecef")+
          scale_x_continuous(limits = c(0,1), breaks = seq(0,1,by=0.1))+
          labs(y="Cantidad de canciones")+
          theme_ridges(font_size = 10)+
          theme(axis.title.y = element_text(size = 8, face = "bold"),
                axis.title.x = element_text(size = 8, face = "bold"))
        
        g_energy <- ggplotly(temp_g_energy, height=245)
        
        ## danceability
        
        temp_g_danceability <- temp_data %>%
          ggplot(aes(x = danceability))+
          geom_histogram(breaks = seq(0,1, by=0.1), fill="#69b3a2", color="#e9ecef")+
          scale_x_continuous(limits = c(0,1), breaks = seq(0,1,by=0.1))+
          labs(y="Cantidad de canciones")+
          theme_ridges(font_size = 10)+
          theme(axis.title.y = element_text(size = 8, face = "bold"),
                axis.title.x = element_text(size = 8, face = "bold"))
        g_danceability <- ggplotly(temp_g_danceability, height=245)
        
        ## valence
        
        temp_g_valence <- temp_data %>%
          ggplot(aes(x = valence))+
          geom_histogram(breaks = seq(0,1, by=0.1), fill="#69b3a2", color="#e9ecef")+
          scale_x_continuous(limits = c(0,1), breaks = seq(0,1,by=0.1))+
          labs(y="Cantidad de canciones")+
          theme_ridges(font_size = 10)+
          theme(axis.title.y = element_text(size = 8, face = "bold"),
                axis.title.x = element_text(size = 8, face = "bold"))
        g_valence <- ggplotly(temp_g_valence, height=245)
        
        ## tempo
        
        temp_g_tempo <- temp_data %>%
          ggplot(aes(x = tempo))+
          geom_histogram(binwidth = 10, fill="#69b3a2", color="#e9ecef")+
          labs(y="Cantidad de canciones", x="BPM")+
          theme_ridges(font_size = 10)+
          theme(axis.title.y = element_text(size = 8, face = "bold"),
                axis.title.x = element_text(size = 8, face = "bold"))
        g_tempo <- ggplotly(temp_g_tempo, height=245)

        
        
        ## speechiness
        
        temp_g_speechiness <- temp_data %>%
          ggplot(aes(x = speechiness))+
          geom_histogram(breaks = seq(0,1, by=0.1), fill="#69b3a2", color="#e9ecef")+
          scale_x_continuous(limits = c(0,1), breaks = seq(0,1,by=0.1))+
          labs(y="Cantidad de canciones")+
          theme_ridges(font_size = 10)+
          theme(axis.title.y = element_text(size = 8, face = "bold"),
                axis.title.x = element_text(size = 8, face = "bold"))
        g_speechiness <- ggplotly(temp_g_speechiness, height=245)
      
        
        
      ##################### variables de control ############
      
      plot_count <<- plot_count + 1
      
      id_div <- paste0("div_", plot_count)
      id_mostrando <<- append(id_mostrando, id_div)
      
      
      id_boton <- paste0("btn_", plot_count)
      
      nombres_monstrando <<- append(nombres_monstrando, temp_nombre)
      
      temp_lista <- list(c(id_div, temp_nombre))
      names(temp_lista) <- id_boton
      
      lista <<- append(lista, temp_lista)
      
      
      
      ############################################################
      
      
      insertUI("#contenedor_artistas",
               where = "beforeEnd",
               ui = div(id = "div_1",
                        wellPanel(
                          fluidRow(column(width = 4,
                                          actionBttn(inputId = id_boton,
                                                     label = strong("X"),
                                                     size = "xs",
                                                     style = "material-circle")
                                          )
                                   ),
                          br(),
                          fluidRow(
                            column(width = 12,
                                   column(width = 4,
                                          wellPanel(style = "border-color: #2c3e50",
                                                    fluidRow(
                                                      column(width = 12, 
                                                             div(style = "height: 160px",
                                                                 renderImage({list(src="www/foto.png", height=160, contentType = 'image/png')},
                                                                             deleteFile = FALSE)),
                                                             h2(strong(temp_nombre)))
                                                    ),
                                                    fluidRow(
                                                      column(width = 5,
                                                             div(span(icon("music"), "Canciones", style = "font-size: 18px; color:gray"),
                                                                 p(style = "font-size: 26px", strong(canciones$n)))
                                                      ),
                                                      column(width = 7,
                                                             div(span(icon("sliders-h"), "Generos", style = "font-size: 18px; color:gray"),
                                                                 p(style = "font-size: 16px", strong(temp_generos))),
                                                             )
                                                    ),
                                                    )),
                                   column(width = 8,
                                          tabBox(width = NULL,
                                                 
                                                 title = strong(icon("sliders-h"), "Distribucion de atributos"),
                                                 tabPanel(title = "Energy",
                                                          icon = icon("bolt"),
                                                          g_energy),
                                                 tabPanel(title = "Danceability",
                                                          icon = icon("child"),
                                                          g_danceability),
                                                 tabPanel(title = "Valence",
                                                          icon = icon("smile-beam"),
                                                          g_valence),
                                                 tabPanel(title = "Tempo",
                                                          icon = icon("heartbeat"),
                                                          g_tempo),
                                                 tabPanel(title = "Speechiness",
                                                          icon = icon("microphone-alt"),
                                                          g_speechiness),
                                                 tabPanel(title = "Duracion",
                                                          icon = icon("stopwatch"),
                                                          g_duracion)
                                          ))
                                   )
                            )
                          
                          )
                        )
               )
      

    }
    
    
    
  })
  
  
 
  
  
  
}

shinyApp(ui=ui, server=server)

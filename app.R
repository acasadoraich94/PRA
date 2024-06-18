# setwd("C:/Users/UOC 2/OneDrive/Màster Universitari en Ciència de Dades/2023/2n semestre/Visualització de dades/PRA/Part II")

# La resta del teu codi
library(dplyr)
library(mapview)
library(sf)
library(leaflet)
library(leafpop)
library(shiny)
library(shinydashboard)
library(shinybusy)
library(shinyalert)
library(htmlwidgets)
library(highcharter)
library(tidyr)
library(glue)
library(openxlsx)
library(haven)
library(corrplot)
library(shinyAce)


# Carregar dades Electorals
df_map <- openxlsx::read.xlsx("df_map.xlsx") %>% 
  mutate_if(grepl("_per", names(.)), round, 2)

# Carregar dades CEO
df <- read_sav("Microdades anonimitzades 2021-2023 (select).sav")

df$VAL_GOV_CAT <- as.numeric(df$VAL_GOV_CAT)
df$IDEOL_0_10 <- as.numeric(df$IDEOL_0_10)

labels_problema <- attr(df$PROBLEMA_REDUIDA, "labels")
label_problema <- attr(df$PROBLEMA_REDUIDA, "label")
df$PROBLEMA_REDUIDA <- ifelse(df$PRE_PROBLEMES == 0, df$PROBLEMA_REDUIDA, df$PRE_PROBLEMES)
attr(df$PROBLEMA_REDUIDA, "labels") <- labels_problema
attr(df$PROBLEMA_REDUIDA, "label") <- label_problema

df_factor <- df

df_factor$SEXE <- factor(df_factor$SEXE,
                         levels = sort(unique(as.numeric(df_factor[["SEXE"]]))),
                         labels = names(attr(df_factor$SEXE, "labels")[attr(df_factor$SEXE, "labels") %in% unique(df_factor$SEXE)]))
sexe <- levels(df_factor$SEXE)

df_factor$EDAT_GR <- factor(df_factor$EDAT_GR,
                         levels = sort(unique(as.numeric(df_factor[["EDAT_GR"]]))),
                         labels = names(attr(df_factor$EDAT_GR, "labels")[attr(df_factor$EDAT_GR, "labels") %in% unique(df_factor$EDAT_GR)]))
edat <- levels(df_factor$EDAT_GR)

df_factor$SIT_LAB <- factor(df_factor$SIT_LAB,
                            levels = sort(unique(as.numeric(df_factor[["SIT_LAB"]]))),
                            labels = names(attr(df_factor$SIT_LAB, "labels")[attr(df_factor$SIT_LAB, "labels") %in% unique(df_factor$SIT_LAB)]))
sit_lab <- c("Treballa", "No treballa", "Està temporalment de baixa", "Està en un ERTO")

df_factor$PROBLEMA_REDUIDA <- factor(df_factor$PROBLEMA_REDUIDA,
                            levels = labels_problema,
                            labels = names(labels_problema))

problema <- names(labels_problema)

df_factor <- df_factor %>%
  mutate(ESTUDIS = case_when(
    ESTUDIS_1_15 %in% 1:3  ~ 1,
    ESTUDIS_1_15 %in% 4:9  ~ 2,
    ESTUDIS_1_15 %in% 10:15 ~ 3,
    ESTUDIS_1_15 %in% 80    ~ 80,
    ESTUDIS_1_15 %in% c(98, 99) ~ 99
  ))

df_factor$ESTUDIS <- factor(df_factor$ESTUDIS, labels = c("Baix", "Mitjà", "Alt", "Altres", "NS/NC"))
estudis <- levels(df_factor$ESTUDIS)



# Resultats electorals
resultats_generals <- data.frame(
  Partit = c("PSC_per", "JUNTS_per", "ERC_per", "PP_per", "VOX_per", "Comuns_per", "CUP_per", "AC_per"),
  Percentatge = c(27.96, 21.61, 13.68, 10.97, 7.96, 5.82, 4.09, 3.78)
)

# Carregar shapefile
base_MUNI_sf <- sf::st_read("LIMADM_MUNICIPI.shp", stringsAsFactors = FALSE) %>% dplyr::select(MUNICIPI)
colnames(base_MUNI_sf)[colnames(base_MUNI_sf)=="MUNICIPI"] <- "CMuni"
base_MUNI_sf$CMuni <- as.numeric(base_MUNI_sf$CMuni)
base_MUNI_sf <- dplyr::left_join(base_MUNI_sf, df_map, by = "CMuni", keep = FALSE)

base_MUNI_sf <- base_MUNI_sf %>%
  mutate(
    Part24_T = stringr::str_replace_all(cut(Participació24, breaks = c(0,50,55,60,65,100), right = FALSE, include.lowest = TRUE), ",", "-"),
    Comuns24_T = stringr::str_replace_all(cut(Comuns_per, breaks = c(0,4,8,12,14,100), right = FALSE, include.lowest = TRUE), ",", "-"),
    CUP24_T = stringr::str_replace_all(cut(CUP_per, breaks = c(0,5,10,15,20,100), right = FALSE, include.lowest = TRUE), ",", "-"),
    JUNTS24_T = stringr::str_replace_all(cut(JUNTS_per, breaks = c(0,15,20,25,30,100), right = FALSE, include.lowest = TRUE), ",", "-"),
    ERC24_T = stringr::str_replace_all(cut(ERC_per, breaks = c(0,15,20,25,30,100), right = FALSE, include.lowest = TRUE), ",", "-"),
    PSC24_T = stringr::str_replace_all(cut(PSC_per, breaks = c(0,15,20,25,30,100), right = FALSE, include.lowest = TRUE), ",", "-"),
    PP24_T = stringr::str_replace_all(cut(PP_per, breaks = c(0,5,10,15,20,100), right = FALSE, include.lowest = TRUE), ",", "-"),
    VOX24_T = stringr::str_replace_all(cut(VOX_per, breaks = c(0,5,10,15,20,100), right = FALSE, include.lowest = TRUE), ",", "-"),
    AC24_T = stringr::str_replace_all(cut(AC_per, breaks = c(0,5,10,15,20,100), right = FALSE, include.lowest = TRUE), ",", "-"),
    habi = ifelse(pob <= 2000, "Menys de 2.000", ifelse(pob > 2000 & pob < 10000, "De 2.000 a 10.000", ifelse(pob >= 10000 & pob < 100000, "De 10.000 a 100.000", ifelse(pob >= 100000, "Més de 100.000", NA)))),
    gran_T = ifelse(pob_gran < 15, "Menys del 15%", ifelse(pob_gran >= 15 & pob_gran < 20, "15 a 20%", ifelse(pob_gran >= 20, "Més del 20%", NA))),
    imm_T = ifelse(pob_estr < 15, "Menys del 15%", ifelse(pob_estr >= 15 & pob_estr < 20, "15 a 20%", ifelse(pob_estr >= 20 & pob_estr < 25, "20 a 25%", ifelse(pob_estr >= 25 & pob_estr < 30, "25 a 30%", ifelse(pob_estr >= 30, "Més del 30%", NA)))))
  )

mypalette_G <- c(rgb(209,234,241,names="[0-50)",max=255),
                 rgb(119,191,213,names="[50-55)",max=255),
                 rgb(51,135,161,names="[55-60)",max=255),
                 rgb(38,102,121,names="[60-65)",max=255),
                 rgb(26,68,81,names="[65-100]",max=255))

# UI components
dbHeader <- dashboardHeader(
  title = div(h3('ELECCIONS AL PARLAMENT DE CATALUNYA', style="margin: 0; margin-top:11px; text-align: left"), 
              h5('12 de maig de 2024', style="margin: 0; margin-bottom:0px; text-align: left")),
  titleWidth = "50%",
  tags$li(class = 'dropdown', tags$style(".main-header .logo {height: 60px}"), add_busy_spinner(spin = "fading-circle", position = "bottom-right"))
)

dbSidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Presentació", tabName = "about", icon = icon("user")),
    menuItem("Intenció de Vot", tabName = "evol"),
    menuItem("Resultats Electorals", tabName = "results"),
    menuItem("Codi Font", tabName = "source_code", icon = icon("code"))
  )
)

dbBody <- dashboardBody(
  tabItems(
    tabItem(tabName = "about",
            fluidPage(
              h2("Presentació"),
              p("Hola! Em dic Albert Casado Raich i he construït aquesta aplicació com a solució
              a la Pràctica 2 de l'assignatura de Visualització de Dades del Màster Universitari de Ciència de Dades de la UOC."),
              p("Per accedir-hi entreu a: ", a("https://acasadoraich.shinyapps.io/PRA2/", href = "https://acasadoraich.shinyapps.io/PRA2/", target = "_blank")),
              tags$br(),
              p("La visualització està dividida en: ",
                      tags$ul(
                        tags$li(em("Intenció de Vot"), ": es pot explorar com ha evolucionat la intenció de vot per als diferents partits polítics des del 2021 fins al 2024. També s'hi mostra una matriu de correlació segons les variables analitzades. L'usuari pot filtrar les dades per rang de dates, sexe, grups d'edat, nivell d'estudis i situació laboral."),
                        tags$li(em("Resultats Electorals"), ": es presenten els resultats de les eleccions al Parlament de Catalunya en un format interactiu. Els usuaris poden veure els percentatges de vot per a cada partit a nivell de municipi. També es poden aplicar filtres demogràfics com la dimensió municipal, el percentatge de població major de 65 anys i el percentatge de població estrangera. A més, es pot fer clic en un municipi del mapa per obtenir informació detallada sobre els resultats electorals en aquest municipi.")),
              tags$br(),
              p("A la pestanya", em("Codi Font"), " hi trobareu el codi R que genera l’aplicació."),
              )
            )
    ),
    tabItem(tabName = "evol",
            fluidPage(
              h2("Intenció de Vot", HTML("<font size='5'>", as.character(actionButton(inputId = "info_filtres1", label = NULL, class = "btn-primary btn-lg", icon = icon("info"))), "</font>")),
              fluidRow(
                column(width = 3, 
                       dateRangeInput("date_range", "Selecciona el rang de dates:",
                                      start = min(df_factor %>% mutate(DATA = as.Date(paste(ANY, MES, DIA, sep = "-"))) %>% pull(DATA), na.rm = TRUE), 
                                      end = max(df_factor %>% mutate(DATA = as.Date(paste(ANY, MES, DIA, sep = "-"))) %>% pull(DATA), na.rm = TRUE))
                ),
                column(width = 2,
                       selectInput("sexe", "Selecciona el sexe:", 
                                   choices = c("Tots", sexe), selected = "Tots")
                ),
                column(width = 2,
                       selectInput("edat", "Selecciona l'edat:", 
                                   choices = c("Tots", edat), selected = "Tots")
                ),
                column(width = 2,
                       selectInput("estudis", "Selecciona el nivell d'estudis:", 
                                   choices = c("Tots", estudis), selected = "Tots")
                ),
                column(width = 3,
                       selectInput("situacio_laboral", "Selecciona la situació laboral:", 
                                   choices = c("Tots", sit_lab), selected = "Tots")
                )
              ),
              br(),
              fluidRow(
                column(width = 6, 
                       box(title = "Gràfic", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                           width = 12,
                           height = "550px",
                           highchartOutput("intencio_vot_chart", height = "480px")
                       )
                ),
                column(width = 6,
                       box(title = "Matriu de Correlació", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                           width = 12,
                           height = "550px",
                           plotOutput("correlation_plot", height = "480px"))
                )
              )
            )
    ),
    tabItem(tabName = "results",
            fluidPage(
              h2("Resultats Electorals", HTML("<font size='5'>", as.character(actionButton(inputId = "info_filtres", label = NULL, class = "btn-primary btn-lg", icon = icon("info"))), "</font>")),
              fluidRow(
                column(width = 3,
                       selectInput(inputId = "mapa", label = h4("Variable: "), choices = c("Participació", "Primer partit", "Segon partit"), multiple = FALSE, selected = "Participació"),
                       checkboxGroupInput(inputId = "habi", label = h4("Dimensió municipal: "), choices = list("Menys de 2.000", "De 2.000 a 10.000", "De 10.000 a 100.000", "Més de 100.000"), selected = NULL),
                       checkboxGroupInput(inputId = "gran", label = h4("% Població a partir de 65 anys: "), choices = list("Menys del 15%", "15 a 20%", "Més del 20%"), selected = NULL),
                       checkboxGroupInput(inputId = "imm", label = h4("% Nascuts/des estranger: "), choices = list("Menys del 15%", "15 a 20%", "20 a 25%", "25 a 30%", "Més del 30%"), selected = NULL)
                ),
                column(width = 9,
                       fluidRow(
                         column(width = 6, valueBoxOutput("votsBox", width = 12)),
                         column(width = 6, valueBoxOutput("participacioBox", width = 12))
                       ),
                       fluidRow(
                         column(width = 6, 
                                box(width = 12, title = "Mapa", solidHeader = TRUE, collapsible = FALSE, status = "primary", 
                                    leafletOutput("map", height = "600px"))
                         ),
                         column(width = 6,
                                box(width = 12, title = "Gràfic % de vot", solidHeader = TRUE, collapsible = FALSE, status = "primary",
                             selectInput(inputId = "municipi_selector", label = h4("Cerca el municipi o fes clic al mapa:"), choices = c("Catalunya", sort(unique(base_MUNI_sf$Municipi))), selected = "Catalunya", multiple = FALSE),
                             highchartOutput("stacked_bar_chart", height = "500px"))
                         )
                       )
                )
              )
            )
    ),
    tabItem(tabName = "source_code",
            fluidPage(
              h2("Codi Font de l'Aplicació"),
              uiOutput("code_editor")
            )
    )
  )
)

ui <- dashboardPage(
  title = "PRA2",
  dbHeader,
  dbSidebar,
  dbBody
)

server <- function(input, output, session) {
  RV <- reactiveValues(selected_municipi = NULL)
  
  style_ft <- "text-align: left; float: top; color: black; padding-left:20px"
  
  txt_fitxa_tecnica1 <- tagList(
    tags$div(style = "padding: 20px; font-size: 16px; line-height: 1.6;",
             tags$h4("Informació Tècnica", style = "font-weight: bold;"),
             tags$ul(style = "list-style-type: none; padding: 0;",
                     tags$li(style = "padding-bottom: 10px;",
                             tags$span(icon("info-circle"), style = "color: #007bff;"),
                             tags$span(" Les dades utilitzades en aquesta pestanya provenen de l'Enquesta del Baròmetre d'Opinió Política, administrada pel Centre d'Estudis d'Opinió (CEO) de la Generalitat de Catalunya, cobrint el període de maig 2021 fins a abril 2024."),
                             tags$a(href = "https://ceo.gencat.cat/ca/barometre/", "Enquesta del Baròmetre d'Opinió Política", target = "_blank")
                     ),
                     tags$li(style = "padding-bottom: 10px;",
                             tags$span(icon("info-circle"), style = "color: #007bff;"),
                             tags$span("El conjunt de dades seleccionat conté 14.000 registres i 30 variables, incloent intenció de vot, record de vot, avaluació del govern i variables sociodemogràfiques com edat, sexe, i ocupació.")
                     ),
                     tags$li(style = "padding-bottom: 10px;",
                             tags$span(icon("info-circle"), style = "color: #007bff;"),
                             tags$span(" L'aplicació permet explorar l'evolució de la intenció de vot i analitzar la correlació entre diferents variables sociodemogràfiques.")
                     )
             )
    )
  )
  
  txt_fitxa_tecnica <- tagList(
    tags$div(style = "padding: 20px; font-size: 16px; line-height: 1.6;",
             tags$h4("Informació Tècnica", style = "font-weight: bold;"),
             tags$ul(style = "list-style-type: none; padding: 0;",
                     tags$li(style = "padding-bottom: 10px;",
                             tags$span(icon("info-circle"), style = "color: #007bff;"),
                             tags$span(" Les dades electorals són provisionals i provenen de la Generalitat de Catalunya.")
                     ),
                     tags$li(style = "padding-bottom: 10px;",
                             tags$span(icon("info-circle"), style = "color: #007bff;"),
                             tags$span(" La resta de dades provenen de l’Idescat i corresponen al padró municipal a 1/1/2023.")
                     ),
                     tags$li(style = "padding-bottom: 10px;",
                             tags$span(icon("info-circle"), style = "color: #007bff;"),
                             tags$span(" A l’hora de dissenyar els mapes de suport a cada partit, s’ha tingut en compte el millor resultat de cada formació per decidir les escales de colors.")
                     ),
                     tags$li(style = "padding-bottom: 10px;",
                             tags$span(icon("info-circle"), style = "color: #007bff;"),
                             tags$span(" L'aplicació permet seleccionar entre diferents variables: 'Participació', 'Primer partit' i 'Segon partit'.")
                     ),
                     tags$li(style = "padding-bottom: 10px;",
                             tags$span(icon("info-circle"), style = "color: #007bff;"),
                             tags$span(" Podeu aplicar filtres demogràfics segons la dimensió municipal, el percentatge de població major de 65 anys i el percentatge de població estrangera.")
                     ),
                     tags$li(style = "padding-bottom: 10px;",
                             tags$span(icon("info-circle"), style = "color: #007bff;"),
                             tags$span(" Feu clic sobre qualsevol municipi del mapa per obtenir informació detallada sobre els resultats electorals en aquest municipi.")
                     ),
                     tags$li(style = "padding-bottom: 10px;",
                             tags$span(icon("info-circle"), style = "color: #007bff;"),
                             tags$span(" A la dreta del mapa, es mostra un gràfic de barres amb els percentatges de vot per a cada partit en el municipi seleccionat.")
                     )
             )
    )
  )
  
  observeEvent(input$info_filtres1, {
    shinyalert(
      title = NULL,
      html = TRUE,
      text = paste(txt_fitxa_tecnica1, collapse = ""),
      type = "info",
      confirmButtonText = "Tanca",
      confirmButtonCol = "#AEDEF4",
      closeOnClickOutside = TRUE,
      size = "m"
    )
  })
  
  observeEvent(input$info_filtres, {
    shinyalert(
      title = NULL,
      html = TRUE,
      text = paste(txt_fitxa_tecnica, collapse = ""),
      type = "info",
      confirmButtonText = "Tanca",
      confirmButtonCol = "#AEDEF4",
      closeOnClickOutside = TRUE,
      size = "m"
    )
  })
  
  observeEvent(input$map_click, {
    lat <- input$map_click$lat
    lng <- input$map_click$lng
    
    clicked_point <- st_sfc(st_point(c(lng, lat)), crs = 4326) %>% st_transform(crs = st_crs(base_MUNI_sf))
    nearest_municipi <- st_nearest_feature(clicked_point, base_MUNI_sf)
    RV$selected_municipi <- base_MUNI_sf[nearest_municipi, ]
    updateSelectInput(session, "municipi_selector", selected = RV$selected_municipi$Municipi[1])
  })
  
  observeEvent(input$municipi_selector, {
    if (input$municipi_selector != "Catalunya") {
      selected_municipi <- input$municipi_selector
      RV$selected_municipi <- base_MUNI_sf %>% filter(Municipi == selected_municipi)
    } else {
      RV$selected_municipi <- NULL
    }
  })
  
  observeEvent(input$mapa, {
    if (is.null(RV$selected_municipi)) {
      RV$selected_municipi <- NULL
    } else {
      updateSelectInput(session, "municipi_selector", selected = RV$selected_municipi$Municipi[1])
    }
  })
  
  observeEvent(input$habi, {
    update_municipi_selector()
    RV$selected_municipi <- NULL
  })
  
  observeEvent(input$gran, {
    update_municipi_selector()
    RV$selected_municipi <- NULL
  })
  
  observeEvent(input$imm, {
    update_municipi_selector()
    RV$selected_municipi <- NULL
  })
  
  update_municipi_selector <- function() {
    filtered_data <- base_data()
    updateSelectInput(session, "municipi_selector", choices = c("Catalunya", sort(unique(filtered_data$Municipi))))
  }
  
  base_data <- reactive({
    b <- base_MUNI_sf
    if (!is.null(input$habi) & length(input$habi) > 0) {
      b <- b %>% filter(habi %in% input$habi)
    }
    if (!is.null(input$gran) & length(input$gran) > 0) {
      b <- b %>% filter(gran_T %in% input$gran)
    }
    if (!is.null(input$imm) & length(input$imm) > 0) {
      b <- b %>% filter(imm_T %in% input$imm)
    }
    if (input$municipi_selector != "Catalunya") {
      b <- b %>% filter(Municipi == input$municipi_selector)
    }
    return(b)
  })
  
  output$votsBox <- renderValueBox({
    data <- base_data()
    vots <- ifelse(is.null(RV$selected_municipi), sum(data$Vots, na.rm = TRUE), sum(RV$selected_municipi$Vots, na.rm = TRUE))
    valueBox(
      paste0(format(vots, big.mark = ".", decimal.mark = ",")),
      "Total Vots",
      icon = icon("check-square"),
      color = "blue"
    )
  })
  
  output$participacioBox <- renderValueBox({
    data <- base_data()
    participacio <- ifelse(is.null(RV$selected_municipi), sum(data$Vots)/sum(data$Cens)*100, mean(RV$selected_municipi$Participació24, na.rm = TRUE))
    valueBox(
      paste0(format(round(participacio, 2), decimal.mark = ","), "%"),
      "Participació",
      icon = icon("bar-chart"),
      color = "blue"
    )
  })
  
  output$map <- renderLeaflet({
    validate(
      need(base_data(), "No hi ha dades")
    )
    
    base_MUNI_sf_b <- base_data()
    
    mylabel <- glue::glue(
      "<strong>Comarca:</strong> {base_MUNI_sf_b$Comarca}
       <br><strong>Municipi:</strong> {base_MUNI_sf_b$Municipi}
       <br><strong>Població:</strong> {base_MUNI_sf_b$pob}
       <br><strong>Població +65 anys:</strong> {paste(base_MUNI_sf_b$pob_gran, '%')}
       <br><strong>Població estrangera:</strong> {paste(base_MUNI_sf_b$pob_estr, '%')}
       "
    ) %>%
      lapply(htmltools::HTML)
    
    if (input$mapa == "Participació" | is.null(input$mapa)){
      
      color <- rgb(51, 135, 161, maxColorValue = 255)
      light_blue <- rgb(204, 235, 255, maxColorValue = 255)
      dark_blue <- "#004B5F"
      
      mypalette <- colorRampPalette(c(light_blue, color, dark_blue), space = "rgb")(5)
      names(mypalette_G) <- c("[0-50)", "[50-55)", "[55-60)", "[60-65)", "[65-100]")
      
      mypalette_G <- mypalette_G[names(mypalette_G) %in% base_MUNI_sf_b$Part24_T]
      base_MUNI_sf_b$Part24_T <- factor(base_MUNI_sf_b$Part24_T, levels = names(mypalette_G))
      
      m <- mapview(
        base_MUNI_sf_b,
        zcol = "Part24_T",
        col.regions = mypalette_G,
        layer.name = "Participació",
        alpha.regions = 0.5,
        lwd = 0.3,
        hide = FALSE,
        label = mylabel,
        popup = FALSE,
        legend = TRUE
      )
      
    } else if (input$mapa == "Primer partit"){
      
      colors_partits <- c(
        rgb(212, 45, 37, names = "PSC", maxColorValue = 255),
        rgb(0, 196, 178, names = "JUNTS", maxColorValue = 255),
        rgb(251, 197, 63, names = "ERC", maxColorValue = 255),
        rgb(0, 162, 226, names = "PP", maxColorValue = 255),
        rgb(148, 191, 54, names = "VOX", maxColorValue = 255),
        rgb(101, 70, 146, names = "Comuns", maxColorValue = 255),
        rgb(255, 241, 0, names = "CUP", maxColorValue = 255),
        rgb(15, 76, 129, names = "AC", maxColorValue = 255))
      
      mypalette_P <- colors_partits[names(colors_partits) %in% base_MUNI_sf_b$Guanyador24]
      base_MUNI_sf_b$Guanyador24 <- factor(base_MUNI_sf_b$Guanyador24, levels = names(mypalette_P))
      
      m <- mapview(
        base_MUNI_sf_b,
        zcol = "Guanyador24",
        col.regions = mypalette_P,
        layer.name = "Primer partit",
        alpha.regions = 0.5,
        lwd = 0.3,
        hide = FALSE,
        label = mylabel,
        popup = FALSE,
        legend = TRUE
      )
      
    } else if (input$mapa == "Segon partit"){
      
      colors_partits <- c(
        rgb(212, 45, 37, names = "PSC", maxColorValue = 255),
        rgb(0, 196, 178, names = "JUNTS", maxColorValue = 255),
        rgb(251, 197, 63, names = "ERC", maxColorValue = 255),
        rgb(0, 162, 226, names = "PP", maxColorValue = 255),
        rgb(148, 191, 54, names = "VOX", maxColorValue = 255),
        rgb(101, 70, 146, names = "Comuns", maxColorValue = 255),
        rgb(255, 241, 0, names = "CUP", maxColorValue = 255),
        rgb(15, 76, 129, names = "AC", maxColorValue = 255))
      
      mypalette_P <- colors_partits[names(colors_partits) %in% base_MUNI_sf_b$Segon24]
      base_MUNI_sf_b$Segon24 <- factor(base_MUNI_sf_b$Segon24, levels = names(mypalette_P))
      
      m <- mapview(
        base_MUNI_sf_b,
        zcol = "Segon24",
        col.regions = mypalette_P,
        layer.name = "Segon partit",
        alpha.regions = 0.5,
        lwd = 0.3,
        hide = FALSE,
        label = mylabel,
        popup = FALSE,
        legend = TRUE
      )
    }
    
    m@map %>% addProviderTiles(provider = "CartoDB") %>%
      htmlwidgets::onRender("
    function(el, x) {
      this.on('click', function(e) {
        var layer = this.layerPointToLatLng(e.layerPoint);
        var clickedLayer = this._layers[e.layerPoint];
        Shiny.onInputChange('map_click', {lat: layer.lat, lng: layer.lng, id: clickedLayer.options.layerId});
      });
    }")
  })
  
  output$stacked_bar_chart <- renderHighchart({
    
    if (is.null(RV$selected_municipi) || input$municipi_selector == "Catalunya") {
      data <- resultats_generals
      
      colors <- c(
        "PSC_per" = rgb(212, 45, 37, maxColorValue = 255),
        "JUNTS_per" = rgb(0, 196, 178, maxColorValue = 255),
        "ERC_per" = rgb(251, 197, 63, maxColorValue = 255),
        "PP_per" = rgb(0, 162, 226, maxColorValue = 255),
        "VOX_per" = rgb(148, 191, 54, maxColorValue = 255),
        "Comuns_per" = rgb(101, 70, 146, maxColorValue = 255),
        "CUP_per" = rgb(255, 241, 0, maxColorValue = 255),
        "AC_per" = rgb(15, 76, 129, maxColorValue = 255)
      )
      
      valid_parties <- data$Partit
      valid_labels <- c("PSC", "JUNTS", "ERC", "PP", "VOX", "Comuns", "CUP", "Aliança Catalana")[match(valid_parties, names(colors))]
      valid_colors <- colors[as.character(valid_parties)]
      
      highchart() %>%
        hc_chart(type = "column") %>%
        hc_title(text = "Resultats a Catalunya") %>%
        hc_xAxis(categories = valid_labels, title = list(text = "Partit")) %>%
        hc_yAxis(title = list(text = "% de vot")) %>%
        hc_plotOptions(column = list(
          colorByPoint = TRUE,
          dataLabels = list(
            enabled = TRUE,
            format = "{point.y:.2f}%"
          )
        )) %>%
        hc_add_series(name = "", data = data$Percentatge, colorByPoint = TRUE, colors = unname(valid_colors)) %>%
        hc_legend(enabled = FALSE)
    } else {
      
      data <- RV$selected_municipi %>%
        as.data.frame() %>%
        select(Municipi, Comuns_per, CUP_per, JUNTS_per, ERC_per, PSC_per, PP_per, VOX_per, AC_per) %>%
        gather(key = "party", value = "percentage", -Municipi) %>%
        mutate(party = factor(party, levels = c("PSC_per", "JUNTS_per", "ERC_per", "PP_per", "VOX_per", "Comuns_per", "CUP_per", "AC_per"))) %>%
        filter(percentage > 0) %>%
        arrange(desc(percentage))
      
      colors <- c(
        "PSC_per" = rgb(212, 45, 37, maxColorValue = 255),
        "JUNTS_per" = rgb(0, 196, 178, maxColorValue = 255),
        "ERC_per" = rgb(251, 197, 63, maxColorValue = 255),
        "PP_per" = rgb(0, 162, 226, maxColorValue = 255),
        "VOX_per" = rgb(148, 191, 54, maxColorValue = 255),
        "Comuns_per" = rgb(101, 70, 146, maxColorValue = 255),
        "CUP_per" = rgb(255, 241, 0, maxColorValue = 255),
        "AC_per" = rgb(15, 76, 129, maxColorValue = 255)
      )
      
      valid_parties <- data$party
      valid_labels <- c("PSC", "JUNTS", "ERC", "PP", "VOX", "Comuns", "CUP", "Aliança Catalana")[match(valid_parties, names(colors))]
      valid_colors <- colors[as.character(valid_parties)]
      
      highchart() %>%
        hc_chart(type = "column") %>%
        hc_title(text = data$Municipi[1]) %>%
        hc_xAxis(categories = valid_labels, title = list(text = "Partit")) %>%
        hc_yAxis(title = list(text = "% de vot")) %>%
        hc_plotOptions(column = list(
          colorByPoint = TRUE,
          dataLabels = list(
            enabled = TRUE,
            format = "{point.y:.1f}%"
          )
        )) %>%
        hc_add_series(name = "", data = data$percentage, colorByPoint = TRUE, colors = unname(valid_colors)) %>%
        hc_legend(enabled = FALSE)
    }
  })
  
  output$intencio_vot_chart <- renderHighchart({
    df_factor <- df_factor %>%
      mutate(DATA = as.Date(paste(ANY, MES, DIA, sep = "-")))
    
    filtered_data <- df_factor %>%
      filter(INT_PARLAMENT_VOT %in% 1:24) %>%
      filter(DATA >= input$date_range[1] & DATA <= input$date_range[2])
    
    if (input$sexe != "Tots") {
      filtered_data <- filtered_data %>% filter(SEXE == input$sexe)
    }
    
    if (input$edat != "Tots") {
      filtered_data <- filtered_data %>% filter(EDAT_GR == input$edat)
    }
    
    if (input$estudis != "Tots") {
      filtered_data <- filtered_data %>% filter(ESTUDIS == input$estudis)
    }
    
    if (input$situacio_laboral != "Tots") {
      filtered_data <- filtered_data %>% filter(SIT_LAB == input$situacio_laboral)
    }
    
    filtered_data <- filtered_data %>%
      group_by(DATA, INT_PARLAMENT_VOT) %>%
      summarize(Intencio_Vot = n(), .groups = 'drop') %>%
      ungroup() %>%
      group_by(DATA) %>%
      mutate(Intencio_Vot = round(Intencio_Vot / sum(Intencio_Vot) * 100, 2)) %>%
      ungroup()
    
    filtered_data$INT_PARLAMENT_VOT <- factor(filtered_data$INT_PARLAMENT_VOT,
                                              levels = sort(unique(as.numeric(filtered_data[["INT_PARLAMENT_VOT"]]))),
                                              labels = names(attr(filtered_data$INT_PARLAMENT_VOT, "labels")[attr(filtered_data$INT_PARLAMENT_VOT, "labels") %in% unique(filtered_data$INT_PARLAMENT_VOT)]))
    
    filtered_data <- filtered_data %>%
      arrange(DATA)
    
    summarize_votes <- function(parties) {
      filtered_data %>%
        filter(INT_PARLAMENT_VOT %in% parties) %>%
        group_by(DATA) %>%
        summarize(Intencio_Vot = sum(Intencio_Vot), .groups = 'drop') %>%
        list_parse2()
    }
    
    highchart() %>%
      hc_chart(type = "line") %>%
      hc_title(text = "Evolució de la Intenció de Vot") %>%
      hc_xAxis(categories = sort(unique(filtered_data$DATA)), title = list(text = "Data de l'enquesta")) %>%
      hc_yAxis(title = list(text = "Intenció de Vot")) %>%
      hc_add_series(name = "PSC", data = summarize_votes("PSC"), color = "#D42D25") %>%
      hc_add_series(name = "JUNTS", data = summarize_votes(c("Junts per Catalunya", "PDeCAT")), color = "#00C4B2") %>%
      hc_add_series(name = "ERC", data = summarize_votes("ERC"), color = "#FBC53F") %>%
      hc_add_series(name = "PP", data = summarize_votes("PP"), color = "#00A2E2") %>%
      hc_add_series(name = "VOX", data = summarize_votes("Vox"), color = "#94BF36") %>%
      hc_add_series(name = "Comuns", data = summarize_votes(c("ICV-EUiA", "Podemos", "En Comú Podem", "Catalunya en Comú Podem")), color = "#654692") %>%
      hc_add_series(name = "CUP", data = summarize_votes("CUP"), color = "#FFF100") %>%
      hc_add_series(name = "Cs", data = summarize_votes("Ciutadans/Ciudadanos"), color = "#FF5824")
  })
  
  
  output$correlation_plot <- renderPlot({
    df_filtered <- df_factor %>%
      mutate(DATA = as.Date(paste(ANY, MES, DIA, sep = "-"))) %>%
      filter(DATA >= input$date_range[1] & DATA <= input$date_range[2])
    
    df_numeric <- df_filtered %>%
      mutate(across(everything(), as.numeric)) %>%
      select_if(is.numeric)
    
    variables <- df_numeric %>%
      select(INT_PARLAMENT_VOT, SEXE, EDAT_GR, ESTUDIS, SIT_LAB, REC_PARLAMENT_VOT, VAL_GOV_CAT, IDEOL_0_10)
    
    corr_matrix <- cor(variables, use = "complete.obs")
    
    colnames(corr_matrix) <- c("Intenció de Vot", "Sexe", "Edat", "Nivell d'Estudis", "Situació Laboral", "Record de Vot",
                               "Valoració del Govern", "Ideologia")
    rownames(corr_matrix) <- colnames(corr_matrix)
    
    corrplot::corrplot(corr_matrix, method = "circle", type = "upper", tl.cex = 0.8, number.cex = 0.8, addCoef.col = "black")
  })
  
  
  output$code_editor <- renderUI({
    aceEditor(
      outputId = "code_ace",
      value = readLines("app.R"),
      mode = "r",
      theme = "tomorrow",
      readOnly = TRUE
    )
    
  })
  
}

shinyApp(ui, server)
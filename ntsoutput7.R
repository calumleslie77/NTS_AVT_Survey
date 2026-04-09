# libraries ----

library(sf)
library(shiny)
library(tidyverse)
library(leaflet)
library(dplyr)
library(leafpop)
library(leaflegend)
library(rlang)
library(paletteer)
library(rsconnect)

# set up ----

#setwd("~/GIS-projects/nts_output")

# AVT

avt <- st_read("data/output/avt_record_clean.gpkg")

# assign fresh fids
avt$id <- row_number(avt$Unique_tree_id)

# Need to jitter the co-ords (don't ask)
avt$lat <- jitter(avt$y)
avt$lng <- jitter(avt$x)

# Tree group and NTS

# Read in tree groups
tg <- st_transform(st_read("data/output/tree_group_clean.gpkg"), 4326)
# Read in NTS boundaries
nts <- st_transform(st_read("data/nts/nts.shp"), 4326)
# Cut to the survey sites
kp <- unique(avt$Site_name)
nts <- nts %>% filter(PROPERTY %in% kp)

# ui ----- 

ui <- navbarPage("NTS AVT Survey", 
                 id="nav",
                 tabPanel("Map",
                          div(class="outer",
                              tags$head(
                                # Include custom CSS
                                includeCSS("styles.css"),
                                includeScript("gomap.js")
                              ),
                              # If not using custom CSS, set height of leafletOutput to a number instead of percent
                              leafletOutput("map", width="100%", height="100%"),
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
                                            draggable = TRUE, top = "15%", left = "1%", right = "auto", bottom = "auto",
                                            width = 330, height = "auto",
                                            h3("Trees in view"),
                                            DT::dataTableOutput("sptab"),
                                            hr(),
                                            plotOutput("thview", height = 250)
                              ),
                              #tags$div(id="cite",
                              #         'Data compiled for NTS'
                              #)
                          )
                 ),
                 tabPanel("Data table", 
                          fluidPage(
                            fluidRow(
                              column(3,
                              selectInput("dst", "Site",
                                          choices = c('All' = "*", 
                                                      'Balmacara' = "Balmacara",
                                                      'Ben Lomond' = "Ben Lomond",
                                                      'Castle Fraser' = "Castle Fraser",
                                                      'House of the Binns' = "House of the Binns",
                                                      'House of Dun' = "House of Dun",
                                                      'Fyvie Castle' = "Fyvie Castle",
                                                      'Leith Hall' = "Leith Hall"))
                              ),
                              column(3,
                                     conditionalPanel("input.davt", 
                                                      selectInput("davt", "AVT",
                                                                  choices = c('All' = "*", 
                                                                              'Ancient' = "Ancient",
                                                                              'Veteran' = "Veteran",
                                                                              'Notable' = "Notable"))
                                     )
                              ),
                              column(3,
                                     conditionalPanel("input.dst", 
                                        selectInput("dth", "Threat status",
                                                 choices = c('All' = "*", 
                                                             'Secure' = "Secure",
                                                             'Threatened' = "Threatened",
                                                             'Critical' = "Critical")))
                              ),
                            column(3,
                                   conditionalPanel("input.dst",
                            selectInput("dep", "Epiphytes", 
                                        c('All' = "*", 
                                          'Flying tree' = "'Flying' tree",
                                          'Fern' = "Fern",
                                          'Bryophyte' = "Bryophyte",
                                          'Lichen' = "Lichen",
                                          'Honeysuckle' = "Honeysuckle",
                                          'Ivy' = "Ivy",
                                          'Other' = "Other")))
                            )
                                   ),
                            fluidRow(
                              column(3,
                                     selectInput("dsp", "Species",
                                                 choices = c('All' = "*",
                                                 unique(avt$Species)))
                              ),
                              column(3,
                                     conditionalPanel("input.dsp", 
                                                      selectInput("dev", "Evidence of",
                                                                  choices = c('All' = "*",
                                                                              'Bats (potential habitat)' = "potential",
                                                                              'Bats (presence/activity)' = "presence",
                                                                              'Fungi' = "Fungi",
                                                                              'Invertebrate activity' = "Invertebrate activity"))
                                     )
                              ),
                              column(3,
                                     conditionalPanel("input.dsp", 
                                                      selectInput("dsty", "Site type",
                                                                  choices = c('All' = "*", 
                                                                              'Parkland' = "Parkland",
                                                                              'Woodland' = "Woodland",
                                                                              'Designed landscape' = "Designed landscape",
                                                                              'Wood pasture' = "Wood pasture",
                                                                              'Formal garden' = "Formal garden",
                                                                              'Crags' = "Crags",
                                                                              'Arable' = "Arable",
                                                                              'Other' = "Other")))
                              ),
                              column(3,
                                     conditionalPanel("input.dst",
                                                      selectInput("ddw", "Decaying wood", 
                                                                  c('All' = "*", 
                                                                    '<10% decaying wood' = "<10% decaying wood",
                                                                    '<50% decaying wood' = "<10% decaying wood",
                                                                    '>50% decaying wood' = ">50% decaying wood",
                                                                    '>90% decaying wood' = ">90% decaying wood",
                                                                    'In the crown' = "In the crown",
                                                                    'On the ground' = "On the ground",
                                                                    'Cracks in bark' = "Cracks in bark",
                                                                    'Hollowing trunk' = "Hollowing trunk",
                                                                    'Visible hollowing' = "Visible hollowing",
                                                                    'Hollowing trunk with opening' = "Hollowing trunk with opening",
                                                                    'Hollowing trunk with holes >15cm' = "Hollowing trunk with holes >15cm",
                                                                    'Other holes or water pockets' = "Other holes or water pockets")))
                              )
                            ),
                            fluidRow(
                              hr(),
                              downloadLink('downloadData', 'Download full dataset as CSV'),
                              hr(),
                              ),
                          DT::dataTableOutput("avttable")
                          )
                 ),
                 tabPanel("Site type",
                          fluidPage(    
                            titlePanel("Species and AVT status by site type"),
                            sidebarLayout(      
                              sidebarPanel(
                                selectInput("stysite", "Site name:", 
                                            choices = c('All' = "*", 
                                                        'Balmacara' = "Balmacara",
                                                        'Ben Lomond' = "Ben Lomond",
                                                        'Castle Fraser' = "Castle Fraser",
                                                        'House of the Binns' = "House of the Binns",
                                                        'House of Dun' = "House of Dun",
                                                        'Fyvie Castle' = "Fyvie Castle",
                                                        'Leith Hall' = "Leith Hall")),
                                selectInput("Site_type", "Site type:", 
                                            choices = c('All' = "*", 
                                                        'Parkland' = "Parkland",
                                                        'Woodland' = "Woodland",
                                                        'Designed landscape' = "Designed_landscape",
                                                        'Wood pasture' = "Wood_pasture",
                                                        'Formal garden' = "Formal_garden",
                                                        'Crags' = "Crags",
                                                        'Arable field' = "Arable field",
                                                        'Other' = "Other")),
                                hr(),
                                htmlOutput("stypeText")
                              ),
                              mainPanel(
                                plotOutput("stypePlot", height = 800)  
                              )
                              
                            )
                          )),
                 tabPanel("Evidence of",
                          fluidPage(    
                            titlePanel("Evidence of"),
                            sidebarLayout(      
                              sidebarPanel(
                                selectInput("evsite", "Site name:", 
                                            choices = c('All' = "*", 
                                                        'Balmacara' = "Balmacara",
                                                        'Ben Lomond' = "Ben Lomond",
                                                        'Castle Fraser' = "Castle Fraser",
                                                        'House of the Binns' = "House of the Binns",
                                                        'House of Dun' = "House of Dun",
                                                        'Fyvie Castle' = "Fyvie Castle",
                                                        'Leith Hall' = "Leith Hall")),
                                selectInput("evid", "Evidence of:", 
                                            choices = c('All' = "*",
                                                        'Bats (potential habitat)' = "potential",
                                                        'Bats (presence/activity)' = "presence",
                                                        'Fungi' = "Fungi",
                                                        'Invertebrate activity' = "Invertebrate activity")),
                                hr(),
                                htmlOutput("evText")
                              ),
                              mainPanel(
                                plotOutput("batPlot", height = 800)  
                              )
                              
                            )
                          )),
                 tabPanel("Threat",
                          fluidPage(    
                            titlePanel("Overall threat status"),
                            sidebarLayout(      
                              sidebarPanel(
                                selectInput("Sitename4", "Site name:", 
                                            choices = c('All' = "*", 
                                                        'Balmacara' = "Balmacara",
                                                        'Ben Lomond' = "Ben Lomond",
                                                        'Castle Fraser' = "Castle Fraser",
                                                        'House of the Binns' = "House of the Binns",
                                                        'House of Dun' = "House of Dun",
                                                        'Fyvie Castle' = "Fyvie Castle",
                                                        'Leith Hall' = "Leith Hall")),
                                hr(),
                                selectInput("AVT3", "AVT status:", 
                                            c('All' = "*", 
                                              'Ancient' = "Ancient",
                                              'Veteran' = "Veteran",
                                              'Notable' = "Notable")),
                                hr(),
                                htmlOutput("thText")
                              ),
                              mainPanel(
                                plotOutput("threatPlot", height = 800)  
                              )
                              
                            )
                          )),
                 tabPanel("Root threat",
                          fluidPage(    
                            titlePanel("Threats to roots"),
                            sidebarLayout(      
                              sidebarPanel(
                                selectInput("rtsite", "Site:", 
                                            choices = c('All' = "*", 
                                                        'Balmacara' = "Balmacara",
                                                        'Ben Lomond' = "Ben Lomond",
                                                        'Castle Fraser' = "Castle Fraser",
                                                        'House of the Binns' = "House of the Binns",
                                                        'House of Dun' = "House of Dun",
                                                        'Fyvie Castle' = "Fyvie Castle",
                                                        'Leith Hall' = "Leith Hall")),
                                selectInput("Root_threat", "Root threat:", 
                                            c('All' = "*", 
                                              'Compaction' = "Compaction",
                                              'Pesticide input' = "Pesticide input",
                                              'Fertiliser input' = "Fertiliser input",
                                              'Digging/ditching/ploughing' = "Digging/ditching/ploughing",
                                              'Animal damage' = "Animal damage")),
                                hr(),
                                htmlOutput("rtText"),
                              ),
                              mainPanel(
                                plotOutput("rootPlot", height = 800)  
                              )
                              
                            )
                          )),
                 tabPanel("Crown threat",
                          fluidPage(    
                            titlePanel("Threats to crown"),
                            sidebarLayout(      
                              sidebarPanel(
                                selectInput("crsite", "Site:", 
                                            choices = c('All' = "*", 
                                                        'Balmacara' = "Balmacara",
                                                        'Ben Lomond' = "Ben Lomond",
                                                        'Castle Fraser' = "Castle Fraser",
                                                        'House of the Binns' = "House of the Binns",
                                                        'House of Dun' = "House of Dun",
                                                        'Fyvie Castle' = "Fyvie Castle",
                                                        'Leith Hall' = "Leith Hall")),
                                selectInput("Crown_threat", "Crown threat:", 
                                            c('All' = "*", 
                                              'Native tree competition' = "Native tree competition",
                                              'Non-native tree competition' = "Non-native tree competition",
                                              'Dieback' = "Dieback",
                                              'Disease' = "Disease")),
                                hr(),
                                htmlOutput("crText")
                              ),
                              mainPanel(
                                plotOutput("crownPlot", height = 800)  
                              )
                              
                            )
                          )),
                 tabPanel("Decaying wood",
                          fluidPage(    
                            titlePanel("Decaying wood"),
                            sidebarLayout(      
                              sidebarPanel(
                                selectInput("dwsite", "Site name:", 
                                            choices = c('All' = "*", 
                                                        'Balmacara' = "Balmacara",
                                                        'Ben Lomond' = "Ben Lomond",
                                                        'Castle Fraser' = "Castle Fraser",
                                                        'House of the Binns' = "House of the Binns",
                                                        'House of Dun' = "House of Dun",
                                                        'Fyvie Castle' = "Fyvie Castle",
                                                        'Leith Hall' = "Leith Hall")),
                                hr(),
                                selectInput("dw", "Decaying wood:", 
                                            choices = c('All' = "*", 
                                                        '<10% decaying wood' = "<10% decaying wood",
                                                        '<50% decaying wood' = "<10% decaying wood",
                                                        '>50% decaying wood' = ">50% decaying wood",
                                                        '>90% decaying wood' = ">90% decaying wood",
                                                        'In the crown' = "In the crown",
                                                        'On the ground' = "On the ground",
                                                        'Cracks in bark' = "Cracks in bark",
                                                        'Hollowing trunk' = "Hollowing trunk",
                                                        'Visible hollowing' = "Visible hollowing",
                                                        'Hollowing trunk with opening' = "Hollowing trunk with opening",
                                                        'Hollowing trunk with holes >15cm' = "Hollowing trunk with holes >15cm",
                                                        'Other holes or water pockets' = "Other holes or water pockets")),
                                hr(),
                                htmlOutput("dwText")
                              ),
                              mainPanel(
                                plotOutput("dwPlot", height = 800)  
                              )
                              
                            )
                          )),
                 tabPanel("Epiphytes",
                          fluidPage(    
                            titlePanel("Epiphytes"),
                            sidebarLayout(
                             sidebarPanel(
                              selectInput("epsite", "Site:", 
                                          choices = c('All' = "*", 
                                                      'Balmacara' = "Balmacara",
                                                      'Ben Lomond' = "Ben Lomond",
                                                      'Castle Fraser' = "Castle Fraser",
                                                      'House of the Binns' = "House of the Binns",
                                                      'House of Dun' = "House of Dun",
                                                      'Fyvie Castle' = "Fyvie Castle",
                                                      'Leith Hall' = "Leith Hall")),
                                selectInput("Epiphytes", "Epiphytes:", 
                                            c('All' = "*", 
                                              'Flying tree' = "'Flying' tree",
                                              'Fern' = "Fern",
                                              'Bryophyte' = "Bryophyte",
                                              'Lichen' = "Lichen",
                                              'Honeysuckle' = "Honeysuckle",
                                              'Ivy' = "Ivy",
                                              'Other' = "Other")),
                                hr(),
                                htmlOutput("epText")
                              ),
                              mainPanel(
                                plotOutput("epPlot", height = 800)  
                              )
                            )
                          )),
                 tabPanel("Girth",
                          fluidPage(    
                            titlePanel("Girth"),
                            sidebarLayout(      
                              sidebarPanel(
                                selectInput("Sitename3", "Site name:", 
                                            choices = c('All' = "*", 
                                                        'Balmacara' = "Balmacara",
                                                        'Ben Lomond' = "Ben Lomond",
                                                        'Castle Fraser' = "Castle Fraser",
                                                        'House of the Binns' = "House of the Binns",
                                                        'House of Dun' = "House of Dun",
                                                        'Fyvie Castle' = "Fyvie Castle",
                                                        'Leith Hall' = "Leith Hall")),
                                hr(),
                                selectInput("AVT1", "AVT status:", 
                                            c('All' = "*", 
                                              'Ancient' = "Ancient",
                                              'Veteran' = "Veteran",
                                              'Notable' = "Notable")),
                                hr(),
                                selectInput("gc", "Plot:", 
                                            c('AVT' = "AVT", 
                                              'Species' = "Species",
                                              'Site' = "Site_name",
                                              'Site type' = "Site_type")),
                                hr(),
                              #  helpText(),
                              #  hr(),
                              htmlOutput("grText"),
                              ),
                              mainPanel(
                                plotOutput("giPlot", height = 800)  
                              )
                            )
                          )),
                 tabPanel("About",
                          #fluidPage(    
                          #   mainPanel(
                          #    fillPage(
                          htmlOutput("abText", height = 800)  
                          #   )
                          # ) 
                          #)
                 ),
                 conditionalPanel("false", icon("crosshair"))
)

# server ----
server <- function(input, output, session) {
  # map panel ----
  # A reactive expression that returns the set of trees that are in map view
  TreesInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(avt[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(avt,
           lat >= latRng[1] & lat <= latRng[2] &
             lng >= lngRng[1] & lng <= lngRng[2])
  })
  # Plot girth of visible trees
  output$gview <- renderPlot({
    # If no trees are in view, don't plot
    if (nrow(TreesInBounds()) == 0)
      return(NULL)
    hist(TreesInBounds()$Girth_m,
         main = "All visible trees: girth (m)",
         #breaks = seq(min(TreesInBounds), max(TreesInBounds)),
         xlab = "Girth (m)",
         col = '#69b3a2',
         border = '#e9ecef') 
  })
  output$thview <- renderPlot({
    # If no trees are in view, don't plot
    if (nrow(TreesInBounds()) == 0)
      return(NULL)
      th <- filter(TreesInBounds(), !is.na(Threat_status))
      ggplot(th, aes(fill=factor(Threat_status, levels=c("Critical", "Threatened", "Secure")), x=fct_rev(fct_infreq(Threat_status)))) + 
        geom_bar(position="stack", stat="count", alpha=.6, width=.4) +
        coord_flip() +
        ggtitle("Threat status") +
        scale_fill_manual(values = c("Critical" = "red", "Threatened" = "orange", "Secure" = "#69b3a2"), guide = "none") +
        #scale_fill_manual(values = c("Ancient" = "#008080FF", "Veteran" = "#70A494FF", "Notable" = "#B4C8A8FF")) +
        labs(x = "", y = "", fill = "") +
        theme_minimal()
  })
  # Plot table of visible tree species
  output$sptab <- DT::renderDT({
    tib <- TreesInBounds() %>%
      group_by(Species) %>%
      summarise(Count = n()) %>%
      st_drop_geometry() %>%
      arrange(desc(Count), by_group = FALSE) %>%
      print(n = nrow(TreesInBounds))
    
    DT::datatable(tib, options = list(dom = "t", rownames = FALSE, colnames = FALSE, filter = "none", autoHideNavigation = TRUE, extensions = "Scroller", scrollX = TRUE))
  })
  # map output -----
  output$map <- renderLeaflet({ 
    # Define tree icons
    avt_icon <- makeAwesomeIcon(
      icon = "tree-conifer",
      library = "glyphicon",
      markerColor = case_when(
        avt$AVT == "Ancient" ~ "darkorange",
        avt$AVT == "Veteran" ~ "lightblue",
        avt$AVT == "Notable" ~ "darkblue",
        is.na(avt$AVT) ~ "grey"
      ),
      iconColor = "white",
      spin = FALSE,
      extraClasses = NULL,
      squareMarker = FALSE,
      iconRotate = 0,
      fontFamily = "monospace",
      text = NULL
    )
    # Define icon list for legend
    icon_list <- awesomeIconList(
      Ancient = makeAwesomeIcon(
        icon = "tree-conifer",
        library = "glyphicon",
        markerColor = "darkorange",
        iconColor = "white"),
      Veteran = makeAwesomeIcon(
        icon = "tree-conifer",
        library = "glyphicon",
        markerColor = "lightblue",
        iconColor = "white"),
      Notable = makeAwesomeIcon(
        icon = "tree-conifer",
        library = "glyphicon",
        markerColor = "darkblue"
      )
    )
    # Define popups 
    pnts <- popupTable(nts, 
                       zcol = c("PROPERTY", "AREA_HA"), 
                       row.numbers = FALSE, 
                       feature.id = FALSE)
    pavt <- paste('<div class="scroll-container">
        <a href="', avt$i1, '" target="_blank"><img class="a" src="', avt$i1, '" alt="No more images" width=100% height=100% style="vertical-align:middle"></a>
        <a href="', avt$i2, '" target="_blank"><img class="a" src="', avt$i2, '" alt="No more images" width=100% height=100% style="vertical-align:middle"></a>
        <a href="', avt$i3, '" target="_blank"><img class="a" src="', avt$i3, '" alt="No more images" width=100% height=100% style="vertical-align:middle"></a>
        <a href="', avt$i4, '" target="_blank"><img class="a" src="', avt$i4, '" alt="No more images" width=100% height=100% style="vertical-align:middle"></a>
        <a href="', avt$i5, '" target="_blank"><img class="a" src="', avt$i5, '" alt="No more images" width=100% height=100% style="vertical-align:middle"></a>
        <a href="', avt$i6, '" target="_blank"><img class="a" src="', avt$i6, '" alt="No more images" width=100% height=100% style="vertical-align:middle"></a>
        <a href="', avt$i7, '" target="_blank"><img class="a" src="', avt$i7, '" alt="No more images" width=100% height=100% style="vertical-align:middle"></a>
        <a href="', avt$i8, '" target="_blank"><img class="a" src="', avt$i8, '" alt="No more images" width=100% height=100% style="vertical-align:middle"></a>
           </div>
        <div class="row">
               <div class="column">
                     <b>ID: </b>',avt$id, 
                 '<br><b>Status: </b>', avt$AVT,
                 '<br><b>Species: </b>', avt$Species,
                 '<br><b>Site: </b>', avt$Site_name, 
                 '<br><b>OSGR: </b>', avt$X10figGR, 
                 '<br><b>Date: </b>', avt$Date, 
                 '<br><b>Access: </b>', avt$Access, 
                 '<br><b>TSRN: </b>', avt$Tree_safety_record, 
                 '<br><b>Evidence of: </b>', avt$Evidence_of, 
                 '<br><b>Crown_threat: </b>', avt$Crown_threat, 
                 '<br><b>Epiphyte species: </b>', avt$Epiphyte_species,
                 '<br><b>Public engagement: </b>', avt$public_engagement_notes, 
                 '</div><div class="column">
                     <b>Form: </b>', avt$Form, 
                 '<br><b>Living: </b>', avt$Status_life, 
                 '<br><b>Standing: </b>', avt$Status_standing, 
                 '<br><b>Girth (m): </b>', avt$Girth_m, 
                 '<br><b>Girth measured at (m): </b>', avt$Girth_measurement_height,
                 '<br><b>Threat status: </b>', avt$Threat_status,
                 '<br><b>Site type: </b>', avt$Site_type, 
                 '<br><b>Decaying wood: </b>', avt$Decaying_wood,
                 '<br><b>Grazed: </b>', avt$Grazing,
                 '<br><b>Notes: </b>', avt$Notes, 
                 '</div></div>') 
    
    ptg <- paste('
    <div class="scroll-container">
    <a href="', tg$i1, '" target="_blank"><img class="a" src="', tg$i1, '" alt="No more images" width=100% height=100% style="vertical-align:middle"></a>
    <a href="', tg$i2, '" target="_blank"><img class="a" src="', tg$i2, '" alt="No more images" width=100% height=100% style="vertical-align:middle"></a>
    <a href="', tg$i3, '" target="_blank"><img class="a" src="', tg$i3, '" alt="No more images" width=100% height=100% style="vertical-align:middle"></a>
    <a href="', tg$i4, '" target="_blank"><img class="a" src="', tg$i4, '" alt="No more images" width=100% height=100% style="vertical-align:middle"></a>
    <a href="', tg$i5, '" target="_blank"><img class="a" src="', tg$i5, '" alt="No more images" width=100% height=100% style="vertical-align:middle"></a>
    <a href="', tg$i6, '" target="_blank"><img class="a" src="', tg$i6, '" alt="No more images" width=100% height=100% style="vertical-align:middle"></a>
    <a href="', tg$i7, '" target="_blank"><img class="a" src="', tg$i7, '" alt="No more images" width=100% height=100% style="vertical-align:middle"></a>
    <a href="', tg$i8, '" target="_blank"><img class="a" src="', tg$i8, '" alt="No more images" width=100% height=100% style="vertical-align:middle"></a>
  </div>
      <div class="row">
               <div class="column">
                <b>ID: </b>',tg$uuid, 
                 '<br><b>Site type: </b>', tg$site_type, 
                 '<br><b>Tree species: </b>', tg$tree_species, 
                 '<br><b>Total trees (est): </b>', tg$totaltrees, 
                 '<br><b>Ancient (est): </b>', tg$est_ancient, 
                 '<br><b>Veteran (est): </b>', tg$est_veteran, 
                 '<br><b>Notable (est): </b>', tg$est_notable,
                 '</div><div class="column">
                <br><b>Evidence of habitat connectivity: </b>', tg$evi_hab_connectivity_continuity,
                 '<br><b>Comments: </b>', tg$comments, 
                 '</div></div>')
    # Define a title
    rr <- "NTS Ancient, Veteran and Notable Tree Survey"
    # leaflet ----
    leaflet(options = leafletOptions(minZoom = 8, maxZoom = 18)) |> 
      addTiles(group = "Basemap") |> 
      setView(-3.936903, 57.116524, zoom = 8) %>%
      #fitBounds(-0.289, 55.015, -9.084, 59.803) %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
      addPolygons(
        #lng = tg$geom,
        #lat = tg$geom,
        #layerId = NULL,
        group = "Tree group",
        stroke = TRUE,
        color = "teal",
        weight = 2,
        opacity = 0.5,
        fill = TRUE,
        fillColor = "teal",
        fillOpacity = 0.4,
        #dashArray = NULL,
        smoothFactor = 1,
        noClip = FALSE,
        popup = ptg,
        popupOptions = list(minWidth = 400, keepInView = TRUE, autoPan = TRUE),
        #label = NULL,
        #labelOptions = NULL,
        #options = pathOptions(),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          color = "teal",
          weight = 3,
          opacity = 0.8,
          fill = TRUE,
          fillColor = "teal",
          fillOpacity = 0.6,
          #dashArray = NULL,
          bringToFront = TRUE,
          sendToBack = FALSE
        ),
        data = tg
        
      ) %>%
      addPolygons(
        #lng = nts$geometry,
        #lat = nts$geometry,
        #layerId = NULL,
        group = "NTS Properties",
        stroke = TRUE,
        color = "red",
        weight = 3,
        opacity = 0.8,
        fill = FALSE,
        #fillColor = "teal",
        #fillOpacity = 0.4,
        #dashArray = NULL,
        smoothFactor = 1,
        noClip = FALSE,
        popup = pnts,
        popupOptions = list(minWidth = 400, keepInView = TRUE, autoPan = TRUE),
        label = ~PROPERTY,
        labelOptions = labelOptions(permanent = TRUE, direction = "bottom", noHide = TRUE),
        #options = pathOptions(),
        highlightOptions = highlightOptions(
          stroke = TRUE,
          color = "red",
          weight = 5,
          opacity = 0.8,
          fill = FALSE,
          #fillColor = "teal",
          #fillOpacity = 0.6,
          #dashArray = NULL,
          bringToFront = TRUE,
          sendToBack = FALSE
        ),
        data = nts
      ) %>%
      addAwesomeMarkers(
        data = avt,
        lng = avt$lng,
        lat = avt$lat,
        icon = avt_icon,
        popup = pavt,
        popupOptions = list(minWidth = 400, keepInView = TRUE, autoPan = TRUE),
        label = NULL,
        labelOptions = labelOptions(interactive = TRUE, opacity = 0.8),
        options = markerOptions(),
        clusterOptions = markerClusterOptions(disableClusteringAtZoom = 16, spiderfyOnMaxZoom = FALSE),
        clusterId = NULL,
        group = "Ancient, veteran and notable trees",
        #layerId = id
      ) %>%
      addControl(rr, 
                 position = "topright") %>%
      addLegendAwesomeIcon(
        icon_list,
        position = "topright",
        title = "Legend",
        labelStyle = "",
        orientation = c("vertical", "horizontal"),
        marker = TRUE,
        #group = "Ancient, veteran and notable trees",
        className = "info legend leaflet-control"
      ) %>%
      #addPopupImages(image = imgs, src ="local", embed = FALSE, 
      #               group = "Ancient, veteran and notable trees", 
      #               src = c("local"), 
      #               height = "250px", 
      #               width = "400px"
      #) %>% 
      addLayersControl(overlayGroups = c("Ancient, veteran and notable trees", "Tree group"),
                       baseGroups = c("Basemap", "Esri World Imagery"), 
                       options = layersControlOptions(collapsed = TRUE)) %>%
      addMiniMap(width = 150, 
                 height = 150) %>%
      addMeasure(
        position = "topleft",
        primaryLengthUnit = "meters",
        secondaryLengthUnit = NULL,
        primaryAreaUnit = "hectares",
        secondaryAreaUnit = NULL,
        activeColor = "red",
        completedColor = "red",
        popupOptions = list(className = "leaflet-measure-resultpopup", autoPanPadding = c(10, 10)),
        captureZIndex = 10000,
        localization = "en",
        decPoint = ".",
        thousandsSep = ","
      ) %>%
      addScaleBar(
        position = c("bottomleft"),
        options = scaleBarOptions() 
        #fitBounds(56.6332,-9.0061, 59.4022,-1.3376)
      ) 
  }) 
  
  # Show a popup at the given location
  showTreePopup <- function(id, lat, lng) {
    selectedTree <- avt[avt$id == id,]
    #pt <- popupTable(selectedTree, 
    #                 zcol = c("AVT", "Species", "Date"), 
    #                 row.numbers = FALSE, 
    #                 feature.id = FALSE)   
    #pi <- popupImage(img = selectedTree$i1, 
    #                 src = "local", embed = FALSE,
    #                 height = "200px", 
    #                 width = "300px")
    
    p1 <- paste('<div class="scroll-container">
    <a href="', selectedTree$i1, '" target="_blank"><img class="a" src="', selectedTree$i1, '" alt="No more images" width=100% height=100% style="vertical-align:middle" ></a>
    <a href="', selectedTree$i2, '" target="_blank"><img class="a" src="', selectedTree$i2, '" alt="No more images" width=100% height=100% style="vertical-align:middle"></a>
    <a href="', selectedTree$i3, '" target="_blank"><img class="a" src="', selectedTree$i3, '" alt="No more images" width=100% height=100% style="vertical-align:middle"></a>
    <a href="', selectedTree$i4, '" target="_blank"><img class="a" src="', selectedTree$i4, '" alt="No more images" width=100% height=100% style="vertical-align:middle"></a>
    <a href="', selectedTree$i5, '" target="_blank"><img class="a" src="', selectedTree$i5, '" alt="No more images" width=100% height=100% style="vertical-align:middle"></a>
    <a href="', selectedTree$i6, '" target="_blank"><img class="a" src="', selectedTree$i6, '" alt="No more images" width=100% height=100% style="vertical-align:middle"></a>
    <a href="', selectedTree$i7, '" target="_blank"><img class="a" src="', selectedTree$i7, '" alt="No more images" width=100% height=100% style="vertical-align:middle"></a>
    <a href="', selectedTree$i8, '" target="_blank"><img class="a" src="', selectedTree$i8, '" alt="No more images" width=100% height=100% style="vertical-align:middle"></a>
          </div>         
                   <div class="row">
                   <div class="column">
                   <b>ID: </b>',selectedTree$id, 
                '<br><b>Status: </b>', selectedTree$AVT,
                '<br><b>Species: </b>', selectedTree$Species,
                '<br><b>Site: </b>', selectedTree$Site_name, 
                '<br><b>Date: </b>', selectedTree$Date, 
                '<br><b>OSGR: </b>', selectedTree$X10figGR, 
                '<br><b>Access: </b>', selectedTree$Access, 
                '<br><b>TSRN: </b>', selectedTree$Tree_safety_record, 
                '<br><b>Evidence of: </b>', selectedTree$Evidence_of, 
                '<br><b>Crown_threat: </b>', selectedTree$Crown_threat, 
                '<br><b>Epiphyte species: </b>', selectedTree$Epiphyte_species,
                '<br><b>Public engagement: </b>', selectedTree$public_engagement_notes, 
                '</div><div class="column">
                   <b>Form: </b>', selectedTree$Form, 
                '<br><b>Living: </b>', selectedTree$Status_life, 
                '<br><b>Standing: </b>', selectedTree$Status_standing, 
                '<br><b>Girth (m): </b>', selectedTree$Girth_m, 
                '<br><b>Girth measured at (m): </b>', selectedTree$Girth_measurement_height,
                '<br><b>Threat status: </b>', selectedTree$Threat_status,
                '<br><b>Site type: </b>', selectedTree$Site_type, 
                '<br><b>Decaying wood: </b>', selectedTree$Decaying_wood,
                '<br><b>Grazed: </b>', selectedTree$Grazing,
                '<br><b>Notes: </b>', selectedTree$Notes, 
                '</div></div>')
    
    
    leafletProxy("map") %>% addPopups(lng, lat, p1, options = popupOptions(minWidth = 400, autoPan = TRUE, keepInView = FALSE)) 
  }
  # datatable output -----
  # Observer for clicks on locator button
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      #dist <- 0.05
      lat <- input$goto$lat
      lng <- input$goto$lng
      zip <- input$goto$zip
      showTreePopup(zip, lat, lng)
      #map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
      map %>% setView(lng, lat, zoom = 18)
    })
  })
  # Data table output
  output$avttable <- DT::renderDataTable({
    df <- avt %>% 
      mutate(Zoom = paste('<a class="go-map" href=""data-lat="', lat, '" data-lng="', lng, '" data-zip="', id,
                            '"><i class="fas fa-search-plus"></i></a>',
                            sep="")) 
    df$Epiphytes <- replace_na(df$Epiphytes, "NA")
    df$Threat_status <- replace_na(df$Threat_status, "NA")
    df$Evidence_of <- replace_na(df$Evidence_of, "NA")
    df$Site_type <- replace_na(df$Site_type, "NA")
    
    df <- filter(df, grepl(input$dst, Site_name), grepl(input$dth, Threat_status), grepl(input$davt, AVT), 
                 grepl(input$dep, Epiphytes), grepl(input$dsp, Species), grepl(input$dev, Evidence_of), 
                 grepl(input$dsty, Site_type), grepl(input$ddw, Decaying_wood)) 
    df <- select(df, Zoom, Date, Site_name, Species, AVT, Form, Status_life, Status_standing, Threat_status, 
                 X10figGR, Site_type, Site_type_other, Girth_m, Girth_measurement_height, Epiphytes, Epiphyte_species, Evidence_of, Decaying_wood, Root_threat,
                 Trunk_threat, Crown_threat, Tree_threat, Grazing, Grazing_notes, Champion, Visible_tag, Notes, Proposed_action1_threattype, 
                 Proposed_action1_agent, Proposed_action1_swp, Proposed_action1_priority., Repeat_assessment., img_files)  
    #df <- select(df, -c(Tree_safety_record, Tree_threat, IrisBG, public_engagement_audio, img_files, i1, i2, i3, i4, i6, i7, i8, Time, ssrn, Example, id, 
    #                    x, y, Site_survey_record, Number_photos, Access, Recorder))
    df <- st_drop_geometry(df)
    action <- DT::dataTableAjax(session, df, rownames = TRUE, outputId = "avttable")
    
    DT::datatable(df, filter="none", options = list(ajax = list(url = action), autoWidth = TRUE, scrollX = TRUE), escape = FALSE, rownames = TRUE, class = "nowrap")
  })
  # Download button
   output$downloadData <- downloadHandler(
     filename = function() {
       paste('avt-', Sys.Date(), '.csv', sep='')
     },
     content = function(con) {
       write.csv(avt, con)
     }
   )
  # plots for tabs ----
  output$sitePlot <- renderPlot({
    i <- filter(avt, grepl(input$Site_name, Site_name), grepl(input$Threat1, Threat_status))
    ggplot(i, aes(fill=AVT, x=fct_rev(fct_infreq(Species)))) + 
      geom_bar(position="stack", stat="count", alpha=.6, width=.4) +
      coord_flip() +
      xlab("") +
      #facet_wrap(~Site_name) +
      ggtitle(input$Site_name, "Tree species, AVT status") +
      scale_fill_manual(values = c("Ancient" = "#008080FF", "Veteran" = "#70A494FF", "Notable" = "#B4C8A8FF")) +
      theme_minimal()
  })
  output$stypePlot <- renderPlot({
    i <- filter(avt, grepl(input$stysite, Site_name), grepl(input$Site_type, Site_type))
    t <- paste(ifelse(input$stysite == "*", "All sites", input$stysite), ": ", ifelse(input$Site_type == "*", "All site types", input$Site_type))
    ggplot(i, aes(fill=AVT, x=fct_rev(fct_infreq(Species)))) + 
      geom_bar(position="stack", stat="count", alpha=.6, width=.4) +
      coord_flip() +
      xlab("") +
      #facet_wrap(~Site_name) +
      ggtitle(t, "Tree species, AVT status") +
      scale_fill_manual(values = c("Ancient" = "#008080FF", "Veteran" = "#70A494FF", "Notable" = "#B4C8A8FF")) +
      theme_minimal()
  })
  output$stypeText <- renderUI({
    sty <- filter(avt, grepl(input$stysite, Site_name), grepl(input$Site_type, Site_type))
    HTML(paste("<div>Count of trees by site type/surroundings. 
              <hr>Total observations shown = <b>", nrow(sty), "</b>
               <br>Proportion of total recorded trees = <b>", sprintf((nrow(sty)/nrow(avt))*100, fmt = '%#.2f'), "%</b>
               <hr>Note: <b>", nrow(filter(avt, is.na(Site_type))), "</b> trees have no recorded surroundings value.
               </div>"
    ))
  })
  output$batPlot <- renderPlot({
    #replace_na(avt$Evidence_of, "NA")
    bb <- filter(avt, grepl(input$evsite, Site_name), grepl(input$evid, Evidence_of))
    t <- paste(ifelse(input$evsite == "*", "All sites", input$evsite), ": ",  
               ifelse(input$evid == "*", "All", ifelse(input$evid == "potential", "Bats (potential habitat)", 
                                                       ifelse(input$evid == "presence", "Bats (presence/activity)", 
                                                              input$evid))))
        ggplot(bb, aes(fill = AVT, x=(fct_rev(fct_infreq(Species))))) +
      geom_bar(position="stack", stat="count", alpha=.6, width=.4) +
      coord_flip() +
      xlab("") +
      ylab("") +
      ggtitle(t, "Host species, AVT status") +
      scale_fill_manual(values = c("Ancient" = "#008080FF", "Veteran" = "#70A494FF", "Notable" = "#B4C8A8FF")) +
      theme_minimal()
  })
  output$evText <- renderUI({
    evv <- filter(avt, grepl(input$evsite, Site_name), grepl(input$evid, Evidence_of))
    HTML(paste("<div>Count of trees with evidence of certain species. 
              <hr>Total observations shown = <b>", nrow(evv), "</b>
               <br>Proportion of total recorded trees = <b>", sprintf((nrow(evv)/nrow(avt))*100, fmt = '%#.2f'), "%</b>
               </div>"
    ))
  })
  output$dwPlot <- renderPlot({
    ddw <- filter(avt, !is.na(Threat_status), grepl(input$dw, Decaying_wood), grepl(input$dwsite, Site_name))
    t <- paste(ifelse(input$dwsite == "*", "All sites", input$dwsite), ": ", ifelse(input$dw == "*", "All", input$dw))
    ggplot(ddw, aes(fill=Threat_status, x=(fct_rev(fct_infreq(Species))))) +
      geom_bar(position="stack", stat="count", alpha=.6, width=.4) +
      coord_flip() +
      xlab("") +
      ylab("") +
      labs(fill = "Threat status") +
      ggtitle(t, "Species and threat status") +
      scale_fill_manual(values = c("Critical" = "red", "Threatened" = "orange", "Secure" = "#69b3a2")) +
      theme_minimal()
  })
  output$dwText <- renderUI({
    dw <- filter(avt, !is.na(Threat_status), grepl(input$dw, Decaying_wood), grepl(input$dwsite, Site_name))
    HTML(paste("<div>Count of trees with decaying wood by tree species and site. 
              <br>Column colours indicate overall threat status.
              <hr>Total observations shown = <b>", nrow(dw), "</b>
               <br>Proportion of total recorded trees = <b>", sprintf((nrow(dw)/nrow(avt))*100, fmt = '%#.2f'), "%</b>
               </div>"
    ))
  })
  output$threatPlot <- renderPlot({
    ts <- filter(avt, Threat_status != "NA", grepl(input$Sitename4, Site_name), grepl(input$AVT3, AVT))
    t <- paste(ifelse(input$Sitename4 == "*", "All sites", input$Sitename4), ": ", ifelse(input$AVT3 == "*", "All", input$AVT3), " trees")
    ggplot(ts, aes(fill=factor(Threat_status, levels=c("Critical", "Threatened", "Secure")), x=fct_rev(fct_infreq(Species)))) + 
      geom_bar(position="stack", stat="count", alpha=.6, width=.4) +
      coord_flip() +
      xlab("") +
      ggtitle(t, "Species, Threat status") +
      scale_fill_manual(values = c("Critical" = "red", "Threatened" = "orange", "Secure" = "#69b3a2")) +
      labs(fill = "Threat status") +
      theme_minimal()
  })
  output$thText <- renderUI({
    th <- filter(avt, Threat_status != "NA", grepl(input$Sitename4, Site_name), grepl(input$AVT3, AVT))
    na <- filter(avt, is.na(Threat_status))
    HTML(paste("<div>Count of trees by overall threat status, tree species and site. 
              <br>Column colours indicate overall threat status. 
              <br>Note: <b>", nrow(na), "</b> (", sprintf((nrow(na)/nrow(avt))*100, fmt = '%#.2f'), "%) trees with NA values for overall threat status not included.
              <hr>Total observations shown = <b>", nrow(th), "</b>
               <br>Proportion of total recorded trees = <b>", sprintf((nrow(th)/nrow(avt))*100, fmt = '%#.2f'), "%</b>
               <hr>Of trees shown: 
               <br>Secure = <b>", nrow(filter(th, Threat_status == "Secure")), "</b>(", sprintf((nrow(filter(th, Threat_status == "Secure"))/nrow(th))*100, fmt = '%#.2f'), "%)
                              <br>Threatened = <b>", nrow(filter(th, Threat_status == "Threatened")), "</b>(", sprintf((nrow(filter(th, Threat_status == "Threatened"))/nrow(th))*100, fmt = '%#.2f'), "%)
                                             <br>Critical = <b>", nrow(filter(th, Threat_status == "Critical")), "</b>(", sprintf((nrow(filter(th, Threat_status == "Critical"))/nrow(th))*100, fmt = '%#.2f'), "%)
               </div>"
    ))
  })
  output$rootPlot <- renderPlot({
    rt <- filter(avt, !is.na(Threat_status), grepl(input$Root_threat, Root_threat), grepl(input$rtsite, Site_name))
    t <- paste(ifelse(input$rtsite == "*", "All sites", input$rtsite), ": ", ifelse(input$Root_threat == "*", "All root threats", input$Root_threat))
    ggplot(rt, aes(fill=Threat_status, x=(fct_rev(fct_infreq(Species))))) +
      geom_bar(position="stack", stat="count", alpha=.6, width=.4) +
      coord_flip() +
      xlab("") +
      ylab("") +
      labs(fill = "Threat status") +
      ggtitle(t, "Species and threat status") +
      scale_fill_manual(values = c("Critical" = "red", "Threatened" = "orange", "Secure" = "#69b3a2")) +
      theme_minimal()
  })
  output$rtText <- renderUI({
    ct <- filter(avt, grepl(input$rtsite, Site_name), grepl(input$Root_threat, Root_threat), !is.na(Threat_status))
    HTML(paste("<div>Count of trees with recorded threats to roots by tree species and site. 
              <br>Column colours indicate overall threat status.
              <hr>Total observations shown = <b>", nrow(ct), "</b>
               <br>Proportion of total recorded trees = <b>", sprintf((nrow(ct)/nrow(avt))*100, fmt = '%#.2f'), "%</b>
               </div>"
    ))
  })
  output$crownPlot <- renderPlot({
    ct <- filter(avt, grepl(input$crsite, Site_name), grepl(input$Crown_threat, Crown_threat), !is.na(Threat_status))
    t <- paste(ifelse(input$crsite == "*", "All sites", input$crsite), ": ", ifelse(input$Crown_threat == "*", "All crown threats", input$Crown_threat))
    ggplot(ct, aes(fill=Threat_status, x=(fct_rev(fct_infreq((Species)))))) +
      geom_bar(position="stack", stat="count", alpha=.6, width=.4) +
      coord_flip() +
      xlab("") +
      ylab("") +
      labs(fill = "Threat status") +
      ggtitle(t, "Site and threat status") +
      scale_fill_manual(values = c("Critical" = "red", "Threatened" = "orange", "Secure" = "#69b3a2")) +
      theme_minimal()
  })
  output$crText <- renderUI({
    ct <- filter(avt, grepl(input$crsite, Site_name), grepl(input$Crown_threat, Crown_threat), !is.na(Threat_status))
    HTML(paste("<div>Count of trees with recorded threats to crown by tree species and site. 
              <br>Column colours indicate overall threat status.
              <hr>Total observations shown = <b>", nrow(ct), "</b>
              <br>Proportion of total recorded trees = <b>", sprintf((nrow(ct)/nrow(avt))*100, fmt = '%#.2f'), "%</b></div>"
    ))
  })
  output$epPlot <- renderPlot({
    e <- filter(avt, grepl(input$epsite, Site_name), grepl(input$Epiphytes, Epiphytes))
    t <- paste(ifelse(input$epsite == "*", "All sites", input$epsite), ": ", ifelse(input$Epiphytes == "*", "All epiphytes", input$Epiphytes))
    ggplot(e, aes(fill=AVT, x=(fct_rev(fct_infreq(Species))))) +
      geom_bar(position="stack", stat="count", alpha=.6, width=.4) +
      #geom_text(aes(label = ..count..), stat = "count", vjust = 0, colour = "red") +
      coord_flip() +
      xlab("") +
      ylab("") +
      ggtitle(t, "Host tree species and AVT status") +
      scale_fill_paletteer_d("rcartocolor::Geyser") +
      theme_minimal()
  })
  output$epText <- renderUI({
    epp <- filter(avt, grepl(input$epsite, Site_name), grepl(input$Epiphytes, Epiphytes))
    HTML(paste("<div>Count of trees with recorded epiphytes by host species and site. 
              <br>See 'Epiphyte_species' column for details on species.
              <hr>Total observations shown = <b>", nrow(epp), "</b>
               <br>Proportion of total recorded trees = <b>", sprintf((nrow(epp)/nrow(avt))*100, fmt = '%#.2f'), "%</b>
               </div>"
    ))
  })
  output$giPlot <- renderPlot({
    g <- subset(avt, grepl(input$Sitename3, Site_name))
    gi <- subset(g, grepl(input$AVT1, AVT))
    gi$Species = with(gi, reorder(Species, Girth_m, median))
    gt <- paste(ifelse(input$Sitename3 == "*", "All sites", input$Sitename3), ": ", ifelse(input$AVT1 == "*", "All", input$AVT1), " trees")
    give.n <- function(x){
      return(c(y = median(x)*1.05, label = length(x))) 
    }
    xl <- paste(levels(gi$Species))
      xll <- paste(levels(gi$Species),"\nN=", table(gi$Species), sep="")
      igc <- as_string(input$gc)
    #xlabs <- xl #ifelse(nchar(input$Sitename3)>2, xll, xl)
     # ifelse(nchar(input$Sitename3)>2, 
    ggplot(gi, aes_string(x=igc, y="Girth_m")) + 
      geom_boxplot(fill="#69b3a2", alpha=0.2) + 
      stat_summary(fun.data = give.n, geom = "label", fun.y = median) + 
      coord_flip() +
      #scale_x_discrete(labels=xl) +
      xlab("") +
      ylab("Girth (m)") +
      ggtitle(gt, "Girth by species") +
      theme_minimal()
  })
  output$grText <- renderUI({
    #ge <- subset(avt, grepl(input$Sitename3, Site_name)) %>%
    #subset(ge, grepl(input$AVT1, AVT))
    ge <- avt %>% filter(grepl(input$Sitename3, Site_name), grepl(input$AVT1, AVT))
    HTML(paste("<div>Distribution of girth by species in metres. 
   <br>Numbers in white boxes indicate number of observations.
              <hr>Total observations shown = <b>", nrow(ge), "</b></div>"
    ))
  })
  output$abText <- renderUI({
    HTML(paste("<div style='background-color:floralwhite;padding:25px;text-align:left'>
<h3>NTS AVT Survey 2025/6</h3>
<p>Seven National Trust for Scotland properties were surveyed to gather data on the abundance, location and characteristics of ancient, veteran and notable trees.</p>
<p>Three experienced ecologists used a custom <a href='qfield.org'>Qfield</a> project installed on Android tablets to collect the data. The following properties were surveyed:</p>
<p><strong>Ben Lomond:</strong> 16-20 May 2025; 23-24 January 2026</p>
<p><strong>Castle Fraser:</strong> 9-13 June 2025</p>
<p><strong>House of the Binns:</strong> 1, 16, 22 July 2025; 4 September 2025</p>
<p><strong>House of Dun:</strong> 20-21 November 2025; 8-9 December 2025</p>
<p><strong>Fyvie Castle:</strong> 28 November 2025; 5, 18, 20 December 2025</p>
<p><strong>Balmacara:</strong> 6-11 December 2025; 17-21 January 2026</p>
<p><strong>Leith Hall:</strong> 9-10 December 2025</p>
<p>Individual AVTs were recorded as point vector data and groups of trees were recorded as polygon vector data.</p>
<p>The resulting point data are accurate to &lt;1m. The dataset contains latitude and longitude coordinates for each observation along with 10-figure Ordnance Survey grid references.</p>
<p>Representative photos were taken of each tree and tree group using the tablets’ cameras. The metadata of these photos include the a timestamp and the latitude and longitude of the tablet’s position at the time the photograph was taken.</p>
<p>Further data was gathered through the completion of a form by the surveyors when recording each observation. This data was designed in part to correspond to the <a href='https://ati.woodlandtrust.org.uk/'>Ancient Tree Inventory</a> maintained by the Woodland Trust.</p>
<h3>Application</h3>
<p>This <a href='https://shiny.posit.co/'>Shiny</a> application displays the survey results as an interactive map, a searchable datatable, and a set of interactive graphs.</p>
<p>The full AVT dataset can be downloaded as a .csv spreadsheet using the link on the Datatable tab.</p>
<p>The application’s source code can be viewed <a href='https://github.com/calumleslie77/NTS_AVT_Survey'>here</a>.</p>
<h3>Licensing</h3>
<p>All survey data, including images, copyright National Trust for Scotland.</p>
<p>All other code copyleft under <a href='www.gnu.org/licenses/gpl-3.0.txt'>GNU GPL 3.0 (or later)</a>.</p>
<p>Contact: calum.leslie@pm.me</p></div>"
    ))
  })
}

# run app ----

shinyApp(ui = ui, server = server)

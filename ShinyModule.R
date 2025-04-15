library("shiny")
library("shinydashboard")
library("move2")
#library("moveShiny")
devtools::load_all("~/moveShiny")
library("stringr")
library("sf")
library("leaflet")
shinyOptions(cache = cachem::cache_disk(file.path(dirname(tempdir()), "myapp-cache")))

# to display messages to the user in the log file of the App in MoveApps
# one can use the function from the src/common/logger.R file:
# logger.fatal(), logger.error(), logger.warn(), logger.info(), logger.debug(), logger.trace()

shinyModuleUserInterface <- function(id, label) {
  # all IDs of UI functions need to be wrapped in ns()
  ns <- NS(id)
  # showcase to access a file ('auxiliary files') that is 
  # a) provided by the app-developer and 
  # b) can be overridden by the workflow user.
#  fileName <- getAuxiliaryFilePath("auxiliary-file-a")
  
  tagList(
    dashboardPage(
      dashboardHeader(title="Radar track explore"),
      dashboardSidebar(          moveDateSelectUI(ns("dateRangeNS")),
                                 moveMapTimeSliderUI(ns("timeSliderNS")),
                            
                                 radarMapUI(ns("radarMapNS"))),
      dashboardBody(          leafletOutput(ns("map")),
                              verbatimTextOutput(ns("prt")))
    )

  )
}

# The parameter "data" is reserved for the data object passed on from the previous app
shinyModule <- function(input, output, session, data) {
  # # all IDs of UI functions need to be wrapped in ns()
   ns <- session$ns
  output$prt<-renderPrint(
    {      list(time$time())})
      output$map <- renderLeaflet(leaflet(height = "600px") |> addTiles())
      map_proxy <- (leafletProxy(("map")) |> setView(5, 52, 5))
      # note the map proxy needs to be defined here to avoid using the wrong id
      time <- moveMapTimeSliderServer("timeSliderNS",
        map_proxy = map_proxy,
        move = dateSelected$move
   
      )
      radarMapServer("radarMapNS",
        map_proxy = map_proxy,
        time = time$time
      )
  dateSelected<-moveDateSelectServer("dateRangeNS", current)
  d <- as.Date("2023-6-30")
  
  #     current<-reactiveVal({
  #   m <-
  #     move2::mt_stack(
  #       move2::movebank_download_study(1258895879,
  #         attributes = NULL,
  #         timestamp_start = as.POSIXct(d),
  #         timestamp_end = as.POSIXct(d + 10)
  #       ),
  #       move2::movebank_download_study(2298738353,
  #         attributes = NULL,
  #         timestamp_start = as.POSIXct(d),
  #         timestamp_end = as.POSIXct(d + 10)
  #       )
  #     )
  # })
      # current<-reactiveVal({
      #   m <-
      #     move2::mt_stack(
      #       move2::movebank_download_study(1605802367,
      #                                      attributes = NULL,
      #                                      timestamp_start = as.POSIXct("2020-10-8"),
      #                                      timestamp_end = as.POSIXct("2020-10-10")
      #       )
      #     )
      # })
      
      current<-reactiveVal({
        m <-
          move2::mt_stack(
            move2::movebank_download_study(1415844328,
                                           attributes = NULL,
                                            timestamp_start = as.POSIXct("2020-10-1"),
                                            timestamp_end = as.POSIXct("2020-10-20")
            )
          )
      })
      
  # current <- reactiveVal(data)
  # 
  # ##--## example code, 1 individual per tab ##--##
  # namesCorresp <- data.frame(nameInd=unique(mt_track_id(data)) , tabIndv=str_replace_all(unique(mt_track_id(data)), "[^[:alnum:]]", ""))
  # ntabs <- length(unique(mt_track_id(data)))
  # tabnames <- str_replace_all(unique(mt_track_id(data)), "[^[:alnum:]]", "")
  # plotnames <- paste0("plot_",tabnames) 
  # output$SidebarUI <- renderUI({
  #   Menus <- vector("list", ntabs)
  #   for(i in 1:ntabs){
  #     Menus[[i]] <-   menuItem(tabnames[i], icon=icon("paw"), tabName = tabnames[i], selected = i==1) }
  #   do.call(function(...) sidebarMenu(id = ns('sidebarMenuUI'),...), Menus)
  # })
  # output$TabUI <- renderUI({
  #   Tabs <- vector("list", ntabs)
  #   for(i in 1:ntabs){
  #     Tabs[[i]] <- tabItem(tabName = tabnames[i],
  #                          plotOutput(ns(plotnames[i]),height="75vh")
  #     )
  #   }
  #   do.call(tabItems, Tabs)
  # })
  # 
  # RVtab <- reactiveValues()
  # observe({
  #   RVtab$indv <- namesCorresp$nameInd[namesCorresp$tabIndv==input$sidebarMenuUI]
  # })
  # for(i in 1:ntabs){
  #   output[[plotnames[i]]] <- renderPlot({
  #     dat <- filter_track_data(data, .track_id=RVtab$indv)
  #     plot(st_geometry(mt_track_lines(dat)))
  #   })
  # }
  # ##--## end of example ##--##
  
  # data must be returned. Either the unmodified input data, or the modified data by the app
  return(reactive({ current() }))
}

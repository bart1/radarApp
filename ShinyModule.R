library("shiny")
library("shinydashboard")
library("move2")
library("moveShiny")
cl <- parallel::makeCluster(6, timeout = 60)

future::plan(future::cluster, workers=cl)

parallel::clusterEvalQ(cl,library(bioRad))
parallel::clusterEvalQ(cl,library(memoise))
parallel::clusterEvalQ(cl,library(shiny))
parallel::clusterEvalQ(cl,library(starsTileServer))


#devtools::load_all("~/moveShiny")
#library("moveShiny")

library("stringr")
library("sf")
library("leaflet")
#shinyOptions(cache = cachem::cache_disk(file.path(dirname(tempdir()), "radarAppCache")))
shinyOptions(cache =cc<- cachem::cache_mem(logfile=stdout()))
cc<-cachem::cache_disk()
parallel::clusterExport(cl, c("cc"))
parallel::clusterEvalQ(cl, shinyOptions(cache=cc))
#parallel::clusterEvalQ(cl,    transform_matrix_cache2 <- memoise::memoise(compiler::cmpfun(moveShiny::transform_matrix), cache = shiny::getShinyOption("cache")))

#shinyOptions(cache = cachem::cache_mem())

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
      dashboardHeader(title = "Radar track explore",
                        dropdownMenu(type = "messages", badgeStatus =NULL# "success"
                                     , icon = icon("gear"),
                                     headerText = "",
                                   messageItem("Track visualization",
                                               sliderInput(min = 0, max = 120,inputId = ns("asdf"),label = "Tail duration (minutes)", value = 20, step = 1)
                                   ),
                                   messageItem("Radar display",
                                               "This is the content of another message."
                                   )
                      )),
      dashboardSidebar(
        moveDateSelectUI(ns("dateRangeNS")),
        moveMapTimeSliderUI(ns("timeSliderNS")),
        radarMapUI(ns("radarMapNS"))
      ),
      dashboardBody(
        leafletOutput(ns("map"), height = "600px"),
        verbatimTextOutput(ns("prt"))
      )
    )
  )
}

# The parameter "data" is reserved for the data object passed on from the previous app
shinyModule <- function(input, output, session, data) {
  # all IDs of UI functions need to be wrapped in ns()
  ns <- session$ns
  output$prt <- renderPrint({
    invalidateLater(500)
    list(future::availableCores(), time$time(), format(Sys.time(), "%H:%M:%OS3"), radar$scanTime())
  })
  output$map <- renderLeaflet(leaflet(height = "600px") |> addTiles())
  map_proxy <- leafletProxy("map") |> setView(15, 62, 5)
  colFun<-reactive({colorFactor("RdYlBu", levels=unique(mt_track_id(current())))})
  time <- moveMapTimeSliderServer("timeSliderNS",
    map_proxy = map_proxy,
    move = dateSelected$move,
    colorFunctionTrackId = colFun,
    updateTime=radar$scanTime |> throttle(1500),tailDurationMinutes =reactive({input$asdf})
  )

  radar<-radarMapServer("radarMapNS",
    map_proxy = map_proxy,
    time = time$time
  )
  dateSelected <- moveDateSelectServer("dateRangeNS", current)
  # moveRadarTime<-reactive({
  #   req(radar$scanTime())
  #   req(dateSelected$move())
  #   m<-dateSelected$move()
  #
  #   mm<-move2::mt_interpolate(m, time = radar$scanTime(), omit = T)
  #   mm<-mm[!sf::st_is_empty(mm),]
  #   mm$event_id<-1:nrow(mm)
  #   mm
  # })
  # moveMapPointsServer("pointsModule",moveRadarTime,
  #                     map_bounds = reactive(input$map_bounds),
  #                     map_marker_click = reactive(input$map_marker_click),
  #                     map_proxy = map_proxy)
  # moveMapLinesServer(map_proxy = map_proxy, id="linesNS", move = moveLastHour)
  #d <- as.Date("2023-6-30")

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

 # current <- reactiveVal({
 #   m <-
 #     move2::mt_stack(
 #       move2::movebank_download_study(1415844328,
 #         attributes = NULL,
 #         timestamp_start = as.POSIXct("2020-10-10"),
 #         timestamp_end = as.POSIXct("2020-10-16")
 #       )
 #     )
 # })

   current <- reactiveVal(data)
  #
  # data must be returned. Either the unmodified input data, or the modified data by the app
  return(reactive({
    current()
  }))
}

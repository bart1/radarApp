library("shiny")
library("shinydashboard")
library("move2")
library("stringr")
library("sf")

# to display messages to the user in the log file of the App in MoveApps
# one can use the function from the src/common/logger.R file:
# logger.fatal(), logger.error(), logger.warn(), logger.info(), logger.debug(), logger.trace()

shinyModuleUserInterface <- function(id, label) {
  # all IDs of UI functions need to be wrapped in ns()
  ns <- NS(id)
  # showcase to access a file ('auxiliary files') that is 
  # a) provided by the app-developer and 
  # b) can be overridden by the workflow user.
  fileName <- getAuxiliaryFilePath("auxiliary-file-a")
  
  tagList(
    dashboardPage(
      dashboardHeader(title = paste("MoveApps R-Shiny Dashboard SDK"),titleWidth=500),
      dashboardSidebar(uiOutput(ns("SidebarUI"))), 
      dashboardBody(
        fluidRow(
          box(
            title = "Local App File", 
            readChar(fileName, file.info(fileName)$size)
          )
        ),
        uiOutput(ns("TabUI"))
      )
    )
  )
}

# The parameter "data" is reserved for the data object passed on from the previous app
shinyModule <- function(input, output, session, data) {
  # all IDs of UI functions need to be wrapped in ns()
  ns <- session$ns
  current <- reactiveVal(data)

  ##--## example code, 1 individual per tab ##--##
  namesCorresp <- data.frame(nameInd=unique(mt_track_id(data)) , tabIndv=str_replace_all(unique(mt_track_id(data)), "[^[:alnum:]]", ""))
  ntabs <- length(unique(mt_track_id(data)))
  tabnames <- str_replace_all(unique(mt_track_id(data)), "[^[:alnum:]]", "")
  plotnames <- paste0("plot_",tabnames) 
  output$SidebarUI <- renderUI({
    Menus <- vector("list", ntabs)
    for(i in 1:ntabs){
      Menus[[i]] <-   menuItem(tabnames[i], icon=icon("paw"), tabName = tabnames[i], selected = i==1) }
    do.call(function(...) sidebarMenu(id = ns('sidebarMenuUI'),...), Menus)
  })
  output$TabUI <- renderUI({
    Tabs <- vector("list", ntabs)
    for(i in 1:ntabs){
      Tabs[[i]] <- tabItem(tabName = tabnames[i],
                           plotOutput(ns(plotnames[i]),height="75vh")
      )
    }
    do.call(tabItems, Tabs)
  })
  
  RVtab <- reactiveValues()
  observe({
    RVtab$indv <- namesCorresp$nameInd[namesCorresp$tabIndv==input$sidebarMenuUI]
  })
  for(i in 1:ntabs){
    output[[plotnames[i]]] <- renderPlot({
      dat <- filter_track_data(data, .track_id=RVtab$indv)
      plot(st_geometry(mt_track_lines(dat)))
    })
  }
  ##--## end of example ##--##
  
  # data must be returned. Either the unmodified input data, or the modified data by the app
  return(reactive({ current() }))
}

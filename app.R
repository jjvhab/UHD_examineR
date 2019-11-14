# UHD examineR v1.0.2 by V.Haburaj
#
# Load and inspect hyperspectral recordings captured with a
# Cubert UHD258 snapshot camera. The imported raster file is
# plotted as RGB image. The raster can be manipulated
# (spatial and spectral filters). By selecting an area of
# interest in the plotted raster, a plot of the mean spectrum
# is generated (+/- sd). The selected spectrum can be saved
# to a data.frame, which can be exported as CSV file. The
# manipulated raster can be saved to a TIF file.
#
# by Vincent Haburaj, vincent.haburaj@fu-berlin.de


# ------------------------
# Load Packages ----------
# ------------------------

if (!require("shiny")) {
  message('package shiny not found!')
} else library(shiny)
if (!require("shinydashboard")) {
  message('package shinydashboard not found!')
} else library(shinydashboard)
if (!require("shinyjs")) {
  message('package shinyjs not found!')
} else library(shinyjs)
if (!require("shinyWidgets")) {
  message('package shinyWidgets not found!')
} else library(shinyWidgets)
if (!require("DT")) {
  message('package DT not found!')
} else library(DT)
if (!require("raster")) {
  message('package raster not found!')
} else library(raster)
if (!require("velox")) {
  message('package velox not found!')
} else library(velox)
if (!require("ggplot2")) {
  message('package ggplot2 not found!')
} else library(ggplot2)
if (!require("hsdar")) {
  message('package hsdar not found!')
} else library(hsdar)


# ------------------------
# Define UI --------------
# ------------------------

# jscode for closing the app:
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

ui = dashboardPage(

  # define skin:
  skin = 'black',

  # define header:
  dashboardHeader(
    title = "UHD examineR",

    # add close button:
    tags$li(actionButton("close", "",
                         icon = icon('power-off'),
                         style="color: #fff; background-color: #FC091B; border-color: #FC091B"),
            class='dropdown')
  ),

  # make sidebar with control widgets:
  dashboardSidebar(

    # needed for close button:
    useShinyjs(),
    extendShinyjs(text = jscode, functions = c("closeWindow")),

    # specify input file:
    fileInput(inputId = 'file',
              label = 'Select raster:'),

    # choose image rotation angle:
    selectInput('sliderROT', label='rotate image clockwise by:',
                choices = c(0,90,180,270),
                selected=0)
  ),

  # make main body:
  dashboardBody(

    # ROW 1:
    fluidRow(

      # image plotting area:
      box(
        status = 'primary',
        title = "Raster",
        solidHeader = TRUE,
        plotOutput("rast", brush = brushOpts(
          id = "rasPlot_brush",
          stroke = 'cyan',
          fill='red'),
          width = '100%',
          height = '600px')
      ),

      column(
        width=6,

        # process raster data:
        tabBox(
          width = NULL,
          title = 'Filter raster',

          # SPATIAL FILTER:
          tabPanel('Spatial filter',

                   # optionally apply spatial filter:
                   checkboxInput("checkboxSpatialFilter",
                                 label = "Spatial filter",
                                 value = FALSE),

                   # select spatial filter:
                   selectInput("selectedSpatialFilter",
                               label = "spatial filter",
                               c("median","mean")),

                   # choose Median window size:
                   selectInput('sliderMED', label='spatial filter window size',
                               choices = c(3,5,7,9,11,13,15,17,19,21),
                               selected=3)
          ),
          tabPanel('Spectral filter',

                   # optionally apply spectral filter:
                   checkboxInput("checkboxSG",
                                 label = "Savitzky-Golay (2nd order)",
                                 value = FALSE),

                   # choose SG filter width:
                   selectInput('sliderSG', label='filter width',
                               choices = c(3,5,7,9,11,13,15),
                               selected=5)
          )
        ),

        box(
          width = NULL,
          title = 'Save filtered raster data',

          # download processed raster:
          downloadButton("downloadRaster", "Save processed raster to TIF"
          )
        ),

        box(
          width=NULL,

          # optionally limit spectral range:
          checkboxInput("checkbox01",
                        label = "Plot: clip data to 470-830 nm",
                        value = TRUE)
        )
      )
    ),

    # ROW 2:
    fluidRow(

      # graph plotting area:
      tabBox(
        title = "VIS-NIR reflectance",
        tabPanel('Selected values', plotOutput("TSplot")),
        tabPanel('All saved values', plotOutput("combiPlot"))
      ),

      column(
        width = 6,

        # controls to choose, label, add and save selected spectrum:
        box(
          width = NULL,
          title = 'Save selected values to table',
          selectInput('selectedValues', 'Select Values:',
                      choices = c('Mean',
                                  'Standard Deviation')),
          textInput('colName', 'Column Name:'),
          actionButton('addValues', 'Add Values', icon=icon('plus')),
          downloadButton("downloadData", "Save table to CSV")
        ),

        # table plotting area:
        box(
          width=NULL,
          DT::dataTableOutput('table')
        )
      )
    )

  )
)


# set maximum fileInput size to 500 MB:
options(shiny.maxRequestSize=500*1024^2)


# ------------------------
# Define server logic ----
# ------------------------

server <- function(input, output) {

  # define stop button:
  observeEvent(input$close, {
    js$closeWindow()
    stopApp()
  })

  # define spatial filter fuction:
  multiFocal <- function(x, w) {
    if(is.character(x)) {
      x <- brick(x)
    }

    # The function to be applied to each individual layer
    if(input$selectedSpatialFilter=="mean"){
      # MEAN:
      vx <- velox(x)
      vx2 <- vx$copy()
      vx2$meanFocal(weights=matrix(1,
                                   as.numeric(input$sliderMED),
                                   as.numeric(input$sliderMED)),
                    bands=c(1:41))
      r <- vx2$as.RasterStack()
      return(r)
    } else {

      # MEDIAN:
      vx <- velox(x)
      vx2 <- vx$copy()
      vx2$medianFocal(wrow=as.numeric(input$sliderMED),
                      wcol=as.numeric(input$sliderMED),
                      bands=c(1:41))
      r <- vx2$as.RasterStack()
      return(r)
    }
  }

  # load raster from file and rotate:
  imgInput <- reactive({
    if (nlayers(stack(input$file$datapath)) != 41) {
      stop('input raster must have 41 bands.')
    } else {
      if (input$sliderROT == 0) {
        cl <- stack(input$file$datapath)
      }
      if (input$sliderROT == 90) {
        cl <- t(flip(stack(input$file$datapath), 2))
      }
      if (input$sliderROT == 180) {
        cl <- flip(flip(stack(input$file$datapath),1),2)
      }
      if (input$sliderROT == 270) {
        cl <- t(flip(stack(input$file$datapath),1))
      }
    }

    # and apply spatial filter if selected:
    if (input$checkboxSpatialFilter) {
      multiFocal(cl)
    } else cl
  })

  #transform to velox for faster processing:
  veloxInput <- reactive({
    if (input$checkboxSG) {

      # Savitzky-Golay-Filter:
      cl <- imgInput()
      m <- as.matrix(values(cl))
      wl <- seq(450,850,10)

      # create speclib object:
      spectral_data <- speclib(m, wl)
      idSpeclib(spectral_data) <- as.character(wl)

      # filter:
      SG <- smoothSpeclib(spectral_data, method = "sgolay", p = 2, n = as.numeric(input$sliderSG))

      # as raster:
      imgSG <- cl
      values(imgSG) <- spectra(SG)
      velox::velox(imgSG)
    } else velox::velox(imgInput())
  })

  # create raster plot:
  output$rast <- renderPlot({
    cl <- imgInput()
    plotRGB(cl, r=22,g=13,b=6,scale=65536, stretch='hist', asp=1)
  })

  # get extent of selected region:
  Coords <- reactive({
    req(input$rasPlot_brush$xmin)

    c(input$rasPlot_brush$xmin, input$rasPlot_brush$xmax,
      input$rasPlot_brush$ymin, input$rasPlot_brush$ymax)
  })

  # get mean reflectance of selected region:
  valueMean  <- eventReactive(input$rasPlot_brush$xmin,{
    vl <- veloxInput()
    as.vector(vl$extract(as(raster::extent(Coords()), 'SpatialPolygons') ,fun=mean))
  })

  # get sd reflection of selected region:
  valueSD  <- eventReactive(input$rasPlot_brush$xmin,{
    vl <- veloxInput()
    as.vector(vl$extract(as(raster::extent(Coords()), 'SpatialPolygons') ,fun=sd))
  })

  # label spectral data:
  waveL <- eventReactive(input$rasPlot_brush$xmin,{
    w <- seq(450,850,10)
    w
  })

  # create and call graph plot:
  output$TSplot <- renderPlot(res = 100, {
    if(valueMean()>1){
      df <- data.frame(
        m = valueMean()/65536,
        ms = valueSD()/65536,
        wl = waveL()
      )
    } else  df <- data.frame(
      m = valueMean(),
      ms = valueSD(),
      wl = waveL()
    )

    # optionally limit plot data:
    if (input$checkbox01) {
      df <- df[3:39,]
    }

    # call:
    ggplot(data=df, aes(x=wl,y=m))+
      geom_line(col='black', lwd=1.5) +
      geom_line(aes(x=wl, y=m+ms), linetype='dashed', color='gray50') +
      geom_line(aes(x=wl, y=m-ms), linetype='dashed', color='gray50') +
      xlab('wavelength [nm]') +
      ylab('reflectance [0,1]') +
      ggtitle('Mean and SD of the region selected')+
      theme_classic() +
      theme(panel.grid.major = element_line(colour = 'grey90'),
            panel.grid.minor = element_line(colour = 'grey90', linetype = 2))

  })

  # create and call a plot of datatable:
  output$combiPlot <- renderPlot(res=100, {
    df <- storedData$df
    if(any(grepl('_sd', colnames(df)))){
      df <- df[, -grep("_sd", colnames(df))]
    }
    df <- reshape2::melt(df, id='wl')

    # call:
    ggplot(data=df, aes(x=wl,y=value,col=variable))+
      geom_line() +
      xlab('wavelength [nm]') +
      ylab('reflectance [0,1]') +
      theme_classic() +
      theme(panel.grid.major = element_line(colour = 'grey90'),
            panel.grid.minor = element_line(colour = 'grey90', linetype = 2),
            legend.title = element_blank())

  })


  # mean of selection:
  selectedMean <- reactive({
    tmp <- valueMean()
    if (tmp>1) {
      data.frame(
        m = tmp[3:39]/65536
      )
    } else  data.frame(
      m = tmp[3:39]
    )
  })

  # sd of selection:
  selectedSD <- reactive({
    tmp <- valueSD()
    if (tmp>1) {
      data.frame(
        m = tmp[3:39]/65536
      )
    } else data.frame(
      m = tmp[3:39]
    )
  })

  # create base table:
  storedData <- reactiveValues(
    df = data.frame(
      wl = seq(470,830,10)
    )
  )

  # add selected values to table:
  observeEvent(input$addValues, {
    if (input$selectedValues == 'Mean') {
      newData <- selectedMean()
    } else newData <- selectedSD()
    storedData$df$newData <- round(newData[,1],5)

    # and label them:
    if (input$selectedValues == 'Mean') {
      names(storedData$df)[names(storedData$df) == 'newData'] <- paste(input$colName,'_mean', sep='')
    } else names(storedData$df)[names(storedData$df) == 'newData'] <- paste(input$colName,'_sd', sep='')
  })

  # plot table:
  output$table <- DT::renderDataTable({
    df <- storedData$df
    datatable(df,
              extensions = c("FixedColumns", "FixedHeader", 'Scroller'),
              options = list(scroller=TRUE,
                             scrollX=TRUE,
                             rownames=FALSE,
                             searching =FALSE,
                             autoWidth=TRUE,
                             scrollY = "400px",
                             fixedHeader = TRUE))
  })

  # download table:
  output$downloadData <- downloadHandler(
    filename = 'selected_spectral_data.csv',
    content = function(file) {
      write.csv(storedData$df, file, row.names = FALSE)}
  )

  # download raster:
  output$downloadRaster <- downloadHandler(
    filename = 'processed_raster_data.tif',
    content = function(file) {
      raster::writeRaster(veloxInput()$as.RasterStack(), file)
    }
  )

}

# ------------------------
# Run the app ------------
# ------------------------

shinyApp(ui = ui, server = server)


# ------------------------
# References -------------
# ------------------------
# citation('shiny')
# Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan
# McPherson (2018). shiny: Web Application Framework for R. R
# package version 1.1.0. https://CRAN.R-project.org/package=shiny
#
# citation('shinydashboard')
# Winston Chang and Barbara Borges Ribeiro (2018). shinydashboard:
# Create Dashboards with 'Shiny'. R package version 0.7.1.
# https://CRAN.R-project.org/package=shinydashboard
#
# citation('shinyjs')
# Dean Attali (2018). shinyjs: Easily Improve the User Experience
# of Your Shiny Apps in Seconds. R package version 1.0.
# https://CRAN.R-project.org/package=shinyjs
#
# citation('shinyWidgets')
# Victor Perrier, Fanny Meyer and David Granjon (2019).
# shinyWidgets: Custom Inputs Widgets for Shiny. R package version
# 0.4.8. https://CRAN.R-project.org/package=shinyWidgets
#
# citation('DT')
# Yihui Xie, Joe Cheng and Xianying Tan (2019). DT: A Wrapper of
# the JavaScript Library 'DataTables'. R package version 0.6.
# https://CRAN.R-project.org/package=DT

# citation('raster')
# Robert J. Hijmans (2019). raster: Geographic Data Analysis and
# Modeling. R package version 2.9-23.
# https://CRAN.R-project.org/package=raster
#
# citation('velox')
#Philipp Hunziker (2018). velox: Fast Raster Manipulation and
# Extraction. R package version 0.2.0.9002.
#
# citation('ggplot2')
# H. Wickham. ggplot2: Elegant Graphics for Data Analysis.
# Springer-Verlag New York, 2016.
#
# citation('hsdar')
# Lukas W. Lehnert, Hanna Meyer, Joerg Bendix (2018). hsdar:
# Manage, analyse and simulate hyperspectral data in R. R package
# version 0.7.1.

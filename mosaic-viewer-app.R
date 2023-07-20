
library(shiny)
library(shinyFiles)
library(DT)
library(data.table)
library(leaflet)
library(tidyverse)
library(sf)
library(raster)
library(terra)

ui <- fluidPage(
  titlePanel("Harvesting Data Entry"),
  
  sidebarLayout(
    sidebarPanel(
      textOutput("objectIdoutput"),
      textOutput("start_date"),
      textOutput("end_date"),
      
      # data entry boxes
      numericInput("id", label = "Object_ID", min = 1, value = 1),
      selectizeInput("select", label = "Was this site harvested?", choices = c("No", "Yes", "NA")),
      numericInput("percent", label = "What percent was harvested?", min = 0, max = 100, value = 0),
      
      # data entry & saving buttons
      actionButton("submit", "Submit"),
      
      # allow user to go back and forth
      actionButton("prev", "Previous Plot"),
      actionButton("nextBtn", "Next Plot"),
      
      # allow user to jump to specific plots
      numericInput("jumpid", label = "Enter ObjectID to jump to associated plot:", 
                   min = 1, value = 1),
      actionButton("jump", label = "View"),
      
      # display data table
      DTOutput("table", width = 550),
      span(htmlOutput("text"), style = "color:red"),
      
      # saving data input
      shinySaveButton("save", "Save as CSV",
                      title = "Enter your file name:",
                      filetype = list(csv = "csv"))
    ),
    
    mainPanel(
      uiOutput("plots")
    )
  )
)

server <- function(input, output, session){
  
  # set coordinate system
  epsg <- 3005
  
  # set row as a reactive value
  currentRow <- reactiveVal(1)
  
  # render table editing instructions
  output$text <- renderText({
    paste("<B>Double-click any cell on the table to enable editing.</B>")
  })
  
  # modal dialog for choosing shapefile in local directory
  volumes = getVolumes()
  shp_paths <- modalDialog(
    title = "Choose shapefile",
    shinyFilesButton("Btn_GetFile", "Choose a file",
                     title = "Please select a file:", multiple = FALSE,
                     buttonType = "default", class = NULL),
    textOutput("shpfile"),
    span(htmlOutput("error"), style = "color:red"),
    footer = tagList(
      actionButton("submit_shp", "Submit"))
  )
  
  # prompt user to choose shapefile upon launching the app
  showModal(shp_paths)
  
  observe({
    shinyFileChoose(input, "Btn_GetFile", roots = volumes, session = session)
    req(input$Btn_GetFile)
    
    # set shapefile path
    if(!is.null(input$Btn_GetFile)){
      file_selected <- parseFilePaths(volumes, input$Btn_GetFile)
      shapefile_path <- as.character(file_selected$datapath)
      
      if(length(shapefile_path) > 0){
        file_form <- tools::file_ext(shapefile_path)
        
        if(file_form == "shp"){
          
          output$error <- renderText({
            "Click the Submit button to proceed."
          })
          
          output$shpfile <- renderText({
            paste("Selected file path:", shapefile_path)
          })
          
          # extract shapefile table
          shp <- vect(shapefile_path)
          shp_data <- as.data.frame(shp)
          shp_data$Obj_ID <- 1:nrow(shp_data)
          
          # select file type for mosaic (GeoTiff or WMS/WMTS server)
          choose_file_type <- modalDialog(
            title = "Choose file type",
            selectInput("file_type",
                        label = "Choose your mosaic file type",
                        choices = c("GeoTiff", "WMS/WMTS server"),
                        selected = NULL,
                        width = "100%"),
            footer = tagList(
              actionButton("submit_type", "Submit"),
              actionButton("back_shp", "Go Back")
            )
          )
          
          observeEvent(input$submit_shp, {
            showModal(
              choose_file_type
            )
          })
          
          observeEvent(input$back_shp, {
            showModal(shp_paths)
          })
          
          #------------------------------------------------------------#
          observeEvent(input$submit_type, {
            req(input$file_type)
            
            # interface navigation/display/saving functions
            #------------------------------# 
            
            # Render current Obj_ID
            output$objectIdoutput <- renderText({
              paste("Current ObjectID: ", shp_data$Obj_ID[currentRow()])
            })
            
            # Render disturbance start and end date    
            output$start_date <- renderText({
              paste("Disturbance start: ", shp_data$t1[currentRow()])
            })
            
            output$end_date <- renderText({
              paste("Disturbance end: ", shp_data$t2[currentRow()])
            })
            
            # view previous plot
            observeEvent(input$prev, {
              currentRow(max(1, currentRow() - 1))
              updateNumericInput(session, "percent", value = 0)
              
              updateNumericInput(session, "id",
                                 value = shp_data$Obj_ID[currentRow()],
                                 min = 1, max = length(shp_data))
            })
            
            # view next plot
            observeEvent(input$nextBtn, {
              currentRow(min(nrow(shp_data), currentRow() + 1))
              updateNumericInput(session, "percent", value = 0)
              
              updateNumericInput(session, "id",
                                 value = shp_data$Obj_ID[currentRow()],
                                 min = 1, max = length(shp_data))
            })
            
            # Jump to specific plot
            observeEvent(input$jump, {
              currentRow(input$jumpid)
              updateNumericInput(session, "percent", value = 0)
              
              updateNumericInput(session, "id",
                                 value = shp_data$Obj_ID[currentRow()],
                                 min = 1, max = length(shp_data))
            })
            
            # Render table
            
            # Create a reactive data table
            values <- reactiveValues(
              table = NULL,
              editable = FALSE
            )
            
            # Set data table default
            values$table <- data.table(object_id = character(),
                                       harvest_record = character(),
                                       percent_harvested = character())
            
            # Update data table when harvest data is submitted
            observeEvent(input$submit, {
              new_row <- data.table(object_id = input$id,
                                    harvest_record = input$select,
                                    percent_harvested = input$percent)
              
              # in case a plot's data is resubmitted
              row_index <- which(values$table$object_id == new_row$object_id)
              
              if (length(row_index) > 0){
                values$table[row_index, ] <- new_row
              } else {
                values$table <- rbind(values$table, new_row)
              }
              
              currentRow(min(nrow(shp_data), currentRow() + 1))
              
              updateNumericInput(session, "percent", value = 0)
              updateNumericInput(session, "id",
                                 value = shp_data$Obj_ID[currentRow()],
                                 min = 1, max = length(shp_data))
              updateSelectizeInput(session, "select",
                                   selected = "No")
              
              # Check if all rows are inputted
              if (currentRow() > nrow(shp_data)) {
                # Disable input fields
                disable("submit")
                disable("percent")
                disable("select")
                disable("id")
              }
            })
            
            # Render the data table
            output$table <- renderDT({
              datatable(values$table, 
                        editable = TRUE)}
            )
            
            # Update the data table when changes are made
            observeEvent(input$table_cell_edit, {
              info <- input$table_cell_edit
              row <- info$row
              col <- info$col
              value <- info$value
              
              # Update the corresponding cell in the data frame
              values$table[row, col] <- value
            })
            
            # save user input modal dialogue
            
            # show file naming modal when "Save as CSV" button is clicked
            observe({
              shinyFileSave(input, "save", roots = volumes, session = session)
              
              file_save <- parseSavePath(volumes, input$save)
              save_path <- as.character(file_save$datapath)
              
              if(length(save_path) > 0){
                write.csv(values$table, file = save_path, row.names = FALSE)
                showModal(modalDialog(
                  "The file has been saved successfully!",
                  footer = tagList(
                    modalButton("OK")
                  )
                ))
              }
            })
            
            #-------------------------------------------------#
            
            # set mosaic display according to selected file type
            
            if(!is.null(input$file_type)) {
              
              #-----------------------------------------------#
              if(input$file_type == "GeoTiff") {
                
                tif_select <- modalDialog(
                  title = "Mosaic display",
                  selectInput("tif_number",
                              label = "Choose the number of Geotiff mosaics to display:",
                              choices = c("1", "2", "3", "4"),
                              selected = NULL,
                              width = "100%"),
                  footer = tagList(
                    actionButton("submit_tif", "Submit")
                  )
                )
                
                showModal(tif_select)
                
                # prompt user to enter geotiff information depending on the number selected
                
                observeEvent(input$submit_tif, {
                  req(input$tif_number)
                  if(!is.null(input$tif_number)) {
                    
                    if(input$tif_number == "1"){
                      showModal(
                        modalDialog(
                          title = "Enter Geotiff file directory",
                          textInput("tif1", "Enter the path to the Geotiff (.TIF) file here",
                                    width = 800),
                          textInput("tif1_layer", "Enter the layer name if this Geotiff"),
                          
                          footer = tagList(
                            actionButton("tif1_submit", "Submit"),
                            actionButton("back1", "Go Back")
                          )
                        )
                      )
                    } else if(input$tif_number == "2"){
                      showModal(
                        modalDialog(
                          title = "Enter Geotiff file directory",
                          textInput("tif2a", "Enter the path to the Geotiff (.TIF) file here",
                                    width = 800),
                          textInput("tif2a_layer", "Enter the layer name if this Geotiff"),
                          textInput("tif2b", "Enter the path to the Geotiff (.TIF) file here",
                                    width = 800),
                          textInput("tif2b_layer", "Enter the layer name if this Geotiff"),
                          footer = tagList(
                            actionButton("tif2_submit", "Submit"),
                            actionButton("back1", "Go Back")
                          )
                        )
                      )
                    } else if(input$tif_number == "3"){
                      showModal(
                        modalDialog(
                          title = "Enter Geotiff file directory",
                          textInput("tif3a", "Enter the path to the Geotiff (.TIF) file here",
                                    width = 800),
                          textInput("tif3a_layer", "Enter the layer name if this Geotiff"),
                          textInput("tif3b", "Enter the path to the Geotiff (.TIF) file here",
                                    width = 800),
                          textInput("tif3b_layer", "Enter the layer name if this Geotiff"),
                          textInput("tif3c", "Enter the path to the Geotiff (.TIF) file here",
                                    width = 800),
                          textInput("tif3c_layer", "Enter the layer name if this Geotiff"),
                          footer = tagList(
                            actionButton("tif3_submit", "Submit"),
                            actionButton("back1", "Go Back")
                          )
                        )
                      )
                    } else {
                      showModal(
                        modalDialog(
                          title = "Enter Geotiff file directory",
                          textInput("tif4a", "Enter the path to the Geotiff (.TIF) file here",
                                    width = 800),
                          textInput("tif4a_layer", "Enter the layer name if this Geotiff"),
                          textInput("tif4b", "Enter the path to the Geotiff (.TIF) file here",
                                    width = 800),
                          textInput("tif4b_layer", "Enter the layer name if this Geotiff"),
                          textInput("tif4c", "Enter the path to the Geotiff (.TIF) file here",
                                    width = 800),
                          textInput("tif4c_layer", "Enter the layer name if this Geotiff"),
                          textInput("tif4d", "Enter the path to the Geotiff (.TIF) file here",
                                    width = 800),
                          textInput("tif4d_layer", "Enter the layer name if this Geotiff"),
                          footer = tagList(
                            actionButton("tif4_submit", "Submit"),
                            actionButton("back1", "Go Back")
                          )
                        )
                      )
                    }
                  }
                })
                
                # allow user to return to tif selection page
                observeEvent(input$back1, {
                  showModal(tif_select)
                })
                
                # load shapefile
                shapefile1 <- as(sf::read_sf(shapefile_path), "Spatial")
                shapefile1$Obj_ID <- 1:nrow(shapefile1)
                
                # create coordinate system
                tif_coords <- data.frame(Obj_ID = 1:nrow(shapefile1))
                tif_coords$area_ha <- shapefile1$area_ha
                
                for (i in 1:nrow(shapefile1)){
                  tif_coords$xmin[i] <- ext(shapefile1[i, ])[1]
                  tif_coords$xmax[i] <- ext(shapefile1[i, ])[2]
                  tif_coords$ymin[i] <- ext(shapefile1[i, ])[3]
                  tif_coords$ymax[i] <- ext(shapefile1[i, ])[4]
                }
                
                # set dynamic zoom
                tif_coords$zoom <- ifelse(tif_coords$area_ha > 40, 2,
                                          ifelse(dplyr::between(tif_coords$area_ha, 17, 40), 4,
                                                 ifelse(between(tif_coords$area_ha, 10, 17), 7,
                                                        ifelse(between(tif_coords$area_ha, 6, 10), 8,
                                                               ifelse(between(tif_coords$area_ha, 3, 6), 11, 18)))))
                
                # render geotiff function
                render_tif <- function(tif_path, layer){
                  renderPlot({
                    tif_brick <- raster::brick(tif_path)
                    
                    a <- (tif_coords$xmax[currentRow()] - tif_coords$xmin[currentRow()])
                    b <- (tif_coords$ymax[currentRow()] - tif_coords$ymin[currentRow()])
                    
                    shape_length <- ifelse(a > b, a, b)
                    
                    border <- (extent(tif_brick)[2] - extent(tif_brick)[1])/shape_length/tif_coords$zoom[currentRow()]
                    
                    bb <- raster::extent(tif_coords$xmin[currentRow()] - border,
                                         tif_coords$xmax[currentRow()] + border,
                                         tif_coords$ymin[currentRow()] - border,
                                         tif_coords$ymax[currentRow()] + border)
                    raster::plotRGB(raster::crop(tif_brick, bb))
                    plot(shapefile1[currentRow(), ], border = "red", lwd = 3, add = TRUE)
                    legend("topright", legend = as.character(layer))
                  })
                }
                
                # plot 1 geotiff
                observeEvent(input$tif1_submit, {
                  observe({
                    if(!is.null(input$tif1)){
                      
                      output$plot1 <- render_tif(as.character(input$tif1), input$tif1_layer)
                      
                      output$plots <- renderUI({
                        tagList(
                          fluidRow(column(6, plotOutput("plot1")))
                        )
                      })
                    }
                  })
                  removeModal()
                })
                
                # plot 2 geotiffs
                observeEvent(input$tif2_submit, {
                  observe({
                    if(!is.null(input$tif2a) & !is.null(input$tif2b)){
                      
                      output$plot1 <- render_tif(as.character(input$tif2a), input$tif2a_layer)
                      output$plot2 <- render_tif(as.character(input$tif2b), input$tif2b_layer)
                      
                      output$plots <- renderUI({
                        tagList(
                          fluidRow(column(6, plotOutput("plot1"))),
                          fluidRow(column(6, plotOutput("plot2")))
                        )
                      })
                    }
                  })
                  removeModal()
                })
                
                # plot 3 geotiffs
                observeEvent(input$tif3_submit, {
                  observe({
                    if(!is.null(input$tif3a) & !is.null(input$tif3b) & !is.null(input$tif3c)){
                      
                      output$plot1 <- render_tif(as.character(input$tif3a), input$tif3a_layer)
                      output$plot2 <- render_tif(as.character(input$tif3b), input$tif3b_layer)
                      output$plot3 <- render_tif(as.character(input$tif3c), input$tif3c_layer)
                      
                      output$plots <- renderUI({
                        tagList(
                          fluidRow(column(6, plotOutput("plot1")),
                                   column(6, plotOutput("plot2"))),
                          fluidRow(column(6, plotOutput("plot3")))
                        )
                      })
                    }
                  })
                  removeModal()
                })
                
                # plot 4 geotiffs
                observeEvent(input$tif4_submit, {
                  observe({
                    if(!is.null(input$tif4a) & !is.null(input$tif4b) & !is.null(input$tif4c) &
                       !is.null(input$tif4d)){
                      
                      output$plot1 <- render_tif(as.character(input$tif4a), input$tif4a_layer)
                      output$plot2 <- render_tif(as.character(input$tif4b), input$tif4b_layer)
                      output$plot3 <- render_tif(as.character(input$tif4c), input$tif4c_layer)
                      output$plot4 <- render_tif(as.character(input$tif4d), input$tif4d_layer)
                      
                      output$plots <- renderUI({
                        tagList(
                          fluidRow(column(6, plotOutput("plot1")),
                                   column(6, plotOutput("plot2"))),
                          fluidRow(column(6, plotOutput("plot3")),
                                   column(6, plotOutput("plot4")))
                        )
                      })
                    }
                  })
                  removeModal()
                })
                
                #-----------------------------------------------#
              } else if (input$file_type == "WMS/WMTS server"){
                
                # load WMS/WMTS server
                # create shapefile
                shapefile1 <- read_sf(shapefile_path)
                shapefile1 <- st_make_valid(shapefile1)
                
                # set Obj_ID
                shapefile1$Obj_ID <- 1:nrow(shapefile1)
                
                shapefile1 <- shapefile1 %>% st_set_crs(epsg)
                shapefile1 <- st_transform(shapefile1, crs = "+proj=longlat +datum=WGS84")
                
                # center shapefile coordinates for display
                shapefile_coord <- st_centroid(shapefile1)
                shapefile_coord$lat <- unlist(map(shapefile_coord$geometry, 2))
                shapefile_coord$long <- unlist(map(shapefile_coord$geometry, 1))
                
                # assign dynamic zoom values
                ## area_ha = 170+ : zoom = 13
                ## area_ha = 170-35 : zoom = 14
                ## area_ha = 10-35 : zoom = 15
                ## area_ha = <10 : zoom = 16
                
                shapefile_coord$zoom <- ifelse(shapefile_coord$area_ha > 170, 13,
                                               ifelse(between(shapefile_coord$area_ha, 35, 170), 14,
                                                      ifelse(between(shapefile_coord$area_ha, 10, 35), 15,
                                                             16)))
                
                # prompt user to choose number of mosaics to display
                
                mosaic_select <- modalDialog(
                  title = "Mosaic display",
                  selectInput("mosaic_number",
                              label = "Choose the number of mosaics to display:",
                              choices = c("1", "2", "3", "4"),
                              selected = NULL,
                              width = "100%"),
                  selectInput("wms_link",
                              label = "Do all mosaics share the same WMS/WMTS server link?",
                              choices = c("Yes", "No"),
                              selected = NULL,
                              width = "100%"),
                  footer = tagList(
                    actionButton("submit_number", "Submit")
                  )
                )
                
                showModal(mosaic_select)
                
                # prompt user to enter WMS/WMTS server information depending on the number selected
                observeEvent(input$submit_number, {
                  req(input$mosaic_number)
                  req(input$wms_link)
                  
                  if(!is.null(input$mosaic_number) & !is.null(input$wms_link)){
                    
                    if(input$wms_link == "Yes"){
                      
                      # mosaics share the same server
                      
                      if(input$mosaic_number == "1") {
                        showModal(
                          modalDialog(
                            title = "Enter WMS/WMTS server link",
                            textInput("input1", "Enter the WMS/WMTS server link here.",
                                      width = 800),
                            textInput("layer1", "Enter the layer name."),
                            
                            footer = tagList(
                              actionButton("input1_submit", "Submit"),
                              actionButton("back", "Go Back")
                            )
                          )
                        )
                      } else if(input$mosaic_number == "2") {
                        showModal(
                          modalDialog(
                            title = "Enter WMS/WMTS server link",
                            
                            textInput("input2a", "Enter the WMS/WMTS server link here.",
                                      width = 800),
                            textInput("layer2a", "Enter the first layer name."),
                            textInput("layer2b", "Enter the second layer name."),
                            
                            footer = tagList(
                              actionButton("input2_submit", "Submit"),
                              actionButton("back", "Go Back")
                            )
                          )
                        )
                      } else if(input$mosaic_number == "3"){
                        showModal(
                          modalDialog(
                            title = "Enter WMS/WMTS server link",
                            
                            textInput("input3a", "Enter the first WMS/WMTS server link here.",
                                      width = 800),
                            textInput("layer3a", "Enter the first layer name."),
                            textInput("layer3b", "Enter the second layer name."),
                            textInput("layer3c", "Enter the third layer name."),
                            
                            footer = tagList(
                              actionButton("input3_submit", "Submit"),
                              actionButton("back", "Go Back")
                            )
                          )
                        )
                      } else {
                        showModal(
                          modalDialog(
                            title = "Enter WMS/WMTS server link",
                            
                            textInput("input4a", "Enter the first WMS/WMTS server link here.",
                                      width = 800),
                            textInput("layer4a", "Enter the first layer name."),
                            textInput("layer4b", "Enter the second layer name."),
                            textInput("layer4c", "Enter the third layer name."),
                            textInput("layer4d", "Enter the fourth layer name."),
                            
                            footer = tagList(
                              actionButton("input4_submit", "Submit"),
                              actionButton("back", "Go Back")
                            )
                          )
                        )
                      }
                      
                    } else {
                      
                      # mosaics don't share the same server
                      
                      if(input$mosaic_number == "1") {
                        showModal(
                          modalDialog(
                            title = "Enter WMS/WMTS server link",
                            textInput("input1", "Enter the WMS/WMTS server link here.",
                                      width = 800),
                            textInput("layer1", "Enter the layer name of this mosaic."),
                            
                            footer = tagList(
                              actionButton("input1_submit", "Submit"),
                              actionButton("back", "Go Back")
                            )
                          )
                        )
                      } else if(input$mosaic_number == "2") {
                        showModal(
                          modalDialog(
                            title = "Enter WMS/WMTS server link",
                            
                            textInput("input2a", "Enter the first WMS/WMTS server link here.",
                                      width = 800),
                            textInput("layer2a", "Enter the layer name of this mosaic."),
                            
                            textInput("input2b", "Enter the second WMS/WMTS server link here.",
                                      width = 800),
                            textInput("layer2b", "Enter the layer name of this mosaic."),
                            
                            footer = tagList(
                              actionButton("input2_submit", "Submit"),
                              actionButton("back", "Go Back")
                            )
                          )
                        )
                      } else if(input$mosaic_number == "3"){
                        showModal(
                          modalDialog(
                            title = "Enter WMS/WMTS server link",
                            
                            textInput("input3a", "Enter the first WMS/WMTS server link here.",
                                      width = 800),
                            textInput("layer3a", "Enter the layer name of this mosaic."),
                            
                            textInput("input3b", "Enter the second WMS/WMTS server link here.",
                                      width = 800),
                            textInput("layer3b", "Enter the layer name of this mosaic."),
                            
                            textInput("input3c", "Enter the third WMS/WMTS server link here.",
                                      width = 800),
                            textInput("layer3c", "Enter the layer name of this mosaic."),
                            
                            footer = tagList(
                              actionButton("input3_submit", "Submit"),
                              actionButton("back", "Go Back")
                            )
                          )
                        )
                      } else {
                        showModal(
                          modalDialog(
                            title = "Enter WMS/WMTS server link",
                            
                            textInput("input4a", "Enter the first WMS/WMTS server link here.",
                                      width = 800),
                            textInput("layer4a", "Enter the layer name of this mosaic."),
                            
                            textInput("input4b", "Enter the second WMS/WMTS server link here.",
                                      width = 800),
                            textInput("layer4b", "Enter the layer name of this mosaic."),
                            
                            textInput("input4c", "Enter the third WMS/WMTS server link here.",
                                      width = 800),
                            textInput("layer4c", "Enter the layer name of this mosaic."),
                            
                            textInput("input4d", "Enter the fourth WMS/WMTS server link here.",
                                      width = 800),
                            textInput("layer4d", "Enter the layer name of this mosaic."),
                            
                            footer = tagList(
                              actionButton("input4_submit", "Submit"),
                              actionButton("back", "Go Back")
                            )
                          )
                        )
                      }
                    }
                  }
                })
                
                # allow user to return to mosaic selection page
                observeEvent(input$back, {
                  showModal(mosaic_select)
                })
                
                # render map function
                render_map <- function(wmts, layer){
                  renderLeaflet({
                    leaflet() %>%
                      setView(lat = shapefile_coord$lat[currentRow()], 
                              lng = shapefile_coord$long[currentRow()], 
                              zoom = shapefile_coord$zoom[currentRow()]) %>%
                      addTiles(group = "OSM") %>%
                      addWMSTiles(
                        wmts,
                        layers = layer,
                        options = WMSTileOptions(format = "image/png", transparent = TRUE)) %>%
                      addPolygons(data = shapefile1[currentRow(), ], weight = 3, col = "red", 
                                  fillColor = "transparent", opacity = 1.0) %>%
                      addLegend(title = "Layer_Name", colors = "red", 
                                labels = layer)
                  })
                }
                
                # plot 1 mosaic
                observeEvent(input$input1_submit, {
                  observe({
                    if(!is.null(input$input1)){
                      output$map1 <- render_map(as.character(input$input1), input$layer1)
                      output$plots <- renderUI({
                        tagList(
                          fluidRow(column(6, leafletOutput("map1", width = 700, height = 450))
                          ))
                      })
                    }
                  })
                  removeModal()
                })
                
                # plot 2 mosaics
                observeEvent(input$input2_submit, {
                  observe({
                    if(!is.null(input$input2a)){
                      
                      output$map1 <- render_map(as.character(input$input2a), input$layer2a)
                      
                      if(input$wms_link == "Yes"){
                        output$map2 <- render_map(as.character(input$input2a), input$layer2b)
                        
                      } else if (input$wms_link == "No" & !is.null(input$input2b)){
                        output$map2 <- render_map(as.character(input$input2b), input$layer2b)
                      }
                      
                      output$plots <- renderUI({
                        tagList(
                          fluidRow(column(6, leafletOutput("map1", width = 700, height = 450))
                          ),
                          fluidRow(column(6, leafletOutput("map2", width = 700, height = 450))
                          )
                        )
                      })
                    }
                  })
                  removeModal()
                })
                
                # plot 3 mosaics
                observeEvent(input$input3_submit, {
                  observe({
                    if(!is.null(input$input3a)){
                      
                      output$map1 <- render_map(as.character(input$input3a), input$layer3a)
                      
                      if(input$wms_link == "Yes"){
                        output$map2 <- render_map(as.character(input$input3a), input$layer3b)
                        output$map3 <- render_map(as.character(input$input3a), input$layer3c)
                        
                      } else if (input$wms_link == "No" & !is.null(input$input3b) & !is.null(input$input3c)){
                        output$map2 <- render_map(as.character(input$input3b), input$layer3b)
                        output$map3 <- render_map(as.character(input$input3c), input$layer3c)
                      }
                      
                      tagList(
                        fluidRow(column(6, leafletOutput("map1", width = 600, height = 450)), 
                                 column(6, leafletOutput("map2", width = 600, height = 450))
                        ),
                        fluidRow(column(6, leafletOutput("map3", width = 600, height = 450))
                        )
                      )
                    } 
                  })
                  removeModal()
                })
                
                # plot 4 mosaics
                observeEvent(input$input4_submit, {
                  observe({
                    if(!is.null(input$input4a)){
                      
                      output$map1 <- render_map(as.character(input$input4a), input$layer4a)
                      
                      if(input$wms_link == "Yes"){
                        output$map2 <- render_map(as.character(input$input4a), input$layer4b)
                        output$map3 <- render_map(as.character(input$input4a), input$layer4c)
                        output$map4 <- render_map(as.character(input$input4a), input$layer4d)
                        
                      } else if(input$wms_link == "No" & !is.null(input$input4b) & !is.null(input$input4c) &
                                !is.null(input$input4d)){
                        output$map2 <- render_map(as.character(input$input4b), input$layer4b)
                        output$map3 <- render_map(as.character(input$input4c), input$layer4c)
                        output$map4 <- render_map(as.character(input$input4d), input$layer4d)
                      }
                      
                      output$plots <- renderUI({
                        
                        tagList(
                          fluidRow(column(6, leafletOutput("map1", width = 600, height = 450)), 
                                   column(6, leafletOutput("map2", width = 600, height = 450))
                          ),
                          fluidRow(column(6, leafletOutput("map3", width = 600, height = 450)),
                                   column(6, leafletOutput("map4", width = 600, height = 450))
                          )
                        )
                      })
                    }
                  })
                  removeModal()
                })
                
              }
            }
          })
          
        } else if (file_form != "shp"){
          
          # Display error message if the file selected is not a shapefile
          output$error <- renderText({
            paste0("ERROR: This file has the extension .", file_form, "! Please select a shapefile
           with the extension .shp.")
          })
          
          output$shpfile <- renderText({
            paste("Selected file path:", shapefile_path)
          })
        }
      }
    }
  })
}
# Run the application 
shinyApp(ui = ui, server = server) 










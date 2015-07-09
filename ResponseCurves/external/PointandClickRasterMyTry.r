#Matching Colin's colors
Col<-c("#000080","#000084","#000089"," #00008d"," #000092"," #000096"," #00009b"," #00009f"," #0000a4"," #0000a8"," #0000ad"," #0000b2"," #0000b6"," #0000bb"," #0000bf"," #0000c4"," #0000c8"," #0000cd"," #0000d1"," #0000d6"," #0000da"," #0000df"," #0000e3"," #0000e8"," #0000ed"," #0000f1"," #0000f6"," #0000fa"," #0000ff"," #0000ff"," #0000ff"," #0000ff"," #0000ff"," #0004ff"," #0008ff"," #000cff"," #0010ff"," #0014ff"," #0018ff"," #001cff"," #0020ff"," #0024ff"," #0028ff"," #002cff"," #0030ff"," #0034ff"," #0038ff"," #003cff"," #0040ff"," #0044ff"," #0048ff"," #004cff"," #0050ff"," #0054ff"," #0058ff"," #005cff"," #0060ff"," #0064ff"," #0068ff"," #006cff"," #0070ff"," #0074ff"," #0078ff"," #007cff"," #0080ff"," #0084ff"," #0088ff"," #008cff"," #0090ff"," #0094ff"," #0098ff"," #009cff"," #00a0ff"," #00a4ff"," #00a8ff"," #00acff"," #00b0ff"," #00b4ff"," #00b8ff"," #00bcff"," #00c0ff"," #00c4ff"," #00c8ff"," #00ccff"," #00d0ff"," #00d4ff"," #00d8ff"," #00dcfe"," #00e0fb"," #00e4f8"," #02e8f4"," #06ecf1"," #09f0ee"," #0cf4eb"," #0ff8e7"," #13fce4"," #16ffe1"," #19ffde"," #1cffdb"," #1fffd7"," #23ffd4"," #26ffd1"," #29ffce"," #2cffca"," #30ffc7"," #33ffc4"," #36ffc1"," #39ffbe"," #3cffba"," #40ffb7"," #43ffb4"," #46ffb1"," #49ffad"," #4dffaa"," #50ffa7"," #53ffa4"," #56ffa0"," #5aff9d"," #5dff9a"," #60ff97"," #63ff94"," #66ff90"," #6aff8d"," #6dff8a"," #70ff87"," #73ff83"," #77ff80"," #7aff7d"," #7dff7a"," #80ff77"," #83ff73"," #87ff70"," #8aff6d"," #8dff6a"," #90ff66"," #94ff63"," #97ff60"," #9aff5d"," #9dff5a"," #a0ff56"," #a4ff53"," #a7ff50"," #aaff4d"," #adff49"," #b1ff46"," #b4ff43"," #b7ff40"," #baff3c"," #beff39"," #c1ff36"," #c4ff33"," #c7ff30"," #caff2c"," #ceff29"," #d1ff26"," #d4ff23"," #d7ff1f"," #dbff1c"," #deff19"," #e1ff16"," #e4ff13"," #e7ff0f"," #ebff0c"," #eeff09"," #f1fc06"," #f4f802"," #f8f500"," #fbf100"," #feed00"," #ffea00"," #ffe600"," #ffe200"," #ffde00"," #ffdb00"," #ffd700"," #ffd300"," #ffd000"," #ffcc00"," #ffc800"," #ffc400"," #ffc100"," #ffbd00"," #ffb900"," #ffb600"," #ffb200"," #ffae00"," #ffab00"," #ffa700"," #ffa300"," #ff9f00"," #ff9c00"," #ff9800"," #ff9400"," #ff9100"," #ff8d00"," #ff8900"," #ff8600"," #ff8200"," #ff7e00"," #ff7a00"," #ff7700"," #ff7300"," #ff6f00"," #ff6c00"," #ff6800"," #ff6400"," #ff6000"," #ff5d00"," #ff5900"," #ff5500"," #ff5200"," #ff4e00"," #ff4a00"," #ff4700"," #ff4300"," #ff3f00"," #ff3b00"," #ff3800"," #ff3400"," #ff3000"," #ff2d00"," #ff2900"," #ff2500"," #ff2200"," #ff1e00"," #ff1a00"," #ff1600"," #ff1300"," #fa0f00"," #f60b00"," #f10800"," #ed0400"," #e80000"," #e40000"," #df0000"," #da0000"," #d60000"," #d10000"," #cd0000"," #c80000"," #c40000"," #bf0000"," #bb0000"," #b60000"," #b20000"," #ad0000"," #a80000"," #a40000"," #9f0000"," #9b0000"," #960000"," #920000"," #8d0000"," #890000"," #840000")
Col<-gsub(" ","",Col)

#reading in the map and converting it to a ggplot format
map<-raster("C:\\temp\\SAHM_workspace\\rf_7_prob_map.tif")
#convert the raster to points for plotting
map.p <- rasterToPoints(map)
#Make the points a dataframe for ggplot
ras <- data.frame(map.p)
#Make appropriate column headings
colnames(ras) <- c("Longitude", "Latitude", "MAP")

ui <- basicPage(
  plotOutput("map", click = "plot_click"),
  verbatimTextOutput("info")
)

server <- function(input, output) {
  output$map <- renderPlot({
  #Plot the Map
    ggplot(data=ras, aes(y=Latitude, x=Longitude)) +
      geom_raster(aes(fill=MAP)) +
      theme_bw() +
      coord_equal() +
      scale_fill_gradientn (
            colours=Col,
            values = c (seq(0,1,length.out=255)))+
      #scale_fill_gradient('MAP (mm/yr)", limits=c(0,2500)) +
      theme(axis.title.x = element_text(size=16),
      axis.title.y = element_text(size=16, angle=90),
      axis.text.x = element_text(size=14),
      axis.text.y = element_text(size=14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      legend.key = element_blank()
      )
  })

  output$info <- renderPrint({
    # With base graphics, need to tell it what the x and y variables are.
    nearPoints(ras, input$plot_click,threshold=500,addDist=TRUE,maxpoints=1)

    # nearPoints() also works with hover and dblclick events
  })
}

shinyApp(ui, server)
runApp("C:\\GoogleDrive\\Interactive\\Rcode\\Shiny\\Examples\\shiny-examples-master\\shiny-examples-master\\063-superzip-example")
server = function(input, output) {
  output$map_plot <- renderPlot({
    ## get index of needed frame
    index <- input$year_choice - 2005
    ## get the corrent map data
    nih_map_data <- genMap(input$dataframe)
    ## plot the map
    ggplot(data = nih_map_data) + 
      geom_polygon(aes(x=long, y = lat, 
                       fill = nih_map_data[[index]],
                       group=group),
                   color="white") +
      theme_bw()
  })
}
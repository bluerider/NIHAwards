server = function(input, output) {
  output$map_plot <- renderPlotly({
    ## get index of needed frame
    index <- input$year_choice - 2005
    ## get the corrent map data
    nih_map_data <- genMap(input$dataframe)
    distribution <- nih_map_data[[index]]
    ## plot the map
    ggplot(data = nih_map_data) + 
      geom_polygon(aes(x=long, y = lat, 
                       fill = distribution,
                       group=group),
                   color="white") +
      theme_bw() + scale_fill_gradient(low="blue", high="red")
  })
}
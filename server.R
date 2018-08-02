server = function(input, output) {
  output$map_plot <- renderPlot({
    nih_map_data <- fifty_states
    ggplot(data = nih_map_data) + 
      geom_polygon(aes(x=long, y = lat , fill = id,
                       group=group), 
                   color="white")
  })
}
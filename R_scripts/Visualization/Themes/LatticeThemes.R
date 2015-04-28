################################################################
### Custom themes for visualization with the lattice package ###
################################################################

### Themes are realized as functions.
### All of them are used in the same manner:
### one has to call them like this: theme <- theme_function(theme_name)
### then the function creates an object of the name theme
### and assigns a particular theme to it.
### For a catalogue of themes to choose see the function's body
### Any further customization to a theme may be either done directly on the object
### or by altering the code here and calling the function once again.

### !!! <--- Theme 1 ---> !!! ###
### Standard black-gray-white V4 Lab theme
V4themes <- function(theme_name) {
      ### Check the input data
      stopifnot(is.character(theme_name) & length(theme_name) == 1)
      choices <- c("standard_bgw")
      theme_name <- match.arg(theme_name, choices = choices)
      stopifnot(theme_name %in% choices)
      
      V4.theme <- trellis.par.get()
      ### Choose a theme
      if(theme_name == "standard_bgw") {
            ### Standard black-gray-white theme; very good for paper publications
            V4.theme$strip.background$col <- "gray"
            V4.theme$plot.polygon$col <- "gray3"
            V4.theme$plot.polygon$border <- "transparent"
            V4.theme$dot.symbol$col <- "gray3"
            V4.theme$plot.symbol$col <- "gray3"
            V4.theme$box.umbrella$col <- "gray3"
            V4.theme$box.rectangle$col <- "black"
      }
      return(V4.theme)
}
base_colour <- "#14509E"
library(showtext)
library(scales)

load_showtext_fonts()


showtext_auto()

theme_regacc_line <- theme_minimal (base_size = 18, 
                                    base_family = "roboto")+
  theme(plot.background = element_rect(fill= "white", 
                                       colour = base_colour),
        panel.background = element_rect(fill= "#FAFAFA", 
                                        colour = base_colour),
        panel.grid.major.y = element_line(colour=base_colour, 
                                          linetype = "dotted"),
        axis.line.x = element_line(colour=base_colour, 
                                   size =rel(1.2)),
        axis.line.y = element_line(colour= base_colour, 
                                   size= rel(1.2)),
        axis.text.x = element_text(colour=base_colour, 
                                   size =rel (1.1)),
        axis.text.y= element_text(colour= base_colour, 
                                  size= rel(1.1), 
                                  hjust=0),
        axis.ticks = element_line(size =rel(1.5),
                                  colour = base_colour),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = rel(0.9),
                                    colour=base_colour,
                                    angle = 0, 
                                    vjust = 1,
                                    face = "italic"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key = element_rect(fill= "white",
                                  colour = NA),
        legend.background = element_rect(fill="white", 
                                         colour = NA, 
                                         linetype = "solid"),
        legend.text = element_text(colour= base_colour),
        plot.title = element_text(face= "bold", 
                                  colour =base_colour, 
                                  hjust =0,
                                  size = rel(1.3), 
                                  margin = margin(10,0,10,0)),
        plot.subtitle = element_text(    colour =base_colour, 
                                         hjust =0,
                                         size = rel(1.1), 
                                         margin = margin(0,0,10,0)),
        plot.caption = element_text(    colour =base_colour, 
                                        hjust =1,
                                        size = rel(1.0)),
        plot.tag = element_text(    colour =base_colour, 
                                    hjust =0,
                                    size = rel(1.0)),
        strip.background = element_rect(fill = base_colour, 
                                        colour = NA),
        strip.text = element_text(face = "bold", 
                                  size = 18, 
                                  hjust = 0.2,
                                  colour = "white"),
        strip.placement = "outside",
        panel.spacing = unit(1, "lines")
  )



#### Palette functions

luis_colours <- c('theme2' = "#AF4B91",
                  'theme3' = "#E6A532",
                  'theme1' = "#466EB4",
                  'theme7' = "#961E2D",
                  'theme8' = "#41AFAA",
                  'theme5' = "#7DAF4B",
                  'theme6' = "#B93C46",
                  'theme9' = "#D7642D",
                  'theme4' = "#00A0E1",
                  'fucsia' = "#CC2299", 
                  'yellow' = "#EECC55",
                  'brown' = "#EECC99",
                  'blue' = "#0000FF",
                  'blue1' = "#11448B",
                  'red' = "#EE0000",
                  'red1' = "#881111",
                  'orange' = "#FF7700",
                  'grey' = "#CCCCCC",
                  'green' = "#668822")


#' Function to extract  colors as hex codes
#'
#' @param ... Character names of colors
#'
luis_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols)) {
    return(luis_colours)
  }
  
  luis_colours[cols]
}


luis_palettes <- list('eurostat' = luis_cols("theme2", "theme3", "theme1", "theme7", "theme8",
                                             "theme5", "theme6", "theme9", "theme4"),
                      'blues' = luis_cols("theme1","theme4","blue","blue1"),
                      'luis' = luis_cols("blue1", "red1", "orange", "green", "grey")
)


#' Return function to interpolate a  color palette
#'
#' @param palette Character name of palette in luis_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
luis_pal<- function (palette= "eurostat",
                     reverse = FALSE, ...) {
  pal <- luis_palettes[[palette]]
  if (reverse) pal <- rev (pal)
  colorRampPalette(pal, ...)
}

#' Color scale constructor for luis colors
#'
#' @param palette Character name of palette in luis_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_colour_luis <- function(palette = "eurostat", discrete = TRUE, reverse = FALSE, ...) {
  pal <- luis_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("luis_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for luis colors
#'
#' @param palette Character name of palette in luis_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_luis <- function(palette = "eurostat", discrete = TRUE, reverse = FALSE, ...) {
  pal <- luis_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("luis_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

# Export to excel
#https://twitter.com/brodriguesco/status/1447468259725434886
show_in_excel <- function(.data){
  tmp <- paste0(tempfile(), ".xlsx")
  openxlsx::write.xlsx(.data,tmp)
  browseURL(tmp)
}



# Function for rounding values in axis
integer_breaks <- function(n = 4, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}

# function for DT
show_table<- function(x){
  datatable(x,
            filter = "top", class = "stripe hover", extensions = "Buttons",
            options = list(  lengthMenu = list(c(20, -1), c("20", "All")),
                             pageLength = 20, dom = "Blfrtip", buttons = c("excel"))
  )}


base_colour <- "#14509E"
library(showtext)
library(scales)
## Loading Google fonts (https://fonts.google.com/)
font_add_google("Roboto", "roboto")


showtext_auto()

theme_regacc_line <- theme_minimal (base_size = 14, 
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

theme_regacc_scatter<- theme(plot.background = element_rect(fill= "#FAFAFA", #panel background
                                                            colour = "black"),# border lines
                             line = element_line(colour = "black"), 
                             rect = element_rect(fill = "#FAFAFA", 
                                                 linetype = 0, 
                                                 colour = NA),
                             panel.background = element_rect(fill= "#FAFAFA"),#background inside axis
                             axis.title = element_blank(), #axis titles
                             axis.line.x = element_line(size = rel(1.5),colour = "#14509E" ),#line of axis
                             axis.line.y = element_line(size = rel(1.5), colour = "#14509E"),
                             axis.text.y = element_text(colour = "black", size = rel(1.1)),
                             axis.text.x = element_text(colour = "black", size = rel(1.1)),
                             axis.ticks = element_line(size =rel(1.1),colour = "black"),
                             panel.grid.major = element_line(size = rel(1.2), colour="#14509E", linetype = 4),
                             panel.grid.minor = element_blank(),
                             legend.title = element_blank(), # remove legend title   
                             legend.position = "none",
                             legend.key = element_rect(fill="white"),
                             legend.background = element_rect(fill="white", colour = "#14509E", linetype = "solid"),#background colour legend
                             plot.title = element_text(face= "bold", colour ="#14509E", hjust =0,size = rel(1.3), margin = margin(10,0,10,0)),
                             strip.background = element_rect(fill = "#14509E", colour = NA),
                             strip.text.x = element_text(face = "bold", size = rel(1.2), hjust = 0.2,colour = "white"), 
                             strip.text.y = element_text(face = "bold", size = rel(1.2), hjust = 0.2,colour = "white"),
                             strip.placement = "outside",
                             panel.spacing = unit(1, "lines"))

theme_regacc_heatmap <- theme(plot.background = element_rect(fill= "#FAFAFA", #panel background
                                                             colour = "black"),# border lines
                              line = element_line(colour = "black"), 
                              rect = element_rect(fill = "#FAFAFA", 
                                                  linetype = 0, 
                                                  colour = NA),
                              panel.background = element_rect(fill= "#FAFAFA"),#background inside axis
                              axis.title = element_blank(), #axis titles
                              axis.line.x = element_blank(),
                              axis.line.y = element_blank(),
                              #axis.text.y = element_text(colour = "#14509E", size = rel(1.1)),
                              axis.text.x = element_text(colour = "#14509E", size = rel(1.1)),
                              axis.ticks = element_blank(),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              legend.title = element_blank(), # remove legend title    
                              legend.position = "right",
                              legend.background = element_rect(fill="white", colour = "#14509E", linetype = "solid"),#background colour legend
                              plot.title = element_text(face= "bold", colour ="#14509E", hjust =0,size = rel(1.3), margin = margin(10,0,10,0)),
                              strip.background = element_rect(fill = "#14509E", colour = NA),
                              strip.text.x = element_text(face = "bold", size = rel(1.2), hjust = 0.2,colour = "white"), 
                              strip.text.y = element_text(face = "bold", size = rel(1.2), hjust = 0.2,colour = "white"),
                              strip.placement = "outside",
                              panel.spacing = unit(1, "lines"))


luis_colors <- c('fucsia' = "#CC2299", 
                 'yellow' = "#EECC55",
                 'brown' = "#EECC99",
                 'blue' = "#0000FF",
                 'blue1' = "#11448B",
                 'red' = "#EE0000",
                 'red1' = "#881111",
                 'orange' = "#FF7700",
                 'grey' = "#CCCCCC",
                 'green' = "#668822",
                 'theme1' = "#738CEE",
                 'theme2' = "#9140FF",
                 'theme3' = "#FFB300",
                 'theme4' = "#26CCFF",
                 'theme5' = "#5FED00",
                 'theme6' = "#CC0029",
                 'theme7' = "#847A77",
                 'theme8' = "#19FF99",
                 'theme9' = "#FF6600")

#' Function to extract  colors as hex codes
#'
#' @param ... Character names of colors 
#'
luis_cols <- function (...) {
  cols <- c(...)
  
  if(is.null(cols))
    return (luis_colors)
  
  luis_colors[cols]
}

#I create two palettes, one of theme 2 and another one for eurostat
luis_palettes <- list('eurostat' = luis_cols("theme1", "theme2", "theme3", "theme4", "theme5", "theme6", "theme7", "theme8", "theme9"),
                      'heat' = luis_cols("red","grey","blue"),
                      'main' = luis_cols("fucsia", "blue","red","orange","green") )

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

#' Color scale constructor for colors
#'
#' @param palette Character name of palette in luis_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_luis <- function(palette = "eurostat", discrete = TRUE, reverse = FALSE, ...) {
  pal <- luis_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("luis_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for  colors
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



# theme_luis_line <- function (base_size = 14, base_family = "sans") {
#   theme_minimal(base_size =base_size, base_family = base_family) %+replace%   
#     
#     theme(plot.background = element_rect(fill= "#FAFAFA", #panel background
#                                          colour = "black"),# border lines
#           line = element_line(colour = "black"), 
#           rect = element_rect(fill = "#FAFAFA", 
#                               linetype = 0, 
#                               colour = NA),
#           panel.background = element_rect(fill= "#FAFAFA"),#background inside axis
#           axis.title = element_blank(), #axis titles
#           axis.line.x = element_line(size = rel(1.1),colour = "#14509E" ),#line of axis
#           axis.line.y = element_line(size = rel(1.1), colour = "#14509E"),
#           axis.text.y = element_text(colour = "black", size = rel(1.1)),
#           axis.text.x = element_text(angle = 50,colour = "black", size = rel(1.1)),
#           axis.ticks = element_line(size =rel(1.1),colour = "black"),
#           panel.grid.major.y = element_line(size = rel(1.2), colour="#14509E", linetype = 2),
#           panel.grid.minor.y = element_line(size = rel(0.8), colour="#14509E", linetype = 2),
#           panel.grid.major.x = element_blank(),
#           panel.grid.minor.x = element_blank(),
#           legend.position = "bottom",
#           legend.title = element_blank(), # remove legend title    
#           legend.background = element_rect(fill="white", colour = "#14509E", linetype = "solid"),#background colour legend
#           plot.title = element_text(face= "bold", colour ="#14509E", hjust =0,size = rel(1.3), margin = margin(10,0,10,0)),
#           strip.background = element_rect(fill = "#14509E", colour = NA),
#           strip.text.x = element_text(face = "bold", size = 10, hjust = 0.2,colour = "white"), 
#           strip.text.y = element_text(face = "bold", size = 10, hjust = 0.2,colour = "white",angle = -90),
#           strip.placement = "outside",
#           panel.spacing = unit(1, "lines"),# space between panels
#           complete = TRUE)
# }
# 
# theme_luis_scatter <- function (base_size = 14, base_family = "sans") {
#   theme_minimal(base_size =base_size, base_family = base_family) %+replace%   
#     
#     theme(plot.background = element_rect(fill= "#FAFAFA", #panel background
#                                          colour = "black"),# border lines
#           line = element_line(colour = "black"), 
#           rect = element_rect(fill = "#FAFAFA", 
#                               linetype = 0, 
#                               colour = NA),
#           panel.background = element_rect(fill= "#FAFAFA"),#background inside axis
#           axis.title = element_blank(), #axis titles
#           axis.line.x = element_line(size = rel(1.1),colour = "#14509E" ),#line of axis
#           axis.line.y = element_line(size = rel(1.1), colour = "#14509E"),
#           axis.text.y = element_text(colour = "black", size = rel(1.1)),
#           axis.text.x = element_text(angle = 50,colour = "black", size = rel(1.1)),
#           axis.ticks = element_line(size =rel(1.1),colour = "black"),
#           panel.grid.major = element_line(size = rel(1.2), colour="#14509E", linetype = 2),
#           panel.grid.minor = element_line(size = rel(0.8), colour="#14509E", linetype = 2),
#           legend.title = element_blank(), # remove legend title   
#           legend.position = "bottom",
#           legend.background = element_rect(fill="white", colour = "#14509E", linetype = "solid"),#background colour legend
#           plot.title = element_text(face= "bold", colour ="#14509E", hjust =0,size = rel(1.3), margin = margin(10,0,10,0)),
#           strip.background = element_rect(fill = "#14509E", colour = NA),
#           strip.text.x = element_text(face = "bold", size = 10, hjust = 0.2,colour = "white"), 
#           strip.text.y = element_text(face = "bold", size = 10, hjust = 0.2,colour = "white",angle = -90),
#           strip.placement = "outside",
#           panel.spacing = unit(1, "lines"),# space between panels
#           complete = TRUE)
# }
# 
# theme_luis_heatmap <- function (base_size = 14, base_family = "sans") {
#   theme_minimal(base_size =base_size, base_family = base_family) %+replace%   
#     
#     theme(plot.background = element_rect(fill= "#FAFAFA", #panel background
#                                          colour = "black"),# border lines
#           line = element_line(colour = "black"), 
#           rect = element_rect(fill = "#FAFAFA", 
#                               linetype = 0, 
#                               colour = NA),
#           panel.background = element_rect(fill= "#FAFAFA"),#background inside axis
#           axis.title = element_blank(), #axis titles
#           axis.line.x = element_blank(),
#           axis.line.y = element_blank(),
#           axis.text.y = element_text(colour = "black", size = rel(1.1)),
#           axis.text.x = element_text(angle = 50,colour = "black", size = rel(1.1)),
#           axis.ticks = element_blank(),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           legend.title = element_blank(), # remove legend title    
#           legend.position = "right",
#           legend.background = element_rect(fill="white", colour = "#14509E", linetype = "solid"),#background colour legend
#           plot.title = element_text(face= "bold", colour ="#14509E", hjust =0,size = rel(1.3), margin = margin(10,0,10,0)),
#           strip.background = element_rect(fill = "#14509E", colour = NA),
#           strip.text.x = element_text(face = "bold", size = 10, hjust = 0.2,colour = "white"), 
#           strip.text.y = element_text(face = "bold", size = 10, hjust = 0.2,colour = "white",angle = -90),
#           strip.placement = "outside",
#           panel.spacing = unit(1, "lines"))
# }


# A function to get eurostat dataset codes
# Example: df<-find_eurostat_code("nama_10r*gdp")

find_eurostat_code <- function(x) {
  df <- read_delim("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&file=table_of_contents_en.txt") %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    filter(type == "dataset" &
      str_detect(code, glob2rx(x))) %>%
    select(code) %>%
    distinct()

  return(df)
}

# A function to get eurostat datasets based on keywords in the description
# Example: df <- find_eurostat_desc("*GDP*")

find_eurostat_desc <- function(x) {
  df <- read_delim("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&file=table_of_contents_en.txt") %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    filter(type == "dataset" &
      str_detect(title, glob2rx(x)))

  return(df)
}

# A function to get eurostat datasets updated after a certain date

# Example: last <-find_eurostat_date("2021-09-25")

find_eurostat_date <- function(x) {
  df <- read_delim("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&file=table_of_contents_en.txt") %>%
    as_tibble() %>%
    janitor::clean_names() %>%
    filter(type == "dataset") %>%
    mutate(last_update_of_data = lubridate::dmy(last_update_of_data)) %>%
    filter(last_update_of_data >= x)
  return(df)
}

# Function to get most common national accounts dictionaries
# codes <- get_label_codes()

get_label_codes <- function() {  
labels<-    tibble(
    dictionary = c("na_item", "nace_r2", "geo", "asset10", "coicop"),
    data = list(
      eurostat::get_eurostat_dic("na_item"),
      eurostat::get_eurostat_dic("nace_r2"),
      eurostat::get_eurostat_dic("geo"),
      eurostat::get_eurostat_dic("asset10"),
      eurostat::get_eurostat_dic("coicop")
    )
  ) %>% 
  unnest(cols=c(data))

return(labels)
  }


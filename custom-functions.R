## includes all custom functions

write_number <- function(number, boundary = 10) {
    
    ## turns integer values smaller than the given boundary value into
    ## the corresponding English words.
    ##
    ## args:
    ## number : integer value
    ## boundary : the smallest value to be printed as numeral
    ##
    ## returns:
    ## a numeral or the corresponding English word.
    ##
    ## dependencies: english
    
    if (!is.integer(number)) ("Input must be an integer!") 
    
    n <- if (number < boundary) {
             
             as.character(english(number))
             
         } else {
             
             number
             
         }
    
    return(n)
    
}

sentence_number <- function(number) {
    
    ## turns integer values into into the corresponding English word
    ## capitlized for the start of the sentence
    ##
    ## args:
    ## number : integer value
    ##
    ## returns:
    ## the corresponding English word, capitalized
    ##
    ## dependencies: english, stringr
    
    if (!is.integer(number)) ("Input must be an integer!") 
    
    n <- if (is.numeric(number)) {
             
             str_to_sentence(as.character(english(number)))
             
         }
    
    return(n)
    
}

format_pval <- function(p_value,
                        boundary = 0.001,
                        significant_figures = 3,
                        remove_p = FALSE) {

    ## format p-values consistently, with either equal or less than
    ## symbol followed by the numeric value to given number of
    ## significant figures
    ##
    ## args:
    ## p_value : number representing p-value
    ## boundary : the smallest value to be printed as numeral
    ## significant_figures : the number of significant figures
    ##
    ## returns:
    ## string "< .001" for values smaller than boundary
    ## or string "= .XXX" for values equal to or greater than the boundary
    ##
    ## dependencies: N/A

    if (!is.numeric(p_value))

        return ("Input must be a number!") 

    if (p_value > 1)

        return ("p-values should have a value between 0 and 0.999!") 

    if (p_value == 1 & p_value > 0.999999)

        return ("$p = 0.999")

    if (!remove_p) {
        
        n <- if (p_value < boundary) {
                 
                 paste('$p$', '$<$', '.001')
                 
             } else {
                 
                 pattern_sig_fig <-
                     
                     paste0("%.", significant_figures, "f")
                 
                 p_value <-
                     
                     sub("^(-?)0.", "\\1.", sprintf(pattern_sig_fig, p_value))
                 
                 paste('$p$', "$=$", p_value)
                 
             }
        
        return(n)
        
    }
    
    else {
        
        n <- if (p_value < boundary) {
                 
                 paste('$<$', '.001')
                 
             } else {
                 
                 pattern_sig_fig <-
                     
                 paste0("%.", significant_figures, "f")
                 
                 p_value <-
                     
                     sub("^(-?)0.", "\\1.", sprintf(pattern_sig_fig, p_value))
                 
                 paste(p_value)
                 
             }
        
        return(n)    

    }
}

format_one <- function(number) {

    ## format numeric value to have one decimal place
    ##
    ## args:
    ## number : value
    ##
    ## returns:
    ## string X.X for all values, including with .0
    ##
    ## dependencies: broman
 
    if (!is.numeric(number))

        return("Input must be an integer!")

    n <- if (is.numeric(number)) {

             myround(number, 1)

         }
    
    return(n)

}

## function to plot a half violin in ggplot2
## copy and paste of gist created by Ben Marwick
## retrieved from gist on github (2020-09-01):
## https://gist.github.com/benmarwick/2a1bb0133ff568cbe28d

# somewhat hackish solution to:
# https://twitter.com/EamonCaddigan/status/646759751242620928
# based mostly on copy/pasting from ggplot2 geom_violin source:
# https://github.com/hadley/ggplot2/blob/master/R/geom-violin.r

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                        position = "dodge", trim = TRUE, scale = "area",
                        show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
            
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
)

## functions for cleaning graphs

## create stars for significance 
stars <-  function(x){

    ## add stars based on a value, normally a p value
    ##
    ## args:
    ## number : x
    ##
    ## returns:
    ## string of stars
    ##
    ## dependencies: tidyverse
    
    case_when(x <= 0.001 ~ "***",
              x < 0.01 ~ "**",
              x <= 0.05 ~ "*",
              x > 0.05 ~ "")

}

## arrange tibble by terzo and remove terzo column
## dependencies: tidyverse
arrange_by_terzo <-  function(x) {

    ## return a tibble with contrada arranged by terzo
    x %>%
        arrange(terzo) %>%
        select(-terzo)

}

## create column with one mention of terzo for pretty tables
## dependencies: tidyverse
tibble_by_terzo <- function(x) {

    ## label terzo for first entry in that terzo
    ## bru, aqu, civ are always first when arranged by terzo
    x %>%
        arrange(terzo) %>%
        mutate(terzo = case_when(contrada == "Bruco" ~ "Camollia",
                                 contrada == "Aquila" ~ "Citt\\`{a}",
                                 contrada == "Civetta" ~ "San Martino",
                                 TRUE ~ ""))        
}

## create headers for each terzo in tables
## dependencies: kableExtra
group_rows_by_terzo <- function(x) {

    ## group table by terzo
    x %>%
        group_rows("terzo di Camollia", 1, 6, bold = FALSE, italic = TRUE) %>%
        group_rows("terzo di Citt\\\\`{a}", 7, 12, bold = FALSE, italic = TRUE, escape = FALSE) %>%
        group_rows("terzo di San Martino", 13, 17, bold = FALSE, italic = TRUE)
        
}

## calculate mean, sd, max, min for a given variable
## dependencies: tidyverse
summary_stats <-  function(x, p) {

    ## create tibble with seperate columns
    x %>%
        summarise_at(c(p), c(mean, sd, min, max), na.rm = TRUE) %>%
        rename(mean = fn1, sd = fn2, min = fn3, max = fn4)
    
}

## create tibble with the relationship of each contrada
## uses object 'contrada_pairs' from 'load-packages'
## dependencies: tidyverse
create_contrada_pairs <-  function(x, tor_ond_reciprocal_rivals = FALSE) {

    ## create two dataframes
    ## these use the same original object to create the relationship between the contrada
    ## the first shows the original relationships from contrada_relationships
    ## e.g. Bruco -- Aquila
    contrada_pairs_1 <- 
        x %>%
        gather(contrada_2, relationship, -contrada) %>%
        rename(contrada_1 = contrada) %>%
        filter(!is.na(relationship))

    ## the second shows the reverse order from contrada_relationships
    ## e.g. Aquila -- Bruco
    contrada_pairs_2 <- 
        x %>%
        gather(contrada_1, relationship, -contrada) %>%
        rename(contrada_2 = contrada) %>%
        select(contrada_1, contrada_2, relationship) %>%
        filter(!is.na(relationship))

    ## combine dataframes 
    contrada_pairs <- 
        bind_rows(contrada_pairs_1, contrada_pairs_2) %>%
        ## remove duplicates of self
        distinct()
    
    ## default option: Torre and Onda are NOT reciprocal rivals
    if (!tor_ond_reciprocal_rivals) {
        
        contrada_pairs %>%
        ## classify onda--torre as rival and torre--onda as neutral
        mutate(relationship = case_when(contrada_1 == "Onda" & contrada_2 == "Torre" ~ "rival",
                                        contrada_1 == "Torre" & contrada_2 == "Onda" ~ "neutral",
                                        TRUE ~ relationship))
    }
    
    ## alternative option: Torre and Onda are reciprocal rivals
    else {
        
        contrada_pairs %>%
        ## classify onda--torre as rival and torre--onda as neutral
        mutate(relationship = case_when(contrada_1 == "Onda" & contrada_2 == "Torre" ~ "rival",
                                        contrada_1 == "Torre" & contrada_2 == "Onda" ~ "rival",
                                        TRUE ~ relationship))
    }
    
}

## create three letter acroynym of contrada names
## needed for attitudes datafile
## dependencies: tidyverse
create_contrada_acronym <-  function(x) {

    x %>%
        ## convert contrada names to three letter strings to match attitudes
        mutate(contrada_1 = str_sub(contrada_1, 1, 3),
               contrada_2 = str_sub(contrada_2, 1, 3)) %>%
        ## select only contrade used in the attitudes sample
        filter(contrada_1 == "Civ" |
               contrada_1 == "Leo" |
               contrada_1 == "Tor") %>%
        ## rename same contrada from self to own
        mutate(relationship = case_when(relationship == "self" ~ "own",
                                        TRUE ~ relationship))
    
}

## theme for maps
## with no text or tick on axes
theme_no_name <-  function(base_size = theme_size, font = NA){
    
    theme_classic(base_size = base_size, base_family = font) +
        
        theme(
            ## remove all axes
            axis.title = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank(),
            ## place legends
            legend.position = "bottom",
            ## other
            strip.background = element_blank()
        )
    
}

## create list of contrade by a defined relationship 
extract_contrade_by_relationship <- function (x, p, q) {

    # filter based on contrada
    x <- x %>% 
        # make contrada lower case and three letter acronym
        mutate(contrada_1 = str_to_lower(str_sub(contrada_1, 1, 3))) %>%
        filter(contrada_1 == p)
    
    ## extract relationship
    x %>%
        filter(relationship == q) %>% 
        pull(contrada_2)
    
}

## extract positions from GPS devices (trkpt) to tibble
path_to_tibble <- function(path) {

    ## parse gpx file
    path_file <- htmlTreeParse(path,
                               error = function (...) {}, useInternalNodes = T)
    
    ## rename headers
    ## elevations <- as.numeric(xpathSApply(path_file, path = "//trkpt/ele", xmlValue))
    times <- xpathSApply(path_file, path = "//trkpt/time", xmlValue)
    coords <- xpathSApply(path_file, path = "//trkpt", xmlAttrs)
    lats <- as.numeric(coords["lat",])
    lons <- as.numeric(coords["lon",])
    
    ## create tibble 
    output_df <- tibble(lat = lats,
                        lon = lons,
                        ## ele = elevations,
                        time = times)
    
    return(output_df)
    
}

## extract positions from GPS devices (trkpt) to tibble
## NB no time means there is no information about time
path_to_tibble_no_time <- function(path) {

    ## parse gpx file
    path_file <- htmlTreeParse(path,
                               error = function (...) {}, useInternalNodes = T)
    
    ## rename headers
    elevations <- as.numeric(xpathSApply(path_file, path = "//trkpt/ele", xmlValue))
    coords <- xpathSApply(path_file, path = "//trkpt", xmlAttrs)
    lats <- as.numeric(coords["lat",])
    lons <- as.numeric(coords["lon",])
    
    ## create tibble 
    output_df <- tibble(lat = lats, lon = lons, ele = elevations)
    
    return(output_df)
    
}

## extract relationship q for contrada p
## compatible with object ``contrada_pairs''
extract_contrade_by_relationship <- function (x, p, q) {
        
    # filter based on contrada
    x <- x %>% 
        filter(contrada_1 == p)
    
    ## extract relationship
    x %>%
        filter(relationship == q) %>% 
        pull(contrada_2)
    
}

## calculate counts and relative frequencies by variable
freq_table <- function (x, ...) {

    x %>%
        group_by(...) %>%
        summarise(n = n()) %>%
        mutate(rel_freq = paste0(round(100 * n/sum(n), 0), "%"))

}

## combine count and pull
pull_n <- function (x) {
        
    x %>% 
        count() %>%
        pull()    
    
}

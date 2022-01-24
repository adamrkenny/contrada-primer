# SPDX-License-Identifier: GPL-3.0

## ----extract-contrada-characteristics, eval = TRUE----------------------------
## create tibble with basic characteristics
contrada_basic_characteristics <-

    contrada_info %>%
    ## select relevant columns
    select(c(contrada, translation_i, symbol_simple, 
             colours_simple, festa_month, festa_day, 
             terzo)) %>%
    ## create new column for contrada symbols (i.e. animals)
    ## by collapsing translation and symbol together if they do not match
    ## note str_to_title to capitalise first letter
    mutate(symbol = if_else(str_to_title(translation_i) != str_to_title(symbol_simple),
                            paste0(str_to_title(translation_i)," (", symbol_simple,")"),
                            str_to_sentence(translation_i))) %>%
    ## edit colours to remove ``and'' and oxford comma
    mutate(colours_simple = str_replace(colours_simple, " and ", ", "),
           colours_simple = str_replace(colours_simple, ",,", ",")) %>%
    ## edit festa month entries following Chicago manual
    mutate(festa_month = str_to_title(case_when(festa_month == "april" ~ "apr",
                                                festa_month == "august" ~ "aug",
                                                festa_month == "september" ~ "sep", 
                                                TRUE ~ as.character(festa_month)))) %>%
    ## create new festa date column 
    unite(festa_date, festa_month, festa_day, sep = " ") %>%
    ## select and order columns to be included in final table
    select(contrada, symbol, colours_simple, festa_date, terzo)

## create tibble of allies and rivals for each contrada

## create two tibbles
## these use the same original object to create the relationship between the contrada

## the first shows the original relationships from contrada_relationships
## e.g. Bruco -- Aquila
contrada_pairs_1 <- 

    contrada_relationships %>%
    gather(contrada_2, relationship, -contrada) %>%
    rename(contrada_1 = contrada) %>%
    filter(!is.na(relationship))

## the second shows the reverse order from contrada_relationships
## e.g. Aquila -- Bruco
contrada_pairs_2 <- 

    contrada_relationships %>%
    gather(contrada_1, relationship, -contrada) %>%
    rename(contrada_2 = contrada) %>%
    select(contrada_1, contrada_2, relationship) %>%
    filter(!is.na(relationship))

## repeat contrada information 17 times 
## this is to combine with contrada_pairs_1/2 and use terzo information
contrada_info_repeated <- 

    contrada_info %>% 
    slice(rep(1:n(), each = 17)) %>%
    select(contrada, terzo)

## symbol to use for contrade with no allies or rivals
no_relationship_symbol <- NA

## create column of allies by contrada
contrada_allies_by_contrada <- 
    
    bind_rows(contrada_pairs_1, contrada_pairs_2) %>%
    ## remove duplicates of self
    distinct() %>%
    bind_cols(., contrada_info_repeated) %>% 
    filter(relationship == "ally") %>%
    ## order of allies by terzo
    group_by(contrada_1, relationship) %>%
    arrange(terzo) %>%
    ## combine names of allies
    mutate(ally = paste0("\\emph{", contrada_2, "}", collapse = ", ")) %>%
    ## ## FIME remove italicisation of comma
    ungroup() %>%
    distinct(contrada_1, ally) %>%
    rename(contrada = contrada_1)

## extract contrade with no allies
contrada_with_no_allies <- 
    
    merge(contrada_allies_by_contrada, select(contrada_info, contrada), all = TRUE) %>%
    filter(is.na(ally)) %>%
    pull(contrada)

## add contrada with no allies to tibble
contrada_allies_by_contrada <-     

    contrada_allies_by_contrada %>%
    add_row(contrada = c(contrada_with_no_allies), 
            ally = rep(no_relationship_symbol, length(contrada_with_no_allies))) %>%
    arrange(contrada) %>%
    rename(contrada1 = contrada) # hack to make compatible with R v 4.0

## create column of rivals by contrada
contrada_rivals_by_contrada <- 

    bind_rows(contrada_pairs_1, contrada_pairs_2) %>%
    ## include Onda--Torre as rival (but not vice versa)
    filter(relationship == "rival" | 
           contrada_1 == "Onda" & relationship == "mixed") %>%
    rename(contrada = contrada_1,
           rival = contrada_2) %>%
    mutate(rival = paste0("\\emph{", rival, "}")) %>%
    select(contrada, rival) 

## extract contrade with no rivals
contrada_with_no_rivals <- 
    
    merge(contrada_rivals_by_contrada, select(contrada_info, contrada), all = TRUE) %>%
    filter(is.na(rival)) %>%
    pull(contrada)

## add contrada with no rivals to tibble
contrada_rivals_by_contrada <-     
    
    contrada_rivals_by_contrada %>%
    add_row(contrada = c(contrada_with_no_rivals), 
            rival = rep(no_relationship_symbol, length(contrada_with_no_rivals))) %>%
    arrange(contrada) %>%
    rename(contrada2 = contrada) # hack to make compatible with R v 4.0

## combine tibbles
combine_contrada_characteristics <- 

    bind_cols(contrada_basic_characteristics,
              contrada_allies_by_contrada,
              contrada_rivals_by_contrada) 


## ----contrada-characteristics, eval = TRUE------------------------------------
## create table

## create tibble with relevant information
tbl_contrada_characteristics <- 
    
    combine_contrada_characteristics %>%
    arrange(terzo) %>% # tibble_by_terzo() %>%
    select(-c(contrada1, contrada2)) %>%
    ## mutate(number = rep(1:nrow(.))) %>% # include numbers 1--17 to match figure of map
    select(-terzo)
    ## select(terzo, ## number,
    ##            everything()) 

## table text

## column names
table_col_names <- c(# "terzo",
                     # "no.\\textsuperscript{2}",
                     ## "\\emph{terzo}",
                     "\\emph{contrada}\\textsuperscript{2}",
                     "name (symbol)\\textsuperscript{3}",
                     "colours\\textsuperscript{4}",
                     "festival\\textsuperscript{5}",
                     "ally\\textsuperscript{6}",
                     "rival\\textsuperscript{7}" 
                     )

## main captions
table_caption <- 
    "\\label{MT-tab:contrada-characteristics}Characteristics of the 17 \\contrade of Siena\\textsuperscript{1}"

## shorter caption for ToC
table_caption_short <- 
    "Characteristics of the 17 \\contrade of Siena"

## footnotes

## general
footnote_table <-  "Information from \\\\citet[32, 36, 47]{Dundes-Falassi_2005}."

## ## id
## footnote_id <- "Number corresponds to territory in \\\\cref{fig:map-contrade}."

## id
footnote_contrada <- "\\\\Contrade arranged by \\\\terzo, one of three areas in the \\\\citycentre[w]." #; \\\\cref{MT-sec:siena-palio})

## symbol
footnote_symbol <- "Symbol only shown if different from the translated name of the \\\\contrada."
## Some \\\\contrada symbols feature embellishments \\\\eg the elephant symbol for \\\\tor carries a tower on its back.

## title
footnote_title <- "Not all \\\\contrade have honorific titles."

## colours
footnote_colour <- "Does not include decorative colours."

## patronal festival
footnote_date <- "Date of patronal festival."

## ally
footnote_ally <- " All alliances are reciprocal. Order of allies as per the \\\\contrada column."

## rival
footnote_rival <- "All rivalries are reciprocal except that between \\\\ond and \\\\tor."

## generate table
## make NA appear as hyphen
options(knitr.kable.NA = "---")

## final table
table_contrada_characteristics <-

    knitr::kable(tbl_contrada_characteristics,
                 format = "latex",
                 booktabs = TRUE,
                 escape = FALSE,
                 linesep = c(rep("", 5), "\\addlinespace", 
                             rep("", 5), "\\addlinespace",
                             rep("", 4)),
                 col.names = table_col_names,
                 caption.short = table_caption_short,
                 caption = table_caption) %>%
    group_rows_by_terzo() %>%
    column_spec(1, italic = TRUE) %>%
    kable_styling(latex_options = "scale_down") %>% # to fit in page 
    footnote(number = c(footnote_table,
                        footnote_contrada,
                        footnote_symbol,
                        footnote_colour,
                        footnote_date,
                        footnote_ally,
                        footnote_rival),
             footnote_as_chunk = FALSE, 
             escape = FALSE, 
             threeparttable = TRUE, 
             fixed_small_size = TRUE) %>%
    landscape()


## ----contrada-map, eval = TRUE, cache = FALSE---------------------------------
## add osm street map
## save as xml
## https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html

##################################################
## GENERAL MAP OF SIENA
##################################################

## this map is of all the territories

#####
## ## set map proj
## ## from: https://github.com/r-spatial/sf/issues/1386
## st_crs(urb_1970_2030) = 4326

#####
## get coordinates of polygons of contrada territories

## clean data
boundaries <-    

    data_boundaries %>% 
    fortify(region = "Name") %>% 
    rename(lon = long, contrada = id) %>%
    select(lon, lat, contrada)

#####
## generate centroids of boundaries polygons

## this is to plot names in middle of contrada area
data_centroids <-

    data_boundaries %>% 
    coordinates() %>% 
    as.data.frame() %>%
    rename(lon = V1, lat = V2) # change name

## generate list of contrada names
contrada_names <-

    boundaries %>%
    select(contrada) %>%
    distinct

## join centroids list to name list
centroids <-

    bind_cols(contrada = contrada_names, data_centroids) %>%
    as_tibble()

## ## labels for contrade to be placed in centroid
## ## label 1--17 based on position in table
## centroids_contrade <-

##     merge(centroids, contrada_info) %>%
##     arrange(terzo) %>% # needed to have same order as table
##     mutate(table_number = as.factor(c(1:17)))

## labels for contrade to be placed in centroid
## three letter acronym
centroids_contrade <-

    merge(centroids, contrada_info) %>%
    arrange(terzo) %>% # same order as table
    mutate(contrada_abbr = contrada)
    ## mutate(contrada_abbr = if_else(contrada %in% c("Lupa", "Oca", "Onda"), 
    ##                                contrada,
    ##                                paste0(str_sub(contrada, 1, 3), ".")
    ##        ))

## lables for non-contrada territories
centroids_noncontrada <-
    
    centroids %>%
    filter(contrada == "Campo" | contrada == "Duomo" | contrada == "Fortezza") %>%
    arrange(desc(contrada)) %>% 
    mutate(table_letter = c("A","B","C"))

##################################################
## location of contrada buildings

## coordinates of buildings

## create list
list_buildings_general <-
    
    c("church", "societa")

## extract buildings
buildings <-

    locations %>%
    filter(building %in% list_buildings_general)

##################################################
## download osm data

## to download and save data 

## siena_osm <- 
    
##     opq("Siena") %>%
##     add_osm_feature(key = "highway") %>%
##     ## save data to call later
##     osmdata_xml(filename = "./data/siena.osm")
##     osmdata_sf()

## to input data if already saved
## siena_osm <- osmdata_sf("Siena", "./data/siena.osm")

##################################################
## map

## boundaries of contrade (excluding non-contrada territories)
boundaries_contrada <- 
    
    boundaries %>%
    filter(contrada != "Duomo" & contrada != "Campo" & contrada != "Fortezza")

## boundaries with terzi
boundaries_contrada_fill <-
    
    merge(boundaries_contrada, contrada_info)

## add fortezza as contrada, in order to generate thick line in map
boundaries_contrada_fill <- 

    boundaries_contrada_fill %>%
    bind_rows(filter(boundaries, contrada == "Fortezza"))

## map labelling and size options

## legend title
location_title <- "building"
terzo_title <- bquote(paste(italic("terzo"))) 

## thickness of point
point_thickness <- 2

## text_size <- 3
size_points <- 2
label_text_size <- 5 # 5.5
contrada_text_size <- 4 # 5.5
theme_size <- (14/5) * label_text_size

## terzi labels
camollia_label <- bquote(paste(italic("Camollia")))
citta_label <- bquote(paste(italic("Città"))) # generate accent in figure
san_martino_label <- bquote(paste(italic("San Martino")))

## tile names: https://github.com/paleolimbot/rosm/blob/master/R/tileurl.R

## osm data colour and size
osm_colour <- "gray80"
osm_size <- 0.2

#####
## ensure projection
## from: https://stackoverflow.com/a/62268361
## from: https://github.com/r-spatial/sf/issues/1419

proj4string(data_boundaries) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +init=epsg:4326")
st_crs(siena_osm$osm_polygons) <- 4326  # or whatever projection your data is in
st_crs(siena_osm$osm_lines) <- 4326  # or whatever projection your data is in

#####

## currently hardcoding nudge for some contrade
## the amount will vary depending on how the image is generated
## a better solution is required (repel doesn't print nicely)

## map of terriories
map_of_territories <-    

    ggplot() +
    layer_spatial(data_boundaries,
                  alpha = 0) +
    ## add osm data: polygons (e.g. square, roundabouts) with no fill
    geom_sf(data = siena_osm$osm_polygons, alpha = 0, color = osm_colour, size = osm_size) +
    ## add osm data: lines that represent roads
    geom_sf(data = siena_osm$osm_lines, color = osm_colour, size = osm_size) +
    geom_polygon(data = boundaries_contrada_fill,
                 aes(lon, lat, group = contrada, fill = terzo),
                 colour = "black",
                 alpha = 0.25, 
                 size = 1) +
    geom_point(data = buildings,
               aes(lon, lat, shape = building),
               size = size_points * 1.25,
               stroke = point_thickness/1.5,
               colour = "black", fill = "white") +
    geom_label(data = filter(centroids_contrade, 
                             !contrada %in% 
                             c("Aquila", "Civetta", "Drago", 
                               "Istrice", "Leocorno", 
                               "Lupa", "Nicchio", "Oca", "Onda", 
                               "Pantera", "Selva",
                               "Valdimontone")),
              aes(lon, lat,
                  label = contrada_abbr, # table_number,
                  fontface = "italic"), 
              colour = "black",
              alpha = 0,
              size = contrada_text_size,
              label.size = 0) +
    ## label aqu
    geom_label(data = filter(centroids_contrade, contrada == "Aquila"),
              aes(lon, lat,
                  label = contrada_abbr, # table_number,
                  fontface = "italic"), 
              colour = "black",
              alpha = 0,
              size = contrada_text_size,
              label.size = 0,
              nudge_y = 0.000125) +
    ## label civ
    geom_label(data = filter(centroids_contrade, contrada == "Civetta"),
              aes(lon, lat,
                  label = contrada_abbr, # table_number,
                  fontface = "italic"), 
              colour = "black",
              alpha = 0,
              size = contrada_text_size,
              label.size = 0,
              nudge_x = 0.00005, # -0.0002,
              nudge_y = -0.0003 # -0.0001
              ) +
    ## label dra
    geom_label(data = filter(centroids_contrade, contrada == "Drago"),
              aes(lon, lat,
                  label = contrada_abbr, # table_number,
                  fontface = "italic"), 
              colour = "black",
              alpha = 0,
              size = contrada_text_size,
              label.size = 0,
              nudge_x = 0.0004,
              nudge_y = 0.00005) +
    ## label ist
    geom_label(data = filter(centroids_contrade, contrada == "Istrice"),
              aes(lon, lat,
                  label = contrada_abbr, # table_number,
                  fontface = "italic"), 
              colour = "black",
              alpha = 0,
              size = contrada_text_size,
              label.size = 0,
              nudge_x = 0.0001) +
    ## label leo
    geom_label(data = filter(centroids_contrade, contrada == "Leocorno"),
              aes(lon, lat,
                  label = contrada_abbr, # table_number,
                  fontface = "italic"), 
              colour = "black",
              alpha = 0,
              size = contrada_text_size,
              label.size = 0,
              ## nudge_x = 0.0004,
              nudge_y = -0.00035) +
    ## label lupa
    geom_label(data = filter(centroids_contrade, contrada == "Lupa"),
              aes(lon, lat,
                  label = contrada_abbr, # table_number,
                  fontface = "italic"), 
              colour = "black",
              alpha = 0,
              size = contrada_text_size,
              label.size = 0,
              nudge_y = 0.0001) +
    ## label nicchio
    geom_label(data = filter(centroids_contrade, contrada == "Nicchio"),
              aes(lon, lat,
                  label = contrada_abbr, # table_number,
                  fontface = "italic"), 
              colour = "black",
              alpha = 0,
              size = contrada_text_size,
              label.size = 0,
              nudge_y = -0.0001) +
    ## label oca
    geom_label(data = filter(centroids_contrade, contrada == "Oca"),
              aes(lon, lat,
                  label = contrada_abbr, # table_number,
                  fontface = "italic"), 
              colour = "black",
              alpha = 0,
              size = contrada_text_size,
              label.size = 0,
              nudge_x = 0.0008) +    
    ## label onda
    geom_label(data = filter(centroids_contrade, contrada == "Onda"),
              aes(lon, lat,
                  label = contrada_abbr, # table_number,
                  fontface = "italic"), 
              colour = "black",
              alpha = 0,
              size = contrada_text_size,
              label.size = 0,
              nudge_y = 0.0005) +    
    ## label pan
    geom_label(data = filter(centroids_contrade, contrada == "Pantera"),
              aes(lon, lat,
                  label = contrada_abbr, # table_number,
                  fontface = "italic"), 
              colour = "black",
              alpha = 0,
              size = contrada_text_size,
              label.size = 0,
              nudge_y = 0.0002) +    
    ## label sel
    geom_label(data = filter(centroids_contrade, contrada == "Selva"),
              aes(lon, lat,
                  label = contrada_abbr, # table_number,
                  fontface = "italic"), 
              colour = "black",
              alpha = 0,
              size = contrada_text_size,
              label.size = 0,
              nudge_x = -0.0005) +
    ## label val
    geom_label(data = filter(centroids_contrade, contrada == "Valdimontone"),
              aes(lon, lat,
                  label = contrada_abbr, # table_number,
                  fontface = "italic"), 
              colour = "black",
              alpha = 0,
              size = contrada_text_size,
              label.size = 0,
              nudge_y = -0.0003) +
    geom_label(data = centroids_noncontrada,
              aes(lon, lat,
                   label = table_letter,
                   fontface = "italic"), 
              colour = "black",
              alpha = 0,
              size = contrada_text_size + 0.5,
              label.size = 0) +
    scale_fill_brewer(palette = "Dark2",
                      name = terzo_title,
                        labels = c(camollia_label,
                                   citta_label,
                                   san_martino_label),
                        na.value = "transparent",
                        na.translate = FALSE) +    
    ## for greyscale: set alpha = 0.5
    ## scale_fill_brewer(palette = "Greys",
    ##                   name = terzo_title,
    ##                   labels = c(camollia_label,
    ##                                citta_label,
    ##                              san_martino_label),
    ##                   na.value = "transparent",
    ##                   na.translate = FALSE) +
    ## scale_fill_grey(start = 0.5,
    ##                 end = 1,
    ##                 name = terzo_title,
    ##                 labels = c(camollia_label,
    ##                                citta_label,
    ##                                san_martino_label),
    ##                 na.value = "transparent",
    ##                 na.translate = FALSE) +
    scale_shape_manual(name = location_title,
                       breaks = c("church", "societa"),
                       labels = c(bquote(paste(italic("contrada"), " church")),
                                  "clubhouse"),
                       values = c(21, 24)) + 
    guides(shape = guide_legend(order = 1, nrow = 2),
           fill = guide_legend(nrow = 2)) +
    xlim(min(boundaries$lon), max(boundaries$lon)) + 
    ylim(min(boundaries$lat), max(boundaries$lat)) +
    theme_no_name() 

map_of_territories

## add scale bar and north arrow
map_with_scalebar <-
    
    map_of_territories +
    annotation_scale(width_hint = 0.3, # to set length to 0.5 km
                     location = "br") +
    annotation_north_arrow(location = "tr",
                           which_north = "true",
                           pad_y = unit(1, "cm"),
                           style = north_arrow_orienteering)


## ----contrada-map-discrepancies, eval = TRUE----------------------------------
## approximate coordinates of boundary discrepancies
## based on pull out in Fiorini_1986

## between drago and istrice
dra_ist <- 
    
    bind_rows(tibble(lat = 43.32055699134471,
                     lon = 11.32409334182739,
                     point = 1),
              tibble(lat = 43.32227418018711,
                     lon = 11.328535079956053,
                     point = 2))

## between chiocciola and pantera
chio_pan <- 
    
    bind_rows(tibble(lat = 43.31296955790226, 
                     lon = 11.325573921203612,
                     point = 1),
              tibble(lat = 43.31406245651139, 
                     lon = 11.327719688415527,
                     point = 2),
              tibble(lat = 43.31487040820429, 
                     lon = 11.327633857727049,
                     point = 3),
              tibble(lat = 43.3149016331749, 
                     lon = 11.328041553497313,
                     point = 4),
              tibble(lat = 43.31423419593446, 
                     lon = 11.327998638153074,
                     point = 5))

## between giraffa and leocorno
gir_leo <- 

    bind_rows(tibble(lat = 43.31977643491727,
                     lon = 11.333277225494385,
                     point = 1),
              tibble(lat = 43.32078335083296,
                     lon = 11.337096691131592,
                     point = 2))


## ----map-italy-province, eval = TRUE------------------------------------------
## load italy dataframe
italy <- 
    
    map_data("italy")

## check dataframe
head(italy)

## get outline

## get tuscany provinces (exc. Siena)
tuscany_provinces <- c("Arezzo",
                       "Firenze",
                       "Grosseto",
                       "Livorno",
                       "Lucca",
                       "Massa-Carrara",
                       "Pisa",
                       "Pistoia",
                       "Prato")

## italy
italy_area <- 
    
    italy %>%
    mutate(italy = "Italy") %>%
    filter(italy == "Italy") %>%
    mutate(row = 1:nrow(.),
           lon = long) %>%
    select(lon, lat, group, region) %>%
    mutate(area = case_when(region %in% tuscany_provinces ~ "region",
                            region == "Siena" ~ "province",
                            TRUE ~ "country")) %>%
    mutate(area = factor(area,
                         levels = c("province", "region", "country"))) 

## clean data
siena_comune_area <-    

    siena_comune %>% 
    fortify(name = "Name") %>%     
    rename(lon = long) %>%
    mutate(group = 1000,
           area = "municipality",
           region = "SienaComune") %>%
    select(lon, lat, group, region, area)

## historic city centre # not necessary as map too zoomed out
siena_centre_area <- 
    
    boundaries %>%
    mutate(group = 500,
           area = "citycentre",
           region = "SienaCentro") %>%
    select(lon, lat, group, region, area)

## dataframe with country, region, province, and municipality areas
italy_areas <- 
    
    rbind(siena_comune_area, italy_area) %>%
    mutate(area = factor(area,
                         levels = c("municipality", "province", "region", "country")))

## set colours of areas
city_colour <- "#C77CFF"
muni_colour <- RColorBrewer::brewer.pal(3, "Dark2")[3]
prov_colour <- RColorBrewer::brewer.pal(3, "Dark2")[2]
regi_colour <- RColorBrewer::brewer.pal(3, "Dark2")[1]  
italy_colour <- osm_colour

## plot map of italy
map_italy <-

    ggplot() +
    geom_sf() +
   geom_polygon(data = mutate(italy_areas,
                              area = case_when(
                                  area == "municipality" ~ "municipality of Siena",
                                  area == "province" ~ "province of Siena",
                                  area == "region" ~ "region of Tuscany",
                                  area == "country" ~ "x_country")), # hacky way to put country at back
                aes(x = lon,
                    y = lat,
                    group = group,
                    fill = area
                    ),
                size = 0,
                alpha = 0.75,
                colour = NA,
                ) +
    layer_spatial(siena_comune,
                  colour = muni_colour,
                  size = 0,
                  alpha = 0
                  ) +
    geom_point(data = filter(lat_lon_italian_cities, city != "Siena"),
               aes(x = lon,
                   y = lat)) +
    geom_point(data = filter(lat_lon_italian_cities, city == "Siena"),
               aes(x = lon,
                   y = lat),
               shape = 21,
               fill = "transparent") +
    geom_text(data = lat_lon_italian_cities,
              aes(x = lon,
                  y = lat,
                  label = city),
              nudge_x = 0.175,
              hjust = "left",
              vjust = c(1, # SI
                        0.5, # RM
                        0 # FL
                        )) +
    scale_colour_manual(name = NULL,
                        breaks = c(
                            "region of Tuscany", "province of Siena", "municipality of Siena"),
                        values = c(muni_colour,
                                   prov_colour,
                                   regi_colour,
                                   italy_colour
                                   )
                        ) +
    scale_fill_manual(name = NULL,
                      breaks = c(
                          "region of Tuscany", "province of Siena", "municipality of Siena"),
                      values = c(muni_colour,
                                 prov_colour,
                                 regi_colour,
                                 italy_colour
                                 )
                      ) +
    theme_void() +
    theme(legend.position = "right") +
    annotation_scale(width_hint = 0.25, # to set length to 0.5 km
                     location = "br") +
    annotation_north_arrow(location = "tr",
                           which_north = "true",
                           pad_y = unit(1, "cm"),
                           height = unit(0.75, "cm"),
                           width = unit(0.75, "cm"),
                           style = north_arrow_orienteering
                           )    

## map of comune
map_comune <- 
    
    ggplot() +
    geom_sf(data = siena_osm$osm_lines, 
            alpha = 0, 
            color = osm_colour, 
            size = osm_size) +
    layer_spatial(siena_comune,
                  fill = muni_colour,
                  colour = muni_colour,
                  alpha = 0.75,
                  size = 0) +
    layer_spatial(data_boundaries,
                  size = 0,
                  fill = city_colour,
                  colour = city_colour,
                  alpha = 0.75) +
    xlim(min(siena_comune_area$lon), max(siena_comune_area$lon)) + 
    ylim(min(siena_comune_area$lat), max(siena_comune_area$lat)) +
    theme_no_name() + 
    annotation_scale(width_hint = 0.3, # to set length to 0.5 km
                     location = "br",
                     pad_y = unit(0.1, "cm"),
                     height = unit(0.1, "cm"))

## ----map-contrade, eval = TRUE,  message = FALSE, warning = FALSE, include = TRUE, echo = FALSE, fig.scap = "The territorial boundaries of the 17 \\contrade of Siena", fig.cap = caption_text, results = "asis", fig.pos = "p!", fig.align = "center", fig.keep = "all", fig.show = "asis", eval.after = "fig.cap"----

## text for figure caption
caption_text <- 
    
     "The territorial boundaries of the 17 \\contrade of Siena. The three non-\\contrada territories are (A) the fortress, (B) the cathedral and its square, and (C) the central square plus the city hall and tower. Boundaries traced by \\adam, based on multiple sources; see \\cref{SI-sec:boundaries}. Dashed line represents location of boundary according to a previous reproduction \\citep[: pull-out map]{Fiorini_1986}. Base map data from \\citeauthor{OpenStreetMap}." # citation or statement: OpenStreetMap (\\url{www.openstreetmap.org/copyright}) 

## see guidance https://wiki.osmfoundation.org/wiki/Licence/Attribution_Guidelines#Static_images                                      

## \\Contrade with names containing more than four characters have been abbreviated; see \\cref{MT-tab:contrada-characteristics} for full names.

## print map
map_with_scalebar +
    ## add osm data: lines that represent walls
    ## geom_sf(data = siena_walls_osm$osm_lines, color = "#e7298a", size = osm_size * 3) +
    geom_path(data = dra_ist,
              aes(lon, lat),
              linetype = 2) +
    geom_path(data = chio_pan,
              aes(lon, lat),
              linetype = 2) +
    geom_path(data = gir_leo,
              aes(lon, lat),
              linetype = 2) ## + 
    ## labs(caption = "Adam R. Kenny & Laura Fortunato; CC BY-SA 4.0")



## ----print-contrada-characteristics, include = TRUE, echo = FALSE, fig.pos = "htb!", results = "asis"----
## print table
table_contrada_characteristics

## ----counts_by_group_relationship---------------------------------------------
## raw counts of the total number of pairs by group relationship
## this does not generate values used in main text, but is general information 

## total number of pairs by group relationship
relationship_counts <- 
    
    contrada_relationships %>%
    gather(other_contrada, relationship, -contrada) %>%
    filter(!is.na(relationship)) %>%
    count(relationship)
    
## total number of pairs (excluding self)
number_contrada_pairs <- 
    
    relationship_counts %>%
    filter(relationship != "self") %>% 
    summarise(number_contrada_pairs = sum(n))

## count number of contrade in total, with at least one/no ally, and with at least one/no rival

## total number of contrade
number_contrade <-     
    
    contrada_relationships %>%
    tally()

## contrade with at least one/no ally and with at least one/no rival

## create two dataframes
##
## these use the same original object to create the relationship between the contrada
## the first shows the original relationships from contrada_relationships
## e.g. Bruco -- Aquila
contrada_pairs_1 <- 
    
    contrada_relationships %>%
    gather(contrada_2, relationship, -contrada) %>%
    rename(contrada_1 = contrada) %>%
    filter(!is.na(relationship))
##
## the second shows the reverse order from contrada_relationships
## e.g. Aquila -- Bruco
contrada_pairs_2 <- 
    
    contrada_relationships %>%
    gather(contrada_1, relationship, -contrada) %>%
    rename(contrada_2 = contrada) %>%
    select(contrada_1, contrada_2, relationship) %>%
    filter(!is.na(relationship))

## combine dataframes
contrada_pairs <- 

    bind_rows(contrada_pairs_1, contrada_pairs_2) %>%
    ## remove duplicates of self
    distinct() %>%
    ## classify onda--torre as rival and torre--onda as neutral
    mutate(relationship = case_when(contrada_1 == "Onda" & contrada_2 == "Torre" ~ "rival",
                                    contrada_1 == "Torre" & contrada_2 == "Onda" ~ "neutral",
                                    TRUE ~ relationship))

## counts of relationship type by contrada

## list of relationships by contrada
relationship_counts_by_contrada <- 
    
    contrada_pairs %>%
    filter(relationship != "self") %>%
    count(contrada_1, relationship)

## ally

## number of contrade with at least one ally
number_contrade_with_ally <- 
    
    relationship_counts_by_contrada %>%
    filter(relationship == "ally" & n > 0) %>% # > 0 not necessary
    count()

## number of contrade with no ally
number_contrade_with_no_ally <- 

    as.numeric(number_contrade - number_contrade_with_ally)

## rival

## number of contrade with at least one rival
number_contrade_with_rival <- 
    
    relationship_counts_by_contrada %>%
    filter(relationship == "rival" & n > 0) %>% # see above
    count()

## number of contrade with no rival
number_contrade_with_no_rival <- 
    
    as.numeric(number_contrade - number_contrade_with_rival)

## overall counts of relationship type # FIXME just to visualise figures

## list of relationships
relationship_counts <- 
    
    contrada_pairs %>%
    filter(relationship != "self") %>%
    count(relationship)

## total number of pairs (excluding self)
number_contrada_pairs <- 
    
    relationship_counts %>%
    filter(relationship != "self") %>% 
    summarise(number_contrada_pairs = sum(n))


## ----values-from-sgourev-operti-2019------------------------------------------
## figures from Sgourev-Operti_2019 pg. 1343

## average length of alliance across 1743--2011 in years
length_alliance <- 141.2

## average length of rivalry across 1743--2011 in years
length_rivalry <- 125.8

## number of alliances formed across 1743--2011
alliances_formed <- 46L

## number of alliances ended across 1743--2011
alliances_ended <- 33L

## number of rivalries formed across 1743--2011
rivalries_formed <- 11L

## number of rivalries ended across 1743--2011
rivalries_ended <- 6L


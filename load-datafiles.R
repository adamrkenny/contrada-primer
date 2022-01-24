
## ./data/

## file containing contrada relationships
contrada_relationships <- read_csv("./data/contrada-descriptives/contrada-relationships.csv")

## contrada information
contrada_info <- read_csv("./data/contrada-descriptives/contrada-information.csv")

## ./data/attitudes

## attitudes item
attitudes <- read_csv("./data/attitudes/data-redacted.csv")

## ./data/contrada-size

## figures for contrada size
size <- read_csv("./data/contrada-size/data.csv")

## ./data/geographical

## geographical boundaries of contrade
data_boundaries <-

    readOGR("./data/geographical/contrada-boundaries.geojson")

## geographical locations of contrada buildings
locations <-

    read_csv("./data/geographical/siena-locations.csv")

## read file
siena_osm <-
        
    osmdata_sf("Siena", "./data/geographical/siena.osm")

## ## osm data for city of Siena walls
## if (!file.exists("./data/geographical/siena-walls.osm")) {

##     ## download osm data
##     siena_walls_osm <- 
        
##         opq ("Siena") %>%
##         add_osm_feature (key = "name", value = "Mura di Siena",
##                          value_exact = FALSE) %>%
##         osmdata_xml(file = "siena-walls.osm")
## }
## ## read file
## siena_walls_osm <-
    
## osmdata_sf("Siena", "./data/geographical/siena-walls.osm")

## geographical boundaries of siena comune
siena_comune <-
    
    readOGR("./data/geographical/siena-comune.geojson", 
            require_geomType = "wkbPolygon")

## read data

lat_lon_italian_cities <- 
    
    read_csv("./data/geographical/italian-cities.csv")

## ./data/survey

## survey
survey <- read_csv("./data/survey/data-redacted.csv")

## ./data/giro-study

## information about participants in giro study
giro_participants <-
    
    read_csv("./data/giro-study/data-participants-redacted.csv")

## information about visits during the giro from ARK notes
giro_visits <-
    
    read_csv("./data/giro-study/giro-visits.csv")

## information about giro from ARK notes
giro_information <-

    read_csv("./data/giro-study/giro-information.csv")

## ./data/gps

## create tibble called route
## which contains tracks from all the participants

## load tibble route from ./data/giro-study/route.rds if it exists
## otherwise create route.rds and store it for faster reading in future compilations
if (!file.exists("./data/giro-study/route.rds")) {

    ## create list with all files
    gps_files <- list.files(path = "./data/gps",  
                            pattern = "tracks-2018-[a-z]{3}-[0-9]{2}-redacted.gpx",
                            full.names = TRUE)
    ## empty list to populate with individual tracks
    list_tracks <- list()
    
    ## create tibble called route
    for (file in gps_files) {
        
        ## convert trackpoints in file into dataframe
        geodf <- path_to_tibble(file) # custom function

        ## manipulate dataframe
        df_tmp <- geodf %>%
            ## convert time in ymdhms format
            mutate(datetime = ymd_hms(time), tz = "UCT") %>%
            ## force to CET timezone
            with_tz("CET") %>% force_tz("CET") %>%
            ## extract hms
            mutate(time = strftime(datetime, format = "%H:%M:%S", tz = "CET")) %>%
            ## extract contrada and id from filename
            mutate(file_name = str_extract(file, regex("tracks-2018-[a-z]{3}-[0-9]{2}"))) %>%
            separate(file_name,
                     into = c("tracks", "year", "contrada", "id"), sep = "-", remove = TRUE) %>%
            ## clean dataframe
            select(-c(tz, tracks, year))
        
        ## output into list
        list_tracks[[file]] <- df_tmp
        
    }

    ## create final tibble
    route <- bind_rows(list_tracks)

    ## write to rds for easy loading
    route %>% write_rds("./data/giro-study/route.rds")

}

## read in route from route.rds file
route <- read_rds("./data/giro-study/route.rds")

## outline of civetta procession
route_outline_civ <- path_to_tibble_no_time("./data/giro-study/route-outline-civ.gpx")

## outline of leocorno procession
route_outline_leo <- path_to_tibble_no_time("./data/giro-study/route-outline-leo.gpx")

## ## get orcidlink.sty
## if (!file.exists("./orcidlink.sty")) {

##     download.file(
##         "https://raw.githubusercontent.com/duetosymmetry/orcidlink-LaTeX-command/master/orcidlink.sty", 
##         destfile = "./orcidlink.sty",
##         method = "curl")

## }

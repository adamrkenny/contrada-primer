# SPDX-License-Identifier: GPL-3.0

## ----giro-basics-civ, eval = TRUE---------------------------------------------
## basic recordings of day of procession
   
## get iso date
date_tmp <-
    
    giro_information %>%
    filter(str_sub(contrada, 1, 3) == "civ") %>%
    select(date) %>%
    pull()

## extract day, month, year, and weekday
date_day_tmp <-
    
    date_tmp %>%
    day()

date_month_tmp <-
    
    date_tmp %>%
    month(., label = TRUE, abbr = FALSE)

cdate_year_tmp <-
    
    date_tmp %>%
    year() %>%
    as.character()

date_weekday_tmp <-
    
    date_tmp %>%
    weekdays()

## create object
date_civ <- 
       
     paste(date_weekday_tmp, date_day_tmp, date_month_tmp)


## ----giro-assign-roles-contrada-civ, eval = TRUE------------------------------
## assign roles and contrada to each participant's gps route

## we aimed to distribute GPS devices to 
## either a flag-waver (alfiere) or a drummer (tamburino) 
## from two contrade: civetta (owl) and leocorno (unicorn)
## NB we also recruited one individual who was neither a flag-waver nor a drummer

## create list of "numbers"
## that correspond to each gps route
## which is associated with a contrada and a role

## key:
## alf = alfiere = flag-waver
## tam = tamburino = drummer
## civ = civetta (owl) contrada
## leo = leocorno (unicorn) contrada

costumed_representatives <- c("alf", "tam", "other")

## role
for (costumed_role in costumed_representatives) {
    
    ## select contrada and role
    contrada_role_tmp <-
        
        giro_information %>%
        filter(str_sub(contrada, 1, 3) == "civ") %>%
        select(gps_label_01:gps_label_12) %>% 
        gather(label, role) %>%
        mutate(label = str_replace(label, "gps_label_", "")) %>%
        filter(role == costumed_role) %>%
        pull(label)
    
    ## create object
    assign(paste0("list_", costumed_role, "_", 
                  "civ"), 
           contrada_role_tmp)
        
}


## ----giro-assign-periods-civ, eval = TRUE-------------------------------------
## remove time periods in which the procession did not take place 
## and assign morning and afternoon periods

## GPS devices recorded approximately one hour before the giro, 
## one hour after the giro, and during the lunch pause.
## These times are removed.
## The times are based on notes made by A.R.K. on the day of the giro

giro_periods <- c("am_start", "am_end", "pm_start", "pm_end")

    ## select time
    for (period in giro_periods) {

        ## select contrada and time
         contrada_time_tmp <-
            
            giro_information %>%
            filter(str_sub(contrada, 1, 3) == "civ") %>%
            ## filter(contrada == focal_contrada) %>%
            select(all_of(period)) %>% 
            pull() %>%
            as.character()
          
        ## create object
        assign(paste0("time_", period, "_", 
                      "civ"), 
               contrada_time_tmp)
        
    }


## character string to remove seconds
remove_secs <- ":00"


## ----giro-costumed-representatives-civ, eval = TRUE---------------------------
## number of costumed representatives as counted by A.R.K.

costumed_representatives <- c("alfieri", "tamburini", "paggi")

## count roles
for (costumed_role in costumed_representatives) {

    ## select contrada and count
    contrada_count_tmp <-
        
        giro_information %>%
        ## filter(contrada == focal_contrada) %>%
        filter(str_sub(contrada, 1, 3) == "civ") %>%
        select(count_alfieri:count_paggi) %>% 
        gather(role, count) %>%
        mutate(role = str_sub(str_replace(role, "count_", ""), 1, 3)) %>%
        filter(role == str_sub(costumed_role, 1, 3)) %>%
        pull(count) %>%
        as.integer()
    
        ## create object
        assign(paste0("count_costumed_", 
                      str_sub(costumed_role, 1, 3), "_", 
                      "civ"), 
               contrada_count_tmp)
    
}

## total
count_costumed_total_civ <- 

    count_costumed_alf_civ + 
    count_costumed_tam_civ + 
    count_costumed_pag_civ



## ----giro-filter-civ, eval = TRUE---------------------------------------------
## filter contrada
route_civ <- 
    
    route %>%
    filter(contrada == "civ")


## ----giro-relationships-civ, eval = TRUE--------------------------------------
## extract relationships

## relationship type can be:
## own, ally, rival, neutral
## first create list of contrada names by type wrt to each contrada
## save as object 

## create list of contrade by a defined relationship 
## should only be in custom-functions.R
## but when it is in custom-functions.R the below script does not compile properly
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

## by relationship
## own
list_own_civ <-
             
    create_contrada_pairs(contrada_relationships) %>%
    extract_contrade_by_relationship("civ", "self")

## ally
list_ally_civ <-
    
    create_contrada_pairs(contrada_relationships) %>%
    extract_contrade_by_relationship("civ", "ally")

## neutral
list_neutral_civ <-

    create_contrada_pairs(contrada_relationships) %>%
    extract_contrade_by_relationship("civ", "neutral")

## rival
list_rival_civ <- 

    create_contrada_pairs(contrada_relationships) %>%
    extract_contrade_by_relationship("civ", "rival")
    
## non contrada
list_non_contrada <- c("Campo", "Duomo", "Fortezza")

## create printable lists, with unemphasised comma

## allies
print_ally_civ <- 
    
    list_ally_civ %>%
    paste("", ., "", collapse="\\emph{,} ", sep="") %>% print

## neutral
print_neutral_civ <- 
    
    list_neutral_civ %>%
    paste("", ., "", collapse="\\emph{,} ", sep="") %>% print



## ----giro-participants-stats-civ, eval = TRUE---------------------------------
## we aimed to distribute GPS devices to either a flag-waver (alfiere)
## or a drummer (tamburino) from two contrade: civetta (owl) and leocorno (unicorn)

## note we also recruited one individual who was neither a flag-waver nor a drummer in leocorno

## key:
## alf = alfiere = flag-waver
## tam = tamburino = drummer
## civ = civetta (owl) contrada
## leo = leocorno (unicorn) contrada

## extract participants per contrada
giro_participants_civ <- 

    giro_participants %>%
    mutate(contrada = str_to_lower(str_sub(contrada, 1, 3))) %>%
    filter(contrada == "civ")

## total number of participants
giro_n_civ <- 

    giro_participants_civ %>%
    pull_n()

## total number of flag-wavers
giro_n_alf_civ <- 

    giro_participants_civ %>%
    filter(role == "alfiere") %>%
    pull_n()

## total number of drummers
giro_n_tam_civ <- 

    giro_participants_civ %>%
    filter(role == "tamburino") %>%
    pull_n()

## total number of other
giro_n_other_civ <- 

    giro_participants_civ %>%
    filter(role == "other") %>%
    pull_n()

## total number of participants minus other
giro_n_analysed_civ <- 
                    
    giro_participants_civ %>%
    filter(role != "other") %>%
    pull_n()   

## age  stats
giro_age_civ <- 

    giro_participants_civ %>%
    summary_stats("age")

## age min
giro_age_mean_civ <- 

    giro_age_civ %>%
    pull(mean)

## age min
giro_age_min_civ <- 

    giro_age_civ %>%
    pull(min)

## age max
giro_age_max_civ <- 

    giro_age_civ %>%
    pull(max)



## ----assign-roles-civ, eval = TRUE--------------------------------------------
## assign roles (alfiere, tamburino, other) to each GPS route 
## the number corresponds to the label on the \gps device

## create new variable ``role''
route_civ <- 

    route_civ %>%
    mutate(role = case_when(id %in% list_tam_civ ~ "tamburino",
                            id %in% list_alf_civ ~ "alfiere",
                            id %in% list_other_civ ~ "other"))

## remove participant with role of ``other''
route_civ <- 

    route_civ %>%
    filter(role != "other") %>%
    droplevels()



## ----assign-periods-civ, eval = TRUE------------------------------------------
## manipulate dataframe
route_civ <-     

    route_civ %>%
    filter(time >= time_am_start_civ, # giro started
           time <= time_pm_end_civ, # giro ended
           time <= time_am_end_civ | time >= time_pm_start_civ) %>% # giro paused for lunch
    ## create morning and afternoon section
    mutate(period = case_when(time <= time_am_end_civ ~ "am", # before lunch
                              time >= time_pm_start_civ ~ "pm")) # after lunch


## ----calculate-time-civ, eval = TRUE------------------------------------------
## calculate time between positions (i.e. interval)

## calculate time difference between positions
## subtract current time from previous
route_civ <- 

    route_civ %>%
    group_by(contrada, id, period) %>% # need to group to calculate by individual and period
    mutate(time_diff = datetime - lag(datetime))


## ----giro-accuracy-civ, eval = TRUE-------------------------------------------
## calculate number of correctly recorded intervals

## calculate hours

## set time format
format_time <- "%H:%M:%S"

## total time
timediff_civ <- 

    strptime(time_am_end_civ, format = format_time) - 
    strptime(time_am_start_civ, format = format_time) +
    strptime(time_pm_end_civ, format = format_time) - 
    strptime(time_pm_start_civ, format = format_time)

## total time numeric
hours_civ <- as.numeric(timediff_civ)

## calculate number of expected points

## 4 points are subtracted from the expected number
## 4 points would be lost if the device did not begin recording at the precise starting time
## 2 in the AM, 2 in the PM
expected_points_civ <- 

    (hours_civ * 60 * 12) - 4

## calculate total number of point per participant
total_points_civ <-

    route_civ %>%
    ## na.omit() %>% # first point will always be NA, so remove from count
    group_by(contrada, id) %>%
    summarise(count_total = n())              

## calculate total number of points with 5 sec interval per participant
total_correct_points_civ <-    

    route_civ %>%
    filter(time_diff == 5) %>%
    group_by(contrada, id) %>%
    summarise(count_correct = n())

## calculate total number of point with > 5 sec interval per participant
total_incorrect_points_civ <-    

    route_civ %>%
    filter(time_diff > 5) %>%
    group_by(contrada, id) %>%
    summarise(count_incorrect = n())

## calculate average per individual 
table_points_per_individual_civ <- 
   
    ## compatibility for R v4.0 and v3.5
    cbind(
        rename(total_correct_points_civ,
               contrada = contrada,
               id = id),
        rename(total_incorrect_points_civ,
               contrada1 = contrada,
               id1 = id),
        rename(total_points_civ,
               contrada2 = contrada,
               id2 = id)
    ) %>% 
    select(contrada, id, count_correct, count_incorrect, count_total) %>%
    mutate(percentage_expected = (count_total / expected_points_civ) * 100) %>%
    mutate(percentage_correct = (count_correct / count_total) * 100) %>%
    mutate(percentage_incorrect = (count_incorrect / count_total) * 100) %>%
    select(contrada, id, count_total, percentage_expected,
           count_correct, percentage_correct,
           count_incorrect, percentage_incorrect)

## calculate total count
table_positions_counts_expected_civ <- 

    table_points_per_individual_civ %>%
    summarise(mean = mean(count_total),
              sd = sd(count_total))

## mean
counts_non_interruptions_civ_mean <- 
    
    table_positions_counts_expected_civ %>%
    select(mean) %>% 
    as.numeric()

## sd
counts_non_interruptions_civ_sd <- 
    
    table_positions_counts_expected_civ %>%
    select(sd) %>% 
    as.numeric()


## calculate coverage = number of points counted over points expected
table_positions_percentage_expected_civ <- 

    table_points_per_individual_civ %>%
    summarise(mean = mean(percentage_expected),
              sd = sd(percentage_expected))

## mean
percentage_non_interruptions_civ_mean <- 
    
    table_positions_percentage_expected_civ %>%
    select(mean) %>% 
    as.numeric()

## sd
percentage_non_interruptions_civ_sd <- 
    
    table_positions_percentage_expected_civ %>%
    select(sd) %>% 
    as.numeric()


## ----data-manipulation-civ, eval = TRUE---------------------------------------
## manipulations

## remove obvious outliers

## the GPS device recorded a position that was outside of Siena (approximately 25km away)
## for civetta paritcipant id = 05
## remove any position that is too far away by setting a maximum latitude of 43.4 
route_civ <- 

    route_civ %>%
    filter(lat < 43.4)

## check time diff in data frame
table(route_civ$time_diff)
## notice extremely large min and max values
## these extremes include two large numbers
## negative extreme caused by GPS device restarting after lunch
## positive extreme caused by GPS device restarting during trial

## remove extremes values in time_diff such as negative intervals
route_civ <- 

    route_civ %>%
    filter(time_diff >= 0) # remove negative intervals

## recalculate time difference between points following above removal of outliers
route_civ <- 

    route_civ %>%
    group_by(contrada, id, period) %>% # need to group to calculate by individual and period
    mutate(time_diff = datetime - lag(datetime))

## distance

## occasionally the data "jumps"
## meaning the device does not record at the prespecified interval of 5 secs

## where the interval is larger than 120 seconds
## create an average coordinate between the previous and subsequent position

## create average position
route_civ <- 

    route_civ %>%
    group_by(contrada, id, period) %>% # need to group to calculate by individual and period
    mutate(lat = if_else(time_diff > 120, (lead(lat) + lag(lat))/2, lat, missing = lat), 
           lon = if_else(time_diff > 120, (lead(lon) + lag(lon))/2, lon, missing = lon)) %>%
    ungroup()


## ----giro-descriptives-time-civ, eval = TRUE----------------------------------
## calculate total time of procession
## by subtracting the endtime from the start time, for both the morning and the evening period
total_time_civ <- 
    
    as.difftime(time_am_end_civ) - as.difftime(time_am_start_civ) + 
    as.difftime(time_pm_end_civ) - as.difftime(time_pm_start_civ)


## ----giro-recreated-civ-code, eval = TRUE, warning = FALSE, message = FALSE, include = TRUE, echo = FALSE, fig.cap = "The re-creation of the \\civ route. Compare with \\cref{fig:giro-map-civ}. See \\cref{SI-sec:giro-distance-civ} for more details.", results = "asis", fig.align = 'center', fig.keep = "all", fig.show = "asis", eval.after = "fig.cap"----
## add order
## clean data
boundaries_recreated_civ <-    
    
    data_boundaries %>% 
    fortify(region = "Name") %>% 
    rename(lon = long, contrada = id) %>%
    select(lon, lat, contrada)##  %>%
    ## filter(contrada != "Fortezza")

## longname of contrada
territory_site_long_civ <- "civetta"
territory_site_long_leo <- "leocorno"

territory_site_long_cap_civ <- "Civetta"
territory_site_long_cap_leo <- "Leocorno"

## function for changing order of visits after lunch to include patronal site
## for civ: patronal site first visit after lunch
order_pm_civ <- function (x) {
    
    x %>%
        mutate(order = if_else(period == "pm", order + 1, order)) 
    
}

## for leo: patronal site second visit after lunch
order_pm_leo <- function (x) {
    
    x %>%
        mutate(order = if_else(period == "pm" & order >= 2, order + 1, order))
    
}

## information on territories visited from A.R.K.'s notes    
giro_visits_civ <-
    
    giro_visits %>% 
    mutate(contrada = str_sub(contrada, 1, 3)) %>%
    filter(contrada == "civ") %>%
    filter(period != "both") %>%
    mutate(territory_name = str_to_sentence(territory_site)) %>% 
    filter(territory_site != "patronal-site") %>% 
    add_row(contrada = "civ",
            territory_site = "fortezza",
            territory_name = "Fortezza") %>%
    mutate(order_visited = if_else(
               territory_site == territory_site_long_civ, 
               as.numeric(0), 
               as.numeric(order_visited))) %>% 
    arrange(order_visited) %>% 
    group_by(period) %>%
    mutate(order = 1:n()) %>% 
    ungroup(period) %>%
    mutate(order = if_else(order < 19, as.numeric(order), NA_real_)) %>%
    order_pm_civ() %>% # mutate(order = if_else(period == "pm", order + 1, order)) %>%
    mutate(period = toupper(period)) %>%
    select(territory_name, order, period) %>%
    rename(contrada = territory_name) %>%
    filter(!is.na(period))

## bind boundaries and visits information
boundaries_visits_civ <-
    
    left_join(boundaries_recreated_civ, 
              giro_visits_civ)

#####
## generate centroids of boundaries polygons

## longname of contrada rival
contrada_civ_riv <- "Leocorno"
contrada_leo_riv <- "Civetta"

## this is to plot names in middle of contrada area
data_centroids_recreated <-

    data_boundaries %>% 
    coordinates() %>% 
    as.data.frame() %>%
    rename(lon = V1, lat = V2) %>% # change name
    mutate(contrada = c("Aquila", "Bruco", "Campo", "Chiocciola", "Civetta",
                        "Drago", "Duomo", "Fortezza", "Giraffa", "Istrice",
                        "Leocorno", "Lupa", "Nicchio", "Oca", "Onda",
                        "Pantera", "Selva", "Tartuca", "Torre", "Valdimontone")) %>%
    filter(contrada != "Fortezza" & 
           contrada != contrada_civ_riv & 
           contrada != territory_site_long_cap_civ)

## generate list of contrada names
contrada_names_recreated_civ <-

    giro_visits_civ %>%
    arrange(contrada) %>%
    ## select(order, period_order) %>%
    select(order, period) %>%
    distinct##  %>%
    ## mutate(period = case_when(str_detect(period_order, "AM") ~ "AM", 
    ##                           str_detect(period_order, "PM") ~ "PM"
    ##                           ))

## join centroids list to name list
centroids_recreated_civ <-

    bind_cols(contrada = contrada_names_recreated_civ, data_centroids_recreated) %>%
    as_tibble() ## %>%
    ## ## add 19 to own
    ## mutate(order = if_else(contrada == territory_site_long_cap_civ, "1/19", as.character(order)))
    
## manually assign period to route

## civetta
route_outline_civ_period <-
    
    route_outline_civ %>%
    mutate(period = if_else(row_number() < 428, "AM", "PM")) 

## leocorno
route_outline_leo_period <-
    
    route_outline_leo %>%
    mutate(period = if_else(row_number() < 449, "AM", "PM")) 

## set text sizes
label_text_size <- 4 * 1.1 # 3.5
theme_size <- (14/5) * label_text_size

## name of contrade
fullname_civ <- "Civetta"
fullname_leo <- "Leocorno"

## get patronal site
## object with coordinate of buildings and relationship between the contrade
buildings_civ <- 
        
        locations %>%
        filter(building == "worship-civ") %>%
        select(lon, lat, contrada, building) %>%
        rename(territory_name = contrada) %>% 
        mutate(contrada = "civ") %>%
        mutate(period = "PM")

## define aesthetics
   
## size of points in maps
size_points <- 3

## thickness of point
point_thickness <- 2

## text for legends
buildings_label <- ""
saint_church_label <- "patronal site"

## label for patronal site
patronal_site_order_civ <- "1"
patronal_site_order_leo <- "2"

## plot graph with routes
giro_recreated_civ <- 

    ggplot() +
    layer_spatial(data = data_boundaries
                , alpha = 0) +
    geom_polygon(data = na.omit(boundaries_visits_civ)
               , aes(lon
                   , lat
                   , group = contrada
                   , fill = period
                     )
               , colour = "black"
               , size = 0.5
               , alpha = 1
                 ) +
    geom_polygon(data = mutate(filter(boundaries_visits_civ, 
                                      contrada == territory_site_long_cap_civ),
                               period = "AM")
               , aes(lon
                   , lat
                   , group = contrada
                     )
               , colour = "black"
               , fill = "grey20"
               , size = 0.5
               , alpha = 1
                 ) +
    geom_polygon(data = mutate(filter(boundaries_visits_civ, 
                                      contrada == territory_site_long_cap_civ),
                               period = "PM")
               , aes(lon
                   , lat
                   , group = contrada
                     )
               , colour = "black"
               , fill = "grey20"
               , size = 0.5
               , alpha = 1
                 ) +
    scale_fill_grey(name = "period"
                  , start = 0.8
                  , end = 0.8
                  , limits = c("AM", "PM")
                  , labels = c("before lunch", "after lunch")
                    ) +
    geom_label(data = buildings_civ 
             , aes(lon
                 , lat 
                 , label = patronal_site_order_civ)
             , colour = "black" # "black" 
             , fill = "grey80"
             , size = label_text_size,
             , label.size = 0.5
             , alpha = 1
             , inherit.aes = FALSE) +
    geom_path(data = route_outline_civ_period
            , aes(lon
                , lat),
            , colour = "#d95f02"
            , size = 1,
            , alpha = 1
              ) +
    geom_label(data = centroids_recreated_civ
             , aes(lon
                 , lat
                 , label = order)
             , alpha = 0
             , size = label_text_size
             , label.padding = unit(0.0, "lines")
             , label.r = unit(0.0, "lines")
             , label.size = 0) + 
    scale_colour_manual(values = c("black", "white")) +
    facet_wrap(. ~ period
             , ncol = 2
             , nrow = 1
             , labeller = labeller(period = c(
                                       "AM" = "before\nthe afternoon break",
                                       "PM" = "after\nthe afternoon break"))
             , strip.position = "top"
             , shrink = TRUE 
             , drop = TRUE
               ) +
    guides(point = guide_legend(nrow = 1, byrow = TRUE)
         , fill = FALSE
         , colour = FALSE) +
    theme_no_name()


## ----giro-descriptives-distance-civ, eval = TRUE------------------------------
## calculate total distance travelled by procession 

## \gps data is quite inaccurate for this purpose:
## take two positions, one might be on the left side of the
## road, then the other on the right hand side; this might be 
## repeated such that the next position is on the left again, etc., 
## which creates a ``zig-zag'' pattern

## if the distance is calculated by summing every individual ``zig-zag'', 
## then we will very likely get an overestimate of the distance travelled.

## thus, we re-created the route using www.maps.openrouteservice.org

## we sequentially added each building (church, clubhouse, or patronal
## site) as waypoints and set the route calculation to the setting
## ``shortest'' (\ie trace route that minimises the distance between
## waypoints)

## see the route drawn on openrouteservice.org:
## civ:
## https://maps.openrouteservice.org/directions?n1=43.319433&n2=11.323616&n3=17&a=43.319683,11.33258,43.321057,11.328782,43.322454,11.328846,43.325571,11.326945,43.326921,11.325262,43.32165,11.329951,43.322875,11.33155,43.321345,11.331754,43.323039,11.332848,43.321096,11.331561,43.320643,11.331807,43.321712,11.333095,43.320869,11.332708,43.321728,11.333106,43.320448,11.331818,43.319854,11.332998,43.315257,11.331936,43.318293,11.332161,43.317076,11.333996,43.31496,11.338695,43.317349,11.336474,43.317731,11.338835,43.319324,11.331325,43.319964,11.33126,43.319578,11.332481,43.320354,11.331593,43.320658,11.332526,43.320877,11.331292,43.319708,11.32841,43.319886,11.33037,43.318481,11.330606,43.318192,11.328192,43.317204,11.327283,43.318129,11.32817,43.316295,11.329833,43.315296,11.327677,43.314047,11.327451,43.315288,11.32772,43.315141,11.328551,43.31318,11.327956,43.31489,11.330606,43.314765,11.329458,43.316436,11.330026,43.316521,11.330777,43.316306,11.328877,43.315288,11.331099,43.318855,11.331668,43.318926,11.332601,43.31948,11.331335,43.319992,11.331348&b=2&c=1&k1=en-US&k2=km

## leo:
## https://maps.openrouteservice.org/directions?n1=43.323695&n2=11.331164&n3=16&a=43.319043,11.334758,43.318598,11.332408,43.317085,11.334037,43.31496,11.338695,43.317458,11.3367,43.317731,11.338835,43.317505,11.336936,43.317364,11.335369,43.319214,11.332397,43.318965,11.330574,43.319933,11.330316,43.319708,11.32841,43.318231,11.328835,43.317204,11.327283,43.317958,11.328042,43.317177,11.328503,43.315366,11.328707,43.314047,11.327451,43.315374,11.328278,43.315141,11.328551,43.31318,11.327956,43.314781,11.330638,43.316521,11.330777,43.318005,11.331453,43.315257,11.331936,43.314765,11.329458,43.314609,11.331325,43.31635,11.330187,43.318153,11.331797,null,null,43.319058,11.332955,43.321205,11.332151,43.321642,11.333095,43.320869,11.332708,43.322188,11.334468,43.323039,11.332848,43.321369,11.331518,43.322875,11.33155,43.321142,11.330531,43.321057,11.328782,43.322454,11.328846,43.325571,11.326945,43.324545,11.327323,43.323453,11.327398,43.321681,11.327934,43.320401,11.331024,43.319355,11.331335,43.318972,11.332655,43.318504,11.331024,43.318067,11.331153,43.319051,11.334758&b=2&c=1&k1=en-US&k2=km

## calcuate distance using outline of route
distance_total_civ <- 
    
    route_outline_civ %>%
    mutate(lon_lag = lag(lon),
           lat_lag = lag(lat)) %>%
    rowwise() %>%
    mutate(distance = distm(c(lon, lat), 
                            c(lon_lag, lat_lag), 
                            fun = distVincentyEllipsoid)) %>%
    ungroup() %>%
    summarise(total_distance = sum(distance, na.rm = TRUE)/1000) %>% # convert to km
    pull()


## ----time-calculations-manipulation-civ, eval = TRUE--------------------------
## Calculate time spent in contrada

#####

## first we need to assign every position from the GPS device
## the corresponding contrada territory it finds itself in

## to do this, we add an empty column called ``territory_name''
## to the route data (which comes from the gps devices)
route_civ <- 
    
    route_civ %>%
    mutate(territory_name = "Empty") # label NA results in issues later

## we then get the boundaries of each territory
## from object data_boundaries

## finally we create a loop
## this inputs territory name from data_boundaries 
## into new variable ``territory_name''

## it looks at the lats and lons of each position in the route
## and asks: is this position in one of the polygons defining a territory?
## if it is, it returns the name of the territory from data_boundaries
for (polygon in 1:length(data_boundaries)) {
    
    polygon_name <- as.character(data_boundaries@data$Name[[polygon]])
    polygon_lons <- data_boundaries@polygons[[polygon]]@Polygons[[1]]@coords[,1]
    polygon_lats <- data_boundaries@polygons[[polygon]]@Polygons[[1]]@coords[,2]

    route_civ <- 
        
        route_civ %>%
        mutate(territory_name = if_else(point.in.polygon(lon, lat, polygon_lons, polygon_lats) == 1,
                                        polygon_name,
                                        territory_name))
}

## we now have route with each position in a territory
## check contrada names in data frame
route_civ %>%
    count(territory_name)
## NB "Empty" territories are where the boundaries buttress imperfectly
## leaving `gaps'

#####

## create tibble with the  relationship type between civ and all other contrade

## relationship type can be:
## own, ally, rival, neutral

## assign a relationship to each territory name
route_relationships_civ <- 
                        
        route_civ %>%
        mutate(relationship = case_when(territory_name %in% list_own_civ ~ "own",
                                        territory_name %in% list_ally_civ ~ "ally",
                                        territory_name %in% list_rival_civ ~ "rival",
                                        territory_name %in% list_neutral_civ ~ "neutral",
                                        territory_name %in% list_non_contrada ~ "non-contrada",
                                        territory_name == "Empty" ~ "empty"))


## ----time-calculations-overall-civ, eval = TRUE-------------------------------
## create objects that are summary tables
## mean time spent in each contrada by relationship type

## calculate time by relationship for each individual
time_by_relationship_civ_per_id <- 
   
    route_relationships_civ %>%
    group_by(contrada, id, territory_name, relationship) %>%    
    summarise(time_spent = sum(time_diff, na.rm = TRUE)) %>%
    mutate(time_spent = as.numeric(time_spent, units = "mins"))

## manually add Fortress with time spent = 0
time_by_relationship_civ_per_id <-

    time_by_relationship_civ_per_id %>%
    ungroup() %>%
    group_by(contrada, id) %>%
    mutate(territory_name = "Fortezza", relationship = "non-contrada", time_spent = 0) %>%
    unique() %>%
    bind_rows(time_by_relationship_civ_per_id, .)

## calculate mean and sd by relationship
time_by_relationship_civ_calculations <- 

    time_by_relationship_civ_per_id %>%
    group_by(relationship) %>%
    summarise(time_mean = mean(time_spent, na.rm = TRUE),
              time_sd = sd(time_spent, na.rm = TRUE),
              count = n()) %>%
    ungroup()

## calculate mean and sd by territory
time_by_territory_civ_calculations <- 

    time_by_relationship_civ_per_id %>%
    group_by(territory_name) %>%
    summarise(time_mean = mean(time_spent, na.rm = TRUE),
              time_sd = sd(time_spent, na.rm = TRUE),
              count = n()) %>%
    ungroup()


## ----extract-average-time-by-relationship-civ---------------------------------
# create list of relationship types
relationship_types <- c("own", "ally", "neutral", "rival", "non_contrada") 

## loop to create object with overall average and sd of time spent by relationship type
for (rel_type in unique(relationship_types)) {

    ## average
    time_average_civ_tmp <- 

    time_by_relationship_civ_calculations %>%
    mutate(relationship = case_when(relationship == "non-contrada" ~ "non_contrada",
                                         TRUE ~ as.character(relationship))) %>%
    filter(relationship == rel_type) %>%
    pull(time_mean)

    ## create object
    assign(paste0("time_average_civ_", rel_type), 
           time_average_civ_tmp)

    ## standard deviation
    time_sd_civ_tmp <- 

    time_by_relationship_civ_calculations %>%
    mutate(relationship = case_when(relationship == "non-contrada" ~ "non_contrada",
                                         TRUE ~ as.character(relationship))) %>%
    filter(relationship == rel_type) %>%
    pull(time_sd)

    ## create object
    assign(paste0("time_sd_civ_", rel_type), 
           time_sd_civ_tmp) 
    
}



## ----create-order-visits-civ--------------------------------------------------
## information on territories visited from A.R.K.'s notes
giro_visits_civ <- 
    
    giro_visits %>% 
    mutate(contrada = str_sub(contrada, 1, 3)) %>%
    filter(contrada == "civ") %>% 
    mutate(territory_name = str_to_sentence(territory_site)) %>% 
    filter(territory_site != "patronal-site") %>% 
    add_row(territory_name = "Empty") %>% 
    add_row(territory_name = "Fortezza") %>% 
    arrange(order_visited) %>% 
    mutate(order = 1:n()) %>% 
    mutate(order = if_else(order < 18, as.numeric(order), NA_real_)) %>%
    arrange(territory_name) %>%
    select(c(territory_name, order))


## ----table-time-by-territory-civ----------------------------------------------
## mean time per territory
time_by_territory_civ <- 
    
    time_by_territory_civ_calculations %>%
    mutate(relationship = case_when(territory_name %in% list_own_civ ~ "own",
                                    territory_name %in% list_ally_civ ~ "ally",
                                    territory_name %in% list_rival_civ ~ "rival",
                                    territory_name %in% list_neutral_civ ~ "neutral",
                                    territory_name %in% list_non_contrada ~ "non-contrada",
                                    territory_name == "Empty" ~ "empty")) %>%
    mutate(type = "contrada") %>%
    select(territory_name, type, relationship, time_mean, time_sd)

## add information on order of visits
time_by_territory_civ <- 
       
    ## compatibility for R v4.0 and v3.5
    bind_cols(time_by_territory_civ, 
              rename(giro_visits_civ,
                     territory_name1 = territory_name)) %>% 
    select(territory_name, type, relationship, time_mean, time_sd, order)  

## mean time overall
time_overall_civ <- 

    time_by_relationship_civ_calculations %>%
    mutate(territory_name = "Total") %>%
    mutate(type = "total") %>%
    select(territory_name, type, everything(), -count)


## ----create-giro-map-civ, eval = TRUE-----------------------------------------
## the cleanest way to make the map showing the GPS routes and time
## calculations is to define multiple layers which you can add together

## the code below is iterative, so that it should work with any given contrada

##################################################
## define a base map
## this outlines the contrade in planar coordinates

base_map <- 

    ggplot() +
    layer_spatial(data = data_boundaries
                , alpha = 0) 

##################################################
## define a layer with empty boundaries

## ggplot layer
layer_boundaries_empty <-  function (x) {
    
    x +
        ## from base_map
        layer_spatial(data = data_boundaries
                    , alpha = 0
                      ## greyscale: remove black
                    , colour = "black"
                      )
    
}

##################################################
## define a layer with osm roads

## define aesthetics

## osm data colour and size
osm_colour <- "gray60" # gray80 in general map
osm_size <- 0.1 # 0.2 in general map

## set projection
st_crs(siena_osm$osm_polygons) <- 4326  # or whatever projection your data is in
st_crs(siena_osm$osm_lines) <- 4326  # or whatever projection your data is in

## ggplot layer
layer_osm <-  function (x) {
    
    x +
        ## add osm data: polygons (e.g. square, roundabouts) with no fill
        geom_sf(data = siena_osm$osm_polygons
              , alpha = 0
              , color = osm_colour
              , size = osm_size) +
        ## add osm data: lines that represent roads
        geom_sf(data = siena_osm$osm_lines
              , color = osm_colour
              , size = osm_size)
    
}

##################################################
## define a layer with the time spent in each territory

## extract time spent in each territory
## for specific contrada

## use object from above that calculated time by relationship for each individual
time_by_relationship_civ_per_id
        
## assign a relationship to each territory name
time_in_territory_civ_tmp <- 
        
        time_by_relationship_civ_per_id %>%
        group_by(contrada, territory_name) %>%
        summarise(time_spent = mean(time_spent, na.rm = TRUE)) %>%
        filter(territory_name != "Empty") %>%
        ungroup()##  %>%
        ## add_row(contrada = "civ",
        ##         territory_name = "Fortezza",
        ##         time_spent = 0)

## boundaries for each contrada

## clean data
boundaries <-    
    
    data_boundaries %>% 
    fortify(region = "Name") %>% 
    rename(lon = long, contrada = id) %>%
    select(lon, lat, contrada)

## boundaries
boundaries_civ_tmp <- 
                   
        boundaries %>%
        mutate(relationship = case_when(contrada %in% list_own_civ ~ "own",
                                        contrada %in% list_ally_civ ~ "ally",
                                        contrada %in% list_rival_civ ~ "rival",
                                        contrada %in% list_neutral_civ ~ "neutral",
                                        contrada %in% list_non_contrada ~ "non-contrada")) %>%
        select(lon, lat, contrada, relationship) %>%
        rename(territory_name = contrada)

## combine object with territory coordinates and the time in each territory    
time_spent_in_territory_civ_tmp <- 
                                
        merge(boundaries_civ_tmp, time_in_territory_civ_tmp, all.x = TRUE) %>%
        as_tibble()

## define aesthetics

time_spent_label <- "time (mins)"

## ggplot layer
layer_time_spent <- function(x) {
    
    x +
        geom_polygon(data = time_spent_in_territory_civ_tmp
                   , aes(lon
                       , lat
                       , group = territory_name
                       , fill = time_spent
                         )
                   , colour = "black"
                   , size = 0 
                   , alpha = 1
                     ) +
        scale_fill_gradient2(name = time_spent_label
                           , low = "#7570b3"
                           , mid = "#f7f7f7"
                           , high =  "#1b9e77" 
                           , midpoint = 35
                           , breaks = c(0, 20, 40, 60)
                           , limits = c(0, 70) # so that we can use same legend
                           , guide = guide_colorbar(ticks.colour = "black",
                                                    frame.colour = "black")
                            )
        ## greyscale version
        ## scale_fill_gradient(name = time_spent_label
        ##                   , low = "white"
        ##                   , high = "grey50" #"#005AB5" # "#132B43" 
        ##                   , breaks = c(0, 20, 40, 60)
        ##                   , limits = c(0, NA) # changed so that we can use same legend
        ##                   , guide = guide_colorbar(ticks.colour = "black",
        ##                                            frame.colour = "black")
        ##                     )
    
}

##################################################
## define a layer for the theme

## define aesthetics

size_points_route <- 0.5

## ggplot layer
layer_routes <- function (x) { 
    
    x +
        geom_path(data = route_civ
                , aes(lon
                    , lat
                    , group = id),
                , size = size_points_route
                , colour = "#d95f02"
                , alpha = 0.25
                  )
    
}

##################################################
## define a layer for the buildings (e.g. church, societa)

## object with coordinate of buildings and relationship between the contrade
buildings_civ <- 
        
        locations %>%
        mutate(relationship = case_when(contrada %in% list_own_civ ~ "own",
                                        contrada %in% list_ally_civ ~ "ally",
                                        contrada %in% list_rival_civ ~ "rival",
                                        contrada %in% list_neutral_civ ~ "neutral",
                                        contrada %in% list_non_contrada ~ "non-contrada")) %>%
        filter(building == "church" |
               building == "worship-civ" |
               building == "societa" & relationship == "ally") %>%
        select(lon, lat, contrada, building, relationship) %>%
        rename(territory_name = contrada) %>% 
        mutate(contrada = "civ")

## define aesthetics
   
## size of points in maps
size_points <- 3

## thickness of point
point_thickness <- 2

## text for legends
buildings_label <- ""
contrada_church_label <- bquote(paste(italic("contrada"), " church"))
contrada_societa_label <- "clubhouse (ally)"
saint_church_label <- "patronal site"

## ggplot layer
layer_buildings <- function (x) {

    x +
        geom_point(data = buildings_civ 
                 , aes(lon
                     , lat 
                     , shape = building)
                 , size = size_points * 0.75
                 , stroke = point_thickness/1.5
                 , colour = "black" 
                 , fill = "white" 
                 , alpha = 1
                 , inherit.aes = FALSE) +
        scale_shape_manual(name = buildings_label
                         , guide = "legend"
                         , labels = c(contrada_church_label,
                                      contrada_societa_label,
                                      saint_church_label)
                         , values = c(21, 24, 22))

}

##################################################
## define a layer for the relationships

## get centroids
## join centroids list to name list
centroids <-

    bind_cols(# contrada names
        contrada = 
            data_boundaries %>% 
            fortify(region = "Name") %>% 
            rename(lon = long, contrada = id) %>%
            select(lon, lat, contrada) %>%
            select(contrada) %>%
            distinct,
            # data centroids
        data_boundaries %>% 
        coordinates() %>% 
        as.data.frame() %>%
        rename(lon = V1, lat = V2) # change name
    ) %>%
    as_tibble()

## centroids of each contrada
## labelled by relationship type
centroids_civ <- 
    
    centroids %>%
    mutate(relationship = case_when(contrada %in% list_own_civ ~ "own",
                                    contrada %in% list_ally_civ ~ "ally",
                                    contrada %in% list_rival_civ ~ "rival",
                                    contrada %in% list_neutral_civ ~ "neutral",
                                    contrada %in% list_non_contrada ~ "non-contrada")) %>%
    filter(relationship == "ally" |
           relationship == "rival"  |
           relationship == "own" ) %>% 
    rename(territory_name = contrada) %>%
    mutate(contrada = "civ")

## define aesthetics
label_text_size <- 5.5

## ggplot layer
layer_relationships <- function (x## , i
                                 ) {

    x +
        geom_label(data = filter(centroids_civ,
                                 territory_name == "Leocorno"),
                   aes(lon
                     , lat
                     , label = relationship
                       ) 
               , alpha = 0.75
               , size = label_text_size
               , label.size = 0
               , nudge_x = 0.00375
               , nudge_y = 0.00125
               , hjust = 0
                 ) +
        geom_segment(data = filter(centroids_civ,
                                 territory_name == "Leocorno"),
                     aes(lon
                       , lat
                       , xend = lon + 0.00375
                       , yend = lat + 0.00125
                         ) 
                 ) +
        geom_label(data = filter(centroids_civ,
                                 territory_name == "Civetta"),
                   aes(lon
                     , lat
                     , label = relationship
                       ) 
                 , alpha = 0.75
                 , size = label_text_size
                 , label.size = 0
                 , nudge_x = 0.0075
                 , nudge_y = 0.0025
                 , hjust = 0
                   ) +
        geom_segment(data = filter(centroids_civ,
                                 territory_name == "Civetta"),
                     aes(lon
                       , lat
                       , xend = lon + 0.0075
                       , yend = lat + 0.0025
                         ) 
                 ) +
        geom_label(data = filter(centroids_civ,
                                 territory_name == "Aquila"),
                   aes(lon
                     , lat
                     , label = relationship) 
                 , alpha = 0.75
                 , size = label_text_size
                 , label.size = 0
                 , nudge_x = -0.005
                 , hjust = 1
                   ) +
        geom_segment(data = filter(centroids_civ,
                                   territory_name == "Aquila"),
                     aes(lon
                       , lat
                       , xend = lon - 0.005
                       , yend = lat
                         ) 
                     ) +
        geom_label(data = filter(centroids_civ,
                                 territory_name == "Giraffa"),
                   aes(lon
                     , lat
                     , label = relationship) 
                 , alpha = 0.75
                 , size = label_text_size
                 , label.size = 0
                 , nudge_x = 0.0025
                 , nudge_y = 0.0025
                 , hjust = 0
                   ) +
        geom_segment(data = filter(centroids_civ,
                                   territory_name == "Giraffa"),
                     aes(lon
                       , lat
                       , xend = lon + 0.0025
                       , yend = lat + 0.0025
                         ) 
                 ) +
        geom_label(data = filter(centroids_civ,
                                 territory_name == "Pantera"),
                   aes(lon
                     , lat
                     , label = relationship) 
                 , alpha = 0.75
                 , size = label_text_size
                 , label.size = 0
                 , nudge_x = -0.0025
                 , nudge_y = -0.00125
                 , hjust = 1
                   ) +
        geom_segment(data = filter(centroids_civ,
                                   territory_name == "Pantera"),
                     aes(lon
                       , lat
                       , xend = lon - 0.0025
                       , yend = lat - 0.00125
                         ) 
                 ) +
        geom_label(data = filter(centroids_civ,
                                 territory_name == "Istrice"),
                   aes(lon
                     , lat
                     , label = relationship) 
                 , alpha = 0.75
                 , size = label_text_size
                 , label.size = 0
                 , nudge_x = -0.00125
                 , nudge_y = 0.00125
                 , hjust = 1
                   ) +
        geom_segment(data = filter(centroids_civ,
                                   territory_name == "Istrice"),
                     aes(lon
                       , lat
                       , xend = lon - 0.00125
                       , yend = lat + 0.00125
                         ) 
                 ) +
        geom_label(data = filter(centroids_civ,
                                 territory_name == "Tartuca"),
                   aes(lon
                     , lat
                     , label = relationship) 
                 , alpha = 0.75
                 , size = label_text_size
                 , label.size = 0
                 , nudge_y = -0.002075 # -0.002175
                 , vjust = 1
                   ) +
        geom_segment(data = filter(centroids_civ,
                                   territory_name == "Tartuca"),
                     aes(lon
                       , lat
                       , xend = lon
                       , yend = lat - 0.002175
                         ) 
                     )
    
}

##################################################
## define a layer for the theme

## define aesthetics

## text_size <- 3
## size_points <- 4

## size of text
## needs to be defined as theme_no_name requires it
## label_text_size <- 5.5
label_text_size <- 4 # 3.5
theme_size <- (14/5) * label_text_size

## gpplot layer
layer_theme <- function (x) {
    
    x +
        xlim(min(boundaries$lon), max(boundaries$lon)) + 
        ylim(min(boundaries$lat), max(boundaries$lat)) +
        guides(shape = guide_legend(order = 1, ncol = 2)) +
        ## make fill have boxes: fill = guide_legend(nrow = 1)) +
        theme_no_name()
    
}

##################################################
## create final map
## that combines the layers

fullname_civ <- "Civetta"
fullname_leo <- "Leocorno"

## add layers
map_civ <- 
        
        base_map %>%
        layer_time_spent() %>%    
        ## layer_osm() %>%
        layer_boundaries_empty() %>%
        layer_routes() %>%
        layer_theme() %>%
        layer_buildings() %>%
        layer_relationships() +
        labs(subtitle = paste0("*", fullname_civ, "* (*n* = ", giro_n_analysed_civ, ")")) +
        ## ## add scale bar
        ## annotation_scale(width_hint = 0.3, # to set length to 0.5 km
        ##                  pad_x = unit(0.125, "cm"),
        ##                  location = "br") +
        ## annotation_north_arrow(location = "tr",
        ##                        which_north = "true",
        ##                        pad_y = unit(0.5, "cm"),
        ##                        height = unit(0.75, "cm"),
        ##                        width = unit(0.75, "cm"),
        ##                        style = north_arrow_orienteering) +
        theme(plot.subtitle = ggtext::element_markdown(hjust = 0.5))


## ----giro-cluster-analysis-civ, eval = TRUE, cache = TRUE---------------------
##################################################
## dbscan parameters

## calculating max_neighbourhood_radius from:
## https://www.datanovia.com/en/lessons/dbscan-density-based-clustering-essentials/

## ## plot graph to find knee value
## dbscan::kNNdistplot(route_civ_01, k = 5) # any route could be used
## abline(h = 0.0001, lty = 2) # check

## set maximum neighbourhood radius for points in a cluster
max_neighbourhood_radius <- 0.0001

## set minimum number of points to be included in cluster 
## as gps signal is every 5 seconds, there are 12 points a minute
## hymn lasts around 1 minute, so set time to be around 2 minutes
min_number_points <- 12 * 2 

##################################################
## make clusters per participant 

## this is through a large for loop which will
## a) create separate routes per participant
## b) create clusters using dbscan and extract the points in each cluster
## c) find centroids of each cluster
## d) create circles of radius 25m (polygons) around centroids
## e) ask if locations of buildings are within the polygons
## f) create a table per participant of visited locations

## NB this does need to be done everytime, as you have to reset lon and lat as coordinates

## get different locations

locations_all <-

    locations
    
locations_churches <-

    locations %>% 
    filter(building == "church")

locations_societa <-

    locations %>% 
    filter(building == "societa")

locations_other <-

    locations %>%
    filter(building != "church" & building != "societa")

## set lon and lat as coordinates

coordinates(locations_all) <- ~ lon + lat

coordinates(locations_churches) <- ~ lon + lat

coordinates(locations_societa) <- ~ lon + lat

## references

## creation of clusters for b) from:
## https://www.datanovia.com/en/lessons/dbscan-density-based-clustering-essentials/

## creation of polygons for d) from:
## https://gis.stackexchange.com/questions/121489/1km-circles-around-lat-long-points-in-many-places-in-world

## check overlap for e) from:
## https://gis.stackexchange.com/questions/121489/1km-circles-around-lat-long-points-in-many-places-in-world

## note that for loop currently has several commented out sections:
## these signal points at which the loop could stop if someone wanted
## to extract data at that point

## practice data (with fewer participants)
## route_civ <- route_civ %>% filter(id == "01" | id == "02")

## create list
list_df <- list()

for (participant in unique(route_civ$id)) {

    ## a) create separate routes per participant
    
    route_tmp <-
        
        route_civ %>%
        filter(id == participant) %>%
        select(lat, lon)
    
    ## b) create clusters using dbscan and extract the points in each cluster

    db_tmp <-
        
        fpc::dbscan(route_tmp,
                    eps = max_neighbourhood_radius,
                    MinPts = min_number_points)

    clustered_route_tmp <-
    
        bind_cols(route_tmp, as_tibble(db_tmp$cluster)) %>%
        rename(cluster_no = value)
    
    ## c) find centroids of each cluster
    
    clustered_centroids_tmp <-
        
        clustered_route_tmp %>%
        group_by(cluster_no) %>%
        summarise(lat = mean(lat),
                  lon = mean(lon))
    
    ## d) create circles of radius 25m (polygons) around centroids

    clustered_centroids_sp_tmp <-
        
        SpatialPointsDataFrame(clustered_centroids_tmp[, c("lon", "lat")],
                               data.frame(ID = seq(1:nrow(clustered_centroids_tmp))),
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

    projected_point_tmp <- 
        
        spTransform(clustered_centroids_sp_tmp,
                    CRS( "+init=epsg:6875" )) # epsg: https://epsg.io/?q=italy

    projected_circles_tmp <- 
        
        gBuffer(projected_point_tmp,
                width = 25, # radius of 25 m
                byid = TRUE) 
    
    circles_sp_tmp <- 
        
        spTransform(projected_circles_tmp,
                    CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
    
    proj4string(locations_all) <- 
        
        proj4string(circles_sp_tmp)
    
    proj4string(locations_churches) <- 
        
        proj4string(circles_sp_tmp)
    
    proj4string(locations_societa) <- 
        
        proj4string(circles_sp_tmp)
    
    ## e) ask if locations of buildings are within the polygons

    ## churches
    
    ## overlap between cluster and point
    overlap_with_churches <-
    
        over(circles_sp_tmp, locations_churches) %>%
        mutate(id = participant)

    list_df[[participant]] <- overlap_with_churches

    ## for overlap
    assign(paste0("overlap_civ_", participant), list_df[[participant]])
       
}

## create final dataframe
df_final_civ <- bind_rows(list_df)


## ----giro-clusters-summary-civ, eval = TRUE-----------------------------------
## descriptives of church entries

## create unique counts per individual
visited_churches_by_id_civ <-
    
    df_final_civ %>%
    filter(!is.na(contrada)) %>%
    group_by(id, contrada) %>%
    summarise(n = n()) %>%
    mutate(n = case_when(n > 1 ~ 1, # make counts at single location only once
                         n == 1 ~ 1))

## total number of church visits
visited_churches_by_contrada_civ <- 
    
    visited_churches_by_id_civ %>%
    group_by(contrada) %>%
    summarise(total = sum(n))

## first select contrada info
contrada_info_terzo <- 

    contrada_info %>%
    select(contrada, terzo)

## make tibble of visits for table
visited_churches_for_table_civ <- 

    visited_churches_by_contrada_civ %>%
    arrange(contrada) %>%
    merge(contrada_info_terzo, all = TRUE) %>%
    mutate(total = if_else(is.na(total), 0, total)) %>%
    mutate(percentage = (total/giro_n_analysed_civ) * 100) %>%
    mutate(relationship = case_when(contrada %in% list_own_civ ~ "own",
                                    contrada %in% list_ally_civ ~ "ally",
                                    contrada %in% list_rival_civ ~ "rival",
                                    contrada %in% list_neutral_civ ~ "neutral",
                                    contrada == "Empty" ~ "empty")) %>%
    arrange(terzo) %>%
    select(terzo, contrada, relationship, total, percentage) %>%
    tibble_by_terzo() 

# create list of relationship types
relationship_types <- c("own", "ally", "neutral", "rival", "non_contrada") 

## loop to create object with average visits by relationship type
for (rel_type in unique(relationship_types)) {

    ## create object
    assign(paste0("visits_civ_", rel_type), 
           visited_churches_for_table_civ %>% 
           group_by(relationship) %>% 
           summarise(mean = mean(total)) %>% 
           filter(relationship == rel_type) %>%
           pull(mean)
    )

}



## ----table-combine-time-order-cluster-civ, eval = TRUE------------------------
## create table with time spent and church visits

## add missing areas (not included in cluster analysis as not visited)
names_to_add <- c("Campo", "Duomo", "Empty", "Fortezza")

## rival name to populate table (not included in cluster analysis as not visited)
rival_civ <- "Leocorno"
rival_leo <- "Civetta"

## add above territories
cluster_visits_civ <- 
    
        visited_churches_by_contrada_civ %>% 
        add_row(contrada = names_to_add) %>%
        add_row(contrada = rival_civ, total = 0) %>%
        arrange(contrada)

## create table
master_table_civ <- 
                 
    bind_cols(time_by_territory_civ, cluster_visits_civ) %>%
    select(-type, -contrada) %>%  
    filter(territory_name != "Empty") %>% 
    arrange(match(relationship, c("own", "ally", "neutral", "rival", "non-contrada")), 
            desc(time_mean)) %>%
    select(relationship, everything()) %>%
    mutate(type = "contrada") %>%
    mutate(territory_name = case_when(territory_name == "Duomo" ~ "cathedral",
                                      territory_name == "Campo" ~ "central square",
                                      territory_name == "Fortezza" ~ "fortress",
                                      TRUE ~ paste0("\\emph{", territory_name, "}"))) %>%
    add_row(relationship = "own", territory_name = "own \\contrada",
            time_mean = time_average_civ_own, 
            time_sd = time_sd_civ_own, 
            order = NA, total = ceiling(visits_civ_own), type = "all") %>%
    add_row(relationship = "ally", territory_name = "alliance",
            time_mean = time_average_civ_ally, 
            time_sd = time_sd_civ_ally, 
            order = NA, total = ceiling(visits_civ_ally), type = "all") %>%
    add_row(relationship = "neutral", territory_name = "neutral relationship",
            time_mean = time_average_civ_neutral, 
            time_sd = time_sd_civ_neutral, 
            order = NA, total = ceiling(visits_civ_neutral), type = "all") %>%
    add_row(relationship = "rival", 
            territory_name = paste0("rivalry (\\", 
                                    str_to_lower(str_sub(rival_civ, 1, 3)), ")"),
                                   ## "rivalry (\\emph{rival_civ})",
            time_mean = time_average_civ_rival, 
            time_sd = time_sd_civ_rival,
            order = NA, total = visits_civ_rival, type = "all") %>%
    add_row(relationship = "non-contrada", territory_name = "non-\\emph{contrada}",
            time_mean = time_average_civ_non_contrada, 
            time_sd = time_sd_civ_non_contrada, 
            order = NA, total = NA, type = "all") %>%
    group_by(relationship) %>%
    arrange(match(relationship, c("own", "ally", "neutral", "rival", "non-contrada")), 
            type,
            desc(time_mean)) %>%
    ungroup() %>%
    mutate(time_mean_sd = if_else(as.numeric(time_sd) < 10, 
                                  paste0(format_one(time_mean), " \\hspace{\\onedigit}(", format_one(time_sd), ")"), 
                                  paste0(format_one(time_mean), " (", format_one(time_sd), ")"))) %>%
    select(territory_name, 
           time_mean_sd, 
           total) %>%
    slice(-2, -21)
    
caption_text_civ <- 
                 
    "\\label{SI-tab:giro-calcs-civ}Time spent and participants in \\contrada churches for the \\civ procession"

caption_text_civ_short <- 
                                  
    "\\civ procession by territory type"

## footnote
table_note <- 
    
    "Table ordered by territory type, then in descending order by mean time spent in each area of the \\\\citycentre[w]."

time_spent_note <- 

    "Time spent in each area of the \\\\citycentre[w], in minutes; the overall value by territory type is the mean across all corresponding areas, with the data pooled across participants."

cluster_note <- 
    
    paste0("Estimated number of participants, out of the total ", giro_n_analysed_civ, ", who entered the \\\\contrada church at least once; the overall value by territory type is the mean across all corresponding areas, rounded up to the nearest integer.")

## formatting for table
## line separations between territory types
linesep_civ <- 

    c("\\addlinespace", 
      rep("", 4), "\\addlinespace", 
      rep("", 11),  "\\addlinespace", 
      "\\addlinespace", 
      rep("", 3)
      )
    
linesep_leo <- 
    
    c("\\addlinespace", 
      rep("", 2), "\\addlinespace", 
      rep("", 13),  "\\addlinespace", 
      "\\addlinespace", 
      rep("", 3)
      )

## indents for individual areas
indent_civ <- 

    c(3:6, 8:18, 21:23)

indent_leo <- 
    
    c(3:4, 6:18, 21:23)

## header
table_col_names <- c("territory type\\textsuperscript{1}",
                     "mean minutes\\textsuperscript{2} ($\\mathit{SD}$)",
                     "\\emph{n} inside church\\textsuperscript{3}"
                     )

## print NAs in table with hyphen
options(knitr.kable.NA = "---")

## table
all_table_civ <- 

    knitr::kable(master_table_civ,
                 format = "latex",
                 booktabs = TRUE,
                 escape = FALSE,
                 digits = c(0, 1, 0),
                 align = c("l", "r", "r"),
                 linesep = linesep_civ,
                 col.names = table_col_names,
                 caption.short = caption_text_civ_short,
                 caption = caption_text_civ
                 ) %>%
    add_indent(indent_civ, level_of_indent = 1) %>%   
    footnote(number = c(table_note, 
                        time_spent_note,
                        cluster_note
                        ), 
             footnote_as_chunk = FALSE, 
             escape = FALSE, 
             threeparttable = TRUE, # TRUE for narrow table 
             fixed_small_size = TRUE)



## ----visited-churches-summary-civ, eval = TRUE, purl = TRUE-------------------
## extract minimum number of visits to churches (excluding zero visits)
visits_min_civ <-
    
    visited_churches_by_contrada_civ %>%
    filter(total > 0) %>%
    summarise(min = min(total, na.rm = TRUE)) %>%
    pull(min) %>%
    as.integer()

## extract minimum number of visits to churches
visits_max_civ <-
    
    visited_churches_by_contrada_civ %>%
    summarise(max = max(total, na.rm = TRUE)) %>%
    pull(max) %>%
    as.integer()

## ----giro-basics-leo, eval = TRUE---------------------------------------------
## basic recordings of day of procession
   
## get iso date
date_tmp <-
    
    giro_information %>%
    filter(str_sub(contrada, 1, 3) == "leo") %>%
    select(date) %>%
    pull()

## extract day, month, year, and weekday
date_day_tmp <-
    
    date_tmp %>%
    day()

date_month_tmp <-
    
    date_tmp %>%
    month(., label = TRUE, abbr = FALSE)

cdate_year_tmp <-
    
    date_tmp %>%
    year() %>%
    as.character()

date_weekday_tmp <-
    
    date_tmp %>%
    weekdays()

## create object
date_leo <- 
       
     paste(date_weekday_tmp, date_day_tmp, date_month_tmp)


## ----giro-assign-roles-contrada-leo, eval = TRUE------------------------------
## assign roles and contrada to each participant's gps route

## we aimed to distribute GPS devices to 
## either a flag-waver (alfiere) or a drummer (tamburino) 
## from two contrade: civetta (owl) and leocorno (unicorn)
## NB we also recruited one individual who was neither a flag-waver nor a drummer

## create list of "numbers"
## that correspond to each gps route
## which is associated with a contrada and a role

## key:
## alf = alfiere = flag-waver
## tam = tamburino = drummer
## civ = civetta (owl) contrada
## leo = leocorno (unicorn) contrada

costumed_representatives <- c("alf", "tam", "other")

## role
for (costumed_role in costumed_representatives) {
    
    ## select contrada and role
    contrada_role_tmp <-
        
        giro_information %>%
        filter(str_sub(contrada, 1, 3) == "leo") %>%
        select(gps_label_01:gps_label_12) %>% 
        gather(label, role) %>%
        mutate(label = str_replace(label, "gps_label_", "")) %>%
        filter(role == costumed_role) %>%
        pull(label)
    
    ## create object
    assign(paste0("list_", costumed_role, "_", 
                  "leo"), 
           contrada_role_tmp)
        
}


## ----giro-assign-periods-leo, eval = TRUE-------------------------------------
## remove time periods in which the procession did not take place 
## and assign morning and afternoon periods

## GPS devices recorded approximately one hour before the giro, 
## one hour after the giro, and during the lunch pause.
## These times are removed.
## The times are based on notes made by A.R.K. on the day of the giro

giro_periods <- c("am_start", "am_end", "pm_start", "pm_end")

    ## select time
    for (period in giro_periods) {

        ## select contrada and time
         contrada_time_tmp <-
            
            giro_information %>%
            filter(str_sub(contrada, 1, 3) == "leo") %>%
            ## filter(contrada == focal_contrada) %>%
            select(all_of(period)) %>% 
            pull() %>%
            as.character()
          
        ## create object
        assign(paste0("time_", period, "_", 
                      "leo"), 
               contrada_time_tmp)
        
    }


## character string to remove seconds
remove_secs <- ":00"


## ----giro-costumed-representatives-leo, eval = TRUE---------------------------
## number of costumed representatives as counted by A.R.K.

costumed_representatives <- c("alfieri", "tamburini", "paggi")

## count roles
for (costumed_role in costumed_representatives) {

    ## select contrada and count
    contrada_count_tmp <-
        
        giro_information %>%
        ## filter(contrada == focal_contrada) %>%
        filter(str_sub(contrada, 1, 3) == "leo") %>%
        select(count_alfieri:count_paggi) %>% 
        gather(role, count) %>%
        mutate(role = str_sub(str_replace(role, "count_", ""), 1, 3)) %>%
        filter(role == str_sub(costumed_role, 1, 3)) %>%
        pull(count) %>%
        as.integer()
    
        ## create object
        assign(paste0("count_costumed_", 
                      str_sub(costumed_role, 1, 3), "_", 
                      "leo"), 
               contrada_count_tmp)
    
}

## total
count_costumed_total_leo <- 

    count_costumed_alf_leo + 
    count_costumed_tam_leo + 
    count_costumed_pag_leo



## ----giro-filter-leo, eval = TRUE---------------------------------------------
## filter contrada
route_leo <- 
    
    route %>%
    filter(contrada == "leo")


## ----giro-relationships-leo, eval = TRUE--------------------------------------
## extract relationships

## relationship type can be:
## own, ally, rival, neutral
## first create list of contrada names by type wrt to each contrada
## save as object 

## create list of contrade by a defined relationship 
## should only be in custom-functions.R
## but when it is in custom-functions.R the below script does not compile properly
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

## by relationship
## own
list_own_leo <-
             
    create_contrada_pairs(contrada_relationships) %>%
    extract_contrade_by_relationship("leo", "self")

## ally
list_ally_leo <-
    
    create_contrada_pairs(contrada_relationships) %>%
    extract_contrade_by_relationship("leo", "ally")

## neutral
list_neutral_leo <-

    create_contrada_pairs(contrada_relationships) %>%
    extract_contrade_by_relationship("leo", "neutral")

## rival
list_rival_leo <- 

    create_contrada_pairs(contrada_relationships) %>%
    extract_contrade_by_relationship("leo", "rival")
    
## non contrada
list_non_contrada <- c("Campo", "Duomo", "Fortezza")

## create printable lists, with unemphasised comma

## allies
print_ally_leo <- 
    
    list_ally_leo %>%
    paste("", ., "", collapse="\\emph{,} ", sep="") %>% print

## neutral
print_neutral_leo <- 
    
    list_neutral_leo %>%
    paste("", ., "", collapse="\\emph{,} ", sep="") %>% print



## ----giro-participants-stats-leo, eval = TRUE---------------------------------
## we aimed to distribute GPS devices to either a flag-waver (alfiere)
## or a drummer (tamburino) from two contrade: civetta (owl) and leocorno (unicorn)

## note we also recruited one individual who was neither a flag-waver nor a drummer in leocorno

## key:
## alf = alfiere = flag-waver
## tam = tamburino = drummer
## civ = civetta (owl) contrada
## leo = leocorno (unicorn) contrada

## extract participants per contrada
giro_participants_leo <- 

    giro_participants %>%
    mutate(contrada = str_to_lower(str_sub(contrada, 1, 3))) %>%
    filter(contrada == "leo")

## total number of participants
giro_n_leo <- 

    giro_participants_leo %>%
    pull_n()

## total number of flag-wavers
giro_n_alf_leo <- 

    giro_participants_leo %>%
    filter(role == "alfiere") %>%
    pull_n()

## total number of drummers
giro_n_tam_leo <- 

    giro_participants_leo %>%
    filter(role == "tamburino") %>%
    pull_n()

## total number of other
giro_n_other_leo <- 

    giro_participants_leo %>%
    filter(role == "other") %>%
    pull_n()

## total number of participants minus other
giro_n_analysed_leo <- 
                    
    giro_participants_leo %>%
    filter(role != "other") %>%
    pull_n()   

## age  stats
giro_age_leo <- 

    giro_participants_leo %>%
    summary_stats("age")

## age min
giro_age_mean_leo <- 

    giro_age_leo %>%
    pull(mean)

## age min
giro_age_min_leo <- 

    giro_age_leo %>%
    pull(min)

## age max
giro_age_max_leo <- 

    giro_age_leo %>%
    pull(max)



## ----assign-roles-leo, eval = TRUE--------------------------------------------
## assign roles (alfiere, tamburino, other) to each GPS route 
## the number corresponds to the label on the \gps device

## create new variable ``role''
route_leo <- 

    route_leo %>%
    mutate(role = case_when(id %in% list_tam_leo ~ "tamburino",
                            id %in% list_alf_leo ~ "alfiere",
                            id %in% list_other_leo ~ "other"))

## remove participant with role of ``other''
route_leo <- 

    route_leo %>%
    filter(role != "other") %>%
    droplevels()



## ----assign-periods-leo, eval = TRUE------------------------------------------
## manipulate dataframe
route_leo <-     

    route_leo %>%
    filter(time >= time_am_start_leo, # giro started
           time <= time_pm_end_leo, # giro ended
           time <= time_am_end_leo | time >= time_pm_start_leo) %>% # giro paused for lunch
    ## create morning and afternoon section
    mutate(period = case_when(time <= time_am_end_leo ~ "am", # before lunch
                              time >= time_pm_start_leo ~ "pm")) # after lunch


## ----calculate-time-leo, eval = TRUE------------------------------------------
## calculate time between positions (i.e. interval)

## calculate time difference between positions
## subtract current time from previous
route_leo <- 

    route_leo %>%
    group_by(contrada, id, period) %>% # need to group to calculate by individual and period
    mutate(time_diff = datetime - lag(datetime))


## ----giro-accuracy-leo, eval = TRUE-------------------------------------------
## calculate number of correctly recorded intervals

## calculate hours

## set time format
format_time <- "%H:%M:%S"

## total time
timediff_leo <- 

    strptime(time_am_end_leo, format = format_time) - 
    strptime(time_am_start_leo, format = format_time) +
    strptime(time_pm_end_leo, format = format_time) - 
    strptime(time_pm_start_leo, format = format_time)

## total time numeric
hours_leo <- as.numeric(timediff_leo)

## calculate number of expected points

## 4 points are subtracted from the expected number
## 4 points would be lost if the device did not begin recording at the precise starting time
## 2 in the AM, 2 in the PM
expected_points_leo <- 

    (hours_leo * 60 * 12) - 4

## calculate total number of point per participant
total_points_leo <-

    route_leo %>%
    ## na.omit() %>% # first point will always be NA, so remove from count
    group_by(contrada, id) %>%
    summarise(count_total = n())              

## calculate total number of points with 5 sec interval per participant
total_correct_points_leo <-    

    route_leo %>%
    filter(time_diff == 5) %>%
    group_by(contrada, id) %>%
    summarise(count_correct = n())

## calculate total number of point with > 5 sec interval per participant
total_incorrect_points_leo <-    

    route_leo %>%
    filter(time_diff > 5) %>%
    group_by(contrada, id) %>%
    summarise(count_incorrect = n())

## calculate average per individual 
table_points_per_individual_leo <- 
   
    ## compatibility for R v4.0 and v3.5
    cbind(
        rename(total_correct_points_leo,
               contrada = contrada,
               id = id),
        rename(total_incorrect_points_leo,
               contrada1 = contrada,
               id1 = id),
        rename(total_points_leo,
               contrada2 = contrada,
               id2 = id)
    ) %>% 
    select(contrada, id, count_correct, count_incorrect, count_total) %>%
    mutate(percentage_expected = (count_total / expected_points_leo) * 100) %>%
    mutate(percentage_correct = (count_correct / count_total) * 100) %>%
    mutate(percentage_incorrect = (count_incorrect / count_total) * 100) %>%
    select(contrada, id, count_total, percentage_expected,
           count_correct, percentage_correct,
           count_incorrect, percentage_incorrect)

## calculate total count
table_positions_counts_expected_leo <- 

    table_points_per_individual_leo %>%
    summarise(mean = mean(count_total),
              sd = sd(count_total))

## mean
counts_non_interruptions_leo_mean <- 
    
    table_positions_counts_expected_leo %>%
    select(mean) %>% 
    as.numeric()

## sd
counts_non_interruptions_leo_sd <- 
    
    table_positions_counts_expected_leo %>%
    select(sd) %>% 
    as.numeric()


## calculate coverage = number of points counted over points expected
table_positions_percentage_expected_leo <- 

    table_points_per_individual_leo %>%
    summarise(mean = mean(percentage_expected),
              sd = sd(percentage_expected))

## mean
percentage_non_interruptions_leo_mean <- 
    
    table_positions_percentage_expected_leo %>%
    select(mean) %>% 
    as.numeric()

## sd
percentage_non_interruptions_leo_sd <- 
    
    table_positions_percentage_expected_leo %>%
    select(sd) %>% 
    as.numeric()


## ----data-manipulation-leo, eval = TRUE---------------------------------------
## manipulations

## remove obvious outliers

## the GPS device recorded a position that was outside of Siena (approximately 25km away)
## for civetta paritcipant id = 05
## remove any position that is too far away by setting a maximum latitude of 43.4 
route_leo <- 

    route_leo %>%
    filter(lat < 43.4)

## check time diff in data frame
table(route_leo$time_diff)
## notice extremely large min and max values
## these extremes include two large numbers
## negative extreme caused by GPS device restarting after lunch
## positive extreme caused by GPS device restarting during trial

## remove extremes values in time_diff such as negative intervals
route_leo <- 

    route_leo %>%
    filter(time_diff >= 0) # remove negative intervals

## recalculate time difference between points following above removal of outliers
route_leo <- 

    route_leo %>%
    group_by(contrada, id, period) %>% # need to group to calculate by individual and period
    mutate(time_diff = datetime - lag(datetime))

## distance

## occasionally the data "jumps"
## meaning the device does not record at the prespecified interval of 5 secs

## where the interval is larger than 120 seconds
## create an average coordinate between the previous and subsequent position

## create average position
route_leo <- 

    route_leo %>%
    group_by(contrada, id, period) %>% # need to group to calculate by individual and period
    mutate(lat = if_else(time_diff > 120, (lead(lat) + lag(lat))/2, lat, missing = lat), 
           lon = if_else(time_diff > 120, (lead(lon) + lag(lon))/2, lon, missing = lon)) %>%
    ungroup()


## ----giro-descriptives-time-leo, eval = TRUE----------------------------------
## calculate total time of procession
## by subtracting the endtime from the start time, for both the morning and the evening period
total_time_leo <- 
    
    as.difftime(time_am_end_leo) - as.difftime(time_am_start_leo) + 
    as.difftime(time_pm_end_leo) - as.difftime(time_pm_start_leo)


## ----giro-recreated-leo-code, eval = TRUE, warning = FALSE, message = FALSE, include = TRUE, echo = FALSE, fig.cap = "The re-creation of the \\leo route. Compare with \\cref{fig:giro-map-leo}. See \\cref{SI-sec:giro-distance-leo} for more details.", results = "asis", fig.align = 'center', fig.keep = "all", fig.show = "asis", eval.after = "fig.cap"----
## add order
## clean data
boundaries_recreated_leo <-    
    
    data_boundaries %>% 
    fortify(region = "Name") %>% 
    rename(lon = long, contrada = id) %>%
    select(lon, lat, contrada)##  %>%
    ## filter(contrada != "Fortezza")

## longname of contrada
territory_site_long_civ <- "civetta"
territory_site_long_leo <- "leocorno"

territory_site_long_cap_civ <- "Civetta"
territory_site_long_cap_leo <- "Leocorno"

## function for changing order of visits after lunch to include patronal site
## for civ: patronal site first visit after lunch
order_pm_civ <- function (x) {
    
    x %>%
        mutate(order = if_else(period == "pm", order + 1, order)) 
    
}

## for leo: patronal site second visit after lunch
order_pm_leo <- function (x) {
    
    x %>%
        mutate(order = if_else(period == "pm" & order >= 2, order + 1, order))
    
}

## information on territories visited from A.R.K.'s notes    
giro_visits_leo <-
    
    giro_visits %>% 
    mutate(contrada = str_sub(contrada, 1, 3)) %>%
    filter(contrada == "leo") %>%
    filter(period != "both") %>%
    mutate(territory_name = str_to_sentence(territory_site)) %>% 
    filter(territory_site != "patronal-site") %>% 
    add_row(contrada = "leo",
            territory_site = "fortezza",
            territory_name = "Fortezza") %>%
    mutate(order_visited = if_else(
               territory_site == territory_site_long_civ, 
               as.numeric(0), 
               as.numeric(order_visited))) %>% 
    arrange(order_visited) %>% 
    group_by(period) %>%
    mutate(order = 1:n()) %>% 
    ungroup(period) %>%
    mutate(order = if_else(order < 19, as.numeric(order), NA_real_)) %>%
    order_pm_leo() %>% # mutate(order = if_else(period == "pm", order + 1, order)) %>%
    mutate(period = toupper(period)) %>%
    select(territory_name, order, period) %>%
    rename(contrada = territory_name) %>%
    filter(!is.na(period))

## bind boundaries and visits information
boundaries_visits_leo <-
    
    left_join(boundaries_recreated_leo, 
              giro_visits_leo)

#####
## generate centroids of boundaries polygons

## longname of contrada rival
contrada_civ_riv <- "Leocorno"
contrada_leo_riv <- "Civetta"

## this is to plot names in middle of contrada area
data_centroids_recreated <-

    data_boundaries %>% 
    coordinates() %>% 
    as.data.frame() %>%
    rename(lon = V1, lat = V2) %>% # change name
    mutate(contrada = c("Aquila", "Bruco", "Campo", "Chiocciola", "Civetta",
                        "Drago", "Duomo", "Fortezza", "Giraffa", "Istrice",
                        "Leocorno", "Lupa", "Nicchio", "Oca", "Onda",
                        "Pantera", "Selva", "Tartuca", "Torre", "Valdimontone")) %>%
    filter(contrada != "Fortezza" & 
           contrada != contrada_leo_riv & 
           contrada != territory_site_long_cap_leo)

## generate list of contrada names
contrada_names_recreated_leo <-

    giro_visits_leo %>%
    arrange(contrada) %>%
    ## select(order, period_order) %>%
    select(order, period) %>%
    distinct##  %>%
    ## mutate(period = case_when(str_detect(period_order, "AM") ~ "AM", 
    ##                           str_detect(period_order, "PM") ~ "PM"
    ##                           ))

## join centroids list to name list
centroids_recreated_leo <-

    bind_cols(contrada = contrada_names_recreated_leo, data_centroids_recreated) %>%
    as_tibble() ## %>%
    ## ## add 19 to own
    ## mutate(order = if_else(contrada == territory_site_long_cap_leo, "1/19", as.character(order)))
    
## manually assign period to route

## civetta
route_outline_leo_period <-
    
    route_outline_leo %>%
    mutate(period = if_else(row_number() < 428, "AM", "PM")) 

## leocorno
route_outline_leo_period <-
    
    route_outline_leo %>%
    mutate(period = if_else(row_number() < 449, "AM", "PM")) 

## set text sizes
label_text_size <- 4 * 1.1 # 3.5
theme_size <- (14/5) * label_text_size

## name of contrade
fullname_civ <- "Civetta"
fullname_leo <- "Leocorno"

## get patronal site
## object with coordinate of buildings and relationship between the contrade
buildings_leo <- 
        
        locations %>%
        filter(building == "worship-leo") %>%
        select(lon, lat, contrada, building) %>%
        rename(territory_name = contrada) %>% 
        mutate(contrada = "leo") %>%
        mutate(period = "PM")

## define aesthetics
   
## size of points in maps
size_points <- 3

## thickness of point
point_thickness <- 2

## text for legends
buildings_label <- ""
saint_church_label <- "patronal site"

## label for patronal site
patronal_site_order_civ <- "1"
patronal_site_order_leo <- "2"

## plot graph with routes
giro_recreated_leo <- 

    ggplot() +
    layer_spatial(data = data_boundaries
                , alpha = 0) +
    geom_polygon(data = na.omit(boundaries_visits_leo)
               , aes(lon
                   , lat
                   , group = contrada
                   , fill = period
                     )
               , colour = "black"
               , size = 0.5
               , alpha = 1
                 ) +
    geom_polygon(data = mutate(filter(boundaries_visits_leo, 
                                      contrada == territory_site_long_cap_leo),
                               period = "AM")
               , aes(lon
                   , lat
                   , group = contrada
                     )
               , colour = "black"
               , fill = "grey20"
               , size = 0.5
               , alpha = 1
                 ) +
    geom_polygon(data = mutate(filter(boundaries_visits_leo, 
                                      contrada == territory_site_long_cap_leo),
                               period = "PM")
               , aes(lon
                   , lat
                   , group = contrada
                     )
               , colour = "black"
               , fill = "grey20"
               , size = 0.5
               , alpha = 1
                 ) +
    scale_fill_grey(name = "period"
                  , start = 0.8
                  , end = 0.8
                  , limits = c("AM", "PM")
                  , labels = c("before lunch", "after lunch")
                    ) +
    geom_label(data = buildings_leo 
             , aes(lon
                 , lat 
                 , label = patronal_site_order_leo)
             , colour = "black" # "black" 
             , fill = "grey80"
             , size = label_text_size,
             , label.size = 0.5
             , alpha = 1
             , inherit.aes = FALSE) +
    geom_path(data = route_outline_leo_period
            , aes(lon
                , lat),
            , colour = "#d95f02"
            , size = 1,
            , alpha = 1
              ) +
    geom_label(data = centroids_recreated_leo
             , aes(lon
                 , lat
                 , label = order)
             , alpha = 0
             , size = label_text_size
             , label.padding = unit(0.0, "lines")
             , label.r = unit(0.0, "lines")
             , label.size = 0) + 
    scale_colour_manual(values = c("black", "white")) +
    facet_wrap(. ~ period
             , ncol = 2
             , nrow = 1
             , labeller = labeller(period = c(
                                       "AM" = "before\nthe afternoon break",
                                       "PM" = "after\nthe afternoon break"))
             , strip.position = "top"
             , shrink = TRUE 
             , drop = TRUE
               ) +
    guides(point = guide_legend(nrow = 1, byrow = TRUE)
         , fill = FALSE
         , colour = FALSE) +
    theme_no_name()


## ----giro-descriptives-distance-leo, eval = TRUE------------------------------
## calculate total distance travelled by procession 

## \gps data is quite inaccurate for this purpose:
## take two positions, one might be on the left side of the
## road, then the other on the right hand side; this might be 
## repeated such that the next position is on the left again, etc., 
## which creates a ``zig-zag'' pattern

## if the distance is calculated by summing every individual ``zig-zag'', 
## then we will very likely get an overestimate of the distance travelled.

## thus, we re-created the route using www.maps.openrouteservice.org

## we sequentially added each building (church, clubhouse, or patronal
## site) as waypoints and set the route calculation to the setting
## ``shortest'' (\ie trace route that minimises the distance between
## waypoints)

## see the route drawn on openrouteservice.org:
## civ:
## https://maps.openrouteservice.org/directions?n1=43.319433&n2=11.323616&n3=17&a=43.319683,11.33258,43.321057,11.328782,43.322454,11.328846,43.325571,11.326945,43.326921,11.325262,43.32165,11.329951,43.322875,11.33155,43.321345,11.331754,43.323039,11.332848,43.321096,11.331561,43.320643,11.331807,43.321712,11.333095,43.320869,11.332708,43.321728,11.333106,43.320448,11.331818,43.319854,11.332998,43.315257,11.331936,43.318293,11.332161,43.317076,11.333996,43.31496,11.338695,43.317349,11.336474,43.317731,11.338835,43.319324,11.331325,43.319964,11.33126,43.319578,11.332481,43.320354,11.331593,43.320658,11.332526,43.320877,11.331292,43.319708,11.32841,43.319886,11.33037,43.318481,11.330606,43.318192,11.328192,43.317204,11.327283,43.318129,11.32817,43.316295,11.329833,43.315296,11.327677,43.314047,11.327451,43.315288,11.32772,43.315141,11.328551,43.31318,11.327956,43.31489,11.330606,43.314765,11.329458,43.316436,11.330026,43.316521,11.330777,43.316306,11.328877,43.315288,11.331099,43.318855,11.331668,43.318926,11.332601,43.31948,11.331335,43.319992,11.331348&b=2&c=1&k1=en-US&k2=km

## leo:
## https://maps.openrouteservice.org/directions?n1=43.323695&n2=11.331164&n3=16&a=43.319043,11.334758,43.318598,11.332408,43.317085,11.334037,43.31496,11.338695,43.317458,11.3367,43.317731,11.338835,43.317505,11.336936,43.317364,11.335369,43.319214,11.332397,43.318965,11.330574,43.319933,11.330316,43.319708,11.32841,43.318231,11.328835,43.317204,11.327283,43.317958,11.328042,43.317177,11.328503,43.315366,11.328707,43.314047,11.327451,43.315374,11.328278,43.315141,11.328551,43.31318,11.327956,43.314781,11.330638,43.316521,11.330777,43.318005,11.331453,43.315257,11.331936,43.314765,11.329458,43.314609,11.331325,43.31635,11.330187,43.318153,11.331797,null,null,43.319058,11.332955,43.321205,11.332151,43.321642,11.333095,43.320869,11.332708,43.322188,11.334468,43.323039,11.332848,43.321369,11.331518,43.322875,11.33155,43.321142,11.330531,43.321057,11.328782,43.322454,11.328846,43.325571,11.326945,43.324545,11.327323,43.323453,11.327398,43.321681,11.327934,43.320401,11.331024,43.319355,11.331335,43.318972,11.332655,43.318504,11.331024,43.318067,11.331153,43.319051,11.334758&b=2&c=1&k1=en-US&k2=km

## calcuate distance using outline of route
distance_total_leo <- 
    
    route_outline_leo %>%
    mutate(lon_lag = lag(lon),
           lat_lag = lag(lat)) %>%
    rowwise() %>%
    mutate(distance = distm(c(lon, lat), 
                            c(lon_lag, lat_lag), 
                            fun = distVincentyEllipsoid)) %>%
    ungroup() %>%
    summarise(total_distance = sum(distance, na.rm = TRUE)/1000) %>% # convert to km
    pull()


## ----time-calculations-manipulation-leo, eval = TRUE--------------------------
## Calculate time spent in contrada

#####

## first we need to assign every position from the GPS device
## the corresponding contrada territory it finds itself in

## to do this, we add an empty column called ``territory_name''
## to the route data (which comes from the gps devices)
route_leo <- 
    
    route_leo %>%
    mutate(territory_name = "Empty") # label NA results in issues later

## we then get the boundaries of each territory
## from object data_boundaries

## finally we create a loop
## this inputs territory name from data_boundaries 
## into new variable ``territory_name''

## it looks at the lats and lons of each position in the route
## and asks: is this position in one of the polygons defining a territory?
## if it is, it returns the name of the territory from data_boundaries
for (polygon in 1:length(data_boundaries)) {
    
    polygon_name <- as.character(data_boundaries@data$Name[[polygon]])
    polygon_lons <- data_boundaries@polygons[[polygon]]@Polygons[[1]]@coords[,1]
    polygon_lats <- data_boundaries@polygons[[polygon]]@Polygons[[1]]@coords[,2]

    route_leo <- 
        
        route_leo %>%
        mutate(territory_name = if_else(point.in.polygon(lon, lat, polygon_lons, polygon_lats) == 1,
                                        polygon_name,
                                        territory_name))
}

## we now have route with each position in a territory
## check contrada names in data frame
route_leo %>%
    count(territory_name)
## NB "Empty" territories are where the boundaries buttress imperfectly
## leaving `gaps'

#####

## create tibble with the  relationship type between leo and all other contrade

## relationship type can be:
## own, ally, rival, neutral

## assign a relationship to each territory name
route_relationships_leo <- 
                        
        route_leo %>%
        mutate(relationship = case_when(territory_name %in% list_own_leo ~ "own",
                                        territory_name %in% list_ally_leo ~ "ally",
                                        territory_name %in% list_rival_leo ~ "rival",
                                        territory_name %in% list_neutral_leo ~ "neutral",
                                        territory_name %in% list_non_contrada ~ "non-contrada",
                                        territory_name == "Empty" ~ "empty"))


## ----time-calculations-overall-leo, eval = TRUE-------------------------------
## create objects that are summary tables
## mean time spent in each contrada by relationship type

## calculate time by relationship for each individual
time_by_relationship_leo_per_id <- 
   
    route_relationships_leo %>%
    group_by(contrada, id, territory_name, relationship) %>%    
    summarise(time_spent = sum(time_diff, na.rm = TRUE)) %>%
    mutate(time_spent = as.numeric(time_spent, units = "mins"))

## manually add Fortress with time spent = 0
time_by_relationship_leo_per_id <-

    time_by_relationship_leo_per_id %>%
    ungroup() %>%
    group_by(contrada, id) %>%
    mutate(territory_name = "Fortezza", relationship = "non-contrada", time_spent = 0) %>%
    unique() %>%
    bind_rows(time_by_relationship_leo_per_id, .)

## calculate mean and sd by relationship
time_by_relationship_leo_calculations <- 

    time_by_relationship_leo_per_id %>%
    group_by(relationship) %>%
    summarise(time_mean = mean(time_spent, na.rm = TRUE),
              time_sd = sd(time_spent, na.rm = TRUE),
              count = n()) %>%
    ungroup()

## calculate mean and sd by territory
time_by_territory_leo_calculations <- 

    time_by_relationship_leo_per_id %>%
    group_by(territory_name) %>%
    summarise(time_mean = mean(time_spent, na.rm = TRUE),
              time_sd = sd(time_spent, na.rm = TRUE),
              count = n()) %>%
    ungroup()


## ----extract-average-time-by-relationship-leo---------------------------------
# create list of relationship types
relationship_types <- c("own", "ally", "neutral", "rival", "non_contrada") 

## loop to create object with overall average and sd of time spent by relationship type
for (rel_type in unique(relationship_types)) {

    ## average
    time_average_leo_tmp <- 

    time_by_relationship_leo_calculations %>%
    mutate(relationship = case_when(relationship == "non-contrada" ~ "non_contrada",
                                         TRUE ~ as.character(relationship))) %>%
    filter(relationship == rel_type) %>%
    pull(time_mean)

    ## create object
    assign(paste0("time_average_leo_", rel_type), 
           time_average_leo_tmp)

    ## standard deviation
    time_sd_leo_tmp <- 

    time_by_relationship_leo_calculations %>%
    mutate(relationship = case_when(relationship == "non-contrada" ~ "non_contrada",
                                         TRUE ~ as.character(relationship))) %>%
    filter(relationship == rel_type) %>%
    pull(time_sd)

    ## create object
    assign(paste0("time_sd_leo_", rel_type), 
           time_sd_leo_tmp) 
    
}



## ----create-order-visits-leo--------------------------------------------------
## information on territories visited from A.R.K.'s notes
giro_visits_leo <- 
    
    giro_visits %>% 
    mutate(contrada = str_sub(contrada, 1, 3)) %>%
    filter(contrada == "leo") %>% 
    mutate(territory_name = str_to_sentence(territory_site)) %>% 
    filter(territory_site != "patronal-site") %>% 
    add_row(territory_name = "Empty") %>% 
    add_row(territory_name = "Fortezza") %>% 
    arrange(order_visited) %>% 
    mutate(order = 1:n()) %>% 
    mutate(order = if_else(order < 18, as.numeric(order), NA_real_)) %>%
    arrange(territory_name) %>%
    select(c(territory_name, order))


## ----table-time-by-territory-leo----------------------------------------------
## mean time per territory
time_by_territory_leo <- 
    
    time_by_territory_leo_calculations %>%
    mutate(relationship = case_when(territory_name %in% list_own_leo ~ "own",
                                    territory_name %in% list_ally_leo ~ "ally",
                                    territory_name %in% list_rival_leo ~ "rival",
                                    territory_name %in% list_neutral_leo ~ "neutral",
                                    territory_name %in% list_non_contrada ~ "non-contrada",
                                    territory_name == "Empty" ~ "empty")) %>%
    mutate(type = "contrada") %>%
    select(territory_name, type, relationship, time_mean, time_sd)

## add information on order of visits
time_by_territory_leo <- 
       
    ## compatibility for R v4.0 and v3.5
    bind_cols(time_by_territory_leo, 
              rename(giro_visits_leo,
                     territory_name1 = territory_name)) %>% 
    select(territory_name, type, relationship, time_mean, time_sd, order)  

## mean time overall
time_overall_leo <- 

    time_by_relationship_leo_calculations %>%
    mutate(territory_name = "Total") %>%
    mutate(type = "total") %>%
    select(territory_name, type, everything(), -count)


## ----create-giro-map-leo, eval = TRUE-----------------------------------------
## the cleanest way to make the map showing the GPS routes and time
## calculations is to define multiple layers which you can add together

## the code below is iterative, so that it should work with any given contrada

##################################################
## define a base map
## this outlines the contrade in planar coordinates

base_map <- 

    ggplot() +
    layer_spatial(data = data_boundaries
                , alpha = 0) 

##################################################
## define a layer with empty boundaries

## ggplot layer
layer_boundaries_empty <-  function (x) {
    
    x +
        ## from base_map
        layer_spatial(data = data_boundaries
                    , alpha = 0
                      ## greyscale: remove black
                    , colour = "black"
                      )
    
}

##################################################
## define a layer with osm roads

## define aesthetics

## osm data colour and size
osm_colour <- "gray60" # gray80 in general map
osm_size <- 0.1 # 0.2 in general map

## set projection
st_crs(siena_osm$osm_polygons) <- 4326  # or whatever projection your data is in
st_crs(siena_osm$osm_lines) <- 4326  # or whatever projection your data is in

## ggplot layer
layer_osm <-  function (x) {
    
    x +
        ## add osm data: polygons (e.g. square, roundabouts) with no fill
        geom_sf(data = siena_osm$osm_polygons
              , alpha = 0
              , color = osm_colour
              , size = osm_size) +
        ## add osm data: lines that represent roads
        geom_sf(data = siena_osm$osm_lines
              , color = osm_colour
              , size = osm_size)
    
}

##################################################
## define a layer with the time spent in each territory

## extract time spent in each territory
## for specific contrada

## use object from above that calculated time by relationship for each individual
time_by_relationship_leo_per_id
        
## assign a relationship to each territory name
time_in_territory_leo_tmp <- 
        
        time_by_relationship_leo_per_id %>%
        group_by(contrada, territory_name) %>%
        summarise(time_spent = mean(time_spent, na.rm = TRUE)) %>%
        filter(territory_name != "Empty") %>%
        ungroup()##  %>%
        ## add_row(contrada = "leo",
        ##         territory_name = "Fortezza",
        ##         time_spent = 0)

## boundaries for each contrada

## clean data
boundaries <-    
    
    data_boundaries %>% 
    fortify(region = "Name") %>% 
    rename(lon = long, contrada = id) %>%
    select(lon, lat, contrada)

## boundaries
boundaries_leo_tmp <- 
                   
        boundaries %>%
        mutate(relationship = case_when(contrada %in% list_own_leo ~ "own",
                                        contrada %in% list_ally_leo ~ "ally",
                                        contrada %in% list_rival_leo ~ "rival",
                                        contrada %in% list_neutral_leo ~ "neutral",
                                        contrada %in% list_non_contrada ~ "non-contrada")) %>%
        select(lon, lat, contrada, relationship) %>%
        rename(territory_name = contrada)

## combine object with territory coordinates and the time in each territory    
time_spent_in_territory_leo_tmp <- 
                                
        merge(boundaries_leo_tmp, time_in_territory_leo_tmp, all.x = TRUE) %>%
        as_tibble()

## define aesthetics

time_spent_label <- "time (mins)"

## ggplot layer
layer_time_spent <- function(x) {
    
    x +
        geom_polygon(data = time_spent_in_territory_leo_tmp
                   , aes(lon
                       , lat
                       , group = territory_name
                       , fill = time_spent
                         )
                   , colour = "black"
                   , size = 0 
                   , alpha = 1
                     ) +
        scale_fill_gradient2(name = time_spent_label
                           , low = "#7570b3"
                           , mid = "#f7f7f7"
                           , high =  "#1b9e77" 
                           , midpoint = 35
                           , breaks = c(0, 20, 40, 60)
                           , limits = c(0, 70) # so that we can use same legend
                           , guide = guide_colorbar(ticks.colour = "black",
                                                    frame.colour = "black")
                            )
        ## greyscale version
        ## scale_fill_gradient(name = time_spent_label
        ##                   , low = "white"
        ##                   , high = "grey50" #"#005AB5" # "#132B43" 
        ##                   , breaks = c(0, 20, 40, 60)
        ##                   , limits = c(0, NA) # changed so that we can use same legend
        ##                   , guide = guide_colorbar(ticks.colour = "black",
        ##                                            frame.colour = "black")
        ##                     )
    
}

##################################################
## define a layer for the theme

## define aesthetics

size_points_route <- 0.5

## ggplot layer
layer_routes <- function (x) { 
    
    x +
        geom_path(data = route_leo
                , aes(lon
                    , lat
                    , group = id),
                , size = size_points_route
                , colour = "#d95f02"
                , alpha = 0.25
                  )
    
}

##################################################
## define a layer for the buildings (e.g. church, societa)

## object with coordinate of buildings and relationship between the contrade
buildings_leo <- 
        
        locations %>%
        mutate(relationship = case_when(contrada %in% list_own_leo ~ "own",
                                        contrada %in% list_ally_leo ~ "ally",
                                        contrada %in% list_rival_leo ~ "rival",
                                        contrada %in% list_neutral_leo ~ "neutral",
                                        contrada %in% list_non_contrada ~ "non-contrada")) %>%
        filter(building == "church" |
               building == "worship-leo" |
               building == "societa" & relationship == "ally") %>%
        select(lon, lat, contrada, building, relationship) %>%
        rename(territory_name = contrada) %>% 
        mutate(contrada = "leo")

## define aesthetics
   
## size of points in maps
size_points <- 3

## thickness of point
point_thickness <- 2

## text for legends
buildings_label <- ""
contrada_church_label <- bquote(paste(italic("contrada"), " church"))
contrada_societa_label <- "clubhouse (ally)"
saint_church_label <- "patronal site"

## ggplot layer
layer_buildings <- function (x) {

    x +
        geom_point(data = buildings_leo 
                 , aes(lon
                     , lat 
                     , shape = building)
                 , size = size_points * 0.75
                 , stroke = point_thickness/1.5
                 , colour = "black" 
                 , fill = "white" 
                 , alpha = 1
                 , inherit.aes = FALSE) +
        scale_shape_manual(name = buildings_label
                         , guide = "legend"
                         , labels = c(contrada_church_label,
                                      contrada_societa_label,
                                      saint_church_label)
                         , values = c(21, 24, 22))

}

##################################################
## define a layer for the relationships

## get centroids
## join centroids list to name list
centroids <-

    bind_cols(# contrada names
        contrada = 
            data_boundaries %>% 
            fortify(region = "Name") %>% 
            rename(lon = long, contrada = id) %>%
            select(lon, lat, contrada) %>%
            select(contrada) %>%
            distinct,
            # data centroids
        data_boundaries %>% 
        coordinates() %>% 
        as.data.frame() %>%
        rename(lon = V1, lat = V2) # change name
    ) %>%
    as_tibble()

## centroids of each contrada
## labelled by relationship type
centroids_leo <- 
    
    centroids %>%
    mutate(relationship = case_when(contrada %in% list_own_leo ~ "own",
                                    contrada %in% list_ally_leo ~ "ally",
                                    contrada %in% list_rival_leo ~ "rival",
                                    contrada %in% list_neutral_leo ~ "neutral",
                                    contrada %in% list_non_contrada ~ "non-contrada")) %>%
    filter(relationship == "ally" |
           relationship == "rival"  |
           relationship == "own" ) %>% 
    rename(territory_name = contrada) %>%
    mutate(contrada = "leo")

## define aesthetics
label_text_size <- 5.5

## ggplot layer
layer_relationships <- function (x## , i
                                 ) {

    x +
        geom_label(data = filter(centroids_leo,
                                 territory_name == "Leocorno"),
                   aes(lon
                     , lat
                     , label = relationship
                       ) 
               , alpha = 0.75
               , size = label_text_size
               , label.size = 0
               , nudge_x = 0.00375
               , nudge_y = 0.00125
               , hjust = 0
                 ) +
        geom_segment(data = filter(centroids_leo,
                                 territory_name == "Leocorno"),
                     aes(lon
                       , lat
                       , xend = lon + 0.00375
                       , yend = lat + 0.00125
                         ) 
                 ) +
        geom_label(data = filter(centroids_leo,
                                 territory_name == "Civetta"),
                   aes(lon
                     , lat
                     , label = relationship
                       ) 
                 , alpha = 0.75
                 , size = label_text_size
                 , label.size = 0
                 , nudge_x = 0.0075
                 , nudge_y = 0.0025
                 , hjust = 0
                   ) +
        geom_segment(data = filter(centroids_leo,
                                 territory_name == "Civetta"),
                     aes(lon
                       , lat
                       , xend = lon + 0.0075
                       , yend = lat + 0.0025
                         ) 
                 ) +
        geom_label(data = filter(centroids_leo,
                                 territory_name == "Aquila"),
                   aes(lon
                     , lat
                     , label = relationship) 
                 , alpha = 0.75
                 , size = label_text_size
                 , label.size = 0
                 , nudge_x = -0.005
                 , hjust = 1
                   ) +
        geom_segment(data = filter(centroids_leo,
                                   territory_name == "Aquila"),
                     aes(lon
                       , lat
                       , xend = lon - 0.005
                       , yend = lat
                         ) 
                     ) +
        geom_label(data = filter(centroids_leo,
                                 territory_name == "Giraffa"),
                   aes(lon
                     , lat
                     , label = relationship) 
                 , alpha = 0.75
                 , size = label_text_size
                 , label.size = 0
                 , nudge_x = 0.0025
                 , nudge_y = 0.0025
                 , hjust = 0
                   ) +
        geom_segment(data = filter(centroids_leo,
                                   territory_name == "Giraffa"),
                     aes(lon
                       , lat
                       , xend = lon + 0.0025
                       , yend = lat + 0.0025
                         ) 
                 ) +
        geom_label(data = filter(centroids_leo,
                                 territory_name == "Pantera"),
                   aes(lon
                     , lat
                     , label = relationship) 
                 , alpha = 0.75
                 , size = label_text_size
                 , label.size = 0
                 , nudge_x = -0.0025
                 , nudge_y = -0.00125
                 , hjust = 1
                   ) +
        geom_segment(data = filter(centroids_leo,
                                   territory_name == "Pantera"),
                     aes(lon
                       , lat
                       , xend = lon - 0.0025
                       , yend = lat - 0.00125
                         ) 
                 ) +
        geom_label(data = filter(centroids_leo,
                                 territory_name == "Istrice"),
                   aes(lon
                     , lat
                     , label = relationship) 
                 , alpha = 0.75
                 , size = label_text_size
                 , label.size = 0
                 , nudge_x = -0.00125
                 , nudge_y = 0.00125
                 , hjust = 1
                   ) +
        geom_segment(data = filter(centroids_leo,
                                   territory_name == "Istrice"),
                     aes(lon
                       , lat
                       , xend = lon - 0.00125
                       , yend = lat + 0.00125
                         ) 
                 ) +
        geom_label(data = filter(centroids_leo,
                                 territory_name == "Tartuca"),
                   aes(lon
                     , lat
                     , label = relationship) 
                 , alpha = 0.75
                 , size = label_text_size
                 , label.size = 0
                 , nudge_y = -0.002075 # -0.002175
                 , vjust = 1
                   ) +
        geom_segment(data = filter(centroids_leo,
                                   territory_name == "Tartuca"),
                     aes(lon
                       , lat
                       , xend = lon
                       , yend = lat - 0.002175
                         ) 
                     )
    
}

##################################################
## define a layer for the theme

## define aesthetics

## text_size <- 3
## size_points <- 4

## size of text
## needs to be defined as theme_no_name requires it
## label_text_size <- 5.5
label_text_size <- 4 # 3.5
theme_size <- (14/5) * label_text_size

## gpplot layer
layer_theme <- function (x) {
    
    x +
        xlim(min(boundaries$lon), max(boundaries$lon)) + 
        ylim(min(boundaries$lat), max(boundaries$lat)) +
        guides(shape = guide_legend(order = 1, ncol = 2)) +
        ## make fill have boxes: fill = guide_legend(nrow = 1)) +
        theme_no_name()
    
}

##################################################
## create final map
## that combines the layers

fullname_civ <- "Civetta"
fullname_leo <- "Leocorno"

## add layers
map_leo <- 
        
        base_map %>%
        layer_time_spent() %>%    
        ## layer_osm() %>%
        layer_boundaries_empty() %>%
        layer_routes() %>%
        layer_theme() %>%
        layer_buildings() %>%
        layer_relationships() +
        labs(subtitle = paste0("*", fullname_leo, "* (*n* = ", giro_n_analysed_leo, ")")) +
        ## ## add scale bar
        ## annotation_scale(width_hint = 0.3, # to set length to 0.5 km
        ##                  pad_x = unit(0.125, "cm"),
        ##                  location = "br") +
        ## annotation_north_arrow(location = "tr",
        ##                        which_north = "true",
        ##                        pad_y = unit(0.5, "cm"),
        ##                        height = unit(0.75, "cm"),
        ##                        width = unit(0.75, "cm"),
        ##                        style = north_arrow_orienteering) +
        theme(plot.subtitle = ggtext::element_markdown(hjust = 0.5))


## ----giro-cluster-analysis-leo, eval = TRUE, cache = TRUE---------------------
##################################################
## dbscan parameters

## calculating max_neighbourhood_radius from:
## https://www.datanovia.com/en/lessons/dbscan-density-based-clustering-essentials/

## ## plot graph to find knee value
## dbscan::kNNdistplot(route_leo_01, k = 5) # any route could be used
## abline(h = 0.0001, lty = 2) # check

## set maximum neighbourhood radius for points in a cluster
max_neighbourhood_radius <- 0.0001

## set minimum number of points to be included in cluster 
## as gps signal is every 5 seconds, there are 12 points a minute
## hymn lasts around 1 minute, so set time to be around 2 minutes
min_number_points <- 12 * 2 

##################################################
## make clusters per participant 

## this is through a large for loop which will
## a) create separate routes per participant
## b) create clusters using dbscan and extract the points in each cluster
## c) find centroids of each cluster
## d) create circles of radius 25m (polygons) around centroids
## e) ask if locations of buildings are within the polygons
## f) create a table per participant of visited locations

## NB this does need to be done everytime, as you have to reset lon and lat as coordinates

## get different locations

locations_all <-

    locations
    
locations_churches <-

    locations %>% 
    filter(building == "church")

locations_societa <-

    locations %>% 
    filter(building == "societa")

locations_other <-

    locations %>%
    filter(building != "church" & building != "societa")

## set lon and lat as coordinates

coordinates(locations_all) <- ~ lon + lat

coordinates(locations_churches) <- ~ lon + lat

coordinates(locations_societa) <- ~ lon + lat

## references

## creation of clusters for b) from:
## https://www.datanovia.com/en/lessons/dbscan-density-based-clustering-essentials/

## creation of polygons for d) from:
## https://gis.stackexchange.com/questions/121489/1km-circles-around-lat-long-points-in-many-places-in-world

## check overlap for e) from:
## https://gis.stackexchange.com/questions/121489/1km-circles-around-lat-long-points-in-many-places-in-world

## note that for loop currently has several commented out sections:
## these signal points at which the loop could stop if someone wanted
## to extract data at that point

## practice data (with fewer participants)
## route_leo <- route_leo %>% filter(id == "01" | id == "02")

## create list
list_df <- list()

for (participant in unique(route_leo$id)) {

    ## a) create separate routes per participant
    
    route_tmp <-
        
        route_leo %>%
        filter(id == participant) %>%
        select(lat, lon)
    
    ## b) create clusters using dbscan and extract the points in each cluster

    db_tmp <-
        
        fpc::dbscan(route_tmp,
                    eps = max_neighbourhood_radius,
                    MinPts = min_number_points)

    clustered_route_tmp <-
    
        bind_cols(route_tmp, as_tibble(db_tmp$cluster)) %>%
        rename(cluster_no = value)
    
    ## c) find centroids of each cluster
    
    clustered_centroids_tmp <-
        
        clustered_route_tmp %>%
        group_by(cluster_no) %>%
        summarise(lat = mean(lat),
                  lon = mean(lon))
    
    ## d) create circles of radius 25m (polygons) around centroids

    clustered_centroids_sp_tmp <-
        
        SpatialPointsDataFrame(clustered_centroids_tmp[, c("lon", "lat")],
                               data.frame(ID = seq(1:nrow(clustered_centroids_tmp))),
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

    projected_point_tmp <- 
        
        spTransform(clustered_centroids_sp_tmp,
                    CRS( "+init=epsg:6875" )) # epsg: https://epsg.io/?q=italy

    projected_circles_tmp <- 
        
        gBuffer(projected_point_tmp,
                width = 25, # radius of 25 m
                byid = TRUE) 
    
    circles_sp_tmp <- 
        
        spTransform(projected_circles_tmp,
                    CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
    
    proj4string(locations_all) <- 
        
        proj4string(circles_sp_tmp)
    
    proj4string(locations_churches) <- 
        
        proj4string(circles_sp_tmp)
    
    proj4string(locations_societa) <- 
        
        proj4string(circles_sp_tmp)
    
    ## e) ask if locations of buildings are within the polygons

    ## churches
    
    ## overlap between cluster and point
    overlap_with_churches <-
    
        over(circles_sp_tmp, locations_churches) %>%
        mutate(id = participant)

    list_df[[participant]] <- overlap_with_churches

    ## for overlap
    assign(paste0("overlap_leo_", participant), list_df[[participant]])
       
}

## create final dataframe
df_final_leo <- bind_rows(list_df)


## ----giro-clusters-summary-leo, eval = TRUE-----------------------------------
## descriptives of church entries

## create unique counts per individual
visited_churches_by_id_leo <-
    
    df_final_leo %>%
    filter(!is.na(contrada)) %>%
    group_by(id, contrada) %>%
    summarise(n = n()) %>%
    mutate(n = case_when(n > 1 ~ 1, # make counts at single location only once
                         n == 1 ~ 1))

## total number of church visits
visited_churches_by_contrada_leo <- 
    
    visited_churches_by_id_leo %>%
    group_by(contrada) %>%
    summarise(total = sum(n))

## first select contrada info
contrada_info_terzo <- 

    contrada_info %>%
    select(contrada, terzo)

## make tibble of visits for table
visited_churches_for_table_leo <- 

    visited_churches_by_contrada_leo %>%
    arrange(contrada) %>%
    merge(contrada_info_terzo, all = TRUE) %>%
    mutate(total = if_else(is.na(total), 0, total)) %>%
    mutate(percentage = (total/giro_n_analysed_leo) * 100) %>%
    mutate(relationship = case_when(contrada %in% list_own_leo ~ "own",
                                    contrada %in% list_ally_leo ~ "ally",
                                    contrada %in% list_rival_leo ~ "rival",
                                    contrada %in% list_neutral_leo ~ "neutral",
                                    contrada == "Empty" ~ "empty")) %>%
    arrange(terzo) %>%
    select(terzo, contrada, relationship, total, percentage) %>%
    tibble_by_terzo() 

# create list of relationship types
relationship_types <- c("own", "ally", "neutral", "rival", "non_contrada") 

## loop to create object with average visits by relationship type
for (rel_type in unique(relationship_types)) {

    ## create object
    assign(paste0("visits_leo_", rel_type), 
           visited_churches_for_table_leo %>% 
           group_by(relationship) %>% 
           summarise(mean = mean(total)) %>% 
           filter(relationship == rel_type) %>%
           pull(mean)
    )

}



## ----table-combine-time-order-cluster-leo, eval = TRUE------------------------
## create table with time spent and church visits

## add missing areas (not included in cluster analysis as not visited)
names_to_add <- c("Campo", "Duomo", "Empty", "Fortezza")

## rival name to populate table (not included in cluster analysis as not visited)
rival_civ <- "Leocorno"
rival_leo <- "Civetta"

## add above territories
cluster_visits_leo <- 
    
        visited_churches_by_contrada_leo %>% 
        add_row(contrada = names_to_add) %>%
        add_row(contrada = rival_leo, total = 0) %>%
        arrange(contrada)

## create table
master_table_leo <- 
                 
    bind_cols(time_by_territory_leo, cluster_visits_leo) %>%
    select(-type, -contrada) %>%  
    filter(territory_name != "Empty") %>% 
    arrange(match(relationship, c("own", "ally", "neutral", "rival", "non-contrada")), 
            desc(time_mean)) %>%
    select(relationship, everything()) %>%
    mutate(type = "contrada") %>%
    mutate(territory_name = case_when(territory_name == "Duomo" ~ "cathedral",
                                      territory_name == "Campo" ~ "central square",
                                      territory_name == "Fortezza" ~ "fortress",
                                      TRUE ~ paste0("\\emph{", territory_name, "}"))) %>%
    add_row(relationship = "own", territory_name = "own \\contrada",
            time_mean = time_average_leo_own, 
            time_sd = time_sd_leo_own, 
            order = NA, total = ceiling(visits_leo_own), type = "all") %>%
    add_row(relationship = "ally", territory_name = "alliance",
            time_mean = time_average_leo_ally, 
            time_sd = time_sd_leo_ally, 
            order = NA, total = ceiling(visits_leo_ally), type = "all") %>%
    add_row(relationship = "neutral", territory_name = "neutral relationship",
            time_mean = time_average_leo_neutral, 
            time_sd = time_sd_leo_neutral, 
            order = NA, total = ceiling(visits_leo_neutral), type = "all") %>%
    add_row(relationship = "rival", 
            territory_name = paste0("rivalry (\\", 
                                    str_to_lower(str_sub(rival_leo, 1, 3)), ")"),
                                   ## "rivalry (\\emph{rival_leo})",
            time_mean = time_average_leo_rival, 
            time_sd = time_sd_leo_rival,
            order = NA, total = visits_leo_rival, type = "all") %>%
    add_row(relationship = "non-contrada", territory_name = "non-\\emph{contrada}",
            time_mean = time_average_leo_non_contrada, 
            time_sd = time_sd_leo_non_contrada, 
            order = NA, total = NA, type = "all") %>%
    group_by(relationship) %>%
    arrange(match(relationship, c("own", "ally", "neutral", "rival", "non-contrada")), 
            type,
            desc(time_mean)) %>%
    ungroup() %>%
    mutate(time_mean_sd = if_else(as.numeric(time_sd) < 10, 
                                  paste0(format_one(time_mean), " \\hspace{\\onedigit}(", format_one(time_sd), ")"), 
                                  paste0(format_one(time_mean), " (", format_one(time_sd), ")"))) %>%
    select(territory_name, 
           time_mean_sd, 
           total) %>%
    slice(-2, -21)
    
caption_text_leo <- 
                 
    "\\label{SI-tab:giro-calcs-leo}Time spent and participants in \\contrada churches for the \\leo procession"

caption_text_leo_short <- 
                                  
    "\\leo procession by territory type"

## footnote
table_note <- 
    
    "Table ordered by territory type, then in descending order by mean time spent in each area of the \\\\citycentre[w]."

time_spent_note <- 

    "Time spent in each area of the \\\\citycentre[w], in minutes; the overall value by territory type is the mean across all corresponding areas, with the data pooled across participants."

cluster_note <- 
    
    paste0("Estimated number of participants, out of the total ", giro_n_analysed_leo, ", who entered the \\\\contrada church at least once; the overall value by territory type is the mean across all corresponding areas, rounded up to the nearest integer.")

## formatting for table
## line separations between territory types
linesep_civ <- 

    c("\\addlinespace", 
      rep("", 4), "\\addlinespace", 
      rep("", 11),  "\\addlinespace", 
      "\\addlinespace", 
      rep("", 3)
      )
    
linesep_leo <- 
    
    c("\\addlinespace", 
      rep("", 2), "\\addlinespace", 
      rep("", 13),  "\\addlinespace", 
      "\\addlinespace", 
      rep("", 3)
      )

## indents for individual areas
indent_civ <- 

    c(3:6, 8:18, 21:23)

indent_leo <- 
    
    c(3:4, 6:18, 21:23)

## header
table_col_names <- c("territory type\\textsuperscript{1}",
                     "mean minutes\\textsuperscript{2} ($\\mathit{SD}$)",
                     "\\emph{n} inside church\\textsuperscript{3}"
                     )

## print NAs in table with hyphen
options(knitr.kable.NA = "---")

## table
all_table_leo <- 

    knitr::kable(master_table_leo,
                 format = "latex",
                 booktabs = TRUE,
                 escape = FALSE,
                 digits = c(0, 1, 0),
                 align = c("l", "r", "r"),
                 linesep = linesep_leo,
                 col.names = table_col_names,
                 caption.short = caption_text_leo_short,
                 caption = caption_text_leo
                 ) %>%
    add_indent(indent_leo, level_of_indent = 1) %>%   
    footnote(number = c(table_note, 
                        time_spent_note,
                        cluster_note
                        ), 
             footnote_as_chunk = FALSE, 
             escape = FALSE, 
             threeparttable = TRUE, # TRUE for narrow table 
             fixed_small_size = TRUE)



## ----visited-churches-summary-leo, eval = TRUE, purl = TRUE-------------------
## extract minimum number of visits to churches (excluding zero visits)
visits_min_leo <-
    
    visited_churches_by_contrada_leo %>%
    filter(total > 0) %>%
    summarise(min = min(total, na.rm = TRUE)) %>%
    pull(min) %>%
    as.integer()

## extract minimum number of visits to churches
visits_max_leo <-
    
    visited_churches_by_contrada_leo %>%
    summarise(max = max(total, na.rm = TRUE)) %>%
    pull(max) %>%
    as.integer()

## ----average-coverage---------------------------------------------------------
## mean coverage
percentage_non_interruptions_mean <-
    
    (percentage_non_interruptions_civ_mean + percentage_non_interruptions_leo_mean)/2

## mean coverage
percentage_non_interruptions_sd <-
    
    (percentage_non_interruptions_civ_sd + percentage_non_interruptions_leo_sd)/2

##################################################

## calculate coverage = number of points counted over points expected
table_positions_percentage_expected <- 

    bind_rows(table_points_per_individual_civ,
              table_points_per_individual_leo) %>%
    ungroup() %>%
    summarise(mean = mean(percentage_expected),
              sd = sd(percentage_expected))

## mean
percentage_non_interruptions_mean <- 
    
    table_positions_percentage_expected %>%
    select(mean) %>% 
    as.numeric()

## sd
percentage_non_interruptions_sd <- 
    
    table_positions_percentage_expected %>%
    select(sd) %>% 
    as.numeric()



## ----giro-temp----------------------------------------------------------------
## temperature
## from SienaMeteo_2018
temp_civ <- 28.7
temp_leo <- 28.4


## ----giro-summary-statistics-average------------------------------------------
## total time
total_time_average <- 
    
    (as.numeric(total_time_civ) + as.numeric(total_time_leo))/2

## total distance
total_distance_average <- 
    
    (distance_total_civ + distance_total_leo)/2


## ----count-location-visits----------------------------------------------------
## counts from A.R.K.'s notes
n_location_visits <- 19L # this includes a single count of own contrada

n_location_visits_civ_am <- 9L 

n_location_visits_civ_pm <- 9L 

n_location_visits_leo_am <- 11L

n_location_visits_leo_am_contrade <-
    
    as.integer(n_location_visits_leo_am - 1)

n_location_visits_leo_pm <- 7L


## ----average-time-calculations-combined---------------------------------------
## get average time and sd for each area in the city centre

## calculate time by relationship for each individual
time_by_relationship_civ_leo_per_id <-
                                          
    bind_rows(route_relationships_civ, route_relationships_leo) %>%
    group_by(contrada, id, territory_name, relationship) %>%    
    summarise(time_spent = sum(time_diff, na.rm = TRUE)) %>%
    mutate(time_spent = as.numeric(time_spent, units = "mins"))

## manually add Fortress with time spent = 0
time_by_relationship_civ_leo_per_id <-

    time_by_relationship_civ_leo_per_id %>%
    ungroup() %>%
    group_by(contrada, id) %>%
    mutate(territory_name = "Fortezza", relationship = "non-contrada", time_spent = 0) %>%
    unique() %>%
    bind_rows(time_by_relationship_civ_leo_per_id, .)

## calculate mean and sd by relationship
time_by_relationship_civ_leo_calculations <- 

    time_by_relationship_civ_leo_per_id %>%
    ungroup() %>%    
    group_by(relationship) %>%
    summarise(time_mean = mean(time_spent, na.rm = TRUE),
              time_sd = sd(time_spent, na.rm = TRUE),
              count = n()) %>%
    ungroup()

# create list of relationship types
area_type <- c("own", "ally", "neutral", "rival", "non_contrada")

## loop across each area
for (each_relationship in area_type) {

    ## average
    time_mean_civ_leo_tmp <- 
        
        time_by_relationship_civ_leo_calculations %>%
        mutate(relationship = 
                   case_when(relationship == "non-contrada" ~ "non_contrada",
                             TRUE ~ as.character(relationship))) %>%
        filter(relationship == each_relationship) %>%
        pull(time_mean)
    
    ## create object
    assign(paste0("time_mean_", each_relationship), time_mean_civ_leo_tmp)
    
    ## standard deviation
    time_sd_civ_leo_tmp <- 
        
        time_by_relationship_civ_leo_calculations %>%
        mutate(relationship = 
                   case_when(relationship == "non-contrada" ~ "non_contrada",
                             TRUE ~ as.character(relationship))) %>%
        filter(relationship == each_relationship) %>%
        pull(time_sd)
    
    ## create object
    assign(paste0("time_sd_", each_relationship), time_sd_civ_leo_tmp) 
    
}

## calculate mean and sd by other contrada
time_by_relationship_civ_leo_calculations <- 

    time_by_relationship_civ_leo_per_id %>%
    ungroup() %>%    
    filter(relationship != "own" | relationship != "non-contrada") %>%
    mutate(contrada_type = "other") %>%
    group_by(contrada_type) %>%
    summarise(time_mean = mean(time_spent, na.rm = TRUE),
              time_sd = sd(time_spent, na.rm = TRUE),
              count = n()) %>%
    ungroup()

## average
time_mean_other_contrada <- 
    
    time_by_relationship_civ_leo_calculations %>%
    pull(time_mean)

## average
time_sd_other_contrada <- 
    
    time_by_relationship_civ_leo_calculations %>%
    pull(time_sd)

## calculate mean and sd by other territories (including non-contrada)
time_by_relationship_civ_leo_calculations <- 

    time_by_relationship_civ_leo_per_id %>%
    ungroup() %>%    
    filter(relationship != "own") %>%
    mutate(territory_type = "other") %>%
    group_by(territory_type) %>%
    summarise(time_mean = mean(time_spent, na.rm = TRUE),
              time_sd = sd(time_spent, na.rm = TRUE),
              count = n()) %>%
    ungroup()

## average
time_mean_other_territory <- 
    
    time_by_relationship_civ_leo_calculations %>%
    pull(time_mean)

## average
time_sd_other_territory <- 
    
    time_by_relationship_civ_leo_calculations %>%
    pull(time_sd)

## calculate mean and sd by area
time_by_relationship_civ_leo_calculations <- 

    time_by_relationship_civ_leo_per_id %>%
    ungroup() %>%    
    group_by(territory_name) %>%
    summarise(time_mean = mean(time_spent, na.rm = TRUE),
              time_sd = sd(time_spent, na.rm = TRUE),
              count = n()) %>%
    ungroup()

## average cathedral
time_mean_cathedral <- 
    
    time_by_relationship_civ_leo_calculations %>%
    filter(territory_name == "Duomo") %>% 
    pull(time_mean)

## average cathedral
time_sd_cathedral <- 
    
    time_by_relationship_civ_leo_calculations %>%
    filter(territory_name == "Duomo") %>% 
    pull(time_sd)

## average central square
time_mean_central_square <- 
    
    time_by_relationship_civ_leo_calculations %>%
    filter(territory_name == "Campo") %>% 
    pull(time_mean)

## sd central square
time_sd_central_square <- 
    
    time_by_relationship_civ_leo_calculations %>%
    filter(territory_name == "Campo") %>% 
    pull(time_sd)








## ----info-giro-civ, eval = TRUE, include = TRUE, echo = FALSE, results = "asis"----
## print
all_table_civ


## ----info-giro-leo, eval = TRUE, include = TRUE, echo = FALSE, results = "asis"----
## print
all_table_leo

## ----giro-maps,  eval = TRUE, include = TRUE, message = FALSE, warning = FALSE, echo = FALSE, fig.scap = "\\gps routes of the \\civ and \\leo procession", fig.cap = caption_text, out.width = "1\\linewidth", fig.width = 7, fig.height = 4, results = "asis", fig.align = "center", fig.keep = "all", fig.show = "asis", eval.after = "fig.cap"----

## text for figure caption
caption_text <-  
    
    ## paste0("The \\gps routes of the \\civ (total $n = ", giro_n_analysed_civ, "$) and \\leo (total $n = ", giro_n_analysed_leo, "$) procession. The location of all \\contrada churches, ally \\contrada clubhouses, and the patronal site are indicated. The 19 territories, 17 \\contrade plus two non-\\contrade territories, are shaded with the time spent in each territory (in minutes). Labels for own, ally, and rival \\contrade territories are provided, while those for neutral and non-\\contrada territories are not. The patronal site for \\civ is the church of Santa Maria in Provenzano and for \\leo it is the basilica of San Francesco (\\leo).") # , both in \\gir territory.")

    paste0("The \\civ and \\leo procession. The 17 \\contrada territories and the three non-\\contrade areas are shaded by the mean time spent in each one across participants. The \\gps route of each participant is shown in orange. Labels are only included for own \\contrada, allies, and rival. The location of each \\contrada church, the clubhouse of each ally, and the patronal site are indicated. The patronal site of \\civ and \\leo are different churches located in the territory of \\gir.") # for \\civ is the church of Santa Maria in Provenzano and for \\leo it is the basilica of San Francesco, both of which are in the same territory.") # , both in \\gir territory.")

## plot
ggpubr::ggarrange(
            map_civ, 
            map_leo +
            ## add scale bar
            annotation_scale(width_hint = 0.3, # to set length to 0.5 km
                             pad_x = unit(0.125, "cm"),
                             location = "br") +
            annotation_north_arrow(location = "tr",
                                   which_north = "true",
                                   pad_y = unit(0.5, "cm"),
                                   height = unit(0.75, "cm"),
                                   width = unit(0.75, "cm"),
                                   style = north_arrow_orienteering)
          , 
            ncol = 2, nrow = 1,
            ## labels = c("Civetta", "Leocorno"),
            font.label = list(size = 12),
            ## font.label = list(face = "italic"),
            legend = "bottom",
            common.legend = TRUE
        ) 

## ## plot with licence
## annotate_figure(
##     ggpubr::ggarrange(
##                 map_civ, 
##                 map_leo +
##                 ## add scale bar
##                 annotation_scale(width_hint = 0.3, # to set length to 0.5 km
##                                  pad_x = unit(0.125, "cm"),
##                                  location = "br") +
##                 annotation_north_arrow(location = "tr",
##                                        which_north = "true",
##                                        pad_y = unit(0.5, "cm"),
##                                        height = unit(0.75, "cm"),
##                                        width = unit(0.75, "cm"),
##                                        style = north_arrow_orienteering)
##               , 
##                 ncol = 2, nrow = 1,
##                 ## labels = c("Civetta", "Leocorno"),
##                 font.label = list(size = 12),
##                 ## font.label = list(face = "italic"),
##                 legend = "bottom",
##                 common.legend = TRUE
##             ),
##     bottom = text_grob("Adam R. Kenny & Laura Fortunato; CC BY-SA 4.0", 
##                        hjust = 1, x = 1)
## )




## ----giro-map-old, eval = FALSE, include = TRUE, echo = FALSE, fig.scap = "The \\gps routes of the \\civ procession", fig.cap = paste0("The \\gps routes of the \\civ procession. The routes of the ", giro_n_analysed_civ," participants are in red. The location of all \\contrade churches, allied \\contrade clubhouses, and the patronal site are indicated. The territories of the 17 \\contrade and the two non-\\contrade are shaded with the time spent in each territory (in minutes). The allied and the rival \\contrade are labelled, while the neutral \\contrade and non-\\contrada territories are not. See \\cref{fig:giro-map-leo} for the \\leo version."), results = "asis", fig.align = 'center', fig.keep = "all", fig.show = "asis", eval.after = "fig.cap", purl = "FALSE"----
## ## print map of civetta
## map_civ


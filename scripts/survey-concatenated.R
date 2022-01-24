# SPDX-License-Identifier: GPL-3.0

## ----survey-participants, eval = TRUE-----------------------------------------
## n of ids
survey_n_recruited <- 
    
    survey %>%
    nrow()


## ----survey-recruited-by-contrada-and-period, eval = TRUE---------------------
## count number of participants by contrada
survey_n_contrada <- 

    survey %>%
    count(contrada) 

survey_contrada_stats <- 
    
    survey_n_contrada %>%
    mutate(percentage = (n/survey_n_recruited) * 100) %>%
    ungroup()

survey_contrade <- c("C05", "C06", "C08", "C11", "C16")

for (focal_contrada in survey_contrade) {
    
    ## filter contrada
    survey_contrada_stats_tmp <- 
        
        survey_contrada_stats %>%
        filter(contrada == focal_contrada)
    
    ## pull n    
    survey_contrada_stats_n <- 
        
        survey_contrada_stats_tmp %>%
        pull(n)
    
    ## create object
    assign(paste0("survey_n_", focal_contrada), 
           survey_contrada_stats_n)
    
    ## pull p
    survey_contrada_stats_p <- 
        
        survey_contrada_stats_tmp %>%
        pull(percentage)
    
    ## create object
    assign(paste0("survey_p_", focal_contrada), 
           survey_contrada_stats_p)
    
}


## ----survey-exclusion-withdrawn, eval = TRUE----------------------------------
## exclude participants who withdrew from questionnaire
survey_n_withdrawn <- 

    survey %>% 
    filter(is.na(id)) %>%
    pull_n()

## total following exclusion
survey <- 
    
    survey %>%
    filter(!is.na(id))

## number of participants after exclusion 
survey_n_analysed <- 
    
    survey %>%
    pull_n()


## ----survey-exclusion-experience, eval = TRUE---------------------------------
## count
survey %>%
    count(experience)

## number of participants to exclude
survey_n_experienced <- 
    
    survey %>%
    filter(str_detect(experience, "yes-contrada") | str_detect(experience, "same-id")) %>%
    pull_n()

## as percentage
survey_p_experienced <- 
    
    (survey_n_experienced / survey_n_recruited) * 100

## remove ids with experience and failure to complete questionnaire
survey <- 
    
    survey %>%
    filter(is.na(experience) | experience != "yes-contrada") %>%
    filter(is.na(experience) | experience != "yes-(same-id)")


## ----survey-exclusion-missing-survey------------------------------------------
## count
survey %>%
    count(is.na(gender) & is.na(better_statement))

## number of participants to exclude
survey_n_missing_data <- 
    
    survey %>%
    filter(is.na(gender) & is.na(better_statement)) %>%
    pull_n()

## as percentage
survey_p_missing_data <- 
    
    (survey_n_missing_data / survey_n_recruited) * 100

## remove ids who have no responses to survey data
survey <- 
    
    survey %>%
    filter(id != "320C16") %>%
    filter(id != "901C06") %>%
    filter(id != "902C06")


## ----survey-exclusion-misc, eval = TRUE---------------------------------------
## participants excluded for miscellaneous reasons

## number of participants to exclude
survey_n_excluded_misc <- 
    
    survey %>%
    ## this participant indicated in their reason for being a member that they were a guest
    ## they are therefore not a member
    filter(str_detect(member_other_text, "ospite")) %>% 
    pull_n()

## remove ids who were guest
survey <- 
    
    survey %>%
    filter(id != "464C06")


## ----survey-exclusion-analysed, eval = TRUE-----------------------------------
## number of participants excluded 
survey_n_excluded_additional <- 
    
    survey_n_withdrawn +
    survey_n_missing_data +
    survey_n_excluded_misc
    
## number of participants after exclusion 
survey_n_analysed <- 
    
    survey %>%
    pull_n()

## percentage of participants analyzed from recruited
survey_p_analysed <- 
    
    (survey_n_analysed / survey_n_recruited) * 100

## number of participants excluded
survey_n_excluded <- 
    
    survey_n_recruited - survey_n_analysed






## ----survey-non-responses, eval = TRUE----------------------------------------
## find minimum and maximum number of non-responses
survey_n_non_responses <- 
    
    survey %>%
    map(~sum(is.na(.))) %>%
    as_tibble() %>%
    # variables that are not analyzed
    select(-c(id:session, 
              years_away, 
              member_other_text, 
              experience, 
              age)) %>%
    gather() %>%
    summarise(min = min(value),
              max = max(value))

## minimum
## total
survey_n_non_responses_min <- 
    
    survey_n_non_responses %>%
    min()

## percentage
survey_p_non_responses_min <- 

    (survey_n_non_responses_min / survey_n_analysed) * 100
    
## maximum
## total
survey_n_non_responses_max <- 
    
    survey_n_non_responses %>%
    max()

## percentage
survey_p_non_responses_max <- 

    (survey_n_non_responses_max / survey_n_analysed) * 100




## ----n-demographic-questions--------------------------------------------------
## total number not answered
survey_n_no_response_demographic <- 
    
    survey %>% 
    filter(is.na(age)) %>% 
    pull_n()

## total number analysed
survey_n_response_demographic <- 
    
    survey_n_analysed - survey_n_no_response_demographic

## ----survey-age, eval = TRUE--------------------------------------------------
## NB we calculated age by subtracting the year of birth from the year of
## data collection while redacting data

## NA

## count
survey_n_age_NA <- 
    
    survey %>%
    filter(is.na(age)) %>%
    pull_n()

## perc
survey_p_age_NA <- 
    
    (survey_n_age_NA / survey_n_analysed) * 100

## n without NAs
survey_n_age_analysed <- 

    survey_n_analysed - survey_n_age_NA

## summarise age
survey_age_summary <- 
    
    survey %>%
    summary_stats("age")

## mean
survey_age_mean <- 

    survey_age_summary %>%
    pull(mean)

## sd
survey_age_sd <- 

    survey_age_summary %>%
    pull(sd)

## min
survey_age_min <- 

    survey_age_summary %>%
    pull(min) %>%
    as.integer()

## max
survey_age_max <- 

    survey_age_summary %>%
    pull(max) %>%
    as.integer()


## ----survey-year-of-birth, eval = TRUE----------------------------------------
## calculate oldest year of birth
survey_year_birth_min <-
    
    survey %>%
    slice(which.max(age)) %>%
    mutate(year_birth = study_year - age) %>%
    pull(year_birth)

## calculate youngest year of birth
survey_year_birth_max <-

    survey %>%
    slice(which.min(age)) %>%
    mutate(year_birth = study_year - age) %>%
    pull(year_birth)


## ----survey-gender, eval = TRUE-----------------------------------------------
## freq table
survey %>%
    freq_table(gender)

## NA

## count
survey_n_gender_NA <- 
    
    survey %>%
    filter(is.na(gender)) %>%
    pull_n()

## perc
survey_p_gender_NA <- 
    
    (survey_n_gender_NA / survey_n_analysed) * 100

## n without NAs
survey_n_gender_analysed <- 

    survey_n_analysed - survey_n_gender_NA

## female

## count
survey_n_female <- 
    
    survey %>%
    filter(gender == "female") %>%
    pull_n()

## perc
survey_p_female <- 
    
    (survey_n_female / survey_n_gender_analysed) * 100

## male

## count
survey_n_male <- 

    survey %>%
    filter(gender == "male") %>%
    pull_n()

## perc
survey_p_male <- 
    
    (survey_n_male / survey_n_gender_analysed) * 100


## ----survey-employment, eval = TRUE-------------------------------------------
## count
survey %>%
    freq_table(employment)

## NB we removed specific types of employment during data redaction
## because it makes individuals more identifiable

## NA

## count
survey_n_employment_NA <- 
    
    survey %>%
    filter(is.na(employment)) %>%
    pull_n()

## perc
survey_p_employment_NA <- 
    
    (survey_n_employment_NA / survey_n_analysed) * 100

## n without NAs
survey_n_employment_analysed <- 

    survey_n_analysed - survey_n_employment_NA

## employed

## count
survey_n_employed <- 
    
    survey %>%
    filter(employment == "employed") %>%
    pull_n()

## perc
survey_p_employed <- 
    
    (survey_n_employed / survey_n_employment_analysed) * 100

## unemployed

## count
survey_n_unemployed <- 
    
    survey %>%
    filter(employment == "unemployed") %>%
    pull_n()

## perc
survey_p_unemployed <- 
    
    (survey_n_unemployed / survey_n_employment_analysed) * 100

## pensioned

## count
survey_n_pensioned <- 
    
    survey %>%
    filter(employment == "pensioned") %>%
    count() %>%
    pull()

## perc
survey_p_pensioned <- 
    
    (survey_n_pensioned / survey_n_employment_analysed) * 100

## student

## count
survey_n_student <- 
    
    survey %>%
    filter(employment == "student") %>%
    pull_n()

## perc
survey_p_student <- 
    
    (survey_n_student / survey_n_employment_analysed) * 100

## other

## count
survey_n_employment_other <- 
    
    survey %>%
    filter(employment == "other") %>% 
    pull_n()

## perc
survey_p_employment_other <- 
    
    (survey_n_employment_other / survey_n_employment_analysed) * 100


## ----survey-income, eval = TRUE-----------------------------------------------
## count
survey %>%
    freq_table(personal_income)

## NA
    
## count
survey_n_income_NA <- 
    
    survey %>%
    filter(is.na(personal_income)) %>%
    pull_n()

## perc
survey_p_income_NA <- 
    
    (survey_n_income_NA / survey_n_analysed) * 100

## n without NAs
survey_n_income_analysed <- 

    survey_n_analysed - survey_n_income_NA

## more than the average

## count
survey_n_income_more <- 
    
    survey %>%
    filter(personal_income == "more") %>%
    pull_n()

## perc
survey_p_income_more <- 
    
    (survey_n_income_more / survey_n_analysed) * 100

## similar to the average

## count
survey_n_income_similar <- 
    
    survey %>%
    filter(personal_income == "similar") %>%
    pull_n()

## perc
survey_p_income_similar <- 
    
    (survey_n_income_similar / survey_n_analysed) * 100

## less thant the average

## count
survey_n_income_less <- 
    
    survey %>%
    filter(personal_income == "less") %>%
    pull_n()

## perc
survey_p_income_less <- 
    
    (survey_n_income_less / survey_n_analysed) * 100


## ----survey-education, eval = TRUE--------------------------------------------
## count
survey %>%
    freq_table(education)

## NA

## count
survey_n_education_NA <- 
    
    survey %>%
    filter(is.na(education)) %>%
    count() %>%
    pull()

## perc
survey_p_education_NA <- 
    
    (survey_n_education_NA / survey_n_analysed) * 100

## n without NAs
survey_n_education_analysed <- 

    survey_n_analysed - survey_n_education_NA

## university (post-laurea ~= PhD)

## count
survey_n_uni_postgrad <- 
    
    survey %>%
    filter(education == "post-laurea") %>%
    pull_n()

## perc
survey_p_uni_postgrad <- 
    
    (survey_n_uni_postgrad / survey_n_education_analysed) * 100

## university (laurea ~= undergraduate and masters)

## count
survey_n_uni_undergrad <- 
    
    survey %>%
    filter(education == "laurea" | 
           education == "laurea-triennale" | 
           education == "laurea-specialistica" | # specialistica and magistrale could be post-grad 
           education == "laurea-magistrale" ) %>%
    pull_n()

## perc
survey_p_uni_undergrad <- 
    
    (survey_n_uni_undergrad / survey_n_education_analysed) * 100

## qualification (qualifica ~= apprenticeship)

## count
survey_n_qualification <- 
    
    survey %>%
    filter(education == "qualifica") %>%
    pull_n()

## perc
survey_p_qualification <- 
    
    (survey_n_qualification / survey_n_education_analysed) * 100

## school (diploma ~= high school)

## count
survey_n_school_high <- 
    
    survey %>%
    filter(education == "diploma") %>%
    pull_n()

## perc
survey_p_school_high <- 
    
    (survey_n_school_high / survey_n_education_analysed) * 100

## school (licenzia-media ~= middle school)

## count
survey_n_school_mid <- 
    
    survey %>%
    filter(education == "licenzia-media") %>%
    pull_n()

## perc
survey_p_school_mid <- 
    
    (survey_n_school_mid / survey_n_education_analysed) * 100

## school (licenzia-elementare ~= primary school)

## count
survey_n_school_primary <- 
    
    survey %>%
    filter(education == "licenzia-elementare") %>%
    pull_n()

## perc
survey_p_school_primary <- 
    
    (survey_n_school_primary / survey_n_education_analysed) * 100

## none

## count
survey_n_education_none <- 
    
    survey %>%
    filter(education == "none") %>%
    pull_n()

## perc
survey_p_education_none <- 
    
    (survey_n_education_none / survey_n_education_analysed) * 100

## other

## count
survey_n_education_other <- 
    
    survey %>%
    filter(education == "other") %>%
    pull_n()

## perc
survey_p_education_other <- 
    
    (survey_n_education_other / survey_n_education_analysed) * 100

## ----survey-birth, eval = TRUE------------------------------------------------
## summary table
survey %>%
    freq_table(location_birth)

## NA

## count
survey_n_birth_NA <- 
    
    survey %>%
    filter(is.na(location_birth)) %>%
    pull_n()

## perc
survey_p_birth_NA <- 
    
    (survey_n_birth_NA / survey_n_analysed) * 100

## n without NAs
survey_n_birth_analysed <- 

    survey_n_analysed - survey_n_birth_NA

## sulle lastre

## count
survey_n_birth_lastre <- 
    
    survey %>%
    filter(location_birth == "sulle-lastre") %>%
    pull_n()

## perc
survey_p_birth_lastre <- 
    
    (survey_n_birth_lastre / survey_n_birth_analysed) * 100

## municipality of Siena

## count
survey_n_birth_municipality <- 

    survey %>%
    filter(location_birth == "municipality-siena") %>%
    pull_n()

## perc
survey_p_birth_municipality <- 
    
    (survey_n_birth_municipality / survey_n_birth_analysed) * 100

## outside of Siena

## count
survey_n_birth_outside <- 

    survey %>%
    filter(location_birth == "outside-siena") %>%
    pull_n()

## perc
survey_p_birth_outside <- 
    
    (survey_n_birth_outside / survey_n_birth_analysed) * 100


## ----survey-birth-bin, eval = TRUE--------------------------------------------
## manipulation
## create index variable with two levels
survey <- 

    survey %>%
    mutate(location_birth_bin = case_when(location_birth == "sulle-lastre" ~ "in Siena",
                                          location_birth == "municipality-siena" ~ "in Siena",
                                          location_birth == "outside-siena" ~ "not in Siena"
                                          ))

## summary table
survey %>%
    freq_table(location_birth_bin)


## ----survey-residence, eval = TRUE--------------------------------------------
## summary table
survey %>%
    freq_table(residence)

## NA

## count
survey_n_residence_NA <- 
    
    survey %>%
    filter(is.na(residence)) %>%
    pull_n()

## perc
survey_p_residence_NA <- 
    
    (survey_n_residence_NA / survey_n_analysed) * 100

## n excluding NAs
survey_n_residence_analysed <- 
    
    survey_n_analysed - survey_n_residence_NA


## ----survey-residence-within-vs-outside-walls, eval = TRUE--------------------
## responses in Siena
list_within_walls <- list("same-contrada",
                          "different-contrada",
                          "within-walls"
                          )
## responses outside Siena
list_outside_walls <- list("outside-walls",
                           "municipality-siena",
                           "province-siena",
                           "region-tuscany", 
                           "italy", 
                           "abroad"
                           )

## manipulation
survey <- 

    survey %>%
    mutate(residence_walls_bin = case_when(residence %in% list_within_walls ~ "within walls",
                                           residence %in% list_outside_walls ~ "outside walls"
                                           ))

## summary table
survey %>%
    freq_table(residence_walls_bin)

## in Siena

## count
survey_n_residence_within_walls <- 
    
    survey %>%
    filter(residence_walls_bin == "within walls") %>%
    pull_n()

## perc
survey_p_residence_within_walls <- 
    
    (survey_n_residence_within_walls / survey_n_residence_analysed) * 100

## outside Siena

## count
survey_n_residence_outside_walls <- 

    survey %>%
    filter(residence_walls_bin == "outside walls") %>%
    pull_n()

## perc
survey_p_residence_outside_walls <- 
    
    (survey_n_residence_outside_walls / survey_n_residence_analysed) * 100


## ----residence-within-walls, eval = TRUE--------------------------------------
## summary table
survey %>%
    freq_table(residence)

## in city centre

## count
survey_n_residence_centre <- 
    
    survey %>%
    filter(residence == "same-contrada" | 
           residence == "different-contrada" | 
           residence == "within-walls") %>%
    pull_n()

## perc
survey_p_residence_centre <- 
    
    (survey_n_residence_centre / survey_n_residence_analysed) * 100

## in same contrada

## count
survey_n_residence_own_contrada <- 
    
    survey %>%
    filter(residence == "same-contrada") %>%
    pull_n()

## perc
survey_p_residence_own_contrada <- 
    
    (survey_n_residence_own_contrada / survey_n_residence_analysed) * 100

## in different contrada

## count
survey_n_residence_other_contrada <- 
    
    survey %>%
    filter(residence == "different-contrada") %>%
    pull_n()

## perc
survey_p_residence_other_contrada <- 
    
    (survey_n_residence_other_contrada / survey_n_residence_analysed) * 100

## not specified contrada

## count
survey_n_residence_within_walls_not_specified <- 
    
    survey %>%
    filter(residence == "within-walls") %>%
    pull_n()

## perc
survey_p_residence_within_walls_not_specified <- 
    
    (survey_n_residence_within_walls_not_specified / survey_n_residence_analysed) * 100

## maximum same contrada

## count
survey_n_residence_own_contrada_max <- 

    survey_n_residence_own_contrada + survey_n_residence_within_walls_not_specified

## perc
survey_p_residence_own_contrada_max <- 

    survey_p_residence_own_contrada + survey_p_residence_within_walls_not_specified

## maximum different contrada

## count
survey_n_residence_other_contrada_max <- 

    survey_n_residence_other_contrada + survey_n_residence_within_walls_not_specified

## perc
survey_p_residence_other_contrada_max <- 

    survey_p_residence_other_contrada + survey_p_residence_within_walls_not_specified


## ----residence-outside-walls, eval = TRUE-------------------------------------
## summary table
survey %>%
    freq_table(residence)

## in city centre

## count
survey_n_residence_siena <- 
    
    survey %>%
    filter(residence == "outside-walls" |
           residence == "municipality-siena" |
           residence == "province-siena") %>%
    pull_n()

## perc
survey_p_residence_siena <- 
    
    (survey_n_residence_siena / survey_n_residence_analysed) * 100

## in municipality

## count
survey_n_residence_siena_municipality <- 
    
    survey %>%
    filter(residence == "municipality-siena") %>%
    pull_n()

## perc
survey_p_residence_siena_municipality <- 
    
    (survey_n_residence_siena_municipality / survey_n_residence_analysed) * 100

## in province

## count
survey_n_residence_siena_province <- 
    
    survey %>%
    filter(residence == "province-siena") %>%
    pull_n()

## perc
survey_p_residence_siena_province <- 
    
    (survey_n_residence_siena_province / survey_n_residence_analysed) * 100

## region of Tuscany

## count
survey_n_residence_tuscany <- 
    
    survey %>%
    filter(residence == "region-tuscany") %>%
    pull_n()

## perc
survey_p_residence_tuscany <- 
    
    (survey_n_residence_tuscany / survey_n_residence_analysed) * 100

## country of Italy

## count
survey_n_residence_italy <- 
    
    survey %>%
    filter(residence == "italy") %>%
    pull_n()

## perc
survey_p_residence_italy <- 
    
    (survey_n_residence_italy / survey_n_residence_analysed) * 100

## abroad

## count
survey_n_residence_abroad <- 
    
    survey %>%
    filter(residence == "abroad") %>%
    pull_n()

## perc
survey_p_residence_abroad <- 
    
    (survey_n_residence_abroad / survey_n_residence_analysed) * 100

## not specified (likely municipality or province)

## count
survey_n_residence_outside_walls_not_specified <- 
    
    survey %>%
    filter(residence == "outside-walls") %>%
    pull_n()

## perc
survey_p_residence_outside_walls_not_specified <- 
    
    (survey_n_residence_outside_walls_not_specified / survey_n_residence_analysed) * 100

## min locally, i.e. in province, including municipality

## count
survey_n_residence_province_min <- 
    
    survey_n_residence_siena_municipality + 
    survey_n_residence_siena_province

# perc
survey_p_residence_province_min <- 
    
    survey_p_residence_siena_municipality + 
    survey_p_residence_siena_province

## max locally, i.e. in province, including municipality

## count
survey_n_residence_province_max <- 
    
    survey_n_residence_siena_municipality + 
    survey_n_residence_siena_province +
    survey_n_residence_outside_walls_not_specified

# perc
survey_p_residence_province_max <- 
    
    survey_p_residence_siena_municipality + 
    survey_p_residence_siena_province +
    survey_p_residence_outside_walls_not_specified

## max further afield, i.e. Tuscany, Italy, abroad

## count
survey_n_residence_outside_province_max <- 
    
    survey_n_residence_tuscany +
    survey_n_residence_italy +
    survey_n_residence_abroad +
    survey_n_residence_outside_walls_not_specified

# perc
survey_p_residence_outside_province_max <- 
    
    survey_p_residence_tuscany +
    survey_p_residence_italy +
    survey_p_residence_abroad +
    survey_p_residence_outside_walls_not_specified


## ----survey-create-years-away-------------------------------------------------
## combine years away and months away into years away
survey <-
    
    survey %>%
    mutate(months_away = months_away/12) %>%
    mutate(years_away = years_away + months_away) %>%
    select(-months_away) 


## ----survey-lifetime-missing, eval = TRUE-------------------------------------
## participants from contrada (in first period) that did not have question 
survey_n_lifetime_missing <- 

    survey %>%
    filter(contrada == "C05" & period == "winter") %>%
    pull_n()

## subset without participants from contrada (in first period) that did not have question
survey_lifetime <- 

    survey %>%
    # create variable which has unique factor for contrada, period, and year 
    unite(cpy, c(contrada, period, study_year), remove = FALSE) %>%
    filter(cpy != "C05_winter_2017") %>%
    select(-cpy)

## n analysed
survey_n_lifetime <- 

    survey_lifetime %>%
    pull_n()


## ----survey-years-away, eval = TRUE-------------------------------------------
## NA

## count
survey_n_years_away_NA <- 
    
    survey_lifetime %>%
    filter(is.na(years_away)) %>%
    pull_n()

## perc
survey_p_years_away_NA <- 
    
    (survey_n_years_away_NA / survey_n_lifetime) * 100

## n without NAs
survey_n_years_away_analysed <- 

    survey_n_lifetime - survey_n_years_away_NA

## summarise years_away
survey_years_away_summary <- 
    
    survey %>%
    summary_stats("years_away")

## mean
survey_years_away_mean <- 

    survey_years_away_summary %>%
    pull(mean)

## sd
survey_years_away_sd <- 

    survey_years_away_summary %>%
    pull(sd)

## min
survey_years_away_min <- 

    survey_years_away_summary %>%
    pull(min)

## max
survey_years_away_max <- 

    survey_years_away_summary %>%
    pull(max)


## ----survey-lifetime-siena-manipulation, eval = TRUE--------------------------
## manipulation
## overall
survey <- 

    survey %>%
    mutate(lifetime_siena = (as.numeric(years_away) / as.numeric(age))) %>%
    ## one member indicate 0 years away but no age, but this would mean lifetime = 0 
    mutate(lifetime_siena = if_else(
               years_away == 0 & is.na(lifetime_siena), 0, lifetime_siena))

## subset
survey_lifetime <- 

    survey_lifetime %>%
    mutate(lifetime_siena = (as.numeric(years_away) / as.numeric(age)))  %>%
    ## one member indicate 0 years away but no age, but this would mean lifetime = 0 
    mutate(lifetime_siena = if_else(
               years_away == 0 & is.na(lifetime_siena), 0, lifetime_siena))

## NA

## count
survey_n_lifetime_NA <- 
    
    survey_lifetime %>%
    filter(is.na(lifetime_siena)) %>%
    pull_n()

## perc
survey_p_lifetime_NA <- 
    
    (survey_n_lifetime_NA / survey_n_lifetime) * 100

## n without NAs
survey_n_lifetime_analysed <- 

    survey_n_lifetime - survey_n_lifetime_NA

## summarise age
survey_lifetime_summary <- 
    
    survey_lifetime %>%
    summary_stats("lifetime_siena") %>%
    as_tibble()

## mean
survey_lifetime_mean <- 

    survey_lifetime_summary %>%
    pull(mean)

## sd
survey_lifetime_sd <- 

    survey_lifetime_summary %>%
    pull(sd)

## min
survey_lifetime_min <- 

    survey_lifetime_summary %>%
    pull(min)

## max
survey_lifetime_max <- 

    survey_lifetime_summary %>%
    pull(max)

## count with 0 lifetime siena
survey_n_lifetime_min <- 

    survey_lifetime %>%
    filter(lifetime_siena == 0) %>%
    pull_n()

## perc with 0% lifetime siena
survey_p_lifetime_min <- 

    (survey_n_lifetime_min / survey_n_lifetime_analysed) * 100
    
## count with 1 lifetime siena
survey_n_lifetime_max <- 

    survey_lifetime %>%
    filter(lifetime_siena == 1) %>%
    pull_n()

## perc with 100% lifetime siena
survey_p_lifetime_max <- 

    (survey_n_lifetime_max / survey_n_lifetime_analysed) * 100

## count with <=0.5 lifetime siena
survey_n_lifetime_half <- 

    survey_lifetime %>%
    filter(lifetime_siena <= 0.5) %>%
    pull_n()

## perc with <=0.5 lifetime siena
survey_p_lifetime_half <- 

    (survey_n_lifetime_half / survey_n_lifetime_analysed) * 100

## plot number of bins
## chosen 25 bins, as the relevant section paper below makes a case for not too many bins  
## rules in Box 1: https://onlinelibrary.wiley.com/doi/full/10.1002/pmrj.12145#pmrj12145-sec-0005-title
bin_no <- 

    ##  round(2*(survey_n_lifetime_analysed^(1/3))) # Rice's rule
    ## round(sqrt(survey_n_lifetime_analysed)) # standard
    25

## range of values per bin
bin_range <- 
    
    1.00 / bin_no

## plot histogram with cumulative distribution
plot_lifetime_siena <- 
    
    survey_lifetime %>% 
    filter(!is.na(lifetime_siena)) %>%    
    ggplot(aes(x = lifetime_siena)) +
    geom_histogram(aes(x = lifetime_siena, 
                       y = (..count..)/sum(..count..))
                 , bins = bin_no
                   ) +
    geom_step(direction = "hv"
            , stat = "ecdf"
            , colour = "#d95f02" 
            , size = 1.1
              ) +
    scale_x_continuous("time lived outside Siena province in proportion to age", 
                       labels = scales::number_format(accuracy = 0.01)) + 
    scale_y_continuous("responses (%)", 
                       labels = percent_format(accuracy = 1, suffix = NULL)) +
    theme_classic()


## ----survey-membership-raw, eval = TRUE---------------------------------------
## summary of reasons for membership 
survey %>%
    count(member_kinship, member_birth, member_choice, member_other) %>%
    mutate(sum = sum(n),
           rel_freq = n/sum(n) * 100)

## NA

## count
survey_n_member_NA <- 
    
    survey %>%
    filter(is.na(member_kinship)) %>%
    pull_n()

## perc
survey_p_member_NA <- 

    (survey_n_member_NA / survey_n_analysed) * 100

## n excluding NAs
survey_n_member_analysed <- 
    
    survey_n_analysed - survey_n_member_NA

## survey with just single option
survey_member_reason_single_option <-

    survey %>%
    filter(
        ## single option member_kinship
        str_detect(member_kinship, "yes") & 
        str_detect(member_birth, "no") & 
        str_detect(member_choice, "no") |
        ## single option member_birth
        str_detect(member_kinship, "no") & 
        str_detect(member_birth, "yes") & 
        str_detect(member_choice, "no") |
        ## single option member_choice
        str_detect(member_kinship, "no") & 
        str_detect(member_birth, "no") & 
        str_detect(member_choice, "yes") |
        ## single option member_other
        str_detect(member_other, "yes"))

## n excluding those who selected two options
survey_n_member_single_option <- 
    
    survey_member_reason_single_option %>%
    pull_n()

## perc excluding those who selected two options
survey_p_member_single_option <- 
    
    (survey_n_member_single_option / survey_n_analysed) * 100

## n of those who selected two options
survey_n_member_two_option <- 
    
    survey_n_member_analysed - survey_n_member_single_option

## perc of those who selected two options
survey_p_member_two_option <- 
    
    (survey_n_member_two_option / survey_n_analysed) * 100

## both parents

## count
survey_n_member_both_parents <- 
    
    survey_member_reason_single_option %>%
    filter(member_kinship  == "yes-both-parents") %>%
    pull_n()

## perc of analysed with single option
survey_p_member_both_parents <- 

    (survey_n_member_both_parents / survey_n_member_single_option) * 100

## one parent (not specified)

## count
survey_n_member_one_parent <- 
    
    survey_member_reason_single_option %>%
    filter(member_kinship == "yes-one-parent") %>%
    pull_n()

## perc
survey_p_member_one_parent <- 

    (survey_n_member_one_parent / survey_n_member_single_option) * 100

## one parent (father)

## count
survey_n_member_father <- 
    
    survey_member_reason_single_option %>%
    filter(member_kinship == "yes-father") %>%
    pull_n()

## perc
survey_p_member_father <- 

    (survey_n_member_father / survey_n_member_single_option) * 100

## one parent (mother)

## count
survey_n_member_mother <- 
    
    survey_member_reason_single_option %>%
    filter(member_kinship == "yes-mother") %>%
    pull_n()

## perc
survey_p_member_mother <- 

    (survey_n_member_mother / survey_n_member_single_option) * 100

## birth

## count
survey_n_member_birth <- 
    
    survey_member_reason_single_option %>%
    filter(member_birth == "yes") %>%
    pull_n()

## perc
survey_p_member_birth <- 

    (survey_n_member_birth / survey_n_member_single_option) * 100

## choice

## count
survey_n_member_choice <- 
    
    survey_member_reason_single_option %>%
    filter(member_choice == "yes") %>%
    pull_n()

## perc
survey_p_member_choice <- 

    (survey_n_member_choice / survey_n_member_single_option) * 100

## other only

## count
survey_n_member_other <- 
    
    survey_member_reason_single_option %>%
    filter(member_other == "yes") %>%
    pull_n()

## perc
survey_p_member_other <- 

    (survey_n_member_other / survey_n_member_single_option) * 100

## both parents and birth

## count
survey_n_member_both_parents_birth <- 
    
    survey %>%
    filter(member_kinship == "yes-both-parents" & member_birth == "yes") %>%
    pull_n()

## perc
survey_p_member_both_parents_birth <- 

    (survey_n_member_both_parents_birth / survey_n_member_analysed) * 100

## one parent (father) and birth

## count
survey_n_member_father_birth <- 
    
    survey %>%
    filter(member_kinship == "yes-father" & member_birth == "yes") %>%
    pull_n()

## perc
survey_p_member_father_birth <- 

    (survey_n_member_father_birth / survey_n_member_analysed) * 100

## one parent (mother) and birth

## count
survey_n_member_mother_birth <- 
    
    survey %>%
    filter(member_kinship == "yes-mother" & member_birth == "yes") %>%
    pull_n()

## perc
survey_p_member_mother_birth <- 

    (survey_n_member_mother_birth / survey_n_member_analysed) * 100

## one parent (father) and choice

## count
survey_n_member_father_choice <- 
    
    survey %>%
    filter(member_kinship == "yes-father" & member_choice == "yes") %>%
    pull_n()

## perc
survey_p_member_father_choice <- 

    (survey_n_member_father_choice / survey_n_member_analysed) * 100

## summary of reason
survey <- 

    survey %>%
    ## count(member_kinship, member_birth, member_choice, member_other) %>%
    mutate(member_reason = case_when(str_detect(member_kinship, "yes") & 
                                     member_birth == "yes" ~ "birth-plus-kinship",
                                     str_detect(member_kinship, "yes") & 
                                     member_birth == "no" ~ "kinship",
                                     member_birth == "yes" ~ "birth",
                                     member_choice == "yes" ~ "choice",
                                     member_other == "yes" ~ "other",
                                     TRUE ~ NA_character_
                                     ))


## ----survey-membership-other-categorisation, eval = TRUE----------------------

## classify member other
survey_member_other <- 
    
    survey %>%
    arrange(study, period, contrada) %>% # sort so tibble prints in a consistent manner
    filter(member_other == "yes") %>%
    mutate(member_other_text = str_to_sentence(member_other_text)) %>% # print as sentence
    select(id, member_other, member_other_text)

## view all entries in order to classify them (alternatively use View()
survey_member_other %>%
    print(n = Inf, 
          ## quote = FALSE, # toggle TRUE/FALSE if na.print not working
          na.print = NULL)

## assign membership reason based on text
survey <- 

    survey %>%
    mutate(member_other_assigned = case_when(
               id == "309C16" ~ "choice",
               id == "324C16" ~ "choice",
               id == "323C16" ~ "choice",
               id == "37C05" ~ "choice",
               id == "145C11" ~ "choice",
               id == "140C11" ~ "choice",
               id == "138C11" ~ "choice",
               id == "118C11" ~ "kinship",
               id == "440C06" ~ "kinship",
               id == "6C06" ~ "choice",
               id == "39C06" ~ "choice",
               id == "450C06" ~ "choice", # could be kinship, mentions dad
               id == "462C06" ~ "choice",
               id == "161C16" ~ "choice",
               id == "171C16" ~ "uninformative",
               id == "209C16" ~ "choice",
               id == "230C16" ~ "choice",
               id == "380C05" ~ "blank",
               id == "386C05" ~ "choice",
               id == "392C05" ~ "choice",
               id == "393C05" ~ "choice",
               id == "418C08" ~ "choice",
               id == "436C08" ~ "kinship",
               id == "307C11" ~ "kinship",
               id == "332C11" ~ "choice",
               id == "337C11" ~ "choice",
               id == "345C11" ~ "choice",
               id == "346C11" ~ "choice",
               id == "361C11" ~ "choice",
               id == "405C06" ~ "choice",
               id == "409C06" ~ "choice",
               id == "411C06" ~ "choice",
               id == "431C06" ~ "kinship",
               id == "469C06" ~ "choice", # this is A.R.K. note
               id == "MA311" ~ "kinship",
               id == "LU209" ~ "choice", # could be kinship, mentions aunt
               id == "LU007" ~ "kinship",
               id == "CI707" ~ "choice",
               id == "FE101" ~ "kinship",
               id == "EA010" ~ "choice"
           ))

## view assigment next to text
survey %>%
    filter(!is.na(member_other_assigned)) %>%
    mutate(member_other_text = str_to_sentence(member_other_text)) %>% # print as sentence
    select(id, member_other_assigned, member_other_text)

## summary
survey %>%
    freq_table(member_other_assigned)

## other who are kinship

## count
survey_n_member_other_kinship <- 
    
    survey %>%
    filter(member_reason == "other" & member_other_assigned == "kinship") %>%
    pull_n()

## perc
survey_p_member_other_kinship <- 

    (survey_n_member_other_kinship / survey_n_member_other) * 100

## other who are choice

## count
survey_n_member_other_choice <- 
    
    survey %>%
    filter(member_reason == "other" & member_other_assigned == "choice") %>%
    pull_n()

## perc
survey_p_member_other_choice <- 

    (survey_n_member_other_choice / survey_n_member_other) * 100

## other who are unclear

## count
survey_n_member_other_unclear <- 
    
    survey %>%
    filter(member_reason == "other" & member_other_assigned %in% c("uninformative", "blank")) %>%
    pull_n()

## perc
survey_p_member_other_unclear <- 

    (survey_n_member_other_unclear / survey_n_member_other) * 100


## ----survey-membership-all-categorisation, eval = TRUE------------------------
# manipulation
survey <- 
    
    survey %>%
    mutate(member_reason =
               case_when(member_kinship = str_detect(member_kinship, "y[a-z]") & 
                             member_birth == "yes" ~ "birth-plus-kinship",
                         member_kinship = str_detect(member_kinship, "y[a-z]") & 
                             member_choice == "yes" ~ "kinship-plus-choice",
                         member_kinship = str_detect(member_kinship, "y[a-z]") ~ "kinship",
                         member_birth == "yes" ~ "birth",
                         member_choice == "yes" ~ "choice",
                         member_other_assigned == "kinship" ~ "kinship",
                         member_other_assigned == "choice" ~ "choice",
                         member_other_assigned == "blank" ~ "not-classifiable",
                         member_other_assigned == "uninformative" ~ "not-classifiable",
                         is.na(member_kinship) & is.na(member_birth) & 
                         is.na(member_choice)  ~ NA_character_,
                         TRUE ~ NA_character_))

## summary table
survey %>%
    select(member_reason) %>%
    freq_table(member_reason)

## membership

## NA

## count
survey_n_member_criteria_NA <- 
    
    survey %>%
    filter(member_reason == "NA" |
           member_other_assigned %in% c("blank", "uninformative")) %>%
    pull_n()

## perc
survey_p_member_criteria_NA <- 

    (survey_n_member_criteria_NA / survey_n_analysed) * 100

## tibble with those only single option
survey_member_criteria <- 

    survey %>%
    filter(member_reason %in% c("birth", "kinship", "choice"))

## n excluding NAs
survey_n_member_criteria_analysed <- 
    
    survey_member_criteria %>%
    pull_n()

## birth

## count
survey_n_member_criteria_birth <- 
    
    survey_member_criteria %>%
    filter(member_reason == "birth") %>%
    pull_n()

## perc
survey_p_member_criteria_birth <- 

    (survey_n_member_criteria_birth / survey_n_member_criteria_analysed) * 100

## kinship

## count
survey_n_member_criteria_kinship <- 
    
    survey_member_criteria %>%
    filter(member_reason == "kinship") %>%
    pull_n()

## perc
survey_p_member_criteria_kinship <- 

    (survey_n_member_criteria_kinship / survey_n_member_criteria_analysed) * 100

## choice

## count
survey_n_member_criteria_choice <- 
    
    survey_member_criteria %>%
    filter(member_reason == "choice") %>%
    pull_n()

## perc
survey_p_member_criteria_choice <- 

    (survey_n_member_criteria_choice / survey_n_member_criteria_analysed) * 100


## ----membership-by-age, eval = TRUE-------------------------------------------

## summarise means of ages
survey_member_age_summary <- 

    survey_member_criteria %>%
    group_by(member_reason) %>% 
    summary_stats("age")

## birth

## mean
survey_member_birth_age_mean <- 

    survey_member_age_summary %>%
    filter(member_reason == "birth") %>%
    pull(mean)

## sd
survey_member_birth_age_sd <- 

    survey_member_age_summary %>%
    filter(member_reason == "birth") %>%
    pull(sd)

## kinship

## mean
survey_member_kinship_age_mean <- 

    survey_member_age_summary %>%
    filter(member_reason == "kinship") %>%
    pull(mean)

## sd
survey_member_kinship_age_sd <- 

    survey_member_age_summary %>%
    filter(member_reason == "kinship") %>%
    pull(sd)

## choice

## mean
survey_member_choice_age_mean <- 

    survey_member_age_summary %>%
    filter(member_reason == "choice") %>%
    pull(mean)

## sd
survey_member_choice_age_sd <- 

    survey_member_age_summary %>%
    filter(member_reason == "choice") %>%
    pull(sd)


## ----membership-by-birth, eval = TRUE-----------------------------------------
## count number of membership by choice
summary_member_choice <- 
    
    survey_member_criteria %>%
    filter(member_reason == "choice") %>%
    count(location_birth_bin)

## total of membership by choice in Siena
survey_n_member_choice_siena <- 
    
    summary_member_choice %>%
    filter(location_birth_bin == "in Siena") %>%
    pull(n) 

## percentage of membership by choice in Siena
survey_p_member_choice_siena <- 

    survey_n_member_choice_siena / survey_n_member_criteria_choice * 100

## total of membership by choice not in Siena
survey_n_member_choice_not_siena <- 
    
    summary_member_choice %>%
    filter(location_birth_bin == "not in Siena") %>%
    pull(n) 

## percentage of membership by choice not in Siena
survey_p_member_choice_not_siena <- 

    survey_n_member_choice_not_siena / survey_n_member_criteria_choice * 100

#####

## count number of membership by kinship
summary_member_kinship <- 
    
    survey_member_criteria %>% 
    filter(member_reason == "kinship") %>%
    count(location_birth_bin)

## total of membership by birth and/or kinship in Siena
survey_n_member_kinship_siena <- 
    
    summary_member_kinship %>%
    filter(location_birth_bin == "in Siena") %>%
    pull(n) 

## percentage of membership by birth and/or kinship in Siena
survey_p_member_kinship_siena <- 

    survey_n_member_kinship_siena / survey_n_member_criteria_kinship * 100

## total of membership by birth and/or kinship not in Siena
survey_n_member_kinship_not_siena <- 
    
    summary_member_kinship %>%
    filter(location_birth_bin == "not in Siena") %>%
    pull(n) 

## percentage of membership by birth and/or kinship not in Siena
survey_p_member_kinship_not_siena <- 

    survey_n_member_kinship_not_siena / survey_n_member_criteria_kinship * 100


## ----survey-membership-kinship, eval = TRUE-----------------------------------
## count
survey %>%
    freq_table(member_kinship)
    
##################################################

## number who indicated parents
survey_n_member_parents <- 

    survey %>%
    filter(str_detect(member_kinship, "yes")) %>%
    pull_n()

## one parent (not state which)

## count
survey_n_member_parents_one <- 
    
    survey %>%
    filter(member_kinship == "yes-one-parent") %>%
    pull_n()

## perc
survey_p_member_parents_one <- 

    (survey_n_member_parents_one / survey_n_member_parents) * 100

## number parents analysed
survey_n_member_parents_analysed <- 

    survey %>%
    filter(str_detect(member_kinship, "yes")) %>%
    filter(member_kinship != "yes-one-parent") %>%
    pull_n()

## both parents

## count
survey_n_member_parents_both <- 
    
    survey %>%
    filter(member_kinship == "yes-both-parents") %>%
    pull_n()

## perc
survey_p_member_parents_both <- 

    (survey_n_member_parents_both / survey_n_member_parents_analysed) * 100

## mother

## count
survey_n_member_parents_mother <- 
    
    survey %>%
    filter(member_kinship == "yes-mother") %>%
    pull_n()

## perc
survey_p_member_parents_mother <- 

    (survey_n_member_parents_mother / survey_n_member_parents_analysed) * 100

## father

## count
survey_n_member_parents_father <- 
    
    survey %>%
    filter(member_kinship == "yes-father") %>%
    pull_n()

## perc
survey_p_member_parents_father <- 

    (survey_n_member_parents_father / survey_n_member_parents_analysed) * 100

## out of total

## both parents

## perc
survey_p_member_parents_both_all <- 

    (survey_n_member_parents_both / survey_n_member_analysed) * 100

## mother

## perc
survey_p_member_parents_mother_all <- 

    (survey_n_member_parents_mother / survey_n_member_analysed) * 100

## father

## perc
survey_p_member_parents_father_all <- 

    (survey_n_member_parents_father / survey_n_member_analysed) * 100


## ----survey-contrada-exogamy, eval = TRUE-------------------------------------
## contrada endogamy
## sum all mentions of both parents
## number
survey_n_endogamy <-

    survey_n_member_both_parents

## contrada exogamy
## sum all mentions of one parent
## number
survey_n_exogamy <-

    survey_n_member_one_parent +
    survey_n_member_father +
    survey_n_member_mother

## total both parents and one parent
survey_n_endogamy_exogamy <-

    survey_n_endogamy + survey_n_exogamy

## percentage both parents and one parent
survey_p_endogamy_exogamy <-

    (survey_n_endogamy_exogamy / survey_n_member_single_option) * 100

## contrada endogamy percentage
survey_p_endogamy <-

    (survey_n_endogamy / survey_n_endogamy_exogamy) * 100

## contrada exogamy percentage
survey_p_exogamy <-

    (survey_n_exogamy / survey_n_endogamy_exogamy) * 100


## ----survey-nominated-father-mother, eval = TRUE------------------------------
## total who indicated father or mother (not one parent)
survey_n_member_nominated_father_mother <-

    survey_n_member_father + survey_n_member_mother

## perc nominated father 
survey_p_member_father_nominated <-

    (survey_n_member_father/survey_n_member_nominated_father_mother) * 100

## perc nominated mother 
survey_p_member_mother_nominated <-

    (survey_n_member_mother/survey_n_member_nominated_father_mother) * 100


## ----survey-protettore, eval = TRUE-------------------------------------------
## freq table
survey %>%
    freq_table(protettore)

## protettore

## NA

## count
survey_n_protettore_NA <- 
    
    survey %>%
    filter(is.na(protettore)) %>%
    pull_n()

## perc
survey_p_protettore_NA <- 
    
    (survey_n_protettore_NA / survey_n_analysed) * 100

## n excluding NAs
survey_n_protettore_analysed <- 
    
    survey_n_analysed - survey_n_protettore_NA

## count
survey_n_protettore <- 
    
    survey %>%
    filter(protettore == "yes") %>%
    pull_n()
    
## perc
survey_p_protettore <- 
    
    (survey_n_protettore / survey_n_protettore_analysed) * 100

## not protettore

survey_n_protettore_no <- 
    
    survey %>%
    filter(protettore == "no") %>%
    pull_n()
    
## perc
survey_p_protettore_no <- 
    
    (survey_n_protettore_no / survey_n_protettore_analysed) * 100


## ----survey-role, eval = TRUE-------------------------------------------------
## summary of role current
survey %>%
    count(role_current)

## summary of role current
survey %>%
    count(role_past)


## ----survey-role-current, eval = TRUE-----------------------------------------
## NB we removed specific positions both currently held or held in the past
## during data redaction, a collaborator was concerned that this would make 
## people easily identifiable

## current role

## NA

## count
survey_n_role_current_NA <- 
    
    survey %>%
    filter(is.na(role_current)) %>%
    pull_n()

## perc
survey_p_role_current_NA <- 
    
    (survey_n_role_current_NA / survey_n_analysed) * 100

## n excluding NAs
survey_n_role_current_analysed <- 
    
    survey_n_analysed - survey_n_role_current_NA

## yes

## count
survey_n_role_current_yes <- 
    
    survey %>%
    filter(role_current == "yes") %>%
    pull_n()

## perc
survey_p_role_current_yes <- 
    
    (survey_n_role_current_yes / survey_n_role_current_analysed) * 100

## no

## count
survey_n_role_current_no <- 
    
    survey %>%
    filter(role_current == "no") %>%
    pull_n()

## perc
survey_p_role_current_no <- 
    
    (survey_n_role_current_no / survey_n_role_current_analysed) * 100


## ----survey-role-past, eval = TRUE--------------------------------------------
## past role

## NA

## count
survey_n_role_past_NA <- 
    
    survey %>%
    filter(is.na(role_past)) %>%
    pull_n()

## perc
survey_p_role_past_NA <- 
    
    (survey_n_role_past_NA / survey_n_analysed) * 100

## n excluding NAs
survey_n_role_past_analysed <- 
    
    survey_n_analysed - survey_n_role_past_NA

## yes

## count
survey_n_role_past_yes <- 
    
    survey %>%
    filter(role_past == "yes") %>%
    pull_n()

## perc
survey_p_role_past_yes <- 
    
    (survey_n_role_past_yes / survey_n_role_past_analysed) * 100

## no

## count
survey_n_role_past_no <- 
    
    survey %>%
    filter(role_past == "no") %>%
    pull_n()

## perc
survey_p_role_past_no <- 
    
    (survey_n_role_past_no / survey_n_role_past_analysed) * 100


## ----survey-role-current-past, eval = TRUE------------------------------------
## count number of participants
survey %>%
    count(role_current, role_past)

## create role_current_past
survey <- 
    
    survey %>%
    mutate(role_current_past = 
               case_when(role_current == "no" & role_past == "no" ~ "neither",
                         role_current == "no" & role_past == "yes" ~ "only past",
                         role_current == "yes" & role_past == "no" ~ "only current",
                         role_current == "yes" & role_past == "yes" ~ "both"
                         )) 

## NA

## count
survey_n_role_current_past_NA <- 
    
    survey %>%
    filter(is.na(role_current_past)) %>%
    pull_n()

## perc
survey_p_role_current_past_NA <- 
    
    (survey_n_role_current_past_NA / survey_n_analysed) * 100

## n excluding NAs
survey_n_role_current_past_analysed <- 
    
    survey_n_analysed - survey_n_role_current_past_NA

## yes to both

## count
survey_n_role_current_past_yes <- 
    
    survey %>%
    filter(role_current_past == "both") %>%
    pull_n()

## perc
survey_p_role_current_past_yes <- 
    
    (survey_n_role_current_past_yes / survey_n_role_current_past_analysed) * 100

## only current

## count
survey_n_role_current_only <- 
    
    survey %>%
    filter(role_current_past == "only current") %>%
    pull_n()

## perc
survey_p_role_current_only <- 
    
    (survey_n_role_current_only / survey_n_role_current_past_analysed) * 100

## only past

## count
survey_n_role_past_only <- 
    
    survey %>%
    filter(role_current_past == "only past") %>%
    pull_n()

## perc
survey_p_role_past_only <- 
    
    (survey_n_role_past_only / survey_n_role_current_past_analysed) * 100

## no to both

## count
survey_n_role_current_past_no <- 
    
    survey %>%
    filter(role_current_past == "neither") %>%
    pull_n()

## perc
survey_p_role_current_past_no <- 
    
    (survey_n_role_current_past_no / survey_n_role_current_past_analysed) * 100


## ----survey-friendship-pre-exclusion, eval = TRUE-----------------------------
# manipulation
survey <- 
    
    survey %>%
    mutate(friends_total = 
               friends_same_contrada + friends_different_contrada + friends_no_contrada)
           
# summary table of total number
survey %>%
    freq_table(friends_total)

## summary table of correct total (5) vs incorrect totals
survey %>%
    freq_table(friends_count = 
                   if_else(friends_total == 5, "correct", "incorrect"))

## create tbl with only correct total
survey_friends <- 
    
    survey %>% 
    filter(friends_total == 5)

## n of participants who provided correct total
survey_n_friends_analysed <- 
    
    survey_friends %>%
    pull_n()

## incorrect friends

## count
survey_n_friends_incorrect <- 
    
    survey %>%
    filter(friends_total != 5) %>%
    pull_n()

## perc
survey_p_friends_incorrect <- 
    
    (survey_n_friends_incorrect / survey_n_analysed) * 100

## NA

## count
survey_n_friends_NA <- 
    
    survey %>%
    filter(is.na(friends_total)) %>%
    pull_n()

## perc
survey_p_friends_NA <- 
    
    (survey_n_friends_NA / survey_n_analysed) * 100


## ----survey-friendship-figure, eval = TRUE------------------------------------
## set order of friends (in graph)
friends_order <- c("5", "4", "3", "2", "1", "0") # 5 on top

## create tibble
survey_friends_combinations <- 

    survey_friends %>%
    select(id, friends_same_contrada, friends_different_contrada, friends_no_contrada) %>%
    count(friends_same_contrada, friends_different_contrada, friends_no_contrada) %>%
    mutate(rel_freq = (n / nrow(survey_friends))) %>%
    ## reorder numbers by converting to factors  
    mutate(same = factor(friends_same_contrada,
                         friends_order)) %>%
    mutate(diff = factor(friends_different_contrada,
                         friends_order)) %>%
    mutate(none = factor(friends_no_contrada,
                         friends_order))

## plot labels
x_axis_label <- bquote(paste(italic("contrada"), " affiliation of friend"))
x_axis_label <- bquote(paste("number of friends by ", italic("contrada"), " affiliation"))
same_label <- "(a)<br>same<br>*contrada*"
different_label <- "(b)<br>another<br>*contrada*"
no_contrada_label <- "(c)<br>not a<br>member"

## width of bars
width_bars <- 1/9

## create plot with all strata together
## NB not printed
plot_friendship <- 

    ggplot(as.data.frame(survey_friends_combinations),
           aes(y = rel_freq,
               axis1 = same, 
               axis2 = diff,
               axis3 = none)) +
    geom_alluvium(aes(fill = same), 
                  width = width_bars, 
                  discern = FALSE) +
    geom_stratum(width = width_bars, 
                 fill = "white", 
                 color = "black", 
                 discern = FALSE) +
    geom_text(aes(label = after_stat(stratum)),
              stat = "stratum", 
              discern = FALSE,
              min.y = 0.025) +
    scale_x_discrete(name = x_axis_label,                         
                     labels = c(same_label, different_label, no_contrada_label),
                     limits = c("same contrada",
                                "different contrada", 
                                "no contrada"), 
                     expand = c(.05, .05)) +
    scale_y_continuous(name = "responses (%)",
                       labels = percent_format(accuracy = 1, suffix = NULL)
                       ) +
    scale_fill_brewer(palette = "Dark2") +
    guides(fill = FALSE) +
    theme_classic() +
    theme(axis.text.x = ggtext::element_markdown())

## create plot for each strata
## NB combined in SI-survey

## loop over each combination for same contrada = c(0:5)
for (i in c(0:5)) {
    
    survey_friends_combinations_tmp <- 
        
        survey_friends %>%
        select(id, friends_same_contrada, friends_different_contrada, friends_no_contrada) %>%
        group_by(friends_same_contrada, friends_different_contrada, friends_no_contrada) %>%
        summarise(n = n()) %>%
        mutate(rel_freq = (n / nrow(survey_friends))) %>%
        ## reorder numbers by converting to factors  
        mutate(same = factor(friends_same_contrada,
                             friends_order)) %>%
        mutate(diff = factor(friends_different_contrada,
                             friends_order)) %>%
        mutate(none = factor(friends_no_contrada,
                             friends_order)) %>%
        mutate(same_highlight = case_when(same == i ~ "yes",
                                          TRUE ~ "no"))

    ## width of bars
    width_bars <- 1/9
    
    ## create plot
    friendship_plot_tmp <- 
        
        ggplot(as.data.frame(survey_friends_combinations_tmp),
               aes(y = rel_freq, 
                   axis1 = same, 
                   axis2 = diff,
                   axis3 = none)) +
        geom_alluvium(aes( 
            alpha = same_highlight),
            fill = "grey40",
            width = width_bars, discern = FALSE) +
        geom_stratum(width = width_bars, 
                     fill = "white", 
                     color = "black", 
                     discern = FALSE) +
        geom_text(aes(label = after_stat(stratum)),
                  stat = "stratum", 
                  discern = FALSE,
                  size = 3.5,
                  alpha = 0.9,
                  min.y = 0.05) +
        scale_x_discrete(name = NULL, # x_axis_label, 
                         limits = c("member of\nthe same", "member of\nanother", "not a\nmember"), 
                         labels = c(same_label, different_label, no_contrada_label),
                         expand = c(.05, .05)
                         ) +
        scale_alpha_discrete(range = c(0, 1)) +
        scale_y_continuous(name = NULL,
                           labels = percent_format(accuracy = 1, suffix = NULL)
                           ) +        
        labs(subtitle = case_when(i == 5 ~ paste(i, "friends from the same *contrada*"),
                                  i == 4 ~ paste(i, "friends from the same *contrada*"),
                                  i == 3 ~ paste(i, "friends from the same *contrada*"),
                                  i == 2 ~ paste(i, "friends from the same *contrada*"),
                                  i == 1 ~ paste(i, "friend from the same *contrada*"),
                                  i == 0 ~ paste(i, "friends from the same *contrada*"),
                                  TRUE ~ NA_character_
                                  )) +
        guides(fill = FALSE, alpha = FALSE) +
        theme_classic() +
        theme(axis.text.x = ggtext::element_markdown(),
              plot.subtitle = ggtext::element_markdown(hjust = 0.5))
    
    ## create object
    assign(paste0("friendship_plot_", i), friendship_plot_tmp)
    
}


## ----survey-friends-summary, eval = TRUE--------------------------------------
## averages for each friend

survey_friends_mean <- 
    
    survey_friends %>%
    select(friends_same_contrada, friends_different_contrada, friends_no_contrada) %>%
    map(mean)

## all five from same contrada
survey_n_friends_five <- 

    survey_friends %>%
    filter(friends_same_contrada == 5) %>%
    pull_n()

## perc
survey_p_friends_five <- 
    
    (survey_n_friends_five / survey_n_friends_analysed) * 100


## ----survey-friendship-totals, eval = TRUE------------------------------------
## total number of friends nominated
total_friends_nominated <- 
    
    survey_n_friends_analysed * 5 %>%
    as.integer()

## of which same contrada
total_friends_same <- 
    
    (survey_friends_mean$friends_same_contrada * survey_n_friends_analysed) %>%
    as.integer()

total_p_friends_same <- 
    
    (total_friends_same / total_friends_nominated) * 100
    
## of which different contrada
total_friends_different <- 
    
    (survey_friends_mean$friends_different_contrada * survey_n_friends_analysed) %>%
    as.integer()

total_p_friends_different <- 
    
    (total_friends_different / total_friends_nominated) * 100

## of which no contrada
total_friends_no <- 
    
    (survey_friends_mean$friends_no_contrada * survey_n_friends_analysed) %>%
    as.integer()

total_p_friends_no <- 
    
    (total_friends_no / total_friends_nominated) * 100


## ----survey-friends-by-contrada, eval = TRUE----------------------------------
## count by contrada and by friends

for (i in c(0:5)) {

    ## same
    survey_n_friends_tmp <-
    
           survey_friends %>%
           filter(friends_same_contrada == i) %>%
           pull_n()
    
    assign(paste0("survey_n_friends_same_", i),    
           survey_n_friends_tmp)

    survey_p_friends_tmp <-
    
           (survey_n_friends_tmp / survey_n_friends_analysed) * 100

    assign(paste0("survey_p_friends_same_", i),    
           survey_p_friends_tmp)

    ## different
    survey_n_friends_tmp <-
    
           survey_friends %>%
           filter(friends_different_contrada == i) %>%
           pull_n()
    
    assign(paste0("survey_n_friends_different_", i),    
           survey_n_friends_tmp)

    survey_p_friends_tmp <-
    
           (survey_n_friends_tmp / survey_n_friends_analysed) * 100

    assign(paste0("survey_p_friends_different_", i),    
           survey_p_friends_tmp)

    ## no
    survey_n_friends_tmp <-
    
           survey_friends %>%
           filter(friends_no_contrada == i) %>%
           pull_n()
    
    assign(paste0("survey_n_friends_no_", i),    
           survey_n_friends_tmp)

    survey_p_friends_tmp <-
    
           (survey_n_friends_tmp / survey_n_friends_analysed) * 100

    assign(paste0("survey_p_friends_no_", i),    
           survey_p_friends_tmp)
    
}

## those with four friends same contrada, and
## one from different contrada
survey_n_friends_same_4_diff_1_no_0 <-
    
    survey_friends %>%
    filter(friends_same_contrada == 4) %>%
    filter(friends_different_contrada == 1) %>%
    pull_n()

survey_p_friends_same_4_diff_1_no_0 <-

    (survey_n_friends_same_4_diff_1_no_0 / survey_n_friends_analysed) * 100

## one not a contrada member
survey_n_friends_same_4_diff_0_no_1 <-
    
    survey_friends %>%
    filter(friends_same_contrada == 4) %>%
    filter(friends_no_contrada == 1) %>%
    pull_n()

survey_p_friends_same_4_diff_0_no_1 <-

    (survey_n_friends_same_4_diff_0_no_1 / survey_n_friends_analysed) * 100


## ----survey-friends-manipulation, eval = TRUE---------------------------------

## we classified those with 3 or more friends who were members of the same
## contrada as having a ``majority of friends from the same contrada''

## those with 2 or fewer friends who were members of the same contrada as 
## having a ``minority of friends from the same contrada''

## manipulation
survey_friends <- 

    survey_friends %>%
    mutate(friends_bin = case_when(friends_same_contrada >= 3 ~ "majority",
                                   friends_same_contrada <= 2 ~ "minority"))

## summary
survey_friends %>%
    count(friends_bin)

## majority

## count
survey_n_friends_majority <- 
    
    survey_friends %>%
    filter(friends_bin == "majority") %>%
    pull_n()

## perc
survey_p_friends_majority <- 
    
    (survey_n_friends_majority / survey_n_friends_analysed) * 100

## minority

## count
survey_n_friends_minority <- 
    
    survey_friends %>%
    filter(friends_bin == "minority") %>%
    pull_n()

## perc
survey_p_friends_minority <- 
    
    (survey_n_friends_minority / survey_n_friends_analysed) * 100

## we classified those with 3 or more friends who were members of a
## contrada as having a ``majority of friends from a contrada''

## manipulation
survey_friends <- 

    survey_friends %>%
    mutate(friends_contrada_bin = 
               case_when(friends_same_contrada + friends_different_contrada >= 3 ~ "majority",
                         friends_same_contrada + friends_different_contrada <= 2 ~ "minority"))

## summary
survey_friends %>%
    count(friends_contrada_bin)

## majority

## count
survey_n_friends_contrada_majority <- 
    
    survey_friends %>%
    filter(friends_contrada_bin == "majority") %>%
    pull_n()

## perc
survey_p_friends_contrada_majority <- 
    
    (survey_n_friends_contrada_majority / survey_n_friends_analysed) * 100

## minority

## count
survey_n_friends_contrada_minority <- 
    
    survey_friends %>%
    filter(friends_contrada_bin == "minority") %>%
    pull_n()

## perc
survey_p_friends_contrada_minority <- 
    
    (survey_n_friends_contrada_minority / survey_n_friends_analysed) * 100



## ----survey-friends-contrada-members, eval = TRUE-----------------------------
## manipulation
survey_friends <- 

    survey_friends %>% 
    mutate(friends_contrada_bin = case_when(friends_no_contrada == 0 ~ "all",
                                            friends_no_contrada > 0 ~ "none"))

## summary
survey_friends %>%
    count(friends_contrada_bin)

## all from contrade

## count
survey_n_friends_all_contrada <- 
    
    survey_friends %>%
    filter(friends_contrada_bin == "all") %>%
    pull_n()

## perc
survey_p_friends_all_contrada <- 
    
    (survey_n_friends_all_contrada / survey_n_friends_analysed) * 100

## none from contrade

## count
survey_n_friends_no_contrada <- 
    
    survey_friends %>%
    filter(friends_contrada_bin == "none") %>%
    pull_n()

## perc
survey_p_friends_no_contrada <- 
    
    (survey_n_friends_no_contrada / survey_n_friends_analysed) * 100


## ----survey-superiority, eval = TRUE------------------------------------------
## summary table
survey %>%
    freq_table(better_statement)

## NA

## count
survey_n_superiority_NA <- 
    
    survey %>%
    filter(is.na(better_statement)) %>%
    pull_n()

## perc
survey_p_superiority_NA <- 
    
    (survey_n_superiority_NA / survey_n_analysed) * 100

## n excluding NAs
survey_n_superiority_analysed <- 

    survey_n_analysed - survey_n_superiority_NA

## strongly agreed

## count
survey_n_superiority_strongly_agree <- 
    
    survey %>%
    filter(better_statement == "strongly-agree") %>%
    pull_n()

## perc
survey_p_superiority_strongly_agree <- 
    
    (survey_n_superiority_strongly_agree / survey_n_superiority_analysed) * 100

## agreed

## count
survey_n_superiority_agree <- 
    
    survey %>%
    filter(better_statement == "agree") %>%
    pull_n()

## perc
survey_p_superiority_agree <- 
    
    (survey_n_superiority_agree / survey_n_superiority_analysed) * 100

## neither agreed nor disagreed

## count
survey_n_superiority_neither <- 
    
    survey %>%
    filter(better_statement == "neither") %>%
    pull_n()

## perc
survey_p_superiority_neither <- 
    
    (survey_n_superiority_neither / survey_n_superiority_analysed) * 100

## disagreed

## count
survey_n_superiority_disagree <- 
    
    survey %>%
    filter(better_statement == "disagree") %>%
    pull_n()

## perc
survey_p_superiority_disagree <- 
    
    (survey_n_superiority_disagree / survey_n_superiority_analysed) * 100

## strongly disagreed

## count
survey_n_superiority_strongly_disagree <- 
    
    survey %>%
    filter(better_statement == "strongly-disagree") %>%
    pull_n()

## perc
survey_p_superiority_strongly_disagree <- 
    
    (survey_n_superiority_strongly_disagree / survey_n_superiority_analysed) * 100


## ----survey-religious-participation-------------------------------------------
## frequency table
survey %>%    
    freq_table(period, participation_religious)

## change level names to remove hyphen
survey <- 
    
    survey %>%
    mutate(participation_religious = str_replace_all(participation_religious, "-", "_"))

## NA

## count
survey_n_participation_religious_NA <- 
    
    survey %>%
    filter(is.na(participation_religious)) %>%
    pull_n()

## perc
survey_p_participation_religious_NA <- 
    
    (survey_n_participation_religious_NA / survey_n_analysed) * 100

## n excluding NAs
survey_n_participation_religious_analysed <- 

    survey_n_analysed - survey_n_participation_religious_NA

## create list
participation_levels <- c("never", "once_a_year", "few_times_a_year",
                          "monthly", "few_times_a_month",
                          "weekly", "few_times_a_week")

## count and perc for each level
for (j in participation_levels) {
    
    ## count
    survey_n_participation_religious_tmp <- 
                                   
        survey %>%
        filter(participation_religious == j) %>%
        pull_n()
    
    ## create object
    assign(paste0("survey_n_participation_religious_", j), 
           survey_n_participation_religious_tmp)
    
    ## perc
    survey_p_participation_religious_tmp <- 
    
        (survey_n_participation_religious_tmp / survey_n_participation_religious_analysed) * 100

        ## create object
    assign(paste0("survey_p_participation_religious_", j), 
           survey_p_participation_religious_tmp)
        
}

## ----survey-social-raw, eval = TRUE-------------------------------------------
## frequency table
survey %>%    
    freq_table(participation_social)

## change level names to remove hyphen
survey <- 
    
    survey %>%
    mutate(participation_social = str_replace_all(participation_social, "-", "_"))

## raw totals

## NA

## count
survey_n_participation_social_NA <- 
    
    survey %>%
    filter(is.na(participation_social)) %>%
    pull_n()

## perc
survey_p_participation_social_NA <- 
    
    (survey_n_participation_social_NA / survey_n_analysed) * 100

## n excluding NAs
survey_n_participation_social_analysed <- 

    survey_n_analysed - survey_n_participation_social_NA

## create list
participation_levels <- c("never", "once_month", "few_times_a_month", 
                          "near_weekly", "weekly", "few_times_a_week", 
                          "near_daily", "daily")

## count and perc for each level
for (j in participation_levels) {
    
    ## count
    survey_n_participation_social_tmp <- 
                                   
        survey %>%
        filter(participation_social == j) %>%
        pull_n()
    
    ## create object
    assign(paste0("survey_n_participation_social_", j), 
           survey_n_participation_social_tmp)
    
    ## perc
    survey_p_participation_social_tmp <- 
    
        (survey_n_participation_social_tmp / survey_n_participation_social_analysed) * 100

        ## create object
    assign(paste0("survey_p_participation_social_", j), 
           survey_p_participation_social_tmp)
        
}



## ----survey-social-manipulation, eval = TRUE----------------------------------
## responses were placed into one of two categories:
## frequent = any response every week or more frequent
## infrequen = any response less frequent than every week

## manipulation
survey <- 

    survey %>%
    mutate(participation_social_freq = case_when(
               participation_social == "daily" ~ "frequent",
               participation_social == "near_daily" ~ "frequent",
               participation_social == "few_times_a_week" ~ "frequent",
               participation_social == "weekly" ~ "frequent",
               participation_social == "near_weekly" ~ "infrequent",
               participation_social == "few_times_a_month" ~ "infrequent",
               participation_social == "once_month" ~ "infrequent",
               participation_social == "never" ~ "infrequent"
               ))

## winter and summer

## NA

## count
survey_n_social_NA <- 
    
    survey %>%
    filter(is.na(participation_social_freq)) %>%
    pull_n()

## perc
survey_p_social_NA <- 
    
    (survey_n_social_NA / survey_n_analysed) * 100

## n excluding NAs
survey_n_social_analysed <- 

    survey_n_analysed - survey_n_social_NA

## frequent

## count
survey_n_social_frequent <- 
    
    survey %>%
    filter(participation_social_freq == "frequent") %>%
    pull_n()

## perc
survey_p_social_frequent <- 
    
    (survey_n_social_frequent / survey_n_social_analysed) * 100

## infrequent

## count
survey_n_social_infrequent <- 
    
    survey %>%
    filter(participation_social_freq == "infrequent") %>%
    pull_n()

## perc
survey_p_social_infrequent <- 
    
    (survey_n_social_infrequent / survey_n_social_analysed) * 100

## winter vs summer

for (j in unique(survey$period)) {
    
    survey_tmp <- 
        
        survey %>%
        filter(period == j)

    ## NA

    ## count
    survey_n_social_tmp_NA <- 
                     
                     survey_tmp %>%
                     filter(is.na(participation_social_freq)) %>%
                     pull_n()

    ## create object
    assign(paste0("survey_n_social_", j, "_NA"), survey_n_social_tmp_NA)
    
    ## perc
    survey_p_social_tmp_NA <- 
                     
                     (survey_n_social_tmp_NA / nrow(survey_tmp)) * 100
    
    ## create object
    assign(paste0("survey_p_social_", j, "_NA"), survey_p_social_tmp_NA)
    
    ## number analysed by period
    survey_n_social_tmp_analysed <- 

    nrow(survey_tmp) - survey_n_social_tmp_NA
    
    ## create object
    assign(paste0("survey_n_social_", j, "_analysed"), survey_n_social_tmp_analysed)
    
    ## frequent
    
    ## count
    survey_n_social_tmp_frequent <- 
                     
                     survey_tmp %>%
                     filter(participation_social_freq == "frequent") %>%
                     pull_n()
    
    ## create object
    assign(paste0("survey_n_social_", j, "_frequent"), survey_n_social_tmp_frequent)
    
    ## perc
    survey_p_social_tmp_frequent <- 
    
        (survey_n_social_tmp_frequent / survey_n_social_tmp_analysed) * 100

    ## create object
    assign(paste0("survey_p_social_", j, "_frequent"), survey_p_social_tmp_frequent)
    
    ## infrequent

    ## count
    survey_n_social_tmp_infrequent <- 
                     
                     survey_tmp %>%
                     filter(participation_social_freq == "infrequent") %>%
                     pull_n()
    
    ## create object
    assign(paste0("survey_n_social_", j, "_infrequent"), survey_n_social_tmp_infrequent)
    
    ## perc
    survey_p_social_tmp_infrequent <- 
    
        (survey_n_social_tmp_infrequent / survey_n_social_tmp_analysed) * 100

    ## create object
    assign(paste0("survey_p_social_", j, "_infrequent"), survey_p_social_tmp_infrequent)
    
}



## ----survey-social, eval = TRUE, include = TRUE, echo = FALSE, message = FALSE, warning = FALSE----

## y-axis label for social
y_axis_social <- 

    "responses (%)"

## y-axis label for organisational
y_axis_organisational <- 

    ""

## create figure
plot_participation_social <-
                       
    survey %>%
    filter(!is.na(participation_social)) %>%
    group_by(period, participation_social) %>%
    summarise(n = n()) %>%
    mutate(rel_freq = (n/sum(n))) %>%
    ungroup() %>%
    mutate(period = factor(period, 
                           levels = c("winter", "summer"))) %>%
    mutate(participation_social = factor(participation_social,
                                        levels = participation_levels)) %>%  
    ggplot() +
    geom_col(aes(x = participation_social,
                 y = rel_freq
                 ),
             position = "dodge") +
    ylim(0, 100) +
    facet_wrap(. ~ period,
               ncol = 1,
               scales = "free_y", 
               labeller = labeller(period = c(
                                       "winter" = 
                                           paste0("winter (n = ", survey_n_social_winter_analysed, ")"),
                                       "summer" = 
                                           paste0("summer (n = ", survey_n_social_summer_analysed, ")")
                                   )),
               shrink = TRUE, 
               drop = TRUE
               ) +
        scale_y_continuous(name = y_axis_social,
                       limits = c(0, 0.25),
                       labels = scales::percent_format(accuracy = 1, suffix = NULL)) +
    scale_x_discrete(name = "frequency of participation",
                     labels = c("never", "once\na month", "2-3 times\na month",
                                "nearly\nevery week", "every\nweek", "2-3 times\na week",
                                "nearly\nevery day", "every\nday", "NA"),
                     guide = guide_axis(n.dodge = 2)
                     ) +
    theme_classic() +
    theme(legend.position = "bottom")
 

## ----survey-organisational-raw, eval = TRUE-----------------------------------
## frequency table
survey %>%    
    freq_table(participation_organisational)

## change level names to remove hyphen
survey <- 
    
    survey %>%
    mutate(participation_organisational = str_replace_all(participation_organisational, "-", "_"))

## raw totals

## NA

## count
survey_n_participation_organisational_NA <- 
    
    survey %>%
    filter(is.na(participation_organisational)) %>%
    pull_n()

## perc
survey_p_participation_organisational_NA <- 
    
    (survey_n_participation_organisational_NA / survey_n_analysed) * 100

## n excluding NAs
survey_n_participation_organisational_analysed <- 

    survey_n_analysed - survey_n_participation_organisational_NA

## create list
participation_levels <- c("never", "once_month", "few_times_a_month", 
                          "near_weekly", "weekly", "few_times_a_week", 
                          "near_daily", "daily")

## count and perc for each level
for (j in participation_levels) {
    
    ## count
    survey_n_participation_organisational_tmp <- 
                                   
        survey %>%
        filter(participation_organisational == j) %>%
        pull_n()
    
    ## create object
    assign(paste0("survey_n_participation_organisational_", j), 
           survey_n_participation_organisational_tmp)
    
    ## perc
    survey_p_participation_organisational_tmp <- 
    
        (survey_n_participation_organisational_tmp / survey_n_participation_organisational_analysed) * 100

        ## create object
    assign(paste0("survey_p_participation_organisational_", j), 
           survey_p_participation_organisational_tmp)
        
}



## ----survey-organisational-manipulation, eval = TRUE--------------------------
## responses were placed into one of two categories:
## frequent = any response every week or more frequent
## infrequen = any response less frequent than every week

## manipulation
survey <- 

    survey %>%
    mutate(participation_organisational_freq = case_when(
               participation_organisational == "daily" ~ "frequent",
               participation_organisational == "near_daily" ~ "frequent",
               participation_organisational == "few_times_a_week" ~ "frequent",
               participation_organisational == "weekly" ~ "frequent",
               participation_organisational == "near_weekly" ~ "infrequent",
               participation_organisational == "few_times_a_month" ~ "infrequent",
               participation_organisational == "once_month" ~ "infrequent",
               participation_organisational == "never" ~ "infrequent"
               ))

## winter and summer

## NA

## count
survey_n_organisational_NA <- 
    
    survey %>%
    filter(is.na(participation_organisational_freq)) %>%
    pull_n()

## perc
survey_p_organisational_NA <- 
    
    (survey_n_organisational_NA / survey_n_analysed) * 100

## n excluding NAs
survey_n_organisational_analysed <- 

    survey_n_analysed - survey_n_organisational_NA

## frequent

## count
survey_n_organisational_frequent <- 
    
    survey %>%
    filter(participation_organisational_freq == "frequent") %>%
    pull_n()

## perc
survey_p_organisational_frequent <- 
    
    (survey_n_organisational_frequent / survey_n_organisational_analysed) * 100

## infrequent

## count
survey_n_organisational_infrequent <- 
    
    survey %>%
    filter(participation_organisational_freq == "infrequent") %>%
    pull_n()

## perc
survey_p_organisational_infrequent <- 
    
    (survey_n_organisational_infrequent / survey_n_organisational_analysed) * 100

## winter vs summer

for (j in unique(survey$period)) {
    
    survey_tmp <- 
        
        survey %>%
        filter(period == j)

    ## NA

    ## count
    survey_n_organisational_tmp_NA <- 
                     
                     survey_tmp %>%
                     filter(is.na(participation_organisational_freq)) %>%
                     pull_n()

    ## create object
    assign(paste0("survey_n_organisational_", j, "_NA"), survey_n_organisational_tmp_NA)
    
    ## perc
    survey_p_organisational_tmp_NA <- 
                     
                     (survey_n_organisational_tmp_NA / nrow(survey_tmp)) * 100
    
    ## create object
    assign(paste0("survey_p_organisational_", j, "_NA"), survey_p_organisational_tmp_NA)
    
    ## number analysed by period
    survey_n_organisational_tmp_analysed <- 

    nrow(survey_tmp) - survey_n_organisational_tmp_NA
    
    ## create object
    assign(paste0("survey_n_organisational_", j, "_analysed"), survey_n_organisational_tmp_analysed)
    
    ## frequent
    
    ## count
    survey_n_organisational_tmp_frequent <- 
                     
                     survey_tmp %>%
                     filter(participation_organisational_freq == "frequent") %>%
                     pull_n()
    
    ## create object
    assign(paste0("survey_n_organisational_", j, "_frequent"), survey_n_organisational_tmp_frequent)
    
    ## perc
    survey_p_organisational_tmp_frequent <- 
    
        (survey_n_organisational_tmp_frequent / survey_n_organisational_tmp_analysed) * 100

    ## create object
    assign(paste0("survey_p_organisational_", j, "_frequent"), survey_p_organisational_tmp_frequent)
    
    ## infrequent

    ## count
    survey_n_organisational_tmp_infrequent <- 
                     
                     survey_tmp %>%
                     filter(participation_organisational_freq == "infrequent") %>%
                     pull_n()
    
    ## create object
    assign(paste0("survey_n_organisational_", j, "_infrequent"), survey_n_organisational_tmp_infrequent)
    
    ## perc
    survey_p_organisational_tmp_infrequent <- 
    
        (survey_n_organisational_tmp_infrequent / survey_n_organisational_tmp_analysed) * 100

    ## create object
    assign(paste0("survey_p_organisational_", j, "_infrequent"), survey_p_organisational_tmp_infrequent)
    
}



## ----survey-organisational, eval = TRUE, include = TRUE, echo = FALSE, message = FALSE, warning = FALSE----

## y-axis label for social
y_axis_social <- 

    "responses (%)"

## y-axis label for organisational
y_axis_organisational <- 

    ""

## create figure
plot_participation_organisational <-
                       
    survey %>%
    filter(!is.na(participation_organisational)) %>%
    group_by(period, participation_organisational) %>%
    summarise(n = n()) %>%
    mutate(rel_freq = (n/sum(n))) %>%
    ungroup() %>%
    mutate(period = factor(period, 
                           levels = c("winter", "summer"))) %>%
    mutate(participation_organisational = factor(participation_organisational,
                                        levels = participation_levels)) %>%  
    ggplot() +
    geom_col(aes(x = participation_organisational,
                 y = rel_freq
                 ),
             position = "dodge") +
    ylim(0, 100) +
    facet_wrap(. ~ period,
               ncol = 1,
               scales = "free_y", 
               labeller = labeller(period = c(
                                       "winter" = 
                                           paste0("winter (n = ", survey_n_organisational_winter_analysed, ")"),
                                       "summer" = 
                                           paste0("summer (n = ", survey_n_organisational_summer_analysed, ")")
                                   )),
               shrink = TRUE, 
               drop = TRUE
               ) +
        scale_y_continuous(name = y_axis_organisational,
                       limits = c(0, 0.25),
                       labels = scales::percent_format(accuracy = 1, suffix = NULL)) +
    scale_x_discrete(name = "frequency of participation",
                     labels = c("never", "once\na month", "2-3 times\na month",
                                "nearly\nevery week", "every\nweek", "2-3 times\na week",
                                "nearly\nevery day", "every\nday", "NA"),
                     guide = guide_axis(n.dodge = 2)
                     ) +
    theme_classic() +
    theme(legend.position = "bottom")
 


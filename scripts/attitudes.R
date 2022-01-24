# SPDX-License-Identifier: GPL-3.0

## ----att-ally-neutral-relationships, eval = TRUE------------------------------
## relationship classification:
## own, ally, rival, neutral
## first create list of contrada names by classification w.r.t. each contrada
## save as object 

## list of analysed contrade
list_att_contrade <-  c("Civetta", "Leocorno", "Torre")

## extract names for each analysed contrada
for (focal_contrada in list_att_contrade) {

    ## by relationship
    ## own
    list_own <-
        
        create_contrada_pairs(contrada_relationships) %>%
        extract_contrade_by_relationship(focal_contrada, "self")
    
    ## ally
    list_ally <-
        
        create_contrada_pairs(contrada_relationships) %>%
        extract_contrade_by_relationship(focal_contrada, "ally")
    
    ## create object
    assign(paste0("list_ally_", str_to_lower(str_sub(focal_contrada, 1, 3))), 
           list_ally)

    number_ally <- 
        
        list_ally %>% length
    
    ## create object
    assign(paste0("number_ally_", str_to_lower(str_sub(focal_contrada, 1, 3))), 
           number_ally)
    
    ## neutral
    list_neutral <-
        
        create_contrada_pairs(contrada_relationships) %>%
        extract_contrade_by_relationship(focal_contrada, "neutral")

    ## create object
    assign(paste0("list_neutral_", str_to_lower(str_sub(focal_contrada, 1, 3))), 
           list_neutral)

    number_neutral <- 
        
        list_neutral %>% length
    
    ## create object
    assign(paste0("number_neutral_", str_to_lower(str_sub(focal_contrada, 1, 3))), 
           number_neutral)
    
    ## rival
    list_rival <- 
        
        create_contrada_pairs(contrada_relationships) %>%
        extract_contrade_by_relationship(focal_contrada, "rival")
    
    ## create printable lists
    
    ## allies
    print_ally <- 
        
        list_ally %>%
        paste0("\\emph{", ., "}", collapse = ", ") %>% 
        print
    
    ## create object
    assign(paste0("print_ally_", str_to_lower(str_sub(focal_contrada, 1, 3))), 
           print_ally)
    
    ## neutral
    print_neutral <- 
                      
        list_neutral %>%
        paste0("\\emph{", ., "}", collapse = ", ") %>% 
        print

    ## create object
    assign(paste0("print_neutral_", str_to_lower(str_sub(focal_contrada, 1, 3))), print_neutral)    
    
}

## pull max and min for ally and neutral

## combine ally
number_ally <- 
    
    tibble(number_ally_civ, number_ally_leo, number_ally_tor) %>% 
    gather()

## max ally
number_ally_max <-
    
    number_ally %>% 
    filter(value == max(value)) %>%
    pull(value)

## min ally
number_ally_min <-
    
    number_ally %>% 
    filter(value == min(value)) %>%
    pull(value)

## combine neutral
number_neutral <- 
    
    tibble(number_neutral_civ, number_neutral_leo, number_neutral_tor) %>% 
    gather()

## max neutral
number_neutral_max <-
    
    number_neutral %>% 
    filter(value == max(value)) %>%
    pull(value)

## min neutral
number_neutral_min <-
    
    number_neutral %>% 
    filter(value == min(value)) %>%
    pull(value)


## ----tor-onda, eval = TRUE----------------------------------------------------
## torre number of neutral relationships minus onda
number_neutral_tor_without_onda <- 
    
    (number_neutral_tor - 1) %>% 
    as.integer()


## ----att-manipulation, eval = TRUE--------------------------------------------
## data exclusion

## total n recruited (pre-exclusion)
att_n_recruited <- 
    
    attitudes %>%
    nrow()

## total n with incomplete entries
att_n_incomplete <- 
    
    attitudes %>%
    filter(complete == "n") %>%
    nrow()

## total n analyed (post-exclusion)
att_n_analysed <- 
    
    attitudes %>% 
    nrow()

## total excluded
att_n_excluded <- 
    
    att_n_recruited - att_n_analysed


## ----att-sample-characteristics, eval = TRUE----------------------------------
## sample characteristics

## number females
att_female_n <- 
    
    attitudes %>% 
    filter(gender == "female") %>% 
    nrow()

## percentage females
att_female_p <- 
    
    (att_female_n / att_n_recruited) * 100

## number males
att_male_n <- 
    
    attitudes %>% 
    filter(gender == "male") %>% 
    nrow()

## percentage males
att_male_p <- 
    
    (att_male_n / att_n_recruited) * 100

## tibble with age summary stats
att_age_stats <- 

    attitudes %>% 
    summary_stats("age")

## mean
att_age_mean <- 

    att_age_stats %>%
    pull(mean)

## sd
att_age_sd <- 

    att_age_stats %>%
    pull(sd)

## min
att_age_min <- 

    att_age_stats %>%
    pull(min) %>%
    as.integer()

## max
att_age_max <- 

    att_age_stats %>%
    pull(max) %>%
    as.integer()


## ----att-n-by-contrada-and-period, eval = TRUE--------------------------------
## count number of participants by contrada
att_n_contrada <- 

    attitudes %>%
    count(contrada) 

## calculate percentage of paritcipants by contrada
att_contrada_stats <- 
    
    att_n_contrada %>%
    mutate(percentage = (n/att_n_analysed) * 100) %>%
    ungroup()

## list of contrada with three letter acronym
list_att_contrade <-  c("civ", "leo", "tor")

## extract number and percentage for each contrada
for (focal_contrada in list_att_contrade) {
    
    ## filter contrada
    att_contrada_stats_tmp <- 
        
        att_contrada_stats %>%
        filter(contrada == focal_contrada)
    
    ## pull n    
    att_contrada_stats_n <- 
        
        att_contrada_stats_tmp %>%
        pull(n)
    
    ## create object
    assign(paste0("att_n_", focal_contrada), 
           att_contrada_stats_n)
    
    ## pull p
    att_contrada_stats_p <- 
        
        att_contrada_stats_tmp %>%
        pull(percentage)
    
    ## create object
    assign(paste0("att_p_", focal_contrada), 
           att_contrada_stats_p)
    
}

## total n analysed from civ and leo
att_n_civ_leo <- 
    
    att_n_civ + att_n_leo


## ----att-analysis-by-relationship-reciprocal, eval = TRUE---------------------
## attitudes by contrada
## depending on whether torre--onda is classified as neutral or rival

## create long dataframe of attitudes
att_long <- 

    attitudes %>%
    select(id, contrada, att_aqu:att_val) %>%
    rename_all(list(~str_replace_all(., "att_", ""))) %>%
    gather(contrada_2, attitude, aqu:val) %>%
    mutate(contrada_1 = str_to_title(contrada),
           contrada_2 = str_to_title(contrada_2))

## create list of relationships
## when tor--onda is classified as: 
## neutral = `standard' label
## rival = `reciprocal' label

## uses custom function create_contrada_pairs

## for standard
list_of_relationships_standard <- 

    contrada_relationships %>%
    create_contrada_pairs(tor_ond_reciprocal = FALSE) %>% ## toggle to TRUE for reciprocal label
    create_contrada_acronym

## combine long dataframe and list of relationships
att_long_standard <- 
    
    merge(att_long, list_of_relationships_standard, 
          by = c("contrada_1", "contrada_2")) %>%
    as_tibble()

## get mean attitudes
att_stats_standard <-     

    att_long_standard %>% 
    group_by(relationship) %>% 
    summary_stats("attitude")

## count of responses by relationship type
## create summary
att_responses_stats <- 

    att_long_standard %>%
    filter(!is.na(attitude)) %>%
    count(relationship)

## list of outgroup relationships
list_outgroups <- c("own", "ally", "neutral", "rival")

## filter by relationship
for (relationship_type in list_outgroups) {

    ## pull n
    att_responses_tmp <- 
    
        att_responses_stats %>%
        filter(relationship == relationship_type) %>%
        pull(n)

    ## create object
    assign(paste0("att_responses_", relationship_type), 
           att_responses_tmp)

}

## all outgs combined
att_responses_outg <- 
    
    att_responses_ally + att_responses_neutral + att_responses_rival

## loop to create object with mean and sd attitudes by relationship type
for (relationship_type in unique(att_stats_standard$relationship)) {

    ## mean
    att_mean_tmp <- 
        
        att_stats_standard %>%
        filter(relationship == relationship_type) %>%
        pull(mean)

    ## create object
    assign(paste0("att_mean_", relationship_type), 
           att_mean_tmp)
    
    ## sd
    att_sd_tmp <- 
            
        att_stats_standard %>%
        filter(relationship == relationship_type) %>%
        pull(sd)

    ## create object
    assign(paste0("att_sd_", relationship_type), 
           att_sd_tmp)

}

## mean and sd attitudes out-groups (ally, neutral, rival) combined

## list of outgroup relationships
list_outgroups <- c("ally", "neutral", "rival")

## label outgroups by relationship
att_long_standard <-     

    att_long_standard %>%
    mutate(group_type = if_else(relationship %in% list_outgroups, "outg", "ing"))

## summary stats
att_standard_group_stats <- 

    att_long_standard %>%
    group_by(group_type) %>% 
    summary_stats("attitude")

## mean
att_mean_outgroups <- 

    att_standard_group_stats %>%
    filter(group_type == "outg") %>%
    pull(mean)

## sd
att_sd_outgroups <- 

    att_standard_group_stats %>%
    filter(group_type == "outg") %>%
    pull(sd)

## conduct wilcoxon test

## create mean attitude towards outg per id
att_ing_outgs <- 

    attitudes %>%
    select(id, contrada, att_aqu:att_val) %>%
    rename_all(list(~str_replace_all(., "att_", ""))) %>%
    mutate(ingroup = case_when(contrada == "tor" ~ tor,
                               contrada == "civ" ~ civ,
                               contrada == "leo" ~ leo)) %>%
    mutate(outgroup = case_when(contrada == "civ" ~ ((rowSums(.[3:20]) - civ) / 16),
                                contrada == "leo" ~ ((rowSums(.[3:20]) - leo) / 16),
                                contrada == "tor" ~ ((rowSums(.[3:20]) - tor) / 16)
                                )) %>%
    select(id, contrada, ingroup, outgroup)

## wilcoxon test
contrast_att_ing_outgs <- 
    
    wilcox.test(att_ing_outgs$ingroup, att_ing_outgs$outgroup, paired = TRUE)

## extract statistic
contrast_ing_outgs_statistic <- 
    
    contrast_att_ing_outgs$statistic %>%
    as.numeric()

## extract p value
contrast_ing_outgs_pvalue <- 

    contrast_att_ing_outgs$p.value %>%
    as.numeric()  


## ----plot-att-relationship, eval = TRUE---------------------------------------
## order and labels of relationship categories
order_relationship_levels <- 

    c("own", "outgroups", "ally", "neutral", "rival")

## y axis
## breaks
breaks_minor <- c(1:7)
## labels
labels_minor <- c("very negative 1", "2", "3", "4", "5", "6", "very positive 7")

## label with arrow
arrow_label <- data.frame(lab = c("very negative", "very positive"),
                          x = c(0, 0), 
                          y = c(1, 7))

## axis labels
contrada_label <- 'italic("contrada")~"status"'
relationship_label <- '"relationship type"'

## summary of data

## load for summarySE function
## which calculates SE and CI 
if(!require(Rmisc)){install.packages("Rmisc")}
library(Rmisc) 

att_summary <- 
    
    bind_rows(mutate(att_long_standard, set = "one"),
          mutate(att_long_standard, set = "two")) %>%
    filter(set == "one" | set == "two" & group_type == "outg") %>%
    mutate(relationship = case_when(set == "two" ~ "outgroups",
                                    TRUE ~ relationship)) %>%
    mutate(set = case_when(relationship %in% c("own", "outgroups") ~ "one",
                           relationship %in% c("ally", "neutral", "rival") ~ "two")) %>%
    mutate(relationship = factor(relationship,
                                 levels = order_relationship_levels)) %>% 
    summarySE(measurevar = "attitude",
              groupvars = c("set", "relationship"),
              na.rm = TRUE) %>%
    mutate(set = recode(set, 
                         "one" = contrada_label, 
                         "two" = relationship_label))        

## detach Rmisc as it clashes with tidyverse
detach("package:Rmisc", unload = TRUE)
detach("package:plyr", unload = TRUE)
detach("package:lattice", unload = TRUE)

## automate relationship labels in same order as order_relationship_levels

## list of outgroup relationships
list_outgroups <- c("own", "outgroups", "ally", "neutral", "rival")

## filter by relationship
for (relationship_type in list_outgroups) {

    ## pull n
    att_n_tmp <- 
    
        att_summary %>%
        filter(relationship == relationship_type) %>%
        pull(N)

    ## create object
    assign(paste0("att_n_", relationship_type), 
           att_n_tmp)

}

## insert object
order_relationship_labels <- 

    c(paste0("own\n(n = ", att_n_own, ")"), 
      paste0("other\n(n = ", att_n_outgroups, ")"), 
      paste0("alliance\n(n = ", att_n_ally, ")"), 
      paste0("neutral relationship\n(n = ", att_n_neutral, ")"), 
      paste0("rivalry\n(n = ",  att_n_rival, ")")
      )

## create plot and save in object
att_plot <- 
    
    bind_rows(mutate(att_long_standard, set = "one"),
              mutate(att_long_standard, set = "two")) %>%
    filter(set == "one" | set == "two" & group_type == "outg") %>%
    mutate(relationship = case_when(set == "two" ~ "outgroups",
                                    TRUE ~ relationship)) %>%
    mutate(set = case_when(relationship %in% c("own", "outgroups") ~ "one",
                           relationship %in% c("ally", "neutral", "rival") ~ "two")) %>%    
    mutate(set = recode(set, 
                        "one" = contrada_label, 
                        "two" = relationship_label)) %>% 
    mutate(relationship = factor(relationship,
                                 levels = order_relationship_levels)) %>% 
    ggplot(aes(x = relationship,
               y = attitude
               )
           ) +
    geom_flat_violin(position = position_nudge(x = .25, 
                                               y = 0),
                     adjust = 2, 
                     trim = FALSE,
                     na.rm = TRUE,
                     colour = "grey40",
                     fill = "grey40"
                     ) +
    geom_point(position = position_jitter(width = .15, height = .15), 
               size = 1, 
               alpha = 0.4,
               na.rm = TRUE
               ) + 
    geom_errorbar(data = att_summary, 
                  aes(ymin = attitude - ci, 
                      ymax = attitude + ci),
                  position = position_nudge(x = 0.25),
                  colour = "black",
                  width = 0.1, 
                  size = 1
                  ) +
    geom_point(data = att_summary,
               position = position_nudge(x = 0.25),
               size = 2.5,
               shape = 23,
               fill = "black",
               colour = "grey90"
               ) +
    facet_grid(. ~ set, 
               scales = "free_x", 
               labeller = label_parsed,
               shrink = TRUE, 
               drop = TRUE
               ) +
    scale_size_continuous(range = c(1, 10)) +
    scale_x_discrete("",
                     breaks = order_relationship_levels, 
                     labels = order_relationship_labels
                     ) + 
    scale_y_continuous("attitude", 
                       breaks = breaks_minor,
                       labels = labels_minor
                       ) +
    guides(fill = FALSE, 
           colour = FALSE
           ) +
    theme_classic() +
    theme(strip.background = element_blank(), 
          strip.placement = "outside")


## ----att-relationship, eval = TRUE, include = TRUE, echo = FALSE, message = FALSE, warning = FALSE, fig.scap = "Distribution of attitudes across \\contrada members", fig.cap = fig_caption, results = "asis", fig.align = "center", fig.keep = "all", fig.show = "asis", eval.after = "fig.cap"----

## text for figure caption
fig_caption <- 
    
    paste("Distribution of attitude across", att_n_analysed, "participants from \\civ, \\leo, and \\tor by \\contrada status and relationship type, on a 7-point scale from very negative to very positive. Shown is the mean and 95\\% confidence intervals. Variation in the number of responses \\emph{n} arises from specific features of the data collection and analysis; see \\cref{SI-sec:att-results} for details. In the plot we add random jitter to the values along both axes for visibility.")

## display plot
att_plot


## ----att-test-by-relationship, eval = TRUE------------------------------------
## compare attitudes

## create mixed-effects model

## reorder factor so that ing is refernce level
att_long_standard <- 
    
    att_long_standard %>%
    mutate(relationship = factor(relationship, 
                                 levels = c("own", "ally", "neutral", "rival"))) 
                                        
## use att_long_reciprocal for analysis with onda classified as rival
## results are qualitatively the same (for both anova and contrasts)
model_full <-

    lmerTest::lmer(attitude ~ relationship  + 
                       (1 | id) + (1 | contrada), 
                   data = att_long_standard)

## show model
summary(model_full)

## run anova
anova_model_full <- anova(model_full, 
                          ddf = "Kenward-Roger")
anova_model_full

## post-hoc comparisons
model_full_ref_grid <- emmeans::ref_grid(model_full)
## compare relationship type
model_full_contrasts <- emmeans::emmeans(model_full_ref_grid, 
                                         pairwise ~ relationship, 
                                         adj = "tukey")
model_full_contrasts$contrasts

## extract ANOVA results

## turn model object into tibble
anova_model_full_tidy <- 

    anova_model_full %>% 
    tidy()

## stats

## ndf
anova_model_ndf <- 

    anova_model_full_tidy %>%
    pull(NumDF)

## ddf
anova_model_ddf <- 

    anova_model_full_tidy %>%
    pull(DenDF)

## p value
anova_model_statistic <- 

    anova_model_full_tidy %>%
    pull(statistic)

## p value
anova_model_pvalue <- 

    anova_model_full_tidy %>%
    pull(p.value)



## ----att-contrasts, eval = TRUE-----------------------------------------------
## make table of pair-wise comparisons

## header
table_col_names <- c("contrast", 
                     "$\\Delta$\\textsuperscript{2}", 
                     "SE",
                     "df", 
                     "t", 
                     "\\pval",
                     "sig.\\textsuperscript{3}"
                     )

## caption
table_caption <- 
    "\\label{SI-tab:att-contrasts}Difference in attitude based on \\contrada classification\\textsuperscript{1}"

table_caption_short <- 
    "Difference in attitude based on \\contrada classification"

## general footnote
footnote_table <-
    "Assessed via \\\\pairwise comparisons with a Tukey correction."

footnote_estimate <- 
    "Mean difference between \\\\contrada classification in contrast."

footnote_signif <-
    "Significance: * \\\\siglevel{.05}, ** \\\\siglevel{.01}, *** \\\\siglevel{.001}"
    
## table
table_att_posthoc <- 
    
    model_full_contrasts$contrasts %>%
    as_tibble() %>%
    mutate(contrast = str_replace_all(contrast, "ally", "alliance")) %>%
    mutate(contrast = str_replace_all(contrast, "neutral", "neutral relationship")) %>%
    mutate(contrast = str_replace_all(contrast, "rival", "rivalry")) %>%
    mutate(contrast = str_replace_all(contrast, "own", "own \\\\contrada")) %>%
    mutate(contrast = str_replace_all(contrast, "-", " \\\\vs")) %>%
    mutate(stars = stars(p.value)) %>%
    mutate(p.value = format_pval(p.value, remove_p = TRUE)) %>%
    knitr::kable(
               format = "latex",
               booktabs = TRUE,
               escape = FALSE,
               linesep = "",
               digits = c(0, 1, 1, 1, 1, 4),
               align = c("l", "r", "r", "r", "r", "r"),
               col.names = table_col_names,
               caption.short = table_caption_short,
               caption = table_caption) %>%
    footnote(number = c(footnote_table,
                        footnote_estimate,
                        footnote_signif), 
             escape = FALSE,
             threeparttable = TRUE,
             fixed_small_size = TRUE)


## ----att-oca-vs-onda-vs-neutral, eval = TRUE----------------------------------
## attitude towards tor participants for both oca and onda
att_stats_oca_onda <- 
    
    att_long_standard %>%
    filter(contrada_1 == "Tor") %>%
    mutate(tor_category = case_when(contrada_2 == "Oca" ~ "oca",
                                    contrada_2 == "Ond" ~ "onda",
                                    relationship == "neutral" ~ "neutral",
                                    relationship == "ally" ~ "ally",
                                    relationship == "own" ~ "own"
                                    )) %>%
    group_by(tor_category) %>%
    summary_stats("attitude") %>%
    ungroup()

## attitude of tor member to oca, to onda, and to neutral (excluding onda)

## list of contrada relatinoships and two contrade that vary for tor
list_contrada_relationships <- c("own", "ally", "neutral", "oca", "onda")

## extract mean and sd of attitude for each relationship or contrada
for (relationship_type in list_contrada_relationships) {
    
    ## filter contrada
    att_tmp <- 
        
        att_stats_oca_onda %>%
        filter(tor_category == relationship_type)

    ## pull mean    
    att_mean <- 
        
        att_tmp %>%
        pull(mean)

    ## create object
    assign(paste0("att_tor_", relationship_type, "_mean"), 
           att_mean)

    ## pull mean    
    att_sd <- 
        
        att_tmp %>%
        pull(sd)

    ## create object
    assign(paste0("att_tor_", relationship_type, "_sd"), 
           att_sd)

}


## ----print-att-contrasts, eval = TRUE, include = TRUE, echo = FALSE, results = "asis"----
## print table
table_att_posthoc


## ----att-additional-descriptives, eval = TRUE---------------------------------
## number of responses 

## list of relationships
list_contrada_relationships <- c("own", "rival", "ally")

## extract number of responses by contrada
for (relationship_type in contrada_relationships) {

    att_n_tmp <- 
        
        att_long_standard %>%
        filter(relationship == relationship_type) %>%
        filter(!is.na(attitude)) %>%
        nrow()

    assign(paste0("att_n_", relationship_type), 
           att_n_tmp)
 
}

## between the mid-point and the positive end of the scale for own contrada
att_own_below_7 <- 
    
    att_long_standard %>%
    filter(relationship == "own" & attitude <= 6) %>%
    nrow()

## between the mid-point and the negative end of the scale for rival contrada
att_rival_above_1 <- 
    
    att_long_standard %>%
    filter(relationship == "rival" & attitude >= 2) %>%
    nrow()

## between the mid-point and the negative end of the scale for allied contrada
att_ally_below_4 <- 
    
    att_long_standard %>%
    filter(relationship == "ally" & attitude <= 3) %>%
    nrow()


# SPDX-License-Identifier: GPL-3.0

## ----figures-falassi, eval = TRUE---------------------------------------------
## summarise figures from Falassi_1983

## smallest contrada by figure falassi
## figure
figure_falassi_min_total <- 
    
    size %>%
    filter(figure_falassi == min(figure_falassi)) %>%
    pull(figure_falassi) %>%
    as.integer()

## contrada
figure_falassi_min_contrada <- 

    size %>%
    filter(figure_falassi == min(figure_falassi)) %>%
    pull(contrada)

## largest contrada by figure falassi
## figure
figure_falassi_max_total <- 
    
    size %>%
    filter(figure_falassi == max(figure_falassi)) %>%
    pull(figure_falassi) %>%
    as.integer()

## contrada
figure_falassi_max_contrada <- 

    size %>%
    filter(figure_falassi == max(figure_falassi)) %>%
    pull(contrada)

## median contrada by figure falassi
## figure
figure_falassi_median_total <- 
    
    size %>%
    filter(figure_falassi == median(figure_falassi)) %>%
    pull(figure_falassi) %>%
    as.integer()

## contrada
figure_falassi_median_contrada <- 

    size %>%
    filter(figure_falassi == median(figure_falassi)) %>%
    pull(contrada)

## total number by figure falassi
figure_falassi_total <-

    size %>%
    summarise(total = sum(figure_falassi)) %>%
    pull() %>%
    as.integer()


## ----table-figures-falassi, eval = TRUE---------------------------------------
## create table with figures from Falassi_1983

## create tibble with relevant columns for table
size_figures_falassi <-

    size %>%
    mutate(rank = dense_rank(desc(figure_falassi))) %>%
    select(terzo, contrada, figure_falassi, rank) %>%
    tibble_by_terzo()

## table text

## column names
table_col_names <- c("terzo", "contrada", "\\emph{n}", "rank\\textsuperscript{2}")

## caption
table_caption <-  "\\label{SI-tab:falassi-data}Members by \\contrada in 1981\\textsuperscript{1}"
table_caption_short <-  "Members by \\contrada in 1981"

## general footnote
footnote_table <- "Data from \\\\citet[79]{Falassi_1983}; see \\\\cref{SI-sec:size-data} for details."

## rank footnote
footnote_rank <- "Descending rank (1: largest, 17: smallest)."

## final table
table_figures_falassi <-

    knitr::kable(size_figures_falassi,
                 format = "latex",
                 format.args = list(big.mark = ","),
                 booktabs = TRUE,
                 ## linesep = "",
                 linesep = c(rep("", 5), "\\addlinespace", 
                             rep("", 5), "\\addlinespace",
                             rep("", 4)),
                 col.names = table_col_names,
                 escape = FALSE,
                 caption.short = table_caption_short,
                 caption = table_caption) %>%
    column_spec(c(1:2), italic = TRUE) %>%
    footnote(number = c(footnote_table, 
                        footnote_rank),
             footnote_as_chunk = FALSE, 
             escape = FALSE, 
             threeparttable = TRUE, 
             fixed_small_size = TRUE) 


## ----figures-falassi-and-census-1981, eval = TRUE-----------------------------
## figures reported in Falassi_1983, pag. 79
size_1981_population <- 61974L          # total population

size_1981_walls <- 14984L               # residents inside the walls

size_1981_members <- 21818L             # contradaioli

## figures reported in Istat_1981, pag. 7
size_1981_census <- 61989L


## ----print-table-figures-falassi, include = TRUE, echo = FALSE, results = "asis"----
## table of 1981 figures from Falassi_1983
table_figures_falassi


## ----figures-sienafree, eval = TRUE-------------------------------------------
## summarise figures from SienaFree_2012

## calculate figures from interviews in SienaFree_2012
size <- 
    
    size %>%
    mutate_if(is.integer, as.numeric) %>%
    mutate(figure_sienafree = case_when(# where approximate, use figure provided
                   figure_overall != "NA" ~ figure_overall,
                                        # where range, calculate mid-point
               figure_range_min != "NA" & figure_range_max != "NA" ~ 
                   (figure_range_min + figure_range_max)/2,
                                        # where divided into old and young protettori, sum
               figure_old != "NA" & figure_young != "NA" ~ 
                   figure_old + figure_young))

## smallest contrada by figure sienafree
## figure
figure_sienafree_min_total <- 
    
    size %>%
    filter(figure_sienafree == min(figure_sienafree, na.rm = TRUE)) %>%
    pull(interview_text)

## contrada
figure_sienafree_min_contrada <- 

    size %>%
    filter(figure_sienafree == min(figure_sienafree, na.rm = TRUE)) %>%
    pull(contrada)

## largest contrada by figure sienafree
## figure
figure_sienafree_max_total <- 
    
    size %>%
    filter(figure_sienafree == max(figure_sienafree, na.rm = TRUE)) %>%
    pull(interview_text)

## contrada
figure_sienafree_max_contrada <- 

    size %>%
    filter(figure_sienafree == max(figure_sienafree, na.rm = TRUE)) %>%
    pull(contrada)

## median contrade by figure sienafree

## as there are 16 contrade with data, choose pos. 8 and 9

## contrada pos. 8
## figure
figure_sienafree_median_one_total <- 
    
    size %>%
    arrange(figure_sienafree) %>%
    slice(8) %>%
    pull(interview_text)
    
## contrada
figure_sienafree_median_one_contrada <- 

    size %>%
    arrange(figure_sienafree) %>%
    slice(8) %>%
    pull(contrada)

## contrada pos. 8
## figure
figure_sienafree_median_two_total <- 
    
    size %>%
    arrange(figure_sienafree) %>%
    slice(9) %>%
    pull(interview_text)
    
## contrada
figure_sienafree_median_two_contrada <- 

    size %>%
    arrange(figure_sienafree) %>%
    slice(9) %>%
    pull(contrada)

## calculate totals for each column, including total minimum and maximum
totals_sienafree <- 

    size %>%
    select_if(is.numeric) %>% 
    summarise_each(list(~ sum(., na.rm = TRUE))) %>%
    mutate(total_sienafree_min = figure_overall + figure_range_min + 
               figure_old + figure_young,
           total_sienafree_max = figure_overall + figure_range_max +
               figure_old + figure_young)

## total lower bound
size_figures_sienafree_lwr <- 
    
    totals_sienafree %>% 
    pull(total_sienafree_min) %>%
    as.integer()

## total upper bound
size_figures_sienafree_upr <- 
    
    totals_sienafree %>% 
    pull(total_sienafree_max) %>%
    as.integer()


## ----table-figures-sienafree, eval = TRUE-------------------------------------
## create table with figures from SienaFree_2012

## create tibble with relevant columns for table
size_figures_sienafree <-

    size %>%
    mutate(reference = paste0("\\citet{Voltolini_", contrada, "}")) %>%
    mutate(rank = min_rank(desc(figure_sienafree))) %>%
    select(terzo, contrada, interview_text, rank, reference) %>%
    tibble_by_terzo()

## manipulate column names to include footnote symbols
size_figures_sienafree <- 
    
    size_figures_sienafree %>%
    mutate(contrada = case_when(contrada == "Aquila" ~ 
                                    paste0(contrada, "\\emph{\\textsuperscript{4}}"),
                                contrada == "Chiocciola" ~ 
                                    paste0(contrada, "\\emph{\\textsuperscript{5}}"),
                                TRUE ~ as.character(contrada)
                                ))

## table text

## column names
table_col_names <- c("terzo", 
                     "contrada", 
                     "\\emph{n}\\textsuperscript{2}", 
                     "rank\\textsuperscript{3}", 
                     "reference")

## caption
table_caption <- "\\label{SI-tab:sienafree-data}\\Protettori by \\contrada in 2012--2013\\textsuperscript{1}"
table_caption_short <- "\\Protettori by \\contrada in 2012--2013"

## general footnote
footnote_table <- "Data from \\\\citet{SienaFree_2012}; see \\\\cref{SI-sec:size-data} for details."

## quote footnote
footnote_quote <- "Variation in text reflects the original quotes in the interviews."

## rank footnote
footnote_rank <- "Descending rank (1: largest, 16: smallest), using the mid-point for ranges. We assign the same rank to equivalent values."

## extra details for each contrada
footnote_aqu <- "The figure refers specifically to \\\\emph{contradaioli protettori} (\\\\ie members of \\\\aqu who pay the annual fee), as distinct from \\\\emph{protettori semplici} (\\\\lit{ordinary protectors}), namely individuals who pay the fee but have no rights relating to \\\\contrada governance (\\\\eg they may be members of another \\\\contrada); see \\\\cref{MT-sec:membership,SI-sec:size-discussion} for details."

footnote_chi <- "The interview mentions approximately 2,080 \\\\protettori, plus approximately 350 \\\\emph{piccoli} (\\\\lit{little ones}) up to 16 years of age. We provide the combined figure, on the assumption that \\\\protettore status applies to both; see \\\\cref{MT-sec:membership,SI-sec:size-discussion} for details."

## make spaces in table appear as NA
options(knitr.kable.NA = "NA") 

## final table
table_figures_sienafree <-

    knitr::kable(size_figures_sienafree,
                 format = "latex",
                 format.args = list(big.mark = ","),
                 booktabs = TRUE,
                 ## linesep =  "",
                 linesep = c(rep("", 5), "\\addlinespace", 
                             rep("", 5), "\\addlinespace",
                             rep("", 4)),
                 col.names = table_col_names,
                 escape = FALSE,
                 caption.short = table_caption_short,
                 caption = table_caption) %>%
    column_spec(c(1:2), italic = TRUE) %>%
    footnote(number = c(footnote_table,
                        footnote_quote,
                        footnote_rank,
                        footnote_aqu,
                        footnote_chi
                        ),
             footnote_as_chunk = FALSE, 
             escape = FALSE, 
             threeparttable = TRUE, 
             fixed_small_size = TRUE)


## ----figures-census-2013, eval = TRUE-----------------------------------------
## figures reported in Comune-di-Siena_census_2013, tables 2 and 9
size_2013_population <- 54126L          # total population

size_2013_walls <- 10623L               # residents inside the walls


## ----print-table-figures-sienafree, include = TRUE, echo = FALSE, results = "asis"----
## table of 2012--2013 figures from SienaFree_2012
table_figures_sienafree


## ----sienafree-additional-data-dra, eval = TRUE-------------------------------
## get numbers from Drago interview, see Voltolini_Drago

## total
dra_total <- 
    
    size %>% 
    filter(contrada == "Drago") %>% 
    pull(figure_overall) %>%
    as.integer()

## by gender

## women

## n
dra_n_female <- 614L

## perc
dra_p_female <- 
    
    (dra_n_female / dra_total) * 100

## by age

## define age groups
dra_age_group <-     
    c("under 12", 
      "between 12 and 16", 
      "between 17 and 30", 
      "between 30 and 40", 
      "between 40 and 65", 
      "over 65")
## define counts
dra_n <-
    c(289, 
      75, 
      240, 
      180, 
      415, 
      115)

## combine age groups
dra_age_groups <- 
    
    tibble(age_group = dra_age_group, 
           n = dra_n, 
           contrada = "dra")

## old
dra_old <- 
    
    dra_age_groups %>%
    filter(!str_detect(string = age_group, pattern = "12")) %>%
    summarise(total = sum(n)) %>%
    pull()

## percentage
dra_p_old <- 
    
    (dra_old / dra_total) * 100

## young
dra_young <- 
    
    dra_age_groups %>%
    filter(str_detect(string = age_group, pattern = "12")) %>%
    summarise(total = sum(n)) %>%
    pull()

## percentage
dra_p_young <- 
    
    (dra_young / dra_total) * 100

## combine old and young
dra_old_young <- 
    
    tibble(age_group = c("young", "old"), 
           n = c(dra_young, dra_old), 
           contrada = "dra")


## ----sienafree-additional-data-chio, eval = TRUE------------------------------
## get numbers from Chiocciola interview, see Voltolini_Chiocciola

## old
chio_old <- 
    
    size %>% 
    filter(contrada == "Chiocciola") %>% 
    pull(figure_old)

## young (defined as under 16)
chio_young <- 
    
    size %>% 
    filter(contrada == "Chiocciola") %>% 
    pull(figure_young)

## total
chio_total <- 
    
    chio_old + chio_young

## percentage old
chio_p_old <- 
    
    (chio_old / chio_total) * 100

## percentage young
chio_p_young <- 
    
    (chio_young / chio_total) * 100

## combine age groups
chio_old_young <- 
    
    tibble(age_group = c("young", "old"), 
           n = c(chio_young, chio_old), 
           contrada = "chio")


## ----sienafree-additional-data-nic, eval = TRUE-------------------------------
## get numbers from Nicchio interview, see Voltolini_Nicchio

## total
nic_total <-
    
    size %>% 
    filter(contrada == "Nicchio") %>% 
    pull(figure_overall) %>%
    as.integer()

## young (defined as minors)
nic_young <- 1000L

## old
nic_old <- 
    
    (nic_total - nic_young)  %>%
    as.integer()

## percentage young
nic_p_young <- 
    
    (nic_young / nic_total) * 100

## percentage old
nic_p_old <- 
    
    (nic_old / nic_total) * 100

## combine age groups
nic_old_young <- 
    
    tibble(age_group = c("young", "old"), 
           n = c(nic_young, nic_old), 
           contrada = "nic")


## ----sienafree-additional-data-tor, eval = TRUE-------------------------------
## get numbers from Torre interview, see Voltolini_Torre

## total
tor_total <- 
    
    size %>% 
    filter(contrada == "Torre") %>% 
    pull(figure_overall) %>%
    as.integer()

## by age

## over 70
tor_over_70 <- 400L

## under 14
tor_under_14 <- 550L

## between 14 and 16
tor_between_14_16 <- 160L

## between 16 and 70
tor_between_16_70 <- 
    
    (tor_total - (tor_over_70 + tor_under_14 + tor_between_14_16))  %>%
    as.integer()

## define age groups
tor_age_group <-     
    c("under 14", 
      "between 14 and 16", 
      "between 16 and 70", 
      "over 70")
## define counts
tor_n <- 
    c(tor_under_14, 
      tor_between_14_16, 
      tor_between_16_70, 
      tor_over_70)

## combine age groups
tor_age_groups <- 
    
    tibble(age_group = tor_age_group, 
           n = tor_n, 
           contrada = "tor")

## old
tor_old <- 
    
    tor_age_groups %>%
    filter(!str_detect(string = age_group, pattern = "14")) %>%
    summarise(total = sum(n)) %>%
    pull() %>%
    as.integer()

## percentage
tor_p_old <- 
    
    (tor_old / tor_total) * 100

## young (defined as under 16)
tor_young <- 
    
    tor_age_groups %>%
    filter(str_detect(string = age_group, pattern = "14")) %>%
    summarise(total = sum(n)) %>%
    pull() %>%
    as.integer()

## percentage
tor_p_young <- 
    
    (tor_young / tor_total) * 100

## combine age groups
tor_old_young <- 
    
    tibble(age_group = c("young", "old"), 
           n = c(tor_young, tor_old), 
           contrada = "tor")


## ----sienafree-ages, eval = TRUE----------------------------------------------
## summarise figures with old and young from SienaFree_2012

## create tibble with old and young for chio, dra, and tor
old_young <- 
    
    bind_rows(chio_old_young, 
              dra_old_young, 
              tor_old_young) %>%
    spread(key = age_group, value = n) %>%
    mutate(p_old = (old / (old + young)) * 100,
           p_young = (young / (old + young)) * 100)

## old

## smallest percentage of old
## figure
old_min_p <- 
    
    old_young %>%
    filter(p_old == min(p_old, na.rm = TRUE)) %>%
    pull(p_old)

## contrada
old_min_contrada <- 

    old_young %>%
    filter(p_old == min(p_old, na.rm = TRUE)) %>%
    mutate(contrada = paste0("\\", contrada)) %>%
    pull(contrada)

## largest percentage of old
## figure
old_max_p <- 
    
    old_young %>%
    filter(p_old == max(p_old, na.rm = TRUE)) %>%
    pull(p_old)

## contrada
old_max_contrada <- 

    old_young %>%
    filter(p_old == max(p_old, na.rm = TRUE)) %>%
    mutate(contrada = paste0("\\", contrada)) %>%
    pull(contrada)

## young

## smallest percentage of young
## figure
young_min_p <- 
    
    old_young %>%
    filter(p_young == min(p_young, na.rm = TRUE)) %>%
    pull(p_young)

## contrada
young_min_contrada <- 

    old_young %>%
    filter(p_young == min(p_young, na.rm = TRUE)) %>%
    mutate(contrada = paste0("\\", contrada)) %>%
    pull(contrada)

## largest percentage of young
## figure
young_max_p <- 
    
    old_young %>%
    filter(p_young == max(p_young, na.rm = TRUE)) %>%
    pull(p_young)


## ----table-sienafree-ages, eval = TRUE----------------------------------------
## create table with old and young from SienaFree_2012

## contrada
young_max_contrada <- 

    old_young %>%
    filter(p_young == max(p_young, na.rm = TRUE)) %>%
    mutate(contrada = paste0("\\", contrada)) %>%
    pull(contrada)

## adjust tibble for table
table_old_young <- 

    old_young %>%
    arrange(contrada) %>%
    mutate(contrada = case_when(contrada == "chio" ~ "\\emph{Chiocciola}",
                                contrada == "dra" ~ "\\emph{Drago}\\textsuperscript{3}",
                                contrada == "tor" ~ "\\emph{Torre}\\textsuperscript{4}"
                                )) %>% 
    select(contrada, young, p_young, old, p_old)

## table text

## column names
table_col_names <- c(
    "\\emph{contrada}\\textsuperscript{2}", # macro introduces space between contrada and ^2 
    "\\emph{n}", "\\%", 
    "\\emph{n}", "\\%")

## caption
table_caption <- "\\label{SI-tab:sienafree-old-young}\\Protettori up to 16 years of age \\vs older by \\contrada in 2012--2013\\textsuperscript{1}"
table_caption_short <- "\\Protettori up to 16 years of age \\vs older by \\contrada in 2012--2013"

## general footnote
footnote_table <- "Data from \\\\citet{SienaFree_2012}; see \\\\cref{SI-sec:size-data} for details."

## contrada footnote
footnote_contrada <-  "Figures for \\\\chio and \\\\tor are approximate; see \\\\cref{SI-tab:sienafree-data} for additional information."

## extra details for each contrada
footnote_dra <- "See \\\\cref{fig:sienafree-ages-dra-tor-1} for details."

footnote_tor <- "See \\\\cref{fig:sienafree-ages-dra-tor-2} for details."
    
## final table
table_sienafree_ages <-

    knitr::kable(table_old_young,
                 format = "latex",
                 format.args = list(big.mark = ","),
                 booktabs = TRUE,
                 linesep =  "",
                 digits = c(0, 0, 0, 1, 0, 1), 
                 align = c("l", "r", "r", "r", "r"),
                 col.names = table_col_names,
                 escape = FALSE,
                 caption.short = table_caption_short,
                 caption = table_caption) %>%
    column_spec(c(2, 4), width = "3em") %>% # if not set, cols occupy far left and far right
    add_header_above(c(" " = 1,
                       "$\\\\leq$ 16 years old" = 2, 
                       "$>$ 16 years old" = 2),
                     escape = FALSE) %>% 
    footnote(number = c(footnote_table, 
                        footnote_contrada,
                        footnote_dra,
                        footnote_tor
                        ),
             footnote_as_chunk = FALSE, 
             escape = FALSE, 
             threeparttable = TRUE, 
             fixed_small_size = TRUE)


## ----print-table-sienafree-ages, include = TRUE, echo = FALSE, results = "asis"----
## table with old vs young data for chio, dra, and tor
table_sienafree_ages


## ----sienafree-ages-dra, eval = TRUE, include = TRUE, echo = FALSE, fig.cap = paste0("Distribution of \\protettori across age groups for \\dra in 2012--2103. The \\citet{SienaFree_2012} interview \\citep{Voltolini_Drago} specifies that the ", format(dra_total, big.mark = ","), " \\protettori included 289 up to age 12 (\\ita{bambini}, \\lit{children}), 75 up to age 16 (\\ita{novizi}, \\lit{novices}), 240 between 17 and 30 (\\ita{giovani}, \\lit{youths}), 180 between 30 and 40, 415 between 40 and 65, and 115 over 65. In the plot we adjust the age group boundaries for internal consistency."), results = "asis", fig.align = 'center', fig.keep = "all", fig.show = "asis",  eval.after = "fig.cap"----
# plot
plot_dra_age_groups <-
    
    dra_age_groups %>%
    mutate(rel_freq = (n / dra_total)) %>%
    ggplot(aes(x = dra_age_group)) +
    geom_col(aes(x = factor(dra_age_group,
                            levels = dra_age_group), 
                 y = rel_freq)) +
    geom_text(aes(y = rel_freq,
                  label = percent(rel_freq, suffix = NULL)), 
              vjust = -0.5) +
    scale_x_discrete("age (years)", 
                     labels = c(expression(""<=11), 
                                expression(12-16), 
                                expression(17-30), 
                                expression(31-40),
                                expression(41-65),
                                expression("">65)
                                )) +
    scale_y_continuous(expression(italic(protettori)~" (%)"), 
                       limits = c(0, 0.7),
                       labels = percent_format(accuracy = 1, suffix = NULL)) +
    guides(fill = FALSE)


## ----sienafree-ages-tor, eval = TRUE, include = TRUE, echo = FALSE, fig.cap = paste0("Distribution of \\protettori across age groups for \\tor in 2012--2013. The \\citet{SienaFree_2012} interview \\citep{Voltolini_Torre} specifies that the approximately ", format(tor_total, big.mark = ","), " \\protettori included 550 up to age 14 (\\ita{piccoli}, \\lit{little ones}), 160 between 14 and 16 (\\ita{giovani}, \\lit{youths}), and 400 over 70. By implication, then, ", format(tor_between_16_70, big.mark = ","), " \\protettori were between 17 and 70. In the plot we adjust the age group boundaries for internal consistency."), results = "asis", fig.align = 'center', fig.keep = "all", fig.show = "asis", eval.after = "fig.cap"----
## plot
plot_tor_age_groups <-

    tor_age_groups %>%
    mutate(rel_freq = (n / tor_total)) %>%
    ggplot(aes(x = age_group)) +
    geom_col(aes(x = factor(age_group,
                            levels = age_group), 
                 y = rel_freq)) +
    geom_text(aes(y = rel_freq,
                  label = percent(rel_freq, suffix = NULL)), 
              vjust = -0.5) +
    scale_x_discrete("age (years)", 
                     labels = c(expression(""<=13), 
                                expression(14-16), 
                                expression(17-70), 
                                expression("">70))) +
    scale_y_continuous(name = NULL, ## expression(italic(protettori)~" (%)"), 
                       limits = c(0, 0.7),
                       labels = percent_format(accuracy = 1, suffix = NULL)) +
    guides(fill = FALSE)



## ----sienafree-ages-dra-tor, eval = TRUE, include = TRUE, echo = FALSE,  fig.scap = "Distribution of \\protettori across age groups", fig.subcap = c(paste0("\\dra ($n = ", format(dra_total, big.mark = ","), "$)"), paste0("\\tor ($n = ", format(tor_total, big.mark = ","), "$)")), fig.cap = caption_text, fig.align = "center", fig.pos = "htb!", fig.ncol = 2, out.width = ".48\\linewidth", eval.after = "fig.cap"----

## text for figure caption
caption_text <- 

    paste0("Distribution of \\protettori across age groups for (\\subref{fig:sienafree-ages-dra-tor-1}) \\dra and (\\subref{fig:sienafree-ages-dra-tor-2}) \\tor in 2012--2013. For \\subref{fig:sienafree-ages-dra-tor-1}, the \\citet{SienaFree_2012} interview \\citep{Voltolini_Drago} specifies that the ", format(dra_total, big.mark = ","), " \\protettori included 289 up to age 12 (\\ita{bambini}, \\lit{children}), 75 up to age 16 (\\ita{novizi}, \\lit{novices}), 240 between 17 and 30 (\\ita{giovani}, \\lit{youths}), 180 between 30 and 40, 415 between 40 and 65, and 115 over 65. For  \\subref{fig:sienafree-ages-dra-tor-2}, the \\citet{SienaFree_2012} interview \\citep{Voltolini_Torre} specifies that the approximately ", format(tor_total, big.mark = ","), " \\protettori included 550 up to age 14 (\\ita{piccoli}, \\lit{little ones}), 160 between 14 and 16 (\\ita{giovani}), and 400 over 70; by implication, then, ", format(tor_between_16_70, big.mark = ","), " \\protettori were between 17 and 70. In both plots we adjust the age group boundaries for internal consistency.")

## size of text
subfig_label_size <- 20

## print plots
plot_dra_age_groups + theme_classic(base_size = subfig_label_size)
plot_tor_age_groups  + theme_classic(base_size = subfig_label_size)


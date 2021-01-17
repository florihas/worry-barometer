# load/install packages ---------------------------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, gganimate, gifski, av, png, RColorBrewer,
               readxl, hrbrthemes, transformr, extrafont)

# get data ----------------------------------------------------------------

table <- read_excel("sorgenbarometer_1988-2020.xlsx", sheet = 1) 
#source: https://credit-suisse.com/media/assets/corporate/docs/about-us/responsibility/worry-barometer/schlussbericht-credit-suisse-sorgenbarometer-2020.pdf

# manipulate data ---------------------------------------------------------

## pivot data

table3 <- table %>%
  pivot_longer(c(4:36), names_to = "year", names_transform = list(year = as.numeric), 
               values_to = "freq", )

## for line chart: generate ranking by year group (step only necessary when including rank in plot)

anim_table <- table3 %>%
  group_by(year) %>%
  arrange(-freq) %>% #sorted by frequency
  mutate(rank=row_number()) %>%
  ungroup()

## for bar chart: generate ranking by year group, only for six topics

anim_table2 <- table3 %>%
  group_by(year) %>%
  arrange(-freq) %>%
  filter(Topic_lab %in% c("AHV", # already filter at this stage because of ranking
                          "Gesundheit, KK", 
                          "Umwelt, Klima", 
                          "Arbeitslosigkeit", 
                          "Drogen, Alkohol", 
                          "Covid-19 & Folgen",
                          "AusländerInnen")) %>%
  mutate(rank=row_number()) %>%
  ungroup() %>% 
  arrange(year, Topic_lab) #sorted by year and topic, important!



# visualize data ----------------------------------------------------------

# line chart --------------------------------------------------------------

#translate topics into English
anim_table$Topic_lab_en <- recode(anim_table$Topic_lab,
                      "AHV" = "Pensions", 
                      "Gesundheit, KK" = "Health, Insur.", 
                      "Umwelt, Klima" = "Envir., Climate", 
                      "Arbeitslosigkeit" = "Unemployment", 
                      "Drogen, Alkohol" = "Drugs, Alcohol", 
                      "Covid-19 & Folgen" = "Covid & Effects",
                      "AusländerInnen" = "Foreigners"
                      )


line <- anim_table %>% 
  # filter(year >= 1995) %>% 
  # filter(rank<=10) %>% #top 10
  filter(Topic_lab %in% c("AHV", #filter at this stage -> possible to remove filter and show more topics
                          "Gesundheit, KK", 
                          "Umwelt, Klima", 
                          "Arbeitslosigkeit", 
                          "Drogen, Alkohol", 
                          "Covid-19 & Folgen",
                          "AusländerInnen")) %>%
  arrange(year) %>% # order dataframe by x value; important!
  # drop_na(freq) %>% 
  ggplot(aes(x=year, y=freq, group=Topic_lab, color=Topic_lab)) +
  geom_point(
    # aes(group = seq_along(year)), # points stay
             alpha = 1, size = 2.5) +
  geom_line(alpha = 0.9, size = 2, lineend = "round") +
  # geom_text(aes(x = 2020.5, label = paste0(Topic_lab_en, " (", rank, ".)"), size = 4), hjust = 0) + #labels: topics + ranks in bracket
  geom_text(aes(x = 2020.5, label = Topic_lab_en), hjust = 0, size = 6) + #labels: topics
  geom_segment(aes(xend = 2020, yend = freq), linetype = 2) + # connecting points and labels
  scale_color_brewer(palette = "Dark2", direction = 1) +
  theme_ipsum_rc() + #rc = font: roboto condensed
  theme(plot.title = element_text(size = 30),
        plot.caption = element_text(size = 13),
        axis.text.x = element_text(size = 17),
        axis.text.y = element_text(size = 17),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none") +
  coord_cartesian( #expand x axis so topic labels don't get cut off
    xlim = c(NA, 2026.5), 
    ylim = c(0, NA)
    ) +
  # scale_x_continuous(expand = c(.1, .1)) + #different way to expand x axis
  # labs( #German
  #   title = "Was die Schweiz beschäftigt", 
  #   x = "",
  #   y = "",
  #   color = "",
  #   caption = "in % Stimmberechtigte, Anteil Nennungen
  #   Data: gfs.bern, CS Sorgenbarometer | N = jeweils ca. 1000") +
  labs(
    title = "What the Swiss Are Concerned About", 
    x = "",
    y = "",
    color = "",
    caption = "in % of eligible voters, share of mentions
    Data: gfs.bern, CS Worry Barometer (1988-2020) | N = approx. 1000 each") +
  transition_reveal(year)


line

##gif for twitter
animate(line, 100, fps = 30, duration = 40,  width = 720, height = 720, start_pause = 10, end_pause = 120)
anim_save("line.gif")

##video for twitter
animate(line, 100, fps = 30, duration = 40, renderer = av_renderer("line.mp4"), 
        width = 720, height = 720, start_pause = 10, end_pause = 120)


# bar chart race ------------------------------------------------------

#translate topics into English

anim_table2$Topic_lab_en <- recode(anim_table2$Topic_lab,
                                   "AHV" = "Pensions", 
                                   "Gesundheit, KK" = "Health, Insur.", 
                                   "Umwelt, Klima" = "Envir., Climate", 
                                   "Arbeitslosigkeit" = "Unemployment", 
                                   "Drogen, Alkohol" = "Drugs, Alcohol", 
                                   "Covid-19 & Folgen" = "Covid & Effects",
                                   "AusländerInnen" = "Foreigners"
)

bar <- 
  anim_table2 %>%
  drop_na(freq) %>% #so labels don't appear from the beginning
  # filter(year >= 1995) %>%
  # filter(rank<=10) %>% #top 10 
  ggplot(aes(rank)) +
  geom_tile(aes(
    y = freq/2,
    height = freq,
    width = 0.9,
    fill = Topic_lab
  ), alpha = 0.8, color = NA) +
  scale_fill_brewer(palette = "Dark2") + #for bars
  scale_color_brewer(palette = "Dark2") + #for text labels
  geom_text(aes(y = 0, label = paste(Topic_lab_en, " "), color = Topic_lab), #paste for space between text and bar
            size = 6, vjust = 0.2, hjust = 1) + 
  # geom_text(aes(y = freq, label = freq, hjust = 0)) + #value labels for each bar
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  # labs( #German
  #   title = "Was die Schweiz {closest_state} beschäftigt", 
  #   x = "", 
  #   y = "in % Stimmberechtigte, Anteil Nennungen",
  #   caption = "Data: gfs.bern, CS Sorgenbarometer | N = jeweils ca. 1000"
  # ) +
  labs( #English
    title = "What the Swiss Are Concerned About in {closest_state}", 
    x = "",
    y = "in % of eligible voters, share of mentions",
    color = "",
    caption = "Data: gfs.bern, CS Worry Barometer | N = approx. 1000 each") +
  theme_ipsum_rc() +
  theme(plot.title = element_text(size = 30),
        plot.caption = element_text(size = 13),
        axis.text.x = element_text(size = 17),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 15),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.margin = margin(30, 30, 30, 140)) +
  transition_states(year, transition_length = 4, state_length = 1, wrap = FALSE) +
  ease_aes("cubic-in-out")

bar

##gif for twitter
animate(bar, 100, fps = 30, duration = 40,  width = 720, height = 720, start_pause = 10, end_pause = 120)
anim_save("bar.gif")

##video for twitter
animate(bar, 100, fps = 30, duration = 40, renderer = av_renderer("bar.mp4"), 
        width = 720, height = 720, start_pause = 10, end_pause = 120)

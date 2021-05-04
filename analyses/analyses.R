# Initializing ====
# Loading libraries
load_libraries <- function(){
  if (!require("dplyr"))
    install.packages("dplyr")
  if (!require("forcats"))
    install.packages("forcats")
  if(!require("geobr"))
    install.packages("geobr")
  if (!require("gganimate"))
    install.packages("gganimate")
  if (!require("ggplot2"))
    install.packages("ggplot2")
  if (!require("ggthemes"))
    install.packages("ggthemes")
  if(!require("gifski"))
    install.packages("gifski")
  if(!require("highcharter"))
    install.packages("highcharter")
  if(!require("hrbrthemes"))
    install.packages("hrbrthemes")
  if(!require("lubridate"))
    install.packages("lubridate")
  if(!require("magrittr"))
    install.packages("magrittr")
  if(!require("maps"))
    install.packages("maps")
  if(!require("plotly"))
    install.packages("plotly")
  if(!require("psych"))
    install.packages("psych")
  if(!require("scales"))
    install.packages("scales")
  if(!require("sf"))
    install.packages("sf")
  if(!require("stringr"))
    install.packages("stringr")
  if(!require("viridis"))
    install.packages("viridis")
}

load_libraries()

# Getting data
df <- read.csv('data\\df.csv', encoding = 'UTF-8')[-1]

# Creating design objects ====
create_design <- function(){
  # Fill color
  fill_color <- '#f68060'
  
  # Caption
  caption <- 'Dados do DATASUS entre os anos de 2010 e 2019 extraídos com PySUS.'
  
  # Setting main theme
  project_theme <- 
    theme_minimal() + 
    theme(legend.position = "none",
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 15),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14))
  
  # Setting map theme
  project_map_theme <-
    theme_minimal() + 
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 15),
          axis.text = element_blank(),
          axis.title = element_blank())
  
  list_of_objects <- list(
    'fill_color' = fill_color,
    'caption' = caption,
    'project_theme' = project_theme,
    'project_map_theme' = project_map_theme
  )
  
  return(list_of_objects)
}

design <- create_design()

# Initial cleaning ====
clean_data <- function(df){
  # Making missings known
  df[df == 'NaN'] <- NA
  
  # State as factor
  df$estado %<>% as.factor
  
  # Arranging dates
  format <- "%d%m%Y"
  df$DTOBITO <- parse_date_time(df$DTOBITO, format)
  df$DTNASC <- parse_date_time(df$DTNASC, format)
  
  # Creating 'month' column
  df$mes <- month(df$DTOBITO)
  
  # Sex as factor
  df$SEXO %<>% as.factor
  
  # Race/color as factor
  df$RACACOR[df$RACACOR == ''] <- NA
  df$RACACOR %<>% as.factor
  
  
  # Medical assistance as factor
  df$ASSISTMED[df$ASSISTMED == ''] <- NA
  df$ASSISTMED %<>% as.factor
  
  # Mother's education as factor
  df$ESCMAE %<>% as.factor
  df$ESCMAE <- factor(df$ESC,
                      levels = c('1 a 3 anos',
                                 '4 a 7 anos',
                                 '8 a 11 anos',
                                 '12 e mais'))
  
  # Marital status as factor
  df$ESTCIV %<>% as.factor
  df$ESTCIV <- factor(df$ESTCIV,
                      levels = c('Solteiro',
                                 'União consensual',
                                 'Casado',
                                 'Separado judicialmente',
                                 'Viúvo'))
  
  levels(df$ESTCIV)[1] <- 'Solteiro/a'
  levels(df$ESTCIV)[3] <- 'Casado/a'
  levels(df$ESTCIV)[4] <- 'Separado/a judicialmente'
  levels(df$ESTCIV)[5] <- 'Viúvo/a'
  
  # Education as factor
  df$ESC %<>% as.factor
  df$ESC <- factor(df$ESC,
                   levels = c('Nenhuma',
                              '1 a 3 anos',
                              '4 a 7 anos',
                              '8 a 11 anos',
                              '12 e mais'))
  
  # Ocupation as factor
  df$OCUP %<>% as.factor
  
  df$OCUP[str_detect(df$OCUP, "\\d")] <- NA
  #' This last function corrects a problem found in the data.
  #' A lot of occupations were not coded correctly by the CBO.
  #' The reason why this happened is unclear.
  #' This resulted in various data points containing 'random' numbers,
  #' such as '1021', '11112' and '14221'. This should be an ocupation,
  #' however, I can't seem to find the code for it, and the CBO isn't
  #' covering this properly. If you know something about this, feel free
  #' to contact me.
  #' 
  #' What the code does is the following:
  #' Detect in `df$OCUP` any string that contains numbers (`\\d` in regex).
  #' When you find this, replace with `NA` (`<- NA`)
  
  # Place of occurence as factor
  df$LOCOCOR[df$LOCOCOR == '6'] <- NA #' There isn't code for '6': replace with NA
  df$LOCOCOR %<>% as.factor
  
  # Received surgery (yes/no) as factor
  df$CIRURGIA[df$CIRURGIA == ''] <- NA
  df$CIRURGIA %<>% as.factor
  
  return(df)
}

df <- clean_data(df)

# Creating dataframes with number of deaths by month/year ====
year_total <- df %>% group_by(ano) %>% count()

month_total <- df %>% group_by(mes) %>% count()

ym_total <- df %>% 
  group_by(ano, mes) %>% 
  count() %>% 
  tidyr::unite(mes, ano, col = 'index', sep = '-')

ym_total

# Creating variations in percentage from one time period to another ====
#' *year_variation*
year_variation <- as.data.frame(year_total)
rownames(year_variation) <- year_variation$ano
year_variation %<>% select(-ano)
year_variation

year_variation <- as.ts(year_variation, start = c(2010), frequency = 1)

year_variation <- year_variation/stats::lag(year_variation, -1) - 1

year_variation <- as.data.frame(year_variation)
#' *month variation*
month_variation <- as.data.frame(month_total)
rownames(month_variation) <- month_variation$mes
month_variation %<>% select(-mes)
month_variation

month_variation <- as.ts(month_variation, start = c(1), frequency = 1)

month_variation <- month_variation/stats::lag(month_variation, -1) - 1

month_variation <- as.data.frame(month_variation)

#' *ym_variation*
ym_variation <- as.data.frame(ym_total)
rownames(ym_variation) <- ym_variation$index
ym_variation %<>% select(-index)
rownames(ym_variation) <- NULL

ym_variation <- as.ts(ym_variation, start = c(2010, 1), frequency = 12)

ym_variation <- ym_variation/stats::lag(ym_variation, -1) - 1

ym_variation <- as.data.frame(ym_variation)

# Visualizing demographics: sex ====
sex_by_year <- 
  
  df %>% 
  # Cleaning
  mutate(ano = as.factor(ano)) %>% 
  
  filter(SEXO != 'NA') %>%
  
  # Plot
  ggplot(aes(x = SEXO, fill = design$fill_color)) +
  
  # Geom
  geom_bar() +
  
  # Y-axis: Limits and ticks
  scale_y_continuous(breaks = c(0, 2500, 5000, 7500, 10000),
                     limits = c(0, 11000)) +
  
  # Labels
  labs(x = 'Sexo',
       y = '',
       title = 'Número de Suicídios Registrados por Sexo no Brasil',
       subtitle = 'Ano: {closest_state}',
       caption = design$caption) +
  
  # Theme
  design$project_theme + 
  
  theme(panel.grid.major.x = element_blank()) +
  
  # Transition
  transition_states(ano)

p <- animate(sex_by_year, 
             duration = 20,
             start_pause = 3,
             end_pause = 3,
             renderer = gifski_renderer(),
             width = 750,
             height = 750)

anim_save("figures\\suicides_by_sex_by_year.gif", p)

# Visualizing demographis: race/color ====
suicides_by_race <- 
  
  df %>% 
  # Cleaning
  
  filter(RACACOR != 'NA') %>%
  
  group_by(RACACOR) %>% 
  
  summarize(n = n()) %>%
  
  arrange(desc(n), .by_group = T) %>%
  
  # Plot
  ggplot(aes(x = reorder(RACACOR, n), y = n, fill = design$fill_color)) +
  
  # Geom
  geom_tile(aes(y = n/2, 
                height = n,
                width = 0.9), fill = design$fill_color) +
  
  # Y-axis: Limits and ticks
  scale_y_continuous(breaks = seq(0, 60000, 10000),
                     limits = c(0, 60000)) +
  
  # Labels
  labs(x = '',
       y = '',
       title = 'Número de Suicídios Registrados por Raça/Cor no Brasil',
       caption = design$caption) +
  
  # Flipping coordinates
  coord_flip() +
  
  # Theme
  design$project_theme

suicides_by_race

# Visualizing demographis: medical assistance ====
suicides_medical_assistance <- 
  
  df %>% 
  # Cleaning
  
  filter(ASSISTMED != 'NA') %>%
  
  group_by(ASSISTMED) %>% 
  
  summarize(n = n()) %>%
  
  arrange(desc(n), .by_group = T) %>%
  
  # Plot
  ggplot(aes(x = reorder(ASSISTMED, n), y = n, fill = design$fill_color)) +
  
  # Geom
  geom_tile(aes(y = n/2, 
                height = n,
                width = 0.9), fill = design$fill_color) +
  
  # Y-axis: Limits and ticks
  scale_y_continuous(breaks = seq(0, 60000, 10000),
                     limits = c(0, 60000)) +
  
  # Labels
  labs(x = 'Recebeu assistência médica?',
       y = 'Número de Suicídios Registrados',
       title = 'Número de Suicídios Registrados que Receberam Assistência Médica no Brasil',
       caption = design$caption) +
  
  # Theme
  design$project_theme +
  
  theme(panel.grid.major.x = element_blank())

suicides_medical_assistance

# Visualizing demographis: education ====
suicides_by_education <- 
  
  df %>% 
  # Cleaning
  
  filter(ESC != 'NA') %>%
  
  group_by(ESC) %>% 
  
  summarize(n = n()) %>%
  
  #arrange(desc(n), .by_group = T) %>%
  
  # Plot
  ggplot(aes(x = ESC, y = n, fill = design$fill_color)) +
  
  # Geom
  geom_tile(aes(y = n/2, 
                height = n,
                width = 0.9), fill = design$fill_color) +
  
  # Y-axis: Limits and ticks
  scale_y_continuous(breaks = seq(0, 60000, 10000),
                     limits = c(0, 60000)) +
  
  # Labels
  labs(x = 'Escolaridade',
       y = '',
       title = 'Número de Suicídios Registrados por Escolaridade no Brasil',
       caption = design$caption) +
  
  # Flipping coordinates
  coord_flip() +
  
  # Theme
  design$project_theme

suicides_by_education

# Visualizing demographis: mother's education ====
suicides_by_mothers_education <- 
  
  df %>% 
  # Cleaning
  
  filter(ESCMAE != 'NA') %>%
  
  group_by(ESCMAE) %>% 
  
  summarize(n = n()) %>%
  
  arrange(desc(n), .by_group = T) %>%
  
  # Plot
  ggplot(aes(x = ESCMAE, y = n, fill = design$fill_color)) +
  
  # Geom
  geom_tile(aes(y = n/2, 
                height = n,
                width = 0.9), fill = design$fill_color) +
  
  # Y-axis: Limits and ticks
  scale_y_continuous(breaks = seq(0, 60000, 10000),
                     limits = c(0, 60000)) +
  
  # Labels
  labs(x = 'Escolaridade da Mãe',
       y = '',
       title = 'Número de Suicídios Registrados por Escolaridade da Mãe no Brasil',
       caption = design$caption) +
  
  # Flipping coordinates
  coord_flip() +
  
  # Theme
  design$project_theme

suicides_by_mothers_education

# Visualizing demographis: marital status ====
suicides_by_marital_status <- 
  
  df %>% 
  # Cleaning
  
  filter(ESTCIV != 'NA') %>%
  
  group_by(ESTCIV) %>% 
  
  summarize(n = n()) %>%
  
  arrange(desc(n), .by_group = T) %>%
  
  # Plot
  ggplot(aes(x = fct_reorder(ESTCIV, n), y = n, fill = design$fill_color)) +
  
  # Geom
  geom_tile(aes(y = n/2, 
                height = n,
                width = 0.9), fill = design$fill_color) +
  
  # Y-axis: Limits and ticks
  scale_y_continuous(breaks = seq(0, 60000, 10000),
                     limits = c(0, 60000)) +
  
  # Labels
  labs(x = 'Escolaridade da Mãe',
       y = '',
       title = 'Número de Suicídios Registrados por Estado Civil',
       caption = design$caption) +
  
  # Flipping coordinates
  coord_flip() +
  
  # Theme
  design$project_theme

suicides_by_marital_status

# Visualizing demographis: ocupation - Pt. 1 ====
suicides_by_ocup_img <-
  
  df %>%
  
  # Cleaning
  filter(OCUP != 'NA') %>% 
  
  group_by(OCUP) %>% 
  
  summarize(n = n()) %>%
  
  arrange(desc(n), .by_group = T) %>% 
  
  top_n(n = 20) %>%
  
  # Plot
  
  ggplot(aes(x = fct_reorder(as.factor(OCUP), n), y = n, group = OCUP)) +
  
  # Geom
  geom_tile(aes(y = n/2, 
                height = n,
                width = 0.9), fill = design$fill_color) +
  
  # Y-axis
  scale_y_continuous(breaks = seq(0, 8000, 2000),
                     limits = c(0, 8000)) +
  
  # Labels
  labs(x = '',
       y = '',
       title = 'Número de Suicídos Registrados por Ocupação/Profissão',
       caption = design$caption) +
  
  
  # Flipping coordinates
  coord_flip() +
  
  # Theme
  design$project_theme

suicides_by_ocup_img

# Visualizing demographis: ocupation - Pt. 2 ====
suicides_by_ocup <-
  
  df %>%
  
  # Cleaning
  filter(OCUP != 'NA') %>% 
  
  mutate(ano = as.factor(ano)) %>% 
  
  group_by(ano, OCUP) %>% 
  
  summarize(n = n()) %>%
  
  arrange(desc(n), .by_group = T) %>% 
  
  top_n(n = 10) %>% #TODO: fix this
  
  mutate(order = 1:n()) %>% 
  
  # Plot
  
  ggplot(aes(x = fct_reorder(as.factor(order), n), y = n, group = OCUP)) +
  
  # Geom
  geom_tile(aes(y = n/2, 
                height = n,
                width = 0.9), fill = design$fill_color) +
  
  # X-axis: Label
  geom_text(aes(y = 1400, label = OCUP), vjust = -1) +
  
  coord_cartesian(clip = "off", expand = T) +
  
  # Y-axis
  scale_y_continuous(breaks = seq(0, 1750, 250),
                     limits = c(0, 1750)) +
  
  # Labels
  labs(x = '',
       y = 'Número de Mortes',
       title = 'Número de Suicídos Registrados por Ocupação/Profissão',
       subtitle = 'Ano: {closest_state}',
       caption = design$caption) +
  
  
  # Flipping coordinates
  coord_flip() +
  
  # Theme
  design$project_theme +
  
  # Transition
  transition_states(ano)

p <- animate(suicides_by_ocup, 
             # end_pause indicates the amount to be paused after ending
             duration = 15,
             start_pause = 3,
             end_pause = 3,
             renderer = gifski_renderer(),
             width = 1000,
             height = 600)

anim_save("figures\\suicides_by_ocup.gif", p)

# Visualizing demographis: surgery (yes/no) ====
suicides_surgery <- 
  
  df %>% 
  # Cleaning
  
  filter(CIRURGIA != 'NA') %>%
  
  group_by(CIRURGIA) %>% 
  
  summarize(n = n()) %>%
  
  arrange(desc(n), .by_group = T) %>%
  
  # Plot
  ggplot(aes(x = reorder(CIRURGIA, n), y = n, fill = design$fill_color)) +
  
  # Geom
  geom_tile(aes(y = n/2, 
                height = n,
                width = 0.9), fill = design$fill_color) +
  
  # Y-axis: Limits and ticks
  scale_y_continuous(breaks = seq(0, 10000, 2500),
                     limits = c(0, 10000)) +
  
  # Labels
  labs(x = 'Recebeu cirurgia?',
       y = '',
       title = 'Número de Suicídios Registrados que Passaram por Cirurgia no Brasil',
       caption = design$caption) +
  
  # Theme
  design$project_theme +
  
  theme(panel.grid.major.x = element_blank())

suicides_surgery

# Visualizing demographis: where did it occur? - Pt. 1 ====
estados <- read_country(year = 2019)

by_state <-
  df %>%
  group_by(estado) %>%
  dplyr::summarise(n = n())

estados <- estados %>% 
  left_join(by_state, by = c('abbrev_state' = 'estado'))

suicide_by_states <- estados %>%
  
  ggplot() +
  
  geom_sf(aes(fill = n), size = .15) +
  
  scale_fill_gradient2(low = "white", high = design$fill_color,
                       name = "",
                       limits = c(0, 25000))  +
  
  labs(x = '',
       y = '',
       title = "Número de Suicídos Registrados por Estado Brasileiro",
       caption  = design$caption) +
  
  theme(plot.caption = element_text(hjust = 0, face= "italic"),
        plot.title.position = "plot", 
        plot.caption.position =  "plot") +
  
  geom_sf_label(aes(label = abbrev_state),
                label.padding = unit(0.5, "mm"),
                size = 3) +
  
  design$project_map_theme

suicide_by_states

# Visualizing demographis: where did it occur? - Pt. 2 ====
estados <- read_country(year = 2019)

by_year_state <-
  df %>%
  group_by(ano, estado) %>%
  dplyr::summarise(n = n())

estados <- estados %>% 
  left_join(by_year_state, by = c('abbrev_state' = 'estado'))

suicide_by_states_by_year <- estados %>%
  
  mutate(ano = as.factor(ano)) %>% 
  
  ggplot() +
  
  geom_sf(aes(fill = n), size = .15) +
  
  scale_fill_gradient2(low = "white", high = design$fill_color,
                       name = "",
                       limits = c(0, 2500))  +
  
  labs(x = '',
       y = '',
       title = "Número de Suicídos Registrados por Estado Brasileiro",
       caption  = design$caption) +

  theme(plot.caption = element_text(hjust = 0, face= "italic"),
        plot.title.position = "plot", 
        plot.caption.position =  "plot") +
  
  design$project_map_theme +
  
  # Transition
  facet_wrap(~ ano, nrow = 2, ncol = 5)

suicide_by_states_by_year

# Visualizing suicides through time: line plot - Pt. 1 ====
format <- '%m-%Y'
ym_total$index <- parse_date_time(ym_total$index, format)

suicides_across_years_img <- 
  ym_total %>% 
  
  # Plot
  ggplot(aes(x = as.Date(index), y = n)) +
  
  # Geom
  geom_line(size = 1, colour = design$fill_color) +
  
  # X-axis: Limits and ticks
  scale_x_date(date_breaks = 'years', date_labels = '%Y') +
  
  # Y-axis: Limits and ticks
  scale_y_continuous(breaks = seq(0, 1500, by = 250),
                     limits = c(0, 1500)) +
  
  # Labels
  labs(x= "",
       y = "",
       title = "Número de Suicídios Registrados no Brasil em 10 Anos",
       caption = design$caption) +
  
  # Removing legend title
  design$project_theme

suicides_across_years_img

# Visualizing suicides through time: line plot - Pt. 2 ====
suicides_across_years <- 
  ym_total %>% 
  
  # Plot
  ggplot(aes(x = as.Date(index), y = n)) +
  
  # Geom
  geom_line(size = 1, colour = design$fill_color) +
  
  # X-axis: Limits and ticks
  scale_x_date(date_breaks = 'years', date_labels = '%Y') +
  
  # Y-axis: Limits and ticks
  scale_y_continuous(breaks = seq(0, 1500, by = 250),
                     limits = c(0, 1500)) +
  
  # Labels
  labs(x= "",
       y = "",
       title = "Número de Suicídios Registrados no Brasil em 10 Anos",
       caption = design$caption) +
  
  # Theme
  design$project_theme +
  
  # Transition
  transition_reveal(as.Date(index)) +
  enter_fade() +
  exit_fade()
  
p <- animate(suicides_across_years, 
             duration = 20,
             start_pause = 3,
             end_pause = 3,
             renderer = gifski_renderer(),
             width = 1000,
             height = 750)

anim_save("figures\\suicides_across_years.gif", p)

# Visualizing number of suicides through time (year): line plot ====
year_variation$index <- parse_date_time(c(2011:2019), '%Y')

# Hyperparameters
global_mean <- mean(year_variation$year_variation)
x_start <- as.Date("2013-03-01")
y_start <- 0.0465
x_end <- as.Date("2014-03-01")
y_end <- global_mean

suicide_var_by_year <-
  year_variation %>% 
  
  # Plot
  ggplot(aes(x = as.Date(index), y = year_variation, # to make 'percent' work
             fill = year_variation)) +
  
  # Geom
  geom_col() +
  
  # X-axis: Limits and ticks
  scale_x_date(date_breaks = 'years', date_labels = '%Y') +
  
  # Y-axis: Limits and ticks
  scale_y_continuous(breaks = seq(0, 0.1, 0.025), limits = c(0, 0.1),
                     labels = percent) +
  
  # X-axis: Colors
  scale_fill_gradient2(low = 'white', high = design$fill_color) +
  
  # Labels
  labs(x = '',
       y = 'Variação em Relação ao Ano Anterior',
       title = 'Variação Percentual no Número de Casos Registrados de Suicídio no Brasil por Ano',
       caption = design$caption) +
  
  # Adding line
  geom_hline(yintercept = global_mean, color = "#181818", linetype = 3) +
  
  # Annotations: Pt. 1
  annotate(geom = "text", x = x_start, y = y_start, 
    label = "A média\nde variação\npor ano",
    vjust = 1, size = 3, color = "#181818") +
  
  # Annotations: Pt. 2
  annotate(geom = "curve", x = x_start, y = y_start - 0.002, xend = x_end, yend = y_end,
    arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
    color = "grey40") +
  
  # Annotations: Pt 3
  annotate(geom = "text", x = as.Date("2017-12-01"), y = 0.075, size = 5,
           label = "Em 2017, houve um aumento de 9.31% de\ncasos de suicídio em comparação a 2016.") +
  
  # Theme
  design$project_theme +
  
  coord_flip()
  
# Visualizing number of suicides through time (month): bar plot ====
# Criando a coluna nome_mes
month_total$nome_mes <- c('Janeiro', 'Fevereiro', 'Março', 
                          'Abril', 'Maio', 'Junho',
                          'Julho', 'Agosto', 'Setembro',
                          'Outubro', 'Novembro', 'Dezembro')
month_total$nome_mes %<>% as.factor

monthly_suicide <-
  month_total %>%
  
  mutate(nome_mes = fct_relevel(nome_mes,
                                'Dezembro', 'Novembro', 'Outubro',
                                'Setembro', 'Agosto', 'Julho', 
                                'Junho', 'Maio', 'Abril',
                                'Março', 'Fevereiro', 'Janeiro')) %>% 
  # Plot
  ggplot(aes(x = nome_mes, y = n)) +
  
  # Geom
  geom_bar(stat = "identity", fill = design$fill_color, width = .8) +
  
  # Labels
  labs(x = '',
       y = '',
       title = 'Número Total de Suicídios Registrados por Mês',
       caption = design$caption) +
  
  # Flipping coordinates
  coord_flip() +
  
  # Theme
  design$project_theme

monthly_suicide

# Visualizing variation (%) in suicides through time (month): bar plot ====
month_variation$index <- c('Fevereiro', 'Março', 'Abril',
                           'Maio', 'Junho', 'Julho',
                           'Agosto', 'Setembro', 'Outubro',
                           'Novembro', 'Dezembro')
month_variation$index <- factor(month_variation$index,
                                levels = c('Fevereiro', 'Março', 'Abril',
                                           'Maio', 'Junho', 'Julho',
                                           'Agosto', 'Setembro', 'Outubro',
                                           'Novembro', 'Dezembro'))

# Hyperparameters
global_mean <- mean(month_variation$month_variation)
x_start <- as.Date("2013-03-01")
y_start <- 0.0465
x_end <- as.Date("2014-03-01")
y_end <- global_mean

suicide_var_by_month <-
  month_variation %>% 
  
  mutate(index = fct_relevel(index,
                             'Dezembro', 'Novembro',
                             'Outubro', 'Setembro', 'Agosto',
                             'Julho', 'Junho', 'Maio',
                             'Abril', 'Março', 'Fevereiro')) %>% 
  
  # Plot
  ggplot(aes(x = index, y = month_variation, # to make 'percent' work
             color = 'black', fill = month_variation)) +
  
  # Geom
  geom_col() +
  
  # Y-axis: Limits and ticks
  scale_y_continuous(breaks = seq(-0.1, 0.1, 0.025), limits = c(-0.1, 0.1),
                     labels = percent) +
  
  # X-axis: Colors
  scale_fill_gradient2(low = 'white', high = design$fill_color) +
  
  # Labels
  labs(x = '',
       y = 'Variação em Relação ao Mês Anterior',
       title = 'Variação Percentual Total no Número de Casos Registrados de Suicídio no Brasil por Mês',
       caption = design$caption) +
  
  # Adding line
  geom_hline(yintercept = global_mean, color = "#181818", linetype = 3) +
  
  # Annotations: Pt. 1
  #annotate(geom = "text", x = x_start, y = y_start, 
  #         label = "A média\nde variação\npor ano",
  #         vjust = 1, size = 3, color = "#181818") +
  
  # Annotations: Pt. 2
  #annotate(geom = "curve", x = x_start, y = y_start - 0.002, xend = x_end, yend = y_end,
  #         arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
  #         color = "grey40") +
  
  # Annotations: Pt 3
  #annotate(geom = "text", x = as.Date("2017-12-01"), y = 0.075, size = 5,
  #         label = "Em 2017, houve um aumento de 9.31% de\ncasos de suicídio em comparação a 2016.") +
  
  # Theme
  design$project_theme +
  
  coord_flip(clip = 'off')

suicide_var_by_month

# Visualizing variation (%) in suicides through time (year/month): bar plot ====
# Criando coluna do mês
ym_variation[120, ] <- NA # criando última linha
ym_variation$index <- rep(c('Fevereiro', 'Março', 'Abril',
                           'Maio', 'Junho', 'Julho',
                           'Agosto', 'Setembro', 'Outubro',
                           'Novembro', 'Dezembro', 'Janeiro'), 10)

ym_variation <- ym_variation[1:119, ] # excluindo última linha

ym_variation$index <- factor(ym_variation$index,
                             levels = c('Janeiro', 'Fevereiro', 'Março', 
                                        'Abril', 'Maio', 'Junho', 'Julho',
                                        'Agosto', 'Setembro', 'Outubro',
                                        'Novembro', 'Dezembro'))

# Criando coluna do ano
ym_variation$ano <- year(ym_total$index)[2:120]
ym_variation$ano %<>% as.factor

# Hyperparameters
global_mean <- mean(ym_variation$ym_variation)
x_start <- as.Date("2013-03-01")
y_start <- 0.0465
x_end <- as.Date("2014-03-01")
y_end <- global_mean

suicide_var_by_year_by_month <-
  ym_variation %>% 
  
  mutate(index = fct_relevel(index,
                             'Dezembro', 'Novembro',
                             'Outubro', 'Setembro', 'Agosto',
                             'Julho', 'Junho', 'Maio',
                             'Abril', 'Março', 'Fevereiro', 'Janeiro')) %>% 
  
  # Plot
  ggplot(aes(x = index, y = ym_variation, # to make 'percent' work
             color = 'black', fill = ym_variation)) +
  
  # Geom
  geom_col() +
  
  # Y-axis: Limits and ticks
  scale_y_continuous(breaks = seq(-0.25, 0.25, 0.05), limits = c(-0.25, 0.25),
                     labels = percent) +
  
  # X-axis: Colors
  scale_fill_gradient2(low = 'white', high = design$fill_color) +
  
  # Labels
  labs(x = '',
       y = 'Variação em Relação ao Mês Anterior',
       title = 'Variação Percentual no Número de Casos Registrados de Suicídio no Brasil por Mês e Ano',
       subtitle = 'Ano: {closest_state}',
       caption = design$caption) +
  
  # Adding line
  geom_hline(yintercept = global_mean, color = "#181818", linetype = 3) +
  
  # Annotations: Pt. 1
  #annotate(geom = "text", x = x_start, y = y_start, 
  #         label = "A média\nde variação\npor ano",
  #         vjust = 1, size = 3, color = "#181818") +
  
  # Annotations: Pt. 2
  #annotate(geom = "curve", x = x_start, y = y_start - 0.002, xend = x_end, yend = y_end,
  #         arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
  #         color = "grey40") +
  
# Annotations: Pt 3
#annotate(geom = "text", x = as.Date("2017-12-01"), y = 0.075, size = 5,
#         label = "Em 2017, houve um aumento de 9.31% de\ncasos de suicídio em comparação a 2016.") +

  # Theme
  design$project_theme +
  
  coord_flip() +
  
  transition_states(ano)

p <- animate(suicide_var_by_year_by_month, 
             duration = 25,
             start_pause = 2,
             end_pause = 2,
             renderer = gifski_renderer(),
             width = 1000,
             height = 750)

anim_save("figures\\suicide_var_by_year_by_month.gif", p)

# Visualizing student suicides ====
student_suicides <-
  df %>%
  
  # Cleaning
  filter(OCUP != 'NA') %>% 
  
  mutate(ano = as.factor(ano)) %>% 
  
  group_by(ano) %>% 
  
  count() %>%
  
  # Plot
  ggplot(aes(x = ano, y = n)) +
  
  # Geom
  geom_bar(stat = "identity", fill = design$fill_color, width = .8) +
  
  # Labels
  labs(x = '',
       y = '',
       title = 'Número de Suicídios Registrados de Estudantes ao Longo dos Anos',
       caption = design$caption) +
  
  # Flipping coordinates
  coord_flip() +
  
  # Theme
  design$project_theme

student_suicides

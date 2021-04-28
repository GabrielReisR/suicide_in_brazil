# Initializing ====
# Loading libraries
if (!require("dplyr"))
  install.packages("dplyr")
if (!require("forcats"))
  install.packages("forcats")
if (!require("gganimate"))
  install.packages("gganimate")
if (!require("ggplot2"))
  install.packages("ggplot2")
if (!require("ggthemes"))
  install.packages("ggthemes")
if(!require("gifski"))
  install.packages("gifski")
if(!require("hrbrthemes"))
  install.packages("hrbrthemes")
if(!require("lubridate"))
  install.packages("lubridate")
if(!require("magrittr"))
  install.packages("magrittr")
if(!require("plotly"))
  install.packages("plotly")
if(!require("psych"))
  install.packages("psych")
if(!require("scales"))
  install.packages("scales")
if(!require("stringr"))
  install.packages("stringr")
if(!require("viridis"))
  install.packages("viridis")

# Getting data
df <- read.csv('data\\df.csv', encoding = 'UTF-8')[-1]

# Setting theme
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

# Initial cleaning ====
# Making missings known
df[df == 'NaN'] <- NA

# Arranging dates
format <- "%d%m%Y"
df$DTOBITO <- parse_date_time(df$DTOBITO, format)
df$DTNASC <- parse_date_time(df$DTNASC, format)

# Creating 'month' column
df$mes <- month(df$DTOBITO)

# Sex as factor
df$SEXO %<>% as.factor

# Race/color as factor
df$RACACOR %<>% as.factor

# Medical assistance as factor
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
df$CIRURGIA %<>% as.factor

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

# plot.ts(year_variation)

# Visualizing demographics: sex ====
sex_by_year <- 
  
  df %>% 
  # Cleaning
  mutate(ano = as.factor(ano)) %>% 
  
  filter(SEXO != 'NA') %>%
  
  # Plot
  ggplot(aes(x = SEXO, fill = '#f68060')) +
  
  # Geom
  geom_bar() +
  
  # Y-axis: Limits and ticks
  scale_y_continuous(breaks = c(0, 2500, 5000, 7500, 10000),
                     limits = c(0, 11000)) +
  
  # Labels
  labs(x = 'Sexo',
       y = 'Número de Suicídios Registrados',
       title = 'Número de Suicídios por Sexo Registrados no Brasil',
       subtitle = 'Ano: {closest_state}') +
  
  # Theme
  project_theme +
  
  # Transition
  transition_states(ano)

p <- animate(sex_by_year, 
             # end_pause indicates the amount to be paused after ending
             duration = 30,
             start_pause = 3,
             end_pause = 3,
             renderer = gifski_renderer(),
             width = 600,
             height = 600)

anim_save("figures\\suicides_by_sex_by_year.gif", p)

# Visualizing demographis: race/color ====
# Visualizing demographis: medical assistance ====
# Visualizing demographis: education ====
# Visualizing demographis: mother's education ====
# Visualizing demographis: marital status ====
# Visualizing demographis: ocupation ====
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
                width = 0.9), fill = "#f68060") +
  
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
       subtitle = 'Ano: {closest_state}') +
  
  
  # Flipping coordinates
  coord_flip() +
  
  # Theme
  project_theme +
  
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
# Visualizing demographis: where did it occur? ====

# Visualizing suicides through time: line plot ====
format <- '%m-%Y'
ym_total$index <- parse_date_time(ym_total$index, format)

suicides_across_years <- 
  ym_total %>% 
  
  # Plot
  ggplot(aes(x = as.Date(index), y = n)) +
  
  # Geom
  geom_line(size = 1, colour = '#f68060') +
  
  # X-axis: Limits and ticks
  scale_x_date(date_breaks = 'years', date_labels = '%Y') +
  
  # Y-axis: Limits and ticks
  scale_y_continuous(breaks = seq(0, 1500, by = 250),
                     limits = c(0, 1500)) +
  
  # Labels
  labs(x= "",
       y = "",
       title = "Número de Suicídios Registrados no Brasil em 10 Anos",
       subtitle = "Dados entre os anos de 2010 e 2019") +
  
  # Theme
  theme_bw() +
  
  # Removing legend title
  theme(legend.position = "none") + 
  
  # Removing legend title
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank()) +
  
  # Transition
  transition_reveal(as.Date(index)) +
  enter_fade() +
  exit_fade()
  
p <- animate(suicides_across_years, 
             # end_pause indicates the amount to be paused after ending
             duration = 30,
             start_pause = 3,
             end_pause = 3,
             renderer = gifski_renderer(),
             width = 1000,
             height = 750)

anim_save("suicides_across_years.gif", p)

# Visualizing variation (%) in suicides through time: bar plot ====
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
  ggplot(aes(x = as.Date(index), y = year_variation # to make 'percent' work
             , fill = year_variation)) +
  
  # Geom
  geom_col() +
  
  # X-axis: Limits and ticks
  scale_x_date(date_breaks = 'years', date_labels = '%Y') +
  
  # Y-axis: Limits and ticks
  scale_y_continuous(breaks = seq(0, 0.1, 0.025), limits = c(0, 0.1),
                     labels = percent) +
  
  # X-axis: Colors
  scale_fill_gradient2(low = 'white', high = 'red') +
  
  # Labels
  labs(x = '',
       y = 'Variação em Relação ao Ano Anterior',
       title = 'Variação Percentual no Número de Casos Registrados de Suicídio no Brasil por Ano') +
  
  # Adding line
  geom_hline(yintercept = global_mean, color = "grey40", linetype = 3) +
  
  # Annotations: Pt. 1
  annotate(geom = "text", x = x_start, y = y_start, 
    label = "A média\nde variação\npor ano",
    vjust = 1, size = 3, color = "grey40") +
  
  # Annotations: Pt. 2
  annotate(geom = "curve", x = x_start, y = y_start - 0.002, xend = x_end, yend = y_end,
    arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
    color = "grey40") +
  
  # Annotations: Pt 3
  annotate(geom = "text", x = as.Date("2017-12-01"), y = 0.075, size = 5,
           label = "Em 2017, houve um aumento de 9.31% de\ncasos de suicídio em comparação a 2016.") +
  
  # Theme
  project_theme +
  
  coord_flip()
  
suicide_var_by_year

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
  geom_bar(stat="identity", fill="#f68060", width = .8) +
  
  # Labels
  labs(x = '',
       y = 'Número de Mortes',
       title = 'Número de Suicídios de Estudantes ao Longo dos Anos') +
  
  # Flipping coordinates
  coord_flip() +
  
  # Theme
  project_theme

student_suicides
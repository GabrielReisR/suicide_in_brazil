# Initializing ====
# Loading libraries
library(dplyr)
library(gganimate)
library(ggplot2)
library(ggthemes)
library(gifski)
library(hrbrthemes)
library(lubridate)
library(magrittr)
library(plotly)
library(psych)
library(viridis)

df <- read.csv('df.csv', encoding = 'UTF-8')[-1]

# Initial cleaning ====
# Making missings known
df[df == 'NaN'] <- NA

# Arranging dates
format <- "%d%m%Y"
df$DTOBITO <- parse_date_time(df$DTOBITO, format)
df$DTNASC <- parse_date_time(df$DTNASC, format)

# Creating 'month' column
df$mes <- month(df$DTOBITO)

# Creating only number of deaths dataframes by month/year ====
year_total <- df %>% group_by(ano) %>% count()

month_total <- df %>% group_by(mes) %>% count()

ym_total <- df %>% 
  group_by(ano, mes) %>% 
  count() %>% 
  tidyr::unite(mes, ano, col = 'index', sep = '-')

ym_total

# Creating variations ====
#' *year_variation*
year_variation <- as.data.frame(year_total)
rownames(year_variation) <- year_variation$ano
year_variation %<>% select(-ano)
year_variation

year_variation <- as.ts(year_variation, start = c(2010), frequency = 1)

year_variation <- year_variation/stats::lag(year_variation, -1) - 1

#' *month variation*
month_variation <- as.data.frame(month_total)
rownames(month_variation) <- month_variation$mes
month_variation %<>% select(-mes)
month_variation

month_variation <- as.ts(month_variation, start = c(1), frequency = 1)

month_variation <- month_variation/stats::lag(month_variation, -1) - 1

month_variation*100

#' *ym_variation*
ym_variation <- as.data.frame(ym_total)
rownames(ym_variation) <- ym_variation$index
ym_variation %<>% select(-index)
rownames(ym_variation) <- NULL

ym_variation <- as.ts(ym_variation, start = c(2010, 1), frequency = 12)

ym_variation <- ym_variation/stats::lag(ym_variation, -1) - 1

head(ym_variation, n = 20)

# plot.ts(year_variation)

# Visualizing suicides through time: line plot ====
format <- '%m-%Y'
ym_total$index <- parse_date_time(ym_total$index, format)

suicides_across_years <- 
  ym_total %>% 
  
  # Plot
  ggplot(aes(x = as.Date(index), y = n)) +
  
  # Geom
  geom_line(size = 1) +
  
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


# Visualizing variation in suicides through time: bar plot ====
yearly_variation <- as.data.frame(year_variation * 100)
yearly_variation$index <- parse_date_time(c(2011:2019), '%Y')

suicide_var_by_year <-
  yearly_variation %>% 
  
  # Plot
  ggplot(aes(x = as.Date(index), y = year_variation, fill = index)) +
  
  # Geom
  geom_col() +
  
  # X-axis: Limits and ticks
  scale_x_date(date_breaks = 'years', date_labels = '%Y') +
  
  # Y-axis: Limits and ticks
  scale_y_continuous(breaks = seq(0, 10, 2.5), limits = c(0, 10)) +
  
  # X-axis: Colors
  scale_fill_gradient2(low = 'darkgreen', mid = 'snow3', high = 'red') +
  
  # Labels
  labs(x = '',
       y = 'Variação em Relação ao Ano Anterior (%)',
       title = 'Variação Percentual no Número de Casos Registrados de Suicídio no Brasil por Ano') +
  
  # Annotations
  annotate(geom = "text", x = as.Date("2014-06-01"), y = 9,
           label = "Em 2017, houve um aumento de 9.31% de\ncasos de suicídio em comparação a 2016.") +
  
  # Theme
  theme_bw() +
  
  # Removing legend title
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank())
  
suicide_var_by_year

# Visualizing sex: bar plot ====
df$SEXO %<>% as.factor

sex_by_year <- 
  
  df %>%
  
  # Cleaning
  mutate(ano = as.factor(ano)) %>% 

  filter(SEXO != 'NA') %>%
  
  # Plot
  ggplot(aes(x = SEXO, fill = SEXO)) +
  
  # Geom
  geom_bar() +
  
  # X-axis: Colors
  scale_fill_brewer(palette = "Dark2") +
  
  # Y-axis: Limits and ticks
  scale_y_continuous(breaks = c(0, 2500, 5000, 7500, 10000),
                     limits = c(0, 11000)) +
  
  # Labels
  labs(x = 'Sexo',
       y = 'Número de Suicídios Registrados',
       title = 'Número de Suicídios por Sexo Registrados no Brasil',
       subtitle = 'Ano: {closest_state}') +
  
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
  transition_states(ano)

p <- animate(sex_by_year, 
             # end_pause indicates the amount to be paused after ending
             duration = 30,
             start_pause = 3,
             end_pause = 3,
             renderer = gifski_renderer(),
             width = 600,
             height = 600)

anim_save("suicides_by_sex_by_year.gif", p)


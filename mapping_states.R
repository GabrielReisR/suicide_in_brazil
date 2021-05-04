if(!require("cowplot"))
  install.packages("cowplot")
if(!require("dplyr"))
  install.packages("dplyr")
if(!require("geobr"))
  install.packages("geobr")
if(!require("ggplot2"))
  install.packages("ggplot2")
if(!require("RColorBrewer"))
  install.packages("RColorBrewer")
if(!require("sf"))
  install.packages("sf")
if(!require("udunits2"))
  install.packages("udunits2")
if(!require("units"))
  install.packages("units")
if(!require('maps'))
  install.packages('maps')

dados <- structure(
  list(X = 1:27, 
       uf = c("Acre", "Alagoas", "Amapá", 
              "Amazônas", "Bahia", "Ceará", "Distrito Federal",
              "Espírito Santo", "Goiás", "Maranhão", "Mato Grosso do Sul",
              "Mato Grosso", "Minas Gerais", "Paraíba", "Paraná", "Pará",
              "Pernambuco", "Piauí", "Rio de Janeiro", "Rio Grande do Norte",
              "Rio Grande do Sul", "Rondônia", "Roraima", 
              "Santa Catarina", "São Paulo", "Sergipe", "Tocantins"), 
       GDP_Per_Capita = c(17.636, 16.375, 20.247, 24.542, 19.324, 17.178, 
                          85.661, 34.493, 28.272, 13.955, 38.925, 39.931, 
                          29.223, 16.107, 38.772, 18.952, 19.623, 15.432, 
                          44.222,19.242 ,40.362 , 25.554, 23.188, 42.149, 
                          48.542, 18.442, 22.933)),
  class = "data.frame", row.names = c(NA, -27L))

dados

states <- read_country(year = 2019)
states$name_state <- tolower(states$name_state)
dados$uf <- tolower(dados$uf)

states <- dplyr::left_join(
  states, dados,
  by = c("name_state" = "uf"))

states

L = min(states$GDP_Per_Capita)
S = max(states$GDP_Per_Capita)


states$GDP_Per_Capita

p <- states %>%
  
  ggplot() +
  
  geom_sf(aes(fill = GDP_Per_Capita ), size = .15) +
  
  scale_fill_gradient2(low = "white", high = "blue",
                      name = "GDP Per Capita (R$)",
                      limits = c(0, 50.000)) +
  
  xlab("") +  ylab("") +
  
  geom_sf_label(aes(label = abbrev_state),
                label.padding = unit(0.5, "mm"),
                size = 3)

p <- p +
  
  labs(title = "GDP per Capita by State",
       caption  = "Authors: Gerson Júnior e Henrique Martins.") +
  
  theme(plot.caption = element_text(hjust = 0, face= "italic"),
        plot.title.position = "plot", 
        plot.caption.position =  "plot") 

p <- p +
  
  theme(legend.position = "bottom") +
  
  theme(legend.title = element_text(size = 10),
        legend.text=element_text(size=10)) +
  
  theme_bw()

plot(p)



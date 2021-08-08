library(geobr)
library(sf)
atreyu_pal<-c("#FE2C01","#AD4625","#AE6146","#C8582B","#F06B20","#02B1DC","#02729B","#2992B9","#2986AD","#2C5C7F")
baroness_pal<-c("#D17775","#9E684D","#C76938","#CF5E1B","#C0703F","#EE9D35","#B37314","#FDCC1B","#98213B","#9E2326")
baroness_pal_seq<-c("#9E2326", "#98213B", "#D17775", "#EE9D35", "#FDCC1B")
atreyu_pal_seq<-c("#FE2C01", "#F06B20", "#C8582B", "#02729B", "#02B1DC")
atreyu_baroness_pal<-c("#FDCC1B", "#EE9D35", "#F06B20", "#FE2C01", "#D17775", "#98213B")
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())
black_white<-theme(axis.ticks = element_blank(), 
                   panel.grid = element_blank(),
                   axis.text = element_blank(),
                   axis.title = element_blank(),
                   plot.background = element_rect(fill = "black"), 
                   plot.subtitle = element_text(colour = "White"), 
                   plot.caption = element_text(colour = "white"))

all_muni <- read_municipality(year= 2010, showProgress = FALSE)
all_states <- read_state(year = 2019)
country<-read_country(year = 2010, showProgress = FALSE)
start_pts<-Sys.time()
points <- st_sample(x = country, size = 500000, type = "random")
stop_pts<-Sys.time()
time<-stop_pts - start_pts
all_muni_plot<- ggplot() +
  geom_sf(data=country, 
          color = baroness_pal_seq[1], 
          fill="White", 
          size=.15, 
          show.legend = FALSE, 
          alpha = 0.5) +
  geom_sf(data=points[1], 
          color = atreyu_pal[1], 
          # fill=baroness_pal_seq[5], 
          size=.0001, 
          show.legend = FALSE) +
  theme_minimal() +
  no_axis + 
  labs(subtitle="Mapa do Brasil Vazio, só um ponto", 
       caption = "Visualização: Rafael Lopes @rafalpx",
       size=8)
all_muni_plot
all_muni_bw<- ggplot() +
  geom_sf(data=country, 
          color = "white", 
          fill="black", 
          size=.15, 
          show.legend = FALSE, 
          alpha = 0.5) +
  geom_sf(data=points[1], 
          color = "white", 
          size=.01, 
          show.legend = FALSE) +
  theme_minimal() +
  black_white+
  labs(subtitle="Mapa do Brasil Vazio, só um ponto", 
       caption = "Visualização: Rafael Lopes @rafalpx", 
       colour = "white",
       size=8)
all_muni_bw
cem_mortos<-all_muni_plot + 
  geom_sf(data=points[1:100], 
          color = atreyu_pal[1], 
          # fill=baroness_pal_seq[5], 
          size=.0001, 
          show.legend = FALSE) +
  labs(subtitle="Mapa do Brasil com 100 pontos aleatoriamente distribuídos", 
       caption = "Visualização: Rafael Lopes @rafalpx",
       size=8)
cem_mortos
cem_mortos_bw<-all_muni_bw + 
  geom_sf(data=points[1:100], 
          color = "white",
          size=.01, 
          show.legend = FALSE) +
  labs(subtitle="Mapa do Brasil com 100 pontos aleatoriamente distribuídos", 
       caption = "Visualização: Rafael Lopes @rafalpx",
       size=8)
cem_mortos_
mil_mortos<-all_muni_plot + 
  geom_sf(data=points[1:1000], 
          color = atreyu_pal[1], 
          # fill=baroness_pal_seq[5], 
          size=.0001, 
          show.legend = FALSE) +
  labs(subtitle="Mapa do Brasil com 1000 pontos aleatoriamente distribuídos", 
       caption = "Visualização: Rafael Lopes @rafalpx",
       size=8)
mil_mortos
mil_mortos_bw<-all_muni_bw + 
  geom_sf(data=points[1:1000], 
          color = "white",
          size=.01, 
          show.legend = FALSE) +
  labs(subtitle="Mapa do Brasil com 1000 pontos aleatoriamente distribuídos", 
       caption = "Visualização: Rafael Lopes @rafalpx",
       size=8)
mil_mortos_bw
dez_mil_mortos<-all_muni_plot + 
  geom_sf(data=points[1:10000], 
          color = atreyu_pal[1], 
          # fill=baroness_pal_seq[5], 
          size=.0001, 
          show.legend = FALSE) +
  labs(subtitle="Mapa do Brasil com 10000 pontos aleatoriamente distribuídos", 
       caption = "Visualização: Rafael Lopes @rafalpx",
       size=8)
dez_mil_mortos
dez_mil_mortos_bw<-all_muni_bw + 
  geom_sf(data=points[1:10000], 
          color = "white",
          size=.01, 
          show.legend = FALSE) +
  labs(subtitle="Mapa do Brasil com 10000 pontos aleatoriamente distribuídos", 
       caption = "Visualização: Rafael Lopes @rafalpx",
       size=8)
dez_mil_mortos_bw
cem_mil_mortos<-all_muni_plot + 
  geom_sf(data=points[1:100000], 
          color = atreyu_pal[1], 
          # fill=baroness_pal_seq[5], 
          size=.01, 
          show.legend = FALSE) +
  labs(subtitle="Mapa do Brasil com 100 mil pontos aleatoriamente distribuídos", 
                     caption = "Visualização: Rafael Lopes @rafalpx",
                     size=8)
cem_mil_mortos
cem_mil_mortos_bw<-all_muni_bw + 
  geom_sf(data=points[1:100000], 
          color = "white",
          size=.01, 
          show.legend = FALSE) +
  labs(subtitle="Mapa do Brasil com 100 mil pontos aleatoriamente distribuídos", 
       caption = "Visualização: Rafael Lopes @rafalpx",
       size=8)
cem_mil_mortos_bw
duzentos_mil_mortos<-all_muni_plot + 
  geom_sf(data=points[1:200000], 
          color = atreyu_pal[1], 
          # fill=baroness_pal_seq[5], 
          size=.01, 
          show.legend = FALSE) +
  labs(subtitle="Mapa do Brasil com 200 mil pontos aleatoriamente distribuídos", 
       caption = "Visualização: Rafael Lopes @rafalpx",
       size=8)
duzentos_mil_mortos
duzentos_mil_mortos_bw<-all_muni_bw + 
  geom_sf(data=points[1:200000], 
          color = "white",
          size=.01, 
          show.legend = FALSE) +
  labs(subtitle="Mapa do Brasil com 200 mil pontos aleatoriamente distribuídos", 
       caption = "Visualização: Rafael Lopes @rafalpx",
       size=8)
duzentos_mil_mortos_bw
trezentos_mil_mortos<-all_muni_plot + 
  geom_sf(data=points[1:300000], 
          color = atreyu_pal[1], 
          # fill=baroness_pal_seq[5], 
          size=.01, 
          show.legend = FALSE) +
  labs(subtitle="Mapa do Brasil com 300 mil pontos aleatoriamente distribuídos", 
       caption = "Visualização: Rafael Lopes @rafalpx",
       size=8)
trezentos_mil_mortos
trezentos_mil_mortos_bw<-all_muni_bw + 
  geom_sf(data=points[1:300000], 
          color = "white",
          size=.01, 
          show.legend = FALSE) +
  labs(subtitle="Mapa do Brasil com 300 mil pontos aleatoriamente distribuídos", 
       caption = "Visualização: Rafael Lopes @rafalpx",
       size=8)
trezentos_mil_mortos_bw
quatrocentos_mil_mortos<-all_muni_plot + 
  geom_sf(data=points[1:400000], 
          color = atreyu_pal[1], 
          # fill=baroness_pal_seq[5], 
          size=.01, 
          show.legend = FALSE) +
  labs(subtitle="Mapa do Brasil com 400 mil pontos aleatoriamente distribuídos", 
       caption = "Visualização: Rafael Lopes @rafalpx",
       size=8)
quatrocentos_mil_mortos
quatrocentos_mil_mortos_bw<-all_muni_bw + 
  geom_sf(data=points[1:400000], 
          color = "white",
          size=.01, 
          show.legend = FALSE) +
  labs(subtitle="Mapa do Brasil com 400 mil pontos aleatoriamente distribuídos", 
       caption = "Visualização: Rafael Lopes @rafalpx",
       size=8)
quatrocentos_mil_mortos_bw
quinhentos_mil_mortos<-all_muni_plot + 
  geom_sf(data=points, 
          color = atreyu_pal[1], 
          # fill=baroness_pal_seq[5], 
          size=.01, 
          show.legend = FALSE) +
  labs(subtitle="Mapa do Brasil com 500 mil pontos aleatoriamente distribuídos", 
       caption = "Visualização: Rafael Lopes @rafalpx",
       size=8)
quinhentos_mil_mortos
quinhentos_mil_mortos_bw<-all_muni_bw + 
  geom_sf(data=points[1:500000], 
          color = "white",
          size=.01, 
          show.legend = FALSE) +
  labs(subtitle="Mapa do Brasil com 500 mil pontos aleatoriamente distribuídos", 
       caption = "Visualização: Rafael Lopes @rafalpx",
       size=8)
quinhentos_mil_mortos_bw








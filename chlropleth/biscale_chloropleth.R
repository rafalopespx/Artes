library(biscale)
library(dplyr)
library(ggplot2)
library(cowplot)
library(geobr)
library(sf)
library(aopdata)

data<-stl_race_income %>% 
  bi_class(x = pctWhite, y = medInc, style = "quantile", dim = 3)

map <- data %>% 
  ggplot() + 
  geom_sf(mapping = aes(fill = bi_class), 
          color = "white", size = 0.1, show.legend = FALSE)+
  bi_scale_fill(pal = "DkBlue", dim = 3)+
  labs(
    title = "Race and Income in St. Louis, MO", 
    subtitle = "Dark Blue Palette"
  )+
  bi_theme()
map

legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Higher % White ",
                    ylab = "Higher Income ",
                    size = 8)
# combine map with legend
finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.2, .65, 0.2, 0.2)
finalPlot

# disaster_areas_br<-read_disaster_risk_area(year = 2010, showProgress = FALSE, simplified = TRUE)
# municipalities_br<-read_municipality(year = 2019, showProgress = FALSE)
# neighborhood_br<-read_neighborhood(year = 2010, showProgress = FALSE)
# pop_arrangements_br<-read_pop_arrangements(year = 2015, showProgress = FALSE)
sp_census_tract<-read_census_tract(year = 2010, showProgress = TRUE, code_tract = 3550308)
sp_disaster_areas<-read_disaster_risk_area(year = 2010, showProgress = FALSE)
sp_disaster_areas<-sp_disaster_areas[which(sp_disaster_areas$code_muni == 3550308), ]
sp_municipality_district<-read_neighborhood(year = 2010)
sp_municipality_district<-sp_municipality_district[which(sp_municipality_district$code_muni == 3550308),]
sp_population<-read_population(city = "sao paulo", year = 2010, showProgress = FALSE, geometry = T)
sp_landuse<-read_landuse(city = "sao paulo", year = 2019, showProgress = FALSE, geometry = T)
sp_population<-sp_population %>% 
  mutate(black_percent_pop = P003/P001, 
         white_percent_pop = P002/P001, 
         non_white_percent_pop = (P003 + P004 + P005)/P001) 
sp_population <- sp_population %>% 
         bi_class(x = non_white_percent_pop, y = R002, style = "quantile", dim = 3)

sp_landuse<-sp_landuse %>% 
  mutate(black_percent_pop = P003/P001, 
         white_percent_pop = P002/P001, 
         non_white_percent_pop = (P003 + P004 + P005)/P001) 
sp_landuse <- sp_landuse %>% 
  bi_class(x = non_white_percent_pop, y = R002, style = "quantile", dim = 3)

# Remove plot axis
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

# Plot all Sao Paulo Health Facilities
sp_hosp_plot<- ggplot()+
  geom_sf(data = subset(sp_landuse, S004 > 0),
          aes(fill="High Complexity"), color=NA, alpha=.7) + # high-complexity health facilities
  geom_sf(data = subset(sp_landuse, S003 > 0),
          aes(fill="Medium Complexity"), color=NA, alpha=.7) + # medium-complexity health facilities
  geom_sf(data = subset(sp_landuse, S002 > 0),
          aes(fill="Low Complexity"), color=NA, alpha=.7) + # low-complexity health facilities
  # geom_sf(data = subset(sp_landuse, S001 > 0),
  #         aes(fill="Total Number"), color=NA, alpha=.7) + # Total health facilities
  geom_sf(data = sp_municipality_district, color="black", fill=NA, size=.15, show.legend = FALSE)+
  scale_fill_viridis_b(option = "viridis", aesthetics = "fill")+
  labs(subtitle="Sao Paulo Health Facilities by Complexity", size=8) +
  theme_minimal() +
  no_axis
sp_hosp_plot


bi_plot<-function(var1, var2){
  bi_df <- subset(x, !is.na(x$var1) & !is.na(x$var2) )%>% 
    bi_class(x = x$var1, y = x$var2, style = "quantile", dim = 3) 
  varnames<-names(x)
  var1name<-varnames[which(varnames == var1)]
  var2name<-varnames[which(varnames == var2)]
  
  bi_plot<- bi_df %>% 
    ggplot() + 
    geom_sf(aes(fill = bi_class, 
            color = bi_class), size = 0.1, show.legend = FALSE)+
    geom_sf(data = sp_muni_trans, color="#2D3E50", fill="NA", size=.15, show.legend = FALSE)+
    bi_scale_fill(pal = "DkViolet", dim = 3)+
    bi_scale_color(pal = "DkViolet", dim = 3)+
    labs(
      subtitle = paste0(var1name, " and ", var2name, " Sao Paulo, SP", sep = ""), 
      caption = "Data: geobr, aopdata; production: @rafalpx", size = 5)+
    bi_theme()
  
  legend <- bi_legend(pal = "DkViolet",
                      dim = 3,
                      xlab = paste0("Higher ", var1name, sep = ""),
                      ylab = paste0("Higher ", var2name, sep = ""),
                      size = 8)
  bi_df_final<-ggdraw() +
    draw_plot(bi_plot, 0, 0, 1, 1) +
    draw_plot(legend, 0.1, 0.1, 0.2, 0.2)
  return(bi_df_final)
}

sp_landuse<-sp_landuse %>% 
  mutate(black_percent_pop = P003/P001, 
         white_percent_pop = P002/P001, 
         non_white_percent_pop = (P003 + P004 + P005)/P001, 
         hc_hosp_percent = S004/P001, 
         mc_hosp_percent = S003/P001, 
         lc_hosp_percent = S002/P001, 
         total_hosp_percent = S001/P001) 

sp_landuse_trans<-st_transform(sp_landuse, crs = st_crs(sp_municipality_district))
sp_muni_trans<-st_transform(sp_municipality_district, crs = st_crs(sp_landuse))

sp_joint<-st_join(sp_municipality_district, sp_landuse_trans)

bi_plot<-subset(sp_landuse, !is.na(E001) & !is.na(R003)) %>% 
  bi_class(x = E001, y = R003, style = "quantile", dim = 3) %>% 
  ggplot()+
  geom_sf(aes(fill = bi_class, 
              color = bi_class), size = 0.1, show.legend = FALSE)+
  geom_sf(data = sp_muni_trans, color="#2D3E50", fill="NA", size=.15, show.legend = FALSE)+
  bi_scale_fill(pal = "DkBlue", dim = 3)+
  bi_scale_color(pal = "DkBlue", dim = 3)+
  bi_theme()

legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = paste0("Higher Population"),
                    ylab = paste0("Higher Average Income"),
                    size = 8)
bi_df_final<-ggdraw() +
  draw_plot(bi_plot, 0, 0, 1, 1) +
  draw_plot(legend, 0.1, 0.1, 0.2, 0.2)
bi_df_final
  

library(readr)
library(geobr)
library(tidyverse)
library(biscale)
library(cowplot)
age_srag_br_est<-read_csv("../dados_processados/integridade_SIVEP/age_dados_srag_est.csv.xz") 
age_srag_br_est<-age_srag_br_est%>% 
  filter(data ==  max(data))  %>% 
  group_by(data, age_class, sg_uf) %>% 
  summarise(N=n())
obs_age_srag_br_est<-read_csv("../dados_processados/integridade_SIVEP/age_dados_obsrag_est.csv.xz")
obs_age_srag_br_est<-obs_age_srag_br_est%>% 
  filter(data ==  max(data)) %>% 
  group_by(data, age_class, sg_uf) %>% 
  summarise(N=n())
state_br<-read_state(code_state = "all", year = 2010)

joint_age_br_est<-merge(x = age_srag_br_est, y = obs_age_srag_br_est, 
                        by = c("age_class", "sg_uf", "data"), 
                        suffixes = c("hosp", "deaths")) %>% 
  mutate(lethality = Ndeaths/(Ndeaths + Nhosp), 
         age_class = as.factor(age_class), 
         abbrev_state = sg_uf)

file.name<-"../Codes_sandwich/Artes/plot_ready_age_class.csv.xz"
write_csv(joint_age_br_est, file = xzfile(description = file.name, compression = -9))

plot_lethality<-function(x, age_class){
  plot_ready<-plot_ready %>% 
    bi_class(x = lethality, y = Nhosp, style = "quantile", dim = 3) %>% 
    left_join(state_br, by = "abbrev_state")
  
  plot<-state_br %>% 
    left_join(x %>% 
                filter(age_class == paste0(age_class)) %>% 
                select(bi_class, abbrev_state), 
              by = "abbrev_state") %>% 
    ggplot() + 
    geom_sf(mapping = aes(fill = bi_class),
            color = "white", size = 0.1, show.legend = FALSE)+
    bi_scale_fill(pal = "DkViolet", dim = 3)+
    labs(
      subtitle = paste0("Deaths and Lethality, Brazil"), 
      caption = "production: @rafalpx", size = 5)+
    bi_theme()
  
  leg<-bi_legend(pal = "DkViolet",
            dim = 3,
            xlab = "More Deaths",
            ylab = "Higher Lethality ",
            size = 8)
  
  plot_final<-ggdraw() +
    draw_plot(plot, 0, 0, 1, 1) +
    draw_plot(leg, 0.1, 0.1, 0.2, 0.2)
  return(plot_final)
}

plot_list<-list()
for (i in unique(joint_age_br_est$age_class)) {
  plot_list[[i]]<-plot_lethality(joint_age_br_est, i)
}
  
plot_list[[1]]
plot_list[[2]]
plot_list[[3]]
plot_list[[4]]
plot_list[[5]]
plot_list[[6]]
plot_list[[7]]
plot_list[[8]]
plot_list[[9]]



sp_disaster_plot<- ggplot()+
  # geom_sf(data = sp_municipality_district, color="#2D3E50", fill="#FEBF57", size=.15, show.legend = FALSE)+
  # geom_sf(data = sp_disaster_areas, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE)+
  geom_sf(data = sp_population, aes(fill=black_percent_pop), color=NA, alpha=.7)+
  scale_fill_viridis_c(option = "cividis", direction = -1)+
  labs(subtitle="Sao Paulo Municipality Risk Areas", size=8) +
  theme_minimal() +
  no_axis
sp_disaster_plot

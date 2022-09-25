library(tidyverse)
library(scales)
library(ggtext)
library(patchwork)


#For countries flags
# devtools::install_github("schliebs/ggoxford")
library(ggoxford)



inflation_contributions <- read_csv("data-clean/inflation_clean_data.csv")
morocco_inflation_rates <- read_csv("data-clean/morocco_inflation_clean_data.csv")
Indice_global <- read_csv("data-clean/all_items_inflation.csv")


library(showtext)

font_add_google("Gochi Hand", "gochi")
font_add_google("Indie Flower", "flower")

showtext_auto()

pal_colors <- RColorBrewer::brewer.pal(4, "Dark2")
pal_colors[4] <- "#9932CC"


facets_order <- distinct(inflation_contributions, facet_) %>% 
  mutate(facet_order = c(1,3,2,4),
         facet_color = c(pal_colors[-1],"grey40"),
         facet_label = c("Food", "All items", "Transport", "Contribution to inflation (in %)"),
         facet_label_comp = c("includes food and non-alcoholic beverages (COICOP division 1)",
                              "",
                              "includes items of (COICOP division 7)",
                              "of <i style='color:#D95F02'>Food, <i style='color:#9932CC'>transport, <i style='color:grey40'>and <i style='color:#1B9E77'>all items less food and transport"),
         facet_label = paste(facet_label, "<i style='font-size: 20px'>", facet_label_comp),
         facet_label = glue::glue("<i style='color:{facet_color}'>{facet_label}"))


#reordering strips
Division_levels <- distinct(inflation_contributions, Division) %>% 
  slice(c(4,3,1,2)) %>% 
  pull(1)


inflation_trend <- morocco_inflation_rates %>% 
  filter(year != 2017, date <= "2022-07-01", coicop %in% c(0,1,7)) %>%
  mutate(global = coicop == 0,
         inflation_label = ifelse((year == 2022 & month == 7), paste0(round(inflation_rate, 1), "%"), "")) %>% 
  ggplot(aes(x = date, y = inflation_rate))+
  geom_line(aes(color = reorder(Division, coicop), size = global), alpha = 0.8)+
  geom_text(aes(label = inflation_label, color = reorder(Division, coicop)),
            # family = "gochi",
            size = 5, fontface = "bold",
            vjust = 3,
            # hjust = 0
  )+
  scale_y_continuous(breaks = function(y) seq((min(y)-1) %/% 1, max(y) %/% 1, by = 4))+
  scale_x_date(breaks = date_breaks("3 months"),
               date_labels = "%m/%y")+
  scale_size_manual(values = c(0.5, 0.8))+
  scale_color_manual(values = pal_colors[c(3,2,4)])+
  scale_size_manual(values = c(1,2))+
  labs(x = "", y = "",
       title = "Year-on-year inflation rate evolution from 2018 in <span style='color:#f42f4c'> Morocco </span>")+
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey80", size = 0.3),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey85"),
        plot.background = element_rect(fill = "grey85", color = "grey85"),
        plot.title = element_markdown(family = "flower", face = "bold", size = 25),
        text = element_text(family = "gochi", face = "bold", size = 20))



inflation_lastmonth <- inflation_contributions %>% 
  left_join(facets_order) %>% 
  left_join(Indice_global) %>% 
  mutate(value_label = ifelse(facet_ == "Contributions", round(value), round(value, 1))) %>%
  mutate(Morocco = LOCATION == "MAR") %>% 
  filter(LOCATION != "BRA", LOCATION != "ARG") %>% 
  mutate(LOCATION = reorder(LOCATION, -Indice_Global),
         Division = factor(Division, levels = Division_levels)) %>%
  ggplot(aes(LOCATION, value))+
  geom_bar(aes(fill = factor(Division), alpha = Morocco),
           position = "stack", stat = "identity")+
  geom_text(aes(label = value_label),
            color = "black",size = 4,
            position = position_stack(vjust = 0.5))+
  facet_wrap(~ reorder(facet_label, facet_order),
             nrow = 4, scales = "free_y")+
  scale_alpha_manual(values = c(0.5, 1))+
  scale_fill_manual(values = pal_colors[c(1,4,2,3)])+
  geom_axis_flags(axis = "x",
                  breaks = Indice_global$LOCATION,
                  labels = "",
                  country_icons = Indice_global$LOCATION,
                  width = 50,
                  lineheight = 2,
                  fontface = "bold")+
  labs(x = "", y = "",
       title = "Year-on-year inflation rate in July 2022 for <span style='color:#f42f4c'> Morocco </span> and other <span style='color:#176ea9'> OECD </span> countries",
       caption = "Data source : hcp.ma, stats.oecd.org")+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "grey85", color = "grey85"),
        strip.background = element_blank(),
        text = element_text(family = "gochi"),
        strip.text = element_markdown(size = 23, face = "bold", hjust = 0),
        plot.title = element_markdown(family = "flower", face = "bold", size = 25),
        plot.caption = element_markdown(size = 15))


final_plot <- inflation_trend / inflation_lastmonth +
  plot_layout(heights = c(1, 3))

png(file = "Stories/inflation_story.png", width = 1200, height = 1800)
final_plot
dev.off()

library(datos)
library(tidyverse)
library(forcats)

paises %>%
  group_by(continente, pais) %>%
  summarise(promedio_edv = mean(esperanza_de_vida)) %>%
  top_n(3, promedio_edv) %>%
  arrange(continente, desc(promedio_edv)) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(
    y = forcats::fct_reorder(pais, promedio_edv),
    x = promedio_edv,
    color = continente
  ),
  show.legend = FALSE) +
  geom_segment(
    aes(
      y = fct_reorder(pais, promedio_edv),
      yend = fct_reorder(pais, promedio_edv),
      x = 55,
      xend = promedio_edv,
      color = continente
    ),
    show.legend = FALSE
  ) +
  facet_wrap( ~ continente, ncol = 1, scale = "free_y") +
  labs(
    x = "Esperanza de vida en años",
    y = " ",
    title = "Top 3 de los países con mayor esperanza de vida promedio \n por continente ",
    subtitle = "Datos medidos cada 5 años desde 1952 a 2007",
    caption = "@gabysandovalm"
  ) +
  theme_minimal() +
  theme(
    strip.background = element_rect(colour = "white", fill = "white"),
    strip.text = element_text(colour = "#626567", face = "bold"),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 10),
    title = element_text(size = 10),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) 



library(tidyverse)
library(gridExtra)

df <- readxl::read_excel("dat/tidy_tuesday_week2.xlsx")

col_names <- c('year', 'CB', 'DE', 'LB', 'OL', 'QB', 'RB', 'S', 'DT', 'TE', 'WR')
position_levels <- c('RB', 'QB', 'OL', 'TE', 'WR', 'CB', 'DE', 'DT', 'LB', 'S')
ofdf_levels <- c('Offense', 'Defense')

df_long <- df %>%
  setNames(col_names) %>%
  gather('CB':'WR', key = 'position', value = 'salary') %>%
  mutate(salary = salary/1000000) %>%
  mutate(year = as.Date(as.character(year), format = "%Y"))

df_top16 <- df_long %>%
  mutate(position = factor(position, levels = position_levels)) %>%
  group_by(year, position) %>%
  top_n(16) %>%
  ungroup()

df_top16 %>%
  ggplot(data = .) +
  geom_jitter(mapping = aes(x = year, y = salary), color = 'gray', alpha = 0.6, na.rm = T) +
  geom_smooth(mapping = aes(x = year, y = salary), se = F, color = 'orange', na.rm = T) +
  scale_y_continuous(limits = c(0, 25)) +
  facet_wrap(~position, nrow = 2)

plot.offense <- df_top16 %>% 
  filter(as.integer(position) < 6) %>% 
  ggplot(data = .) +
  geom_jitter(mapping = aes(x = year, y = salary), color = 'gray', alpha = 0.6, na.rm = T) +
  geom_smooth(mapping = aes(x = year, y = salary), se = F, color = 'orange', na.rm = T, span = 0.3) +
  scale_y_continuous(limits = c(0, 25)) +
  facet_wrap(~position, nrow = 1) +
  ggtitle("Offense") +
  scale_x_date(date_breaks = "4 years", date_labels = paste0("'", "%y")) +
  labs(
    x = "Year",
    y = "Salary (Millions of USD)"
  ) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA),
    text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank()
  )

plot.defense <- df_top16 %>% 
  filter(as.integer(position) > 5) %>% 
  ggplot(data = .) +
  geom_jitter(mapping = aes(x = year, y = salary), color = 'gray', alpha = 0.6, na.rm = T) +
  geom_smooth(mapping = aes(x = year, y = salary), se = F, color = 'orange', na.rm = T, span = 0.3) +
  scale_y_continuous(limits = c(0, 25)) +
  facet_wrap(~position, nrow = 1) +
  ggtitle("Defense") +
  scale_x_date(date_breaks = "4 years", date_labels = paste0("'", "%y")) +
  labs(
    x = "Year",
    y = "Salary (Millions of USD)"
  ) +
  theme(
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA),
    text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor.y = element_blank()
  )

g2 <- ggplotGrob(plot.offense)
g3 <- ggplotGrob(plot.defense)
g <- rbind(g2, g3, size = "first")
g$widths <- unit.pmax(g2$widths, g3$widths)
grid.newpage()
grid.draw(g)
grid.arrange(g, top=textGrob(paste0("Average cap value of 16 highest-paid NFL players in each position", "\n"), gp=gpar(fontsize=18,font=8)))

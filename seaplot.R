library(ggplot2) # görselleştirme için

seaplot = function(seating_data) {
  seating_data |>
  ggplot(aes(seat_x_grid, seat_y_grid, label = seats)) +
  geom_point(shape = 0, size = 9.5) +
  geom_point(
    aes(seat_x, seat_y), shape = 15, color = "rosybrown", 
    size = 9.5, na.rm = TRUE
  ) +
  geom_text() +
  labs(
    x = NULL, y = NULL,
    title = paste0("Raslantısal oturma deseni - ", sum(!is.na(seating_data$seat_order)), " kullanıcı")
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.ticks = element_blank(), 
    axis.text = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
}
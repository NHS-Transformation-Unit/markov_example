theme_tu_white <- function(hex_col, hex_text = "#ffffff") {
  theme(strip.background = element_rect(fill = hex_col),
        strip.text = element_text(colour = hex_text, size = 14),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 16, color = hex_col),
        plot.subtitle = element_text(size = 12),
        panel.background = element_rect(fill = "#ffffff"),
        panel.grid.major.y = element_line(color = "#cecece", linewidth = 0.1),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "#cecece", linewidth = 0.1),
        axis.line = element_line(color = "#000000"),
        legend.position = "right",
        legend.text = element_text(size = 12)
  )
  
}

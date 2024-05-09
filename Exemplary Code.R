library(ggplot2)

df = read.csv("approval_topline.csv")

df$end_date = as.Date(df$end_date)
tikz = c("JAN. 23, 2021", "AUG. 11", "FEB. 27", "SEPT. 15", "APRIL 3", "OCT. 20", "MARCH 20")

custikz = as.Date(c("2021-01-23", "2021-08-11", "2022-02-27",
                    "2022-09-15", "2023-04-03", "2023-10-20", "2024-03-20"))
#theme
FiveTheme = theme_minimal(base_size = 10) +
  theme(
    text = element_text(family = 'Helvetica'),
    plot.title = element_text(family = 'Helvetica', face = 'bold', hjust = 0.5),
    plot.background = element_rect(fill = 'white', color = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.line.x = element_line(color = 'lightgray'),
    axis.text.x = element_text(color = "gray30"),
    axis.line.y = element_line(color = 'lightgray'),
    axis.text.y = element_text(color = "gray30"),
    legend.position = "none",
    plot.margin = margin(b = 0.5, r = 20),
    plot.subtitle = element_text(size = 6, hjust = 0.5)
  )
#graph
ggplot(df, aes(x = end_date)) +
  geom_line(aes(y = approve_estimate, color = "Approval Estimate")) +
  geom_line(aes(y = disapprove_estimate, color = "Disapproval Estimate")) +
  labs(title = element_blank(),
       subtitle = "95% OF POLLS PROJECTED\n TO FALL WITHIN THIS RANGE",
       y = element_blank(),
       x = element_blank()) +
  scale_color_manual(values = c("Approval Estimate" = "green3", "Disapproval Estimate" = "#FF00DB")) +
  geom_ribbon(data = df, aes(x = end_date, ymin = approve_lo, ymax = approve_hi), fill = "green3", alpha = 0.2)+
  geom_ribbon(data = df, aes(x = end_date, ymin = disapprove_lo, ymax = disapprove_hi), fill = "#FF00DB", alpha = 0.2)+
  scale_x_date(breaks = custikz, labels = tikz)+
  scale_y_continuous(breaks = seq(20, 80, by = 10),
                     labels = function(x) paste0(x, ifelse(x == 80, "% ", "   ")))+
  coord_cartesian(ylim = c(20, 80), xlim = as.Date(c("2021-01-23", "2024-03-20")),
                  expand = F)+
  FiveTheme

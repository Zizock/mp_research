# script for drawing graphs in Chapter 2, section 2

rm(list=ls())
library(tidyverse)
library(reshape2)
library(zoo)
library(psych)
library(here)
library(patchwork)

# read data
total_data <- read_excel(here("data", "summarized_for_graph.xlsx"), sheet="Sheet1")

total_data$Date <- as.Date(total_data$Date)

total_data <- total_data %>% filter(Date <= as.Date("2019-12-01"))

# plot 0: 15+ lfpr and unemployment
library(cowplot)

# ==== fig2.1 ====

# ==== fig2.1 part1 ====
p1 <- ggplot(total_data, aes(x = Date)) +
  geom_rect(aes(xmin = as.Date("2008-11-01"), xmax = as.Date("2014-10-01"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01) +
  geom_line(aes(y = us_lfpr_15plus), color = "blue", linewidth = 1) +
  labs(y = "US Labor ForceParticipation Rate (15+)", x = "Date") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(axis.title.y = element_text(color = "blue"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid = element_blank())

# Create the plot for Interest Rate (secondary axis)
p2 <- ggplot(total_data, aes(x = Date)) +
  geom_line(aes(y = us_unemployment), color = "red", linewidth = 1) +
  scale_y_continuous(
    position = "right"
  ) +
  labs(y = "US Unemployment Rate", x = NULL) +
  theme_minimal() +
  theme(axis.title.y = element_text(color = "red"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())

# # Overlay the two plots
# final_plot <- ggdraw() +
#   draw_plot(p1) +
#   draw_plot(p2, 0, 0, 1, 1, align = "vh", axis = "y")
# 
# # Display the final plot
# print(final_plot)

aligned_plots <- align_plots(p1, p2, align="hv", axis="tblr")
final_graph_us <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])

# ==== fig2.1 part2 ====

p1 <- ggplot(total_data, aes(x = Date)) +
  geom_rect(aes(xmin = as.Date("2013-04-01"), xmax = as.Date("2019-12-01"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01) +
  geom_line(aes(y = japan_lfpr_15plus), color = "blue", linewidth = 1) +
  labs(y = "Japan Labor ForceParticipation Rate (15+)", x = "Date") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(axis.title.y = element_text(color = "blue"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid = element_blank())

# Create the plot for Interest Rate (secondary axis)
p2 <- ggplot(total_data, aes(x = Date)) +
  geom_line(aes(y = japan_unemployment), color = "red", linewidth = 1) +
  scale_y_continuous(
    position = "right"
  ) +
  labs(y = "Japan Unemployment Rate", x = NULL) +
  theme_minimal() +
  theme(axis.title.y = element_text(color = "red"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())

# # Overlay the two plots
# final_plot <- ggdraw() +
#   draw_plot(p1) +
#   draw_plot(p2, 0, 0, 1, 1, align = "vh", axis = "y")
# 
# # Display the final plot
# print(final_plot)

aligned_plots <- align_plots(p1, p2, align="hv", axis="tblr")
final_graph_japan <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])

# ==== fig2.1 combine ====
final_plot <- (final_graph_us / final_graph_japan)  +
  plot_layout(heights = c(1, 1),)

final_plot

# ==== fig2.2 ====

# plot 1: both countries, lfpr 15+
us_total <- ggplot(total_data, aes(x = Date, y = us_lfpr_15plus)) +
  geom_rect(aes(xmin = as.Date("2008-11-01"), xmax = as.Date("2014-10-01"),
              ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01) +
  geom_line(color = "black", linewidth = 1) +
  labs(title = "US Labor Force Participation Rate (15+)",
       x = "Date",
       y = "Labor Force Participation Rate (%)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid = element_blank())

# Plot for Japan Labor Force Participation Rate (15+)
japan_total <- ggplot(total_data, aes(x = Date, y = japan_lfpr_15plus)) +
  geom_rect(aes(xmin = as.Date("2013-04-01"), xmax = as.Date("2019-12-01"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01) +
  geom_line(color = "black", linewidth = 1) +
  labs(title = "Japan Labor Force Participation Rate (15+)",
       x = "Date",
       y = "Labor Force Participation Rate (%)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid = element_blank())


# final_plot <- (us_total / japan_total)  +
# plot_layout(heights = c(1, 1),)
# 
# final_plot

# plot 2: bot hcountries, by age
# Plot for US Labor Force Participation Rate (25-54)
us_1 <- ggplot(total_data, aes(x = Date, y = us_lfpr_2554)) +
  geom_rect(aes(xmin = as.Date("2008-11-01"), xmax = as.Date("2014-10-01"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01) +
  geom_line(color = "black", linewidth = 1) +
  labs(title = "US Labor Force Participation Rate (25-54)",
       x = "Date",
       y = "Labor Force Participation Rate (%)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid = element_blank())

# Plot for US Labor Force Participation Rate (55+)
us_2 <- ggplot(total_data, aes(x = Date, y = us_lfpr_55plus)) +
  geom_rect(aes(xmin = as.Date("2008-11-01"), xmax = as.Date("2014-10-01"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01) +
  geom_line(color = "black", linewidth = 1) +
  labs(title = "US Labor Force Participation Rate (55+)",
       x = "Date",
       y = "Labor Force Participation Rate (%)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid = element_blank())

# Plot for Japan Labor Force Participation Rate (15-64)
japan_1 <- ggplot(total_data, aes(x = Date, y = japan_lfpr_1564)) +
  geom_rect(aes(xmin = as.Date("2013-04-01"), xmax = as.Date("2019-12-01"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01) +
  geom_line(color = "black", linewidth = 1) +
  labs(title = "Japan Labor Force Participation Rate (15-65)",
       x = "Date",
       y = "Labor Force Participation Rate (%)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid = element_blank())

# Plot for Japan Labor Force Participation Rate (65+)
japan_2 <- ggplot(total_data, aes(x = Date, y = japan_iflr_65plus)) +
  geom_rect(aes(xmin = as.Date("2013-04-01"), xmax = as.Date("2019-12-01"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01) +
  geom_line(color = "black", linewidth = 1) +
  labs(title = "Japan Labor Force Participation Rate (65+)",
       x = "Date",
       y = "Labor Force Participation Rate (%)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid = element_blank())

final_plot <-   ((us_total + japan_total) / (us_1 + japan_1) / (us_2 + japan_2))  +
  plot_layout(heights = c(1, 1, 1), widths = c(2, 2, 2))
final_plot

# ==== fig2.3 ====

# plot labor size for Japan
# Plot for Japan Labor Force Size (15+)
japan_1 <- ggplot(total_data, aes(x = Date, y = japan_size_15plus/1000000)) +
  geom_rect(aes(xmin = as.Date("2013-04-01"), xmax = as.Date("2019-12-01"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01) +
  geom_line(color = "black", linewidth = 1) +
  labs(title = "Japan Labor Force Size (15+)",
       x = "Date",
       y = "Japan Labor Force Size (Million)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid = element_blank())

# Plot for Japan Labor Force Size (15-64)
japan_2 <- ggplot(total_data, aes(x = Date, y = japan_size_1564/1000000)) +
  geom_rect(aes(xmin = as.Date("2013-04-01"), xmax = as.Date("2019-12-01"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01) +
  geom_line(color = "black", linewidth = 1) +
  labs(title = "Japan Labor Force Size (15-64)",
       x = "Date",
       y = "Japan Labor Force Size (Million)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid = element_blank())

final_plot <-   (japan_1 / japan_2)  +
  plot_layout(heights = c(1, 1))
final_plot

# ==== other not used ====

# plot lfpr against ssr
library(cowplot)

p1 <- ggplot(total_data, aes(x = Date)) +
  geom_line(aes(y = us_lfpr_15plus), color = "blue", size = 1) +
  labs(y = "US Labor ForceParticipation Rate (15+)", x = "Date") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(axis.title.y = element_text(color = "blue"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid = element_blank())

# Create the plot for Interest Rate (secondary axis)
p2 <- ggplot(total_data, aes(x = Date)) +
  geom_line(aes(y = us_ssr), color = "red", size = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  scale_y_continuous(
    position = "right"
  ) +
  labs(y = "US Shadow Interest Rate", x = NULL) +
  theme_minimal() +
  theme(axis.title.y = element_text(color = "red"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())

# # Overlay the two plots
# final_plot <- ggdraw() +
#   draw_plot(p1) +
#   draw_plot(p2, 0, 0, 1, 1, align = "vh", axis = "y")
# 
# # Display the final plot
# print(final_plot)

aligned_plots <- align_plots(p1, p2, align="hv", axis="tblr")
final_graph_us <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])

p1 <- ggplot(total_data, aes(x = Date)) +
  geom_line(aes(y = japan_lfpr_15plus), color = "blue", size = 1) +
  labs(y = "Japan Labor ForceParticipation Rate (15+)", x = "Date") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(axis.title.y = element_text(color = "blue"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid = element_blank())

# Create the plot for Interest Rate (secondary axis)
p2 <- ggplot(total_data, aes(x = Date)) +
  geom_line(aes(y = japan_ssr), color = "red", size = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  scale_y_continuous(
    position = "right"
  ) +
  labs(y = "Japan Shadow Interest Rate", x = NULL) +
  theme_minimal() +
  theme(axis.title.y = element_text(color = "red"),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())

# # Overlay the two plots
# final_plot <- ggdraw() +
#   draw_plot(p1) +
#   draw_plot(p2, 0, 0, 1, 1, align = "vh", axis = "y")
# 
# # Display the final plot
# print(final_plot)

aligned_plots <- align_plots(p1, p2, align="hv", axis="tblr")
final_graph_japan <- ggdraw(aligned_plots[[1]]) + draw_plot(aligned_plots[[2]])

final_plot <- (final_graph_us / final_graph_japan)  +
plot_layout(heights = c(1, 1),)

final_plot

# ==== fig2.10 ====
# labor participation by gender

# US male
us_1 <- ggplot(total_data, aes(x = Date, y = us_male)) +
  geom_rect(aes(xmin = as.Date("2008-11-01"), xmax = as.Date("2014-10-01"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01) +
  geom_line(color = "black", linewidth = 1) +
  labs(title = "US Labor Force Participation Rate, Male",
       x = "Date",
       y = "Labor Force Participation Rate (%)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid = element_blank())

# US female
us_2 <- ggplot(total_data, aes(x = Date, y = us_female)) +
  geom_rect(aes(xmin = as.Date("2008-11-01"), xmax = as.Date("2014-10-01"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01) +
  geom_line(color = "black", linewidth = 1) +
  labs(title = "US Labor Force Participation Rate, Female",
       x = "Date",
       y = "Labor Force Participation Rate (%)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid = element_blank())

# Japan male
japan_1 <- ggplot(total_data, aes(x = Date, y = japan_male)) +
  geom_rect(aes(xmin = as.Date("2013-04-01"), xmax = as.Date("2019-12-01"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01) +
  geom_line(color = "black", linewidth = 1) +
  labs(title = "Japan Labor Force Participation Rate, Male",
       x = "Date",
       y = "Labor Force Participation Rate (%)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid = element_blank())

# Japan female
japan_2 <- ggplot(total_data, aes(x = Date, y = japan_female)) +
  geom_rect(aes(xmin = as.Date("2013-04-01"), xmax = as.Date("2019-12-01"),
                ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.01) +
  geom_line(color = "black", linewidth = 1) +
  labs(title = "Japan Labor Force Participation Rate, Female",
       x = "Date",
       y = "Labor Force Participation Rate (%)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 60, hjust = 1),
        panel.grid = element_blank())

final_plot <-   ((us_1 + japan_1) / (us_2 + japan_2))  +
  plot_layout(heights = c(1, 1), widths = c(2, 2))
final_plot









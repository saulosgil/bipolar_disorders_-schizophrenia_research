# Packages ------------------------------------------------------------------------------------
library(tidyverse)

# Loading data --------------------------------------------------------------------------------
df <- read_delim("acute_response.csv",
                 delim = ";",
                 escape_double = FALSE,
                 trim_ws = TRUE)

# Plot ----------------------------------------------------------------------------------------
# Acute response
df |>
  ggplot(aes(x = tempo,
             y = il6,
             fill = tempo)) +
  geom_col(
    data = ~group_by(.x,
                     tempo)  |>
      summarize(il6 = mean(il6))  |>
      ungroup(),
    color = "black",
    fill = c("white", "#99A3A4"),
    show.legend = FALSE
  ) +
  geom_point(aes(shape = grupo),
             size = 3,
             show.legend = FALSE) +
  geom_line(aes(group = sujeito,
                linetype = grupo),
            show.legend = TRUE) +
  scale_linetype_manual(values=c("solid", "longdash"))+
  labs(title = "IL-6",
       y = "IL-6 (pg/ml)") +
  xlab(label = "") +
  scale_x_discrete(labels = c("Pre", "IM")) +
  scale_fill_discrete(guide="none") +
  theme_classic() +
  ggtitle("IL-6") +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12))

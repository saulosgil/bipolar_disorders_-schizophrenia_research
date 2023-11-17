# Loading data - SCH --------------------------------------------------------------------------------
df <- read_delim("bd.csv",
                 delim = ";",
                 escape_double = FALSE,
                 trim_ws = TRUE)

### MADRS
madrs <-
  df |>
  ggplot(aes(x = tempo,
             y = madrs,
             fill = tempo)) +
  geom_col(
    data = ~group_by(.x,
                     tempo)  |>
      summarize(madrs = mean(madrs))  |>
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
  labs(title = "MADRS",
       y = "a.u.") +
  xlab(label = "") +
  scale_x_discrete(labels = c("Pre", "Post")) +
  scale_fill_discrete(guide="none") +
  theme_classic() +
  ggtitle("MADRS") +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12))

madrs

### Young
young <-
  df |>
  ggplot(aes(x = tempo,
             y = young,
             fill = tempo)) +
  geom_col(
    data = ~group_by(.x,
                     tempo)  |>
      summarize(young = mean(young))  |>
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
  labs(title = "YOUNG",
       y = "a.u.") +
  xlab(label = "") +
  scale_x_discrete(labels = c("Pre", "Post")) +
  scale_fill_discrete(guide="none") +
  theme_classic() +
  ggtitle("YOUNG") +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12))

young
# Plot ----------------------------------------------------------------------------------------
# Schizofrenia  ---------------------------------------------------------------------------
## Chronic response
# Loading data - BD --------------------------------------------------------------------------------
df <- read_delim("sch.csv",
                 delim = ";",
                 escape_double = FALSE,
                 trim_ws = TRUE)

### PANSS P
panss_p <-
  df |>
  ggplot(aes(x = tempo,
             y = panss_p,
             fill = tempo)) +
  geom_col(
    data = ~group_by(.x,
                     tempo)  |>
      summarize(panss_p = mean(panss_p))  |>
      ungroup(),
    color = "black",
    fill = c("white", "#99A3A4"),
    show.legend = FALSE
  ) +
  geom_point(aes(),
             shape = 17,
             size = 3,
             show.legend = FALSE) +
  geom_line(aes(group = sujeito,
                linetype = grupo),
            show.legend = TRUE) +
  scale_linetype_manual(values="longdash")+
  labs(title = "PANSS P",
       y = "a.u.") +
  xlab(label = "") +
  scale_x_discrete(labels = c("Pre", "Post")) +
  scale_fill_discrete(guide="none") +
  theme_classic() +
  ggtitle("PANSS P") +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12))

panss_p

# panss_n

### PANSS P
panss_n <-
  df |>
  ggplot(aes(x = tempo,
             y = panss_n,
             fill = tempo)) +
  geom_col(
    data = ~group_by(.x,
                     tempo)  |>
      summarize(panss_n = mean(panss_n))  |>
      ungroup(),
    color = "black",
    fill = c("white", "#99A3A4"),
    show.legend = FALSE
  ) +
  geom_point(aes(),
             shape = 17,
             size = 3,
             show.legend = FALSE) +
  geom_line(aes(group = sujeito,
                linetype = grupo),
            show.legend = TRUE) +
  scale_linetype_manual(values = "longdash")+
  labs(title = "PANSS N",
       y = "a.u.") +
  xlab(label = "") +
  scale_x_discrete(labels = c("Pre", "Post")) +
  scale_fill_discrete(guide="none") +
  theme_classic() +
  ggtitle("PANSS N") +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12))

panss_n

# layout --------------------------------------------------------------------------------------
(madrs + young)/(panss_p + panss_n)


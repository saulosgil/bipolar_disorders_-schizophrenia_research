# Packages ------------------------------------------------------------------------------------
library(tidyverse)
library(patchwork)

# Loading data --------------------------------------------------------------------------------
df <- read_delim("acute_response.csv",
                 delim = ";",
                 escape_double = FALSE,
                 trim_ws = TRUE)

# Plot ----------------------------------------------------------------------------------------
## Acute response
### IL-6
il6 <-
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

il6

### IL-4
il4 <-
  df |>
  ggplot(aes(x = tempo,
             y = il4,
             fill = tempo)) +
  geom_col(
    data = ~group_by(.x,
                     tempo)  |>
      summarize(il4 = mean(il4))  |>
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
  labs(title = "IL-4",
       y = "IL-4 (pg/ml)") +
  xlab(label = "") +
  scale_x_discrete(labels = c("Pre", "IM")) +
  scale_fill_discrete(guide="none") +
  theme_classic() +
  ggtitle("IL-4") +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12))

il4

### IL-2
il2 <-
  df |>
  ggplot(aes(x = tempo,
             y = il2,
             fill = tempo)) +
  geom_col(
    data = ~group_by(.x,
                     tempo)  |>
      summarize(il2 = mean(il2))  |>
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
  labs(title = "IL-2",
       y = "IL-2 (pg/ml)") +
  xlab(label = "") +
  scale_x_discrete(labels = c("Pre", "IM")) +
  scale_fill_discrete(guide="none") +
  theme_classic() +
  ggtitle("IL-2") +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12))

il2

### IL-10
il10 <-
  df |>
  ggplot(aes(x = tempo,
             y = il10,
             fill = tempo)) +
  geom_col(
    data = ~group_by(.x,
                     tempo)  |>
      summarize(il10 = mean(il10))  |>
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
  labs(title = "IL-10",
       y = "IL-10 (pg/ml)") +
  xlab(label = "") +
  scale_x_discrete(labels = c("Pre", "IM")) +
  scale_fill_discrete(guide="none") +
  theme_classic() +
  ggtitle("IL-10") +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12))

il10

### TNF
tnf <-
  df |>
  ggplot(aes(x = tempo,
             y = tnf,
             fill = tempo)) +
  geom_col(
    data = ~group_by(.x,
                     tempo)  |>
      summarize(tnf = mean(tnf))  |>
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
  labs(title = "TNF",
       y = expression(TNF-~alpha  (pg/ml))) +
  xlab(label = "") +
  scale_x_discrete(labels = c("Pre", "IM")) +
  scale_fill_discrete(guide="none") +
  theme_classic() +
  ggtitle(expression(TNF-~alpha)) +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12))

tnf

### IFN
ifn <-
  df |>
  ggplot(aes(x = tempo,
             y = ifn,
             fill = tempo)) +
  geom_col(
    data = ~group_by(.x,
                     tempo)  |>
      summarize(ifn = mean(ifn))  |>
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
  labs(title = "IFN",
       y = expression(IFN-~gamma  (pg/ml))) +
  xlab(label = "") +
  scale_x_discrete(labels = c("Pre", "IM")) +
  scale_fill_discrete(guide="none") +
  theme_classic() +
  ggtitle(expression(IFN-gamma)) +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12))

ifn

# layout --------------------------------------------------------------------------------------
(il2 + il4 + il6)/(il10 + tnf + ifn)











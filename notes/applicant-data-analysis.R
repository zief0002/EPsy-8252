library(dplyr)
library(ggplot2)
library(readxl)
library(stringr)

qme <- read_excel("~/Dropbox/qme/students-and-applicants/qme-applicants.xlsx")
head(qme)


qme2 = qme %>%
  mutate(
    gre_v = as.numeric(str_remove(GRE_V, "\\*")),
    gre_q = as.numeric(str_remove(GRE_Q, "\\*")),
    gre_w = as.numeric(str_remove(GRE_W, "\\*"))
    ) %>%
  select(Admit, gre_v, gre_q, gre_w) %>%
  filter(gre_v < 200)

qme2 %>%
  filter(Admit == "Yes" | Admit == "No") %>%
  group_by(Admit) %>%
  summarize(
    Verbal = mean(gre_v, na.rm = TRUE),
    Verbal_SD = sd(gre_v, na.rm = TRUE),
    Quant  = mean(gre_q, na.rm = TRUE),
    Quant_SD = sd(gre_q, na.rm = TRUE),
    Write  = mean(gre_w, na.rm = TRUE),
    Write_SD = sd(gre_w, na.rm = TRUE)
  ) %>%
  knitr::kable("html", digits = 1, col.names = c("Admit", rep(c("M", "SD"), 3))) %>%
  kableExtra::add_header_above(c(" " = 1, "Verbal" = 2, "Quant" = 2, "Write" = 2))


p1 = ggplot(data = qme2, aes(x = gre_v)) +
  geom_dotplot() +
  facet_wrap(~Admit, ncol = 1) +
  ggtitle("GRE Verbal")

p2 = ggplot(data = qme2, aes(x = gre_q)) +
  geom_dotplot() +
  facet_wrap(~Admit, ncol = 1) +
  ggtitle("GRE Quantitative")

p3 = ggplot(data = qme2, aes(x = gre_w)) +
  geom_dotplot() +
  facet_wrap(~Admit, ncol = 1) +
  ggtitle("GRE Writing")

gridExtra::grid.arrange(p1, p2, p3, nrow = 1)


install.packages(c("ggplot2", "dplyr", "tidyr", "patchwork", "esquisse"))

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

zadanie_1 <- function() {
  x <- seq(0, 1, length.out = 200)
  df <- data.frame(
    x = x,
    sin_x = sin(x),
    tg_x = tan(x),
    y_x = x
  )
  
  df_long <- df %>%
    pivot_longer(cols = -x, names_to = "Funkcja", values_to = "Wartosc")
  
  p <- ggplot(df_long, aes(x=x, y = Wartosc, color = Funkcja)) + 
    geom_line(size = 1.2) + 
    scale_color_manual(
      values = c("sin_x" = "blue", "tg_x" = "red", "y_x" = "green"), 
      labels = c(expression(sin(x)), expression(tg(x)), "y = x")
    ) + 
    labs(
      title = "funkcje trygonometryczne i liniowa",
      x = "x",
      y = "f(x)",
      color = "Legenda"
    ) +
    theme_minimal()
  print(p)
}
zadanie_1()


zadanie_2 <- function() {
  dane <- data.frame(
    Student = factor(c(1, 2, 3, 4, 5)),
    Ocena_1 = c(4, 5, 4.5, 3, 5),
    Ocena_2 = c(4.5, 4.5, 5, 3.5, 4)
  )
  
  df_long = dane %>%
    pivot_longer(cols = starts_with("Ocena"), names_to = "Typ_Oceny", values_to = "Stopien")
  
  p<- ggplot(df_long, aes(x = Student, y = Stopien, fill = Typ_Oceny)) +
    geom_col(position = "dodge", width = 0.7) +
    scale_fill_brewer(palette = "Paired", labels = c("Ocena 1", "Ocena 2")) +
    labs(
      title = "Wyniki studentów",
      x = "Student",
      y = "Ocena",
      fill = "Legenda"
    ) +
    theme_light()
  
  print(p)
}
zadanie_2()


zadanie_3 <- function() {
  x <- seq(-2*pi, 2*pi, length.out = 300)
  
  df <- data.frame(x = x) %>%
    mutate(
      sin = sin(x),
      cos = cos(x),
      tg = tan(x),
      ctg = 1/tan(x)
    ) %>%
    pivot_longer(cols = -x, names_to = "Funkcja", values_to = "y")
  
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_line(color = "blue") +
    facet_wrap(~Funkcja, scales = "free_y") +
    coord_cartesian(ylim = c(-3, 3)) +
    labs(title = "Funkcje trygonometryczne") +
    theme_bw()
  
  print(p)
}


zadanie_3()


zadanie_4 <- function() {

  rysuj_pie <- function(data, title_str) {
    ggplot(data, aes(x = "", y = wartosc, fill = kategoria)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(title = title_str, x = NULL, y = NULL, fill = NULL) +
      theme_void() +
      geom_text(aes(label = wartosc), position = position_stack(vjust = 0.5))
  }
  
  df_plec <- data.frame(kategoria = c("Kobieta", "Mężczyzna"), wartosc = c(100, 95))
  df_wiek <- data.frame(kategoria = c("<20", "21-30", "31-40", "41-50", ">51"), 
                        wartosc = c(20, 40, 55, 121, 61))
  df_wyksztalcenie <- data.frame(kategoria = c("Podstawowe", "Średnie", "Wyższe"), 
                                 wartosc = c(10, 85, 123))
  
  p1 <- rysuj_pie(df_plec, "Struktura Płci")
  p2 <- rysuj_pie(df_wiek, "Struktura Wieku")
  p3 <- rysuj_pie(df_wyksztalcenie, "Struktura Wykształcenia")
  
  final_plot <- p1 / p2 / p3 + plot_layout(guides = 'collect')
  
  print(final_plot)
}

zadanie_4()


zadanie_5 <- function() {
  set.seed(123)
  n <- 1000
  
  punkty <- data.frame(
    x = runif(n, 0, 1),
    y = runif(n, 0, 1)
  )
  
  punkty <- punkty %>%
    mutate(
      pozycja = ifelse(y < x & y < (1 - x), "Poniżej", "Powyżej")
    )
  
  p <- ggplot(punkty, aes(x = x, y = y, color = pozycja)) +
    geom_point(alpha = 0.6) +
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color = "black", linetype = "dashed") +
    geom_segment(aes(x = 0, y = 1, xend = 1, yend = 0), color = "black", linetype = "dashed") +
    scale_color_manual(values = c("Poniżej" = "red", "Powyżej" = "blue")) +
    coord_fixed() +
    labs(title = "Monte Carlo w kwadracie jednostkowym") +
    theme_minimal()
  
  print(p)
}

zadanie_5()


## ZADANIE 6

set.seed(42)
czas <- 1:100
zmiany <- rnorm(100, mean = 0.1, sd = 2)
cena <- cumsum(zmiany) + 100
df_line <- data.frame(dzien = czas, cena = cena)

ggplot(df_line, aes(x = dzien, y = cena)) +
  geom_path(color = "green", size = 1) +
  labs(title = "Symulacja kursu akcji", y = "Cena") +
  theme_classic()



kolory <- sample(c("Czerwony", "Czarny", "Srebrny", "Biały"), 200, replace = TRUE, prob = c(0.1, 0.4, 0.3, 0.2))
df_bar <- data.frame(kolor = kolory)

ggplot(df_bar, aes(x = kolor, fill = kolor)) +
  geom_bar() +
  scale_fill_manual(values = c("Biały"="#f0f0f0", "Czarny"="#333333", "Czerwony"="firebrick", "Srebrny"="grey70"))


grupa_A <- rnorm(500, mean = 70, sd = 10)
grupa_B <- rnorm(500, mean = 55, sd = 15)
df_dens <- data.frame(
  wynik = c(grupa_A, grupa_B),
  grupa = rep(c("Grupa A", "Grupa B"), each = 500)
)

ggplot(df_dens, aes(x = wynik, fill = grupa)) +
  geom_density(alpha = 0.5) +
  labs(title = "Porównanie gęstości", x = "Punkty") +
  theme_minimal()



browser_data <- data.frame(
  browser = c("Chrome", "Firefox", "Safari", "Edge", "Inne"),
  share = c(60, 15, 10, 10, 5)
)

browser_data <- browser_data %>% 
  arrange(desc(share)) %>%
  mutate(prop = share / sum(share) * 100) %>%
  mutate(ypos = cumsum(prop) - 0.5*prop )

ggplot(browser_data, aes(x = 2, y = prop, fill = browser)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y", start = 0) +
  labs(title = "Przeglądarki", x = NULL, y = NULL) +
  theme_void()


##ZADANIE 7


set.seed(999)
n_obs <- 500
dane_mieszkania <- data.frame(
  dzielnica = sample(c("Centrum", "Mokotów", "Praga", "Ursynów"), n_obs, replace = TRUE),
  powierzchnia = round(rnorm(n_obs, mean = 60, sd = 20), 1),
  cena_za_m2 = round(rnorm(n_obs, mean = 12000, sd = 2000), 0),
  rok_budowy = sample(1980:2023, n_obs, replace = TRUE),
  winda = sample(c(TRUE, FALSE), n_obs, replace = TRUE)
)

dane_mieszkania <- dane_mieszkania %>%
  mutate(cena_calkowita = powierzchnia * cena_za_m2 * ifelse(dzielnica == "Centrum", 1.2, 1.0))

esquisse::esquisser(dane_mieszkania)

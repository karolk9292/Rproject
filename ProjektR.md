---
title: "Analiza Długości Śledzia w Czasie"
knit(input="readme.rmd", output = "readme.md")
author: "Karol Kuchta"
date: "`r Sys.Date()`"
output:
  html_document:
    toc: true
    theme: united

---


```{r}
library(knitr)
library(purrr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(gganimate)
library(gifski)
library(png)
library(caret)
```

Header 1
------------------------------
Lista wykorzystanych bibliotek:

```{r}

cat(sapply(search()[grep("package:",search())],function(x){return(paste0('library("',gsub("package:","",x),'")','\n'))}))
```

Header 2
------------------------------
Ustawienie ziarna w celu uzyskiwania tego samego wyniku:

```{r}
set.seed(5)
```

Header 3
------------------------------
Wczytanie i przypisanie dancyh do zmiennej:

```{r}
d = read.csv("http://www.cs.put.poznan.pl/dbrzezinski/teaching/sphd/sledzie.csv",sep = ',', header = T, na.strings = c("?"))
year = rep(1:60, each = 52582/60)
d1 = cbind(d[1:52560,], year)
```

Header 4
------------------------------
Przetworzenie brakujacych danych:

```{r}
for(i in 1:ncol(d1)){
  d1[is.na(d1[,i]), i] <- mean(d1[,i], na.rm = TRUE)
}
```

Header 5
------------------------------
Podstawowe statystyki:

```{r}
dim(d1)
summary(d1)
```

Header 6
------------------------------
Prezentacja wartości atrybutów:

```{r}
d1 %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

d1 %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(sample = value)) +
  facet_wrap(~ key, scales = "free") +
  stat_qq() + stat_qq_line()
```

Header 7
------------------------------
Związki korelacji pomiędzy zmiennymi:

```{r}
t = d1[setdiff(colnames(d1), c('X', 'xmonth'))]
cor(t)
corrplot(cor(t), method = "circle")
```

Header 8
------------------------------
Animacja przedstawiająca zmianę długości śledzia na przestrzeni 60 lat:

```{r}
p <- ggplot(
  d1, 
  aes(x = length, y = year, size = recr, colour = "red")
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() 


t = p + transition_time(year) +
  labs(title = "Year: {frame_time}")
animate(t, fps = 2)
```

Header 9, 10
------------------------------
Utworzenie modeli regrecyjnych:

```{r}
d1.idx = sample(1:nrow(d1), 0.7*nrow(d1), replace = F)
prac = d1[d1.idx,]
test = d1[-d1.idx,]
val = train( length ~ sst + nao + fbar + lcop1 + chel1 + year, data = d1,method = "lm", trControl = trainControl( method = "cv", number = 10, verboseIter = T))

model_lm2 = lm(length ~ sst + nao + fbar + lcop1 + chel1 + year, data = prac)
model_lm3 = lm(length ~ sst + nao + fbar + lcop1 + chel1  + year, data = test)



lenght_value_prac = predict(model_lm2, prac)
lenght_value_test = predict(model_lm3, test)

difference_prac = lenght_value_prac - prac$length
difference_test = lenght_value_test - test$length

rmse_prac = sqrt(mean(difference_prac^2))
rmse_test = sqrt(mean(difference_test^2))
```

Prezentacja otrzymanych statystyk: 

```{r}
val
summary(model_lm2)
summary(model_lm3)
rmse_prac
rmse_test
```

Zmienna sst ktora ma ujemna korelacje ma wpływ na to, że rozmiar śledzia maleje wraz z upływem lat.
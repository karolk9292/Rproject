library(knitr)
library(purrr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(gganimate)
library(gifski)
library(png)

cat(sapply(search()[grep("package:",search())],function(x){return(paste0('library("',gsub("package:","",x),'")','\n'))}))


set.seed(5)

f = file.choose()
d = read.csv(f,sep = ',', header = T, na.strings = c("?"))
d

for(i in 1:ncol(d)){
  d[is.na(d[,i]), i] <- mean(d[,i], na.rm = TRUE)
}
d

dim(d)
summary(d)

d %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()


library(knitr)
library(purrr)
library(tidyr)
library(ggplot2)


cat(sapply(search()[grep("package:",search())],function(x){return(paste0('library("',gsub("package:","",x),'")','\n'))}))


set.seed(5)

f = file.choose()
d = read.csv(f,sep = ',', header = T, na.strings = c("?"))
d

for(i in 1:ncol(d)){
  d[is.na(d[,i]), i] <- mean(d[,i], na.rm = TRUE)
}
d

dim(d)
summary(d)

d %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()


d %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(sample = value)) +
  facet_wrap(~ key, scales = "free") +
  stat_qq() + stat_qq_line()

t = d[setdiff(colnames(d), c('X', 'xmonth'))]
cor(t)
corrplot(cor(t), method = "circle")
d[c("length", "sst", "xmonth", "recr")]

p <- ggplot(
  d, 
  aes(x = length, y = length, size = recr, colour = "red")
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(2, 12)) +
  scale_x_log10() 
p

t = p + transition_time(xmonth) +
  labs(title = "Month: {frame_time}")
animate(t, fps = 2)

# Esquerda na cidade de direita

library(readxl)
dados <- read_excel("dados.xlsx")
library(tidyverse)

dados <- subset(dados, select = c(DeLiz2020, Jailson2004, Bairro))

# Esquerda em Rio do Sul? continuidade espacial?
# Jaílson 2004
# Jean de Liz 2020 
# 19 bairros
dados$Bairro

summary(dados$DeLiz2020)
summary(dados$Jailson2004)

cor.test(dados$Jailson2004, dados$DeLiz2020)
model <- lm(DeLiz2020 ~ Jailson2004, data=dados)
summary(model)
# a cada 1 por cento a mais para Jaílson em 2004, 
# estima-se que De Liz tenha recebido 0,28% a mais naquele bairro

ggplot(data = dados, aes(x = Jailson2004, y = DeLiz2020)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) + geom_text(aes(label = Bairro))

# é incrível a continuidade no tempo dessa relação.


# Algo a mais?

# se usar níves
dados$Jailson <- ntile(dados$Jailson2004, 2)
dados$Jailson <- as.factor(dados$Jailson)
levels(dados$Jailson)
levels(dados$Jailson) <- c('1','2')


by(dados$DeLiz2020, dados$Jailson, mean)

g <- ggplot(dados, aes(Jailson, DeLiz2020))
g + geom_boxplot() +  xlab("Nível de apoio a de Liz em 2020") +
  ylab("% votos válidos Jaílson 2004")


bar.m <- ggplot(data = dados, aes(Jailson,DeLiz2020))
bar.m + geom_bar(stat = "summary", fun = "mean",
                 fill = "steelblue") + 
  stat_summary(aes(label=round(..y.., 2)),
               fun = mean, geom = "text", 
               size=4, vjust=10, color = "black")+
  stat_summary(fun.data = mean_se, geom="errorbar", 
               width = 0.6) +
  labs(title = "Médias por Grupo - Jean de Liz e Jaílson", 
       subtitle = "", x = "Níveis de apoio Jaílson (PT) em 2004", y = "Votos Válidos De Liz(PDT)/2020", 
       caption = "Fonte: TSE") + theme_bw() + theme(text = element_text(size = 13))


ggplot(data = dados) +
  geom_point(mapping = aes(x=Jailson, y =DeLiz2020 , color = DeLiz2020 > 16.88 )) # q a votação dele

# conclusão, vários fatores explicam a distribuição espacial do voto à esquerda
# em rio do sul em 2020. A votação de 2004 é uma delas.


dados$dLiz <- dados$DeLiz2020 > 16.88
voto1 <- glm(dLiz ~ Jailson2004, data = dados, family=binomial(link=logit))
library(sjPlot)
tab_model(voto1, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
(1.26-1)*100

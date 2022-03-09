# ultima comparação renda

library(ggplot2)
library(readxl)
d <- read_excel("DADOS_2.xlsx")
a <- read_excel("DADOS3.xlsx")
cor.test(a$Jailson2004, a$RENDA)
cor.test(d$J.deLiz2016, d$RENDA)
cor.test(d$DeLiz2020, d$RENDA)

pleito <- c("Jaílson2004", "Jean de Liz2016", "Jean de Liz2020")
cor <- c(-53,-29,-41)
horr <- data.frame(pleito, cor)


bar <- ggplot(horr, aes(pleito, cor))
bar + geom_bar(stat = "identity") + 
  labs(title = "Voto e renda dos bairros de Rio do Sul", 
       subtitle = "Esquerda em 2004, 2016 e 2020 em 16 bairros",
       x = "Eleição", y= "Correlação(eleição x renda média bairro)",
       caption = "Valores negativos indicam maior propensão a 
       votação em bairros mais pobres") + 
  theme_bw() + theme(text = element_text(size = 12))

dados44 <- read_excel("dados44.xlsx")
model1 <- lm(DeLiz2020 ~ Jailson2004, data=dados44) 
library(sjPlot)
tab_model(model1, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
model2 <- lm(DeLiz2020 ~ J.deLiz2016, data = dados44)
tab_model(model2, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
model3 <- lm(DeLiz2020 ~ J.deLiz2016 + Jailson2004, data = dados44)
tab_model(model2, model3, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
model4 <- lm(DeLiz2020 ~ J.deLiz2016 + RENDA, data = dados44)
tab_model(model2, model3, model4, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
model5 <- lm(DeLiz2020 ~ J.deLiz2016 + RENDA + Jailson2004, data = dados44)
tab_model(model2, model3, model4, model5, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
library(coefplot)
coefplot(model5, intercept = FALSE)
model11 <- lm(J.deLiz2016 ~ Jailson2004, data = dados44)
tab_model(model11, show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
coefplot(model3, intercept = FALSE) # usar essa

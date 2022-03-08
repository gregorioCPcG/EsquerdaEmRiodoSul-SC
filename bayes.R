# dif
#priori uniforme 
# s inicial (votação de 2016)
#posteriori 1 voitação em 2020
#posteriori2 da esquerda chance de ganhar 18% (retirado da novas analises.R em Preferitura Rio do Sul Hist)

# Posteriori 3 calcular a chance de vitória da esquerda em 2020 tendo em 
#base que Bolsonaro reduziu(ver na pasta reduziu para Rio do Sul D:\previsaoBolso22AltoVale) #caiu 20
# (ver correlaçãpo bolsonaro e Dliz 0.77 - pasta NOVO RSL) # fazer vezes 0.68


options("scipen"=100, "digits"=4)


#chance de ganhar mais do que 16.8%8
#chance de ganhar mais de 35%
set.seed(2) #conforme orientações do curso ibpad
a1 <- 1
a2 <- 1
n <- 100
s <- 13.44
#10 mil tentativas
#fórmula rbeta(10000, a1 + s, a2 + n-s) 
#acrescentar '>= .x)/10000' para responder à pergunta
sum(rbeta(10000, a1+s, (a2+(n-s))) > 0.1688)/10000#16,88%
sum(rbeta(10000, a1+s, (a2+(n-s))) > 0.35)/10000#35%
hist(rbeta(10000, a1+s, (a2+(n-s))))

#posteriori 1 - votação de 2020
a1 <- a1+s #novo a1
a2 <- a2+(n-s) #novo a2
n <- 100
s <- 16.88
hist(rbeta(10000, a1+s, (a2+(n-s))))
sum(rbeta(10000, a1+s, (a2+(n-s))) > 0.1688)/10000#16,88%
sum(rbeta(10000, a1+s, (a2+(n-s))) > 0.35)/10000#35%

# posteriori 2 - chance de vitória histórica 18%
a1 <- a1+s #novo a1
a2 <- a2+(n-s) #novo a2
n <- 100
s <- 18
hist(rbeta(10000, a1+s, (a2+(n-s))))
sum(rbeta(10000, a1+s, (a2+(n-s))) > 0.1688)/10000#16,88%
sum(rbeta(10000, a1+s, (a2+(n-s))) > 0.35)/10000#35%


# posteriori 3 HADDAD (x queda bolsonaro) 0.68*20 # otimista
a1 <- a1+s #novo a1
a2 <- a2+(n-s) #novo a2
n <- 100
s <- 18+10#5 dos 20 q o Bolsonaro perdeu
hist(rbeta(10000, a1+s, (a2+(n-s))))
sum(rbeta(10000, a1+s, (a2+(n-s))) > 0.1688)/10000#16,88%
sum(rbeta(10000, a1+s, (a2+(n-s))) > 0.30)/10000#35%
a <- (rbeta(10000, a1+s, (a2+(n-s))))
max(a)
min(a)
boxplot(a*100)
summary(a)
IQR(a)
hist(a)
media <- mean(a)
sumar <- summary(a)
sumar
media
rm(a1,a2,n,s)
# testar intervalos
Jean <- a
Jean <- data.frame(Jean)
rm(a)
n <- 1000
samp <- sample_n(Jean, n)
samp %>%
  summarise (mean_samp60 = mean(Jean), med_s60 = median(Jean),
             se_s60 = sd(Jean))
sumar#para conferir
sd(Jean$Jean) # para conferir

#confidence 95%
z_star_95 <- qnorm(0.975)
z_star_95
samp %>%
  summarise(lower = mean(Jean) - z_star_95 * (sd(Jean) / sqrt(n)),
            upper = mean(Jean) + z_star_95 * (sd(Jean) / sqrt(n)))

# confidence levels
params <- Jean %>%
  summarise(mu = mean(Jean))

samp %>%
  summarise(samp_min = min(Jean), samp1_max = max(Jean))
Jean %>%
  summarise(pop_min = min(Jean), pop_max = max(Jean))#para conferir

library(statsr)
library(dplyr)
ci <- Jean %>%
  rep_sample_n(size = n, reps = 50, replace = TRUE) %>%
  summarise(lower = mean(Jean) - z_star_95 * (sd(Jean) / sqrt(n)),
            upper = mean(Jean) + z_star_95 * (sd(Jean) / sqrt(n)))
ci %>%
  slice(1:5) # só pra ver
ci <- ci %>%
  mutate(capture_mu = ifelse(lower < params$mu & upper > params$mu, "yes", "no"))
ci %>%
  slice(1:20) # só pra ver

ci_data <- data.frame(ci_id = c(1:50, 1:50),
                      ci_bounds = c(ci$lower, ci$upper),
                      capture_mu = c(ci$capture_mu, ci$capture_mu))
ggplot(data = ci_data, aes(x = ci_bounds, y = ci_id, 
                           group = ci_id, color = capture_mu)) +
  geom_point(size = 2) +  # add points at the ends, size = 2
  geom_line() +           # connect with lines
  geom_vline(xintercept = params$mu, color = "darkgray") # draw vertical line

# 99% Confidence Level

ci99 <- Jean %>%
  rep_sample_n(size = n, reps = 50, replace = TRUE) %>%
  summarise(lower = mean(Jean) - 2.58 * (sd(Jean) / sqrt(n)),
            upper = mean(Jean) + 2.58 * (sd(Jean) / sqrt(n)))

ci99 %>%
  slice(1:5)
#
ci99 <- ci99 %>%
  mutate(capture_mu = ifelse(lower < params$mu & upper > params$mu, "yes", "no"))
#
ci99_data <- data.frame(ci99_id = c(1:50, 1:50),
                        ci99_bounds = c(ci$lower, ci$upper),
                        capture_mu = c(ci$capture_mu, ci$capture_mu))
#
ggplot(data = ci99_data, aes(x = ci99_bounds, y = ci99_id, 
                             group = ci99_id, color = capture_mu)) +
  geom_point(size = 2) +  # add points at the ends, size = 2
  geom_line() +           # connect with lines
  geom_vline(xintercept = params$mu, color = "darkgray") # draw vertical line

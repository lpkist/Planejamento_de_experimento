# Lucas Perondi Kist
# RA236202

# O seguinte pacote é necessário para o correto funcionamento do código
# Caso não estejam instalados, rodar a linha de código abaixo
install.packages("tidyverse")


# Carregando o pacote
library(tidyverse)


# Leitura dos dados disponibilizados
dados <- matrix(c(1, 1, 1 , 1, 1, 1, 26, 38,
                  2, 1, 1 , 2, 2, 2, 16, 6,
                  3, 1, 2, 1, 1, 2, 3, 17,
                  4, 1, 2, 2, 2 , 1, 18, 16,
                  5, 2, 1, 1, 2, 2, 0, 5,
                  6, 2, 1, 2, 1,  1, 0, 1,
                  7, 2, 2, 1, 2 , 1, 4, 5,
                  8, 2, 2, 2, 1, 2, 5, 3),
                nrow = 8, byrow = T) %>% data.frame()
colnames(dados) <- c("Realização ","A","B","C","D","E","P1","P2")

# Cálculo do SN
dados <- dados %>% mutate("SN" = -10*log((P1^2+P2^2)/2,10))

# ANOVAs realizadas, segundo descrito no relatório
anova(lm(SN~I(factor(A))+I(factor(B))+I(factor(C))+I(factor(D))+I(factor(E)), dados)) 

anova(lm(SN~I(factor(A))+I(factor(B))+I(factor(C))+I(factor(D)), dados))

anova(lm(SN~I(factor(A))+I(factor(B))+I(factor(C)), dados))

anova(lm(SN~I(factor(A))+I(factor(C)), dados))

anova(lm(SN~I(factor(A)), dados)) 

anova(lm(SN~I(factor(A)), dados), lm(SN~I(factor(A))+I(factor(B))+I(factor(C))+I(factor(D))+I(factor(E)), dados))

# Média dos SNs para cada nível dos fatores
A <- dados %>% group_by(A) %>% summarise(mean(SN)) %>% pivot_wider(names_from = A, values_from = `mean(SN)`)
B <- dados %>% group_by(B) %>% summarise(mean(SN))%>% pivot_wider(names_from = B, values_from = `mean(SN)`)
C <- dados %>% group_by(C) %>% summarise(mean(SN))%>% pivot_wider(names_from = C, values_from = `mean(SN)`)
D <- dados %>% group_by(D) %>% summarise(mean(SN))%>% pivot_wider(names_from = D, values_from = `mean(SN)`)
E <- dados %>% group_by(E) %>% summarise(mean(SN))%>% pivot_wider(names_from = E, values_from = `mean(SN)`)

efEx <- cbind("Fator" = c("A","B","C","D","E"),rbind(A,B,C,D,E)) %>% pivot_longer(cols=2:3,names_to = "Nível", values_to = "Média de SN")
# Gráfico
ggplot(efEx,aes(x = as.numeric(Nível), y=`Média de SN`))+geom_point()+
  geom_line()+facet_wrap(~Fator, nrow=2)+theme_bw()+
  scale_x_continuous(breaks = 1:2)+labs(x = "Nível", y= "Média de SN")+
  theme(text = element_text(size=13))


# Leitura dos dados disponibilizados (SN já estava computado)
dados2 <- matrix(c(1, 1, 1, 1, 1, 19.1, 20.0, 19.6, 19.6, 19.9, 16.9, 9.5, 15.6, 24.025,
                  2, 1, 2, 2, 2, 21.9, 24.2, 19.8, 19.7, 19.6, 19.4, 16.2, 15.0, 25.522,
                  3, 1, 3, 3, 3, 20.4, 23.3, 18.2, 22.6, 15.6, 19.1, 16.7, 16.3, 25.335,
                  4, 2, 1, 2, 3, 24.7, 23.2, 18.9, 21.0, 18.6, 18.9, 17.4, 18.3, 25.904,
                  5, 2, 2, 3, 1, 25.3, 27.5, 21.4, 25.6, 25.1, 19.4, 18.6, 19.7, 26.908,
                  6, 2, 3, 1, 2, 24.7, 22.5, 19.6, 14.7, 19.8, 20.0, 16.3, 16.2, 25.326,
                  7, 3, 1, 3, 2, 21.6, 24.3, 18.6, 16.8, 23.6, 18.4, 19.1, 16.4, 25.711,
                  8, 3, 2, 1, 3, 24.4, 23.2, 19.6, 17.8, 16.8, 15.1, 15.6, 14.2, 24.832,
                  9, 3, 3, 2, 1, 28.6, 22.6, 22.7, 23.1, 17.3, 19.3, 19.9, 16.1, 26.152),
                nrow = 9, byrow = T) %>% data.frame()
colnames(dados2) <- c("Realização ","A","B","C","D"," N1"," N2"," N3"," N4"," N5"," N6"," N7"," N8","SN")
# SN está correto
dados2 %>%  mutate(aux = -10*log(((1/` N1`)^2+(1/` N2`)^2+(1/` N3`)^2+
                                    (1/` N4`)^2+(1/` N5`)^2+(1/` N6`)^2+
                                    (1/` N7`)^2+(1/` N8`)^2)/8,10))

dados2 <- dados2 %>% select(A,B,C,D,SN)
# Decomposição da soma dos quadrados
anovaa <- anova(lm(SN~I(factor(A))+I(factor(B))+I(factor(C))+I(factor(D)), dados2))
rbind(anovaa[1:4,1:3], I(anovaa[1:4,1:3] %>% colSums())[1:3]) 

# Média dos SNs para cada nível de cada fator
a <- dados2 %>% group_by(A) %>% summarise(mean(SN)) %>% pivot_wider(names_from = A, values_from = `mean(SN)`)
b <- dados2 %>% group_by(B) %>% summarise(mean(SN))%>% pivot_wider(names_from = B, values_from = `mean(SN)`)
c <- dados2 %>% group_by(C) %>% summarise(mean(SN))%>% pivot_wider(names_from = C, values_from = `mean(SN)`)
d <- dados2 %>% group_by(D) %>% summarise(mean(SN))%>% pivot_wider(names_from = D, values_from = `mean(SN)`)


efn <- cbind("Fator" = c("A","B","C","D"),rbind(a,b,c,d)) %>% pivot_longer(cols=2:4,names_to = "Nível", values_to = "Média de SN")
# Gráficos
efn %>% 
  ggplot(aes(x = as.numeric(Nível), y=`Média de SN`))+geom_point()+
  geom_line()+facet_wrap(~Fator, nrow=2)+theme_bw()+ylim(c(24,27))+
  scale_x_continuous(breaks = 1:3)+labs(x = "Nível", y= "Média de SN")+
  theme(text = element_text(size=13))

# Caso seja necessário instalar os pacotes, utilizar
# install.packages("tidyverse")
# install.packages("patchwork")
# install.packages("xtable")
# install.packages("PMCMRplus")

# Se der erro de enconding, basta abrir utilizando encoding UTF-8

# Importando os pacotes
library(tidyverse)
library(patchwork)
library(xtable)
library(PMCMRplus)

# Lendo os dados
dados <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR5ZZ7wErlNDmDqPoq1bx3ssfahqNfZO3qZbJUCoH3jPWH5zsvGtSlfl5CxI3t1Qz7yBLq45m-kZY5C/pub?output=csv")

# Dados coletados
dados %>% mutate(Lote = factor(Lots),
                 Conteiner = factor(Containers),
                 R1 = `Réplica 1`/1e4,
                 R2 = `Réplica 2`/1e4,
                 R3 = `Réplica 3`/1e4) %>% 
  dplyr::select(-c(`Réplica 1`, `Réplica 2`, `Réplica 3`, Lots, Containers)) %>% 
  xtable(digits = 4) %>%  print(include.rownames = F)
# Manipulando o banco de dados
dados <- dados %>%  mutate(R1 = `Réplica 1`/1e4,
                 R2 = `Réplica 2`/1e4,
                 R3 = `Réplica 3`/1e4,
                 Lote = factor(Lots),
                 Conteiner = factor(Containers)) %>% 
  dplyr::select(-c(`Réplica 1`, `Réplica 2`, `Réplica 3`, Lots, Containers)) %>% 
  pivot_longer(cols = c("R1", "R2", "R3"), values_to = "Medida", names_to = "Réplica")

# Definindo o tema dos gráficos 
meu_tema <- theme_bw()+   theme(text = element_text(size=17))


# Análise descritiva
# Histograma das medidas
hist1 <- dados %>% ggplot(aes(x = Medida))+
  geom_histogram(color = 1, fill = "darkblue")+
  meu_tema+
  labs(x = "Medida", y = "Contagem")+
  scale_y_continuous(breaks = seq(0,10,by=2))+
  scale_x_continuous(breaks = seq(99.75,100.75,by=.25),
                     labels = str_replace(as.character(seq(99.75,100.75,by=.25)),"\\.",","))

# Quantidades relevantes das medidas
summary(dados$Medida)

# Boxplot das medidas
bp1 <- dados %>% ggplot(aes(y = Medida))+
  geom_boxplot(color = 1, fill = "darkblue")+
  meu_tema+
  labs(x = "", y = "Medida")+
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank())+
  xlim(c(-1,1))+
  scale_y_continuous(breaks = seq(99.75,100.75,by=.25),
                     labels = str_replace(as.character(seq(99.75,100.75,by=.25)),"\\.",","))

# União dos gráficos em uma figura

#pdf("descritiva.pdf", width = 7, height = 4)
(hist1+bp1)
#dev.off()


# Visualização das medidas em ordem
dados %>% arrange(Medida) %>% view

# Análise exploratória

# Boxplots das medidas por lote

#pdf("lote.pdf", width = 7, height = 4)
dados %>% ggplot(aes(x = Lote, y = Medida))+
  geom_boxplot(color = 1, fill ="darkblue")+
  meu_tema+
  labs(x = "Lote", y = "Medida")+
  scale_y_continuous(breaks = seq(99.75,100.75,by=.25),
                     labels = str_replace(as.character(seq(99.75,100.75,by=.25)),"\\.",","))

#dev.off()


# Medidas por contêiner
#pdf("conteiner.pdf", width = 7, height = 4)
dados %>% ggplot(aes(x = Conteiner, y = Medida, color = Lote))+
  geom_point()+
  meu_tema+
  labs(x = "Contêiner", y = "Medida")+
  scale_y_continuous(breaks = seq(99.75,100.75,by=.25),
                     labels = str_replace(as.character(seq(99.75,100.75,by=.25)),"\\.",","))

#dev.off()


# Análise inferencial
# ANOVA
modelo <- aov(Medida ~ Lote/Conteiner, dados)
sq <- modelo %>%
  summary() 


# Cálculo dos valores das estatísticas F e p-valores de acordo com as fórmulas
# apresentadas na metodologia
sq[[1]]$`F value` <-sq[[1]]$`Mean Sq`/lead(sq[[1]]$`Mean Sq`,1)
sq[[1]]$`Pr(>F)`[1] <- 1-pf(sq[[1]]$`F value`[1],2,9)
sq[[1]]$`Pr(>F)`[2] <- 1-pf(sq[[1]]$`F value`[2],9,24)

sq %>% xtable() %>% print(include.rownames = F)
sq[[1]] %>% colSums()
sq[[1]]$`Pr(>F)`


# Diagnóstico do modelo

# QQ-plot normal
#pdf("qqplot.pdf", width = 7, height = 4)
data.frame(res = modelo$residuals) %>% ggplot(aes(sample = res))+
  geom_qq()+
  geom_qq_line()+
  labs(x = "Quantis teóricos", y = "Quantis amostrais")+
  meu_tema
#dev.off()  

# Teste Shapiro-Wilk
shapiro.test(modelo$residuals)


# Resíduos por contêiner
#pdf("varres.pdf", width = 7, height = 4)
data.frame(Resíduo = modelo$residuals, Contêiner = dados$Conteiner) %>%
  ggplot()+
  geom_point(aes(x = `Contêiner`, y = Resíduo))+
  meu_tema
#dev.off()

# Teste de Hartley para os contêineres
hartleyTest(modelo$residuals,g = dados$Conteiner)


# Resíduos por lote
#pdf("varres_l.pdf", width = 7, height = 4)
data.frame(Resíduo = modelo$residuals, Lote = dados$Lote) %>%
  ggplot()+
  geom_point(aes(x = Lote, y = Resíduo))+
  meu_tema
#dev.off()


# Teste de Hartley para os lotes
hartleyTest(modelo$residuals,g = dados$Lote)


# Resíduos por valor ajustado
#pdf("fitted.pdf", width = 7, height = 4)
data.frame(Resíduo = modelo$residuals, "Valor ajustado" = modelo$fitted.values) %>%
  ggplot()+
  geom_point(aes(x = Valor.ajustado, y = Resíduo))+
  meu_tema+
  labs(x = "Valor ajustado")+
  scale_x_continuous(breaks = seq(99.75,100.75,by=.25),
                     labels = str_replace(as.character(seq(99.75,100.75,by=.25)),"\\.",","),limits = c(99.75,100.6))

#dev.off()




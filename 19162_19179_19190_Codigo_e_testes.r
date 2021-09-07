# Antônio Hideto Borges Kotsubo, 19162
# Ian de Almeida Pinheiro,       19179
# Matheus Seiji Luna Noda,       19190

# Inicialização
names <- c("horario", "temp", "vento", "umid", "sensa")
cepagri <- read.csv("C:/Users/cotuca/Downloads/cepagri.csv", header = F, sep = ";", col.names = names)
cepagri <- subset(cepagri, as.integer(format(as.Date(cepagri$horario, format="%d/%m/%Y"),"%Y")) > 2014)
cepagri <- subset(cepagri, as.integer(format(as.Date(cepagri$horario, format="%d/%m/%Y"),"%Y")) < 2021)
cepagri
teste <- subset(cepagri, cepagri$sensa < 60)
# Trazemos a tabela ao projeto e filtramos seus dados inválidos.

# Bibliotecas utilizadas no projeto. 
library(dplyr)
library(lubridate)
library(magrittr)
library(ggplot2)
library(ggridges)
library(viridis)
library(hrbrthemes)

# Código do gráfico de violino, da variação entre sensação e temperatura, pela umidade.
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}
# Função copiada da lib plyr, para evitar conflito com a lib dplyr.
teste_m <- teste
mDia <- teste_m %>%
  mutate(dia = floor_date(as.Date(horario, format="%d/%m/%Y"))) %>%
  group_by(dia) %>%
  summarize(sensacao = round_any(sensa,10), temp = round_any(as.numeric(temp), 10), humidade = mean(umid), diff = round_any(-as.numeric(temp)+sensacao, 10))
# Média dos dados.
 ggplot(mDia, aes(x= diff, y = humidade, fill = as.factor(diff))) + geom_violin() + theme_light()
# Criação do gráfico.
 
 
# Código do gráfico de violino, da variação entre sensação e temperatura, pelo vento.
round_any = function(x, accuracy, f=round){f(x/ accuracy) * accuracy}

teste_m <- teste
mDia <- teste_m %>%
 mutate(dia = floor_date(as.Date(horario, format="%d/%m/%Y"))) %>%
 group_by(dia) %>%
 summarize(sensacao = round_any(sensa,10), temp = round_any(as.numeric(temp), 10), vento = mean(vento), diff = round_any(-as.numeric(temp)+sensacao, 10))

ggplot(mDia, aes(x= diff, y = vento, fill = as.factor(diff))) + geom_violin() + theme_light()
 

# Gráfico com densidade das temperaturas de cada mês, ao longo de todos os anos.
teste_m <- teste
mMes <- teste_m %>%
 mutate(dia = floor_date(as.Date(horario, unit="months", format="%d/%m/%Y"))) %>%
 group_by(dia) %>%
 summarize(Temperatura = mean(as.numeric(temp)), Sensacao = mean(sensa), mes=month(dia))

ggplot(mMes, aes(x= Temperatura, y = factor(month.name[mes], c(month.name), ordered = TRUE), fill = stat(x))) + 
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  labs(x = "Temperatura (°C)", y = "Média dos meses") + 
  scale_fill_viridis_c(name = "Temp. [C]", option = "C") + 
  theme_ridges(font_size = 13, grid = TRUE)

# Gráfico com densidade das sesações térmicas de cada mês, ao longo de todos os anos.
ggplot(mMes, aes(x= Sensacao, y = factor(month.name[mes], c(month.name), ordered = TRUE), fill = stat(x))) + 
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01) +
  labs(x = "Sensação (°C)", y = "Média dos meses") + 
  scale_fill_viridis_c(name = "Sens. [C]", option = "C") + 
  theme_ridges(font_size = 13, grid = TRUE)

# Código que gera a correlação entre os campos da tabela.
cor(data.frame(data=as.numeric(as.Date(teste$horario, format="%d/%m/%Y")) - 16435, sensa=teste$sensa, temp=as.numeric(teste$temp), vento=teste$vento, umid=teste$umid))
# Nota-se que subtraímos 16435, a fim de que a data seja um número que parte de 1 no dia 01/01/2015


# Gráfico da média de sesação térmica por dia, ao longo do tempo.
teste_m <- teste

mDia_sensa <- teste_m %>%
  mutate(dia = floor_date(as.Date(horario, format="%d/%m/%Y"))) %>%
  group_by(dia) %>%
  summarize(media_sensa = mean(sensa))

png("grafico_sensa.png")
grafico_sensam <- ggplot(mDia_sensa, aes(x = dia, y = media_sensa))
grafico_sensam <- grafico_sensam + geom_point(aes(colour = media_sensa), alpha = 0.5) +
scale_color_continuous(low = "blue",high = "red")

grafico_sensam
dev.off(3)
dev.list()

# Gráfico da média de temperatura por dia, ao longo do tempo.
teste_m <- teste
mDia_temp <- teste_m %>%
  mutate(dia = floor_date(as.Date(horario, format="%d/%m/%Y"))) %>%
  group_by(dia) %>%
  summarize(Temperatura = mean(as.numeric(temp)))

png("grafico_temp_dia.png")
grafico_tempm <- ggplot(mDia_temp, aes(x = dia, y = Temperatura))
grafico_tempm <- grafico_tempm + geom_point(aes(colour = Temperatura), alpha = 0.5) +
  scale_color_continuous(low = "blue",high = "red")

grafico_tempm
dev.list()
dev.off(2)

# Gráfico da média de temperatura por ano, ao longo do tempo.
teste_m <- teste
mAno_temp <- teste_m %>%
  mutate(Ano = floor_date(unit = "year", x = as.Date(horario, format="%d/%m/%Y"))) %>%
  group_by(Ano) %>%
  summarize(Temperatura = mean(as.numeric(temp)))

png("grafico_temp_ano.png")
grafico_tempm_year <- ggplot(mAno_temp, aes(x = Ano, y = Temperatura))
grafico_tempm_year <- grafico_tempm_year + geom_col() + coord_cartesian(ylim= c(21,23)) + scale_color_continuous(low = "blue",high = "red")

grafico_tempm_year
dev.off(4)
dev.list()

# Gráfico de dispersão que relaciona a sensação térmica à temperatura.
teste_m <- teste

mDia_sensa <- teste_m %>%
  mutate(horario = floor_date(as.Date(horario, format="%d/%m/%Y"))) %>%
  group_by(horario) %>%
  summarize(media_sensa = mean(sensa))

mDia_temp <- teste_m %>%
  mutate(horario = floor_date(as.Date(horario, format="%d/%m/%Y"))) %>%
  group_by(horario) %>%
  summarize(media_temp = mean(as.numeric(temp)))

mTemp_sensa <- data.frame(media_temp = mDia_temp$media_temp, media_sensa = mDia_sensa$media_sensa)
                

png("grafico_temp_sensa.png")
grafico_temp_sensa <- ggplot(mTemp_sensa, aes(x = media_temp, y = media_sensa))
grafico_temp_sensa <- grafico_temp_sensa + geom_point() + geom_smooth()

grafico_temp_sensa
dev.list()
dev.off(3)

# Gráfico de sensação térmica por temperatura, sobreposto à temperatura por umidade.
teste_m <- teste
mDia_sensa <- teste_m %>%
  mutate(horario = floor_date(as.Date(horario, format="%d/%m/%Y"))) %>%
  group_by(horario) %>%
  summarize(media_sensa = mean(sensa))
mDia_temp <- teste_m %>%
  mutate(horario = floor_date(as.Date(horario, format="%d/%m/%Y"))) %>%
  group_by(horario) %>%
  summarize(media_temp = mean(as.numeric(temp)))
mDia_umid <- teste_m %>%
  mutate(horario = floor_date(as.Date(horario, format="%d/%m/%Y"))) %>%
  group_by(horario) %>%
  summarize(media_umid = mean(umid))
mTemp_sensa_umid  <- data.frame(media_temp = mDia_temp$media_temp, media_sensa = mDia_sensa$media_sensa, media_umid = mDia_umid$media_umid )
png("grafico_temp_sensa_umid.png")
grafico_temp_sensa_umid <- ggplot(mTemp_sensa_umid , aes(x=media_temp)) +
  
  geom_point(aes(y=media_sensa), color=rgb(0.9,0,0,0.6)) + 
  geom_point(aes(y=media_umid / 3), color=rgb(0,0.1,0.9,0.6)) + 
  
  scale_y_continuous(
    
    name = "Sensação",
    
    sec.axis = sec_axis(~.*3, name="Umidade")
  )
grafico_temp_sensa_umid
dev.list()
dev.off(2)

# Gráfico de sensação térmica por temperatura, sobreposto à temperatura por vento.
teste_m <- teste
mDia_sensa <- teste_m %>%
  mutate(horario = floor_date(as.Date(horario, format="%d/%m/%Y"))) %>%
  group_by(horario) %>%
  summarize(media_sensa = mean(sensa))
mDia_temp <- teste_m %>%
  mutate(horario = floor_date(as.Date(horario, format="%d/%m/%Y"))) %>%
  group_by(horario) %>%
  summarize(media_temp = mean(as.numeric(temp)))
mDia_vento <- teste_m %>%
  mutate(horario = floor_date(as.Date(horario, format="%d/%m/%Y"))) %>%
  group_by(horario) %>%
  summarize(media_vento = mean(vento))
mTemp_sensa_vento  <- data.frame(media_temp = mDia_temp$media_temp, media_sensa = mDia_sensa$media_sensa, media_vento = mDia_vento $media_vento)
png("grafico_temp_sensa_vento.png")
grafico_temp_sensa_vento<- ggplot(mTemp_sensa_vento  , aes(x=media_temp)) +
  
  geom_point(aes(y=media_sensa), color=rgb(0.9,0,0,0.6)) + 
  geom_point(aes(y=media_vento/ 3), color=rgb(0,0.1,0.9,0.6)) + 
  
  scale_y_continuous(
    
    name = "Sensação",
    
    sec.axis = sec_axis(~.*3, name="Vento")
  )
grafico_temp_sensa_vento
dev.list()
dev.off(3)

# TESTE - Variação entre invernos e verões
teste_m <- teste

mMes_temp <- teste_m %>%
  mutate(horario = floor_date(unit = "month", x = as.Date(horario, format="%d/%m/%Y"))) %>%
  group_by(horario) %>%
  summarize(media_temp = mean(as.numeric(temp)))

inverno <- subset(mMes_temp, month(horario) > 5 & month(horario) < 10)
verao <- subset(mMes_temp, month(horario) < 4 | month(horario) > 11)

mInverno <- inverno %>%
  mutate(horario = year(floor_date(unit = "year", x = as.Date(horario, format="%d/%m/%Y")))) %>%
  group_by(horario) %>%
  summarize(media_temp = mean(as.numeric(media_temp)))

mVerao <- verao %>%
  mutate(horario = year(floor_date(unit = "year", x = as.Date(horario, format="%d/%m/%Y")))) %>%
  group_by(horario) %>%
  summarize(media_temp = mean(as.numeric(media_temp)))

mVerao_temp <- rep(mVerao$media_temp, each = 2)
mVerao_temp[1:6 * 2] <- mInverno$media_temp
mVerao_temp <- mVerao_temp[1:11]
mInverno_temp <- mVerao_temp[2:12]

ano <- c(paste(as.character(rep(2015:2019, each = 2)), c(" - inverno", " - verão")), "2020 - inverno")
tabelaInvernoVerao <- data.frame(ano = ano, temp_anterior = mVerao_temp, temp_atual = mInverno_temp, diferenca = mInverno_temp - mVerao_temp)
names(tabelaInvernoVerao) <- c("Estação", "Temperatura anterior", "Temperatura atual", "Variação")
tabelaInvernoVerao
###############################
# TCC DATA SCIENCE ANALYTICS #
##############################

# VEC PARA POLITICAS ANTICICLICAS DURANTE A COVID-19

setwd("C:/Users/Carolina/OneDrive/MBA USPEsalq/TCC")
options(scipen=999)

### Pacotes ###

library(tidyverse)
library(readxl)
library(BETS)
library(sidrar)
library(mFilter)
library(vars)
library(tseries)
library(tsDyn)
library(seastests)
library(seasonal)

### Coleta de dados: BCB e IBGE ###

# Deflator IPCA (mensal e trimestral)
deflator = read_excel("deflator_ipca.xlsx", sheet="d1")
deflator_tri = deflator %>%
  group_by(date = format(as.yearqtr(date, "%b-%Y"), "%YQ%q")) %>%
  summarise_all(mean)

# PIB trimestral 
pib = get_sidra(api = '/t/1846/n1/all/v/all/p/all/c11255/90707/d/v585%200')
pib = pib %>%
  mutate(date = as.yearqtr(`Trimestre (Código)`, format = "%Y%q")) %>%
  filter(date >= '1998Q1') %>%
  filter(date < '2022Q2')
pib = pib[,c("date","Valor")]

# Deflacionar PIB trimestral
pib = cbind(pib, deflator_tri)
pib$valor = pib$Valor * pib$deflator
pib = pib[,c(1,5)]

# IPCA mensal (% acumulado em 12 meses)
ipca <- get_sidra(api = "/t/1737/n1/all/v/2265/p/all/d/v2265%202")
ipca = ipca %>%
  mutate(date = parse_date(`Mês (Código)`, format="%Y%m")) %>% 
  filter(date >= '1998-01-01')
ipca = ipca[,c("date","Valor")]

# Selic mensal
selic <- BETSget(4189)

# Credito mensal
credito <- BETSget(20539)
credito = credito %>%
  filter(between(date, as.Date('1998-01-01'), as.Date('2022-03-01')))

# Deflacionar crÃ©dito mensal
credito = cbind(credito, deflator)
credito$valor = credito$value * credito$deflator
credito = credito[,c(1,5)]

# Divida liquida interna mensal
divida <- BETSget(4480)
divida = divida %>%
  filter(between(date, as.Date('1998-01-01'), as.Date('2022-03-01')))

# Deflacionar divida liquida mensal
divida = cbind(divida, deflator)
divida$valor = divida$value * divida$deflator
divida = divida[,c(1,5)]

# Gastos com saúde e seguridade social trimestral dessazonalizado
gastos = get_sidra(api = "/t/1846/n1/all/v/all/p/all/c11255/93405/d/v585%200")
gastos = gastos %>%
  mutate(date = as.yearqtr(`Trimestre (Código)`, format = "%Y%q")) %>%
  filter(date >= '1998Q1') %>%
  filter(date < '2022Q2')
gastos = gastos[,c("date","Valor")]

# Deflacionar gastos trimestrais
gastos = cbind(gastos, deflator_tri)
gastos$valor = gastos$Valor * gastos$deflator
gastos = gastos[,c(1,5)]

### Transformar em ts ###

# Janela temporal: jan/1998-dez/2021
# Nivel e diff

pib = ts(pib$valor, start = c(1998,1), end = c(2021,4), frequency = 4) %>% log()
dpib = diff(pib, differences = 1, lag = 1)

ipca = ts(ipca$Valor, start = c(1998,1), end = c(2021,12), frequency = 12)
ipca = aggregate(ipca, nfrequency = 4, mean) %>% log()
dipca = diff(ipca, differences = 1, lag = 1)

selic = ts(selic[140:403], start = c(1998,1), end = c(2021,12), frequency = 12)
selic = aggregate(selic, nfrequency = 4, mean) %>% log()
dselic = diff(selic, differences = 1, lag = 1)

credito = ts(credito$valor, start = c(1998), end = c(2021,12), frequency = 12)
credito = aggregate(credito, nfrequency = 4, mean) %>% log()
dcredito = diff(credito, differences = 1, lag = 1)

divida = ts(divida$valor, start = c(1998,1), end = c(2021,12), frequency = 12)
divida = aggregate(divida, nfrequency = 4, mean) %>% log()
ddivida = diff(divida, differences = 1, lag = 1)

gastos = ts(gastos$valor, start = c(1998,1), end = c(2021,4), frequency = 4) %>% log()
dgastos = diff(gastos, differences = 1, lag = 1)

### Exploracao inicial ###

# PIB: retirar sazonalidade
pib_decompose = decompose(pib)
plot(pib_decompose) # sazonalidade e tendencia ascendente
acf(pib, lag=12) # autocorrelacao residual
isSeasonal(pib, test="combined", freq=4) # sazonalidade

pib = seas(pib, x11 = "")
pib = ts(pib$data[,1], start = c(1998,1), end = c(2021,4), frequency = 4)
dpib = diff(pib, differences = 1, lag=1)

# IPCA
ipca_decompose = decompose(ipca)
plot(ipca_decompose) # tendencia
acf(ipca, lag=12) # autocorrelacao residual
isSeasonal(ipca, test="combined", freq=4) # sem sazonalidade

# Selic
selic_decompose = decompose(selic)
plot(selic_decompose) # sazonalidade e tendencia queda com flutuaÃ§Ãµes
acf(selic, lag=12) # autocorrelacao residual
isSeasonal(selic, test="combined", freq=4) # sem sazonalidade

# Credito: retirar sazonalidade
credito_decompose = decompose(credito)
plot(credito_decompose) # sazonalidade e tendencia ascendente
acf(credito, lag=12) # autocorrelacao residual
isSeasonal(credito, test="combined", freq=4) # sazonalidade

credito = seas(credito, x11 = "")
credito = ts(credito$data[,1], start = c(1998,1), end = c(2021,4), frequency = 4)
dcredito = diff(credito, differences = 1, lag=1)

# Divida
divida_decompose = decompose(divida)
plot(divida_decompose) # sazonalidade e tendencia
acf(divida, lag=12) # autocorrelacao residual
isSeasonal(divida, test="combined", freq=4) # sem sazonalidade

# Gastos: retirar sazonalidade
gastos_decompose = decompose(gastos)
plot(gastos_decompose) # sazonalidade e tendencia ascendente
acf(gastos, lag=12) # autocorrelacao residual
isSeasonal(gastos, test="combined", freq=4) # sazonalidade

gastos = seas(gastos, x11 = "")
gastos = ts(gastos$data[,1], start = c(1998,1), end = c(2021,4), frequency = 4)
dgastos = diff(gastos, differences = 1, lag=1)

# Gráficos
plot(cbind(pib, gastos, divida, ipca, credito, selic),
     main = "Séries em nível")

plot(cbind(dpib, dipca, dselic, dcredito, ddivida, dgastos),
     main="Séries em primeira diferença")

# Regressao Múltipla
ols = lm(pib ~ ipca + credito + gastos + divida + selic)
summary(ols)

### Teste ADF (Ho: há raiz unitaria) ###
# estatistica de teste < valor critico = rejeita Ho = série estacionaria

# Nivel
VARselect(pib)
ur.df(pib, type = "none", lags = 1) %>% summary()  # nao estacionaria

VARselect(ipca)
ur.df(ipca, type = "none", lags = 5) %>% summary() # nao estacionaria

VARselect(selic)
ur.df(selic, type = "none", lags = 1) %>% summary() # nao estacionaria

VARselect(credito)
ur.df(credito, type = "none", lags = 4) %>% summary() # nao estacionaria

VARselect(divida)
ur.df(divida, type = "none", lags = 2) %>% summary() # nao estacionaria

VARselect(gastos)
ur.df(gastos, type = "none", lags = 2) %>% summary() #  nao estacionaria

# Diff
VARselect(dpib)
ur.df(dpib, type = "none", lags = 9) %>% summary() # nao estacionaria

VARselect(dipca)
ur.df(dipca, type = "none", lags = 4) %>% summary() # estacionaria

VARselect(dselic)
ur.df(dselic, type = "none", lags = 1) %>% summary() # estacionaria

VARselect(dcredito)
ur.df(dcredito, type = "none", lags = 3) %>% summary() # nao estacionaria

VARselect(ddivida)
ur.df(ddivida, type = "none", lags = 1) %>% summary() # estacionaria

VARselect(dgastos)
ur.df(dgastos, type = "none", lags = 1) %>% summary() # estacionaria

### Teste KPSS (Ho: serie é estacionaria) ###
# Ho: a série é estacionaria. Se p-valor < 0.05 então não rejeita Ho

#Nivel
kpss.test(pib)
kpss.test(ipca)
kpss.test(selic)
kpss.test(credito)
kpss.test(divida)
kpss.test(gastos)

#Diff
kpss.test(dpib)
kpss.test(dipca)
kpss.test(dselic)
kpss.test(dcredito)
kpss.test(ddivida)
kpss.test(dgastos)

### Teste de cointegração ###

### VEC EM NIVEL ### 

endog <- cbind(pib, ipca, gastos, credito,  divida, selic)

# Seleção de lags
lagselect <- VARselect(endog, lag.max = 10, type = "const")
lagselect$selection

k = 2 # k is the lag order of the series (levels) in the VAR

# Teste Johansen (Traço)
# ver r em que test < valor critico a 5% de signifancia
ca.jo1 = ca.jo(endog, type = "trace", ecdet = "const", K = k, spec = "transitory")
summary(ca.jo1) # r=2

# Teste Johansen (MaxEigen)
ca.jo2 = ca.jo(endog, type = "eigen", ecdet = "const", K = k, spec = "transitory")
summary(ca.jo2) # r=1

r = 1

# Construir modelo VEC
vecm = VECM(endog, lag=k, r=r, estim=("ML"), include = "none", LRinclude = "const")
summary(vecm)


# Tranformar VEC em VAR
vec2var <- vec2var(ca.jo2, r=r)
vec2var # Transformamos em VAR apenas para calcular graficos da IRF

### Teste de autocorrelacao serial ###
# Ho: ausencia de autocorrelacao. Se p-valor > 0,05 não rejeita Ho: sem autocorrelação
serial.test(vec2var, lags.bg = 10, type = "BG") # tem aurocorrel serial
serial.test(vec2var, lags.bg = 10, type = "PT.asymptotic") # sem autocorrel serial


### Causalidade de Granger ###
# Ho: x não causa y. Se p-valor < 0.05 entao rejeita Ho e x causa y
var = VAR(endog, p=2, type="const")
causality(var, cause="pib")$Granger # causality
causality(var, cause="ipca")$Granger
causality(var, cause="gastos")$Granger
causality(var, cause="divida")$Granger
causality(var, cause="credito")$Granger
causality(var, cause="selic")$Granger # causality


### Estimando IRFs ###

irf_1 <- irf(vec2var, 
             impulse = "pib", 
             response = "ipca", 
             n.ahead = 12, 
             boot = TRUE,
             ortho = TRUE)
plot(irf_1, ylab="PIB", main = "Resposta do PIB ao impulso do IPCA")



irf_2 <- irf(vec2var, 
             impulse = "pib", 
             response = "selic", 
             n.ahead = 12, 
             boot = TRUE,
             ortho = TRUE)
plot(irf_2, ylab="PIB", main = "Resposta do PIB ao impulso da Selic")


irf_3 <- irf(vec2var, 
             impulse = "pib", 
             response = "credito", 
             n.ahead = 12, 
             boot = TRUE,
             ortho = TRUE)
plot(irf_3, ylab="PIB", main = "Resposta do PIB ao impulso do crédito")


irf_4 <- irf(vec2var, 
             impulse = "pib", 
             response = "gastos", 
             n.ahead = 12, 
             boot = TRUE,
             ortho = TRUE)
plot(irf_4, ylab="PIB", main = "Resposta do PIB ao impulso dos gastos")


irf_5 <- irf(vec2var, 
             impulse = "pib", 
             response = "divida", 
             n.ahead = 12, 
             boot = TRUE,
             ortho = TRUE)
plot(irf_5, ylab="PIB", main = "Resposta do PIB ao impulso da dívida")

irf_6 <- irf(vec2var, 
             impulse = "pib", 
             response = "pib", 
             n.ahead = 12, 
             boot = TRUE,
             ortho = TRUE)
plot(irf_6, ylab="PIB", main = "Resposta do PIB ao impulso do PIB")

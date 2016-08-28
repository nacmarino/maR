library(metafor)

data <- read.table("08 - vies e nao-independencia/dados/data.txt", header = T, sep = '\t')
head(data)

#Calcular os tamanhos de efeito e as vari?ncias
model_data <- escalc(measure = "ZCOR", ri = data$correlation, ni = data$N, data=data, method="REML")

#Visualiza os tamanhos de efeito e as vari?ncias
head(model_data)

#Gera o ajuste ao modelo selecionado (random effects)
model_data_rma <- rma(yi, vi, data = model_data, method="REML")

#Visualiza os resultados
model_data_rma 

#Transforma os resultados de volta a escala inicial
predict(model_data_rma, transf = transf.ztor)

#Plota um forest plot ranqueado baseado no N
forest(model_data_rma, slab = paste(data$Study), order = order(data$N), transf = transf.ztor, cex = 1)

#Plota um funnel plot
funnel(x = model_data_rma, yaxis = "sei")

#Calculo da signific?ncia da assimetria
ranktest(model_data_rma)

#Trim and Fill
#Ajusta o modelo trim and fill
model_tf <- trimfill(model_data_rma)

#Display results
model_tf

#Transforma os resultados de volta a escala inicial
predict(model_tf, transf = transf.ztor)

#Plota um funnel plot
funnel(model_tf)

#Calculo da signific?ncia da assimetria do modelo original
ranktest(model_data_rma)

#Calculo da signific?ncia da assimetria do modelo trimed and filled
ranktest(model_tf)

#Plota um quantile-quantile plot
qqnorm(model_data_rma, type = "rstandard", pch = 19)

#Fail-safe Number
fail <- fsn(yi = model_data$yi, vi = model_data$vi, type = "Rosenthal", alpha = 0.05)

#Mostra os resultados do fsn
fail

#########################################################
#########MODELOS HIERARQUICOS############################
#########################################################

hier <- read.table("08 - vies e nao-independencia/dados/data_hier.txt", header = T, sep = '\t')

names(hier)

head(hier)

#Calcular os tamanhos de efeito e as vari?ncias
dat <-escalc(measure = "ZCOR", ri = hier$correlation, ni = hier$N, data = hier, method="REML")

#Criar um modelo n?o-hier?rquico
res.dat.NH <- rma(yi = dat$yi, vi, data = dat, method="REML")

#Visualiza os resultados do modelo n?o-hier?rquico
res.dat.NH

#Criando o modelo hier?rquico multivariado
res.dat <- rma.mv(yi = dat$yi, vi, random = ~1|author, data = dat, method = "REML")

#Visualiza os resultados do modelo multivariado
res.dat

###################################
#####MODELO COM MODERADOR##########
###################################

res.mod <- rma(yi = dat$yi, vi, mods = ~ group, data = dat, method = "REML")

res.mod



#Modelo complexo com fator hier?rquico e moderado
res.complex <- rma.mv(yi = dat$yi, vi, random = ~1|author, mods = ~ group, data = dat, method = "REML")
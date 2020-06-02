##-------------------------------
## trabalho: Bruna, Daiane e Luiz
##-------------------------------
## casos acumulados no Brasil
casos = c(1, 1, 1, 2, 2, 2, 2, 3, 7, 13, 19, 25, 25, 34, 52, 
          77, 98, 121, 200, 234, 291, 428, 621, 904, 1128,
          1546, 1891, 2201, 2433, 2915, 3417, 3904, 4256,
          4579, 5717, 6836, 7910, 9056, 10278, 11130, 12056, 
          13717, 15927, 17857, 19638, 20727, 22169, 23430, 25262,
          28320, 30425, 33682, 36599, 38654, 40581, 43079, 45757,
          49429, 52995, 58509, 61888, 66501, 71886, 78162, 85380,
          91299, 96396, 101147, 107780)
t = seq(1:length(casos))
modelo = lm(log(casos)~t)
confint(modelo, level = 0.95)

# plotando o modelo e as bandas de confiança
plot(t, log(casos),pch = 16, ylim = c(0,15),
     main = "Casos acumulados", xlab = "Dias após o 1º")
lines(1.4545+0.1715199*t, col = "blue")
lines(0.9279313+0.1584693*t, col = "gray", lty = 2)
lines(1.9790242+0.1845705*t, col = "gray", lty = 2)
text(60, 2.8, "IC - 95% ", cex =0.8, lty=c(1), col = "darkgray")
##-------------------------------
## regioes

# 2 regiao
(which(log(casos)>1.9790242+0.1845705*t))
abline(v = 22, lty = 2, col = "red")
abline(v = 39, lty = 2, col = "red")

# 3 regiao
(which(log(casos)<1.9790242+0.1845705*t))

#--------
# dummies
q1 = factor(c(rep(0, 21), rep(1, 18), rep(0, 30)))
q2 = factor(c(rep(0, 39), rep(1, 30)))

## dados diarios
infectados =c(1,0,0,1,0,0,0,1,4,6,6,6,0,9,18, 25,21,23,79,34,57,137,193,283,224,418,345,310,
                232,482,502,487,353, 323,1138,1119,1076,1146,1222,852,926,1661,2210,1930,
                1781,1089,1442,1261,1832,3058,2105,3257,2917,2055,1927,2498,2678,3735,3503,
                5514,3379,4613,5385,6276, 7218, 5919,
                5097, 4751, 6633)

# considerando as regioes  
plot(infectados, pch = 16, main = "Novos casos") 
abline(v = 22, col = "red", lty = 2)
abline(v = 40, col = "red", lty = 2)

# modelo linear generalizado 
md = glm(infectados ~ t, family = "poisson")
summary(md)
lines(fitted(md), col = "red")


# modelo linear generalizado com dummies
md = glm(infectados ~ t + q1 + q2, family = "poisson")
summary(md)
lines(fitted(md), col = "blue")

#----------------------
# modelo misto
#library("lme4")
#dummie = c(rep(0, 21), rep(1, 18), rep(2,30))
#m0 = glmer(infectados ~ t +(t | dummie),
#            family = "poisson")
#summary(m0)
#ranef(m0)
#lines(fitted(m0))
#plot(infectados, pch = 16) 
#lines(exp(-1.27+0.253*t[1:21]), col = "red")
#lines(22:39, exp(2.45+0.114*t[22:39]), col = "blue")
#lines(40:69, exp(4.65+0.06*t[40:69]), col = "green")
#lines(fitted(md), col = "gray", lty = 2)
##----------------
## separando os modelos
plot(infectados, pch = 16) 

infectados_01= infectados[1:22]
infectados_02= infectados[23:39]
infectados_03=infectados[40:69]

md1<-glm(infectados_01 ~ t[1:22], family = "poisson")
summary(md1)
a=lines(1:22,fitted(md1), col="blue")

md2<- glm(infectados_02 ~ t[23:39], family = "poisson")
summary(md2)
b=lines(23:39,fitted(md2), col="orange")

md3<- glm(infectados_03 ~ t[40:69], family = "poisson")
summary(md3)
c=lines(40:69,fitted(md3), col="green")

# intervalos
confint(md1)
confint(md2)
confint(md3)
#----------------------------
# Para a população retirada















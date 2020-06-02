#---------------------------------------
## Rotina aula do julio 12/05/2020
#---------------------------------------
casos = c(1, 1, 1, 2, 2, 2, 2, 3, 7, 13, 19, 25, 25, 34, 52, 
          77, 98, 121, 200, 234, 291, 428, 621, 904, 1128,
          1546, 1891, 2201, 2433, 2915, 3417, 3904, 4256,
          4579, 5717, 6836, 7910, 9056, 10278, 11130, 12056, 
          13717, 15927, 17857, 19638, 20727, 22169, 23430, 25262,
          28320, 30425, 33682, 36599, 38654, 40581, 43079, 45757,
          49429, 52995, 58509, 61888, 66501, 71886, 78162, 85380,
          91299, 96396, 101147, 107780, 114715, 125218, 135106,
          145328, 155939, 162699)
t = seq(1:length(casos))

#----------------------------------
## considerando apenas um tau
l_max = max(t) - 5
r2 = c()
tau = c()
lv = c()
for(i in 5:l_max){
  N = max(t)-i
  dummie = c(rep(0, i), rep(1, N))
  modelo = lm(log(casos)~ t + dummie )
  lv = c(lv,sum((fitted(modelo) - log(casos))^2))
  r2 = c(r2, summary(modelo)$r.squared)
  tau = c(tau, min(which(dummie!=0)))}

plot(tau, r2, ylim = c(0,1), type = "l")
abline(v = 16, lty = 2)

plot(tau, (lv), type = "l", ylim = c(0, 100), ylab = "LV")
abline(v = 16, lty = 2)
abline(v = 55, lty = 2)
#----------------------------------
## usando as duas dummies
d1 = c(rep(0, 16), rep(1,59))
d2 = c(rep(0, 54), rep(1, 21))
modelo = lm(log(casos) ~ d1 + d2 + t)
lvd1d2 = sum((fitted(modelo) - log(casos))^2)
abline(h = lvd1d2, col = "red")

plot(log(casos))
lines((fitted(modelo)))
#----------------------------------


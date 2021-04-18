

espectros <- read.table('espectros.txt')

nombres <- read.table('nombres.txt', sep = '\t')  
wavenumber <- read.table('wavenumber.txt')

colnames(espectros) <- t(nombres)

str(nombres)


#Todos estan de a 3 menos el 16 y el 17, que son "@A Room T 4 min 1.CSV" "@A Room T 4 min 2.CSV"
promedios <- matrix(,ncol=15, nrow=1869)
promedios <- as.data.frame(promedios)
str(promedios)
# Primer parte, antes del 16
nombres.prom1 <- vector('character', 5)
for (i in 1:1869){
  for (j in 1:5){
    promedios[i,j] <- (espectros[i,(3*j)-2] + espectros[i,((3*j)-2)+1] + espectros[i,(((3*j)-2)+2)])/3 
   nombres.prom1[j] <- t(nombres)[,3*j]
    
  }
}		
names(promedios)[1:5] <- nombres.prom1


# Segunda parte, el 16 y el 17 
colnames(espectros[,c(16,17)])
colnames(espectros[,18:20])
for(i in 1:1869){
  
  promedios[i,6] <- (espectros[i,16]+espectros[i,17])/2
}

names(promedios)[6] <- t(nombres)[17]

nombres.prom3 <- vector('character', 15)
for (i in 1:1869){
  for (j in 7:15){
    promedios[i,j] <- (espectros[i,(3*j)-3] + espectros[i,((3*j)-3)+1] + espectros[i,(((3*j)-2)+1)])/3 
    nombres.prom3[j] <- t(nombres)[,(3*j)-1]
  }
}		
names(promedios)[7:15] <- nombres.prom3[7:15]
head(promedios)
promedios.t <- t(promedios)
colnames(promedios.t) <- wavenumber[,1]

# Primer parte, antes del 16

rownames(promedios.t) <- sub('.CSV', '', rownames(promedios.t))
promedios.t <- as.data.frame(promedios.t)
write.csv(promedios.t, 'promedios.csv')

color <- 1:15
par(new = F)
for(i in 1:15){
  
  plot(wavenumber[,1],
       promedios.t[i,],
       ylim = c(0,0.3),
       xlim = c(4000,400),
       type = 'l',
       col = color[i],
       ylab = 'Absorbancia u.a.',
       xlab = 'numero de onda cm-1'
       
       
        ) 
par(new=T)
  }

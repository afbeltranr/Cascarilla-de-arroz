nombres <- list.files(pattern = '.CSV')
datos <- lapply(nombres, read.csv, header=FALSE)
wavenumber <- datos[[1]][1]
datos2 <- lapply(datos, "[", 	2) 

names(datos2) <- nombres
datos3 <- as.data.frame(datos2)
colnames(datos3) <- nombres
write.table(datos2, "espectros.txt", sep="\t", col.names = TRUE)
write.table(nombres, "nombres.txt", sep="\t", col.names = TRUE)
write.table(wavenumber, "wavenumber.txt", sep="\t", col.names = TRUE)
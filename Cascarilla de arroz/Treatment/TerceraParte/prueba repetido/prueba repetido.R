nombres <- list.files(, pattern = '.CSV')
data <- lapply(nombres, read.csv, header = FALSE)
wavenumber <- data[[1]][,1]
data2 <- lapply(data, '[', 2)
data2 <- as.data.frame(data2)
datat <- t(data2)
colnames(datat) <- wavenumber
rownames(datat) <- nombres

for(i in 1:4){
  
  plot(wavenumber,
    datat[i,],
    ylim = c(0, 0.015),
    xlim = c(1700,400),
    type = 'l',
    col = c(1,2,2,2))
}

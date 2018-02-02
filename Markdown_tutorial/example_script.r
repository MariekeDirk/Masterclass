## ---- importData ----

ENSO <- rnorm(50, 0, 3)

temp <- rnorm(50, 25, 10)

## ---- calcCor ----

ensoCor <- cor(ENSO, temp, use="complete.obs")

## ---- makePlot ----


aplot <- ggplot(data=data.frame(temp, ENSO)) +
geom_line(aes(x=ENSO, y=temp))



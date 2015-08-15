library(xkcd)
library(ggplot2)
library(extrafont)

download.file("http://simonsoftware.se/other/xkcd.ttf",
              dest="xkcd.ttf", mode="wb")
system("mkdir ~/.fonts")
system("cp xkcd.ttf ~/.fonts")
font_import(pattern = "[X/x]kcd", prompt=FALSE)
fonts()
fonttable()
if(.Platform$OS.type != "unix") {
## Register fonts for Windows bitmap output
loadfonts(device="win")
} else {
loadfonts()
}

val <- sin(seq(from = pi, to = pi*2, length.out = 40))

val <- val * seq(from = 6, to = 0.5, length.out = length(val))

val <- val + runif(length(val),-0.5,0.5)

plot(val)

pl <- data.frame(dribble = val, age = seq(1:40))


output <- ggplot(pl, aes(x = age, y = dribble)) +
  geom_point() +
  xkcdaxis(xrange = range(pl$age), yrange = range(pl$dribble))

output




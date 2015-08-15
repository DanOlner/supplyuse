#These both work
1 + 
  2


1
+ 2

dat <- data.frame(x = seq(1:10), y=runif(10))

#fine
ggplot(dat, aes(x = x, y = y)) +
         geom_point()

#not fine!
ggplot(dat, aes(x = x, y = y)) 
  + geom_point()


#orig minimum wage
mw <- 6.5
#increments through inflation
mwi <- mw
#inflation
inf <- 0.02

for(yr in 2015:2020) {
  
  mwi = mwi + (mwi * inf)
  
}

mwi
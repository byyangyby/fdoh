df <- read.csv('/home/joebrew/Documents/fdoh/public/parker/frLunch.csv')
#df <- read.delim('/home/joebrew/Documents/fdoh/public/school/all_schools_complete.txt')

source('/home/joebrew/Documents/fdoh/public/parker/helpers.R')
df$coordinator_cost <- 100
df$incentive_cost
df$public <- round(0.85 * df$totMem)
df$private <- df$totMem - df$public

df$net_rev60 <- net_rev(number_private = df$private,
                      number_nonprivate = df$public,
                      ir = 100,
                      collaborative = TRUE)

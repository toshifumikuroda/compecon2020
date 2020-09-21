library(pwr)

pwr.p.test(h = ES.h(p1 = 0.75, p2 = 0.50),
           sig.level = 0.05,
           power = 0.80,
           alternative = "greater")
coin <- pwr.p.test(h = ES.h(p1 = 0.75, p2 = 0.50),
                   sig.level = 0.05,
                   power = 0.80,
                   alternative = "greater")
plot(coin)

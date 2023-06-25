## create promo A versus promo B
promo_a <- rnorm(100, mean = 550, sd = 10)
promo_b <- rnorm(100, mean = 400, sd = 8)

## ho: promo a - b = 0
## ha: promo a- b != 0
result <- t.test(promo_a, promo_b, alternative = "two.sided")

ifelse(result$p.value <= 0.05,"sig", "Not Sig")

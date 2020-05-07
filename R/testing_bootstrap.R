library(dplyr)

source('hello.R')

tst <- tibble()

# For k samples size
for (k in c(20,30,40,50,60,80,100,200,300)) {

  pvalues_orig <- vector('numeric')
  pvalues_boot <- vector('numeric')

  bcount <- 0

  for (j in 1:500) {

    n <- k
    # Generate heteroscedastic data
    y <- 1:n
    sd <- runif(n, min = 0, max = 4)
    error <- rnorm(n, 0, sd*y)
    X <- y+error
    df <- data.frame(y, X)
    m <- lm(y ~ X, data = df)

    p.value <- white_test(m)$p_value
    LM <- white_test(m)$w_stat
    pvalues_orig <- c(pvalues_orig, p.value)


    # Bootstrapped White test
    # Paper's methodology

    pvalues_boot <-  c(pvalues_boot, white_test_boot(m)$p_value)

  }

  # final data.frame
  tst <- bind_rows(tst,
                   tibble(
                     sample_size = k,
                     pvalues_boot = pvalues_boot,
                     pvalues = pvalues_orig))

}

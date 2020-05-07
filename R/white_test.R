white_test <- function(model) {

  # Squared residuals of fitted model
  squared_residuals <- model$residuals^2
  # get fitted values
  .fitted <- fitted(model)
  # auxiliary regression
  aux_r_suared <- summary(lm(squared_residuals ~ .fitted + I(.fitted^2)))$r.squared
  # test statistic
  w_stat <- length(.fitted) * aux_r_suared
  # compute p-value
  p_value <- 1 - pchisq(w_stat, 2)

  output <-
    structure(
      list(
        w_stat = w_stat,
        p_value = p_value
      ),
      class = "white_test"
    )

  output
}



white_test_boot <-
  function(model, bootstraps = 1000) {
    if (bootstraps < 10) {
      bootstraps <- 10
      warning("At least 10 bootstrap samples is recommended. Setting 'bootstrap_samples' to 10 even though at least 500 is recommended.")
    }

    # White test with original data
    wt <- white_test(model)


    # Bootstrapped method
    # Paper: Bootstrapped White’s test for heteroskedasticity in regression models
    # Jinook Jeong, Kyoungwoo Lee
    bcount <- 0
    wsb <- vector(mode = 'numeric')
    for (i in seq_len(bootstraps)) {
      .fitted <- fitted(model)
      var_res <- summary(model)$sigma^2
      bootstrapped_error <- var_res * rnorm(n = length(.fitted), mean = 0, sd = 1)

      new_y <- model$model[[1]] + bootstrapped_error
      aux_m <- lm(as.formula(gsub(".*~","new_y ~", format(model$call$formula))), data = model$model)
      aux_r_suared <- summary(lm(aux_m$residuals^2 ~ fitted(aux_m) + I(fitted(aux_m)^2)))$r.squared
      white_stat_b <- length(new_y) * aux_r_suared

      # compute p-value
      if(abs(white_stat_b) >= abs(wt$w_stat)) {
        bcount <- bcount + 1
      }

      wsb[i] <- white_stat_b
    }


    output <-
      structure(
        list(
          w_stat = wsb,
          p_value = bcount/1000,
          iters = bootstraps
        ),
        class = "white_test"
      )

    output

  }



print.white_test <- function(x, ...) {
  if(length(x$w_stat) == 1) {
    cat("White's test results\n",
        paste0("Null hypothesis: Homoskedasticity of the residuals"),
        paste0("Alternative hypothesis: Heteroskedasticity of the residuals"),
        paste0("Test Statistic: ", round(x$w_stat, 2)),
        paste0("P-value: ", round(x$p_value, 6)),
        sep = "\n")
  } else {
    x$w_stat <- ifelse(length(x$w_stat) <= 10,
                       paste0(round(x$w_stat, 2), collapse = ", "),
                       paste0(paste0(round(x$w_stat[1:10], 2), collapse = ", "), ",..."))

    cat("Bootstrapped White's test results\n",
        paste0("Null hypothesis: Homoskedasticity of the residuals"),
        paste0("Alternative hypothesis: Heteroskedasticity of the residuals"),
        paste0("Number of bootstrap samples: ", x$iters),
        paste0("Boostrapped Test Statistic: ", x$w_stat),
        paste0("P-value: ", round(x$p_value, 6)),
        sep = "\n")
  }

}

plot.white_test <-
  function(x, ...) {
    hist(x$w_stat,
         breaks = "FD",
         col = "peachpuff",
         border = "black",
         prob = TRUE,
         xlab = "Test Statistic",
         main = paste0("Bootstrapped White's test (bootstrap samples = ", x$iters, ")"))
    lines(density(x$w_stat),
          lwd = 2,
          col = "chocolate3")
  }




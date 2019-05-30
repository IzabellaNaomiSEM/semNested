
semNested <- function(fit1, fit2, model1, model2){
  # Test whether fit1 and fit2 are lavaan-objects, else stop.
  if((is(fit1, 'lavaan') == FALSE) | (is(fit2, 'lavaan') == FALSE)) {
    stop("Fit1 and/or fit2 are not Lavaan objects. Cannot use this function.")
  }

  # Compute N1 and N2
  N1 <- lavInspect(fit1, what = "ntotal")
  N2 <- lavInspect(fit2, what = "ntotal")

  # Compute DF1 and DF2
  DF1 <- fitMeasures(fit1)['df']
  DF2 <- fitMeasures(fit2)['df']

  # Compute Sigma1 and Sigma2
  Sigma1 <- inspect(fit1, "sigma")
  Sigma2 <- inspect(fit2, "sigma")

  # Compute fit based on Sigma other model
  fit1_2 <- cfa(model1, sample.cov = Sigma2, sample.nobs = N1)
  fit2_1 <- cfa(model2, sample.cov = Sigma1, sample.nobs = N2)

  # Obtain Chi-statistic for fit1_2 and fit2_1, and compare to 0
  round(lavInspect(fit1_2, what = "test")[[1]]$stat, 5) == 0
  round(lavInspect(fit2_1, what = "test")[[1]]$stat, 5) == 0

  # Test whether model is saturated
  if(DF1 == 0){
    message <- "Model 1 is likely a saturated model. All models are nested in saturated models."
  } else if (DF2 == 0) {
    message <- "Model 2 is likely a saturated model. All models are nested in saturated models."
  }

  # Test whether fit1_2 and fit2_1 have a perfect fit. Is not: models are likely not nested
  if(round(lavInspect(fit1_2, what = "test")[[1]]$stat, 5) == 0 | round(lavInspect(fit2_1, what = "test")[[1]]$stat, 5) == 0){
    DF1 - DF2
    } else {
      message <- "The models are likely not nested in each other."
    }

  # Test which model is more complex (fewer DF), test which model is nested in the other.
  if((DF1 - DF2) == 0) {
    message <- "The models are likely equal to each other.  Likely, neither model is nested in the other."
  } else if ((DF1- DF2) > 0 ) {
    message <- "Model 1 is likely nested in model 2."
  } else if ((DF1 - DF2) < 0) {
    message <- "Model 2 is likely nested in model 1."
  }

  # Make plots of fit1 and fit2
  par(mfrow=c(2,1))
  semPaths(fit1, style = "lisrel", pastel = T,
           groups = "latents", what = "est",
           edge.label.cex = 1, fade = F,
           layout = "tree2", sizeMan = 2.5, sizeLat = 4,
           asize = 4, esize = 5, label.cex = 1.5)
  title(sub = "Model 1")

  semPaths(fit2, style = "lisrel", pastel = T,
           groups = "latents", what = "est",
           edge.label.cex = 1, fade = F,
           layout = "tree2", sizeMan = 2.5, sizeLat = 4,
           asize = 4, esize = 5, label.cex = 1.5)
  title(sub= "Model 2")

  # Output Table
  # Output Chi-squares fit1_2 and fit2_1
  C1_2 <- round(lavInspect(fit1_2, what = "test")[[1]]$stat, 5)
  C2_1 <- round(lavInspect(fit2_1, what = "test")[[1]]$stat, 5)

  # Output P-values fit1_2 and fit2_1
  p1_2 <- lavInspect(fit2_1, what = "test")[[1]]$pvalue
  p2_1 <- lavInspect(fit1_2, what = "test")[[1]]$pvalue


  # Make Output List with DF, Chi-square, and p-value for both models based on Sigma of other model
  Output <- list("Conclusion" = message, "DF1" = DF1, "DF2" = DF2, "X1_2" = C1_2,
                 "X2_1" = C2_1, "P1_2" = p1_2, "P2_1" = p2_1)

  class(Output) <- c('semNested','list')

  cat(bold("---Output semNested---"), "\n",  "\n",
    underline("Main conclusion"), "\n",
    Output$Conclusion, "\t", "\n", "\n",
    underline("Test statistics"), "\n",
    "The degrees of freedom of model 1 are", Output$DF1, "\t", "\n",
    "The degrees of freedom of model 2 are", Output$DF2, "\t", "\n",
    "The Chi-statistic of model 1 based on model 2 is", Output$X1_2, Output$P1_2, ",", "\t", "\n",
    "The Chi-statistic of model 2 based on model 1 is", Output$X2_1, Output$P2_1, ",", "\t", "\n", "\n",
    cyan("Please view the plots to compare model 1 and model 2"),"\n","\n")
}


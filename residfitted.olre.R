# Paul Johnson
# University of Glasgow
# 27 July 2016

# R function to output Pearson residuals and fitted values from
# a Poisson-lognormal GLMM (i.e. a Poisson GLMM with an observation-level 
# random effect or OLRE) model object fitted using lme4::glmer.
# The usual method of residuals and fitted values using the functions stats::residuals 
# and stats::fitted treat the OLRE as a random effect rather than part of the 
# Poisson-lognormal error distribution. This is problematic when using a Pearson 
# residuals v fitted values plot to assess the fit of the model, using e.g. the plot.merMod
# method for plot (see examples). Further justification here:
# https://stat.ethz.ch/pipermail/r-sig-mixed-models/2013q3/020770.html
# The function is adapted from a function I wrote on the same thread:
# https://stat.ethz.ch/pipermail/r-sig-mixed-models/2013q3/020817.html

# Value: returns a data frame with the columns "Fitted" and "PearsonResiduals"

residfitted.olre <- 
  function(object) {
    require(AICcmodavg)
    require(lme4)
    response<-model.frame(object)[[1]]
    n <- length(response)
    re <- lme4::ranef(object)
    re.length <- sapply(re, nrow)
    od.term <- names(re)[re.length == n]
    pois.log.norm <-
      length(od.term) == 1 && fam.link.mer(object)$family == "poisson" && fam.link.mer(object)$link == "log"
    if(!pois.log.norm) {
      warning("Fitted model is not Poisson-lognormal. Using stats::residuals and stats::fitted.")
      f <- fitted(object)
      r <- residuals(object, type = "pearson")
    } 
    if(pois.log.norm) {
      od.ranef <- re[[od.term]][[1]]
      f <- exp(log(fitted(object)) - od.ranef)
      r <- 
        (response - f) / sqrt(f + (f^2) * c(exp(lme4::VarCorr(object)[[od.term]]) - 1))
    }
    return(data.frame(Fitted = f, PearsonResiduals = r))
  }

# function to make an x-y scatter plot and add a LOESS line

plot.loess <-
  function(x, y = NULL, ...)
  {
    if(is.data.frame(x) && ncol(x) == 2) {
      plot.data <- x
      xy.labels <- names(plot.data)
      names(plot.data) <- c("x", "y")
    } else {
      plot.data <- data.frame(x = x, y = y)
      xy.labels <- names(plot.data)
    } 
    plot.data$loess.line <- predict(loess(y ~ x, data = plot.data, ...))
    plot.data <- plot.data[order(plot.data$x), ]
    plot(plot.data[,c("x", "y")], xlab = xy.labels[1], ylab = xy.labels[2])
    abline(h = 0)
    points(plot.data[,c("x", "loess.line")], type="l", col="red")
  }



# examples

if(F) {

  library(lme4)    
  library(glmmADMB)
  
  # fit three models to the grouse ticks count data (see ?grouseticks)
  # first, a Poisson GLMM that doesn't account for overdispersion
  form0 <- TICKS ~ YEAR + scale(HEIGHT) + (1 | BROOD) + (1 | LOCATION)
  fit.pois  <- glmer(form0, family = "poisson", data = grouseticks)

  # second, model overdispersion by adding an OLRE: "+ (1 | INDEX)"
  # giving a Poisson lognormal GLMM (see http://www.ncbi.nlm.nih.gov/pubmed/11393830)
  # also see Xavier Harrison's article on this: https://peerj.com/articles/616/
  form1 <- TICKS ~ YEAR + scale(HEIGHT) + (1 | BROOD) + (1 | LOCATION) + (1 | INDEX)
  fit.poisln  <- glmer(form1, family = "poisson", data = grouseticks)

  # finally, model overdispersion with a negative binomial distribution
  fit.nb <- glmmadmb(form0, family = "nbinom", data=grouseticks)
  
  # calculate Pearson residuals and fitted values
  residfitted.olre(fit.pois)
  residfitted.olre(fit.nb)
  residfitted.olre(fit.poisln)
  # if the model isn't a Poisson-lognormal GLMM the standard stats functions are used 
  # with a warning.

  # both the Poisson-lognormal and the NB fit much better than the Poisson,
  # and the NB fits slightly better than the Poisson-lognormal  
  AIC(fit.pois, fit.poisln, fit.nb)
  
  # assess the fit of the model by plotting Pearson residuals against fitted values
  par(mfrow = c(2, 2))
  plot.loess(x = fitted(fit.pois), y = residuals(fit.pois, type = "pearson"))
  title("Poisson GLMM residuals v fitted plot")
  plot.loess(x = fitted(fit.poisln), y = residuals(fit.poisln, type = "pearson"))
  title("Poisson-lognormal GLMM residuals v fitted plot")
  plot.loess(x = fitted(fit.nb), y = residuals(fit.nb, type = "pearson"))
  title("Negative binomial GLMM residuals v fitted plot")
  
  # the standard residuals v fitted plots look fine for the Poisson & NB fits, 
  # but the Poisson-lognormal plot shows a nasty trend and severe heteroscedasticity.
  # this is because the overdispersion effects are included in the fitted values.
  # to be comparable with the negative binomial GLMM plot, the residuals need to be 
  # subtracted from the fitted values and added to the residuals. this is what the
  # residfitted.olre function does:
  plot.loess(residfitted.olre(fit.poisln))
  title("Corrected Poisson-lognormal GLMM residuals v fitted plot")
  
  # now the residuals v fitted plot from the Poisson-lognormal GLMM can be
  # assessed alongside the other two models using the usual criteria: linearity
  # and homoscedasticity. the residuals from the NB and Poisson-lognormal distributions
  # look very similar, which isn't surprising because each is a Poisson distribution 
  # with added (really mutliplied) gamma- and lognormal-distributed noise 
  # respectively, and gamma and lognormal distributions can be quite similar.


}

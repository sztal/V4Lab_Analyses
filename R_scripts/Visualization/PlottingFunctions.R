#############################################
### Plotting tools for the V4 Lab project ###
#############################################

### !!! <--- Function 1 ---> !!! ### (START)
#######################################################################
### Compute Agresti-Coull confidence interval for a binary variable ###
#######################################################################
acInterval <- function(vec, alpha = 0.05, n = NULL, x = NULL) {
      ### This function take 4 arguments:
      ###   - vec : a vector of numerical binary data
      ###   - alpha : significance level
      ###   - n : if not NULL it indicates the sample size
      ###   - x : if not null it indicates the number of successes
      ### In general if vec is not NULL then n and p must be NULL and vice versa
      
      ### Check the input arguments
      stopifnot(is.numeric(vec) | is.null(vec),
                all(levels(factor(vec, ordered = TRUE)) == c("0", "1")),
                is.null(n) | (is.numeric(n) & length(n) == 1),
                is.null(x) | (is.numeric(x) & length(x) == 1),
                is.numeric(alpha) & (alpha > 0 | alpha < 1))
      if(is.null(vec) & is.null(x) & is.null(n)) stop(message = "No input data given")
      if(!is.null(vec) & !is.null(x) & !is.null(n)) stop(message = "Give a vector or params, not both")
      if(is.null(vec) & (is.null(n) | is.null(x))) stop(message = "Give both p and n params")
      
      ### Compute the z parameter
      z <- qnorm(1 - alpha/2)
      ### Choose the computation approach
      if(!is.null(vec)) {
            n <- length(vec)
            x <- sum(vec)
      }
      ndash <- n + z^2
      pdash <- (1/ndash) * (x + .5 * z^2)
      wing <- z * sqrt((1/ndash) * pdash * (1 - pdash))
      CI <- pdash + c(-1, 1) * wing
      return(CI)
}
### !!! <--- Function 1 ---> !!! ### (END)


### !!! <--- Function 2 ---> !!! ### (START)
###############################################################
### Plot binary varibales with/without confidence intervals ###
###############################################################
plotBinary <- function(data, ci=TRUE, reverse=FALSE, reorder=TRUE, vline=FALSE, alpha=.05, theme = NULL) {
      ### This function takes 5 arguments:
      ###   - datac : a data.frame with the variables
      ###   - ci : flag indicating wheter confidence intervals should plotted (Agresti-Coull)
      ###   - reverse : flag indicating whether 1 - p should be returned instead of p
      ###   - reorder : flag indicating whether variables should be reordered
      ###   - vline : flag indicating whether a vertical line at 0.5 should be plotted
      ###   - alpha : significance level for the confidence interval
            
      ### Check the input data
      stopifnot(is.data.frame(data),
                is.logical(ci), is.logical(reverse), is.logical(reorder), is.logical(vline),
                is.null(theme) | is.list(theme))
      
      ### Load lattice and latticeExtra packages
      stopifnot(library(lattice, logical.return=TRUE),
                library(latticeExtra, logical.return=TRUE))
      
      ### Prepare data
      vars <- names(data)
      p <- apply(data, 2, mean, na.rm=TRUE)
      lo <- vector(mode="numeric", length=length(vars))
      up <- vector(mode="numeric", length=length(vars))
      pdat <- data.frame(vars=vars, p=p, lo=lo, up=up)
      rownames(pdat) <- vars
      if(reverse) pdat[, "p"] <- 1 - pdat[, "p"]
      n <- dim(data)[1]
      ### Get A-C intervals
      for(var in vars) {
            p <- pdat[var, "p"]
            x <- p*n
            cint <- acInterval(vec=NULL, alpha=alpha, n=n, x=x)
            veclo <- cint[1]
            vecup <- cint[2]
            pdat[var, "lo"] <- veclo
            pdat[var, "up"] <- vecup
      }
      if(reorder) dplot <- dotplot(reorder(vars, p) ~ p, data = pdat)
      else dplot <- dotplot(vars ~ p, data = pdat)
      if(!is.null(theme)) dplot <- update(dplot, par.settings=theme)
      if(ci) {
            dplot <- update(dplot, lo=pdat$lo, up=pdat$up,
                            panel=function(x, y, lo, up, ...) {
                                  panel.dotplot(x, y, ...)
                                  panel.arrows(x0=lo, y0=y,
                                               x1=up, y1=y, code=3,
                                               angle=90, length=0.05)
                            })
      }
      if(vline) dplot <- dplot + layer(panel.abline(v=0.5, lwd=.8, lty=2))
      if(reverse) dplot <- update(dplot, xlab = "Probability of a Failure")
      else dplot <- update(dplot, xlab = "Probability of a Success")
      return(dplot)
}
### !!! <--- Function 2 ---> !!! ### (END)
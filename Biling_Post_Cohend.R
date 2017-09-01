
HDI <- function(x, y, target = .95) {
  dx <- diff(x)
  areas <- dx * .5 * (head(y,-1) + tail(y, -1))
  peak <- which.max(areas)
  range <- c(peak, peak)
  found <- areas[peak]
  while(found < target) {
    if(areas[range[1]-1] > areas[range[2]+1]) {
      range[1] <- range[1]-1
      found <- found + areas[range[1]-1]
    } else {
      range[2] <- range[2]+1
      found <- found + areas[range[2]+1]
    }
  }
  val<-x[range]
  attr(val, "indexes")<-range
  attr(val, "area")<-found
  return(val)
}

##################
##################
Post.d <- function(t, n1, n2 = NA){
options(warn = -1)
         N = ifelse(is.na(n2), n1, (n1 * n2)/(n1 + n2))
        df = ifelse(is.na(n2), n1 - 1, (n1 + n2) - 2)
  
     prior = function(x) dnorm(x)
likelihood = function(x) dt(t, df, x*sqrt(N))
  marginal = integrate(function(x) prior(x)*likelihood(x), -Inf, Inf)[[1]]
 posterior = function(x) prior(x)*likelihood(x) / marginal
  
mode = optimize(posterior, interval = c(-3, 3), maximum = TRUE, tol = 1e-12)[[1]]
  
G = function(){
  par(xpd = TRUE, mgp = c(2, .2, 0)) 
post = curve(posterior, -1, 3, n = 1e4, lwd = 2, ylab = NA, axes = FALSE, 
      xlab = "Population Effect Size", font.lab = 2, yaxs = "i", bty = "n")$y

axis(1, at = c(-1, -.5, 0, 2, 2.5, 3), font = 2)
axis(1, at = mode, lab = round(mode, 3), col = 2, col.axis = 2, font = 2)
    a = seq(-1, 3, l = 1e4)
    b = posterior(a)
    CI = HDI(a, b)
    segments(mode, 0, mode, max(post), col = 2, lty = 2)
    segments(CI[1], 0, CI[2], 0, lwd = 4, lend = 1, col = "magenta")
    points(mode, 0, pch = 21, bg = "cyan", cex = 1.6, col = "magenta")
    text(CI, 0, round(CI, 3), col = "magenta", font = 2, pos = 3)
}

library(ReporteRs)

doc = addPlot(docx(), fun = G, vector.graphic = TRUE, width = 5, height = 4,  
            par.properties = parCenter(), editable = TRUE)

writeDoc(doc, file = "Biling_Cohendrealexample.docx" )
}
Post.d(t = 5.03, n1 = 24)       
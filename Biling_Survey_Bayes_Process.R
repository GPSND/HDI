     prior = function(x) dbeta(x, 15.56689246, 7.051444) 
likelihood = function(x) dbinom(55, 100, x)
 posterior = function(x) prior(x)*likelihood(x)

G <- function(){
 par(mfcol = c(1, 3), mgp = c(2, .1, 0), mar = c(5, .3, 15, .1), xpd = NA, yaxt = "n", bty = "n", yaxs = "i", lwd = 2, las = 1, font.lab = 2, font = 2)
 
 Prior = curve(prior, n = 1e4, xlab = "Proportion of \"B\"", ylab = NA, xaxt = "n")$y
 mtext("\u00D7", 4, line = 1, cex = 3, font = 1)
 text(.7, max(Prior), "Prior", pos = 3)
 axis(1, at = axTicks(1), lab = paste0(axTicks(1)*1e2, "%"))
 
 
 Likelihood = curve(likelihood, n = 1e4, xlab = "Proportion of \"B\"", ylab = NA, xaxt = "n")$y
 mtext("=", 4, line = 1, cex = 3, font = 1)
 text(.55, max(Likelihood), "Likelihood", pos = 3)
 axis(1, at = c(0, .2, .4, .7, .85, 1), labels = paste0(c(0, .2, .4, .7, .85, 1)*1e2, "%"))
 axis(1, at = .55, labels = paste0(55, "%"), col = 2, col.axis = 2)
 
 
 Posterior = curve(posterior, n = 1e4, xlab = "Proportion of \"B\"", ylab = NA, xaxt = "n")$y
 text(.58, max(Posterior), "Posterior", pos = 3)
 axis(1, at = c(0, .2, .4, .7, .85, 1), labels = paste0(c(0, .2, .4, .7, .85, 1)*1e2, "%"))
 axis(1, at = mode, labels = paste0(round(mode, 4)*1e2, "%"), col = 2, col.axis = 2 )
 segments(CI[1], 0, CI[2], 0, lend = 1, lwd = 3, col = "magenta", xpd = NA)
 points(mode, 0, pch = 19, cex = 1.5, col = "magenta", xpd = NA)
 }

library(ReporteRs)

doc = addPlot(docx(), fun = G, vector.graphic = TRUE, width = 6, height = 4,  
           par.properties = parCenter(), editable = TRUE)

writeDoc(doc, file = "Biling_Bayes_Process.docx" )




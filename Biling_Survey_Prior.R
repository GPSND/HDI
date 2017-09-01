
#G = function(){
C = c(15.56689246, 7.051444)
par(mgp = c(2, .2, 0))
curve(dbeta(x, C[1], C[2]), n = 1e4, axes = FALSE, lwd = 2, yaxs = "i", xpd = TRUE, 
          xlab = "Proportion of preference for (B)", ylab = NA, font.lab = 2)
axis(1, at = axTicks(1), labels = paste0(axTicks(1)*1e2, "%") )
x = seq(.6, .8, l = 10)
y = dbeta(x, C[1], C[2])
arrows(x, 0, x, y, code = 2, length = .1, angle = 13, col = 4, lend = 1)
x = c(seq(0, .577, l = 28), seq(.822, 1, l = 10))
y = dbeta(x, C[1], C[2])
arrows(x, 0, x, y, code = 1, length = .1, angle = 13, col = 2, lend = 1)
#}

library(ReporteRs)

doc = addPlot(docx(), fun = G, vector.graphic = TRUE, width = 5, height = 4,  
              par.properties = parCenter(), editable = TRUE)

writeDoc(doc, file = "Binom_Prior.docx" )
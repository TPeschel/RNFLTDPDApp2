###
# Tests 2020 10 16
# Was funktioniert bereits
###
rm(list = ls())

source("R/pdf/pdf.2.0.R")

#fn <- "visitor/franziska_neu/LI02737058_all_follow_ups.pdf"
fn <- "visitor/franziska_20201020/LI05134278_recent_follow_ups_alle_Haken_gesetzt.pdf"
fn <- "visitor/franziska_20201020/LI05134278_single_exam_report_margins_spacefnotes_comments.pdf"

(rws <- PDF.TOOLS$xtrct.rows(text = PDF.TOOLS$get.text(pdf.filename = fn)))

rws[1:6]
obs <- list()
pat <- list()

(obs$hdr <- paste0(rws[[1]], collapse = " "))
(obs$dev <- paste0(rws[[2]], collapse = " "))

(name.pos <- PDF.TOOLS$xtrct.text(rows = rws[1:5], pattern = "Patient:"))
(dob.pos <- PDF.TOOLS$xtrct.text(rows = rws[1:5], pattern = "DOB:"))

pat$name <- paste0(rws[[3]][(1 + grep("Patient:", rws[[3]])) : (grep("DOB:", rws[[3]]) - 1)], collapse = " ")

pat$id <- if(0 < length(ex <- grep("Exam", rws[[4]]))){
	paste0(rws[[4]][(1 + grep("ID:", rws[[4]])) : ex[1]], collapse = " ")
}else {
	paste0(rws[[4]][(1 + grep("ID:", rws[[4]])) : length(rws[[4]])], collapse = " ")
}


(pat$name <- rws[[name.pos$row]][[3]])
(dob.txt <- rws[[dob.pos$row]][[dob.pos$col + 1]])


(pat$dob <- PDF.TOOLS$interprete.date.string(dob.txt))

(doe.pos <- PDF.TOOLS$xtrct.text(rows = rws[3:5], pattern = "Exam"))
(doe.txt <- rws[[2 + doe.pos$row]][[doe.pos$col + 1]])
(obs$doe <- PDF.TOOLS$interprete.date.string(doe.txt))

pat$age <- as.double(( obs$doe - pat$dob ) / 365.25)



(mm <- PDF.TOOLS$xtrct.text(rows = rws, pattern = "^.*mm.*"))

for(r in unique(mm$row)) cat(r, ": ", paste0(rws[[r]], collapse = " | "), "\n")

(dob <- PDF.TOOLS$xtrct.text(rows = rws, pattern = "^.*DOB.*"))

for(r in unique(dob$row)) cat(r, ": ", paste0(rws[[r]], collapse = " | "), "\n")

(birthday <- rws[[dob$row[1]]][[dob$col[1]+1]])

PDF.TOOLS$interprete.date.string(birthday)



PDF.TOOLS$xtrct.text(rows = rws, pattern = "\\[µm\\]", row = 40)

PDF.TOOLS$xtrct.text(rows = rws, pattern = "\\[µm\\]", row = 1, col = -Inf, len = 2)

PDF.TOOLS$xtrct.text(
	rows      = rws
	,pattern  = "\\[µm\\]"
	,fun      = function( txt, ... ) paste0( gsub( "[\\(\\) +]", "", txt ), ... )
	,sep      = ""
	,collapse = " "
)

PDF.TOOLS$xtrct.text( rws, "Sex:", 1, -Inf, Inf, function( txt, ... ) paste0( txt, ... ), sep = "", collapse = " " )
PDF.TOOLS$xtrct.text( rws, "DOB:", 1, 2, 1 )
PDF.TOOLS$xtrct.text( rws, "Patient:", 1, 2, 2 )
PDF.TOOLS$xtrct.text( rws, "Sex:", 1, 1 )
PDF.TOOLS$xtrct.text( rws, "Sex:", 1, 2, 1 )



gfx <- PDF.TOOLS$xtrct.gfx( fn, dpi = 300, rgba.sel = 1 : 3 )

par( mfrow = c( 1, 1 ) )
plot( c(0,1),c(0,1))
rasterImage( as.raster( gfx[ seq.int( 1, dim( gfx )[ 1 ], 3 ), seq.int( 1, dim( gfx )[ 2 ], 5 ), ], max = 1 ), 0, 0, 1, 1 )

plot.os <- GFX.TOOLS$cut.rect(
	gfx  = gfx,
	ymin = 1. - .45,  # .55,
	ymax = 1. - .375, #.625,
	xmin = .105,
	xmax = .410 )

plot.od <- GFX.TOOLS$cut.rect(
	gfx  = gfx,
	ymin = 1. - .45,  # .55,
	ymax = 1. - .375, #.625,
	xmin = 1. - .410,
	xmax = 1. - .105 )

par( mfrow = c( 1, 2 ) )
plot( c(0,1),c(0,1))
rasterImage( as.raster( plot.os[ , , ], max = 1 ), 0, 0, 1, 1 )
plot( c(0,1),c(0,1))
rasterImage( as.raster( plot.od[ , , ], max = 1 ), 0, 0, 1, 1 )

grad.y.os <- GFX.TOOLS$gradient$y( plot.os, kernel = c( -1. / 510., 0, +1. / 510. ), improve = F )
grad.x.os <- GFX.TOOLS$gradient$x( plot.os, kernel = c( -1. / 510., 0, +1. / 510. ), improve = F )
grad.os   <- GFX.TOOLS$gradient$xy( plot.os, kernel.x = c( -1. / 510., 0, +1. / 510. ), kernel.y = c( -1. / 510., 0, +1. / 510. ), improve.x = F, improve.y = F )
grad.y.od <- GFX.TOOLS$gradient$y( plot.od, kernel = c( -1. / 510., 0, +1. / 510. ), improve = F )
grad.x.od <- GFX.TOOLS$gradient$x( plot.od, kernel = c( -1. / 510., 0, +1. / 510. ), improve = F )
grad.od   <- GFX.TOOLS$gradient$xy( plot.od, kernel.x = c( -1. / 510., 0, +1. / 510. ), kernel.y = c( -1. / 510., 0, +1. / 510. ), improve.x = F, improve.y = F )
c.os      <- GFX.TOOLS$combine.gfx.and.gradient( plot.os, grad.os )
c.od      <- GFX.TOOLS$combine.gfx.and.gradient( plot.od, grad.od )

par( mfrow = c( 2, 2 ) )
plot( c(0,1),c(0,1))
rasterImage( as.raster( 1 + grad.y.os[ , , ], max = 2 ), 0, 0, 1, 1, interpolate = F )
plot( c(0,1),c(0,1))
rasterImage( as.raster( 1 + grad.y.od[ , , ], max = 2 ), 0, 0, 1, 1, interpolate = F )
plot( c(0,1),c(0,1))
rasterImage( as.raster( 1 + grad.x.os[ , , ], max = 2 ), 0, 0, 1, 1, interpolate = F )
plot( c(0,1),c(0,1))
rasterImage( as.raster( 1 + grad.x.od[ , , ], max = 2 ), 0, 0, 1, 1, interpolate = F )

par( mfrow = c( 2, 2 ) )
plot( c(0,1),c(0,1))
rasterImage( as.raster( plot.os[ , , ], max = 1 ), 0, 0, 1, 1, interpolate = F )
plot( c(0,1),c(0,1))
rasterImage( as.raster( plot.od[ , , ], max = 1 ), 0, 0, 1, 1, interpolate = F )
plot( c(0,1),c(0,1))
rasterImage( as.raster( 1 + grad.os[ , , ], max = 2 ), 0, 0, 1, 1, interpolate = F )
plot( c(0,1),c(0,1))
rasterImage( as.raster( 1 + grad.od[ , , ], max = 2 ), 0, 0, 1, 1, interpolate = F )

par( mfrow = c( 1, 2 ) )
plot( c(0,1),c(0,1))
rasterImage( as.raster( 1 + c.os[ , , ], max = 2 ), 0, 0, 1, 1, interpolate = F )
plot( c(0,1),c(0,1))
rasterImage( as.raster( 1 + c.od[ , , ], max = 2 ), 0, 0, 1, 1, interpolate = F )

grad.y.os <- GFX.TOOLS$gradient$y( plot.os, kernel = c( -1. / 2., 0, +1. / 2. ), improve = T )
grad.x.os <- GFX.TOOLS$gradient$x( plot.os, kernel = c( -1. / 2., 0, +1. / 2. ), improve = T )
grad.os   <- GFX.TOOLS$gradient$xy( plot.os, kernel.x = c( -1. / 2., 0, +1. / 2. ), kernel.y = c( -1. / 2., 0, +1. / 2. ), improve.x = T, improve.y = T )
grad.y.od <- GFX.TOOLS$gradient$y( plot.od, kernel = c( -1. / 2., 0, +1. / 2. ), improve = T )
grad.x.od <- GFX.TOOLS$gradient$x( plot.od, kernel = c( -1. / 2., 0, +1. / 2. ), improve = T )
grad.od   <- GFX.TOOLS$gradient$xy( plot.od, kernel.x = c( -1. / 2., 0, +1. / 2. ), kernel.y = c( -1. / 2., 0, +1. / 2. ), improve.x = T, improve.y = T )

#dev.off()
par( mfrow = c( 3, 2 ) )
plot( c(0,1),c(0,1))
rasterImage( as.raster( grad.y.os[ , , ], max = 1 ), 0, 0, 1, 1 )
plot( c(0,1),c(0,1))
rasterImage( as.raster( grad.y.od[ , , ], max = 1 ), 0, 0, 1, 1 )
plot( c(0,1),c(0,1))
rasterImage( as.raster( grad.x.os[ , , ], max = 1 ), 0, 0, 1, 1 )
plot( c(0,1),c(0,1))
rasterImage( as.raster( grad.x.od[ , , ], max = 1 ), 0, 0, 1, 1 )
plot( c(0,1),c(0,1))
rasterImage( as.raster( grad.os[ , , ], max = 1 ), 0, 0, 1, 1 )
plot( c(0,1),c(0,1))
rasterImage( as.raster( grad.od[ , , ], max = 1 ), 0, 0, 1, 1 )

par( mfrow = c( 1, 2 ) )
plot( c(0,1),c(0,1))
rasterImage( as.raster( grad.od[ , , ], max = 1), 0, 0, 1, 1 )
plot( c(0,1),c(0,1))
rasterImage( as.raster( grad.os[ , , ], max = 1), 0, 0, 1, 1 )


#PDF.TOOLS$xtrct.graph( rgb,  )
rows <- PDF.TOOLS$xtrct.rows(
	PDF.TOOLS$get.text("visitor/LI00454692_RNFLT.pdf")
)

gfx <- PDF.TOOLS$xtrct.gfx("visitor/LI00454692_RNFLT.pdf")


library(ggplot2)
library(png)
library(grid)
img <- readPNG(system.file("img", "Rlogo.png", package="png"))
g <- rasterGrob(img, interpolate=TRUE)

ggplot(geom="blank") +
	geom_raster(aes(x,y),data=g)

par(mfrow=c(1,1))
library(plotly)
#m <- matrix(hcl(0, 80, seq(50, 80, 10)), nrow = 4, ncol = 5)
m <- matrix((1:81)%%2, nrow = 9, ncol = 9)[1:8,1:8]
(r <- as.raster(m))
plot(r)
plot_ly() %>%
	add_heatmap(x=1:8,y=1:8,m)# %>%
	# layout(images = list(
	# 	source = raster2uri(r), # converts a raster object to a data URI.
	# 	xref = "x", yref = "y", x = 0, y = 0, sizex = 1, sizey = 1,
	# 	sizing = "stretch", xanchor = "left", yanchor = "bottom"
	# ))

###
# Tests 2020 10 16
# Was funktioniert bereits
###
rm(list = ls())

source("R/pdf/pdf.2.0.R")
source("R/pdf/spectralis/pdf_spectralis.R")

(path.pdfs <- "../RNFLTDPDADATA/spectralis_pdfs/")

(all.pdfs <- dir(path.pdfs, "pdf", recursive = T))

all.pats <- Reduce(rbind.data.frame,lapply(
	all.pdfs,
	function(a) {
		
		print(a)
		#a <- "Overview Report/LI00592436_Sehnerv2012_.pdf"
		Spectralis$xtrct.obs.info(a)
	}
))

View(all.pats)

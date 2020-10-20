rm(list = ls())

source("R/pdf/pdf.2.0.R")

(path.project <- getwd())

(path.pdfs <- paste0(path.project, "/visitor"))

(path.target <- paste0(path.project, "/spectralis_pdfs"))

(paths.to.all.pdfs <- dir(path = path.pdfs, pattern = "pdf$", recursive = T))

for(fn in paths.to.all.pdfs) {

	#dbg
	#(fn <- paths.to.all.pdfs[[1]])
	
	f <- paste0(path.pdfs, "/", fn)
	
	e <- try(PDF.TOOLS$get.info(f))
	
	if(!inherits(e, "try-error")){
	
		rws <- PDF.TOOLS$xtrct.rows(
			text = PDF.TOOLS$get.text(
				pdf.filename = f))
	
		if(0 < length(rws)) {
		
			#dir.name <- paste0(sub("-", "_", sub("[, ]", "", rws[[1]])), collapse = "_")
			dir.name <- paste0(rws[[1]], collapse = " ")
			
			dir.name <- paste0(path.target, "/", dir.name)
			
			if(!dir.exists(dir.name)) dir.create(dir.name, recursive = T)
			
			file.copy(f, dir.name)
		}
	}
}

setwd(path.project)

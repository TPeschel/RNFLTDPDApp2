Spectralis <- structure(
	list(
		"General" = structure(
			list(
				obs_info = function(a) {
					
					rows <- PDF.TOOLS$xtrct.rows(PDF.TOOLS$get.text(paste0(path.pdfs, "/", a)))
					
					name <- paste0(rows[[3]][2 : (grep("DOB:", rows[[3]]) - 1)], collapse = " ")
					dob  <- rows[[3]][(grep("DOB:", rows[[3]]) + 1)]
					sex  <- rows[[3]][(grep("Sex:", rows[[3]]) + 1)]
					
					id   <- if(0 < length(grep("ID:",  rows[[4]]))) rows[[4]][(grep("ID:",  rows[[4]]) + 1)] else NA
					doe  <- if(0 < length(grep("Exam", rows[[4]]))) rows[[4]][(grep("Exam", rows[[4]]) + 1)] else NA
					
					dia  <- if(0 < length(grep("Diadgno", rows[[5]]))) rows[[5]][(grep("Diagnos", rows[[5]]) + 1)] else NA
					com  <- if(0 < length(grep("Comment", rows[[5]]))) rows[[5]][(grep("Comment", rows[[5]]) + 1)] else NA
					
					db <- PDF.TOOLS$xtrct.text(rows, "database")
					db <- if(0 < nrow(db)) paste0(rows[[db$row[1]]][(db$col[1] + 1) : length(rows[[db$row[1]]])], collapse = " ") else NA
					
					sw <- PDF.TOOLS$xtrct.text(rows, "Version")
					sw <- if(0 < nrow(sw)) rows[[sw$row[1]]][[sw$col[1] + 1]] else NA
					
					oc <- PDF.TOOLS$xtrct.text(rows[1:4], "O[SD]")
					oc <- if(0 < nrow(oc)) rows[[oc$row[1]]][[oc$col[1]]] else NA
					
					data.table::data.table(
						OBS = paste0(rows[[1]], collapse = " "),
						PAT = name,
						ID  = id,
						SEX = sex,
						DOB = dob,
						DOE = doe,
						OCU = oc,
						DIA = dia,
						COM = com,
						DEV = paste0(rows[[2]], collapse = " "),
						SW  = sw,
						DB  = db
					)
				}
			),
			class = "General"
		),
		"Glaucoma Overview" = structure(
			list(
				obs_info = function(a) {
					
					#Spectralis$General$obs_info(a)
				}
			),
			class = "Glaucoma Overview"
		),
		"Minimum Rim Width & RNFL Analysis Single Exam Report" = structure(
			list(
				obs_info = function(a) {
					
					#Spectralis$General$obs_info(a)
				}
			),
			class = "Minimum Rim Width & RNFL Analysis Single Exam Report"
		),
		"Overview Report" = structure(
			list(
				obs_info = function(a) {
					
					#Spectralis$General$obs_info(a)
				}
			),
			class = "Overview Report"
		),
		"RNFL & Asymmetry Analysis Single Exam Report" = structure(
			list(
				obs_info = function(a) {
					
					#Spectralis$General$obs_info(a)
				}
			),
			class = "RNFL & Asymmetry Analysis Single Exam Report"
		),
		"RNFL Change Report, All Follow-Ups" = structure(
			list(
				obs_info = function(a) {
					
					#Spectralis$General$obs_info(a)
				}
			),
			class = "RNFL Change Report, All Follow-Ups"
		),
		"RNFL Change Report, Recent Follow-Ups" = structure(
			list(
				obs_info = function(a) {
					
					#Spectralis$General$obs_info(a)
				}
			),
			class = "RNFL Change Report, Recent Follow-Ups"
		),
		"RNFL Single Exam Report OU" = structure(
			list(
				obs_info = function(a) {
					
					#Spectralis$General$obs_info(a)
				}
			),
			class = "RNFL Single Exam Report OU"
		),
		"RNFL Trend Report" = structure(
			list(
				obs_info = function(a) {
					
					#Spectralis$General$obs_info(a)
				}
			),
			class = "RNFL Trend Report"
		),
		obs_info = function(a) {
			
			obs_info_gen <- Spectralis$General$obs_info(a)
			
			obs_info_spec <- Spectralis[[obs_info_gen$OBS]]$obs_info(a)
			
			cbind.data.frame(obs_info_gen, obs_info_spec)
		}
	),
	class = "Spectralis"
)

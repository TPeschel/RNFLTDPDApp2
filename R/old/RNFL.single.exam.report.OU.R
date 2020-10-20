rm( list = ls( ) )

if( ! exists( "pdf.sourced" ) ) source( "pdf.R" )

RNFLSingleExamReportOU <- list(
	
	ID   = "RNFLSingleExamReportOU",
	
	TXT  = function( filename ) xtrct.lines( pdf_text( filename ) ),
	
	GFX  = function( filename ) render.pdf( filename ),
	
	INFO = function( txt ) {
		
		line_      <- txt[[ grep( "Patient:", txt ) ]]
		
		name.pos   <- grep( "Patient:", line_ )
		dob.pos    <- grep( "DOB:", line_ )
		sex.pos    <- grep( "Sex:", line_ )
		
		name       <- Reduce( paste, line_[ ( name.pos + 1 ) : ( dob.pos - 1 ) ] )
		dob.t      <- line_[ dob.pos + 1 ]
		dob        <- interprete.date.string( dob.t )
		sex        <- line_[ sex.pos + 1 ]
		
		line_      <- txt[[ grep( "Exam.:", txt ) ]]
		doe.pos    <- grep( "Exam.:", line_ )
		doe.t      <- line_[ doe.pos + 1 ]
		doe        <- interprete.date.string( doe.t )
		
		line_      <- txt[[ grep( "database:", txt ) ]]
		ref.db.pos <- grep( "database:", line_ )
		ref.db     <- Reduce( paste, line_ [ ( ref.db.pos + 1 ) : length( line_ ) ] )
		
		doc        <- Reduce( paste, txt[[ 1 ]] )
		
		meas       <- Reduce( paste, txt[[ 2 ]] )
		
		line_      <- grep( "Software", txt )
		
		sw         <- Reduce( paste, txt[[ line_ ]] )
		
		data.frame(
			DOC      = doc,
			SOFTWARE = sw,
			MEAS     = meas,
			DOE.TXT  = doe.t,
			DOE      = doe,
			NAME     = name,
			DOB.TXT  = dob.t,
			DOB      = dob,
			SEX      = sex,
			REF.DF   = ref.db
		)
	},
	
	PLOTS = function( gfx, txt ) {
		
		lapply(
			1 : length( pdf.types[[ "RNFLSingleExamReportOU" ]] [[ "PLOTS" ]] ),
			function( i ) {
			
				#delete!
				#txt = text
				#i = 1
				
				pt <- pdf.types[[ "RNFLSingleExamReportOU" ]] [[ "PLOTS" ]] [[ i ]]
				
				percs <- xtrct.percentiles( 
					remove.horizontal.lines( 
						compute.gradient( 
							xtrct.graph( 
								gfx, 
								pt[[ "GRAPH.RECT.SCAN" ]] 
							),
							improve = T
						)
					),
					pt
				)
				
				curve <- xtrct.measurement( 
					xtrct.graph(
						gfx,
						pt[[ "GRAPH.RECT.SCAN" ]] 
					),
					pt
				)
				
				# line_   <- grep( "\\bOD\\b|\\bOS\\b", txt, perl = T )
				# oc      <- txt[[ line_[ 1 ] ]] [ grep( "\\bOD\\b|\\bOS\\b", txt[[ line_[ 1 ] ]], perl = T ) ]
				oc      <- c( "OD", "OS" )[ i ]

				line_   <- txt[[ grep( "DOB:", txt ) ]]
				dob.pos <- grep( "DOB:", line_ )
				dob.t   <- line_[ dob.pos + 1 ]
				dob     <- interprete.date.string( dob.t )
				
				line_   <- txt[[ grep( "Exam.:", txt ) ]]
				doe.pos <- grep( "Exam.:", line_ )
				doe.t   <- line_[ doe.pos + 1 ]
				doe     <- interprete.date.string( doe.t )
				
				list(
					INFO = data.frame(
						
						DOC.TYPE  = Reduce( paste, txt[[ 1 ]] ),
						MEAS.TYPE = Reduce( paste, txt[[ 2 ]] ),
						NAME      = pt[[ "ID" ]],
						OCULUS    = oc,
						AGE       = round( as.numeric( doe - dob ) / 365.25, 1 )
					),
					PERCS = percs,
					CURVE = curve
				)
			}
		)
	}
)

Analyzers <- list(
	RNFLSingleExamReportOU = RNFLSingleExamReportOU
)

( path.pdfs <- "visitor/allpdfs/" )

( filenames.pdfs <- get.names.of.all.pdfs.in.directory( path.pdfs ) )

( headers.pdfs <- xtrct.headers( filenames.pdfs, path.pdfs ) )

( headers.pdfs <- headers.pdfs[ ! is.na( headers.pdfs$header ), ] )

( headers.pdfs <- headers.pdfs[ headers.pdfs$header == "RNFLSingleExamReportOU", ] )
# 
#( afn <- absolut.file.name( headers.pdfs$path, headers.pdfs$fname ) )

#delete!
#headers.pdfs <- headers.pdfs[ grep( "lena_1980", headers.pdfs$fname ), ]

for( hdr.id in seq_len(nrow( headers.pdfs ) ) ) {

	current.header          <- headers.pdfs[ hdr.id, ]
	current.pdf.analyzer.id <- as.character( current.header[[ "header" ]] )
	current.pdf.analyzer    <- Analyzers[[ current.pdf.analyzer.id ]] 
	current.pdf.type        <- pdf.types[[ current.pdf.analyzer.id ]]

	fn <- absolut.file.name( current.header[[ "path" ]], current.header[[ "fname" ]] )
	
	print( fn )
	( text <- current.pdf.analyzer$TXT( fn ) )
	
	print( info <- current.pdf.analyzer$INFO( text ) )
	( gfx  <- current.pdf.analyzer$GFX( fn ) )
	( plots <- current.pdf.analyzer$PLOTS( gfx = gfx, txt = text ) )
	
	par( mfrow = c( 1, length( plots ) ) )
	
	for( i in 1 : length( plots ) ) {
	
		p <- plots[[ i ]]
		d <- p$PERCS
		c <- p$CURVE
		i. <- p$INFO
		
		ylab <- Reduce( paste, text[[ grep( c( "\\[µm\\]" ), text )[ 2 ] ]] )
		
		rect.real <- current.pdf.type [[ "PLOTS" ]] [[ i ]] [[ "GRAPH.RECT.REAL" ]]
		
		plot(  
			d$X768$angle, 
			d$X768$r95.meas, 
			col  = "green", 
			type = 'l',
			xlab = "Position°",
			ylab = ylab,
			xlim = c( 
				rect.real [[ "X.MIN" ]],
				rect.real [[ "X.MAX" ]] 
			),
			ylim = c( 
				rect.real [[ "Y.MIN" ]],
				rect.real [[ "Y.MAX" ]] 
			)
		)
		lines( d$X768$angle, d$X768$r05.meas, col = "yellow" )
		lines( d$X768$angle, d$X768$r01.meas, col = "red" )
		lines( c$X768$angle, c$X768$crv.meas, col = "black" )
		title( Reduce( paste, i.[ 3 : length( i. ) ] ) )
	}
	
	par( mfrow = c( 1, 1 ) )
}

rm( list = ls( ) )

if( ! exists( "pdf.sourced" ) ) source( "pdf.R" )

GlaucomaOverview <- list(
	
	ID   = "GlaucomaOverview",
	
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
	
	PLOTS = function( gfx, txt, pdf.type.id = "GlaucomaOverview" ) {
		#txt<-text
		lapply(
			pdf.types[[ pdf.type.id ]] [[ "PLOTS" ]],
			function( pt ) {
				#pt <- pdf.types[[ "GlaucomaOverview" ]] [[ "PLOTS" ]][[ 1]]
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
				
				line_   <- grep( "\\bOD\\b|\\bOS\\b", txt, perl = T )
				oc      <- txt[[ line_[ 1 ] ]] [ grep( "\\bOD\\b|\\bOS\\b", txt[[ line_[ 1 ] ]], perl = T ) ]

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

( path.pdfs <- "visitor/allpdfs/" )

( filenames.pdfs <- get.names.of.all.pdfs.in.directory( path.pdfs ) )

( headers.pdfs <- xtrct.headers( filenames.pdfs, path.pdfs ) )

( headers.pdfs <- headers.pdfs[ ! is.na( headers.pdfs$header ), ] )

( headers.pdfs <- headers.pdfs[ headers.pdfs$header == "GlaucomaOverview", ] )

#( afn <- absolut.file.name( headers.pdfs$path, headers.pdfs$fname ) )

for( i in 1 : length( headers.pdfs ) ) {

	hdr <- headers.pdfs[ i, ]
	
	current.pdf.analyzer <- GlaucomaOverview
	current.pdf.type     <- pdf.types[[ "GlaucomaOverview" ]]

	fn <- absolut.file.name( hdr[[ "path" ]], hdr[[ "fname" ]] )
	
	print( fn )
	( text <- current.pdf.analyzer$TXT( fn ) )
	
	pt <- pdf.types[[ current.pdf.analyzer$ID ]]
	
	print( info <- current.pdf.analyzer$INFO( text ) )
	( gfx  <- current.pdf.analyzer$GFX( fn ) )
	( plots <- current.pdf.analyzer$PLOTS( gfx = gfx, txt = text ) )

	par( mfrow = c( 1, length( plots ) ) )
	
	for( i in 1 : length( plots ) ) {

		pi <- pt$PLOTS[[ i ]]
		
		p  <- plots[[ i ]]
		d  <- p$PERCS
		c  <- p$CURVE
		i. <- p$INFO
		
		#ylab <- Reduce( paste, text[[ grep( c( "RNFL|Thickness", "Minimum|Rim|Width" )[ i ], text )[ 1 ] ]] )
		ylab <- pi$YLAB.FUN( text )
		
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

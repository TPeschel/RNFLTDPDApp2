rm( list = ls( ) )

if( ! exists( "pdf.sourced" ) ) source( "pdf.R" )

RNFLChangeReportAllFollowUps <- list(
	
	ID   = "RNFLChangeReport,AllFollow-Ups",
	
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
		
		line_      <- txt[[ grep( "Baseline", txt ) ]]
		doe1.pos   <- grep( "Baseline", line_ )
		doe1.t     <- line_[ doe1.pos + 1 ]
		doe1       <- interprete.date.string( doe1.t )
		
		line_      <- txt[[ grep( "Follow-Up.*#", txt ) ]]
		doe2.pos   <- grep( "#", line_ )
		doe2.t     <- line_[ doe2.pos + 1 ]
		doe2       <- interprete.date.string( doe2.t )
		
		line_      <- txt[[ grep( "database:", txt ) ]]
		ref.db.pos <- grep( "database:", line_ )
		ref.db     <- Reduce( paste, line_ [ ( ref.db.pos + 1 ) : length( line_ ) ] )
		
		doc        <- Reduce( paste, txt[[ 1 ]] )
		
		meas       <- Reduce( paste, txt[[ 2 ]] )
		
		line_      <- grep( "Software", txt )
		
		sw         <- Reduce( paste, txt[[ line_ ]] )
		
		data.frame(
			DOC       = doc,
			SOFTWARE  = sw,
			MEAS      = meas,
			DOE.TXT   = doe2.t,
			DOE       = doe2,
			DOE.B.TXT = doe1.t,
			DOE.B     = doe1,
			NAME      = name,
			DOB.TXT   = dob.t,
			DOB       = dob,
			SEX       = sex,
			REF.DF    = ref.db
		)
	},
	
	PLOTS = function( gfx, txt ) {
		
		lapply(
			1 : length( pdf.types[[ "RNFLChangeReport,AllFollow-Ups" ]] [[ "PLOTS" ]] ),
			function( i ) {
				
				#dbg
				#i <- 1
				
				pt <- pdf.types[[ "RNFLChangeReport,AllFollow-Ups" ]] [[ "PLOTS" ]][[ i ]]
				
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
				
				line_      <- txt[[ grep( c( "Baseline", "Follow-Up.*#" )[ i ], txt ) ]]
				doe.pos    <- grep( c( "Baseline", "#" )[ i ], line_ )
				doe.t      <- line_[ doe.pos + 1 ]
				doe        <- interprete.date.string( doe.t )

				list(
					# INFO = data.frame(
					# 	
					# 	DOC.TYPE  = Reduce( paste, txt[[ 1 ]] ),
					# 	MEAS.TYPE = Reduce( paste, txt[[ 2 ]] ),
					# 	NAME      = pt[[ "NAME" ]],
					# 	OCULUS    = oc,
					# 	AGE       = round( as.numeric( doe - dob ) / 365.25, 1 )
					# ),
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

( headers.pdfs <- headers.pdfs[ headers.pdfs$header == "RNFLChangeReport,AllFollow-Ups", ] )

( afn <- absolut.file.name( headers.pdfs$path, headers.pdfs$fname ) )

current.pdf.analyzer <- RNFLChangeReportAllFollowUps
current.pdf.type <- pdf.types$`RNFLChangeReport,AllFollow-Ups`

for( fn in afn ) {
	
	#delete!
	#fn <- afn[ 1 ]
	
	current.pdf.analyzer$ID
	
	print( fn )
	( text <- current.pdf.analyzer$TXT( fn ) )
	
	print( info <- current.pdf.analyzer$INFO( text ) )
	( gfx  <- current.pdf.analyzer$GFX( fn ) )
	( plots <- current.pdf.analyzer$PLOTS( gfx = gfx, txt = text ) )
	
	par( mfrow = c( 1, length( plots ) ) )
	
	for( i in seq_len( length( plots ) ) ) {
		
		p <- plots[[ i ]]
		d <- p$PERCS
		c <- p$CURVE
		i. <- p$INFO
		
		ylab <- Reduce( paste, text[[ grep( c( "\\[µm\\]" ), text )[ i ] ]] )
		
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

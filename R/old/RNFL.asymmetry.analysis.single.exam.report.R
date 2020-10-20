rm( list = ls( ) )

if( ! exists( "pdf.sourced" ) ) source( "pdf.R" )

RNFLAsymmetryAnalysisSingleExamReport <- list(
	
	ID   = "RNFL&AsymmetryAnalysisSingleExamReport",
	
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
			pdf.types[[ "RNFL&AsymmetryAnalysisSingleExamReport" ]] [[ "PLOTS" ]],
			function( pt ) {
				
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
						NAME      = pt[[ "NAME" ]],
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

( fn <- dir( "visitor/allpdfs/", ".*Asymmetry.*.pdf" ) )
( afn <- absolut.file.name( "visitor/allpdfs/", fn ) )

current.pdf.analyzer <- RNFLAsymmetryAnalysisSingleExamReport
current.pdf.type <- pdf.types$`RNFL&AsymmetryAnalysisSingleExamReport`

for( fn in afn ) {
	
	#delete!
	#fn <- afn[ 1 ]
	
	current.pdf.analyzer$ID
	
	print( fn )
	( text <- current.pdf.analyzer$TXT( fn ) )
	
	print( info <- current.pdf.analyzer$INFO( text ) )
	( gfx  <- current.pdf.analyzer$GFX( fn ) )
	( plots <- current.pdf.analyzer$PLOTS( gfx = gfx, txt = text ) )
	
	par( mfrow = c( 1, 1 ) )
	
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


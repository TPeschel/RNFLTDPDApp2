#rm( list = ls( ) )

library( "pdftools" )
#library( "Rcpp" )

LIST <- function( ..., pre = NULL, post = NULL ) {
	
	l <- as.list( ... )
	
	if( is.null( names( l ) ) )
		
		names( l ) <- paste0( pre, l, post )
	
	names( l )[ nchar( names( l ) ) == 0 ] <- paste0( pre, l[ nchar( names( l ) ) == 0 ], post )
	
	l
}

absolut.file.name <- function( path, file.name ) {
	
	path <- as.character( path )

	file.name <- as.character( file.name )
	
	#delete!
	# path = headers.pdfs$path
	# file.name = headers.pdfs$fname
	
	f <- substr( path, nchar( path ), nchar( path ) ) != "/"

	path[ f ] <- paste0( path[ f ], "/" )
	
	paste0( path, file.name )
}

GFX.TOOLS <- structure(
	
	list(
		
		#' cut.rect
		#'
		#' @param gfx a rgb graphic extracted by xtrct.rgb
		#' @param first.row min row where to cut
		#' @param last.row  max row where to cut
		#' @param first.col min col where to cut
		#' @param last.col  max col where to cut
		#' @param rgba.sel  channels
		#'
		#' @return rgb array of required dimensions
		cut.rect    = function( gfx, xmin, xmax, ymin, ymax, rgba.sel = c( 1 : 3 ) ) {
			
			y0 <- ymin * dim( gfx )[ 1 ]
			y1 <- ymax * dim( gfx )[ 1 ]
			x0 <- xmin * dim( gfx )[ 2 ]
			x1 <- xmax * dim( gfx )[ 2 ]
			
			gfx[ y0 : y1, x0 : x1, rgba.sel ]
		},
		
		gradient    = list(
			
			x = grad.x<-function( gfx, kernel = c( -1, 0, 1 ), improve = T ) { 
				
				k.len <- length( kernel )
				
				y.len <- dim( gfx )[ 1 ]
				
				x.len <- dim( gfx )[ 2 ]
				
				rgba  <- array( data = as.numeric( gfx ), dim = dim( gfx ) )
				
				grad <- kernel[ 1 ] * rgba[ , 1 : ( x.len + 1 - k.len ), ]
				
				if( 1 < k.len )
					for( i in 2 : k.len ) 
						grad <- grad + kernel[ i ] * rgba[ , i : ( x.len + i - k.len ), ]
				
				if( improve == T )  grad <- ifelse( 0 < abs( grad ), 1, 0 )
				
				grad
			},
			y = grad.y<-function( gfx, kernel = c( -1, 0, 1 ), improve = T ) { 
				
				k.len <- length( kernel )
				
				y.len <- dim( gfx )[ 1 ]

				x.len <- dim( gfx )[ 2 ]
				
				rgba  <- array( data = as.numeric( gfx ), dim = dim( gfx ) )
				
				grad <- kernel[ 1 ] *  rgba[ 1 : ( y.len - 2 ), , ]
				
				if( 1 < k.len )
					for( i in 2 : k.len ) 
						grad <- grad + kernel[ i ] *  rgba[ i : ( y.len + i - k.len ), , ]
				
				if( improve == T ) grad <- ifelse( 0 < abs( grad ), 1, 0 )
				
				grad
			},
			xy = grad.xy<-function( gfx, kernel.x = c( -1, 0, 1 ), kernel.y = c( -1, 0, 1 ), improve.x = T, improve.y = T ) { 
				
				g.x <- grad.x( gfx, kernel.x, improve.x )
				g.y <- grad.y( gfx, kernel.y, improve.y )
				d   <- sapply( 1 : 3, function( i ) min( dim( g.x )[ i ], dim( g.y )[ i ] ) )
				
				.5 * ( g.x[ 1 : d[ 1 ], 1 : d[ 2 ], 1 : d[ 3 ] ] + g.y[ 1 : d[ 1 ], 1 : d[ 2 ], 1 : d[ 3 ] ] )
			}
		),
		combine.gfx.and.gradient = function( gfx, grad ) {
			
			rgba <- array( data = as.numeric( gfx ), dim = dim( gfx ) )
			
			d    <- dim( grad )
			
			rgba[ 2 : ( d[ 1 ] + 1 ), 2 : ( d[ 2 ] + 1 ), ] / 255 * grad[ , , ] 
		}
	),
	class = "GFX.TOOLS"
)

GRAPH.PROPERTIES <- structure(
	
	list(
		
		DESCRIPTION = "Properties for finding a certain graph NEAR: assumed position and size of the graph in percent",
		NEAR        = list(
			y = .5,
			X = .5,
			H = .5,
			W = .5
		)
	),
	
	class = "GRAPH.PROPERTIES"
)

( PDF.TOOLS <- structure( 
	list(
		DESCRIPTION = "A set of tools for extracting data from a pdf.",
		
		#' xtrct.gfx
		#'
		#' @param pdf.filename 
		#' @param dpi 
		#'
		#' @return a rgba array if required
		xtrct.gfx   = function( pdf.filename, dpi = 1000, rgba.sel = c( 1 : 3 ), dims.perm = c( 3, 2, 1 ) ) {
			
			( aperm(
				pdf_render_page(
					pdf.filename,
					dpi = dpi
				),
				perm = dims.perm,
			) )[ , , rgba.sel ]
		},
		
		#' get.text
		#'
		#' @param pdf.filefname filename of pdf file
		#'
		#' @return a character vector of length 1
		get.text    = function( pdf.filename ) {
			
			pdftools::pdf_text( pdf.filename )
		},
		
		#' get.info
		#'
		#' @param pdf.filename 
		#'
		#' @return pdf list with pdf info
		get.info    = function( pdf.filename ) {
		
			pdftools::pdf_info( pdf.filename )
		},

		#' xtrct.rows
		#'
		#' @param text text extracted from pdf with pdf_text( ) 
		#'
		#' @return a list of character vectors
		xtrct.rows  = function( text ) {
	
			rows <- stringr::str_extract_all( text, ".*\n" )
			
			rows <-
				lapply(
					rows[[ 1 ]],
					function( l ) {
						
						s                <- strsplit( l, " +" )
						s                <- s[[ 1 ]][ 0 < nchar( s[[ 1 ]] ) ]
						sl               <- s[ length( s ) ]
						s[ length( s ) ] <- substr( sl, 1, nchar( sl ) - 1 )
						s
					}
				)
			
			rows
		},
		
		#' xtrct.text
		#'
		#' @param rows list of rows extracted with xtrc.rows
		#' @param pattern a pattern that helps to find the correct rows and cols
		#' @param row the rows where the character vectors start
		#' @param col the cols where the character vectors start
		#' @param len the lengths of the character vectors
		#' @param fun a function for combining the elements of the text vectors
		#' @param ... additional arguments for fun
		#'
		#' @return a character vector of lenght 1
		xtrct.text  = function( rows, pattern ) {
			
			# delete!
			# rows <- xtrct.lines( pdf_text( "visitor/allpdfs/CP_OD_Glaucoma Overview.pdf" ) )
			
			rows.ids <- grep( pattern = pattern, x = rows )
			
			if( length( rows.ids ) < 1) return( data.frame( row = integer( 0 ), col = integer( 0 ), off = integer( 0 ), len = integer( 0 ), txt = character( 0 ) ) )
			
			return(
				as.data.frame(drop = FALSE,
					Reduce(
						rbind.data.frame, 
						lapply(
							rows.ids,
							function( r ) {
						
								#dbg
								#r <- rows.ids[[ 3 ]]
								c <- grep( pattern, rows[[ r ]] )
								
								Reduce(rbind.data.frame, lapply(
									c,
									function( c. ){
									
										#dbg
										#c. <- c[[ 1 ]]
										i <- regexpr( pattern, rows[[ r ]][[ c. ]] )
										ii <- as.integer( i )
										
										list(
											row = r, 
											col = c.,
											off = ii,
											len = as.integer( attr( i, "match.length" ) ), 
											txt = substr( rows[[ r ]][[ c. ]], ii, ii + as.integer( attr( i, "match.length" ) ) - 1 )
										)
									}
								))
							}
						)
					)
				)
			)
		},
		
		#' interprete.date.string
		#'
		#' @param text 
		#'
		#' @return NULL if not interpetation is possible a lubridate::dmy if not
		interprete.date.string = function( text ) {
			
			suppressWarnings({
			#dbg text = birthday
			
			if( identical( text, character( 0 ) ) ) return( NULL )
			
			d <- lubridate::dmy( text, locale = "de_DE.utf8" )
			
			if( is.na( d ) ) {
				
				d<-lubridate::dmy( text, locale = "it_IT.utf8" )
			} 
			
			if( is.na( d ) ) {
				
				d<-lubridate::dmy( text, locale = "en_EN.utf8" )
			}
			})
			
			d
		},
		
		#' xtrct.string
		#'
		#' @param rows list of rows extracted with xtrc.rows
		#' @param pattern a pattern that helps to find the correct rows and cols
		#' @param row the rows where the character vectors start
		#' @param col the cols where the character vectors start
		#' @param len the lengths of the character vectors
		#' @param fun a function for combining the elements of the text vectors
		#' @param ... additional arguments for fun
		#'
		#' @return a character vector of lenght 1
		xtrct.string  = function( rows, pattern = "Name:", row = NULL, col = NULL, len = NULL, fun = paste, ... ) {
			
			# delete!
			# rows <- xtrct.lines( pdf_text( "visitor/allpdfs/CP_OD_Glaucoma Overview.pdf" ) )
			
			rows.ids <- grep( pattern = pattern, x = rows )
			
			if( length( rows.ids ) < 1) return( NULL )
			
			if( is.null( row ) ) row <- 1
			
			row <- row[ 1 ]
			
			if( length( rows ) < row ) row <- length( rows )
			
			if( row < 1 ) row <- 1
			
			text <- rows[[ rows.ids[ row ] ]]
			
			if( is.null( col ) ) col <- 1
			
			col <- col[ 1 ] + grep( pattern = pattern, x = text )[ 1 ] - 1
			
			if( col < 0 ) col <- 1
			
			if( is.null( len ) || ( length( text ) - col + 1 < len ) ) len <- length( text ) - col + 1
			
			len <- len[ 1 ]
			
			text <- text[ col : ( col + len - 1 ) ]
			
			#			Reduce( fun, text, ... )
			
			fun( text, ... )
		}
	),
	class = "PDF.TOOLS"
) )



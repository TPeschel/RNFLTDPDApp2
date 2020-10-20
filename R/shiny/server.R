source( "pdf.R" )

server <- function(input, output, session) {
	onStop( function( ) { if( file.exists( "tmp/tmp.pdf" ) ) file.remove( "tmp/tmp.pdf" ) } )
	
	rv <- reactiveValues( )
	
	rv$visitor <- NULL
	
	rv$sex     <- "unknown"
	rv$birth   <- Sys.Date( )
	rv$exam    <- Sys.Date( ) + 20
	rv$age     <- 20
	
	output$pdfview <- renderUI( {
		tags$iframe( style = "height:500px; width:100%", src = "tmp/upload-pdf.pdf" )
	} )
	
	observeEvent(
		eventExpr = input$file_input,
		handlerExpr = {
			
			withProgress(
				{
					incProgress( 1, "Uploading...", detail = "and copying..." )
					
					file.copy( input$file_input$datapath, "tmp/tmp.pdf", overwrite = T )
					
					incProgress( 1, "Render pdf...", detail = "from copy" )
					
					output$pdfview <-
						renderUI( {
							tags$iframe( style = paste0( "height:", screen.height( ) - y.offset - 20, "px; width:100%" ), src = "tmp/tmp.pdf" )
						} )
					
					incProgress( 1, "Analyse pdf...", detail = "extract plot" )
					
					rv$visitor <- xtrct.plot.from.pdf( "tmp/tmp.pdf" )
					
					incProgress( 1, "Analyse pdf...", detail = "extract dates" )
					
					d <- xtrct.text.from.pdf( "tmp/tmp.pdf" )
					
					output$idText <- renderText( paste0( "Pat ID: ", rv$id <- d [[ "id" ]] ) )
					
					output$birthText <- renderText( paste0( "Birth: ", rv$birth <- d [[ "birth" ]] ) )
					
					output$examText  <- renderText( paste0( "Exam: ", rv$exam  <- d [[ "exam" ]] ) )
					
					output$ageText <- renderText( paste0(   "Age: ", rv$age <- d [[ "age" ]] ) )
					
					output$sexText <- renderText( paste0(   "Sex: ", rv$sex <- d [[ "sex" ]] ) )
					
					#write.csv2( rv$visitor, file = paste0( "data/visitor_", d [[ "id" ]], "_", d [[ "exam" ]], ".csv" ) )
					
					output$table_diffs <- DT::renderDataTable( {
						rv$visitor
					},
					extensions = 'Buttons',
					server  = F,
					options = tableOptions
					)
					
					incProgress( 1, "Analyse pdf...", detail = "finished" )
					
				},
				min = 0, max = 5, value = 0, message = "Load And Analyse PDF"
			)
		}
	)
}

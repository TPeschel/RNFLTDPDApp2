library( "dplyr" )
library( "DT" )
library( "markdown" )
library( "plotly" )
library( "rhandsontable" )
library( "shiny" )
library( "shinycssloaders" )
library( "shinydashboard" )
library( "stringr" )

ui <- dashboardPage(
	
	title = "RNFLT - App",
	skin = "black",
	header = dashboardHeader(
		title = "RNFLT"#,
			#titleWidth = '97.5%'
		),
	sidebar = dashboardSidebar(
		sidebarMenu(
			menuItem(
				text    = "VISITOR",
				tabName = "TAB_VIEW_VISITOR_PDF"
			)
		)
	),
	body = dashboardBody(
		tabItems(
			# tabItem(
			# 	tabName = "TAB_UPLOAD_VISITOR_PDF"
			# ),
			tabItem(
				tabName = "TAB_VIEW_VISITOR_PDF",
				fluidRow(
					box(
						width = 4,
						title = "UPLOAD A PDF",
						collapsible = T,
						collapsed = F,
						fileInput( 'file_input', 'upload file ( . pdf format only)', accept = c( '.pdf' ) ),
						fluidRow(
							column( width = 6, h5( textOutput( "idText" ) ) ),
							column( width = 6, h5( textOutput( "sexText" ) ) ) 
						),
						hr( ),
						fluidRow(
							column( width = 6, h5( textOutput( "birthText" ) ) ),
							column( width = 6, h5( textOutput( "examText" ) ) )
						),
						hr( ),
						fluidRow(
							column( width = 12, h5( textOutput( "ageText" ) ) ) 
						)
					),
					box(
						width = 8,
						title = "PDF",
						height = "auto",
						collapsible = T,
						collapsed = F,
						uiOutput( "pdfview" )
					)
				)
			)
		)
	)
)

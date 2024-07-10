ui <- fluidPage(
		
		# CSS settings
		tags$head(
				
				tags$style(HTML("
										th, td {
										border: 1px solid black;
										}
										th, td {
										padding: 5px 5px 5px 5px;
  										text-align: center;
										}
										h5 {
										font-weight:bold;
										}
										hr {
										border-top: 2px solid #428BCA;
										}
										"))
		),
		
		
		# Application title
		titlePanel("Risk Factors"),
		
		# tabs at top of page
		tabsetPanel(
				# main tab with parameters and results
				tabPanel("Parameters",fluid="TRUE",
						sidebarLayout(
								#side bar with parameter settings
								sidebarPanel(
										# sidebar is 80% of vertical height of screen with a scroll bar
										style = "height: 80vh; overflow-y: auto;",
										
										h3(style="font-weight:bold;", "Model Settings:"),
										# set model
										uiOutput("select_distribution"),
										hr(), #horizontal line
										# set model parameters
										grouped_par_dists_UI("group1"),
										hr(), #horizontal line
										# set population characteristics
										grouped_par_dists_UI("group2"),
										hr(), #horizontal line
										# set population characteristics
										grouped_par_dists_UI("group3"),
										h3(style="font-weight:bold;", "Product Characterstics:"),
										# set product characteristics
										grouped_par_dists_UI("group4"),
										h3(style="font-weight:bold;", "Population Characterstics:"),
										# set population characteristics
										grouped_par_dists_UI("group5")										
								),
								
								
								
								# main panel with plot and settings
								mainPanel(
										fluidRow(
												# plot
												div(style = "overflow-x: auto; position: relative;",
													plotOutput("Totals", hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"))),
												uiOutput("hover_info", style = "pointer-events: none"),
												# lifetime dose
												uiOutput("dose_text"),
												# current settings
												uiOutput("settings")
												
										
										)
								)
						
						
						)
				),
				# data source tab
				tabPanel("Data Sources",fluid="TRUE",
						sidebarLayout(
								sidebarPanel(
										# select parameter
										uiOutput("par_data")
								),
								mainPanel(fluidRow(
												column(width = 8,
														# print HTML formatted source
														uiOutput("description")
												)
										)
								
								
								
								
								)
						)
				)
		
		)
)
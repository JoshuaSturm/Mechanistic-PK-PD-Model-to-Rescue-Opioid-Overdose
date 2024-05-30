server <- function(input, output, session) {

	# dataframe with all routes for selected contaminant
	route_list <- reactive({
				route_data %>% data.frame %>% filter(contaminant == input$contaminant) %>% select(route)
			})
	# dataframe with all model for selected route
	model_list <- reactive({
				model_data %>% data.frame %>% filter(contaminant == input$contaminant, route == input$route) %>% select(model)
			})
	
	# full dataframe for all parameters for selected model
	par_list <- reactive({
				par_data %>% data.frame %>% filter(contaminant == input$contaminant, route == input$route, model==input$model)
			})
	
	

	# initialize the reactive variable used to store user settings
	updated_par_data <- reactiveValues(settings=as.data.frame(par_data))
	
	# full dataframe for the intial settings ... doesn't need to be reactive?
	default_distribution_settings <- reactive({
				par_data %>% data.frame
			})
	
	## The following UI elements automatically populate the selections based on data available in the JSON file
	# UI element to pick the route based on the selected contaminant
	output$routes <- renderUI({
				req(input$contaminant)
				tagList(
						selectInput("route", "Contaminated Product:",
								choices=route_list()[,],
								selected=""
						),
				)
			})
	# UI element to pick the model based on the selected route
	output$model <- renderUI({
				req(input$route)
				tagList(
						selectInput("model", "Dose-Response Model:",
								choices=model_list()[,],
								selected=""
						),
				)
			})

	# initialize the populations for all parameters when changing contaminant, route, or model
	population <- reactiveValues(pops=c())
	observeEvent({
				input$contaminant
				input$route
				input$model},
			{
				
				settings <- updated_par_data$settings %>% filter(contaminant == input$contaminant, model==input$model, route == input$route)
				
				temp_pops <- data.frame()
				for (par in par_list()$par){
					parameter_settings <- settings[which(settings$par == par),]
					
					# sample the distribution for each parameter
					pop <- switch(parameter_settings$dist_to_use,
							"normal" =  {
								rnorm(n, mean=parameter_settings$normal.mean.init, sd=parameter_settings$normal.sd.init)
							},
							"log_normal" =  {
								mean <- parameter_settings$log_normal.mean.init
								sd <- parameter_settings$log_normal.sd.init
								logmean <-  log(mean^2 / sqrt(sd^2 + mean^2))
								logsd <- sqrt(log(1 + (sd^2 / mean^2)))
								rlnorm(n, logmean, logsd)
							},
							"uniform" = {
								runif(n, parameter_settings$uniform.init.min, parameter_settings$uniform.init.max)
							},
					
					)
					temp_pop <- data.frame(pop=pop)
					colnames(temp_pop) <- par
					if (par == head(par_list()$par, 1)){
						temp_pops <- temp_pop
					} else{
						temp_pops <- cbind(temp_pops, temp_pop)
					}
				}
				#store the results in a reactive variable
				population$pops <- temp_pops
			})
	
	# initialize the plot
	risk_prob <- reactiveValues(default=c())
	observeEvent({
				input$contaminant
				input$route
				input$model},
			{
				req(population$pops)
				# calculate risk (uses functions/EPA_equation.R)
				if (input$model == "Extrapolation from Lifetime Exposure to Benzene in the Air (EPA IRIS 2000)"){
					risk <- EPA_equation(input$route, df=population$pops)
				}
				
				# calculate the acculated probability for each risk group (1e-8 to 1e-3)
				accumulated_prob <- c()
				for (x in c(10^(-8:-3))){
					sum <- length(risk[which(risk >= x)])
					accumulated_prob <- rbind(accumulated_prob, data.frame(group=x, prob=sum/length(risk), settings="Default Settings"))
				}
				risk_prob$default <- accumulated_prob$prob
			}
	)
	
	# load the parameter selection module for each group of paramters (hardcoded to be 3 groups) (/modules/grouped_par_dists.R)
	group1_out <- grouped_par_dists_Server("group1", parent=session$input, group_idx=1)
	group2_out <- grouped_par_dists_Server("group2", parent=session$input, group_idx=2)
	group3_out <- grouped_par_dists_Server("group3", parent=session$input, group_idx=3)
	
	# update the settings and sampled population based on group 1 changes
	observe({
				req(group1_out(), input$contaminant, input$model, input$route)
				if (!is.null(group1_out()$settings) & !is.null(group1_out()$par)){

					
					updated_par_data$settings[which(updated_par_data$settings[,"contaminant"]==input$contaminant &
									updated_par_data$settings[,"model"] == input$model &
									updated_par_data$settings[,"route"]==input$route &
									updated_par_data$settings[,"par"] == c(group1_out()$par)),] <- group1_out()$settings
					
				}
				if (!is.null(group1_out()$pop) & !is.null(group1_out()$par)){
					population$pops[,which(names(population$pops) == group1_out()$par)] <- group1_out()$pop
				}
			})
	
	# update the settings and sampled population based on group 2 changes
	observe({
				req(group2_out(), input$contaminant, input$model, input$route)
				if (!is.null(group2_out()$settings) & !is.null(group2_out()$par)){
					
					
					updated_par_data$settings[which(updated_par_data$settings[,"contaminant"]==input$contaminant & 
											updated_par_data$settings[,"model"] == input$model &
											updated_par_data$settings[,"route"]==input$route &
											updated_par_data$settings[,"par"] == c(group2_out()$par)),] <- group2_out()$settings
					
				}
				if (!is.null(group2_out()$pop) & !is.null(group2_out()$par)){
					population$pops[,which(names(population$pops) == group2_out()$par)] <- group2_out()$pop
				}
			})
	
	# update the settings and sampled population based on group 3 changes
	observe({
				req(group3_out(), input$contaminant, input$model, input$route)
				if (!is.null(group3_out()$settings) & !is.null(group3_out()$par)){
					
					
					updated_par_data$settings[which(updated_par_data$settings[,"contaminant"]==input$contaminant &
											updated_par_data$settings[,"model"] == input$model &
											updated_par_data$settings[,"route"]==input$route &
											updated_par_data$settings[,"par"] == c(group3_out()$par)),] <- group3_out()$settings
					
				}
				if (!is.null(group3_out()$pop) & !is.null(group3_out()$par)){
					population$pops[,which(names(population$pops) == group3_out()$par)] <- group3_out()$pop
				}
			})

	risk_probs <- reactive({
				req(input$route, group1_out(),group2_out(),group3_out(), risk_prob$default)
				# calculate risk (uses functions/EPA_equation.R)
				if (input$model == "Extrapolation from Lifetime Exposure to Benzene in the Air (EPA IRIS 2000)"){
					risk <- EPA_equation(input$route, df=population$pops)
				}
				
				# calculate the acculated probability for each risk group (1e-8 to 1e-3)
				accumulated_prob <- c()
				for (x in c(10^(-8:-3))){
					sum <- length(risk[which(risk >= x)])
					accumulated_prob <- rbind(accumulated_prob, data.frame(group=x, prob=sum/length(risk), settings="User-Provided Settings"))
				}
				
				accumulated_prob
			})
	
	
	
	# main plot	
	output$Totals<-renderPlot({
				req(input$route, group1_out(),group2_out(),group3_out(), risk_prob$default, risk_probs())
				accumulated_prob <- risk_probs()
				default_prob <- data.frame(group=c(10^(-8:-3)), prob=risk_prob$default, settings="Default Settings")
				
				# main plot settings
				concern_plot <- ggplot(data=accumulated_prob, aes(x=group,y=prob, color=settings))
				concern_plot <- concern_plot+ geom_line(size=1.5) + geom_point(size=3)
				concern_plot <- concern_plot+ geom_line(data=default_prob, aes(x=group,y=prob, color=settings), size=1.5)
				concern_plot <- concern_plot+ geom_point(data=default_prob, aes(x=group,y=prob, color=settings), size=3)
				concern_plot <- concern_plot+ ylim(0,1)
				concern_plot <- concern_plot+ scale_color_manual(values=c("black", rgb(66/255,139/255,202/255)))
				concern_plot <- concern_plot+ scale_x_continuous(breaks=c(10^(-8:-3)), labels=c(
								"≥1 in 100 million", 
								"≥1 in 10 million", 
								"≥1 in 1 million", 
								"≥1 in 100 thousand", 
								"≥1 in 10 thousand",
								"≥1 in 1 thousand"), trans = "log10")
				
				concern_plot <- concern_plot+ ylab("Probability")
				concern_plot <- concern_plot+ xlab("Excess Cancer Risk")
				
				concern_plot <- concern_plot+ ggtitle("Estimated Probability of Excess Cancer Risk from Lifetime Exposure to Product")
				
				concern_plot <- concern_plot+ theme_bw()	
				concern_plot <- concern_plot+ theme(
#						legend.position = "none",
						legend.title= element_text(color="black", size=12),
						legend.text= element_text(color="black", size=12),
						# Hide panel borders and remove grid lines
						panel.border = element_blank(),
						panel.grid.major = element_line(colour = "grey",size=0.25),
						panel.grid.minor = element_line(colour = "grey",size=0.25),
						# Change axis line
						axis.line = element_line(colour = "black"),
						axis.title.x = element_text(color="black", size=12),
						axis.title.y = element_text(color="black", size=12),
						axis.text.x= element_text(color="black", size=12, angle=30, hjust=1),
						axis.text.y= element_text(color="black", size=12),
						title= element_text(face="bold"))
				
				concern_plot


			},width=800)
			
			
	output$hover_info <- renderUI({
				req(input$route, group1_out(),group2_out(),group3_out(), risk_prob$default, risk_probs())
				accumulated_prob <- risk_probs()
				default_prob <- data.frame(group=c(10^(-8:-3)), prob=risk_prob$default, settings="Default Settings")
				
				data <- rbind(accumulated_prob, default_prob)
				hover <- input$plot_hover
				point <- nearPoints(data, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
				if (nrow(point) == 0) return(NULL)
				
				# create style property fot tooltip
				# background color is set so tooltip is a bit transparent
				# z-index is set so we are sure are tooltip will be on top
				style <- paste0("position:absolute; z-index:100; ","left:", hover$coords_css$x+10, "px; top:", hover$coords_css$y-35, "px; padding: 5px 5px 5px 5px;")
				
				# actual tooltip created as wellPanel
				wellPanel(
						style = style,
						HTML(paste0("<b> Probability: </b>", signif(point$prob,4)))
				)
			})
			
	# build HTML table with the current plot settings and warnings
	output$settings <- renderUI({
				req(input$contaminant, input$model, input$route, group1_out(),group2_out(),group3_out())
				
				# build the header row
				all_text <- paste0("<h5>Current settings:</h5>")
				table_text <- paste0("<table>
									  <tr>
									    <th>Parameter</th>
									    <th>Distribution</th>
									    <th>Settings</th>
									    <th>Unit</th>
									  </tr>
										")
				# text and style changes to make if warning is displayed	
				warning <- "<td style='color:red;border: 0px solid black;background-color:transparent;'>Warning: selected values differ from defaults (see data sources)</td>"
				warning_tag <- "<td style='background-color:#FA5F55'>"
				# build a row for each parameter
				for (this_par in par_list()$par){
					# get the current parameter settings
					parameter_settings <- updated_par_data$settings %>% filter(contaminant == input$contaminant, model == input$model, route == input$route, par == this_par)
					# check for suffix (like e-6 for lifetime risk)
					if(!is.na(parameter_settings$suffix)){
						suffix <- parameter_settings$suffix
					} else {
						suffix <- ""
					}
					# get the default settings to check against current settings (display warning if not the same)
					default_settings <- default_distribution_settings() %>% filter(contaminant == input$contaminant, model == input$model, route == input$route, par == this_par)
					
					warnings_text <- ""
					front_tag <- "<td>"
					# if the selected type of distribution is not the same as the default, display warning
					if(parameter_settings$dist_to_use != default_settings$dist_to_use){
						warnings_text <- warning
						front_tag <- warning_tag
					} else {
						# if the selected distribution is the same as the defualt, check the initial values
						# if the initial values are different from the default, display warning
						filtered_default <- default_settings %>% select(!c(..JSON, document.id, contaminant.idx, contaminant, model.idx, model, route.idx, route, group.idx, groups, par.idx, par, unit, suffix, source, source_image_path, dist_to_use))
						filtered_settings <- parameter_settings %>% select(!c(..JSON, document.id, contaminant.idx, contaminant, model.idx, model, route.idx, route, group.idx, groups, par.idx, par, unit, suffix, source, source_image_path, dist_to_use))
						
						default_dists <- strsplit(colnames(filtered_default), "\\.") %>% sapply(unique)
						settings_dists <- strsplit(colnames(filtered_settings), "\\.") %>% sapply(unique)
						
						filtered_default <- filtered_default[,which(default_dists[1,]==default_settings$dist_to_use & (default_dists[2,]%in% "init" | default_dists[3,] %in% "init"))]
						filtered_settings <- filtered_settings[,which(settings_dists[1,]==parameter_settings$dist_to_use & (settings_dists[2,]%in% "init" | settings_dists[3,] %in% "init"))]
						
						if(!identical(filtered_default, filtered_settings)){
							warnings_text <- warning
							front_tag <- warning_tag
						}
						
					}
					# build the table row
					par_text <- paste0(front_tag, this_par, "</td>")
					dist_text <- paste0(front_tag, gsub("_", "-", parameter_settings$dist_to_use), "</td>")
					settings_text <- switch(parameter_settings$dist_to_use,
							"uniform" = paste0(front_tag,"min ", parameter_settings$uniform.init.min, suffix, " and max ", parameter_settings$uniform.init.max, suffix, "</td>"),
							"normal" = paste0(front_tag,"mean ", parameter_settings$normal.mean.init, suffix, " and sd ", parameter_settings$normal.sd.init, suffix, "</td>"),
							"log_normal" = paste0(front_tag,"mean ", parameter_settings$log_normal.mean.init, suffix, " and sd ", parameter_settings$log_normal.sd.init, suffix, "</td>")
							)
					
					unit_text <- paste0(front_tag,parameter_settings$unit ,"</td>")
					row_text <- paste0("<tr>", par_text, dist_text, settings_text, unit_text, warnings_text, "</tr>")
					table_text <- paste(table_text, row_text)
				}
				# output the full table
				HTML(paste0(all_text, table_text, "</table>"))
					
				})
				
	# text report of average daily dose of contaminant. Just hardcoded for now. Probably should make it a second output from the model equation function instead
	output$dose_text <- renderUI({
				req(input$contaminant, input$model, input$route, group1_out(),group2_out(),group3_out())
				product_absorption <- population$pops[,"Benzene Absorption"]/100
				product_absorption[which(product_absorption > 1)] <- 1
				
				product_per_day <- population$pops[,"Daily Frequency"]
				product_days_per_year <- population$pops[,"Annual Frequency"]
				skin_percent <- population$pops[,"Percentage of Skin Coverage"]
				skin_area <- population$pops[,"Surface Area"]
				density <- population$pops[,"Application Density"]
				product_conc <- population$pops[,"Benzene Concentration"]
				
				# ug/g * % * % * m^2 * mg/cm^2 *cm^2/m^2 * g/mg * 1/day * day/year * year/day * mg/ug = mg/day
				dose <- product_conc*product_absorption*skin_percent*skin_area*density*10000/1000*product_per_day*product_days_per_year/365/1000
				dose_quant <- quantile(dose, c(0.025, 0.25, 0.5, 0.75, 0.975))
				
				text <- paste0("<h5>Lifetime Average Daily Dose:</h5>The median lifetime average daily dose of ", input$contaminant," for the product is ", format(signif(dose_quant[["50%"]],3), scientific=T)," mg/day (95%CI, ",format(signif(dose_quant[["2.5%"]],3), scientific=T)," to ",format(signif(dose_quant[["97.5%"]],3), scientific=T)," mg/day)")
				HTML(text)
			})
	# ui element to select the parameter based on selected contaminant, route, and model in the data source tab
	# splits the list of pars into groups
	output$par_data <- renderUI({
				req(input$contaminant, input$model, input$route)
				# named list of parameters to choose. Use list() inside of list if the group only has one parameter. 
				# Use c() inside list if the group has more than one parameter.
				# currently hard coded to assume group 1 only has 1 parameter
				# should build list in a for loop or something where it checks the size of the group's par list
				pars_to_choose <- list(list(par_list()[par_list()$group.idx==1, "par"]),
						c(par_list()[par_list()$group.idx==2, "par"]),
						c(par_list()[par_list()$group.idx==3, "par"]))
				names(pars_to_choose) <- unique(par_list()$groups)
				tagList(
						selectInput("par_data", "Select Parameter", pars_to_choose,
								selected=""
						)
				)
			})
	# display the source text and image (if it has one) in the data source tab
	output$description <- renderUI({
				req(input$par_data)
				this_par_data <- par_list() %>% filter(par == input$par_data)
				if(!is.na(this_par_data$source_image_path)){
					image <- paste0("<img src='",this_par_data$source_image_path,"' width='800px', height='400px'>")
				} else {image <- ""}
				tagList(	
					HTML(this_par_data$source),
					HTML(image)
				)
				
			})
	
}
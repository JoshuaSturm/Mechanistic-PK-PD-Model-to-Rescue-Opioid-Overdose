
grouped_par_dists_UI <- function(id) {
	ns <- NS(id) # set the namespace for the module id
	# output the reactive ui elements 
	tagList(
			uiOutput(ns("pars")),
			uiOutput(ns("distributions")),
			uiOutput(ns("dist_UI")),
			uiOutput(ns("default"))
	)
	
	
	
}
grouped_par_dists_Server <- function(id, parent, group_idx) { # parent is input from main UI
	moduleServer(id, function(input, output, session){
				# full dataframe for the parameters in the group
				group_par_list <- reactive({
							par_data %>% data.frame %>% filter(contaminant == parent$contaminant, model==parent$model, route == parent$route, group.idx == group_idx)
						})			
	
				# full dataframe for the selected parameter
				this_par_list <- reactive({
							group_par_list() %>% filter(par == input$par)
						})
				# distributions available for the selected parameter
				this_par_dists <- reactive({
							temp1 <- this_par_list() %>% 
									select(!c(..JSON, document.id, contaminant.idx, contaminant, model.idx, model, route.idx, route, group.idx, groups, par.idx, par, unit, suffix, source, source_image_path, dist_to_use)) %>% 
									select_if(~ !any(is.na(.)))
							temp2 <- strsplit(colnames(temp1), "\\.") %>% sapply(unique)
							unique(temp2[1,])
						})
				# initialize the reactive variable used to store user settings
				updated_par_data <- reactiveVal({
							par_data %>% data.frame
						})
				
				## The following observeEvent calls update the stored settings when changes are made
				# make changes to the stored values when user changes the distribution
				observeEvent({
							input$dist
						}, {
							temp <- updated_par_data()
							temp[which(temp$contaminant == parent$contaminant & temp$model==parent$model & temp$route == parent$route & temp$par == input$par), 
									c("dist_to_use")] <- c(input$dist)
							updated_par_data(temp)
						})
				# make changes to the stored values when user changes the normal distribution settings
				observeEvent({
							input$norm_mean
							input$norm_sd
						}, {
							temp <- updated_par_data()
							temp[which(temp$contaminant == parent$contaminant & temp$model==parent$model & temp$route == parent$route & temp$par == input$par), 
									c("normal.mean.init", "normal.sd.init")] <- c(input$norm_mean, input$norm_sd)
							
							updated_par_data(temp)
							
						})
				# make changes to the stored values when user changes the log-normal distribution settings
				observeEvent({
							input$lnorm_mean
							input$lnorm_sd
						}, {
							temp <- updated_par_data()
							temp[which(temp$contaminant == parent$contaminant & temp$model==parent$model & temp$route == parent$route & temp$par == input$par), 
									c("log_normal.mean.init", "log_normal.sd.init")] <- c(input$lnorm_mean, input$lnorm_sd)
							updated_par_data(temp)
							
						})
				# make changes to the stored values when user changes the uniform distribution settings
				observeEvent({
							input$unif_range
						}, {
							temp <- updated_par_data()
							temp[which(temp$contaminant == parent$contaminant & temp$model==parent$model & temp$route == parent$route & temp$par == input$par), 
									c("uniform.init.min", "uniform.init.max")] <- c(input$unif_range[1], input$unif_range[2])
							updated_par_data(temp)
						})
				
				# filter stored data for the selected parameter
				distribution_settings <- reactive({
							req(input$par)
							updated_par_data() %>% filter(contaminant == parent$contaminant, model==parent$model, route == parent$route, par == input$par)
						})
				# get default data for the selected parameter
				default_distribution_settings<- reactive({
							req(input$par)
							par_data %>% data.frame %>% filter(contaminant == parent$contaminant, model==parent$model, route == parent$route, par == input$par)
						})
				
				init_settings <- reactiveValues(init=c())
				# only update the initial input values when the parameter or distribution changes or when the reset to default button is pressed
				# if instead the initial input values update every time the slider moves, changing the slider too quickly causes it to get stuck in a loop (present in version 1.5)
				observeEvent({
							input$par
							input$dist
							input$default
						},
						{
							req(input$par)
							init_settings$init <- distribution_settings()				
						}, ignoreNULL = FALSE)
				# UI element to select the parameter based on the group
				output$pars <- renderUI({
							req(parent$contaminant, parent$model, parent$route)
							choices <- group_par_list()[,"par"] 
							names(choices) <- paste0(group_par_list()[,"par"] , " (", group_par_list()[,"unit"],")")
							tagList(
									selectInput(session$ns("par"), paste0(unique(group_par_list()$groups),":"), choices,
											selected=""
									)
							)
						})
				# UI element to select the distribution based on the parameter
				output$distributions <- renderUI({
							req(input$par)
							choices <- this_par_dists()
							names(choices) <- gsub("_", "-", this_par_dists())
							tagList(
									selectInput(session$ns("dist"), "Distribution:",
											choices=choices,
											selected=init_settings$init[,"dist_to_use"]
									)
							)
						})
				# UI element to set the values for the distribution based on the selected distribution
				output$dist_UI <- renderUI({
							req(input$par, input$dist)
							if(!is.na(init_settings$init$suffix)){
								suffix <- init_settings$init$suffix
							} else {
								suffix <- ""
							}
							temp <- init_settings$init %>% select(!c(..JSON, source))
							switch(input$dist,
									"normal" = tagList(
											sliderInput(session$ns("norm_mean"), "Mean: ", 
													min=init_settings$init$normal.mean.min, 
													max=init_settings$init$normal.mean.max, 
													value=init_settings$init$normal.mean.init,
													step=init_settings$init$normal.mean.step,
													post=suffix),
											sliderInput(session$ns("norm_sd"), "SD: ", 
													min=init_settings$init$normal.sd.min, 
													max=init_settings$init$normal.sd.max, 
													value=init_settings$init$normal.sd.init,
													step=init_settings$init$normal.sd.step,
													post=suffix)
									),
									"log_normal" = tagList(
											sliderInput(session$ns("lnorm_mean"), "Mean: ", 
													min=init_settings$init$log_normal.mean.min, 
													max=init_settings$init$log_normal.mean.max, 
													value=init_settings$init$log_normal.mean.init,
													step=init_settings$init$log_normal.mean.step,
													post=suffix),
											sliderInput(session$ns("lnorm_sd"), "SD: ", 
													min=init_settings$init$log_normal.sd.min, 
													max=init_settings$init$log_normal.sd.max, 
													value=init_settings$init$log_normal.sd.init,
													step=init_settings$init$log_normal.sd.step,
													post=suffix)
									),
									"uniform" = tagList(
											sliderInput(session$ns("unif_range"), "Min and Max:", 
													min=init_settings$init$uniform.range.min, 
													max=init_settings$init$uniform.range.max, 
													value=c(init_settings$init$uniform.init.min, 
															init_settings$init$uniform.init.max),
													step=init_settings$init$uniform.step.blank,
													post=suffix)
									)
							
							)
						})
				# reset to defualt button
				# only appears when the selected values are different from the defaults
				output$default <- renderUI({
							req(input$par, isTruthy(input$unif_range) || isTruthy(input$norm_sd) || isTruthy(input$lnorm_sd))
							filtered_default <- default_distribution_settings() %>% select(!c(..JSON, document.id, contaminant.idx, contaminant, model.idx, model, route.idx, route, group.idx, groups, par.idx, par, unit, suffix, source, source_image_path, dist_to_use))
							filtered_settings <- distribution_settings() %>% select(!c(..JSON, document.id, contaminant.idx, contaminant, model.idx, model, route.idx, route, group.idx, groups, par.idx, par, unit, suffix, source, source_image_path, dist_to_use))
							
							default_dists <- strsplit(colnames(filtered_default), "\\.") %>% sapply(unique)
							settings_dists <- strsplit(colnames(filtered_settings), "\\.") %>% sapply(unique)
							
							filtered_default <- filtered_default[,which(default_dists[1,]==input$dist & (default_dists[2,]%in% "init" | default_dists[3,] %in% "init"))]
							filtered_settings <- filtered_settings[,which(settings_dists[1,]==input$dist & (settings_dists[2,]%in% "init" | settings_dists[3,] %in% "init"))]
							
							
							if(!identical(filtered_default, filtered_settings) | default_distribution_settings()$dist_to_use != distribution_settings()$dist_to_use){
								tagList(
										actionButton(session$ns("default"), "Reset to default")
										)
							} else {
								HTML("")
							}
							
						})
				# if the reset to default button is pressed, update the settings to the defaults
				observeEvent({
							input$default
						},
						{
							temp <- updated_par_data()
							temp[which(temp$contaminant == parent$contaminant & temp$model == parent$model & temp$route == parent$route & temp$par == input$par),] <- default_distribution_settings()
							updated_par_data(temp)
						})
				
				population <- reactiveVal({})
				# sample the distribution for the current settings
				observeEvent({
							updated_par_data()
							input$par
						},
						{
							req(input$par, isTruthy(input$unif_range) || isTruthy(input$norm_sd) || isTruthy(input$lnorm_sd))
							parameter_settings <- updated_par_data() %>% filter(contaminant == parent$contaminant, model==parent$model, route == parent$route, par == input$par)
							
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
							population(pop)
						})
				# output the selected parameter, sampled distribution, and current settings
				out <- reactive({
							list(par=input$par, pop=population(), settings=distribution_settings())
						})
				return(out)
	})
}
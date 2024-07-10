# load necessary libraries
library(shiny)
library(ggplot2)
library(shinyjs)
library(bslib)
library(tidyjson)
library(dplyr)

# source all functions and modules
functions <- list.files(path="functions", pattern = "\\.R$",  
		ignore.case = TRUE) 

modules <- list.files(path="modules", pattern = "\\.R$",  
		ignore.case = TRUE) 

if (length(functions) != 0 && length(modules) != 0){
	files_to_source <- c(paste0("functions/", functions), paste0("modules/", modules))
	lapply(files_to_source, source)
} else if (length(functions) == 0){
	files_to_source <- c(paste0("modules/", modules))
	lapply(files_to_source, source)
} else if (length(modules) == 0){
	files_to_source <- c(paste0("functions/", functions))
	lapply(files_to_source, source)
} 

# process data from the json file
raw_data <- read_json("data/contaminant_data.json")
contaminant_data <- raw_data %>% enter_object("contaminants") %>% gather_array("contaminant.idx") %>% spread_values(contaminant = jstring(name))
contaminant_names <- as.data.frame(contaminant_data) %>% select(contaminant)


route_data <- contaminant_data %>% enter_object("routes") %>% gather_array("route.idx") %>% spread_values(route = jstring(type))

model_data <- route_data %>% enter_object("models") %>% gather_array("model.idx") %>% spread_values(model = jstring(model))

model_data <- route_data %>% enter_object("models") %>% gather_array("model.idx") %>% spread_values(model = jstring(model))

distribution_data <- model_data %>% enter_object("distributions") %>% gather_array("distribution.idx") %>% spread_values(distribution = jstring(distribution))

group_data <- model_data %>% enter_object("groups") %>% gather_array("group.idx") %>% spread_values(groups = jstring(group))

par_data <- group_data %>% enter_object("pars") %>% gather_array("par.idx") %>% spread_all

selected_contaminant <- "benzene"
selected_route <- "sunscreen"
selected_model <- "Extrapolation from Lifetime Exposure to Benzene in the Air (EPA IRIS 2000)"
default_dist <- "Experimentally derived distributions"
# set the population size
n <- 100000

# useful for debugging
#devmode(TRUE)

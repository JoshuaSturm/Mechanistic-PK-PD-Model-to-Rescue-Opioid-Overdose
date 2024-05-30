EPA_equation <- function(route, df, air_per_day=20, air_absorption=0.5) {
	
	
		
		body_weight <- df[,"Body weight"]
		product_absorption <- df[,"Benzene Absorption"]/100
		product_absorption[which(product_absorption > 1)] <- 1
#		print(quantile(product_absorption*100, c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)))
		
		product_per_day <- df[,"Daily Frequency"]
		product_days_per_year <- df[,"Annual Frequency"]
		air_unit_risk <- df[,"Lifetime Risk of Exposure to 1 unit of Benzene in the Air"]*1e-6
		skin_percent <- df[,"Percentage of Skin Coverage"]
		skin_area <- df[,"Surface Area"]
		density <- df[,"Application Density"]
		product_conc <- df[,"Benzene Concentration"]
		
		# BW normalized dose from 1 ug/m^3 continuous daily exposure
		# using 70 kg here so body weight still effects the outcome
		dose_air<-1*air_per_day*air_absorption/70
		
		# risk per 1 ug/kg/day
		slope_factor<- air_unit_risk/dose_air
		
		#benzene concentration in product that would produce a dose of 1 ug/kg/day
		# 1 ug/kg/day * body weight / (mass per use * use per day * days per year/total days)
		# mass per use = absorption * % skin covered * total skin area * density
		# [density] = mg/cm^2 * 10000 cm^2/m^2 * 1g/1000mg = g/m^2 
		benzene_conc_product<-1*body_weight/(product_absorption*skin_percent*skin_area*density*10000/1000*product_per_day*product_days_per_year/365)
		#unit is ug/g, which = ppm
		
		#product unit risk
		product_unit_risk<- slope_factor/benzene_conc_product
		product_risk <- product_unit_risk*product_conc
		return(product_risk)
	
	
}
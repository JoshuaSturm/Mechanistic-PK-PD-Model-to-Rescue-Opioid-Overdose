EPA_equation <- function(route, df, air_per_day=20, air_absorption=0.5) {
	
	
		
		body_weight <- df[,"Body weight"]
		product_absorption <- df[,"Benzene Absorption (dermal)"]/100
		product_absorption[which(product_absorption > 1)] <- 1		
		product_absorption[which(product_absorption < 0)] <- 0
		
		inhaled_absorption <- df[,"Benzene Absorption (inhaled)"]/100
		inhaled_absorption[which(inhaled_absorption > 1)] <- 1		
		inhaled_absorption[which(inhaled_absorption < 0)] <- 0
		
		inhaled_fraction <- df[,"Evaporated Benzene"]/100
		inhaled_fraction[which(inhaled_fraction > 1)] <- 1		
		inhaled_fraction[which(inhaled_fraction < 0)] <- 0
		
		absorbed_fraction <- df[,"Benzene Available for Dermal Absorption"]/100
		absorbed_fraction[which(absorbed_fraction > 1)] <- 1		
		absorbed_fraction[which(absorbed_fraction < 0)] <- 0
		
		product_per_day <- df[,"Daily Frequency"]
		product_days_per_year <- df[,"Annual Frequency"]
		air_unit_risk <- df[,"Lifetime Risk of Exposure to 1 unit of Benzene in the Air"]*1e-6
		skin_percent <- df[,"Percentage of Skin Coverage"]
		skin_area <- df[,"Surface Area"]
		density <- df[,"Application Density"]
		product_conc <- df[,"Benzene Concentration"]
		
		breathing_rate <- df[,"Minute Ventilation"]
		t1 <- df[,"Exposure time (near-field)"]
		V1 <- df[,"Distribution volume (near-field)"]
		t2 <- df[,"Exposure time (far-field)"]
		V2 <- df[,"Distribution volume (far-field)"]
		
		# BW normalized dose from 1 ug/m^3 continuous daily exposure
		# using 70 kg here so body weight still effects the outcome
		dose_air<-1*air_per_day*air_absorption/70
		
		# risk per 1 ug/kg/day
		slope_factor<- air_unit_risk/dose_air
		
		# Suncreen per use (g)
		sunscreen <- skin_percent*skin_area*density*10000/1000
		
		# inhaled dose per use (g)
		inhaled_dose <- sunscreen*inhaled_fraction*inhaled_absorption*breathing_rate/1000*(t1/V1 + t2/V2)
		
		# dermal dose per use (g)
		dermal_dose <- sunscreen*absorbed_fraction*product_absorption
		
		# uses per day (average throughout a year)
		use_per_day <- product_per_day*product_days_per_year/365
		
		#benzene concentration in product that would produce a dose of 1 ug/kg/day
		# 1 ug/kg/day * body weight / (mass per use * use per day * days per year/total days)
		# mass per use = absorption * % skin covered * total skin area * density
		# [density] = mg/cm^2 * 10000 cm^2/m^2 * 1g/1000mg = g/m^2 
		benzene_conc_product<-1*body_weight/((dermal_dose+inhaled_dose)*use_per_day)
		#unit is ug/g, which = ppm
		
		#product unit risk
		product_unit_risk<- slope_factor/benzene_conc_product
		product_risk <- product_unit_risk*product_conc
		return(product_risk)
	
	
}
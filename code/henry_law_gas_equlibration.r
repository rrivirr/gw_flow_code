
# convert temperature to Kelvin
pclntgp$tempK<-pclntgp$temp_c+273.15


# Convert temperature
X=(2.7182818^(-58.0931+(90.5069*(100/pclntgp$tempK))+(22.294*log(pclntgp$tempK/100))))*((0.0821*273.15)+((-1636.75+(12.0408*273.15)-(3.27957*0.01*273.15*273.15)+(3.16528*0.00001*273.15*273.15*273.15))/1000))

# assuming barometric pressure (bp) of 1, this can be changed to match the BP on a given day
bp<-1

# volumes of water and gas used for headspace equilibration (in Liters)
liquid_volume_l<-0.08
headspace_volume_l<-0.02


# Calculations for CH4
AP = (pclntgp$ch4_ppm * X * bp)/(0.0821*273.15)

AJ = pclntgp$ch4_ppm/(0.0821*273.15)

AV = (AJ*headspace_volume_l)+(AP*liquid_volume_l)

BB = AV/liquid_volume_l

pclntgp$ch4_wc_umoll_lakes<-BB


# Calculations for N2O
AP = (pclntgp$n2o_ppm * X * bp)/(0.0821*273.15)

AJ = pclntgp$n2o_ppm/(0.0821*273.15)

AV = (AJ*headspace_volume_l)+(AP*liquid_volume_l)

BB = AV/liquid_volume_l

pclntgp$n2o_wc_umoll_lakes<-BB


# Calculations for CO2
pclntgp$co2_ppm<-as.numeric(pclntgp$co2_ppm)
AP = (pclntgp$co2_ppm * X * bp)/(0.0821*273.15)

AJ = pclntgp$co2_ppm/(0.0821*273.15)

AV = (AJ*headspace_volume_l)+(AP*liquid_volume_l)

BB = AV/liquid_volume_l

pclntgp$co2_wc_umoll_lakes<-BB
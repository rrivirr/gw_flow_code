# Thermistor Ring Modeling

## Setting up the model
* heater is on for 30 seconds and then off for 240 seconds.
* when extracting data from the model, you want to record:
	* Values right when the heater is turned on.
	* Values at the end of the 240 second off phase.
	* For each timestep above, record 20 observations and average for subsequent calculations.
# Processing model data
## Objective
* The water is heated for a period of 30 seconds. After this, we measure temperature at the beginning and then end of a 4 minute stretch and compare how the temperature has changed over the course of time. This allows us to track the moment of a heat plume.
* We will compare model performance at different flow rates.

## Part 1: Run the model
* Select a wattage for the heating element.
* For each wattage, run the model for flow rates of 0.5, 1, 5, 10, 15, and 30 feet per day.
* Extract the temperature data for each of the thermistor rings.
* Repeat for different wattage heating elements.

## Part 2: Process the model data
* For each experiment run, record the temperature at two points: 1) when the heater is turned off and 2) 4 minutes (or whatever the specified time is) after the heater has turned off. Do this just for the first cycle of each model.
* Add these numbers to a table that includes temperatures at the beginning and end for each ring. Also record the water velocity for a given experiment.
* Do the above for each experimental run.

## Part 3: Run the calibration code
* Open the R code and run calibration.

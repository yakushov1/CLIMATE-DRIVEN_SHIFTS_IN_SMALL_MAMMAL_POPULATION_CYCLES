The folder contains a typical RStudio project.

To reproduce all the calculations included in the manuscript, first open the climateVStype_of_dynamic file.Pro using RStudio. 
In this case, you will not need to configure the working directories.

The scripts directory contains all the necessary scripts.
For convenience, they are numbered and named in the same way as the subsections  of the manuscript.
Each file contains the source code for reproducing the calculations and graphs presented in the article.


The images directory contains the illustrations included in the publication.


Finally, the data directory contains all the data needed for calculations.

- numbers_of_dominant_species.csv - total numbers of four dominant species by year

- winter_survival.csv -Information on winter survival of small mammals.
 --- Spec - species name
 --- Cyl_day_100_aug - Number in recalculation of the number of specimens per 100 cylinder-days (in August)
 --- Cyl_day_100_jun - the same in June
 --- Type - regime of dynamic (cyclical or not-cyclical)
 --- survival_rate - the difference between Cyl_day_100_jun and Cyl_day_100_aug
 --- dominant - common or uncommon species

- bad_snow_1976_1994.csv 
  contains 3 columns
 --- Sn_description: 1 if the snow was "bad" and 0 if "good". See the manuscript for details.
 --- Sn is the depth of the snow cover.
 --- Tavg - average temperature

- bad_snow_2005_2023.csv
  The same, but for the period 2005-2023.
  The additional diff column indicates the number of temperature transitions through 0 degrees on this day.


- Bakhta_annualy_temperature.csv
Data on the average annual temperature at the Bakhta weather station.
 --- Tavg - the average annual temperature.
 --- Tavg_SE - the standard error of the average  temperature
 --- Tavg_base - average temperature for the base period 1961-1990
 --- Tavg_base_SE - standard error of the average for the base period
 --- Tavg_last_decade - the average temperature for 2013-2022.
 --- Tavg_last_decade_SE - the standard error of the average for this period.
 --- T_diff - the difference between Tavg_base and Tavg_last_decade 
 --- Tavg_roll_mean - moving average for the entire period
 --- Tavg_roll_mean_from_1976 - moving average from 1976


- Bakhta_monthly_temperature.csv -The same, but for every month.

- numbers_of_days_with_bad_snow.csv
The number of days with unfavorable snow cover (details in the manuscript).
 --- count is the number of such days
 --- catch is the type of dynamics (cyclical or non-cyclical). The type of dynamics for a particular year has been determined in previous published papers.
 
 - climatic details
 folder with other climatic data. See manuscript or code for details






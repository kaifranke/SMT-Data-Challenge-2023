# SMT-Data-Challenge-2023

This is the GitHub reposity for the Sports Analytics Club at the University of Minnesota's submission to the 2023 SMT Data Challenge. The contributors to this are Kai Franke, Jack Rogers, Jackson Balch, and Isaac Blumhoefer. In our project for this year, we are looking at the relationship between the pitcher, catcher, and middle infielder to determine how they effect each other in trying to throw a basestealer out. 

The following files are in this repository:
* smtClean.R: the file involving all of the cleaning and preparation for this project.
  * loading data from files
  * taking spatial data and putting into important moments
  * feature engineering with important features such as distance from base, times of plays, location of ball at specific points, and runner information, and if there was a stolen base or not
  * cleaning the data to make it ready to input in the model
* SBsimple.csv
  * data that is ready to be input into the model,
  * has different moments in time for location of the ball, time since last event, etc. 

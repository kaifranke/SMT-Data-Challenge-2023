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
  * Field descriptions
    * X: id number for every row
    * game_str: individual game ID
    * timestamp: timestamp of each individual play with respect to the game
    * timediff: the time since the start of every play, with 0 being the time the pitch is released
    * playerAct: the player making the action in the play
    * event: description of what happened in this specific moment of the play
    * SB: a 1 if the play resulted in a stolen base, 0 if the player was thrown out
    * popTime: the time it took from the catcher catching the ball to the middle infielder catching the ball
    * exchange: the time it took for the catcher to throw the ball after catching it
    * timeOfThrow: how long it took for the throw from the catcher to get to the middle infielder
    * leadoff: the distance that the base stealer led off on the play
    * rDist2B: the distance that the base stealer was away from second base at this moment in time
    * maxSpeed: the maximum speed of the base stealer in this particular play
    * ball_x: the location of the ball in this moment of time with respect to left or right of second base with 0 being straight up the middle
    * ball_y: the location of the ball in this moment of time with respect to home plate with 0 being on home plate and 60.5 being at the pitcher's mound
    * ball_z: the location of the ball in this moment of time with respect to the ground with 0 being on the ground

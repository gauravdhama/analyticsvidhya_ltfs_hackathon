# analyticsvidhya_ltfs_hackathon
Team PredictOverflow : Solution to the analytics vidhya ltfs hackathon


Our approach was actually quite simple, after trying out a variety of techniques such as prophet, gluonts and ETS with multiple seasonalities in R we found that a simple neural network in R's forecasts package with appropriate number of lags actually gave the best accuracy.

We created an ensemble of 15 such models with different lags, which was giving us the best score on the public leaderboard (39/833). However we did not fare so well in the private leaderboard (89/833).

Later when the results were out we found that a single nnetar model in R's forecast package with 37 lags along with a box-cox transformation would have given us a 27th place on the private leaderboard and ~45th place on the public leaderboard. I have also attached the code for everyone's reference.






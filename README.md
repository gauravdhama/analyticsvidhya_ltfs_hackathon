# analyticsvidhya_ltfs_hackathon
Team PredictOverflow : Solution to the analytics vidhya ltfs hackathon

Our approach was actually quite simple, after trying out a variety of techniques such as prophet, gluonts and ETS with multiple seasonalities in R we found that a simple neural network in R's forecasts package with appropriate number of lags actually gave the best accuracy.

We created an ensemble of 15 such models with different lags, which was giving us the best score on the public leaderboard (39/833). However we did not fare so well in the private leaderboard (89/833).

Later when the results were out we found that a single nnetar model in R's forecast package with 37 lags along with a box-cox transformation would have given us a 27th place on the private leaderboard and ~45th place on the public leaderboard. I have also attached the code for everyone's reference.

Note that we removed the last point from segment 1 as it seemed anomalous to us and removing it was giving better results than including it. When we investigated more, we found that the last point corresponds to when the budget was presented on 5th of July, 2019. This was probably due to decline in auto and 2 wheeler loans immediately after the budget hence this would have led to a mean shift in segment 1 time series. 

Hence, getting good MAPE's on this series was difficult without getting vehicle sales data on a monthly or daily level. We did not find any such satisfactory data to include in our model. Not knowing the mean of the time series meant that one would have to continuously probe the public leaderboard to get some estimates of where the future mean lies. Hence everybody is getting a very high MAPE, even the first place solution.

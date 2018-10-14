# Machine Learning to Predict Playoff/Championship Success in the NBA
A school project completed by Benjamin Hwang, Kenneth Bogart, and Emily Lutz

## Preface
As an avid Basketball and NBA fan I wanted to focus my final undergraduate Machine Learning project on whether or not you can use regular season data to predict playoff and championship success. Questions such as: What characteristics do teams that make the playoffs have in common and can we build a model to predict playoff and championship success using only regular season data? Since there is a large amount of data publicly available through [basketball-reference.com](https://www.basketball-reference.com/) I proposed this project to my teammates Kenneth and Emily who agreed to help me explore these questions further.

## Summary
Using twelve seasons (2003 - 2015) of basketball data we broke our approach into two steps: 

1. Can we predict who will make the playoffs?

For playoff predictions we used cross validation and L1 regularization techniques to build logistic regression and decision tree models with a resulting final accuracy of 85%. This was a pleasant surprise since we did not take into account (purposefully) performance relative to other teams since making the playoffs is determined on relative wins to other teams in your conference. In addition, the features we used to build our model do not need a full regular season to predict so it would be possible to make a prediction based on mid season results.

2. Of those playoff teams can we predict who will win the championship?

For championship success of playoff teams we used Ordered Logistic Regression to try and rank where playoff teams would finish (i.e rank 1 = champion, 2 = Finals, 3 = Conference Finals etc.). We also used PCA to see how we could remove features that redundantly explained variance in our data since we experienced convergence errors from our model (most likely stemming from the limited amount of data that 12 seasons can afford i.e. 12 seasons means there are only 12 championship cases that you can observe). From this we also decided to create two different models: one that used advanced basketball stats (these are developed stats that are calculated from basic stats) and basic basketball stats (i.e how many field goals made in a game, what percentage of field goals made from field goals attempted, etc.). From this we found that our basic stats model did a better job of classifying the lower ranked teams whereas our advanced stats model did better at predicting the champion. Discussion of accuracy is futher detailed in our [report](https://github.com/bhwan1118/ML_Bball_Project/blob/master/Bogart_Hwang_Lutz_Final_Project.pdf) as it is pretty variable due to sparseness of the data we collected. That being said when this project was being conducted the NBA playoffs were in progress and our advanced stat model ended up predicting the eventual champion (Golden State Warriors) correctly! 

## For more in-depth coverage please see our written [report](https://github.com/bhwan1118/ML_Bball_Project/blob/master/Bogart_Hwang_Lutz_Final_Project.pdf)







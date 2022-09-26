# Body Fat Percentage Analysis

For details, please check the report [HERE](https://github.com/yovalishere/Medical/blob/main/Body%20Fat%20Percentage%20Analysis/Fat_Report.pdf)
### Project description
This project focuses on comparing the performance of different regression models in
terms of predicting body fat percentage.  5 regression models were used. They are :
1) Linear regression
2) Ridge regression
3) Partial least squares
4) Principal component regression
5) LASSO regression

### Data description
The data set “fat” is gathered from the “faraway” library of R. Body circumference
measurements (eg. Age, weight, height etc) are recorded for 252 men. Each man’s
percentage of body fat was accurately estimated by an underwater weighing technique. The
dataset consists of 252 rows and 18 columns. 
The first column (ie. brozek) is the response variables while the remaining 17
variables are the potential predictors. 

### Project findings
**LASSO regression** (Model 5) gives the lowest average testing error as well as testing error variance. That means it is the optimal model in this case.<br><br>
<img src="https://github.com/yovalishere/Medical/blob/main/Body%20Fat%20Percentage%20Analysis/Testing%20err_fat.jpg" width="550" height="350" />

<img src="https://github.com/yovalishere/Medical/blob/main/Body%20Fat%20Percentage%20Analysis/Testing%20err%20var_fat.jpg" width="550" height="350" />



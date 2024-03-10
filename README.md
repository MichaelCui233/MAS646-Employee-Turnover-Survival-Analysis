# MAS646-Employee-Turnover-Survival-Analysis
In this project, we need to query dataset from SQL server and build COX model to explore key factors affecting turnover rate in the company. 
# SQL
We queried four datasets from the server. 

**Employee** dataset includes "id", "age", "gender", "highestdegree", "workerhasdegree", "segment", "region", "joblevel", "termination", "hire_date", "tenure", "reason". **"Reason"** includes **Quit**, **Fired**, or **Employed**.

For all employee, we created **Performance** dataset. Every employee has a performance rating on 1st day of every month. 

**"Rating"** has 5 levels order low to high: underperformance < inconsistent < Solid < Strong < outstanding. 

**"Appointments"** is the number of appointments one employee made with clients. 

**"Signups"** is the number of clients signup after the appointment. We also calculated "ratio of signups to appointments in Performance".

We created 2 more datasets about performance for "employee quit" and "employee fired". **Performance_fired** and **Performance_quit**

# R
We first did some preprocessing of our datasets. 

Sort "highestdegree" to only "BA", "MBA", "MS", and "others". Convert "rating" from category to numerical variables from 0 to 4.

We built COX model on single feature first, then selected some features with low z-score and high correlation and put them in one model to test the performance. 

"Age", "gender", "rating", "ratio of signups to appointments", "highest degree"(MBA, MS, Others), and "job level"(Regular, Senior) are key factors to the turnover rate. 

# Conclusion
Female employees have a higher retention probability compared to male employees, indicating a potential gender bias issue that needs to be addressed.

Employees with higher degrees, such as BA, MBA, and MS, have a lower probability of not quitting, whereas those with lower degrees, such as AA and BBA, have a higher probability of not being fired. This indicates the need for offering additional training and development opportunities for employees to enhance their skills and increase their job satisfaction.

Senior employees have a high probability of not quitting or being fired, indicating the need for a clear career progression plan and growth opportunities for employees.

Performance is a crucial factor in employee retention, and underperformance is a significant predictor of both quitting and being fired. Hence, a performance improvement plan needs to be implemented to support underperforming employees and reduce the risk of turnover.


# universitycredits


##data_manipulation.R: clean, manipulate and reshape the raw data file to produce the data frames for analysis. Have to run this script first before running the other two scripts.

##by_student_analysis.R: produce a data frame with each student in each row, variables include Y (dropout) and Xs including various variables to predict droptout. The data frame will be used for classification.
  One extra thing to do: compute distance to school based on zip code information

##unitsPred_v2.R: to predict total units by enrolled students in previous N semesters. Tried several regression models. The models have relatively high R square. The major problem is that error rate is much bigger at two ends. The constant variance assumption might be violated.

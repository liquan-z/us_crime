Flexdashboard to explore US crime data
=========
This repository folder provides the data and R codes for the flexdashboard created to explore US crime data from 1995 to 2019. The data file contains US statewise occurances of different types of crimes and also includes metropolitan and non-metropolitan area data. 

This shiny application has Rmd file namely usCrime.Rmd which load data files: usaCrimeDat.rds, crime1995-2019.rds, rank.csv and metro.rds. You can clone these files from here. Basicly, crime1995-2019.rds and metro.rds are obtained from merge.r file, usCrimeDat.rds is based on merge2.r. This flexdashboard has five pages: First page contains mapping for US crime from 1995 to 2019 and rank of state total crime rate per 100,000 inhabitants for each state. Second page is Barplot Animation by specific area of each state for 2010-2019. Third page contians the time series plot and box plot of each crime type for 1995-2019. Fourth page shows clustering results by mapping and bubble plot for 2005-2019. The final page collects the orgnized dataset crime1995-2019 and metro.

Before running rmd file please make sure packages listed are already installed. 

The application is deployed on RStudio as well. 



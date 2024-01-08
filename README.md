# Pedagogical Extended Prone

Welcome on the github repository: Pedagogical Extended Prone

This github repo contains mock data and the code for the activities of the ESICM course: "Data Science for the Intensive Care" which 
you can find here: https://academy.esicm.org/enrol/index.php?id=377#intropage-courseoverview

To use this github, see the explanatory video here: https://youtu.be/5gBzo-DPR5s

Then follow these steps: 
- install R and RStudio on your computer (to be done only once)
- Pull this github repo on your local computer. 
- open the .Rproj file, this will automatically open RStudio. 
- within the Rproject in RStudio, open the results.Rmd file and run it. 

You should see the mocked results of the study appear in RStudio. 
You can now try to understand / decipher the pipeline that allowed to start from the mocked data up to the final results. 

If you are already familiar with R the main sources which can help you understand the organisation of this project are: 

- "What they forgot to teach you about R" by Jenny Bryan: https://rstats.wtf/

- "The {targets} R package user manual": https://books.ropensci.org/targets/

In brief, you will find the raw and mocked data in the file random_data. All the scripts are in the folder "code/R" and are mainly: 
- data_generation.R which allows for the regeneration of mocked data
- functions_data.R which stores all functions used to curate data (i.e.: clean them + reorganize them in a way they can be analyzed)
- functions_analyze.R which stores all functions used to create final tables
- functions_visualization.R which stores all functions used to create graphics
- packages.R is a script which loads all packages needed for this project

If you know nothing about R I first encourage you to go through those two books: 

"Hands-on programming with R": https://rstudio-education.github.io/hopr/ (you can skip this one if you have some vague notion of what a programming language is)

"R for Data Science" by Hadley Wickham: https://r4ds.had.co.nz/, bear in mind that a second edition will be soon available. 

Enjoy ! 


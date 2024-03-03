# `Mini-Frac` Shiny Application

## **Description:**

This web application allows to analyze a **mini-frac test** used in oil and gas industry and calculate hydraulic fracture parameters. 
It is developed for oil and gas professionals and analysts working with hydraulic fracturing. The app is available in **English** and **Russian**. 

[![Button-8.png](https://i.postimg.cc/XvwTVW8C/Button-8.png)](https://sergei-makarov.shinyapps.io/MiniFrac/)

A user is required to input data about the oil and gas reservoir and upload the Excel file with mini-frac test results (a sample file can be downloaded in the app). The application calculates output parameters 
(fracture half length, fracture width, fluid efficiency and etc.) and creates charts visualizing the data. The report with the analysis results is available to download in Excel and Word formats. 

Full methodology and logic of mini-frac test are described in attached `PDF files` in English and Russian.

## **Project overview**

The application consists of 2 main files: `ui.R` and `server.R`.

The Word output file is created from RMarkdown files: `Results_Eng.Rmd`, `Results_Ru.Rmd`. 
Files containing the sample mini-frac tests: `minifrac_test.xlsx`, `minifrac_test_en.xlsx`

In order to run the application, ensure to download the necessary packages as in `requirements.txt` and set the working directory to local. 


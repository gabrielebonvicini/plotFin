# plotFin

## Installation

You can download the 'plotFin' package from github by running the following code: 


``` r
install.packages("devtools") #if you don't have devtools installed 
library(devtools) 
devtools::install_github("gabrielebonvicini/plotFin")
1 #run this line only if R asks you if you want to updated packages dependecies
library(plotFin)
```
After the Third line R can ask you if you want to updates some packages which plotFin depends on. 
We suggest you to accomplish this request to avoid any kind of problem.
#### Possible issues you can face downloading the package 
-	If you already have the package installed (maybe you installed it before) and you want to reinstall it, you have to manually remove the package from your computer. 
To do this you search for the path of the R library in your computer running `.libPaths()`and  then search for the plotFin folder and delete it.
-	You may face some issues with a file called 00LOCK. In this case you will need to remove this file manually by using the pacman package. 
Thus, run the following code (source: [link](https://stackoverflow.com/questions/26570912/error-in-installation-a-r-package))

``` r
install.packages(“pacman”)
library(pacman)
p_unlock()  
```

## Description
`plotFin` allows users to plot various types of financial data with just a single line of code. The package aims to streamline the process of creating plots, making it easy and efficient. Typically, creating a plot involves downloading the data, cleaning and transforming it, and then plotting it using an appropriate visualization method that effectively captures the key features of the data. However, `plotFin` automates this entire process within its functions.

To illustrate this with a simple example, let's consider the `plot.return()` You can select the desired stock for which you want to plot the time series of returns, specify the period you want to display, and choose the appropriate time frame. Once these parameters are set, you're good to go! 

## Useful links 
- Github: https://github.com/gabrielebonvicini/plotFin
- Bug Reports: https://github.com/gabrielebonvicini/plotFin/issues

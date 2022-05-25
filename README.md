# plotFin

## Installation

You can download the 'plotFin' package from github by running the following code: 


``` r
install.packages("devtools") #if you don't have devtools installed 
library(devtools) 
devtools::install_github("gabrielebonvicini/plotFin)
#1 #run this line only if R asks you if you want to updated packages dependecies
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
`plotFin` allows the user to plot financial data of different types with a simple line of code. The purpose of the package is to simplify the process of creating a 
plot to makes it as easy and quick as possible. 
Indeed, in order to do that, you usually have to download the data, clean  and transform them in the desired output, and then plot the data with an appropraite plot, 
which is able to capture and highlight the main features of what you are displaying. 
With  `plotFin` all this process is done inside the functions. 
To explain it with the simplest example, let's consider the `plot.return()` function: you choose the stock you want to plot the time series of the returns, the period 
from which you want to display them as well as the time frame, and you are done! 

## Useful links 
- Github: https://github.com/gabrielebonvicini/plotFin
- Bug Reports: https://github.com/gabrielebonvicini/plotFin/issues

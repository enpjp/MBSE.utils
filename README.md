# Purpose

This package has be created to assist in the analysis of XML files created by **Model Based Systems Engineering** (MBSE) software. As such, the functions may not work as expected on XML files created by other software tools.

This is still a development package, but care has been taken to ensure that it passes all CRAN-R tests without error.

# Installation

To install MBSE.utils use the following code snippet:

    library(devtools)
    library(roxygen2)
    install_github("enpjp/MBSE.utils")
    
# Data

Some lightweight data has been included as a feedstock for the vignettes in the form of an XML file and it's representation as datumTriples.

#  Vignettes

Some vignettes are planned so you might want to install from Github using:

    install_github("enpjp/MBSE.utils", 
    dependencies = TRUE, 
    build_vignettes = TRUE)
    
If you fork the repository use this snippet to update your local copy of vignettes:

    install(dependencies = TRUE, build_vignettes = TRUE)
   
But never use `build_vignettes()` as, apparently, bad things will happen to the state of package code.

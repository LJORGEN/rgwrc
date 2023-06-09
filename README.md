# rgwrc
Geographically Weighted Regression Clustering

This package utilizies the strengths of the MGWR python package in conjuction with R geo-spatial analysis and clustering algorithms to
generate neighborhoods to be used as location proxies in regression analysis.

rgwrc requires the installation of the miniconda package to serve as a bridge between R and Python.

In addition a directory called "rgwrc_hold" will be genertated in the package directory in the computer's R library automatically
when the package is loaded. This is necessary to establish a perstistant link between live R data being run in the rgwrc functions
and the Python scripts.

Refer to the 'help' page in R studio for function details and examples.

#####R code to install rgwrc from github#####

library(devtools)

devtools::install_github("LJORGEN/rgwrc")

#############################################

MGWR is under the BSD 3-Clause License: https://github.com/pysal/mgwr/blob/master/LICENSE.

rgwrc is not endorsed by the authors and contributors of MGWR.

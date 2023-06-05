# rgwrc
Geographically Weighted Regression Clustering

This package utilizies the strengths of the MGWR python package in conjuction with R sf package along with clustering algorithms to
generate neighborhoods to be used as location proxies in regression analysis.

rgwrc requires the installation of the miniconda package to serve as a bridge between R and Python. 

In addition a directory called "rgwrc_hold" will be genertated in your computers Public folder under C:/Users/Public/ automatically
when the package is loaded. This is necessary to establish a perstistant link between live R data and the Python scripts. If you do 
not have the path C:/Users/Public/ it is recommended to construct one. Otherwise the package will not be able to fully function.

Refer to the 'help' page in R studio for function details and for examples on how to use the functions.

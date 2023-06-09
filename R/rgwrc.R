#####Load required dependencies#####
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This is version ", packageVersion(pkgname),
                        " of ", pkgname)

  packageStartupMessage("rgwrc uses functions from the following R libraries:dplyr,data.table,NbClust,sf,reticulate")

  packageStartupMessage("rgwrc uses the following python libraries:pandas, scikit-learn, mgwr, numpy==1.22.4, geopandas.
For information on installing miniconda and the assocated python libraries run 'rgwrc_miniconda_info()'.
For more information on mgwr please vist https://mgwr.readthedocs.io/en/latest/index.html")

  #set the working directory to the library containing the rgwrc package
  #setwd(gsub("/rgwrc","",system.file(package="rgwrc")))

  #user_permission <- utils::askYesNo("Is miniconda already installed?
#No to install miniconda.")

  #if (isFALSE(user_permission)) {
    #R libraries
    #library(dplyr)
    #library(data.table)
    #library(NbClust)
    #library(sf)
    #library(reticulate)

    #reticulate::install_miniconda(path = miniconda_path(), update = TRUE, force = FALSE)

    #install python libraries
    #py_install(packages = c("pandas", "scikit-learn","mgwr"))
    #py_install(packages = c("numpy==1.22.4")) #default MGWR package numpy version is depreciated
    #py_install(packages = c("geopandas"))
    #use_condaenv(condaenv = "r-reticulate", conda = "auto")
    #py_config()
  #} else if ((isTRUE(user_permission))) {
    #R libraries
    #library(dplyr)
    #library(data.table)
    #library(NbClust)
    #library(sf)
    #library(reticulate)
  #}
}
#####set wd upon loading package#####
#.onLoad <- function(libname, pkgname) {
#  setwd(gsub("/rgwrc","",system.file(package="rgwrc")))
#}


#' Mode Function
#'
#' Finds the most occurred feature in a vector
#' @param x input vector
#' @return Mode of the vector
#' @examples
#' x <- c(1,2,4,7,7,7,8,8,11,12,13,13,15);
#' Mode(x);
#' @export
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' Find Mode Function
#'
#' Finds the most occurred group for some given 'clusters' given some number of elements
#' 'near_g' within some vector 'x'.
#' @param x input vector of element distinces to some origin element
#' @param near_g choose the maximum number of elements from x for the Mode function
#' @param clusters x's associated cluster column
#' @return Mode of the vector
#' @examples
#' df <- data.frame(a = c(0,1,1,2,3,4,5), b = c("0","0","0","1","1","1","1"))
#' Find_Mode(df$a, near_g = 3,clusters = df$b)
#' @export
Find_Mode <- function(x,near_g,clusters) {
  #remove self distance calculations from the pool
  #xx <- x[ x != 0 ]
  #Mode(slice_min(order_by = xx, n = near_g))
  xxx <- data.frame(
    "x" = x
    ,"clusters" = clusters
    , stringsAsFactors = FALSE)
  xx <- subset(xxx, x > 0)
  clustx <- xx %>% slice_min( x, n = near_g)
  Mode( clustx[ncol(clustx)][[1]] ) #find the majority cluster
}

#' Near Group Cleanup Function
#'
#' Assigns elements a group based on the group Mode of some n number of nearest elements.
#' Uses matrix operations for fast performance.
#' @param data1 ID and coords
#' @param data2clust ID and clusters
#' @param near_g the number of nearest elements to use for the grouping process
#' @return Mode of the vector
#' @examples
#' FolioID = c('1','2','3','4','5','6','7')
#' x = c(1,2,4,5,1,3,1)
#' y = c(3,5,6,3,1,5,2)
#' clust_1 = c(6,2,2,6,6,2,6)
#' df = data.frame(FolioID,x,y)
#' df2 = data.frame(FolioID,clust_1)
#' near.group.fast.cleanup(df,df2,near_g = 3) #pick odd number for mode function
#' plot(df$x,df$y,col= as.factor(df2$clust_1))
#' plot(df$x,df$y,col=as.factor(cleanclust$clust_1) )
#' @export
near.group.fast.cleanup <- function(data1,data2clust,near_g){ #data can only be FolioID and coords
  #function start
  start_time <- Sys.time()

  data1$FolioID <- data1[,1]
  data1 <- data.frame(
    "FolioID" = data1[,1]
    ,"coordx" = data1[,2]
    ,"coordy" = data1[,3]
    , stringsAsFactors = FALSE)

  data2clust$FolioID <- data2clust[,1]

  data1$FolioID <- as.character(data1$FolioID)
  #data2$FolioID <- as.character(data2$FolioID)
  data2clust$FolioID <- as.character(data2clust$FolioID)
  data1 <- data1[!duplicated(data1[ , c("FolioID")]),]
  #data2 <- data2[!duplicated(data2[ , c("FolioID")]),]
  data2clust <- data2clust[!duplicated(data2clust[ , c("FolioID")]),]
  varname <- colnames(data2clust[2])
  #####construct distance matrix
  dfc <- data1 #rbind(df,data2)
  #dfc <- dfc[!duplicated(dfc[ , c("FolioID")]),]
  dfc$FolioID <- as.character(dfc$FolioID)
  #dfc <- subset(dfc, select=-c(FolioID))
  dfdistm=dist(dfc[,c(2,3)])
  dfdistm=as.matrix(dfdistm, labels=TRUE) #
  #colnames(dfdistm) <- rownames(dfdistm) <- dfc[['FolioID']] #name ids in rows and add id column

  #df = data.frame(FolioID = row.names(dfdistm), dfdistm)
  df = data.frame(dfdistm)
  dflpy <- apply(df, 2,Find_Mode,near_g=near_g,clusters=data2clust[ncol(data2clust)])
  #df2 <- do.call(cbind, dflpy)
  #names(df2) <- NULL

  cleanclust <- data.frame(
    "FolioID" = data1$FolioID
    ,varname = dflpy
    , stringsAsFactors = FALSE)
  names(cleanclust)[names(cleanclust)=="varname"] <- paste0(colnames(data2clust[2]),"_Cleaned")

  .GlobalEnv$cleanclust <- cleanclust

  #####clusterSC Polygons#####
  datalp <- left_join(data1,cleanclust, by = "FolioID")
  xy <- datalp[,c(colnames(cleanclust[2]),'coordx','coordy')]
  names(xy)[names(xy) == colnames(cleanclust[2])] <- 'ID'
  xyc <- nrow(xy)
  xy <- xy[with(xy, ID %in% names(which(table(ID)>=4))), ] #polygons require at least 4 points,
  #this function drops ids in clusters with less than four points
  xyrc <- nrow(xy)
  if (nrow(xy) >= 4) {

    xys = st_as_sf(xy, coords=c('coordx','coordy'))

    polys = xys %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise() %>%
      st_cast("POLYGON")

    polys = polys %>%
      st_convex_hull()

    #polys$area = st_area(polys)
    st_agr(polys) = "constant" #attribute variables are assumed to be spatially constant throughout all geometries

    tryCatch({ #intersection can fail if multiple polygons have close overlaps, tryCatch will let the loop continue if the error occurs

      diffPoly  <- st_intersection(polys) %>%
        filter(n.overlaps == 1)

    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    #plot(diffPoly)
    #plot(st_make_valid(diffPoly) )

    if ( exists("diffPoly") ) { #diffPoly will fail to exist if intersection function has error

      diffPoly$nonoverarea <- st_area(diffPoly)
      #sum(diffPoly$nonoverarea)
      #st_area(st_union(polys))
      pp <- sum(diffPoly$nonoverarea)/st_area(st_union(polys)) #pct non overlap
    } else {
      pp <- NA
    }

  } else {
    pp <- NA
  }

  end_time <- Sys.time()
  #end of run
  cat(paste0("Running Time ",round(end_time - start_time,2) ))
  cat("\n")
  cat(paste0("New Polygon NonOverlap ",pp, sep="\n" ))
}

#' Assign population clusters
#'
#' Assign each element of population their cluster based off the nearest
#' respective model data element cluster.
#' @param data1 Population data.frame containing the ID and coords
#' @param data2 Model data.frame containing the ID, cluster and coords
#' @param crs_frame coordinate reference frame, default is Lat Long
#' @return Mode of the vector
#'
#' @export
near.group.A <- function(data1,data2, crs_frame = 4326){
  data1$FolioID <- data1[,1]
  data1 <- data.frame(
    "FolioID" = data1[,1]
    ,"Longitude" = data1[,2]
    ,"Latitude" = data1[,3]
    , stringsAsFactors = FALSE)
  data2 <- data.frame(
    "FolioID" = data2[,1]
    ,"Longitude" = data2[,2]
    ,"Latitude" = data2[,3]
    ,"cluster" = data2[,4]
    , stringsAsFactors = FALSE)

  data1$FolioID <- as.character(data1$FolioID)
  data2$FolioID <- as.character(data2$FolioID)

  data1 <- data1[!duplicated(data1[ , c("FolioID")]),]
  data2 <- data2[!duplicated(data2[ , c("FolioID")]),]

  data1$Latitude <- as.numeric(data1$Latitude)
  data2$Latitude <- as.numeric(data2$Latitude)

  data1$Longitude <- as.numeric(data1$Longitude)
  data2$Longitude <- as.numeric(data2$Longitude)

  #varname1 <- colnames(data2[4])
  #varname2 <- colnames(data2[4])
  #ames(data1)[names(data1)== paste0(varname)] <- "cluster"
  #names(data2)[names(data2)== paste0(varname2)] <- "cluster"

  #data1 <- subset(data1, cluster == clustnum )


  newgroup <- cbind(
    data1,
    nearest_group = data2[
      st_nearest_feature(
        st_as_sf(data1, coords = c("Longitude", "Latitude"), crs = crs_frame),
        st_as_sf(data2, coords = c("Longitude", "Latitude"), crs = crs_frame)
      ),
    ]$cluster
  )

  #plot(newgroup$Longitude,newgroup$Latitude,col=newgroup$nearest_group)

  newgroup <- newgroup[,c("FolioID","nearest_group")]
  names(newgroup)[names(newgroup)== "nearest_group"] <- paste0(varname2)

  return(newgroup)
}

#' Euclidean distance in 2-D
#'
#' Assign initial fixed equidistant centers for consistent Kmeans results
#' @param x x coord
#' @param y y coord
#' @return eucledian distance between a vector of 2-D points
#'
#' @export
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

#' Initial Kmeans Starting Points
#'
#' Assign initial fixed equidistant centers for consistent Kmeans results
#' @param x coords
#' @param n number of clusters
#' @return Fixed equidistant centers
#'
#' @export
initialKMeansStartingPoints <- function(x, n = 10) {

  points <- numeric()

  dTable <- NULL

  points[1] <- which.min(apply(x, 1, function(c)

  {

    euc.dist(c, rep(0, ncol(x)))

  }))

  for(i in 2:n)

  {

    dTable <- cbind(dTable, apply(x, 1, function(c) { euc.dist(c, x[points[i - 1],])}))

    points[i] <- which.max(apply(dTable, 1, min))
  }
  points

}

#' Geographically Weighted Regression Clustering Best Fit Function
#'
#' Assigns the weighted predictions from a GWR model in addition to coordinates
#' to Kmeans and Hierarchical clustering algorithms. Regression analysis is performed
#' on each clustering iteration up to some designated max to determine what is the optimal neighborhood
#' composition to use. In addition polygon overlap of the clusters is taken into account.
#' @param yf Dependent variable.
#' @param x Independent variables.
#' @param data data.frame.
#' @param direc method to use for step wise regression, options are 'none', 'both', 'forward', 'backward'.
#' The default is 'backward'.
#' @param lonlat IF TRUE calculates the great circle distance, otherwise uses euclidean distance.
#' The default is TRUE.
#' @param weight kernel distribution function: 'gaussian','bisquare', default is 'bisquare'.
#' @param clustnum Number of clusters to run the function over.
#' @param Fixed_K Assign initial starting centers for Kmeans for consistent results, for repeated analysis
#' @param bestC If "Yes" uses the NbClust package to determine the optimal number of clusters, can be time
#' consuming, default is 'No'. Specific methods can be used as the input as well,
#' i.e. 'friedman', see the NbClust function for more methods.
#' @param distmethod The metric system to use for the distance matrix, default is 'euclidean'.
#' Other methods are "maximum", "manhattan", "canberra", "binary" or "minkowski". See dist
#' for more details.
#' @param hclustmethod Method to use for hclust function, default is 'complete'. Other methods:
#' "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid".
#' See hclust for more details.
#' @return GWR summary results for analysis of the overall GWR performance. GWRC summary
#' results that describe the models performance using the given number of clusters as
#' location proxies. In addition a list of data.frames is outputted called 'GWCBFDataCPY'.
#' 'GWCBFDataCPY' contains all the clustering data results as a result of GWRC.
#' @examples
#' #Run the Geographically Weighted Regression Clustering Best Fit function.
#' mgwr_c_bf_cpy(
#' yf = zillow_1k[,c("value"),drop = FALSE],
#' x = zillow_1k[,c("nbaths","nbeds","area","age")],
#' data = zillow_1k,
#' direc = 'backward',
#' lonlat = FALSE,
#' weight = 'gaussian',
#' clustnum = 8,
#' bestC = "No",
#' distmethod = "euclidean",
#' hclustmethod = "complete"
#' )
#' #Import ggplot.
#' library(ggplot2)
#' #view the Kmeans clustering results for 3 clusters.
#' ggplot(GWCBFDataCPY[[3]], aes(coordx, coordy, colour = as.factor(clusterKMeans_3))) +
#' geom_point()
#' @export
mgwr_c_bf_cpy <- function(yf, x, data, direc = 'backward', lonlat = TRUE, weight = 'bisquare', clustnum, Fixed_K = "Yes", bestC = "No", distmethod = "euclidean", hclustmethod = 'complete'
) { #geographic weighted regression best fit calculator

  #yf dependent variable
  #x indepedent variable list (use colnames) ex: my_colnames1 <- colnames(data)
  #direc is the method to use for step wise regression options are 'none', 'both', 'forward', 'backward'
  #clustnum is the number of clusters to run the function over
  #distmethod the metric system to use for the distance matrix
  #hclustmethod method to use for hclust function
  #GWCBFModel <- list()
  GWCBFDataCPY <- list()

  comptable <- data.frame(
    "clusternum" = integer()
    #,"Lin_Model_R2" = numeric()
    ,"HC_Lin_Model_R2" = numeric()
    ,"HC_NonOverlap" = numeric()
    ,"KMeans_Lin_Model_R2" = numeric()
    ,"KMeans_NonOverlap" = numeric()
    #,"AP_Lin_Model_R2" = numeric()
    #,"AP_NonOverlap" = numeric()
    #,"KMeanShift_Lin_Model_R2" = numeric()
    #,"KMeanShift_NonOverlap" = numeric()
    #,"SC_Lin_Model_R2" = numeric()
    #,"SC_NonOverlap" = numeric()
    #,"DBSCAN_Lin_Model_R2" = numeric()
    #,"DBSCAN_NonOverlap" = numeric()
    #,"OPTICS_Lin_Model_R2" = numeric()
    #,"OPTICS_NonOverlap" = numeric()
    #,"Birch_Lin_Model_R2" = numeric()
    #,"Birch_NonOverlap" = numeric()
    , stringsAsFactors = FALSE)

  #standardize
  colnames(data)[1] ="Longitude"
  colnames(data)[2] ="Latitude"
  colnames(data)[3] ="TASP"
  colnames(yf)[1] ="TASP"
  #use reformulate from the stats package to create the formula function
  yf  <- colnames(yf)
  x  <- colnames(x)
  fmla <- reformulate(x, response = yf)
  lm.data.ln <- lm(fmla
                   , data = data)

  #save dataframe to be used in python script
  if ("FolioID" %in% colnames(data)) {
    mgwrdata <- subset(data, select = -c(FolioID))
  } else {
    mgwrdata <- data
  }
  #colnames(mgwrdata)[1] ="Longitude"
  #colnames(mgwrdata)[2] ="Latitude"
  #colnames(mgwrdata)[3] ="TASP"
  mgwrdata <- select(mgwrdata, Longitude, Latitude, TASP, everything()) #data.frame reorder using dplyr

  #save R dataframe as csv for python scripts to call from
  write.csv(mgwrdata, gsub("/","\\\\",file.path(file.path(system.file(package="rgwrc"), "rgwrc_hold"),"sales_r.csv")), row.names=FALSE)

  TFdf <- data.frame( #put results into an R object
    "TF" = lonlat
    , stringsAsFactors = FALSE)

  write.csv(TFdf, gsub("/","\\\\",file.path(file.path(system.file(package="rgwrc"), "rgwrc_hold"),"TFdf_r.csv")), row.names=FALSE)
#set working directory#
#original working directory
prime.wd <- getwd()
#set the working directory to the library containing the rgwrc package for python script
setwd(gsub("/rgwrc","",system.file(package="rgwrc")))
  if (weight == "gaussian") {

    #run python mgwr gaussian kernel
    start_time <- Sys.time()
    #source_python("py_mgwr_gauss_auto.py") #python script
    source_python(system.file("python/py_mgwr_gauss_auto.py",package="rgwrc"))
    end_time <- Sys.time()
    cat(paste0("Python MGWR Running Time ",round(end_time - start_time,2) ))
    #end of run
  } else {
    #run the gwr function with bisquare kernel
    start_time <- Sys.time()
    #source_python("py_mgwr_bisquare_auto.py")
    source_python(system.file("python/py_mgwr_bisquare_auto.py",package="rgwrc"))
    end_time <- Sys.time()
    cat(paste0("Python MGWR Running Time ",round(end_time - start_time,2) ))
    #end of run
  }
#reset working directory to the original working directory
setwd(prime.wd)

  GWResults <- data.frame( #put results into an R object
    "pred" = as.numeric( gwr_adapt_python$predy )
    , stringsAsFactors = FALSE)

  data <- cbind(data,GWResults) #join predicted values back onto the original data frame

  names(data)[names(data)=="Longitude"] <- "coordx"
  names(data)[names(data)=="Latitude"] <- "coordy"

  data.scaled <- subset(data, select = c("pred","coordx","coordy"))
  ####data.scaled <- subset(CSFR, select = c("predsqft","long","coordy")) #use predsqft!!!!
  #data.scaled <- subset(SFRSALES, select = c("long","coordy"))
  data.scaled <- data.scaled[complete.cases(data.scaled$pred),] #Remove NA's

  df <- apply(data.scaled, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
  df <- as.data.frame(df)
  data.scaled$coordy <- df$coordy
  data.scaled$coordx <- df$coordx
  data.scaled$pred <- df$pred ### data.scaled$predsqft <- df$predsqft
  #convert to numeric#
  data.scaled$coordx <- as.numeric(data.scaled$coordx)
  data.scaled$coordy <- as.numeric(data.scaled$coordy)

  #export scaled data to python package
  #write.csv(data.scaled, "C:\\Program Files\\R\\R-4.3.0\\library\\rgwrc\\rgwrc_hold\\datascaled_r.csv", row.names=FALSE)

  #dist from stats package
  dist_mat <- dist(data.scaled, method = distmethod) #'euclidean'
  #hclust from stats package
  #create a cluster dendrogram (using average method)
  hclust_avg <- hclust(dist_mat, method = hclustmethod) #'average'

  if (bestC == "Yes"){
    #load the NbClust package
    ncpc <- NbClust(data.scaled, distance= distmethod, min.nc=2, max.nc= clustnum, method= hclustmethod, index = "all")
    bc <- length(unique(ncpc$Best.partition))
  } else if (bestC == "No") {
    bc <- "No Cluster Fit Method Used"
  } else {
    ncpc <- NbClust(data.scaled, distance= distmethod, min.nc=2, max.nc= clustnum, method= hclustmethod, index = bestC)
    bc <- length(unique(ncpc$Best.partition))
  }
  #set a progress bar
  cat("\n")
  pb <- txtProgressBar(min = 0, max = clustnum - 1, initial = 0)
  #for loop for clustnum
  for( i in 2:clustnum ){

    #export cluster number i

    #Run Python AI clustering Algorthims
    #start_time <- Sys.time()
    #source_python("py_cluster_models.py")
    #end_time <- Sys.time()
    #cat(paste0("Python MGWR Running Time ",round(end_time - start_time,2) ))
    #end of run

    #####R Kmeans Code######

    ######calculate K-means######


    if (Fixed_K == "Yes"){
      # ---- Calculate optimal number of time groups With Eucledian Distance-----
      initial.points <- initialKMeansStartingPoints(data.scaled, i)
      #fixed centers for consistent results
      KmeansT = kmeans(data.scaled, centers = data.scaled[initial.points[1:i],])$cluster
    } else {
      #random centers
      KmeansT = kmeans(data.scaled, centers = i)$cluster
    }

    py.cluster.groups <- data.frame( #put results into an R object
      "clusterKMeans_" = as.factor( KmeansT )
      #,"clusterAP_" = as.factor( AffinityPropagationT )
      #,"clusterKMeanShift_" = as.factor( MeanShiftT )
      #,"clusterSC_" = as.factor( SpectralClusteringT )
      #,"clusterDBSCAN_" = as.factor( DBSCANT )
      #,"clusterOPTICS_" = as.factor( OPTICST )
      #,"clusterBirch_" = as.factor( brcT )
      , stringsAsFactors = FALSE)

    K_Count <- length(unique(  py.cluster.groups$clusterKMeans_)) #determine the count of unique clusters
    #SC_Count <- length(unique( py.cluster.groups$clusterSC_ )) #determine the count of unique clusters

    PYNMS <- names(py.cluster.groups)
    #names(py.cluster.groups) <- paste0(PYNMS, i)
    names(py.cluster.groups)[names(py.cluster.groups) == 'clusterKMeans_'] <- paste0("clusterKMeans_",K_Count )
    #names(py.cluster.groups)[names(py.cluster.groups) == 'clusterSC_'] <- paste0("clusterSC_",SC_Count )


    datalp <- cbind(data,py.cluster.groups)

    #choose the number of clusters to use
    cut_avg <- cutree(hclust_avg, k = i)
    #use mutate from dplyr
    varnameHC <- paste0("clusterHC_",i)
    ##warning '=' will not work with expressions on lhs, use := with !!
    datalp <- mutate(datalp, !! paste0("clusterHC_",i) := cut_avg)
    #data coming out as both a data table and data frame, convert to data frame before proceeding
    datalp <- as.data.frame(datalp) #edit 4-17
    datalp[,ncol(datalp)] <- as.factor(datalp[,ncol(datalp)])
    #create cluster variable binaries
    #use model.matrix from the stats package to create dummy columns

    #HC
    #use as.formula with paste0 so model.matrix function knows which column to choose
    fmlamm <- as.formula(paste0("~ clusterHC_",i, " - 1"))
    cd <- model.matrix(fmlamm, data = datalp)
    xhc <- (paste0(varnameHC, 1:i))
    #use reformulate from the stats package to create the formula function
    fmla2 <- reformulate(c(x,xhc), response = yf)

    #Kmeans
    #i_py_name <- K_Count - 1 #python starts at 0, adjust for the difference
    fmlammKmeans <- as.formula(paste0("~ clusterKMeans_",K_Count, " - 1"))
    cdKmeans <- model.matrix(fmlammKmeans, data = datalp)
    varnameKmeans <- paste0("clusterKMeans_",K_Count)
    xKmeans <- (paste0(varnameKmeans, 1:i))
    fmla2Kmeans <- reformulate(c(x,xKmeans), response = yf)

    #clusterAP
    #fmlammclusterAP <- as.formula(paste0("~ clusterAP_",i, " - 1"))
    #cdclusterAP <- model.matrix(fmlammclusterAP, data = datalp)
    #varnameclusterAP <- paste0("clusterAP_",i)
    #xclusterAP <- (paste0(varnameclusterAP, 1:i))
    #fmla2clusterAP <- reformulate(c(x,xclusterAP), response = yf)

    #clusterKMeanShift
    #fmlammclusterKMeanShift <- as.formula(paste0("~ clusterKMeanShift_",i, " - 1"))
    #cdclusterKMeanShift <- model.matrix(fmlammclusterKMeanShift, data = datalp)
    #varnameclusterKMeanShift <- paste0("clusterKMeanShift_",i)
    #xclusterKMeanShift <- (paste0(varnameclusterKMeanShift, 1:i))
    #fmla2clusterKMeanShift <- reformulate(c(x,xclusterKMeanShift), response = yf)

    #clusterSC
    #i_py_SC_name <- SC_Count - 1 #python starts at 0, adjust for the difference
    #fmlammclusterSC <- as.formula(paste0("~ clusterSC_",SC_Count, " - 1"))
    #cdclusterSC <- model.matrix(fmlammclusterSC, data = datalp)
    #varnameclusterSC <- paste0("clusterSC_",SC_Count)
    #xclusterSC <- (paste0(varnameclusterSC, 0:i_py_SC_name))
    #fmla2clusterSC <- reformulate(c(x,xclusterSC), response = yf)

    #clusterDBSCAN
    #fmlammclusterDBSCAN <- as.formula(paste0("~ clusterDBSCAN_",i, " - 1"))
    #cdclusterDBSCAN <- model.matrix(fmlammclusterDBSCAN, data = datalp)
    #varnameclusterDBSCAN <- paste0("clusterDBSCAN_",i)
    #xclusterDBSCAN <- (paste0(varnameclusterDBSCAN, 1:i))
    #fmla2clusterDBSCAN <- reformulate(c(x,xclusterDBSCAN), response = yf)

    #clusterOPTICS
    #fmlammclusterOPTICS <- as.formula(paste0("~ clusterOPTICS_",i, " - 1"))
    #cdclusterOPTICS <- model.matrix(fmlammclusterOPTICS, data = datalp)
    #varnameclusterOPTICS <- paste0("clusterOPTICS_",i)
    #xclusterOPTICS <- (paste0( varnameclusterOPTICS, 1:i))
    #fmla2clusterOPTICS <- reformulate(c(x,xclusterOPTICS), response = yf)

    #clusterBirch
    #fmlammclusterBirch <- as.formula(paste0("~ clusterBirch_",i, " - 1"))
    #cdclusterBirch <- model.matrix(fmlammclusterBirch, data = datalp)
    #varnameclusterBirch <- paste0("clusterBirchs_",i)
    #xclusterBirch <- (paste0(varnameclusterBirch, 1:i))
    #fmla2clusterBirch <- reformulate(c(x,xclusterBirch), response = yf)



    #run the linear model with the gwr cluster dummy variables
    datacd <- cbind(datalp,cd,cdKmeans) #join all dummyvariables onto the data

    if (direc == 'none') {
      lm.data <- lm(fmla2
                    , data = datacd)
      lm.data.sum <- summary(lm.data)
      #Kmeans
      lm.data.Kmeans <- lm(fmla2Kmeans
                           , data = datacd)
      lm.data.sum.Kmeans <- summary(lm.data.Kmeans)
      #clusterAP
      #lm.data.clusterAP <- lm(fmla2clusterAP
      #                     , data = datacd)
      #lm.data.sum.clusterAP <- summary(lm.data.clusterAP)
      #clusterKMeanShift
      #lm.data.clusterKMeanShift <- lm(fmla2clusterKMeanShift
      #                     , data = datacd)
      #lm.data.sum.clusterKMeanShift <- summary(lm.data.clusterKMeanShift)
      #clusterSC
      #lm.data.clusterSC <- lm(fmla2clusterSC
      #                     , data = datacd)
      #lm.data.sum.clusterSC <- summary(lm.data.clusterSC)
      #clusterDBSCAN
      #lm.data.clusterDBSCAN <- lm(fmla2clusterDBSCAN
      #                     , data = datacd)
      #lm.data.sum.clusterDBSCAN <- summary(lm.data.clusterDBSCAN)
      #clusterOPTICS
      #lm.data.clusterOPTICS <- lm(fmla2clusterOPTICS
      #                     , data = datacd)
      #lm.data.sum.clusterOPTICS <- summary(lm.data.clusterOPTICS)
      #clusterBirch
      #lm.data.clusterOPTICS <- lm(fmla2clusterOPTICS
      #                     , data = datacd)
      #lm.data.sum.clusterOPTICS <- summary(lm.data.clusterOPTICS)

    } else {
      lm.data <- lm(fmla2
                    , data = datacd)
      lm.data <- step(lm.data, direction = c(direc),trace=0) #trace = 0 suppress the output
      lm.data.sum <- summary(lm.data)
      #Kmeans
      lm.data.Kmeans <- lm(fmla2Kmeans
                           , data = datacd)
      lm.data.Kmeans <- step(lm.data.Kmeans, direction = c(direc),trace=0) #trace = 0 suppress the output
      lm.data.sum.Kmeans <- summary(lm.data.Kmeans)
      #clusterAP
      #lm.data.clusterAP <- lm(fmla2clusterAP
      #                     , data = datacd)
      #lm.data.clusterAP <- step(lm.data.clusterAP, direction = c(direc),trace=0) #trace = 0 suppress the output
      #lm.data.sum.clusterAP <- summary(lm.data.clusterAP)
      #clusterKMeanShift
      #lm.data.clusterKMeanShift <- lm(fmla2clusterKMeanShift
      #                     , data = datacd)
      #lm.data.clusterKMeanShift <- step(lm.data.clusterKMeanShift, direction = c(direc),trace=0) #trace = 0 suppress the output
      #lm.data.sum.clusterKMeanShift <- summary(lm.data.clusterKMeanShift)
      #clusterSC
      #lm.data.clusterSC <- lm(fmla2clusterSC
      #                     , data = datacd)
      #lm.data.clusterSC <- step(lm.data.clusterSC, direction = c(direc),trace=0) #trace = 0 suppress the output
      #lm.data.sum.clusterSC <- summary(lm.data.clusterSC)
      #clusterDBSCAN
      #lm.data.clusterDBSCAN <- lm(fmla2clusterDBSCAN
      #                     , data = datacd)
      #lm.data.clusterDBSCAN <- step(lm.data.clusterDBSCAN, direction = c(direc),trace=0) #trace = 0 suppress the output
      #lm.data.sum.clusterDBSCAN <- summary(lm.data.clusterDBSCAN)
      #clusterOPTICS
      #lm.data.clusterOPTICS <- lm(fmla2clusterOPTICS
      #                     , data = datacd)
      #lm.data.clusterOPTICS <- step(lm.data.clusterOPTICS, direction = c(direc),trace=0) #trace = 0 suppress the output
      #lm.data.sum.clusterOPTICS <- summary(lm.data.clusterOPTICS)
      #clusterBirch
      #lm.data.clusterBirch <- lm(fmla2clusterBirch
      #                            , data = datacd)
      #lm.data.clusterBirch <- step(lm.data.clusterBirch, direction = c(direc),trace=0) #trace = 0 suppress the output
      #lm.data.sum.clusterBirch <- summary(lm.data.clusterBirch)

    }

    ####PctGroupOverlap######
    #paste0("clusterHC_",i)
    xy <- datalp[,c(paste0("clusterHC_",i),'coordx','coordy')]
    names(xy)[names(xy) == paste0("clusterHC_",i)] <- 'ID'
    xyc <- nrow(xy)
    xy <- xy[with(xy, ID %in% names(which(table(ID)>=4))), ] #polygons require at least 4 points,
    #this function drops ids in clusters with less than four points
    xyrc <- nrow(xy)
    if (nrow(xy) >= 4) {

      xys = st_as_sf(xy, coords=c('coordx','coordy'))

      polys = xys %>%
        dplyr::group_by(ID) %>%
        dplyr::summarise() %>%
        st_cast("POLYGON")

      polys = polys %>%
        st_convex_hull()

      #polys$area = st_area(polys)
      st_agr(polys) = "constant" #attribute variables are assumed to be spatially constant throughout all geometries

      tryCatch({ #intersection can fail if multiple polygons have close overlaps, tryCatch will let the loop continue if the error occurs

        diffPoly  <- st_intersection(polys) %>%
          filter(n.overlaps == 1)

      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      #plot(diffPoly)
      #plot(st_make_valid(diffPoly) )

      if ( exists("diffPoly") ) { #diffPoly will fail to exist if intersection function has error

        diffPoly$nonoverarea <- st_area(diffPoly)
        #sum(diffPoly$nonoverarea)
        #st_area(st_union(polys))
        pp <- sum(diffPoly$nonoverarea)/st_area(st_union(polys)) #pct non overlap
      } else {
        pp <- NA
      }

    } else {
      pp <- NA
    }

    #####Kmeans Polygons#####
    xy <- datalp[,c(paste0("clusterKMeans_",K_Count),'coordx','coordy')]
    names(xy)[names(xy) == paste0("clusterKMeans_",K_Count)] <- 'ID'
    xyc <- nrow(xy)
    xy <- xy[with(xy, ID %in% names(which(table(ID)>=4))), ] #polygons require at least 4 points,
    #this function drops ids in clusters with less than four points
    xyrc <- nrow(xy)
    if (nrow(xy) >= 4) {

      xys = st_as_sf(xy, coords=c('coordx','coordy'))

      polys = xys %>%
        dplyr::group_by(ID) %>%
        dplyr::summarise() %>%
        st_cast("POLYGON")

      polys = polys %>%
        st_convex_hull()

      #polys$area = st_area(polys)
      st_agr(polys) = "constant" #attribute variables are assumed to be spatially constant throughout all geometries

      tryCatch({ #intersection can fail if multiple polygons have close overlaps, tryCatch will let the loop continue if the error occurs

        diffPoly  <- st_intersection(polys) %>%
          filter(n.overlaps == 1)

      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      #plot(diffPoly)
      #plot(st_make_valid(diffPoly) )

      if ( exists("diffPoly") ) { #diffPoly will fail to exist if intersection function has error

        diffPoly$nonoverarea <- st_area(diffPoly)
        #sum(diffPoly$nonoverarea)
        #st_area(st_union(polys))
        ppKMeans <- sum(diffPoly$nonoverarea)/st_area(st_union(polys)) #pct non overlap
      } else {
        ppKMeans <- NA
      }

    } else {
      ppKMeans <- NA
    }

    #lm.data <- lm(fmla2
    #              , data = datacd)
    #print(paste0("clusterHC_",i, "_R_Square_Value_" , summary(lm.data)$r.squared))
    GWCBFDataCPY[[i]] <- datacd
    #GWCBFModel[[i]] <- lm.data


    comptable[nrow(comptable) + 1,] <- list(
      i
      #,summary(lm.data.ln)$r.squared
      ,lm.data.sum$r.squared
      ,pp  #getting negative values check later
      #KMeans
      ,lm.data.sum.Kmeans$r.squared
      ,ppKMeans
      #clusterAP
      #,lm.data.sum.clusterAP$r.squared
      #,ppclusterAP
      #clusterKMeanShift
      #,lm.data.sum.clusterKMeanShift$r.squared
      #,ppclusterKMeanShift
      #clusterSC
      #,lm.data.sum.clusterSC$r.squared
      #,ppclusterSC
      #clusterDBSCAN
      #,lm.data.sum.clusterDBSCAN$r.squared
      #,ppclusterDBSCAN
      #clusterOPTICS
      #,lm.data.sum.clusterDBSCAN$r.squared
      #,ppclusterDBSCAN
      #clusterBirch
      #,lm.data.sum.clusterDBSCAN$r.squared
      #,ppclusterDBSCAN
    )
    setTxtProgressBar(pb, i - 1)
  }
  #end for loop
  #
  #
  #.GlobalEnv$GWCBFModel <- GWCBFModel
  .GlobalEnv$GWCBFDataCPY <- GWCBFDataCPY
  .GlobalEnv$GWCBFcomptable <- comptable

  #use par() to plot the graph as a single
  par(mfrow = c(1, 1))
  plot(comptable$clusternum, comptable$HC_NonOverlap,
       ylab='Goodness of Fit',
       xlab='Number of Clusters',
       main='Highest Performing HC Cluster Model', col = 'black',pch = 16 )
  points(comptable$clusternum, comptable$HC_Lin_Model_R2, col = 'green',pch = 16)
  abline(h= max(comptable$HC_Lin_Model_R2), col = 'dark red')
  #Kmeans plot
  plot(comptable$clusternum, comptable$KMeans_NonOverlap,
       ylab='Goodness of Fit',
       xlab='Number of Clusters',
       main='Highest Performing KMeans Cluster Model', col = 'black',pch = 16  )
  points(comptable$clusternum, comptable$KMeans_Lin_Model_R2, col = 'green',pch = 16)
  abline(h= max(comptable$KMeans_Lin_Model_R2), col = 'dark red')
  #SC plot
  #plot(comptable$clusternum, comptable$SC_NonOverlap,
  #    ylab='Goodness of Fit',
  #   xlab='Number of Clusters',
  #     main='Highest Performing KMeans Cluster Model', col = 'black',pch = 16  )
  #points(comptable$clusternum, comptable$SC_Lin_Model_R2, col = 'green',pch = 16)
  #abline(h= max(comptable$SC_Lin_Model_R2), col = 'dark red')
  #reset par() options
  #dev.off()

  #print(p2)
  #nothing else runs after return command
  cat("\n")
  cat(paste0("Data Count ",nrow(data), sep="\n" ))
  cat("\n")
  cat(paste0("GWR Quasi-Global R2 ",gwr_adapt_python$R2, sep="\n" ))
  cat("\n")
  cat(paste0("Linear Model with No Locations R2 ",summary(lm.data.ln)$r.squared, sep="\n" ))
  cat("\n")
  cat(paste0("NbClust Best Number Of Clusters ",bc, sep="\n" ))
  cat("\n")
  cat(paste0("The Cluster Model Used ",direc, sep="\n" ))
  cat("\n")
  cat("---------------------------------------------------------------------------------------------------------\n")
  return(comptable)
}

#' GWR
#'
#' Find the GWR predicted values given some Model data
#' @param salesdata Model data.frame first 3 columns must be ordered coords then sales
#' @param kernel kernel distribution function: 'gaussian','bisquare','gaussian' is the default
#' @return predicted values data.frame and GWR summary results
#' @examples
#' mgwrdata <- zillow_1k
#'
#' gwrresults <- gwr_py(salesdata = mgwrdata, kernel = "bisquare", latlong = FALSE)
#'
#' @export
gwr_py <- function(salesdata = mgwrdata, kernel = 'gaussian', latlong = TRUE) {
  #data must start with coords then the dependent variable, then the rest
  colnames(mgwrdata)[1] ="Longitude"
  colnames(mgwrdata)[2] ="Latitude"
  colnames(mgwrdata)[3] ="TASP"
  #save R dataframe as csv for python scripts to call from
  write.csv(mgwrdata, gsub("/","\\\\",file.path(file.path(system.file(package="rgwrc"), "rgwrc_hold"),"sales_r.csv")), row.names=FALSE)

  #is latitude and longitude being used
  TFdf <- data.frame( #put results into an R object
    "TF" = latlong
    , stringsAsFactors = FALSE)

  write.csv(TFdf, gsub("/","\\\\",file.path(file.path(system.file(package="rgwrc"), "rgwrc_hold"),"TFdf_r.csv")), row.names=FALSE)
#set working directory#
#original working directory
prime.wd <- getwd()
#set the working directory to the library containing the rgwrc package for python script
setwd(gsub("/rgwrc","",system.file(package="rgwrc")))
  if ( kernel == 'gaussian' ) {
    start_time <- Sys.time()
    #source_python("py_mgwr_gauss_auto.py")
    source_python(system.file("python/py_mgwr_gauss_auto.py",package="rgwrc"))
    end_time <- Sys.time()
    cat(paste0("Python MGWR Running Time ",round(end_time - start_time,2) ))
  } else {
    start_time <- Sys.time()
    #source_python("py_mgwr_bisquare_auto.py")
    source_python(system.file("python/py_mgwr_bisquare_auto.py",package="rgwrc"))
    end_time <- Sys.time()
    cat(paste0("Python MGWR Running Time ",round(end_time - start_time,2) ))
  }
#reset working directory to the original working directory
setwd(prime.wd)
  GWResults <- data.frame( #put results into an R object
    "pred" = as.numeric( gwr_adapt_python$predy )
    , stringsAsFactors = FALSE)
 return(GWResults)
}

#' GWR Predict
#'
#' Find the GWR predicted values for the population given some Model data.
#' Include an ID column in the table as the 4th column to preserve order.
#' @param salesdata Model data.frame first 3 columns must be ordered coords then sales
#' @param populationdata Population data.frame first 2 columns must be ordered coords
#' @param kernel kernel distribution function: 'gaussian','bisquare','gaussian' is the default
#' @return predicted value data.frame of the Model assigned
#' directly to the global environment, predicted value data.frame for
#' the population assigned directly to the global environment, and GWR summary results
#' @examples
#' mgwrdata <- head(zillow_1k,300) # the model data with independent variable
#' mgwrpopData <- tail(subset(zillow_1k, select = -value),700)
#' gwr.predict_py(
#'   salesdata = mgwrdata,
#'   populationdata = mgwrpopData,
#'   kernel = "bisquare",
#'   latlong = FALSE
#' )
#' @export
gwr.predict_py <- function(salesdata = mgwrdata, populationdata = mgwrpopData, kernel = 'gaussian', latlong = TRUE) {
  #data must start with coords then the dependent variable, then the rest
  colnames(mgwrdata)[1] ="Longitude"
  colnames(mgwrdata)[2] ="Latitude"
  colnames(mgwrdata)[3] ="TASP"
  #save R dataframe as csv for python scripts to call from
  write.csv(mgwrdata, gsub("/","\\\\",file.path(file.path(system.file(package="rgwrc"), "rgwrc_hold"),"sales_r.csv")), row.names=FALSE)
  mgwrpopData$Longitude <- mgwrpopData[,1]
  mgwrpopData$Latitude <- mgwrpopData[,2]
  mgwrpopData <- select(mgwrpopData, Longitude, Latitude, everything()) #data.frame reorder using dplyr
  #save R dataframe as csv for python scripts to call from
  write.csv(mgwrpopData, gsub("/","\\\\",file.path(file.path(system.file(package="rgwrc"), "rgwrc_hold"),"pop_r.csv")), row.names=FALSE)

  split <- data.frame( #put results into an R object
    "splitnum" = ceiling(nrow(mgwrpopData)/nrow(mgwrdata))
    , stringsAsFactors = FALSE)

  write.csv(split, gsub("/","\\\\",file.path(file.path(system.file(package="rgwrc"), "rgwrc_hold"),"split_r.csv")), row.names=FALSE)

  #is latitude and longitude being used
  TFdf <- data.frame( #put results into an R object
    "TF" = latlong
    , stringsAsFactors = FALSE)

  write.csv(TFdf, gsub("/","\\\\",file.path(file.path(system.file(package="rgwrc"), "rgwrc_hold"),"TFdf_r.csv")), row.names=FALSE)
#set working directory#
#original working directory
prime.wd <- getwd()
#set the working directory to the library containing the rgwrc package for python script
setwd(gsub("/rgwrc","",system.file(package="rgwrc")))
  if ( kernel == 'gaussian' ) {
    start_time <- Sys.time()
    #source_python("py_gwr_predict_auto_gauss.py")
    source_python(system.file("python/py_gwr_predict_auto_gauss.py",package="rgwrc"))
    end_time <- Sys.time()
    cat(paste0("Python MGWR Running Time ",round(end_time - start_time,2) ))
  } else {
    start_time <- Sys.time()
    #source_python("py_gwr_predict_auto.py")
    source_python(system.file("python/py_gwr_predict_auto.py",package="rgwrc"))
    end_time <- Sys.time()
    cat(paste0("Python MGWR Running Time ",round(end_time - start_time,2) ))
  }
#reset working directory to the original working directory
setwd(prime.wd)

  GWRModelResults <- data.frame( #put results into an R object
    #"FolioID" = sales$FolioID
    "pred" = as.numeric( gwr_adapt_python$predy ) #,"pred" = exp( as.numeric( gwr_adapt_python$predy ) )#log transformation
    , stringsAsFactors = FALSE)
  .GlobalEnv$GWRModelResults <- GWRModelResults

  GWRPredictResults <- data.frame( #put results into an R object
    #"FolioID" = popData$FolioID
    "pred" = as.numeric( unlist(lapply(gwr_results_list, '[[', 'predy')) ) #unpack the predicted column from the MGWR results
    , stringsAsFactors = FALSE)
  #nrow(subset(GWRPredictResults, pred > 0))

  .GlobalEnv$GWRPredictResults <- GWRPredictResults
  #GWRData <- rbind(GWRModelResults,GWRPredictResults)
  #return(GWRData)
}

#' Empty rgwrc_hold
#'
#' Finds all the files in the rgwrc_hold directory and then deletes them.
#' @return Deletes all files in the rgwrc_hold directory.
#' @examples
#' empty_rgwrc_hold();
#' @export
empty_rgwrc_hold <- function() {
  f <- list.files(file.path(system.file(package="rgwrc"), "rgwrc_hold"), include.dirs = F, full.names = T, recursive = T)
  # remove the files
  file.remove(f)
}

#' MiniConda Installation Info
#'
#' Tells the user how to install miniconda and the python packages to use.
#'
#' @examples
#' rgwrc_miniconda_info();
#' @export
rgwrc_miniconda_info <- function() {
message("#####Install reticulate to serve as the link between R and Miniconda#####
install.packages(\"reticulate\")
library(reticulate) #Attach reticulate
#####Install Miniconda#####
install_miniconda(path = miniconda_path(), update = TRUE, force = FALSE)
use_condaenv(condaenv = \"r-reticulate\", conda = \"auto\") #Select the version of Python to be used by reticulate
py_config() #Retrieve information about the version of Python currently being used by reticulate
#####Install the Pyhton libraries needed for the rgwrc pckage#####
py_install(packages = c(\"pandas\")) #<- package names need quotations i.e. \"pandas\"
py_install(packages = c(\"numpy==1.22.4\")) #default MGWR package numpy version is depreciated
py_install(packages = c(\"geopandas\"))
py_install(packages = c(\"scikit-learn\"))
py_install(packages = c(\"mgwr\"))
        ")
}

#' Convert a list of trees to a above ground biomass raster
#'
#' Function that creates a raster of above ground biomass (AGB) from a list of trees.
#' The raster can either represent crown-distributed or stem-localized AGB.
#' @param trees.dt A list of trees provided as a data.table
#' with specific requirements: Each row represents one individual. The columns need to
#' have the following names and content: X = X-coordinate of tree position, Y = Y-coordinate
#' of tree position; CD = tree crown diameter (m), AGB = above ground biomass (t). Ideally the 
#' data should also have a column TreeID with unique IDs for each individual.
#' @param minx Minimal X-coordinate
#' @param maxx Maximal X-coordinate
#' @param miny Minimal Y-coordinate
#' @param maxy Maximal Y-coordinate
#' @param dist Parameter specifying how AGB will be distributed in space: options are "stem" 
#' for stem-localized, "uniform" for uniformly crown-distributed or "Gaussian" for bell shaped
#' crown-distributed.
#' @param periodic Boolean enabling periodic boundaries, i.e. if set to TRUE crown parts of trees 
#' that exceed the edges of the area reappear on the opposite side.
#' @param sd Standard deviation of the optional Gaussian distribution (default = 0.25)
#' @return Raster object
#' @keywords raster rasterization above ground biomass AGB treelist tons crown distributed stem localized uniform Gaussian
#' @export
#' @examples in progress
#' @author Nikolai Knapp, nikolai.knapp@ufz.de

biomass.raster.from.treelist.dt <- function(trees.dt, minx=0, maxx, miny=0, maxy, dist="uniform", periodic=F, sd=0.25){
  
  # Package requirements
  require(data.table)
  
  # Settings for testing
  #     minx <- 0
  #     maxx <- 100
  #     miny <- 0
  #     maxy <- 100
  #     dist="Gaussian"
  #     trees.dt <- data.table(trees.df)
  #   trees.dt <- data.table(tree.list)
  
  # Convert treelist to data.table
  trees.dt <- data.table(trees.dt)
  # Subset trees that are inside the area
  trees.dt <- subset(trees.dt, X >= minx & X < maxx & Y >= miny & Y < maxy)
  # If necessary add tree IDs 
  if(!("TreeID" %in% names(trees.dt))){
    trees.dt[, TreeID := 1:nrow(trees.dt)]
  }
  # Create rows for all coordinate combinations and bind them 
  maxID <- max(trees.dt$TreeID, na.rm=T)
  
  # Stem-localized case
  if(dist == "stem"){
    trees.dt[, X := round(X)]
    trees.dt[, Y := round(Y)]
    
    # Add empty area coordinates, to include areas without trees
    coord.dt <- data.table(expand.grid(X=minx:maxx, Y=miny:maxy, AGB=0))
    trees.dt <- rbind(trees.dt, coord.dt, fill=T)
    
    # Cast AGB values into X-Y-space to create a matrix. Sum all AGB of different
    # trees that fall into the same pixel
    suppressWarnings(mx <- acast(data=trees.dt, Y~X, value.var="AGB", fun.aggregate=sum))
  }
  # Crown-distributed case
  else {
    # Repeat each row in the tree data.table as many times as there are pixels in the
    # bounding box in Y direction
    trees.pixel.dt <- trees.dt[rep(1:.N, ceiling(CD))]
    # Number the replicates
    trees.pixel.dt[, Ybox := 1:.N, by=c("TreeID")]
    
    # Repeat each row in the tree data.table as many times as there are pixels in the
    # bounding box in X direction
    trees.pixel.dt <- trees.pixel.dt[rep(1:.N, ceiling(CD))]
    # Number the replicates
    trees.pixel.dt[, Xbox := 1:.N, by=c("TreeID", "Ybox")]
    
    # Shift the bounding boxes to the respective tree XY-positions
    trees.pixel.dt[, Xbox := round(Xbox + X - CD/2)]
    trees.pixel.dt[, Ybox := round(Ybox + Y - CD/2)]
    
    # Subset only pixels that fall into the circular tree crown areas
    trees.pixel.dt <- subset(trees.pixel.dt, in.circle(Xcor=Xbox, Ycor=Ybox, Xctr=X, Yctr=Y, radius=CD/2))
    
    # Count the pixels of each tree
    trees.pixel.dt[, PixelCount := .N, by=TreeID]
    
    # Uniform distribution
    if(dist == "uniform"){
      # Calculate AGB that each tree contributes to each pixel if AGB were uniformly
      # distributed inside the crown area
      trees.pixel.dt[, AGB.dist := AGB/PixelCount]
      # Gaussian distribution  
    } else if(dist == "Gaussian"){
      # Calculate AGB that each tree contributes to each pixel if AGB were Gaussian
      # distributed inside the crown area
      calc.2D.gaussian.density <- function(sd, w, Xctr, Yctr, Xcor, Ycor){
        distance <- ((Xcor-Xctr)^2+(Ycor-Yctr)^2)^0.5
        norm.distance <- distance/w
        output <- dnorm(norm.distance, mean=0, sd=sd)
        # output <- exp(-5*norm.distance^2)
        return(output)
      }
      trees.pixel.dt[, Gauss.density := calc.2D.gaussian.density(sd=sd, w=CD, Xctr=X, Yctr=Y, Xcor=Xbox, Ycor=Ybox)]
      trees.pixel.dt[, sum.Gauss.density := sum(Gauss.density), by="TreeID"]
      trees.pixel.dt[, AGB.dist := AGB*Gauss.density/sum.Gauss.density]
    }
  
    # Deal with the margins
    if(periodic == F){
      # Without periodic boundaries option just cut off every pixel that falls
      # outside the area borders
      trees.pixel.dt <- subset(trees.pixel.dt, Xbox >= minx & Xbox < maxx & Ybox >= miny & Ybox < maxy)
    } else {
      # With periodic boundaries option let tree crowns which exceed the area
      # borders reappear at the opposite side using modulo division of coodinates
      trees.pixel.dt[, Xbox := Xbox %% maxx]
      trees.pixel.dt[, Ybox := Ybox %% maxy]
    }
    
    # Add empty area coordinates, to include areas without trees
    coord.dt <- data.table(expand.grid(Xbox=minx:maxx, Ybox=miny:maxy, AGB.dist=0))
    trees.pixel.dt <- rbind(trees.pixel.dt, coord.dt, fill=T)
    
    # Cast AGB values into X-Y-space to create a matrix. Sum all AGB of different
    # trees that fall into the same pixel
    suppressWarnings(mx <- acast(data=trees.pixel.dt, Ybox~Xbox, value.var="AGB.dist", fun.aggregate=sum))
  }
  
  # Convert the matrix into a raster
  agb.ras <- rowmx2ras(mx)
  agb.ras <- setExtent(agb.ras, c(minx, maxx, miny, maxy))
  return(agb.ras)
}

















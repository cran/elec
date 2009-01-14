`maximumMarginBound` <-
function( Z, votes=NULL ) {
## return the maximum margin reduction for each precint by computing
## all margin reductions between pairs of winners & losers and then
## scaling by that pair's total margin to get a proportion and then
## taking the max of all such proportions (usually will be the last 
## winner to the closest loser).
## Return:   Vector (of length of precincts) of maximum possible error for 
##           each precinct.
    
  if ( is.null( Z$winners ) ) {
    stop( "Need to count votes before computing margins in computeMargins()" )
  }
  if ( is.null( Z$Ms ) ) {
    stop( "Need to compute pairwise margins before computing error bounds." )
  }
  if ( is.null( Z$V[[Z$tot.votes.col]] ) ) {
    stop( "Vote total column, ", Z$tot.votes.col, " not found." )
  }
  
  
  err.bound = data.frame( row.names = row.names(Z$V) )
  
  for ( i in Z$winners ) {
    for ( j in Z$losers ) {	
      mrg = paste(i,j,sep="_")
      err.bound[[mrg]] = (Z$V[[Z$tot.votes.col]] + Z$V[[i]] - Z$V[[j]])/Z$Ms[[i,j]]		
    }
  }

  res = apply( err.bound, 1, max )
  
  if ( !is.null( votes ) ) {
  	stopifnot( !is.null( votes$PID ) && is.character( votes$PID ) )
  	res[ votes$PID ]
  } else {
  	res
  }
  
}


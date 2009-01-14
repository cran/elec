`elec.data` <-
function( V, C.names=names(V)[2:length(V)], f = 1, 
  audit=NULL, pool=TRUE, tot.votes.col="tot.votes", PID.col="PID" ) {
  ## Make the 'Z' matrix that holds all the vote totals, etc., as well as some
  ## precomputed information such as vote margins between candidates, the theoretical
  ## winners, and so on.
  ##
  ## make.Z does some cleaning and renaming of the passed data structure.  In particular
  ## it will rename the tot.votes column to "tot.votes" if it is not that name already.
  ##
  ## Param    V: Voter matrix OR 2-element list with Voter Matrix followed 
  ##             by Candidate names
  ##       pool: Combine small candidates into single pseudo-candidates to increase 
  ##             power
  ##
  ## Return:  A "elec.data" data structure. 
  ##         Note: Will _add_ PID (precinct ID) column (and generate PIDs)
  ##               if no PID provided.  Also, rownames
  ##               are always PIDs (so indexing by PID works)


  computeMargins = function( Z ) {
    ## Used by make.Z
    ##
    ## Return: the pairwise margins (as # of votes) as a data.frame with the rows being
    ## the winners and the columns being the losers.
    
    if ( is.null( Z$winners ) ) {
      stop( "Need to count votes before computing margins in computeMargins()" )
    }
    Ms = data.frame( row.names = Z$winners )
    
    for ( j in Z$losers ) {
      Ms[[j]] = rep(0, length(Z$winners))	
      
      for ( i in Z$winners ) {
        
        Ms[[ i, j ]] = Z$totals[[i]] - Z$totals[[j]]
        
      }
    }

    Ms
  }



  Z = list()
  class( Z ) = "elec.data"
  
  if ( length(V) == 2 ) {
    C.names = V[[2]]
    V = V[[1]]
  }
  
  Z$N = length(V[[1]])     # number of precincts
  Z$C = length(C.names)    # number of candidates
  Z$f = f                  # number of possible winners

  Z$V = V
  Z$C.names = C.names

  ##	Z$alpha = 0.10
  ##	Z$a_s = rep(0.5,10)^(1:10)

  stopifnot( all( Z$C.names %in% names(Z$V) ) )
  
  if ( is.numeric(tot.votes.col) ) {
  	tot.votes.col = names(V)[[tot.votes.col]]
  }
  if ( is.na(l <- match( tot.votes.col, names(V) ) ) ) {
    warning( "No tot.votes.col found in vote matrix--will be recounting" )
  } else {
  	names(Z$V)[l] = "tot.votes"
  }
  Z$tot.votes.col = "tot.votes"

  Z = countVotes(Z)

  if( !is.null( audit ) ) {
    stopifnot( all( Z$C.names %in% names(audit) ) )
    Z$audit = audit
  }

  ## Pooling!
  ln = length(Z$losers)
  if ( pool && 
      ln > 2 && ( (Z$totals[Z$losers[ln]] + Z$totals[Z$losers[ln-1]]) < Z$totals[Z$losers[1]] )) {
    
    grb = ln - 1
    while( grb > 1 &&  sum( Z$totals[Z$losers[grb:ln]] ) < Z$totals[Z$losers[1]] ) {
      grb = grb - 1
    }
    grb = grb + 1
    to.pool = Z$losers[grb:ln]
    
    Z$V[["pool"]] = apply(Z$V[to.pool],1,sum)
    Z$V = Z$V[setdiff(names(Z$V),to.pool)]
    if ( !is.null( audit )) {  # pool the audit info too.
      Z$audit[["pool"]] = apply(Z$audit[to.pool],1,sum)
      Z$audit = Z$audit[setdiff(names(Z$audit),to.pool)]
    }
    Z$C.names = c( setdiff(Z$C.names, to.pool ), "pool" )
    
    Z$C = length(Z$C.names)    # number of candidates
    Z = countVotes(Z)
    ln = ln - 1

  }
  
  ## Compute pairwise margins between all winners and losers
  Z$Ms = computeMargins( Z )
  
  if ( is.numeric(PID.col) ) {
  	names(Z$V)[PID.col] = "PID"
  } else if ( is.na(l <- match( PID.col, names(V) ) ) ) {
    warning( "No PID.col found in vote matrix--will be generated" )
  } else {
  	names(Z$V)[l] = "PID"
  }
  Z$PID.col = "PID"

  if ( is.null( Z$V$PID ) ) {
    PID = paste( "P", rownames(V), sep="-" )
    names(PID) = "PID"
    Z$V = cbind( PID, Z$V )
  }	
  Z$V$PID = as.character(Z$V$PID)
  stopifnot( sum(duplicated(Z$V$PID)) == 0 )
  rownames(Z$V) = Z$V$PID
  
  Z
}


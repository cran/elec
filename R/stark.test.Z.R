`stark.test.Z` <-
function( Z, 
  calc.e_p=calc.pairwise.e_p,
  w_p = weight.function("no.weight"),
  max_err = maximumMarginBound,
  bound.col = Z$tot.votes.col,
  strat.col = NULL,
  drop=NULL,
  strat.method = NULL,
  err.override = NULL,
  n = NULL, t = NULL, q = NULL
  ) {
  
  ## Param Z: The object holding all the voting information
  ##    In particular: Z$V:     The table of reported votes
  ##                   Z$audit: The table of audits as differences from 
  ##                            recorded votes
  ## Param calc.e_p: The Function used to calculate maximum error bounds
  ## Param      w_p: The function used to calculate weights of error
  ##                 (A list of two functions)
  ##   	    max_err: Function to compute _max_ error bounds for each precint
  ##      strat.col: Name of column that determines how to stratify
  ##          t,n,q: Elements of the test statistic.  Can pass to avoid computation
  ##                 if those values are already known (e.g., for a simulation)
  
   ##stopifnot( !is.null( strat.col) && is.null(strat.method) )
  stopifnot( is.null(drop) || !is.null(Z$V[[drop]]) )
  
  if ( is.null( t ) ) { 	
    res = compute.stark.t( Z, bound.col, calc.e_p, w_p, err.override=err.override,
      return.revised.audit = TRUE)
    Z$audit = res[[2]]
    t = res[[1]]
  }
  
                                        #cat( "weight function = " )
                                        #print( w_p )
  
  if ( is.null( q ) ) {
    Z$V$e.max = max_err( Z )
    q = find.q( Z$V, t, bound.col, Z$margin, w_p=w_p, drop=drop )
  }
  
  
                                        # computing sample size.
  if ( is.null( n ) ) {
    n = length(Z$audit[[1]])
    passed_n = NULL
  } else {
    ## compute scaling of n
    passed_n = n / length(Z$audit[[1]])
  }
  
  
  ## Once q has been computed, calculate final p-values.
  
  
  if ( is.null( strat.col ) ) {
	if ( !is.null(drop) ) {
 	 	eff.N = Z$N - sum(Z$V[[drop]])
 	} else {
  		eff.N = Z$N
  	}
    p.value = find.stark.SRS.p( eff.N, n, q )
    method = "Stark's Election Audit Test (SRS Audit)"
    DAT = paste( "# precincts = ", eff.N, " of ", Z$N, ", f/C=", Z$f, " of ", 
      Z$C, ", n=",n, sep="")
    
  } else {
	## Analysis as if the minimum sampling fraction applied
	##  everywhere.  Bound from sampling *with* replacement

	if ( !is.null(drop) ) {
		stop( "dropped column and stratification code not working right--no drop col allowed" )
	}
	
	## Find stratification levels ###
    strat = find.stratification( Z$V, Z$audit, strat.col )
        	
    ## The following takes out completely missed strata for which there
    ## is no audit data, and assigns maximum error to it
    not.audited = strat[[strat.col]][strat$sample<0.001]
    big.N = eff.N
    if ( length( not.audited ) > 0 ) {
      
      skipped = Z$V[[strat.col]] %in% not.audited
      cat( "# precincts missed", length(Z$V[skipped,]$e.max), " prec in ", length( not.audited ), "\n")
      M.red = sum( Z$V[skipped,]$e.max )
      
                                        #print( Z$V[skipped,] )
      
      qp = find.q( Z$V[!skipped, ], t, bound.col, Z$margin, threshold=(1-M.red), w_p=w_p )
                                        #               warning( "Some strata have no audit data--will subtract max error to get B=", 
                                        #               round( (1-M.red), digits=3), ", and q reduced to ", qp, " from ", q )
      
      big.N = big.N - (q - qp) 
      cat( " del q ", (q-qp), round( M.red, digits=3 ), "\n" )
      q = qp
    }
    
    ## find effective n by taking smallest audit percentage of strata (other than 0).
    n <- floor(big.N*min(strat$sample[strat$sample>=0.001]))
    if ( !is.null(passed_n) ) { 
      n = floor( passed_n *big.N*min(strat$sample[strat$sample>=0.01]))
    }
    p.value = pbinom(0,n,q/big.N)
    method = "Stark's Election Audit Test (Conservative Stratafication)"
    DAT = paste( "# precincts = ", Z$N, ", f/C=", Z$f, " of ", 
      Z$C, ", n=",length(Z$audit[[1]]), 
      " (", n, ")", sep="")
    if ( big.N != Z$N ) {
      DAT = paste( DAT, " N'=", big.N )
    }
  } # end stratification block
  
  names(t) = "max err";
  names(q) = "q";
  
  structure(list(statistic = t, p.value = p.value,
                 method=method, parameter=q,
                 data.name = DAT,
                 Z=Z, n=n), class = "htest")
}


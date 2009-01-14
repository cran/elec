`CAST.calc.sample` <-
function( Z, beta = 0.9, stages=2, t=3, 
				small.cut = NULL,
				strata=NULL, drop=NULL, 
				method=c("select", "binomial","hypergeometric"),
				calc.e.max = TRUE,
				bound.function = maximumMarginBound ) {

	method=match.arg(method)
	
	if ( !is.null(strata) && is.null( Z$V[[strata]] ) ) {
		warning( "No strata column of name '", strata, "'--assuming single strata" )
		strata=NULL
	}
	
	if ( is.null( drop ) ) {
		Z$V$known = FALSE
		drop="known"
	}
	
	beta1 = beta^(1/stages)
	
	# convert t to fraction of margin
	if ( t >= 1 ) { t = t / Z$margin }
	
	# Ignore small precincts?  Even if not,
	# there is no point in auditing precincts smaller than
	# t.
	if ( is.null( small.cut ) ) {
		small.cut = t
	} else if ( small.cut >= 1 ) {
		small.cut = small.cut / Z$margin
	}
	
	if ( calc.e.max ) {
		Z$V$e.max = bound.function( Z )
	}
	
	Z$V$small = Z$V$e.max <= small.cut
	
	# do not consider all precincts that are either known or too small to pay
	# attention to.
	skip = Z$V[[drop]] | Z$V$small
	Z$V$skip = skip
	
	if ( is.null(strata) ) {
		strat.size = sum( !skip )
	} else {
		strat.size = table( Z$V[ !skip, strata ] )
	}
	
	# Reduce the threshold by all non-known precincts that we are skipping,
	# as we must assume the error in them is as large as possible.
	threshold = 1 - sum( Z$V$e.max[ !Z$V[[drop]] & Z$V$small ] )
	N = Z$N - sum(skip)
	
	if ( threshold <= 0 ) {
		q = -1
		n = Z$N
		
	} else {
		
	q = find.q( Z$V, drop=skip, t, Z$tot.votes.col, threshold=threshold )
	
	stopifnot( N >= q )
	
	
	if ( method=="select") {
		method = ifelse( length(strat.size)==1, "hypergeometric","binomial")
	}
	
	if ( q == 0 ) {
		n = N
	} else if ( method=="hypergeometric" ) {
		n = ceiling( uniroot( function(x) { 
			lchoose( N - q, round(x) ) - lchoose( N, round(x) ) - log(1-beta1) }, 
							  c(0,N-q) )$root )
	} else {
		n = ceiling( log( 1-beta1 ) / log( ( N - q ) / N ) )
	}
	
	}
	
	if ( n > N ) {
		n = N
	}

	ns = ceiling( n * strat.size / N )
	
	# calculate estimated work
	V2 = Z$V[ !skip, ]
	if ( is.null( strata ) ) {
		E.votes = round( mean(V2$tot.votes) * ns, digits=1 )
	} else {
		mns = tapply( V2$tot.votes, V2[[strata]], mean )
		E.votes = round( ns * mns[names(ns)], digits=1 )
	}
	
	res = list( beta=beta, beta1 = beta1, stages=stages, t=t, q=q, N=N, n=n, 
				stratas=strat.size, strata=strata, ns=ns, method=method, Z=Z, 
				threshold=threshold, skipped = sum(skip), 
				bound.function=bound.function,
				E.votes=E.votes )
	class(res) = "audit.plan"
	res
}


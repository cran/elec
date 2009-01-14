`make.sample` <-
function( M, N, strata=1, per.winner=NULL, 
						worst.e.max = NULL, 
						R = NULL, 
						tot.votes=100000 ) {
	if ( !is.null( per.winner ) ) {
		v = c( per.winner, per.winner - M, 1 - 2 * per.winner + M )
		names(v) = c("WNR", "LSR", "OTR" )
		stopifnot( min(v) >= 0 && max(v) < 1 )
		stopifnot( v[[1]] == max(v) )
	} else {
		stopifnot( M < 1 && M > 0 )
		v = c( (1+M) / 2, (1-M)/2 )
		names(v) = c("WNR", "LSR" )
	}
	
	st = paste( "ST", 1:strata, sep="-" )
	V = round( (tot.votes / N) * matrix( rep( v, N ), ncol=length(v), byrow=TRUE ) )
	V = data.frame( V )
	V = cbind( paste("P", 1:N, sep="-"), rep( st, length.out=N ), apply( V, 1, sum ), V )
	V[[1]] = as.character( V[[1]] )
	names(V) = c( "PID", "strata", "tot.votes", names(v) )

	if ( !is.null( worst.e.max ) ) {
		# calc how bad worst prec is, and then use p(x) = c/sqrt(x)
		# as density function of precincts.  Invert the CDF, and eval
		# at orders (1:N)/N to get the new precinct sizes.  Scale
		# and go!
		t1 = V[1, ]
		e.max = t1$tot.votes - t1$LSR + t1$WNR
		stopifnot( is.null( R ) )
		R = worst.e.max / e.max
	}
	if ( !is.null(R) ) {   # using the ratio of bad to baseline, go.
		stopifnot( R > 2 )
		p = (R-2)/(R-1)
		C = (1-p)/R^(1-p)
		wts = ( ((1:N)/N) / (C * (R-1)) )^(R-1) 
		
		V[names(v)] = round( V[names(v)] * wts )
		V["tot.votes"] = apply( V[names(v)], 1, sum )
	}
	
	make.Z( V, C.names=names(v) )
}


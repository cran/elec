`trinomial.bound` <-
function( n = 11, k = 2, d=40, e.max=100, 
				xlim=c(0.4,1), ylim=c(0,0.55),
				alpha.lvls=c(10),
				zero.threshold = 0.3,
				tick.lines=NULL,
				alpha.lwd = 2, 
				bold.first=FALSE,
				plot=TRUE,
				p.value.bound = NULL,
				grid.resolution=300,
				... ) {
					
					
					
## chance of getting something as or even less extreme
## than k errors of size d and no errors larger.
## (Step-Down Set)
prob.S = function( p_0, p_d = 1-p_0, n=11, k = 2 ) {
	
	if ( p_0 + p_d > 1 ) {
		NA
	} else {
		ks = 0:k
	
		sum( sapply( ks, function( x ) { 
			choose( n, x ) * ( p_0 ^ (n - x) ) * ( (p_d) ^ x ) 
		} ) )
	}	
}

## Derivative of above w.r.t p_0
d.prob.S.d.p_0 = function( p_0, p_d = 1-p_0, n=11, k = 2 ) {
	
	if ( p_0 + p_d > 1 ) {
		NA
	} else {
		ks = 0:k
	
		sum( sapply( ks, function( x ) { 
			choose( n, x ) * (n - x) * ( p_0 ^ (n - x - 1) ) * ( (p_d) ^ x ) 
		} ) )
	}
	
}

## Derivative of above w.r.t p_d
d.prob.S.d.p_d = function( p_0, p_d = 1-p_0, n=11, k = 2 ) {
	
	if ( p_0 + p_d > 1 ) {
		NA
	} else {
		ks = 1:k
	
		sum( sapply( ks, function( x ) { 
			choose( n, x ) * (x) * ( p_0 ^ (n - x) ) * ( (p_d) ^ (x-1) ) 
		} ) )
	}
}


## calc prob.S on vector of p_0s
prob.Ss = function( x, ... ) { sapply( x, prob.S, ... ) }

	
## the bound given the probability vector
## with, if desired, the lambda penalty for 
## the alpha constraint.
err.bound = function( p_0, p_d, n = 11, k = 2, d=40, 
					e.max=100, 
					penalize=FALSE, alpha=0.05, ... ) {
	if ( p_0 + p_d > 1 ) {
		NA
	} else {
		b = p_d * d + (1 - p_0 - p_d) * e.max
		if ( penalize ) {
			b = b - 100000 * (prob.S( p_0, p_d, n, k ) - alpha)^2
		}
		b
	}
}

## gradient of error bound function, with d/dp_0 and d/dp_d
grad.err.bound = function( x, d=40, e.max=100, 
						penalize=TRUE, alpha=0.05, ... ) {
	p_0 = x[1]
	p_d = x[2]
	
	if ( p_0 + p_d > 1 ) {
		c( NA, NA )
	} else {
		prS = prob.S( p_0, p_d )
		
		c( -e.max - 200000 * (prS - alpha) * d.prob.S.d.p_0(p_0, p_d, ... ),
			d - e.max - 200000 * (prS - alpha) * d.prob.S.d.p_d(p_0, p_d, ... ) )
	}
}



## the bound function that takes probs as a list
err.bound.pen = function( x, ... ) {
   err.bound( x[1], x[2], ... )
}

					
					
					
	p0s = seq( xlim[[1]], xlim[[2]], length.out = grid.resolution )
	pds = seq( ylim[[1]], ylim[[2]], length.out = grid.resolution )

	## PLOT THE ALPHA LINES
	vSS <- Vectorize(prob.S, c("p_0", "p_d"))

	# outer product, make 2D array applying vSS to all values of p0s and pds
	alphas = 100 * outer( p0s, pds, vSS, n=n, k=k )
	
	if ( plot ) {
		lwd = rep( alpha.lwd, length( alpha.lvls ) )
		if ( bold.first ) {
			lwd[1] = 2 * lwd[1]
		}
		contour( p0s, pds, alphas, levels=alpha.lvls, lwd=lwd, frame=FALSE, 
				xlab=expression(pi['0']), 
				ylab=bquote( pi['d'] ~ ", d =" ~ .(round(d,digits=2)) ),
				method="edge", ... )
	
		## BOUNDING BOX
		abline( a = 1, b = -1 )
		abline( h=0 )
	}
	
	## THE BOUND CONTOUR LINES
	vBND = Vectorize( err.bound, c("p_0","p_d") )

	bounds = outer( p0s, pds, vBND, n=n, k=k, d=d)
	
	if ( plot ) {
		contour( p0s, pds, bounds, lty=3, add=TRUE )
		if ( !is.null( tick.lines ) ) {
			contour( p0s, pds, bounds, lty=5, levels=tick.lines, add=TRUE )
		}
	}
	
	## Finding the max along the main alpha line

	alpha = alpha.lvls[[1]]
	del = abs( alphas - alpha ) < zero.threshold
	zt = zero.threshold

	while( sum( del, na.rm=TRUE ) < 75 ) {
		zt = zero.threshold + zt
		warning( "zero threshold too small--increasing it to ", zt )
		del = abs( alphas - alpha ) < zt
	}

	del = !is.na(del) & del
	b = bounds
	b[ !del ] = 0
	loc = which.max(b) - 1
	loc.y = pds[ 1 + floor( loc / ncol(b) ) ]
	loc.x = p0s[ 1 + loc %% ncol(b) ]
	if ( plot ) {
		points( loc.x, loc.y, pch=19 )
	}
	
	## find the p.value corresponding to the passed bound
	if (!is.null( p.value.bound ) ) {
		
		del = abs( bounds - p.value.bound ) < zero.threshold / 5
		del = !is.na(del) & del
		a = alphas
		a[ !del ] = 0
		loc = which.max(a) - 1
		p.value = max(a)
		loc.y = pds[ 1 + floor( loc / ncol(a) ) ]
		loc.x = p0s[ 1 + loc %% ncol(a) ]
		if ( plot ) {
			points( loc.x, loc.y, pch=20, col="red" )
		}
	} else {
		p.value = NULL
	}

		
	list( n=n, k=k, d=d, e.max=e.max, max=max(b), 
			p=c(loc.x, loc.y, 1-loc.x-loc.y),
			p.value=p.value ) 
}


`tri.calc.sample` <-
function( Z, beta=0.75, guess.N = 20, p_d = 0.1, swing = 5, 
							power=0.90,
							bound = c("e.plus", "WPM", "passed") ) {

	bound = match.arg(bound)
	
	if ( bound == "passed" ) {
		if ( is.null( Z$V$e.max ) ) {
			stop( "No e.max column, and told to not calculate it" )
		}
	} else if ( bound == "e.plus" ) {
		Z$V$e.max = maximumMarginBound( Z )
	} else {
		Z$V$e.max = 0.4 * Z$V[[Z$tot.votes.col]] / Z$margin
	}
	
	## Compute the taints that would be due to swings as large as 
	## passed.
	Z$V$swing.ep = calc.pairwise.e_p( Z, Z$V, err.override=swing )
	Z$V$taint = Z$V$swing.ep / Z$V$e.max
	
	V = Z$V[ order( Z$V$e.max, decreasing=TRUE ), ]
	cumMass = cumsum(V$e.max)
	
	T = sum( V$e.max )
	if ( p_d > 0 ) {
		P = (power^(1/guess.N) - 1 + p_d) / p_d
		cut = sum( cumMass <= P * T)
		d.plus=max(V$taint[1:cut])
	} else { 
		stopifnot( p_d == 0 )
		P = 1
		cut = nrow(V)
		d.plus=0
	}
	
	n = ceiling( log( 1 - beta ) / log( 1 + d.plus * p_d - 1/T ) )
	
	## calculated expected amount of work
	Ep = nrow(V) - sum( (1-V$e.max/T)^n )
	Evts =  sum( V[[Z$tot.votes.col]] * ( 1 - (1-V$e.max/T)^n ) )
	
	res <- list( beta=beta, stages=1, beta1=beta, 
				P=P, cut=cut, swing=swing, d.plus=d.plus, p_d = p_d, T=T, n=n,
				E.p = Ep, E.vts = Evts,
				bound=bound,
				Z = Z )
				
	class(res) <- "audit.plan.tri"
	res	
}


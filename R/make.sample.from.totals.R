`make.sample.from.totals` <-
function( vote.W, vote.L, totals ) {
	
	GT = sum(totals)
	v = c( vote.W/GT, vote.L/GT )
	names(v) = c("WNR", "LSR" )
	stopifnot( min(v) >= 0 && max(v) < 1 )
	stopifnot( v[[1]] == max(v) )
	

	N = length( totals )	
	V = matrix( rep( v, N ), ncol=length(v), byrow=TRUE )
	V = data.frame( V )
	V = cbind( rep( "ST-1", length.out=N ), totals, V )
	names(V) = c( "strata", "tot.votes", names(v) )

	V[names(v)] = round( V[names(v)] * totals )
	
	make.Z( V, C.names=names(v) )
}


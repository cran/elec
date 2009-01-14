`make.truth.ex.bad` <-
function( Z ) {
	n = nrow(Z$V)
	stopifnot( n %% 8 == 0 )
	C1 = c( rep( 80, n/8 ), rep( 124, 7*n/8 ) )
	C2 = c( rep( 160, n/8 ), rep( 113, 7*n/8 ) )
	C3 = c( rep( 13, n/8 ), rep( 15, 7*n/8 ) )
	reord = sample( 1:n, n )
	
	V = data.frame( strata = rep( c(rep(c("S1","S1.VBM"), 3),"S2","S2.VBM"), n/8 ),
					tot.votes = rep( 255, n ),
					C1=C1[reord], C2=C2[reord], C3=C3[reord] )
			
	PID = paste( V$strata, rownames(V), sep="-" )
	names(PID) = "PID"
	V = cbind( PID, V )
	V$PID = as.character(V$PID)
	
	make.Z( V, c( "C1", "C2", "C3" ) )
}


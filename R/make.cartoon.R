`make.cartoon` <-
function( n = 400, vote.dist=c(125,113,13), stratify=TRUE ) {
	n = n - n %% 4
	
	V = data.frame( strata = rep( c(rep(c("S1","S1.VBM"), 3),"S2","S2.VBM"), n/4 ),
					tot.votes = rep( 255, 2*n ),
					C1 = rep( vote.dist[[1]], 2*n ),
					C2 = rep( vote.dist[[2]], 2*n ),
					C3 = rep( vote.dist[[3]], 2*n ) )
	
				
	if ( !stratify ) {
		V$strata = "S1"
	}
	PID = paste( V$strata, rownames(V), sep="-" )
	names(PID) = "PID"
	V = cbind( PID, V )
	V$PID = as.character(V$PID)
	
	
	make.Z( V, c( "C1", "C2", "C3" ) )
}


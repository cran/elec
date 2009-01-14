`do.audit` <-
function( Z, truth, audit.names, ns=NULL ) {
	
	if ( is.null( audit.names ) ) {
		audit.names = CAST.sample( ns )
	}
	rownames(truth$V) = truth$V$PID
	truth$V = truth$V[ Z$V$PID, ]  # get in same order
	
	os = truth$V
	os[Z$C.names] =  Z$V[Z$C.names] - os[Z$C.names]
	os$clear = apply( os[Z$C.names], 1, function(X) { sum( abs(X) ) == 0 } )
	os = os[ os$PID %in% audit.names, ]
	
	os
}


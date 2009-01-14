`audit.totals.to.OS` <-
function( Z, audit ) {
	stopifnot( !is.null( Z$V$PID ) )
	stopifnot( !is.null( audit$PID ) )
	stopifnot( all( Z$C.names %in% colnames(audit) ) )
	stopifnot( all( audit$PID %in% rownames(Z$V) ) )
	audit$PID = as.character(audit$PID)
	
	audit[ Z$C.names ] = Z$V[ audit$PID, Z$C.names ] - audit[ Z$C.names ]  
	if ( is.null( audit$tot.votes ) ) {
		audit$tot.votes = Z$V[ audit$PID, "tot.votes" ]
	}
	
	audit
}


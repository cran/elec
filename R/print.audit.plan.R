`print.audit.plan` <-
function( x, ... ) {
  P = x
	cat( "Audit plan: beta=", P$beta, "stages=", P$stages, " beta1=", P$beta1, 
			"\n\t\tt=", P$t, "q=", P$q, "N=", P$N, "/", P$Z$N, "\tn=", P$n, "met=", P$method,
			"\n\t\tskipped=", P$skipped, " %mar lost =", (1-P$threshold), " E[votes audted] =", sum(P$E.votes), "\n" )
	if ( length(P$stratas) > 1 ) {
		tb =  rbind( P$stratas, P$ns, P$E.votes )
		rownames(tb) = c( "N", "n", "E.vts" )
		colnames(tb) = names(P$ns)
		print(tb)
	}
}


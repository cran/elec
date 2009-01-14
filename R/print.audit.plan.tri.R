`print.audit.plan.tri` <-
function( x, ... ) {
	cat( "Audit plan: beta=", x$beta, "  stages=", x$stages, "  beta1=", x$beta1, 
			"\n\t\td^+=", x$d.plus, " (vote swing of ", x$swing, ")    p_d=", x$p_d, 
			"\n\t\tP=", x$P, "  cut=", x$cut, "  T=", x$T,
			"\n\t\tn=", x$n, "  met=", "  PPEB w/ ", x$bound, sep="",
			"\n\t\tE[# pcts audited]=", round( x$E.p, digits=1), "   E[votes audited]=", round(x$E.vts), "\n" )
#			"\n\t\tskipped=", x$skipped, " %mar lost =", (1-x$threshold), "\n" )
#	if ( length(x$stratas) > 1 ) {
#		print( rbind( x$stratas, x$ns ) )
#	}
}


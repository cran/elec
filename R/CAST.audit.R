`CAST.audit` <-
function( Z, audit=NULL, plan=NULL, ... ) {
	if ( is.null( plan ) ) {
		plan = CAST.calc.sample( Z, ... )
	}

	browser()
	if ( is.null(audit) ) {
		plan$Z$audit = Z$audit
	} else {
		plan$Z$audit = audit
	}
		
	res = stark.test.Z( plan$Z, drop="skip", max_err=plan$bound.function,
			bound.col=Z$tot.votes.col,
			strat.col=plan$strata )
	res$certify = res$p.value < (1 - plan$beta1)
	
	cat( "Certify election: ", res$certify, "\n" )
	res
}


`trinomial.audit` <-
function( Z, audit ) {

	audit$e.max = maximumMarginBound( Z, audit )
	audit = audit.totals.to.OS(Z, audit)
	errs = compute.audit.errors( Z, audit=audit, 
					w_p=weight.function("taint"), bound.col="e.max"  )
			
	audit$MOS = errs$err * Z$margin
	audit$taint = errs$err.weighted
	k = sum( (audit$taint > 0) * (audit$count) )
	d.max = max(audit$taint)

	list( n=sum(audit$count), k=k, d.max=d.max, audit=audit )
}


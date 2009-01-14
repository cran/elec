`sim.race` <-
function( n=800, beta=0.75, stages=2, 
						truth.maker=make.truth.opt.bad,
						print.trail=FALSE) {

	Z = make.cartoon(n=n)
	Z$V$known = FALSE
	rownames(Z$V) = Z$V$PID
	Z
	
	truth = truth.maker(Z)
	rownames(truth$V) = truth$V$PID
	truth
			
	s = 0
	tots = rep( 0, stages )

	while ( s < stages && (s == 0 || t > samp.info$t) ) {
		s = s + 1
		samp.info = CAST.calc.sample( Z, stages=stages, beta=beta, drop="known" )
		
		tots[s] = sum( samp.info$ns )
		
		## add entropy and then use this function to generate table of audits
		audit.names = CAST.sample( Z, samp.info, 
							print.trail=print.trail, known="known" )
		aud = do.audit( Z, truth, audit.names )
		Z$audit = aud
		
		t = compute.stark.t( Z, "tot.votes" )
		
		if ( t > samp.info$t ) { # escalate!
			# replace count information with the 'true' audit
			# information for all audited precincts.
			Z$V[audit.names,] = truth$V[audit.names,]
			# mark these precincts as known so that future stages will
			# not audit again.
			Z$V[ audit.names, "known" ] = TRUE
			# rebuild Z to update margin totals, etc.
			Z = make.Z( Z$V, Z$C.names )  
		}
	}
	c( s, sum(tots), t < samp.info$t )
}


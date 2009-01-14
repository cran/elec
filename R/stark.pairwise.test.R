`stark.pairwise.test` <-
function( votes, audits, C.names=NULL, f=1, pool=TRUE, ... ) {
  ## Pairwise test: Look at all pairs of winners and losers, compute the p-value
  ## according to methods defined by passed parameters, and then return the
  ## worst p-value, using the bound that the chance that _all_ the statistics would be
  ## that small under their respective H0 is bounded by the chance that the hardest-to-
  ## detect statistic would be that small.
  ## param     C.names:  if NULL will derive from cols 2 on of votes
  ## param   strat.col:  if NULL will not stratify
  
  all.Z= make.Z(votes, C.names, f, audit=audits, pool=pool )
  
  max.pv = -1
  best.T = NULL

  for ( w in all.Z$winners ) {
    for ( l in all.Z$losers ) {
      ##cat( "Examining ", w, " beating ", l, "\n" )
      Z = make.Z( all.Z$V, C.names=c(w, l), 1, audit=all.Z$audit, pool=FALSE )
      T = stark.test.Z( Z, ... )
      ##print(T)
      if ( T$p.value > max.pv ) {
        max.pv = T$p.value
        best.T = T
      }
      ##print( "finished iteration" )
    }
  }

  best.T$method = paste( best.T$method, "(pairwise)" )
  best.T$data.name = paste( best.T$data.name, " (overall F/C=",all.Z$f, "/", all.Z$C, ")", sep="" )
  
  best.T
}


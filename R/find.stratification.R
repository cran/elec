`find.stratification` <-
function( D, aud, strat.col ) {
  ## Finding how audit interacted with stratification levels for a table of votes and audits
  ## param     D:    Table of votes
  ##         aud:    Table of audit data
  ##     strat.col:  The column to use that identifies the stratification levels
  ## Return: Table of strata, for each strata (row) the name of the strata,
  ##         the number of precincts, the number of audited precincts
  ##         and percent of precincts audited is returned.
  if ( !(strat.col %in% names(D) ) ) {
    stop( "Cannot find stratification since strat.col '", strat.col, "' is not in vote data object." )
  }
  
  if ( !( strat.col %in% names(aud) ) ) {
  	stop( "Cannot find stratification since strat.col '", strat.col, "' is not in audit data object." )
  }
  
  nPrecincts <- table( D[[strat.col]] )
  audPrecincts = table( aud[[strat.col]] )
  tbl = merge( nPrecincts, audPrecincts, by="Var1", all.x=TRUE )

  names(tbl) = c(strat.col, "n", "audit" )
  tbl$audit[is.na(tbl$audit)] = 0
  tbl$sample = tbl$audit / tbl$n
  
  tbl
}


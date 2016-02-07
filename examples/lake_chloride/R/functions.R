massFlowStrict <- function(times, m_lake, pars) {
  # Calculate the mass-flow rate
  print(pars['c_lands'])
  rate <- pars['c_lands'] * pars['v_lands'] - (pars['v_lands'] + pars['v_flushs'])*(1/pars['v_lakes'])*pars['m_lakes']
  return(list(rate)) # return rate as a list-type
}
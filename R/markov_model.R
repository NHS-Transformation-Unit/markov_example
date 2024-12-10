markov_model <- function(state_names, start_states, transition_probs, cycles = 10) {
  
  num_states <- length(start_states)
  states <- matrix(0, nrow = cycles + 1,
                   ncol = num_states
                   )
  states[1, ] <- start_states
  
  for (i in 1:cycles){
    
    states[i + 1, ] <- states[i, ] %*% transition_probs
    
  }

  colnames(states) <- state_names
  
  return(states)
    
}

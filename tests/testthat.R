library(testthat)

source("R/markov_model.R")

state_names <- c("Healthy", "Mild Disease", "Severe Disease", "Deceased")
start_states <- c(1000, 200, 100, 0)
transition_probs <- matrix(
  c(0.90, 0.08, 0.02, 0.00,  # Healthy to Healthy, Mild, Severe, Deceased
    0.00, 0.85, 0.10, 0.05,  # Mild to Healthy, Mild, Severe, Deceased
    0.00, 0.00, 0.80, 0.20,  # Severe to Healthy, Mild, Severe, Deceased
    0.00, 0.00, 0.00, 1.00), # Deceased is an absorbing state
  nrow = 4, byrow = TRUE
)


# Test 1 - Function returns matrix with correct dimensions ----------------

test_that("markov_model returns a matrix of correct dimensions", {
  result <- markov_model(state_names, start_states, transition_probs, cycles = 5)
  
  # Check the number of rows (cycles + 1) and columns (number of states)
  expect_equal(dim(result), c(6, 4))  # 5 cycles + initial state = 6 rows, 4 states = 4 columns
  
  # Check the column names match the state names
  expect_equal(colnames(result), state_names)
})


# Test 2 - Check the counts after the first and second cycles -------------

test_that("markov_model returns correct counts for each state after first and second cycles", {
  result <- markov_model(state_names, start_states, transition_probs, cycles = 2)
  
  # Expected counts after the first cycle
  expected_cycle_1 <- c(
    (1000 * 0.90) + (200 * 0.00) + (100 * 0.00),  # Healthy
    (1000 * 0.08) + (200 * 0.85) + (100 * 0.00),  # Mild Disease
    (1000 * 0.02) + (200 * 0.10) + (100 * 0.80),  # Severe Disease
    (1000 * 0.00) + (200 * 0.05) + (100 * 0.20)   # Deceased
  )
  
  names(expected_cycle_1) <- state_names
  
  # Expected counts after the second cycle
  expected_cycle_2 <- c(
    (expected_cycle_1[1] * 0.90) + (expected_cycle_1[2] * 0.00) + (expected_cycle_1[3] * 0.00) + (expected_cycle_1[4] * 0),
    (expected_cycle_1[1] * 0.08) + (expected_cycle_1[2] * 0.85) + (expected_cycle_1[3] * 0.00) + (expected_cycle_1[4] * 0),
    (expected_cycle_1[1] * 0.02) + (expected_cycle_1[2] * 0.10) + (expected_cycle_1[3] * 0.80) + (expected_cycle_1[4] * 0),
    (expected_cycle_1[1] * 0.00) + (expected_cycle_1[2] * 0.05) + (expected_cycle_1[3] * 0.20) + (expected_cycle_1[4] * 1.00)
  )
  
  names(expected_cycle_2) <- state_names
  
  # Check the counts for each state after the first cycle (Cycle 1)
  expect_equal(result[2, ], expected_cycle_1, tolerance = 1e-2)
  
  # Check the counts for each state after the second cycle (Cycle 2)
  expect_equal(result[3, ], expected_cycle_2, tolerance = 1e-2)
})

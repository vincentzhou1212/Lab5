#####################################   Question 1   ######################################
## Modify your function from the Problem 2 (Lab5 Activity). The function should simulate N 
## rounds of the game (instead of just one) and return the proportion of times you win the 
## bet. Run the function with N = 1000 and 10000.

## Create the function
batting.result = function(number, time){
  die = c(1:6)
  answer = sample(die, size = time, replace = TRUE)
  if(any(answer == number)) {
    result = TRUE
  }
  else {
    result = FALSE
  }
  return(result)
}

batting.simulate = function(N){
  counter = 0
  for (i in 1:N) {
    move = batting.result(6, 4)
    if (move == TRUE) {
      counter = counter + 1
    }
  }
  return(counter/N)
}

## Run the function with N = 1000 
batting.simulate(1000)

## Run the function with N = 10000
batting.simulate(10000)

#####################################   Question 2   ######################################
## Write a function that will find the smallest element of a given vector. Your function 
## should return the smallest element and index of the smallest element.

min.element = function (v) {
  s.elem = v[1]
  s.index = 1
  for (i in 2:length(v)) {
    if (v[i] < s.elem) {
      s.elem = v[i]
      s.index = i
    }
  }
  paste("smallest element -", s.elem, ", and index is", s.index, ".")
}


get.next.permutation <- function() {
  # Find longest non-increasing suffix
  i <- length(curr)
  while (i > 1 && curr[i-1] >= curr[i]) { 
    i <- i-1
  }
  
  # Now i is the head index of the suffix
  
  # Are we at the last permutation already?
  if (i <= 1)
    return(0) # done 
  
  # Let curr[i - 1] be the pivot
  # Find rightmost element that exceeds the pivot
  j = length(curr)
  while (curr[j] <= curr[i - 1]) {
    j <- j-1
  }
  # Now the value curr[j] will become the new pivot
  # Assertion: j >= i
  
  # Swap the pivot with j
  temp = curr[i - 1]
  curr[i - 1] = curr[j]
  curr[j] = temp
  
  # Reverse the suffix
  j = length(curr)
  while (i < j) {
    temp = curr[i]
    curr[i] = curr[j]
    curr[j] = temp
    i <- i + 1
    j <- j - 1
  }
  
  # Successfully computed the next permutation
  return(curr)
}




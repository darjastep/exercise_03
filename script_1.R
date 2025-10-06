
# task 2

IndexOfMin <- function (array, first, last) {
   index <- first
   for (k in (first + 1):last) {
     if (array[k] < array[index]) {
       index <- k
      }
    }
  return (index)
}


print(IndexOfMin(c(2,3,5,1,8,7),1,5))




# task 3
SelectionSort <- function(array, n) {
  for (i in 1:(n - 1)) {
    j <- which.min(array[i:n]) + (i - 1)
    
    if (i != j) {
      temp <- array[i]
      array[i] <- array[j]
      array[j] <- temp
    }
  }
  return(array)
}

print(SelectionSort(c(5,9,3,4,1), 6))

# task 4
RecursiveSelectionSort <- function(array, first, last) {
  if (first < last) {
    index <- IndexOfMin(array, first, last)
    
    #swap array first and array index
    temp <- array[first]
    array[first] <- array[index]
    array[index] <- temp

    array <- RecursiveSelectionSort(array, first + 1, last)
  }
  return(array)
}

print(RecursiveSelectionSort(c(5,9,3,4,1,9),1, 6))

      
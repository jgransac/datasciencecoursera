## Put comments here that give an overall description of what your
## functions do
## Two functions. makeCacheMatrix is building an object matrix with functions. This object is able to indicate if it can return its inverted matrix or not.
## cacheSolve retrieved the cached inverted matrix if it exists. If not , cachesolve calculates the inverted matrix using the special matrix object and its function to:
## - get the original matrix
## - cache the inverted matrix in the special object

## Write a short comment describing this function
## function makeCacheMatrix is able to set and get matrix inverted. This is the special matrix and cache object.

makeCacheMatrix <- function(x = matrix()) {
  MatInvert <- NULL ## when using this function, we rebuild the matrix to invert
  set <- function(y) { ## set function is used in case we want to reuse the special matrix and set it other values
    x <<- y
    MatInvert <<- NULL
  }
  get <- function() x ## get the original matrice not inverted yet
  setsolve <- function(a_MatInvert) MatInvert <<- a_MatInvert ## set the inverted matrix, this funcrion should not be used by any user, just the cacheSolve fnction
  getsolve <- function() MatInvert ## return the inverted matrix. can be used by user, but built for cacheSolve 
  list(set = set, get = get, ## list of function available to special matrix object
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
## function cacheSolve is computing the inverted matrix from an object special matrix if the inverted matrix is not cached.
## The function cachesolve is able to return its inversion if it exists.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matinverted <- x$getsolve() ## if matrix inverted not cached, then calculation
  if(!is.null(matinverted)) {
    message("getting cached data")
    return(matinverted)
  }
  ## the cache is null: calculation of the matrix inverted and cache it
  data <- x$get()
  matinverted <- solve(data, ...)
  x$setsolve(matinverted) ## store the cachee in the special matrix object
  matinverted ## return cache
}


## TESTS

## > cx=rbind(c(10, -1/4), c(-1/4, 10))  
## > cx
## [,1]  [,2]
## [1,] 10.00 -0.25
## [2,] -0.25 10.00

## > X<-makeCacheMatrix(cx)
## > X$getsolve()
## NULL ## nothing calculated yet

## > cacheSolve(X)
##             [,1]        [,2]
## [1,] 0.100062539 0.002501563
## [2,] 0.002501563 0.100062539

## > cacheSolve(X)
## getting cached data
##             [,1]        [,2]
## [1,] 0.100062539 0.002501563
## [2,] 0.002501563 0.100062539
## 
## X$getsolve()
##             [,1]        [,2]
## [1,] 0.100062539 0.002501563
## [2,] 0.002501563 0.100062539
## 
## Other test using set() function
## > c=rbind(c(1, -1/8), c(-1/8, 1))
## > X$set(c)
## > cacheSolve(X)
## [,1]      [,2]
## [1,] 1.0158730 0.1269841
## [2,] 0.1269841 1.0158730
## > X$getsolve()
## [,1]      [,2]
## [1,] 1.0158730 0.1269841
## [2,] 0.1269841 1.0158730
## > cacheSolve(X)
## getting cached data
## [,1]      [,2]
## [1,] 1.0158730 0.1269841
## [2,] 0.1269841 1.0158730
## 
## 
## 
## 
## 
## 
## 

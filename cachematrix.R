# Matrix inversion can be a costly calculation. This pair of functions caches
# the inverse of a matrix rather than computing it repeatedly.

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # Make an empty variable to hold the cached matrix
    cache <- NULL

    # The set function sets the value of the value of the matrix to be cached
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }

    # The get function gets the value of the value of the matrix to be cached
    get <- function() x

    # The setsolve function sets the inverted matrix to the cache
    setsolve <- function(solve) cache <<- solve

    # The getsolve function gets the inverted matrix from the cache (may be null)
    getsolve <- function() cache

  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


# This function computes the inverse of the "matrix" returned by
# `makeCacheMatrix` above. If the inverse has already been calculated (and the
# matrix has not changed), then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    # Call the cache fetch function to look for a cached result
    cache <- x$getsolve()

    # Check if the result is cached, if it is, print a message and return early
    # with the cached result.
    if(!is.null(cache)) {
        message("getting cached data")
        return(cache)
    }

    # If the result is not cached, we'll have to take the time to solve it.
    data <- x$get()
    cache <- solve(data, ...)

    # Then we save the newly calculated result so it will be cached for next time.
    x$setsolve(cache)

    # Finally we return the new result.
    cache
}

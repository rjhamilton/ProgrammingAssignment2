####
# matrix wrapper object that maintains cached values
makeCacheMatrix <- function(mCache = matrix()) {
	invCache <- NULL

	set <- function(m) {
		mCache <<- m
		invCache <<- NULL # invalidate dependent objects here
	}
	get <- function() mCache
	setinv <- function(inv) invCache <<- inv
	getinv <- function() invCache

	list(
	    set = set
	  , get = get
	  , setinv = setinv
	  , getinv = getinv
	)
}

####
# invert a matrix wrapper object and cache the result for subsequent calls
cacheSolve <- function(m, ...) {
	inv <- m$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}

	# do the heavy lifting here
	inv <- solve(m$get(), ...)
	m$setinv(inv)
	inv
}

####
# test it!
#N <- 3
#m <- matrix(floor(runif(N*N, 0, 100)), ncol=N)
#m
#mWrapped <- makeCacheMatrix(m)
#cacheSolve(mWrapped)
#cacheSolve(mWrapped)

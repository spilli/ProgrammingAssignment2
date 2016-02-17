## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## The following two functions enable caching of matrix inversion. 

## The makeCacheMatrix function creates a special "vector", which is a list 
## containing a function to
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the Matrix inverse
## 4. get the value of the Matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The cacheSolve function computes the Matrix inverse of the special "vector" 
## with the above function. However, it first checks to see if the matrix inversion
## has already been calculated. If so, it gets the matrix inverse value from the 
## cache and skips the computation and hence saves computation cycles. Otherwise it 
## calculates the matrix inversion of the input matrix and sets the value of the 
## inverse in the cache via setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting chached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

## -------------- Sample execution -----------------
## execute the following commands one at a time

## > a = matrix(rnorm(1000000), nrow=1000, ncol=1000)
## > temp <- makeCacheMatrix(a)
## > ainv <- cacheSolve(temp) # this takes about 2-3 seconds
## > ainv <- cacheSolve(temp) # this is almost instantaneous
## getting chached data <--- this is the output
## > ainv <- cacheSolve(temp) # this is almost instantaneous
## getting chached data <--- this is the output
## > b <- a %*% ainv # you should get 1000 order identity matrix (diag = 1)

## -------------- end of Sample execution -----------

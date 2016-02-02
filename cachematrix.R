## makeCacheMatrix, allows the creation of a cached matrix to ensure that
## time consuming functions (such as calculation of the inverse) are stored
## in memory and do not need to be recomputed
## chceSolve, is the corresponding function that allows the recall of the cached
## value if it exists otherwise the value is computed

## Creates a CachedMatrix
## set (), is a function that stores ensures that the varible is searched for
## in it's parent environment and if the binding of the variable is not
## locked then the value is reassigned
## get () is a function to retrive the matrix
## setinv () is a function to set the value if the inverse matrix
## getinv () allows the retrival of the inverse matrix, once cached

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
      x<<-y
      inv<<-NULL
    }
    get<-function(){
      x
    }
    setinv<-function(inverse){z
      inv<<-inverse
    }
    getinv<-function(){
      inv
    }
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Calculates the inverse of the matrix
## If it has already been calculated then it returns the cached value
## otherwise it executes the solve() function to calculate the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv<-x$getinv()
      if(!is.null (inv)){
        message("getting cached data")
        return(inv)
      }
      data<-x$get()
      inv<-solve(data,...)
      data$setinv(inv)
      inv
      
}

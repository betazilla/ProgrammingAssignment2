
## The function makeCacheMatrix stores a list of functions

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set<-function(y){ 
                x<<-y  #set changes vector x to the input y
                m<<-NULL  # restores the value of m to NULL
        }
        get<-function() x  #returns vector x from the main function
        setsolve<-function(solve) m<<-solve  #store the value of solve function to m
        getsolve<-function() m # returns the stored value
        list(set=set, get=get, setsolve=setsolve, getsolve=getsolve) # creates a list to store the 4 functions created in makeCacheMatrix
}


## cacheSolve checks if a stored value already exists, eif not it solves the Matrix (inverts it)

cacheSolve <- function(x, ...) {
        m<-x$getsolve()
                if(!is.null(m)){  #checks if the value stored in m is not NULL
                        message("getting cached data :)") # if it exitsts, print message
                        return(m) # returns the value stored in m
                }
        data<-x$get() #if no value exists, get data from x
        m<-solve(data,...) # solve the data(matrix)
        x$setsolve(m) # stores the new value
        m
## Returns a matrix that is the inverse of 'x'
}

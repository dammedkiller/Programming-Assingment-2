##Function to create a special matrix with its inverse
##Parameter x is the matrix we want to store and calculate its inverse
##Code made by Jorge Sendino

makeCacheMatrix <- function (x=matrix()) {
      
        inv<-NULL
        ##This function set the matrix with the value of the parameter and its inverse
        ##to zero(inverse will later be set)
        setMatrix<- function(y) {
                x<<-y
                inv<<-NULL
        }
        ##Function to set the inverse
        setInv <- function(y) {
                inv<<-y
        }
        ##Function to get both values
        getMatrix <- function() x
        getInv <- function () inv
        
        list(setMatrix=setMatrix,setInv=setInv, getMatrix=getMatrix, getInv=getInv)
        
}

##Function to calculate the inverse of the matrix specified by the parameter
##If the stored value is correct, return it.

cacheSolve <- function(x, ...) {
       
        inv<-x$getInv()
        m<-x$getMatrix()
        I<-diag(1,nrow(m), ncol(m))
        ##Check if the inverse exists and if it is the actual inverse of the matrix
        ##If both condition true, return cached matrix
        if(!is.null(inv)){
                if (all(m%*%inv==I)) {
                        message("getting cached data")
                        return(inv)
                }
        }
        ##Else, calculate the inverse, stored it and return it.
        else{
                inv<-solve(x$getMatrix())
                x$setInv(inv)
        }  
        
        inv  

}

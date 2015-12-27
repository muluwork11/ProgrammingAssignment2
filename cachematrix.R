## makeCacheMatrix() takes in invertible square matrix as input. It also
## creates the following functions and returns them as a list to be used by
## cacheSolve(), which calculates inverse of matrix 'x'.
## setMat()    -- makes the matrix input from makeCacheMatrix()available 
##             -- in the working in environment
## getMat()    -- gets the matrix
## saveInv()   -- saves the inverse calculated by cacheSolve() in the cache
## getInv()    -- retrieves matrix inverse if exists already

makeCacheMatrix <- function(x = matrix()) {
        
        #Initialize cache for saving calculated inverse values
        cache <- NULL
        
        ## Make the matrix input to makeCacheMatrix() available in the 
        ## working environment
        
       setMat <- function(y){
               x <<- y
               cache <<- NULL
       }
       
       ## Function to get the matrix input by other functions
       getMat <- function (){
               x
       }
       
       ## Function to save "inverse" into cache
       saveInv <- function(inverse){
               cache <<-inverse
       }
       
       ## Function to retrieve the matrix inverse
       getInv <- function (){
               cache
        }
               
       ## Return functions to expose to other 
       list(getMat=getMat, setMat=setMat, getInv=getInv  ,saveInv=saveInv)
               

}


## cacheSolve() calculates and returns the inverse of matrix 'x'. 
## It utilizes the functions inside makeCacheMatrix() to get the matrix 'x',
## previously computed inverse if exists and also save new inverse values to
## cache

cacheSolve <- function(x, ...) {
       
        ## Call to retrieve previously calculated inverse
        inverse <- x$getInv()
        
        ## Checks the value of cache and returns if it exists
        ## If not, it calculates and returns the new inverse matrix
        
        if (is.null(inverse)){
                
                ## Print a message to console about the new calculation
                print("New matrix inverse calculation in progress..")
                
                ## Gets the matrix 'x' and saves it to temp variable
                tempMat <- x$getMat()
                
                ## Calculates the inverse matrix 
                inverse <- solve(tempMat, ...)
                
                ## Saves new inverse to cache
                x$saveInv(inverse)
                
        }
        else{  ## Finds matrix inverse in cache
        
                print("Matrix inverse exists in the cache")
               
        }
         
        
        ## Return a matrix that is the inverse of 'x' 
        ## either from the cache or new computation      
        
        return (inverse) 
}

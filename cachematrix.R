#Overview-> The first function takes a matrix, and will produce a list for
#containing a function that
#1.set the matrix
#2.Get the matrix
#3.Set the inverse of the matrix
#4.Get the inverse of the matrix


#The inverse can be calculated using the function solve(), a function that
#is contained in the the standard R library


#The functions are thoroughly commented throughout


#This is a function to cache the mean of a matrix, x
makeCacheMatrix<-function(x = matrix()){
  #inv is the inverse of the matrix, it is first set to NULL, to clear any
  #value it may contain.
  #It also defines the value of inv, which is needed to stop the function 
  #from failing
  inv<-NULL
  
  set<-function(y){
    #The use of the <<- operator ensures that the values are set in the
    #function makeCacheMatrix, and not just the function set
    x<<-y
    inv<<- NULL
    
  }
  #This function returns the value x, and requires no parameters
  get <- function() x
  #setInv uses the solve function (a function that comes in the standard R
  #library) to calculate the Inverse of matrix x. It then assigns that value
  #to x (with y being assigned the value of the orig matrix in the earlier
  #part of the function)
  setinv<- function(solve) x<<-solve
  #getinv returns the inverse, similar to get
  getinv<- function() inv
  
  #This creates a list of the functions, which can be called by cacheSolve
  list(set=set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

#The 2nd function will take the list that has been created by the first 
#function, and then it looks into the cache to see if the inverse of the 
#matrix has already been calculated. If it has, then it will pull the value
#from the cache. If it has not, it will then calculate the inverse

#CacheSolve takes z, which is a list, which is the output AFTER calling
#the function makeCacheMatrix on a matrix x
cacheSolve<-function(z, ...){
  #This calls the function getinv from the list x
  inv <- z$getinv()
  #If inv is not NULL (ie, there is a value for x), rather than recalculating
  #the inverse, the value can be retrived from the cache
  if(!is.null(inv)){
    message("Retriving from cache")
    #returns the inv value from the cache
    return(inv)
  }
  
  #if inv is null...
  #returns the orignal matrix (x) from the make cache matrix formula above
  #and assigns it the value mat
  mat <- z$get()
  #uses the solve function to get the inverse of the
  inv <- solve(mat,...)
  #Adds it to the cache via setinv
  z$setinv(inv)
  #returns the inverse of the matrix
  inv
}

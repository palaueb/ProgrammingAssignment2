## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix			<- function(theMatrix = matrix()) {
	if(!is.matrix(theMatrix)){			## let's start checking if the matrix is a valid one
		stop("I need a matrix, not another kind of data.")
	}
	
	cachedMatrix 		<- NULL 		## this is our cached Matrix data, we reset it each time the matrix is set or the object created
	set <- function(newMatrixData) {		## this method is used to set new data into the Matrix
		theMatrix	<<- newMatrixData 	## Set the new data onto our variable theMatrix (that contains the original data)
		cachedMatrix	<<- NULL
	}
	get <- function(){				## function to return the original theMatrix data
		return(theMatrix)			## using return to clarify the code
	}
	setInverse <- function(inverseMatrix){		## the function used from cacheSolve to store the inverted data
		cachedMatrix 	<<- inverseMatrix 	## set the cached Matrix data to the inverse of the original Matrix
	}
	getInverse <- function(){			## function to return the cachedMatrix data
		return(cachedMatrix)			## using return to clarify the code
	}
	list(						## we return a list of methods to give functionality to the instance of the object makeCacheMatrix
		set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse
	)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(cacheMatrix, ...) {		## the ellipses (dot dot dot) is for the arguments of the solve function
	inverseMatrix <- cacheMatrix$getInverse()	## do we have it inverted?
	if(!is.null(inverseMatrix)) {			## if it's inverted yet, we return it
		message("getting cached data") 		## we do the announcement!!!
		return(inverseMatrix) 			## if it's cached, we return the cached inverse matrix data
	}
	originalMatrixData <- cacheMatrix$get()		## we take the original matrix data
	
	if(det(originalMatrixData) == 0){		## first we check if the matrix can be inverted, if not, just return an error
		stop("The provided matrix is can not be inverted.")
	}
	
	inverseMatrix <- solve(originalMatrixData, ...)	## and calculate the inverse of the matrix
	cacheMatrix$setInverse(inverseMatrix) 		## then we save the calculated inverse onto the original object
	#return(inverseMatrix) 				## and return out inverse Matrix. I preffer to use return(XX) to clarify the code
}


######## ##     ##    ###    ##     ## ########  ##       ######## 
##        ##   ##    ## ##   ###   ### ##     ## ##       ##       
##         ## ##    ##   ##  #### #### ##     ## ##       ##       
######      ###    ##     ## ## ### ## ########  ##       ######   
##         ## ##   ######### ##     ## ##        ##       ##       
##        ##   ##  ##     ## ##     ## ##        ##       ##       
######## ##     ## ##     ## ##     ## ##        ######## ######## 

## we start creating a matrix that we know can be inverted
originalMatrix <- matrix(nrow=3,ncol=3,c(5, 1, 0, 3,-1, 2, 4, 0,-1),dimnames = list(LETTERS[4:6],LETTERS[1:3]))
originalMatrix
## then create the object that will give us the functionalities
matrixObject <- makeCacheMatrix(originalMatrix)
matrixObject
## and let's inverse the matrix and cache it, and check the result
cacheSolve(matrixObject)
matrixObject$getInverse()


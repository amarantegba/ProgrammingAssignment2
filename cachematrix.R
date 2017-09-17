## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## A função makeCacheMatrix gera matriz colocando-a em "cache" permitindo posterior
## recuperação, montando a matriz ou identificando quando ela não é inversível
## Exemplo :
##  a <- matrix(c(3,2,4,3),2,2)
##  makeCacheMatrix(a)
##  minhamatriz$get()
##  minhamatriz$getInverse
## no caso a matriz deste exemplo é inversível :
      [,1] [,2]
[1,]    3    4
[2,]    2    3
## A matriz inversa é:
      [,1] [,2]
[1,]    3    4
[2,]    2    3

## Exemplo de matriz não inversível
##  a <- matrix(c(3,1,6,2),2,2)
##  makeCacheMatrix(a)
##  minhamatriz$get()
##  minhamatriz$getInverse
## NÃO É INVERSÍVEL


--------------------------------------------------------------------
##Exercício de programação da semana3
## Geraldo Barbosa do Amarante
##
makeCacheMatrix <- function(matriz = matrix()) {
        inversa <- NULL
        set <- function(y) {
                matriz <<- y
                inversa <<- NULL

        }
        get <- function() matriz
        setInverse <- function(inverse) inversa <<- inverse
        getInverse <- function() inversa
        list(set = set,get = get,setInverse = setInverse,
             getInverse = getInverse)
}
----------------------------------------------------------------------


## Write a short comment describing this function

##Exercício de programação da semana3
## Geraldo Barbosa do Amarante
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversa <- x$getInverse()
        if (!is.null(inversa)) {
                return(inversa)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inversa)
        inversa
}


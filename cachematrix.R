## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## 初始化一个列表来存储矩阵及其逆
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL  ## 每次矩阵更新时清空缓存的逆矩阵
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    ## 返回列表包含对应的函数
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## 尝试获取缓存的逆矩阵
    m_inv <- x$getinverse()
    ## 检查是否已经缓存了逆矩阵
    if (!is.null(m_inv)) {
        message("getting cached data")
        return(m_inv)
    }
    ## 获取原矩阵
    data <- x$get()
    ## 计算逆矩阵
    m_inv <- solve(data, ...)
    ## 缓存逆矩阵
    x$setinverse(m_inv)
    m_inv
}

#' Mouse in the maze
#'
#' This function can help the mouse finding its way to escape the maze from 1 to n^2.
#' We ristrict the maze only can be n^2 size maze, and n>1.
#' @param n The length of maze.
#' @return You can know what the path does mouse go throuth and what the path length is.
#' @examples
#' mouse(3)
#' @export
mouse<-function(n){
  m<-n^2
  i<-1:(n-2)
  right<-n*i+1
  j<-2:(n-1)
  left<-n*j
  pos<-1
  path<-c()

  while (pos!=m) {
    if (pos==1) {
      pos<-sample(c(2,n+1),size=1)
      path<-c(path,pos)
    }
    else if (1<pos && pos<n) {
      pos<-sample(c(pos-1,pos+1,pos+n),size=1)
      path<-c(path,pos)
    }
    else if (pos==n) {
      pos<-sample(c(pos-1,pos+n),size=1)
      path<-c(path,pos)
    }
    else if (any(pos==right)==T) {
      pos<-sample(c(pos+n,pos-n,pos+1),size=1)
      path<-c(path,pos)
    }
    else if (any(pos==left)==T) {
      pos<-sample(c(pos+n,pos-n,pos-1),size=1)
      path<-c(path,pos)
    }
    else if (pos==(m-n+1)) {
      pos<-sample(c(pos+1,pos-n),size=1)
      path<-c(path,pos)
    }
    else if (m-n+1<pos && pos<m) {
      pos<-sample(c(pos+1,pos-1,pos-n),size=1)
      path<-c(path,pos)
    }
    else {
      pos<-sample(c(pos+1,pos-1,pos+n,pos-n),size=1)
      path<-c(path,pos)
    }

  }
  cat("The path is {",path,"}",".\n")
  cat("And the path length is", length(path),".")
}

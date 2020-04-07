rowdigits <- function(data,row) return(as.numeric(data[row,][which(as.numeric(data[row,])>0)]))
coldigits <- function(data,col) return(as.numeric(data[,col][which(as.numeric(data[,col])>0)]))
blockdigits <- function(data,row,col) return(as.numeric(unlist(data[(floor((row-0.1)/3)*3+1):(floor((row-0.1)/3)*3+3),
                                                        (floor((col-0.1)/3)*3+1):(floor((col-0.1)/3)*3+3)])))

sudoku <- matrix(data=c(
  6,0,0,2,1,0,0,3,0,
  5,0,9,0,0,0,6,0,0,
  2,0,0,9,7,0,0,0,4,
  0,0,2,3,0,4,0,0,0,
  0,6,0,0,5,0,0,9,0,
  0,0,0,1,0,9,7,0,0,
  9,0,0,0,3,8,0,0,6,
  0,0,7,0,0,0,2,0,5,
  0,8,0,0,4,2,0,0,9), nrow=9, ncol=9, byrow=FALSE
)
sudoku <- as.data.frame(sudoku)
poss <- expand.grid(1:9,1:9,stringsAsFactors = F)
poss[,3:11] <- NA

for(i in 1:9) for(j in 1:9){
  if(sudoku[i,j]==0){
    avail <- setdiff(1:9,union(union(rowdigits(sudoku,i),coldigits(sudoku,j)),blockdigits(sudoku,i,j)))
    poss[which(poss$Var1==i & poss$Var2==j),3:(2+length(avail))] <- avail
  }
}

poss <- poss[which(!(is.na(poss$V3))),which(apply(poss,2,sum,na.rm=T)>0)]
nc <- ncol(poss)
rownames(poss) <- 1:nrow(poss)

poss$sel <- poss$V3
m <- 0
for(i in 1:nrow(poss)){
  for(j in 1:length(which(!(is.na(poss[i,3:nc]))))){
    poss$sel[i] <- poss[i,(2+j)]
    for(k in c(1:nrow(poss))[which(c(1:nrow(poss))!=i)]){
     for(l in 1:length(which(!(is.na(poss[k,3:nc]))))){
       poss$sel[k] <- poss[k,(2+l)]
       print(paste(poss$sel,collapse=''))
       m <- m+1
       for(n in 1:nrow(poss)) sudoku[poss$Var1[n],poss$Var2[n]] <- poss$sel[n]
       success <- NULL
       for(o in 1:9) for(p in 1:9){
         success <- c(success,ifelse(length(intersect(intersect(rowdigits(sudoku,o),coldigits(sudoku,p)),blockdigits(sudoku,o,p)))==9,T,F))
         if(length(which(success==F))==0) break()
       }
     }
    }
  }
}


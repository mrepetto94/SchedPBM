library("Rglpk")
library("qpcR") 

lagmatrix <- function(x,max.lag) embed(c(rep(NA,max.lag), x), max.lag+1)

Type <- c("N", "SN", "M1","MP","P1","P3","M+N","SP1")

nt <- length(Type)
Hour <- c(3,7,7,8,6,6,6,7.5)
nh <- length(Hour)

Schedule <- data.frame(Type,Hour)

days <- 1:3
nd <- length(days)

Worker <- c("Tizio", "Caio")
nw <- length(Worker) 

weeklyHour <- c(42,42)
monthlyHour <- c(170,170)

presence <- matrix(TRUE, nrow = nw, ncol=(nt*nd),byrow=TRUE)

ncol <- (nt*nd*nw)+
	(nw)

n <- dim(presence)[2] 

nrow <- (nw)

obj <- rep(0L, ncol)
obj <- replace(obj, (((ncol + 1) - nw):(ncol)), 1)

mat <- matrix (0L, ncol = ncol, nrow = nrow)

q <- 1
i <- 1

presence  <- ifelse(presence == TRUE, 1, 0)


#create the binary constraint for the deviational variable
for (q in 1: nw){	
	slot<- (q-1)*n
	span <- 1:n
	mat[q,slot+span] <- rep(Hour,nd)
	mat[q, (ncol - nw + q)] <- -1 
}

#creates the daily hour constraint
replace <- rep(c((Hour),rep(0,n-nh)),nw)

replace <-lagmatrix(replace, nt*nd)[,c(1,(1+(1*nt)),(1+(2*nt)))] #to be adjusted with the days
replace[is.na(replace)] <- 0
matobj <- matrix(0L, ncol = nd, nrow = nw)
replace <- t(rbind(replace, matobj))

mat <- rbind(mat, replace)

#creates the schedule type constraints
#replace <- rep(c(1,rep(0,n-1)),nw)
#replace <-lagmatrix(replace, n-1) #to be adjusted with the days
#replace[is.na(replace)] <- 0
#matobj <- matrix(0L, ncol = n, nrow = nw)
#replace <- t(rbind(replace, matobj))

#mat <- rbind(mat, replace)


dir <- c(rep("==",nw),
	 rep(">=",nd))
#	 rep(">=",n))

type <- c(rep("B",n*nw),
	rep("C", nw))

rhs <- c(rep(0,nw),
	 rep(36,nd))
#	 rep(1,n))

#bounds <- list(lower = list(ind = 1:ncol, val = rep(0,ncol)),
#	       upper = list(ind = (((n*nw)+1):ncol), monthlyHour))

system <- data.frame(mat,dir,rhs)

sol <- Rglpk_solve_LP(obj,mat,dir,rhs,types=type)

solution  <- sol$solution[1:(length(sol$solution)-nw)]
solution <- matrix(solution, nrow=nw,ncol=nt*nd, byrow=TRUE)
df <- data.frame(solution)
colnames(df)<-rep(Type,nd)
rownames(df)<-Worker
t(df)

write.csv2(system,"system.csv")




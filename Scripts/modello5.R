library("Rglpk")
library("dplyr")
library("qpcR") 

lagmatrix <- function(x,max.lag) embed(c(rep(NA,max.lag), x), max.lag+1)


Type <- c("N", "SN", "M1","MP","P1","P3")

nt <- length(Type)
Hour <- c(3,7,7,8,6,6)
nh <- length(Hour)

Schedule <- data.frame(Type,Hour)

days <- 1:3
nd <- length(days)
restday <- 3


Worker <- c("Solari","Laino","Stella","Forgali","Giraudi","Vitale","Ileshi","Abid")

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


#####################Soft constraints#######################
#create the binary constraint for the deviational variable
for (q in 1: nw){	
	slot<- (q-1)*n
	span <- 1:n
	mat[q,slot+span] <- rep(Hour,nd)
	mat[q, (ncol - nw + q)] <- -1 
}

#####################Hard constraints#######################
#creates daily structure of the schedule 

replace <- rep(c(1,rep(0,n-1)),nw)
replace <-lagmatrix(replace, n-1)
replace[is.na(replace)] <- 0
matobj <- matrix(0L, ncol = n, nrow = nw)
replace <- t(rbind(replace, matobj))

mat <- rbind(mat, replace)

#create daily limit constraint
replace <- c(rep(1,nt),rep(0,(nt*nd*nw)-nt))
replace <- lagmatrix(replace, (n*nw)-1)[, seq(1,nt*nw*nd,nt)]
replace[is.na(replace)]<-0
matobj <- matrix(0L, ncol = dim(replace)[2], nrow = nw)
replace <- t(rbind(replace, matobj))

mat <- rbind(mat, replace)

#create the constraint related to the rest days 
replace <- c(rep(1,nt*restday),rep(0,((n*nw)-(nt*restday))))
span <-nd - restday
replace <- lagmatrix(replace, ((nt*span)+1))[,seq(1,(span*nt)+1,nt)]

i<-1
replace <- as.matrix(replace)
reminder <- replace
for(i in 1:(nw-1)){
reminder <-rbind(matrix(NA,ncol=dim(reminder)[2],nrow=n),head(reminder,-n))
replace <- cbind(replace,reminder)
}

replace[is.na(replace)] <- 0
matobj <- matrix(0L, ncol = dim(replace)[2], nrow = nw)
replace <- t(rbind(replace, matobj))

mat <- rbind(mat, replace)

##################Left side declariation and variable type #
dir <- c(rep("==",nw),
	 rep(">=",nt*nd),
	 rep("<=",nd*nw),
	 rep("<=", nw+(span*nw)))

type <- c(rep("B",n*nw),
	rep("C", nw))

rhs <- c(rep(0,nw),
	 rep(c(rep(1,2),2,rep(1,3)),nd),
	 rep(1, nd*nw),
	 rep(restday-1, nw+(span*nw)))

#bounds <- list(lower = list(ind = 1:ncol, val = rep(0,ncol)),
#	       upper = list(ind = (((n*nw)+1):ncol), monthlyHour))

system <- data.frame(mat,dir,rhs)

sol <- Rglpk_solve_LP(obj,mat,dir,rhs,types=type)

solution  <- sol$solution[1:(length(sol$solution)-nw)]
solution <- matrix(solution, nrow=nw, byrow=TRUE)
df <- data.frame(solution)
colnames(df)<-rep(Type,nd)
rownames(df)<-Worker
df <- t(df)

write.csv2(system,"system.csv")



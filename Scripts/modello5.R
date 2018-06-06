library("Rglpk")
library("dplyr")
library("qpcR") 

lagmatrix <- function(x,max.lag) embed(c(rep(NA,max.lag), x), max.lag+1)


Type <- c("N", "SN", "M1","MP","P1","P3")

nt <- length(Type)
Hour <- c(3,7,7,8,6,6)
nh <- length(Hour)

Schedule <- data.frame(Type,Hour)

days <- 1:8
nd <- length(days)
restday <- 7


Worker <- c("Solari","Laino","Stella","Forgali","Giraudi","Vitale","Ileshi","Abid","Prova")

nw <- length(Worker) 

weeklyHour <- rep(42,nw)
monthlyHour <- rep(170,nw)

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

presence[1,] <- rep(c(1,1,1,1,1,1),nd)
presence[2,] <- rep(c(1,1,1,1,1,1),nd)
presence[3,] <- rep(c(1,1,1,1,1,1),nd)
presence[4,] <- rep(c(1,1,1,1,1,1),nd)
presence[5,] <- rep(c(1,1,1,1,1,1),nd)
presence[6,] <- rep(c(1,1,1,1,1,1),nd)
presence[7,] <- rep(c(1,1,1,1,1,1),nd)
presence[8,] <- rep(c(1,1,1,1,1,1),nd)

presence <- as.vector(t(presence))


#####################Soft constraints#######################
#create the binary constraint for the deviational variable
for (q in 1: nw){	
	slot<- (q-1)*n
	span <- 1:n
	mat[q,slot+span] <- rep(Hour,nd)
	mat[q, (ncol - nw + q)] <- -1 
}

#####################Hard constraints#######################
#creates daily structure of the schedule [no nights] 

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

#create the rest/holyday/ill constraint
replace <- c(1,rep(0,ncol-nw-1))
replace <- lagmatrix(replace, ncol - nw -1)
replace [is.na(replace)] <- 0
matobj <- matrix(0L, ncol = dim(replace)[2], nrow = nw)
replace <- t(rbind(replace, matobj))

mat <- rbind(mat, replace)

#create the P3/M1 constraint
pthree <- match("P3", Type)
mone <- match("M1", Type)+nt
replace <- c(rep(0,pthree-1),1,rep(0,mone-1-pthree),1,rep(0,(nt*2)-(mone)), rep(0,ncol-nw-nt*2))
replace <- lagmatrix(replace,(n*nw)-1)[, seq(1,nt*nw*nd,nt)]
replace [is.na(replace)] <- 0
matobj <- matrix(0L, ncol = dim(replace)[2], nrow = nw)
replace <- t(rbind(replace, matobj))

mat <- rbind(mat, replace)

#create the weekly hour constraint
replace <- c(rep(Hour,(nt)),rep(0,ncol-nw-(nh*nt)))
replace <- lagmatrix(replace, ncol - nw -1)
replace [is.na(replace)] <- 0
matobj <- matrix(0L, ncol = dim(replace)[2], nrow = nw)
replace <- t(rbind(replace, matobj))

mat <- rbind(mat, replace)

##################Left side declariation and variable type #
dir <- c(rep("==",nw),
	 rep(">=",(nt*nd)),
	 rep("<=",nd*nw),
	 rep("<=", nw+(span*nw)),
	 rep("<=", length(presence)),
	 rep("<=",nd*nw),
	 rep("<=",nt*nd*nw))

type <- c(rep("B",n*nw),
	rep("C", nw))

rhs <- c(rep(0,nw),
	 rep(c(1,1,2,rep(1,3)),nd),
	 rep(1, nd*nw),
	 rep(restday-1, nw+(span*nw)),
	 presence,
	 rep(1, nd*nw),
	 rep(weeklyHour,(nd*nt)))

bounds <- list(lower = list(ind = 1:ncol, val = rep(0,ncol)),
	       upper = list(ind = ((ncol-nw+1):ncol),val = monthlyHour))

system <- data.frame(mat,dir,rhs)
write.csv2(system,"system.csv")

sol <- Rglpk_solve_LP(obj,mat,dir,rhs,types=type, bounds = bounds)

solution  <- sol$solution[1:(length(sol$solution)-nw)]
SumWorker <-tail(sol$solution,nw) 
solution <- matrix(solution, nrow=nw, byrow=TRUE)
df <- data.frame(solution)
colnames(df)<-rep(Type,nd)
rownames(df)<-Worker
df <- t(df)
soldf <- rbind(df,SumWorker)
df <- rbind(df,SumWorker*0)

####Create the schedule in a readable way

dfschedule <- matrix("F", nrow=nd, ncol=nw)

i <- 1
q <- 1
scheduletype <- rownames(df)
for (q in 1:nd){
k<-i
for ( i in k:(k+nt)){
	location<-grep(1,df[i,])
	dfschedule[q,location]<-scheduletype[i]
}

}

dfschedule <- data.frame(dfschedule, row.names = days)
colnames(dfschedule)<- Worker

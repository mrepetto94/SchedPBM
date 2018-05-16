library("Rglpk")
library("shiny")
library("rhandsontable")

ui <- fluidPage( 
		titlePanel("Worker schedule assistants"),
		sidebarLayout(
			      sidebarPanel(
					   dateRangeInput("date", label = "Select the range to plan the schedule"),
					   rHandsontableOutput('table'),
					   actionButton("go", "Calculate")
					   ),
			      mainPanel(
					tableOutput("result")
					)
			      )
		)

server <- function(input,output,session) ({

	df1 <- reactive({	
		range <- input$date[1] : input$date[2]
		Day <- 1 + abs(as.numeric(input$date[1] - range))
		class(range) <- "Date"
		Weekday <- as.character(weekdays(range, abbreviate = TRUE))
		Hour <- ifelse( ((Weekday == "sab") | (Weekday == "dom")), 6, 11)
		data.frame(Day,Weekday,Hour)
	})
		output$table<-renderRHandsontable(
						  rhandsontable(df1())
)

dataframe <- eventReactive(input$go,{

				   lun <- grep("lun",hot_to_r(input$table)[,2])
				   #Generate the month days schedule
				   d <-  hot_to_r(input$table)[,3]
				   lowhour <- grep(6,d)  
				   month <- length(d)/30
				   #Take the first monday
				   firstm <- lun[1]
				   #Take the number of weeks within the month
				   weeksinmonth <- length(firstm:length(d)) %/% 7
				   
				   #Hour limit per week per employee
				   weeklimit<- c(42,30)
				   #Number of worker
				   worker <- 2
				   
				   #Number of hour per month
				   maxhour <- c(175,135)
				   #workable days in a row (i.e 6 days and the 7 is off)
				   wdays <- 7 
				   
				   span <- length(d) - wdays
				   
				   #generate the empty matrix
				   mat <- matrix(0L, 
						 ncol=(worker + (length(d)*worker*3)), 
						 nrow=(worker + length(d) + (length(d)* worker * 4 )+ (length(d) * worker)+ (length(lowhour) * worker) + (span * worker)+((length(d)-7)*worker) + (weeksinmonth*worker)))
				   
				   #generate the empy objective vector
				   obj <- rep(0L, (ncol(mat)))
				   
				   #turn on the deviational variables
				   obj <- replace(obj,((length(obj)-worker+1):(length(obj))),1)
				   
				   #initialize the soft constraints
				   i<-1
				   q<-1
				   k <- length(d)
				   
				   for( i in 1:worker){	
					   mat[i,]<-replace(mat[i,], q:(k+q-1),1)
					   mat[i,]<-replace(mat[i,],length(obj)-worker+i,-1)
					   q<-q+k
				   }

#initialize the hard constraints referring to the day hour cap

q <- i + 1
m <- 1
for( i in q:(q+k-1)){ 
	mat[i,]<-replace(mat[i,],c(m,m+k),1) #this is intended for only two workers
	m <- m+1
}

#big M constraint to binarity
y <- 1 
for(y in 1: worker){
	
q <- i + 1
m <- 1 + ((y-1)*(k))

for (i in q:(q+(k-1))){
	mat[i,] <- replace(mat[i,],c(m,(m+(k*worker))),c(-1,1))
	m <- m + 1
}
}

y <- 1 
for(y in 1: worker){
	
q <- i + 1
m <- 1 + ((y-1)*(k))

for (i in q:(q+(k-1))){
	mat[i,] <- replace(mat[i,],c(m,(m+(k*worker))),c(1,-10))
	m <- m + 1
}
}

y <- 1 
for(y in 1: worker){
	
q <- i + 1
m <- 1 + ((y-1)*(k))

for (i in q:(q+(k-1))){
	mat[i,] <- replace(mat[i,],c(m,(m+(k*(worker^2)))),c(-1,1))
	m <- m + 1
}
}

y <- 1 
for(y in 1: worker){
	
q <- i + 1
m <- 1 + ((y-1)*(k))

for (i in q:(q+(k-1))){
	mat[i,] <- replace(mat[i,],(c(m,(m+(k*(worker^2))))),c(1,-10))
	m <- m + 1
}
}

#limit on schedule between 4 and 6

y <- 1 
for(y in 1: worker){
	
q <- i + 1
m <- 1 + ((y-1)*(k))

for (i in q:(q+(k-1))){
	mat[i,] <- replace(mat[i,],(c(m,(m+(k*(worker^2))))),c(-1,6))
	m <- m + 1
}
}

#limit on 3/3 hour
y <- 1
for(y in 1:worker){
	q<-i+1
	m <- ((y-1)*(k))
	x<-1
for (i in q:(q+(length(lowhour)-1))){
	mat[i,] <- replace(mat[i,],m+lowhour[x],-1)
	mat[i,] <- replace(mat[i,],(m+(k*worker) + lowhour[x]),4)
	x<-x+1
}
}

#set the working days constraints using binary variable
y<-1
for(y in 1:worker){

q <- i + 1
m <- 1 + ((y-1)*(k))
for (i in q : (q + (span-1))){
	mat[i,] <- replace(mat[i,], ((k*worker)+(m:(m+wdays))), 1)
	m <- m + 1
}
}

#constraint to avoid doing the same work off
y<-1
for(y in 1:worker){

q <- i + 1
m <- 1 + ((y-1)*(k))
for (i in q : (q + (k-1-7))){
	mat[i,] <- replace(mat[i,], m+c((k*worker),((k*worker)+7)), 1)
	m <- m + 1
}
}

y<-1
for(y in 1:worker){

q <- i + 1
m <- 1 + ((y-1)*(k))
for (i in q : (q + ((weeksinmonth)-1))){
	mat[i,] <- replace(mat[i,], (m + c((firstm-1)) : ((firstm-2)+7)), 1)
	m <- m + 7
}
}

write.csv(mat, file="mat.csv")

dir <- c(rep("==",worker),
	rep(">=",k),
	rep("<=",k*4*worker),
	rep("<=",k*worker),
	rep("<=",length(lowhour)*worker),
	rep("<=",worker*span),
	rep(">=",worker*(k-7)),
	rep("<=",weeksinmonth * worker))

type <- c(rep("I",k*2),
	rep("B",k*4),
	rep("C",worker))

rhs <- c(rep(0,worker),
	d,
	rep(0, k*2*worker),
	rep(3, k*2*worker),
	rep(0, k*worker),
	rep(0, length(lowhour)*worker),
	rep(wdays, span *worker),
	rep(1, (k-7)*worker), 
	rep(weeklimit[1],weeksinmonth), 
	rep(weeklimit[2],weeksinmonth))

bounds <- list(lower = list(ind = 1:(k*2), val = rep(0,k*2)),
	       upper = list(ind = c(1:(k*2),((dim(mat)[2] - (worker-1)):(dim(mat)[2]))), val = c(rep(8,k*2), (maxhour*month))))

#print(dim(mat))
#print(length(rhs))
#print(length(dir))

solution <- Rglpk_solve_LP(obj,mat,dir,rhs,types=type, bounds = bounds)


Worker1 <- solution$solution[1:k]
Worker2 <- solution$solution[(k+1):(k*2)]
#Isworking1 <- solution$solution[((k*2)+1):(k*3)] 
#Isworking2 <- solution$solution[((k*3)+1):(k*4)]
#Isworking31 <- solution$solution[((k*4)+1):(k*5)] 
#Isworking32 <- solution$solution[((k*5)+1):(k*6)]

Worker1 <- c(Worker1, sum(Worker1))
Worker2 <- c(Worker2, sum(Worker2))
#Isworking1 <- c(Isworking1, sum(Isworking1))
#Isworking2 <- c(Isworking2, sum(Isworking2))
#Isworking31 <- c(Isworking31, sum(Isworking31))
#Isworking32 <- c(Isworking32, sum(Isworking32))
Sum <- c(as.character(d), "TOT")



df <- data.frame(Worker1,Worker2,Sum)
df

})
				   

output$result<-renderTable({dataframe()},
	striped = TRUE, 
	bordered = TRUE,
	rownames = TRUE, 
	digits= 0)


})
shinyApp(ui = ui, server = server)

	


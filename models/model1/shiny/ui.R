library("Rglpk")
library("shiny")
library("rhandsontable")

# Define UI for application that plots random distributions 
shinyUI(fluidPage( 
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


	)

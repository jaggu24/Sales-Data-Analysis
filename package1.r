library(corrplot)
library(psych)
library(graphics)
library(corrgram)
library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      #         sliderInput("bins",
      #                     "Number of bins:",
      #                     min = 1,
      #                     max = 50,
      #                     value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Tab1","First", plotOutput("distPlot")),
        tabPanel("Tab2","Second", plotOutput("distPlot1")),
        tabPanel("Tab3","Third")
      )
    )
  )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
#    x    <- faithful[, 2] 
#    bins <- seq(min(x), max(x))
    par(mfrow=c(2,2))
    boxplot(BigMart$Item_Visibility,horizontal = TRUE,main="Item Visibility",col="Yellow")
    boxplot(BigMart$Item_Weight,horizontal = TRUE,main="Item Weight",col="Yellow")
    boxplot(BigMart$Item_MRP,horizontal = TRUE,main="Item MRP",col="Yellow")
    boxplot(BigMart$Item_Outlet_Sales,horizontal = TRUE,main="Item Output Sales",col="Yellow")
    
    # draw the histogram with the specified number of bins
#    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  output$distPlot1 <- renderPlot({
    # generate bins based on input$bins from ui.R
    #x    <- faithful[, 2] 
    #bins <- seq(min(x), max(x))
#    plot(r1$Ozone~r1$Temp)
    par(mfrow=c(2,2))
    hist(BigMart$Item_Visibility,main="Item Visibility",col="Yellow",xlab = "Visiblity")
    hist(BigMart$Item_Weight,main="Item Weight",col="Yellow",xlab = "Weight")
    hist(BigMart$Item_MRP,main="Item MRP",col="Yellow",xlab = "MRP")
    hist(BigMart$Item_Outlet_Sales,main="Item Output Sales",col="Yellow",xlab = "Outlet Sales")
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
})
print("Hello")
r1=na.omit(airquality)

Train <- read.csv(paste("Train.csv",sep=""))
str(Train)
Index <- which(Train$Item_Fat_Content=="LF"|Train$Item_Fat_Content=="low fat")
Train[Index,"Item_Fat_Content"] <- "Low Fat"
Index2 <- which(Train$Item_Fat_Content=="reg")
Train[Index2,"Item_Fat_Content"] <- "Regular"
View(Train)
BigMart <- na.omit(Train)
View(BigMart)
dim(BigMart)
describe(BigMart)
Fat_Level <- xtabs(~BigMart$Item_Fat_Content)
Fat_Level
Item <- xtabs(~BigMart$Item_Type)
Item
Outlet_Size <- xtabs(~BigMart$Outlet_Size)
Outlet_Size
Outlet_Identifier <- xtabs(~BigMart$Outlet_Identifier)
Outlet_Identifier
Outlet_Location <- xtabs(~BigMart$Outlet_Location_Type)
Outlet_Location
Outlet_Type <- xtabs(~BigMart$Outlet_Type)
Outlet_Type
table1 <- xtabs(~BigMart$Item_Type+BigMart$Item_Fat_Content)
table1
table2 <- xtabs(~BigMart$Item_Type+BigMart$Outlet_Location_Type)
table2
table3 <- xtabs(~BigMart$Outlet_Type+BigMart$Outlet_Size)
table3
table4 <- xtabs(~BigMart$Outlet_Type+BigMart$Outlet_Location_Type)
table4
par(mfrow=c(1,3))
plot(x=BigMart$Item_MRP,y=BigMart$Item_Outlet_Sales  ,col=c("Red","Orange"),xlab="MRP",ylab="Outlet Sales")
plot(y=BigMart$Item_Outlet_Sales,x=BigMart$Item_Weight,col=c("Red","Orange"),xlab="Item Weight",ylab="Output Sales")
plot(x=BigMart$Item_Visibility ,y=BigMart$Item_Outlet_Sales,col=c("Red","Orange"),xlab="Item Visibility",ylab="Output Sales")
Corr_Matrix <- BigMart[,c(2,4,6,8,12)]
cor(Corr_Matrix)
corrplot(corr=cor(Corr_Matrix),method="ellipse")
corrgram(BigMart,upper.panel =panel.pie,text.panel =panel.txt, lower.panel = panel.shade)
pairs(~BigMart$Item_Visibility+BigMart$Item_Weight+BigMart$Item_MRP+BigMart$Item_Outlet_Sales,col="Red")
Test1 <- xtabs(~BigMart$Item_Visibility+BigMart$Item_Outlet_Sales)
Test2  <-xtabs(~BigMart$Item_Weight+BigMart$Item_Outlet_Sales)
Test3  <-xtabs(~BigMart$Item_MRP+BigMart$Item_Outlet_Sales)
chisq.test(Test1)
chisq.test(Test2)
chisq.test(Test3)
t.test(BigMart$Item_MRP,BigMart$Item_Outlet_Sales)
# Run the application 
shinyApp(ui = ui, server = server)
#https://rpubs.com/pcrao22/BigMart_SalesPrediction  Reference



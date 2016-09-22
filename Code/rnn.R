# Finance demo for rnn package
# data are in www folder
# just an app.R file to keep it simple, hopefully, it will stay this way
# training_amount","Number of days of training"
library(rnn)
library(shiny)
library(sigmoid)
load("DataWork/VixDat.Rdata")
# getting the inputs from the server to the model
server <- function(input, output) {
  VixDat.train <- reactive({
    print(length(1:input$training_amount))
    VixDat <- VixDat[,1:input$training_amount,]
    # i think it should VixDat <- VixDat[1:input$training_amount,"selected cols"]
    array(VixDat,dim=c(1,input$training_amount,4))
  })
  
  y.train <- reactive({
    print(length(1:input$training_amount+input$prediction_gap))
    y <- VixDat[,1:input$training_amount+input$prediction_gap,as.numeric(input$target)]
    matrix(y, ncol=input$training_amount)
  })
  # once you hit go on the GUI your will have all the parameters transfered to the training
  # returns just the model, before training
  model <- eventReactive(input$go,{
    withProgress(message = "Training network", value=0, {
      model <- trainr(VixDat = VixDat.train(),
                      Y = y.train(),
                      learningrate = input$lr,
                      numepochs = input$epoch,
                      hidden_dim = input$hidden)
      return(model)
    })
  })
  # creating the test set
  VixDat.test <- reactive({
    # print(length(1:(max(dim(VixDat))-input$prediction_gap)))
    VixDat <- VixDat[,1:(max(dim(VixDat))-input$prediction_gap),]
    array(VixDat,dim=c(1,(max(dim(VixDat))-input$prediction_gap),4))
  })
  
  y.test <- reactive({
    print(length((1+input$prediction_gap):max(dim(VixDat))))
    y <- VixDat[,(1+input$prediction_gap):max(dim(VixDat)),as.numeric(input$target)]
    matrix(y, ncol=(max(dim(VixDat))-input$prediction_gap))
  })
  # testing the model vs. the testset
  y.pred <- reactive({
    predictr(model(), VixDat.test())
  })
  
  # output for the model
  output$PredictPlot <- renderPlot({
    par(mfrow=c(2,1))
    for(i in seq(4)){
      plot(VixDat.test()[,,i],yaxt="n",type="l",col=i,ylab="")
      par(new=T)
    }
    title(main="Independent variables")
    par(new=F)
    # y.test()[1,] %>% length %>% print
    plot(y.test()[1,],yaxt="n",type="l",col="green",main="Dependent variables",ylab="")
    par(new=T)
    # y.pred()[1,] %>% length %>% print
    plot(y.pred()[1,],yaxt="n",type="l",col="red",ylab="")
    abline(v=input$training_amount)
    legend("topright",col = c("green","red"),pch=1,legend = c("target","prediction"))
  })
}
# default values and GUI structure
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h4("Data parameters"),
      selectizeInput("target","currency to predict",choices=c("EURUSD"=1,"CHFUSD"=2,"GBPUSD"=3,"JPYUSD"=4),selected=1),
      numericInput("prediction_gap","Number of day in advance to predict, the bigger, the more difficult",1),
      numericInput("training_amount","Number of days of training, the bigger, the easier",1000),
      h4("Network parameters"),
      numericInput("epoch","number of epochs",50),
      numericInput("hidden","number of hidden units",10),
      numericInput("lr","learning rate",0.01),
      actionButton("go","Train")
    ),
    mainPanel(plotOutput("PredictPlot",height = "800px"))
  )
)

shinyApp(ui = ui, server = server)

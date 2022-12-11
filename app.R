library(shiny)
library(shinydashboard)
library(maps)
library(dplyr)
library(leaflet)
library(shinycssloaders)
library(shinythemes)
#library(datadigest)
library(rio)
library(DT)
library(stargazer)
library(randomForest)
library(caret)

# Dataset to be explored. You can replace `diamonds` with your own dataset.
raw_df <- diamonds

# Pre-compute some variables to be used by app
not_numeric <- sapply(names(raw_df), function(x) !is.numeric(raw_df[[x]]))
df <- raw_df
ui <- dashboardPage(skin="red",
                    
                    #add title
                    dashboardHeader(title="Rao's Final Project",titleWidth=1000),
                    
                    #define sidebar items
                    dashboardSidebar(sidebarMenu(
                      menuItem("About", tabName = "about"),
                      menuItem("Data Exploration", tabName = "de"),
                      menuItem("Modeling", tabName = "md"),
                      menuItem("Data", tabName = "data")
                    )),
                    
                    #define the body of the app
                    dashboardBody(
                      tabItems(
                        # First tab content
                        tabItem(tabName = "about",
                                fluidRow(
                                  column(6,
                                         #Description of App
                                         h1("Description of data"),
                                         
                                         #box to contain description
                                         box(background="red",width=12,
                                             imageOutput("myImage"),
                                             h5("This dataset contains information about 53,940 round-cut diamonds. How do we know? Each row of data represents a different diamond and there are 53,940 rows of data."),
                                             h5("There are 10 variables measuring various pieces of information about the diamonds."),
                                             h5("There are 3 variables with an ordered factor structure: cut, color, & clarity. An ordered factor arranges the categorical values in a low-to-high rank order. For example, there are 5 categories of diamond cuts with “Fair” being the lowest grade of cut to ideal being the highest grade."),
                                             h5("There are 6 variables that are of numeric structure: carat, depth, table, x,."),
                                             h5("There is 1 variable that has an integer structure: price."),
                                             h5("R will always have documentation (in the help page; ?diamonds)")
                                         )
                                  ),
                                  column(6,
                                         #Description of Data
                                         h1("App Functionality"),
                                         #box to contain description
                                         box(background="red",width=12,
                                             h4("This app Provides 3 tabs Data Exploration,Modeling and Data"),
                                             h4("Data Exploration Tab:-"),
                                             h5("∗ Create numerical and graphical summaries"),
                                             h5("∗ Change the type of plot shown and type of summary reported"),
                                             h5("∗ Change the variables and filter the rows to change the data used in the plots/summaries"),
                                             h4("Modeling Tab:-"),
                                             h5("The user has an option to fit three supervised learning models. Depending on their response they will fit a multiple linear regression or generalized linear regression model, regression or classification tree, and a random forest model. This page has three tabs to it:
"),                                          h5("∗ Modeling Info tab:"),
                                             h6("∗ Explains these three modeling approaches, the benefits of each,and the drawbacks of each. "),
                                             h5("∗ Model Fitting tab:"),
                                             h6(" Gives the ability to split the data into a training and test set by being able to choose the proportion of data used in each.The user can choose the model settings for each model. For all models, they can select the variables used as predictors.The user can press a button and fit all three models on the training data.· Fit statistics (such as RMSE) on the training data is reported for each model on the mainpanel along with summary of the fit "),
                                             h5("∗ Prediction tab:"),
                                             h6("The user can choose any one of the models for prediction. They can select the values of the predictors and obtain a prediction for the response."),
                                             h4("Data Tab:- The user can"),
                                             h5("∗ Scroll through the data set"),
                                             h5("∗ Subset this data set (rows and columns"),
                                             h5("∗ Save the data as a csv file ")
                                         )
                                  )
                                )
                        ),
                        
                        #actual app layout      
                        tabItem(tabName = "de",
                                fluidPage(
                                  titlePanel("Dataset Explorer"),
                                  sidebarPanel(
                                    sliderInput("sampleSize", "Plot sample size (n)", min = 1, max = nrow(df),
                                                value = min(1000, nrow(df)), step = nrow(df) / 50, round = 0),
                                    radioButtons("sampleType", "Plot sample type",
                                                 choices = list("Random n" = "random", "First n" = "first")),
                                    numericInput("sampleSeed", "Sample seed", value = 1),
                                    
                                    selectInput("x", "X", names(df)),
                                    selectInput("y", "Y", c("None", names(df)), names(df)[[2]]),
                                    
                                    # only allow non-numeric variables for color
                                    selectInput("color", "Color", c("None", names(df)[not_numeric])),
                                    
                                    p("Jitter and smoothing are only available when two numeric variables 
      are selected."),
                                    checkboxInput("jitter", "Jitter"),
                                    checkboxInput("smooth", "Smooth")
                                  ),
                                  mainPanel(
                                    tabsetPanel(type="tabs",
                                                tabPanel("Plot", plotOutput("plot")),
                                                tabPanel("Summary", verbatimTextOutput("summary")),
                                                tabPanel("Summary of avg price by carat", verbatimTextOutput("summary2"))
                                    )
                                    
                                  )
                                )
                        ),
                        #Modeling tab
                        tabItem(tabName = "md",
                                fluidPage(
                                  titlePanel("Modeling"),
                                  tabsetPanel(type = "tabs",
                                              tabPanel("Modeling Info",fluidRow(
                                                #add in latex functionality if needed
                                                withMathJax(),
                                                column(4,
                                                       #Description of mdodel 1
                                                       h3("Linear Regression"),
                                                       
                                                       #box to contain description
                                                       box(background="red",width=12,
                                                           h5("The first model is Multiple Linear Regression. We are choosing all the variables as predictors, where as keeping the price as the response"),
                                                           h6("* Simple linear regression model can be extended in many ways."),
                                                           h6("*Can include more explanatory variables and/or higher order terms and or Polynomial terms"),
                                                           h6("EX of Higher terms is"),
                                                           h6("\\(\\ Yi = β0 + \\)"),
                                                           h6("\\(\\ β1x1i + β2x2i \\)"),
                                                           h6("\\( + β3x1ix2i\\)"),
                                                           h6("\\( + Ei\\)"),
                                                           h6("EX of polynomial terms is"),
                                                           h6("\\(\\ Yi = β0 + β1xi \\)"),
                                                           h6("\\( + β2x2i \\)"),
                                                           h6("\\( + Ei\\)"),
                                                           h6("We are going to use price~. as the formula"),
                                                           h5("Pros:"),
                                                           h6("The first is the ability to determine the relative influence of one or more predictor variables to the criterion value."),
                                                           h6("The second advantage is the ability to identify outliers, or anomalies"),
                                                           h5("Cons:"),
                                                           h6("Any disadvantage of using a multiple regression model usually comes down to the data being used. Two examples of this are using incomplete data and falsely concluding that a correlation is a causation.")
                                                           
                                                           
                                                       )
                                                ),
                                                column(4,
                                                       #Description of Data
                                                       h3("Bagged Tree Model"),
                                                       #box to contain description
                                                       box(background="red",width=12,
                                                           h5("The Second model is Bagged Tree Regression. We are choosing all the variables as predictors, where as keeping the price as the response"),
                                                           h6("Split up predictor space into regions, different predictions for each region"),
                                                           h6("Classification tree if goal is to classify (predict) group membership"),
                                                           h6("Regression tree if goal is to predict a continuous response"),
                                                           h6("We are going to use price~. as the formula for the Regression Tree"),
                                                           h5("Pros:"),
                                                           h6("* Simple to understand and interpret output"),
                                                           h6("* Predictors don't need to be scaled"),
                                                           h6("* No statistical assumptions necessary"),
                                                           h6("* Built in variable selection"),
                                                           h5("Cons:"),
                                                           h6("* Small changes in data can vastly change tree"),
                                                           h6("* Greedy algorithm necessary (no optimal algorithm)"),
                                                           h6("* Need to prune (usually)")
                                                       )
                                                ),column(4,
                                                         #Description of Data
                                                         h3("Random Forest Model"),
                                                         #box to contain description
                                                         box(background="red",width=12,
                                                             h5("The Third model is Random Forest Regression"),
                                                             h6("Uses same idea as bagging"),
                                                             h6("Create multiple trees from bootstrap samples"),
                                                             h6("Average results"),
                                                             h6("We are going to use the number of predictors as 3(mtry). as the formula"),
                                                             h5("Difference from Bagged Tree model"),
                                                             h6("Don't use all predictors!"),
                                                             h6("Use a random subset of predictors for each bootstrap sample/tree fit")                                                           )
                                                )
                                              )),
                                              tabPanel("Model Fitting",
                                                       fluidRow(
                                                         column(12,
                                                                sidebarPanel(sliderInput(
                                                                  "Slider1",
                                                                  label = h4("Train/Test Split %"),
                                                                  min = 0,
                                                                  max = 100,
                                                                  value = 75
                                                                ),
                                                                textOutput("cntTrain"),
                                                                textOutput("cntTest")),
                                                                column(4,
                                                                       selectInput(
                                                                         "SelectX",
                                                                         label = "Select variables:",
                                                                         choices = names(raw_df),
                                                                         #selected = names(raw_df)[3],
                                                                         multiple = TRUE
                                                                       )),
                                                                # verbatimTextOutput("ip"),
                                                                # actionButton("do5", "MLR"),
                                                                # actionButton("do6", "Bagged Tree"),
                                                                # actionButton("do7", "Random Forest"),
                                                         ),fluidRow(
                                                           tabsetPanel(type="tabs",
                                                                       tabPanel("MLR",column(6,
                                                                                             box(
                                                                                               withSpinner(verbatimTextOutput("Model1")),
                                                                                               width = 6,
                                                                                               title = "Model Summary"
                                                                                             )),column(6,
                                                                                                       box(
                                                                                                         withSpinner(verbatimTextOutput("ImpVar1")),
                                                                                                         width = 5,
                                                                                                         title = "Variable Importance"
                                                                                                       ))),
                                                                       tabPanel("Bagged Tree",column(8,
                                                                                                     box(
                                                                                                       withSpinner(plotOutput("rfplot2")),
                                                                                                       width = 6,
                                                                                                       title = "Variable Imp Plot"
                                                                                                     )),column(4,
                                                                                                               box(
                                                                                                                 withSpinner(verbatimTextOutput("Model2")),
                                                                                                                 width = 6,
                                                                                                                 title = "Model Summary RMSE"
                                                                                                               ))),
                                                                       tabPanel("Random Forest",column(6,
                                                                                                       box(
                                                                                                         withSpinner(plotOutput("rfplot3")),
                                                                                                         width = 6,
                                                                                                         title = "Variable Imp Plot"
                                                                                                       )),column(6,
                                                                                                                 box(
                                                                                                                   withSpinner(verbatimTextOutput("Model3")),
                                                                                                                   width = 6,
                                                                                                                   title = "Model Summary RMSE"
                                                                                                                 )))
                                                                       
                                                           )
                                                         )
                                                       )
                                              ),
                                              tabPanel("Prediction",
                                                       fluidRow(
                                                         selectizeInput('var10', 'Select color', choices = c("choose" = "", levels(df$color))),
                                                         actionButton("pab", "Subset Rows"),
                                                         dataTableOutput("pred")
                                                         # box(withSpinner(plotOutput("Prediction")), width = 6, title = "Best Fit Line"),
                                                         # box(withSpinner(plotOutput("residualPlots")), width = 6, title = "Diagnostic Plots")
                                                       ))
                                  )
                                )
                        ),
                        
                        #Data tab
                        tabItem(tabName = "data",
                                fluidRow(
                                  sidebarPanel(
                                    actionButton("do1", "Data"),
                                    selectizeInput('var1', 'Select color', choices = c("choose" = "", levels(df$color))), 
                                    selectizeInput('var2', 'Select cut', choices = c("choose" = "", levels(df$cut))),
                                    selectizeInput('var3', 'Select clarity', choices = c("choose" = "", levels(df$clarity))),
                                    actionButton("do3", "Subset Rows"),
                                    selectInput("Columns","Columns", names(raw_df), multiple = TRUE),
                                    actionButton("do2", "Subset Columns"),
                                    downloadButton("do4", "Export")
                                  ),
                                  mainPanel(
                                    
                                    #verbatimTextOutput("dfStr")
                                    dataTableOutput("summary1")
                                  )
                                )
                        )
                      )
                      
                      
                    )
)

# Define server logic ----
server <- function(input, output,session) {
  #-------------------------About----------------------
  output$myImage <- renderImage({
    filename <- normalizePath(file.path('/Users/umerao/Desktop/SASHomeWIN/images','Diamonds.jpg'))
    # Return a list containing the filename
    list(src = filename, contentType = 'image/jpeg', width = 280, height = 400,alt = "Alternate text")
  })
  #------------------------Data Exploration------------------------------
  # get new dataset sample for plotting
  idx <- reactive({
    if (input$sampleType == "first") {
      1:input$sampleSize
    } else {
      set.seed(input$sampleSeed)
      sample(nrow(raw_df), input$sampleSize)
    }
  })
  df <- reactive(raw_df[idx(), , drop = FALSE])
  
  # Get head of selected data
  output$snippet <- renderPrint({
    head(df(), n = 15)
  })
  
  plot_type <- reactive({
    if (input$y != "None")
      is.numeric(raw_df[[input$x]]) + is.numeric(raw_df[[input$y]])
    else
      -1
  })
  # Create plot
  output$plot <- renderPlot({
    if (plot_type() == 2) {
      # both numeric variables: scatterplot
      # also allow for color, jitter & smoothing
      p <- ggplot(df(), aes_string(x = input$x, y = input$y))
      
      if (input$jitter)
        p <- p + geom_jitter(alpha = 0.5)
      else
        p <- p + geom_point(alpha = 0.5)
      
      if (input$smooth)
        p <- p + geom_smooth()
      
      # color change
      if (input$color != "None")
        p <- p + aes_string(color = input$color)
    } else if (plot_type() == 1) {
      # one numeric var, one character var: boxplot
      # allow color, don't allow jitter or smoothing
      p <- p <- ggplot(df(), aes_string(x = input$x, y = input$y)) +
        geom_boxplot()
      
      # fill change
      if (input$color != "None")
        p <- p + aes_string(fill = input$color)
    } else if (plot_type() == 0) {
      # two character variables: heatmap
      # don't allow color, jitter or smoothing
      temp_df <- reactive(df()[, c(input$x, input$y), drop = FALSE] %>%
                            group_by(across()) %>%
                            summarize(count = n())
      )
      p <- ggplot(temp_df(),
                  mapping = aes_string(x = input$x, y = input$y, fill = "count")) +
        geom_tile() +
        scale_fill_gradient(low = "#e7e7fd", high = "#1111dd")
    } else {
      # only one variable: univariate plot
      # allow color, don't allow jitter or smoothing
      p <- ggplot(df(), aes_string(x = input$x))
      
      if (is.numeric(raw_df[[input$x]]))
        p <- p + geom_histogram()
      else
        p <- p + geom_bar()
      
      # fill change
      if (input$color != "None")
        p <- p + aes_string(fill = input$color)
    }
    
    # add title
    if (plot_type() >= 0) {
      p <- p + labs(title = paste(input$y, "vs.", input$x))
    } else {
      p <- p + labs(title = paste("Distribution of", input$x))
    }
    
    # add styling
    p <- p +
      theme_bw() +
      theme(plot.title = element_text(size = rel(1.8), face = "bold", hjust = 0.5),
            axis.title = element_text(size = rel(1.5)))
    
    print(p)
    
  }, height=500)
  
  # Generate data summaries
  output$summary <- renderPrint({
    summary(raw_df)
  })
  output$summary2 <- renderPrint({
    new_data1<- raw_df %>% group_by(carat) %>% summarise(avg = mean(price))
    new_data1
  })
  output$str <- renderPrint({
    str(raw_df)
  })
  #--------------------------------Modeling--------------------------
  InputDataset_model <- reactive({
    if (is.null(input$SelectX)) {
      dt <- raw_df
    }
    else{
      dt <- raw_df[, c("price",input$SelectX)]
    }
    
  })
  
  splitSlider <- reactive({
    input$Slider1 / 100
  })
  
  set.seed(100)  # setting seed to reproduce results of random sampling
  trainingRowIndex <-
    reactive({
      sample(1:nrow(InputDataset_model()),
             splitSlider() * nrow(InputDataset_model()))
    })# row indices for training data
  
  trainingData <- reactive({
    tmptraindt <- InputDataset_model()
    tmptraindt[trainingRowIndex(), ]
  })
  
  testData <- reactive({
    tmptestdt <- InputDataset_model()
    tmptestdt[-trainingRowIndex(),]
  })
  
  output$cntTrain <-
    renderText(paste("Train Data:", NROW(trainingData()), "records"))
  
  output$cntTest <-
    renderText(paste("Test Data:", NROW(testData()), "records"))
  #testdt<-testData()
  #Code section for Linear Regression-----------------------------------------------------------------------------
  # subset columns button
  #observeEvent(input$do5, {
  
  
  f <- reactive({
    #as.formula(paste( "price ~ ",input$SelectX,))
    # var=paste(input$SelectX,collapse = "+")
    # as.formula(paste("price~",var))
    as.formula(paste("price~ ."))
  })
  
  
  Linear_Model <- reactive({
    #vars <- as.matrix(raw_df[, input$SelectX])
    lm(f(), data = trainingData())
    #lm(price ~ .,data=trainingData())
  })
  
  output$Model1 <- renderPrint(summary(Linear_Model()))
  #Variable Importance
  tmpImp <- reactive({
    
    imp <- as.data.frame(varImp(Linear_Model()))
    imp <- data.frame(overall = imp$Overall,
                      names   = rownames(imp))
    imp[order(imp$overall, decreasing = T),]
    
  })
  
  output$ImpVar1 <- renderPrint(tmpImp())
  #output$rfplot <- renderPlot(barPlot(Linear_Model()))
  #-------------------------------prediction-------------------------
  price_predict <- reactive({
    predict(Linear_Model(), testData())
  })
  
  tmp <- reactive({
    
    tmp1 <-testData()
    
    
    # tmp1 <- select(testData() ,price)
    #tmp1[, "price"]
    #tmp1 <- %>% select(var1)
  })
  
  observeEvent(input$pab, {
    tmp1 <- reactive({
      tmp<- testData() %>% filter(color==input$var10)
      tmp2 <-tmp })
    price_predict <- reactive({
      predict(Linear_Model(), tmp1())
    })
    actuals_preds <-
      reactive({
        #  new_data<-testData[,"price"]
        data.frame(cbind(actuals = tmp1(), predicted = price_predict()))
        
      })
    output$pred <- DT::renderDataTable ({
      actuals_preds()
      
      
    })
  })
  actuals_preds <-
    reactive({
      #  new_data<-testData[,"price"]
      data.frame(cbind(actuals = tmp(), predicted = price_predict()))
    })
  output$pred1 <-reactive({renderText({
    
    paste0(mean(actuals_preds()$price))
  }) })
  output$pred <- DT::renderDataTable ({
    actuals_preds()
    
  })
  # Fit <-
  #   reactive({
  #     (
  #       plot(
  #         actuals_preds()$actuals,
  #         actuals_preds()$predicted,
  #         pch = 16,
  #         cex = 1.3,
  #         col = "blue",
  #         main = "Best Fit Line",
  #         xlab = "Actual",
  #         ylab = "Predicted"
  #       )
  #     )
  #   })
  # 
  # output$Prediction <- renderPlot(Fit())
  # 
  # output$residualPlots <- renderPlot({
  #   par(mfrow = c(2, 2)) # Change the panel layout to 2 x 2
  #   plot(Linear_Model())
  #   par(mfrow = c(1, 1)) # Change back to 1 x 1
  #   
  # })
  
  # })
  #--------------------Bagged Tree------------------------
  # observeEvent(input$do6, {
  
  
  
  class_Model <- reactive({
    bgFit <- randomForest(f(), data = trainingData(),
                          ntree = 20, importance = TRUE)
    mean(sqrt(bgFit$mse))
    # pred <- predict(bgFit, obs = testData()$price)
    # treeRMSE <- sqrt(mean((pred-testData()$price)^2))
  })
  
  class_Model1 <- reactive({
    bgFit <- randomForest(f(), data = trainingData(),
                          ntree = 20, importance = TRUE)
  })
  
  
  output$Model2 <- renderPrint(paste(class_Model()))
  #Variable Importance
  tmpImp <- reactive({
    
    imp <- as.data.frame(varImp(class_Model1()))
    imp <- data.frame(overall = imp$Overall,
                      names   = rownames(imp))
    imp[order(imp$overall, decreasing = T),]
    
  })
  
  #output$ImpVar <- renderPrint(tmpImp())
  output$rfplot2 <- renderPlot(varImpPlot(class_Model1(),type=1))
  # })
  #--------------------RandomForest Tree------------------------
  #observeEvent(input$do7, {
  
  
  
  
  rf_Model <- reactive({
    
    rfFit <- randomForest(price ~ ., data = trainingData(), mtry = 3,
                          ntree = 20, importance = TRUE)
    mean(sqrt(rfFit$mse))
  })
  
  rf_Model1 <- reactive({
    
    rfFit <- randomForest(price ~ ., data = trainingData(),mtry=3,
                          ntree = 20, importance = TRUE)
  })
  
  
  output$Model3 <- renderPrint(paste(rf_Model()))
  #Variable Importance
  # tmpImp <- reactive({
  # 
  #   imp <- as.data.frame(varImp(rf_Model1()))
  #   imp <- data.frame(overall = imp$Overall,
  #                     names   = rownames(imp))
  #   imp[order(imp$overall, decreasing = T),]
  # 
  # })
  tmpImp <- reactive({
    
    imp <- as.data.frame(varImp(rf_Model1()))
    imp <- data.frame(overall = imp$Overall,
                      names   = rownames(imp))
    imp[order(imp$overall, decreasing = T),]
    
  })
  #output$ImpVar <- renderPrint(tmpImp())
  
  output$rfplot3 <- renderPlot(varImpPlot(rf_Model1(),type=1), width = 300)
  #})
  
  #--------------------------------Data-----------------------------
  # Data button
  observeEvent(input$do1, {
    output$summary1 <- DT::renderDataTable ({
      new_data<-raw_df
      new_data
    })
  })
  # subset rows button
  observeEvent(input$do2, {
    output$summary1 <- DT::renderDataTable ({
      new_data<-raw_df[,input$Columns]
      new_data
    })
  })
  # subset columns button
  observeEvent(input$do3, {
    output$summary1 <- DT::renderDataTable ({
      new_Data<-raw_df %>% filter(color == input$var1) %>% filter(cut == input$var2) %>% filter(clarity == input$var3)
      new_Data
    })
  })
  # Dowload CSV
  output$do4 <- downloadHandler(
    filename = function() {
      paste0("diamonds.csv")
    },
    content = function(con) {
      write.csv(raw_df, con, row.names = F, na = "")
    }
  )
}

# Run the app ----
shinyApp(ui = ui, server = server)
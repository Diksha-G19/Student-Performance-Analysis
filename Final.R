# Load libraries
library(shiny)
library(randomForest)
library(ggplot2)
library(dplyr)
library(caret)
library(e1071)

# Load dataset
data <- read.csv("student-mat-formatted.csv")

# Classification label
G3_column <- data$G3
pass_label <- as.factor(ifelse(G3_column >= 10, "Pass", "Fail"))

# Select prediction features
selected_vars <- c("G1", "G2", "absences", "studytime", "schoolsup", "G3")
data <- data[ , names(data) %in% selected_vars | names(data) == "pass"]
data$pass <- pass_label
data$schoolsup <- as.factor(data$schoolsup)

# Train/test split
set.seed(123)
splitIndex <- createDataPartition(data$pass, p = 0.8, list = FALSE)
trainData <- data[splitIndex, ]
testData <- data[-splitIndex, ]

# Ensure factor consistency
trainData$schoolsup <- factor(trainData$schoolsup)
testData$schoolsup <- factor(testData$schoolsup, levels = levels(trainData$schoolsup))

# Train models
rf_model_cls <- randomForest(pass ~ G1 + G2 + absences + studytime + schoolsup, data = trainData, ntree = 100, importance = TRUE)
rf_model_reg <- randomForest(G3 ~ G1 + G2 + absences + studytime + schoolsup, data = trainData, ntree = 100)

# Model metrics
pred_cls <- predict(rf_model_cls, testData)
conf_mat <- confusionMatrix(pred_cls, testData$pass)
pred_reg <- predict(rf_model_reg, testData)
mse <- mean((pred_reg - testData$G3)^2)
rmse <- sqrt(mse)
r2 <- cor(pred_reg, testData$G3)^2

# UI
ui <- fluidPage(
  titlePanel("\U0001F393 Student Performance Analysis"),
  sidebarLayout(
    sidebarPanel(
      h4("Enter Student Details:"),
      numericInput("G1", "Grade Period 1 (G1)", value = 10, min = 0, max = 20),
      numericInput("G2", "Grade Period 2 (G2)", value = 10, min = 0, max = 20),
      numericInput("absences", "Absences", value = 4, min = 0, max = 93),
      selectInput("studytime", "Study Time", choices = c(1, 2, 3, 4)),
      selectInput("schoolsup", "Family Support (yes/no)", choices = levels(trainData$schoolsup)),
      actionButton("predict", "Predict Performance")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Prediction",
                 h4("Predicted Status (Classification):"),
                 verbatimTextOutput("prediction_class"),
                 h4("Predicted Grade G3 (Regression):"),
                 verbatimTextOutput("prediction_reg")
        ),
        tabPanel("Model Metrics",
                 h4("Classification Metrics:"),
                 verbatimTextOutput("cls_metrics"),
                 h4("Regression Metrics:"),
                 verbatimTextOutput("reg_metrics")
        ),
        tabPanel("Feature Importance",
                 plotOutput("importancePlot")
        ),
        tabPanel("Confusion Matrix",
                 plotOutput("confMatPlot")
        ),
        tabPanel("EDA",
                 h4("Histogram of Final Grades (G3):"),
                 plotOutput("histPlot"),
                 h4("Boxplot of G3 by Study Time:"),
                 plotOutput("boxPlot"),
                 h4("Scatter Plot of G1 vs G3:"),
                 plotOutput("scatterPlot")
        ),
        tabPanel("Dataset Summary",
                 h4("Structure:"),
                 verbatimTextOutput("structureOutput"),
                 h4("Summary Statistics:"),
                 verbatimTextOutput("summaryOutput")
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  inputData <- reactive({
    req(input$predict)
    isolate({
      data.frame(
        G1 = input$G1,
        G2 = input$G2,
        absences = input$absences,
        studytime = as.numeric(input$studytime),
        schoolsup = factor(input$schoolsup, levels = levels(trainData$schoolsup))
      )
    })
  })
  
  output$prediction_class <- renderPrint({
    req(input$predict)
    pred <- predict(rf_model_cls, inputData())
    paste("Predicted Result:", as.character(pred))
  })
  
  output$prediction_reg <- renderPrint({
    req(input$predict)
    pred <- predict(rf_model_reg, inputData())
    paste("Predicted Grade (G3):", round(pred, 1))
  })
  
  output$cls_metrics <- renderPrint({
    acc <- conf_mat$overall["Accuracy"]
    prec <- conf_mat$byClass["Precision"]
    rec <- conf_mat$byClass["Recall"]
    f1 <- conf_mat$byClass["F1"]
    cat("Accuracy:", round(acc, 3), "\n")
    cat("Precision:", round(prec, 3), "\n")
    cat("Recall:", round(rec, 3), "\n")
    cat("F1 Score:", round(f1, 3), "\n")
  })
  
  output$reg_metrics <- renderPrint({
    cat("MSE:", round(mse, 2), "\n")
    cat("RMSE:", round(rmse, 2), "\n")
    cat("R² Score:", round(r2, 3), "\n")
  })
  
  output$importancePlot <- renderPlot({
    data_vis <- read.csv("student-mat-formatted.csv")
    data_vis$pass <- as.factor(ifelse(data_vis$G3 >= 10, "Pass", "Fail"))
    data_vis$schoolsup <- as.factor(data_vis$schoolsup)
    data_vis$famsup <- as.factor(data_vis$famsup)
    
    extra_vars <- c("G1", "G2", "absences", "studytime", "schoolsup",
                    "failures", "goout", "Dalc", "Walc", "health", "famsup")
    
    rf_vis <- randomForest(pass ~ ., data = data_vis[, c(extra_vars, "pass")], ntree = 100, importance = TRUE)
    
    imp <- importance(rf_vis)
    imp_df <- data.frame(Feature = rownames(imp), Importance = imp[, "MeanDecreaseGini"])
    
    ggplot(imp_df, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Extended Feature Importance (Classification)", x = "Feature", y = "Importance")
  })
  
  output$confMatPlot <- renderPlot({
    cm_df <- as.data.frame(conf_mat$table)
    ggplot(cm_df, aes(x = Prediction, y = Reference, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = Freq), color = "white", size = 6) +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(title = "Confusion Matrix", x = "Predicted", y = "Actual") +
      theme_minimal()
  })
  
  output$histPlot <- renderPlot({
    ggplot(data, aes(x = G3)) +
      geom_histogram(fill = "orange", bins = 20, color = "black") +
      theme_minimal() +
      labs(title = "Histogram of Final Grades (G3)", x = "G3", y = "Count")
  })
  
  output$boxPlot <- renderPlot({
    ggplot(data, aes(x = as.factor(studytime), y = G3)) +
      geom_boxplot(fill = "lightgreen") +
      theme_minimal() +
      labs(title = "Boxplot of G3 by Study Time", x = "Study Time", y = "G3")
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(data, aes(x = G1, y = G3)) +
      geom_point(color = "purple", alpha = 0.6) +
      theme_minimal() +
      labs(title = "Scatter Plot of G1 vs G3", x = "G1", y = "G3")
  })
  
  output$structureOutput <- renderPrint({
    str(data)
  })
  
  output$summaryOutput <- renderPrint({
    summary(data)
  })
}

# Run app
shinyApp(ui = ui, server = server)

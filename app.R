library(shiny)
library(bslib)
library(ggplot2)
library(reshape2)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "united"),
  tabsetPanel(
    tabPanel("Input",
             numericInput("first_year", "First year", value = 2017, min = 0, max = 9999),
             numericInput("last_year", "Last year", value = 2019, min = 0, max = 9999), 
             numericInput("tail_factor", "Tail Factor", value = 1.1, min = 0, max = 9999),
             uiOutput("paid_claims")     
    ),
    tabPanel("Results",
             "Cumulative Paid Claims Table",
             tableOutput("cum_table"),
             "Cumulative Paid Claims Graph",
             plotOutput("cum_graph"))
  )
 
)


server <- function(input, output, session) {

  num_years <- reactive(input$last_year - input$first_year + 1)

  first_year <- reactive(input$first_year)
  
  last_year <- reactive(input$last_year)
  
  tail_factor <- reactive(input$tail_factor)
  
  
  claims <- reactive(
    {
      claim_list <- list()
      for (i in 1:num_years()) {
        claim_list[[i]] <- lapply(1:(num_years() - i + 1), function(k) {
          numericInput(paste0(first_year() + i - 1, "input", k),
                       paste0("Loss Year ", first_year() + i - 1, ", Development Year ", k),
                       value = 1, min = 0, max = Inf)
        } )
      }
      claim_list
    }
  )
  
  output$paid_claims <- renderUI(claims())
  
  
  cum_claims <- reactive(
    {
      cum <- vector(mode = "list", length = num_years()) 
      for (i in 1:num_years()){
        cum_sum <- 0
        for (k in 1:(num_years() - i + 1)){
          cum_sum <- cum_sum + input[[paste0((first_year() + i - 1), "input", k)]]
          cum[[i]] <- c(cum[[i]], cum_sum )
        }
      }
      cum
    }
  )
  
  
  dev_factors <- reactive(
    {
     df_list <- c()
     for (i in 2:num_years()){
       sum_1 <- 0
       sum_2 <- 0
       for (k in 1:(num_years() - i + 1)){
         sum_1 <- sum_1 + cum_claims()[[k]][i]
         sum_2 <- sum_2 + cum_claims()[[k]][i-1]
       }
       df_list <- c(df_list, sum_1/sum_2)
     }
     df_list <- c(df_list, tail_factor())
     df_list
    }
  )
  
  output$dev_factors <- renderPrint(dev_factors())
  
  
  triangle_result <- reactive(
    {
      result <- list()
      for (i in 1:(num_years())){
        pred_claims <- cum_claims()[[i]][num_years() - i + 1] 
        pred_array <- c()
        for (k in 1:i){
          pred_claims <- pred_claims * (dev_factors()[num_years() - i + k])
          pred_array <- c(pred_array, pred_claims)
        }
        result[[i]] <- pred_array
      }
      result
    }
  )
  
  cum_table <- reactive(
    {
      cum_matrix <- as.data.frame(matrix(0, num_years(), num_years() + 1))
      for (i in 1:num_years()) {
        cum_matrix[i, ] <- round(c(cum_claims()[[i]], triangle_result()[[i]]), digits = 0)
      }
      cum_matrix <- cbind(seq(first_year(), last_year()), cum_matrix)
      colnames <- c("Loss Year")
      for (i in 1:(num_years()+1)){
        colnames <- c(colnames, paste("Development Year", i, sep = " "))
      }
      colnames(cum_matrix) <- colnames
      cum_matrix
    }
  )
  
  shown_table <- reactive(
    {
      remove_decimals <- function(x){
        format(x, nsmall = 0, big.mark = ",")
      }
      shown <- cbind(cum_table()[, 1], sapply(cum_table()[, -1], remove_decimals))
      colnames(shown)[1] <- "Loss Year"
      shown
    }
  )
  
  output$cum_table <- renderTable(shown_table())
  
  
  output$cum_graph <- renderPlot(
    {
      table_to_melt <- cum_table()
      colnames(table_to_melt) <- c("LossYear", seq(1, num_years() + 1))
      cum_table_melted <- melt(table_to_melt, id.vars = "LossYear")
      cum_table_melted$"LossYear" <- as.factor(cum_table_melted$"LossYear")
      print(ggplot(cum_table_melted, aes(x = variable, y = value), label = value)
      + geom_line(aes_string(color = "LossYear", group = "LossYear"))
      + labs(x = "Development Year", y = "Cumulative Paid Claims ($)")
      + geom_point(aes_string(color = "LossYear", group = "LossYear"))
      + geom_text(aes(label = value), vjust = -1))
    }
  )
  
}


shinyApp(ui, server)


















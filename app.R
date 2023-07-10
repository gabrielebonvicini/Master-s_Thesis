##To call the functions and data needed within the application 
source("help.R")

#Section 1: Shiny application -----------------------------------------------------

##In this section a Shiny application to automatize the results will be created

##1.1: UI ---------------------------------------------------------

ui <- fluidPage(
  ##1 Main and subtitle 
  h2("Analyse transaction costs and returns"), #title
  p("You can select the field and quartile of which you want to assess the transaction costs
    and returns of a simple strategy. The strategy buy stocks at opening price
    and sell them at closing price each day in the period from 2018 to end of 2021"), #subtitle
  
  ##2 Inputs definition
  ##2.1 Input 1 -- > Quartile 
  sidebarLayout(
    selectInput(inputId = "quartile",
                label = "Select the Quartile",
                choices = c("lower", "middle", "upper", "comparison lower/upper")
    ),
    ##2.2 Input 2 -- > Pillar 
    selectInput(
      inputId = "pillar",
      label = "Select the pillar",
      choices = c("ENV", "GOV", "SOCIAL", "ESG")
    )
    ##End of sidebarLayout()
  ),
  
  ##3 Outputs
  mainPanel(
    tableOutput(outputId = "Output_tab"), #table output
    
    plotOutput(outputId = "Output_plot") #plot output
    ##End of mainPanel()
  )
  ##End of fluid page
)

##1.2: Server ----------------------------------------------------------------

server <- function(input, output){ 
  
  ##1 Table Output (dataset definition)
  output$Output_tab <- renderUI({
    ##1.1 Create a different table if comaprison is chosen (comparison needs two tables)
    if(input$quartile == "comparison lower/upper"){
      ##1.2 Data.frames  
      df1 <- trade_analysis(quartile = "lower", pillar = input$pillar)
      df2 <- trade_analysis(quartile = "upper", pillar = input$pillar)
      
      ##1.3 Data.frame for the table (Upper quartile) 
      table_upper <- data.frame(
        c(
          "Returns of the period" = paste0( "$ ", sum(df2$returns_dollar,na.rm = T) %>% round(digits = 2) ) ,
          "Transaction costs (roundtrip costs)" = paste0( "$ ", sum(df2$tcost_dollar, na.rm = T) %>% round(digits = 2) ),
          "Higher daily Return" = paste0( max(df2$returns_perce[,1]*100)%>% round(digits = 2), " %" ), 
          "Lower daily Return" = paste0( min(df2$returns_perce[,1]*100) %>% round(digits = 2), " %")
        )
      )
      
      table_upper <- cbind(table_upper, c(
        "", "", rownames( df2$returns_perce)[ which.max(df2$returns_perce[,1])], 
        rownames( df2$returns_perce)[ which.min(df2$returns_perce[,1])]
      ) )
      colnames(table_upper) <- c( "Value" , "Date occurred" ) 
      
      ##1.4 Data.frame for the table (lower quartile)
      table_lower <- data.frame(
        c(
          "Returns of the period" = paste0( "$ ", sum(df1$returns_dollar,na.rm = T) %>% round(digits = 2) ) ,
          "Transaction costs (roundtrip costs)" = paste0( "$ ", sum(df1$tcost_dollar,na.rm = T) %>% round(digits = 2) ),
          "Higher daily Return" = paste0( max(df1$returns_perce[,1]*100)%>% round(digits = 2), " %" ), 
          "Lower daily Return" = paste0( min(df1$returns_perce[,1]*100) %>% round(digits = 2), " %")
        )
      )
      
      table_lower <- cbind(table_lower, c(
        "", "", rownames( df1$returns_perce)[ which.max(df1$returns_perce[,1])], 
        rownames( df1$returns_perce)[ which.min(df1$returns_perce[,1])]
      ) )
      colnames(table_lower) <- c( "Value" , "Date occcurred" )
      
      ##1.5 Upper + Lower 
      table <- cbind(table_upper, table_lower)
    } 
    ##1.5 Else if not comparison ... one single dataframe should be created
    else {
      df <- trade_analysis(quartile = input$quartile, pillar = input$pillar)
      table <- data.frame(
        c(
          "Returns of the period" = paste0( "$ ", sum(df$returns_dollar,na.rm = T) %>% round(digits = 2) ) ,
          "Transaction costs (roundtrip costs)" = paste0( "$ ", sum(df$tcost_dollar,na.rm = T) %>% round(digits = 2) ),
          "Higher daily Return" = paste0( max(df$returns_perce[,1]*100)%>% round(digits = 2), " %" ), 
          "Lower daily Return" = paste0( min(df$returns_perce[,1]*100) %>% round(digits = 2), " %")
        )
      )
      
      table <- cbind(table, c(
        "", "", rownames( df$returns_perce)[ which.max(df$returns_perce[,1])], 
        rownames( df$returns_perce)[ which.min(df$returns_perce[,1])]
      ) )
      colnames(table) <- c( "Value" , "Date occcurred" )
      ##End of else
    }
    
    ##2 Table Output (table creation)
    
    ##2.1 If comaprison upper lower ... 
    if(input$quartile == "comparison lower/upper"){
      t <- table %>% 
        kbl( caption = paste0("Period from ", rownames(df2$tcost_perce)[1], " to ", 
                              rownames(df2$tcost_perce)[nrow(df2$tcost_perce)]) ) %>%
        kable_styling(full_width = T, html_font = "Cambria", "striped")  %>%
        add_header_above( c("." = 1, "Upper quartile" = 2, "Lower quartile" = 2 ) ) %>%
        add_footnote(label = "The Upper quartile is the one with the stocks with more disagreement" )
      ##End of if
    } 
    ##Else if not comparison upper/lower ...
    else {
      t <- table %>%
        kbl( caption = paste0("Period from ", rownames(df$tcost_perce)[1], " to ", 
                              rownames(df$tcost_perce)[nrow(df$tcost_perce)]) ) %>%
        kable_styling(full_width = T, html_font = "Cambria", "striped") 
      ##End of else
    }
    
    return( HTML(as.character(t)) ) #transform the HTML into a table 
    ##End of renderUI() (table output)
  })
  
  ##3 Plot Output 
  output$Output_plot <- renderPlot({
    ##3.1 Again if comaprison upper/lower is selected, more plots are needed 
    if(input$quartile == "comparison lower/upper"){
      par(mfrow = c(2,2)) #four charts
      
      ##3.2 Image of the cumulated transaction costs and returns
      df1 <- trade_analysis("lower" , pillar = input$pillar)
      df2 <- trade_analysis("upper", pillar = input$pillar)
      
      ##3.3 Build the cumulative sum of returns
      cumu_sum_ret <- numeric(length(df1$returns_perce[,1])) #empty element
      for(i in 1:length(df1$returns_perce[,1])){   
        if(i == 1){ #the first element
          cumu_sum_ret[i] <- df1$returns_perce[,1][i]*100 
          ##End of if 
        } else { #The following elements  
          cumu_sum_ret[i] <- cumu_sum_ret[i-1] + df1$returns_perce[,1][i]*100
          ##End of else
        }
        ##End of loop
      }
      
      ##3.4 The same for the upper quartile 
      
      ## Buidl the cumulative sum of returns
      cumu_sum_ret2 <- numeric(length(df2$returns_perce[,1])) #now for returns
      for(i in 1:length(df2$returns_perce[,1])){   
        if(i == 1){ 
          cumu_sum_ret2[i] <- df2$returns_perce[,1][i]*100 
          ##End of if 
        } else { 
          cumu_sum_ret2[i] <- cumu_sum_ret2[i-1] + df2$returns_perce[,1][i]*100
          ##End of else
        }
        ##End of loop
      }
      
      ##3.5 Set the x-axis
      ##Date axis 
      date_axes <- as.Date( colnames(prices_day)[4:length(prices_day)], 
                            tryFormats = "%d/%m/%Y" ) #the x_axes need to be of a date format
      
      ##3.6 Plot the lower quartile 
      ##Scatter plot of transaction costs 
      plot(df1$tcost_perce[,1]*100, x = date_axes, xaxt = "n" , yaxt = "n" , 
           #xaxt = "n" -- > do not show x-axis
           ylim = c(-10,10), cex = 0.3, pch = 20, #pch = 20 -- > fulld dots 
           xlab =  "Dates", ylab = "percentage") 
      ##Scatter plot of returns
      points(y = df1$returns_perce[,1]*100, x = date_axes, col = "red", cex = 0.3, pch = 20)  
      ##Legend
      legend("topright", legend = c("Costs", "Returns"), col = c("black", "red"),pch = 20)
      
      ##3.7 Fix the axes 
      ##The x-axix (1)
      axis.Date(1, at = seq(date_axes[1], date_axes[length(date_axes)] , "6 months") ,  
                labels = T, format = "%m/%Y", tcl = -0.5) #add an axes with dates 
      ##The y-xis (2) 
      axis(2, at = seq(-10,10,by = 1), labels = F )  
      axis(2, at = seq(-10,10,by = 2), labels = paste0(seq(-10,10,by = 2) , "%"), las = 1 ) 
      #las = 1 -- > show horizontally                           
      
      ##3.8 Plot line of cumulated rets   
      plot(x = date_axes, y = cumu_sum_ret, type = "l", xaxt = "n", yaxt = "n" ,  
           ylim = c(-50,50), ylab = "percentage", xlab = "Dates") 
      ##x-axis (1)
      axis.Date(1, at = seq(date_axes[1], date_axes[length(date_axes)] , "6 months") , 
                labels = T, format = "%m/%Y", tcl = -0.5) #add an axes with dates 
      ##Legend
      legend("topright", legend = c( "Cumulative returns"), 
             col = c("black"), lwd = 2)
      
      ##y-axis (2)
      axis(2, at =  seq(-50,50, by = 10), labels = paste0( seq(-50,50, by = 10) , "%") , 
           las = 1) 
      
      ##Add a main 
      title( paste0(input$pillar, "  ", " lower quartile portfolio" ), 
             outer = T, line = -1) #line = -1 -- > above the first two charts
      
      ##3.9 Do the same for the upper quartile
      
      ##Scatter plot of returns and transaction costs
      ##Transaction costs
      plot(df2$tcost_perce[,1]*100, x = date_axes, xaxt = "n" , yaxt = "n" ,  
           ylim = c(-10,10), cex = 0.3, pch = 20,
           xlab =  "Dates", ylab = "percentage" ) 
      #Returns 
      points(y = df2$returns_perce[,1]*100, x = date_axes, col = "red", cex = 0.3, pch = 20)
      ##Legend
      legend("topright", legend = c("Costs", "Returns"), col = c("black", "red"), pch = 20)
      
      #x-axis (1)
      axis.Date(1, at = seq(date_axes[1], date_axes[length(date_axes)] , "6 months") , 
                labels = T, format = "%m/%Y", tcl = -0.5) #add an axes with dates 
      ##y-axis (2)
      axis(2, at = seq(-10,10,by = 1), labels = F )  
      axis(2, at = seq(-10,10,by = 2), labels = paste0(seq(-10,10,by = 2) , "%"), las = 1 )
      
      ##Line showing cumulated returns 
      ##Line chart
      plot(x = date_axes, y = cumu_sum_ret2, type = "l", xaxt = "n", yaxt = "n" ,  
           ylim = c(-50,50), ylab = "percentage", xlab = "Dates") #costs
      ##x-axis 
      axis.Date(1, at = seq(date_axes[1], date_axes[length(date_axes)] , "6 months") , 
                labels = T, format = "%m/%Y", tcl = -0.5) #add an axes with dates 
      ##Legend
      legend("topright", legend = c("Cumulative returns"), 
             col = c("black"), lwd = 2)
      
      ##y-axis (2) 
      axis(2, at =  seq(-50,50, by = 10), labels = paste0( seq(-50,50, by = 10) , "%") , 
           las = 1) #to write horizontally  
      
      ##Add a title  
      title( paste0(input$pillar, " ", " upper quartile portfolio" ), 
             outer = T, line = -18) #line = -18 -- > In the middle of the four charts
      ##End of if (comparison == T)
    } 
    ##3.10 Else (if input$quartile != comparison -- > two plots are needed instead of four)
    else  { 
      par(mfrow = c(1,2)) #two charts 
      df <- trade_analysis(quartile = input$quartile, pillar = input$pillar)
      
      ##Cumulated returns
      cumu_sum_ret <- numeric(length(df$returns_perce[,1])) 
      for(i in 1:length(df$returns_perce[,1])){   
        if(i == 1){ 
          cumu_sum_ret[i] <- df$returns_perce[,1][i]*100 
          ##End of if 
        } else { 
          cumu_sum_ret[i] <- cumu_sum_ret[i-1] + df$returns_perce[,1][i]*100
          ##End of else
        }
        ##End of loop
      }
      
      ##Date axis  
      date_axes <- as.Date( colnames(prices_day)[4:length(prices_day)], 
                            tryFormats = "%d/%m/%Y" ) #the x_axes need to be of a date format
      
      ##Scatter plot of rets and transaction costs (percentage)
      plot(df$tcost_perce[,1]*100, x = date_axes, xaxt = "n" , yaxt = "n" ,  
           ylim = c(-10,10), cex = 0.3, pch = 20, #costs scatter
           xlab =  "Dates", ylab = "percentage" ) 
      points(y = df$returns_perce[,1]*100, x = date_axes, col = "red", cex = 0.3, pch = 20) #returns 
      legend("topright", legend = c("Costs", "Returns"), col = c("black", "red"),pch = 20)
      ##Fix the axes
      axis.Date(1, at = seq(date_axes[1], date_axes[length(date_axes)] , "6 months") , 
                labels = T, format = "%m/%Y", tcl = -0.5) #add an axes with dates 
      axis(2, at = seq(-10,10,by = 1), labels = F )  
      axis(2, at = seq(-10,10,by = 2), labels = paste0(seq(-10,10,by = 2) , "%"), las = 1 )
      
      ##Line cahrt with cumulated elements 
      plot(x = date_axes, y = cumu_sum_ret, type = "l", xaxt = "n", yaxt = "n" ,  
           ylim = c(-50,50), ylab = "percentage", xlab = "Dates") #rets
      axis.Date(1, at = seq(date_axes[1], date_axes[length(date_axes)] , "6 months") , 
                labels = T, format = "%m/%Y", tcl = -0.5) #add an axes with dates 
      legend("topright", legend = c("Cumulative returns"), 
             col = c("black"), lwd = 2)
      axis(2, at = seq(-50,50, by = 10), labels = paste0( seq(-50,50, by = 10) , "%"), 
           las = 1 ) #to write horizaontally 
      
      ##Main
      title( paste0(input$pillar, input$quartile , " " ,"quartile portfolio") ,
             outer = T,line = -1)
      
      ##End of else
    }
    ##End of renderPlot()
  })
  ##End of server()  
}

##1.3: Run the application --------------------------------------------------------

shinyApp(ui = ui , server = server)


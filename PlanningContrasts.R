#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI 
ui <- fluidPage(theme = "bootstrap.min.css",
  
  # Application title
   titlePanel("Planning for precise contrast estimates"), 
             p(),
             span("Author: Gerben Mulder, October, 2018"),
             p(),
             span("This application plans sample sizes for precise contrast estimates in one factor designs."),  
             p(),
             span("For a tutorial on how to use the app: "), a("Tutorial", href="https://the-small-s-scientist.blogspot.com/2018/11/contrast-tutorial.html"),   
             p(), 
             p(),
    #add panel for PFP
  fluidRow(column(4,
                  wellPanel(p(), p(),
                            numericInput("tMOE", "Target MOE (fraction of sd; f):", value=0.40, min = .01, step=.01),
                            numericInput("assu", "Assurance:", value=.80, min = 0, max=.99, step=.01),
                            radioButtons("ncond", "Number of conditions:", choices=c(2, 3, 4), selected = 2, inline = TRUE,
                                         width = NULL),
                            selectInput("DesignType", "Type of design:", choices = c("Independent", "Dependent"), 
                                        selected="Indepdendent"),
                            conditionalPanel(
                              condition = "input.DesignType == 'Dependent'",
                              numericInput("cor", "Cross-condition correlation:", value=0, min=0, max=.99, step=.01)
                            ), 
                            conditionalPanel(
                              condition = "input.ncond > 2",
                              selectInput("contrastType", "Choose contrasts:", choices = c("Helmert contrasts", "Custom contrast"), 
                                          selected="Custom contrast")
                            )
                            
                         ) #end panel
  ), #end first column
  column(8, 
         conditionalPanel(condition = "input.ncond==2",
            column(4,               
           wellPanel(
             strong("Contrast Weights:"),
             p(),
             textInput("cw.1", value=1, label="Condition 1:"),
             textInput("cw.2", value=-1, label="Condition 2:")
           ) #end panel
            ) #end column
         ), #end conditionalPanel 1
         conditionalPanel(condition = "input.ncond==3 & input.contrastType=='Helmert contrasts'",
                          column(4, 
                            wellPanel(
                            strong("Weights Contrast 1:"),
                            p(),
                            textInput("cw3.1.1", value=1, label="Condition 1:"),
                            textInput("cw3.1.2", value="-1/2", label="Condition 2:"),
                            textInput("cw3.1.3", value="-1/2", label="Condition 3:")
                          ) #end panel
                          ), #end column
                          column(4, 
                          wellPanel(
                            strong("Weights Contrast 2:"),
                            p(),
                            textInput("cw3.2.1", value=0, label="Condition 1:"),
                            textInput("cw3.2.2", value=-1, label="Condition 2:"),
                            textInput("cw3.2.3", value=--1, label="Condition 3:")
                          ) #end panel
                          ) #end column
         ), #end conditionalPanel 2
         conditionalPanel(condition = "input.ncond==4 & input.contrastType=='Helmert contrasts'",
           column(3, wellPanel(strong("Weights Contrast 1:"),
                               p(),
                               textInput("cw.1.1", value="1", label="Condition 1:"),
                               textInput("cw.1.2", value="-1/3", label="Condition 2:"),
                               textInput("cw.1.3", value="-1/3", label="Condition 3:"), 
                               textInput("cw.1.4", value="-1/3", label="Condition 4:"))),
           column(3, wellPanel(strong("Weights Contrast 2:"),
                               p(),
                               textInput("cw.2.1", value=0, label="Condition 1:"),
                               textInput("cw.2.2", value=1, label="Condition 2:"),
                               textInput("cw.2.3", value="-1/2", label="Condition 3:"),
                               textInput("cw.2.4", value="-1/2", label="Condition 4:"))),
           column(3, wellPanel(strong("Weights Contrast 3:"),
                               p(),
                               textInput("cw.3.1", value=0, label="Condition 1:"),
                               textInput("cw.3.2", value=0, label="Condition 2:"),
                               textInput("cw.3.3", value=1, label="Condition 3:"),
                               textInput("cw.3.4", value=-1, label="Condition 4:")))
           
         ), #end conditionalPanel 3
         conditionalPanel(condition = "input.ncond==3 & input.contrastType=='Custom contrast'",
                          column(4, 
                                 wellPanel(
                                   strong("Specify Contrast:"),
                                   p(),
                                   textInput("cw3.1.1", value="0", label="Condition 1:"),
                                   textInput("cw3.1.2", value="1", label="Condition 2:"),
                                   textInput("cw3.1.3", value="-1", label="Condition 3:")
                                 ) #end panel
                          ) # end column
                          ), #end Conditional planel 4 
        conditionalPanel(condition = "input.ncond==4 & input.contrastType=='Custom contrast'",
                         column(4, 
                              wellPanel(
                              strong("Specify Contrast:"),
                              p(),
                             textInput("cw3.1.1", value= "0", label="Condition 1:"),
                             textInput("cw3.1.2", value="0", label="Condition 2:"),
                             textInput("cw3.1.3", value="1", label="Condition 3:"),
                             textInput("cw3.1.4", value="-1", label="Condition 4:")
                                                  ) #end panel
                                           ) # end column
        ) # end conditional Panel 4
   ) # end column
  ),  # end first row
  fluidRow(
    column(4, 
    wellPanel(
    actionButton("plan", "Get sample sizes")
      ) #end panel
    ) # end column
  ), #end second row 
  fluidRow(
 
    column(8, tabsetPanel(
      tabPanel("Planning Results", verbatimTextOutput("ans")),
      tabPanel("Expected Results Figure", plotOutput("expResults"))
                      ) # end tabsetPanel
    ) # end column
  ) #end third row
) #end fluidPage

#define planning functions (note to self: move to global in final version)

#Expected Margin of Error

calcMOE <- function(n, cor = 0, k = 2, w = c(-1, 1)) {
  if (cor !=0) {
    df = (k -1)*(n-1) 
  } else {
    df = k*(n - 1) 
  } 
  ct = qt(.975, df)
  moe = ct*sqrt(sum(w^2)*(1 - cor)/n)
}

transFraction <- function(x) eval(parse(text = x))


sampleSize <- function(tMoE, assu= .80, cor = 0, k = 2, w = c(-1, 1)) {
  
  #error checking: 
  if (tMoE == 0) stop("Target MoE (f) must be larger than 0.")
  
  if (assu < .50) stop("Assurance must be .50 or larger.") 
  if (assu >= 1) stop("Assurance must be smaller than 1.")
  
  if (abs(cor) >= 1) stop("Correlation must be between -1 and 1.") 
  

  qMoe = function(n, q = assu, cor = cor, k = k, w = w) {
    if (cor !=0) {
      df = (k -1)*(n-1) 
    } else {
      df = k*(n - 1) 
    } 
    ct = qt(.975, df)
    cchi = qchisq(q, df)
    ct*sqrt(sum(w^2)*(1 - cor)/n*(cchi/df)) 
  }  
  
  #squared loss function for optimization
  
  cost <- function(n, tMoE, assu = assu, cor = cor, k = k, w = w) {
    (qMoe(n, q = assu, cor = cor, k = k, w = w) - tMoE)^2
  }
  
  #find sample size using optimization
  
  ans = optimize(cost, c(1, 5000),  tMoE=tMoE, assu = assu, cor = cor, k = k, w = w)$minimum 
  n = ceiling(ans)
  
  return(c( "n: "=n, "f: "=tMoE, "assu: "=assu, "k: "=k, "cor: "=cor))
}  




# Define server logic 

server <- function(input, output) {
   
  
    observeEvent(input$plan, { #this happens when sample size planning button is pushed
    tMOE <- as.numeric(isolate(input$tMOE))
    assu <- as.numeric(isolate(input$assu))
    ncond <- as.numeric(isolate(input$ncond))
    cor <- as.numeric(isolate(input$cor))
    contrType <- isolate(input$contrastType)
    design <- isolate(input$DesignType)
    
    
    # insert error checking 
    if (tMOE == 0) tMOE = .05
    
    if (assu < .50) assu = .50 
    if (assu >= 1) assu = .95
    
    if (abs(cor) >= 1) cor = .99 
    
    flagCor <- ifelse(cor == 0 & design == "Dependent", TRUE, FALSE)
    if (design == "Independent") cor = 0
      
    
    if (ncond == 2) {
      weights = c(isolate(input$cw.1), isolate(input$cw.2))
      weights = sapply(weights, transFraction)
      planWeight = weights 
      
     flag = all.equal(sum(planWeight), 0)
     if (flag == TRUE )  flag = all.equal(sum(abs(planWeight)), 2)
    
    } else if (ncond==3 & contrType=="Helmert contrasts") {
      weights = c(isolate(input$cw3.1.1), isolate(input$cw3.1.2), isolate(input$cw3.1.3), 
                  isolate(input$cw3.2.1), isolate(input$cw3.2.2), isolate(input$cw3.2.3))
      weights = sapply(weights, transFraction)
      weights = matrix(weights, 2, 3, byrow = TRUE)
      
      #insert error checking for contrasts
      flag = all.equal(sum(weights), 0)
      if (flag == TRUE) flag  = all.equal(sum(abs(weights[1, ])), 2)
      if (flag == TRUE ) flag = all.equal(sum(abs(weights[2, ])), 2)
      
      
      sumSquaredWeights = apply(weights^2, 1, sum)
      sumSquaredWeights == max(sumSquaredWeights) -> check
      planWeight = weights[which(check==TRUE)[1],]
    } else if (ncond==4 & contrType=="Helmert contrasts") {
      weights = c(isolate(input$cw.1.1), isolate(input$cw.1.2), isolate(input$cw.1.3), isolate(input$cw.1.4), 
                  isolate(input$cw.2.1), isolate(input$cw.2.2), isolate(input$cw.2.3), isolate(input$cw.2.4), 
                  isolate(input$cw.3.1), isolate(input$cw.3.2), isolate(input$cw.3.3), isolate(input$cw.3.4))
      weights = sapply(weights, transFraction)
      weights = matrix(weights, 3, 4, byrow = TRUE)
      
      #insert error checking for contrasts 
      flag = all.equal(sum(weights), 0)
      if (flag == TRUE) flag = all.equal(sum(abs(weights[1, ])), 2)
      if (flag == TRUE) flag = all.equal(sum(abs(weights[2, ])), 2)
      if (flag == TRUE) flag = all.equal(sum(abs(weights[3, ])), 2)
      
      sumSquaredWeights = apply(weights^2, 1, sum)
      sumSquaredWeights == max(sumSquaredWeights) -> check
      planWeight = weights[which(check==TRUE)[1],]
    } else if  (ncond == 3 & contrType=="Custom contrast") {
      weights = c(isolate(input$cw3.1.1), isolate(input$cw3.1.2), isolate(input$cw3.1.3))
      weights = sapply(weights, transFraction)
      flag = all.equal(sum(weights), 0)
      if (flag == TRUE) flag = all.equal(sum(abs(weights)), 2)
      planWeight = weights
    } else if (ncond == 4 & contrType=="Custom contrast") {
      weights = c(isolate(input$cw.3.1), isolate(input$cw.3.2), isolate(input$cw.3.3), isolate(input$cw.3.4))
      weights = sapply(weights, transFraction)
      flag = all.equal(sum(weights), 0)
      if (flag == TRUE) flag = all.equal(sum(abs(weights)), 2)
      planWeight = weights
      
    }
    
    
    if (flag != TRUE) {
    error <- "An error occured; check that the contrasts weights sum to zero and that the absolute values of the weights sum to two"  
    output$ans <- renderPrint(error)
    output$expResults <- renderPlot(plot(c(1, 1), main="An error occured", 
                                         type="n", ylab="error", xlab="error"), height=400, width=600)
    } else if (flagCor == TRUE) {
    error <- "An error occured: for the dependent design the correlation must be larger than zero"
    output$ans <- renderPrint(error)
    output$expResults <- renderPlot(plot(c(1, 1), main="An error occured", 
                                         type="n", ylab="error", xlab="error"), height=400, width=600)
    } else {
    result <- sampleSize(tMOE, assu = assu, cor = cor, k = ncond, w = planWeight)
    n = unname(result[1])
    if (cor > 0)  {
      totalSampleSize = n
    }  else {
      totalSampleSize = ncond*n
    }
    
    if (ncond == 2) {
    eMOE = calcMOE(n, cor=cor, k = ncond, w = planWeight)
    } 
    if (ncond == 3 & contrType == "Helmert contrasts") {
      eMOE = c(calcMOE(n, cor=cor, k = ncond, w = weights[1,]), calcMOE(n, cor=cor, k = ncond, w = weights[2,]))
      eMOE = cbind(weights, eMOE)
      row.names(eMOE) = c("c1", "c2")
      colnames(eMOE) = c("w1", "w2", "w3", "Expected MOE")
       }
    if (ncond == 4 & contrType == "Helmert contrasts") {
      eMOE = c(calcMOE(n, cor=cor, k = ncond, w = weights[1,]), calcMOE(n, cor=cor, k = ncond, w = weights[2,]), 
               calcMOE(n, cor=cor, k = ncond, w = weights[3,]))
      eMOE = cbind(weights, eMOE)
      row.names(eMOE) = c("c1", "c2", "c3")
      colnames(eMOE) = c("w1", "w2", "w3", "w4", "Expected MOE")
    }
    if (ncond == 3 & contrType == "Custom contrast") {
      eMOE = calcMOE(n, cor=cor, k = ncond, w = planWeight)
    }
    if (ncond == 4 & contrType == "Custom contrast") {
      eMOE = calcMOE(n, cor=cor, k = ncond, w = planWeight)
    }
    
    output$ans <- renderPrint(list("Result:"=result, "Expected MoE"=eMOE, "Total Sample size:"=totalSampleSize))
    
    
    effectsize = c(0, .20, .50, .80)
    f = round(calcMOE(n, cor=cor, k = ncond, w = planWeight), 2)
    d = effectsize
    upper = d + f
    lower = d - f
  output$expResults <- renderPlot(
    
    {
      plot(effectsize, d, ylim=c(-1.5, 1.5), xaxt="n", ylab="Estimated effect size", 
         xlab="True effect size", main=paste("Expected MOE is f =", f))
    axis(1, at=c(0, .20, .50, .80), labels=c("Zero", "Small", "Medium", "Large"))
    arrows(effectsize, d, effectsize, upper, length=.10, angle=90)
    arrows(effectsize, d, effectsize, lower, length=.10, angle=90)
    abline(h=c(0, -.20, .20, -.50, .50, -.80, .80), lty=3)
    text(.4, .90, "Large", cex=.8)
    text(.4, .60, "Medium", cex=.8)
    text(.4, .30, "Small", cex=.8)
    text(.4, -.90, "Large", cex=.8)
    text(.4, -.60, "Medium", cex=.8)
    text(.4, -.30, "Small", cex=.8)
    
    }, height=400, width=600
  ) #end renderPlot
    } #end conditional statement on flag
      }) #end observeEvent
    
  }# end server
  


# Run the application 
shinyApp(ui = ui, server = server)
library(shiny)

# Define UI for the application
ui <- fluidPage(
  # CSS to make the page narrower
  tags$head(
    tags$style(HTML("
      .container-fluid {
        max-width: 1200px;
      }
    "))
  ),
  
  # Application title
  titlePanel("Sample size calculation for maximin MCT"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      width = 7,  # Make sidebar broader
      tabsetPanel(
        id = "tabs",
        tabPanel("Type of design",
                 radioButtons("choice", "Choose Option:", choices = c("No constraints", "Fixed total center size", "Fixed number of centers", "Fixed center size for A and B"))
        ),
        tabPanel("Parameters",
                 uiOutput("dynamicUI")
        )
      )
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      width = 5,  # Make main panel smaller
      tabsetPanel(
        tabPanel("Results",
                 strong(verbatimTextOutput("result1")),
                 strong(verbatimTextOutput("result2")),
                 strong(verbatimTextOutput("result3")),
                 strong(verbatimTextOutput("result4")),
                 strong(verbatimTextOutput("result5"))
        ),
       
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Dynamic UI for second tab based on choice
  output$dynamicUI <- renderUI({
    if (input$choice == "No constraints") {
      fluidRow(
        column( 6,
        sliderInput("psi", "Range for variance ratio of A versus B:", min = 0.1, max = 10, value = c(0.5, 2), step=0.05),   
        sliderInput("max.slopevar", "Maximum standardized slope variance:", min = 0.01, max = 1, value = 0.20),
        sliderInput("alfa", "Type I error rate:",min = 0.01, max = 1, value = 0.05),
        sliderInput("power", "Power:", min = 0.01, max = 1, value = 0.8),
        numericInput("ES", "Effect size:",min = 0.001, max = 1.000, value = 0.500, step = 0.001) ),
        
        column(6, 
        numericInput("c.C", "Center-specific costs:", value = 200),
        numericInput("c.sA", "Subject-specific costs in A:", value = 20),
        numericInput("c.sB", "Subject-specific costs in B:", value = 4),
        
        actionButton("Btn", "Calculate"),
      ) )
      
    } else if (input$choice == "Fixed total center size") {
      fluidRow(
        column( 6,
        sliderInput("psi", "Range for variance ratio of A versus B:", min = 0.1, max = 10, value = c(0.5, 2), step=0.05),   
        sliderInput("max.slopevar", "Maximum standardized slope variance:", min = 0.01, max = 1, value = 0.20),
        sliderInput("alfa", "Type I error rate:",min = 0.01, max = 1, value = 0.05),
        sliderInput("power", "Power:", min = 0.01, max = 1, value = 0.8),
        numericInput("ES", "Effect size:",min = 0.001, max = 1.000, value = 0.500, step = 0.001) ),
        
        column(6,
        numericInput("n.min", "Minimum center size:", value = 4),
	  numericInput("n.max", "Maximum center size:", value = 8),
               
        numericInput("c.C", "Center-specific costs:", value = 200),
        numericInput("c.sA", "Subject-specific costs in A:", value = 20),
        numericInput("c.sB", "Subject-specific costs in B:", value = 4),
        
        actionButton("Btn", "Calculate"),
      ) )
    }
      else if (input$choice == "Fixed number of centers") {
        fluidRow(
          column( 6,
                sliderInput("psi", "Range for variance ratio of A versus B:", min = 0.1, max = 10, value = c(0.5, 2), step=0.05),   
                sliderInput("max.slopevar", "Maximum standardized slope variance:", min = 0.01, max = 1, value = 0.20),
                sliderInput("alfa", "Type I error rate:",min = 0.01, max = 1, value = 0.05),
                sliderInput("power", "Power:", min = 0.01, max = 1, value = 0.8),
                numericInput("ES", "Effect size:",min = 0.001, max = 1.000, value = 0.500, step = 0.001) ),
          
          column(6,
          numericInput("K.min", "Minimum number of centers:", value = 12),
	    numericInput("K.max", "Maximum number of centers:", value = 24),
               
          numericInput("c.C", "Center-specific costs:", value = 200),
          numericInput("c.sA", "Subject-specific costs in A:", value = 20),
          numericInput("c.sB", "Subject-specific costs in B:", value = 4),
        
        actionButton("Btn", "Calculate"),
      ) ) }  

      else if (input$choice == "Fixed center size for A and B") {
        fluidRow(
          column( 6,
                sliderInput("psi", "Range for variance ratio of A versus B:", min = 0.1, max = 10, value = c(0.5, 2), step=0.05),   
                sliderInput("max.slopevar", "Maximum standardized slope variance:", min = 0.01, max = 1, value = 0.20),
                sliderInput("alfa", "Type I error rate:",min = 0.01, max = 1, value = 0.05),
                sliderInput("power", "Power:", min = 0.01, max = 1, value = 0.8),
                numericInput("ES", "Effect size:",min = 0.001, max = 1.000, value = 0.500, step = 0.001) ),
          
          column(6,
          numericInput("n.a", "Center size for A:", value = 4),
	    numericInput("n.b", "Center size for B:", value = 6),
               
          numericInput("c.C", "Center-specific costs:", value = 200),
          numericInput("c.sA", "Subject-specific costs in A:", value = 20),
          numericInput("c.sB", "Subject-specific costs in B:", value = 4),
        
        actionButton("Btn", "Calculate"),
      ) ) }  

  })
 

  # Perform calculations based on user input and choice
  reactiveResult1 <- eventReactive(input$Btn, {
  if (input$choice == "No constraints") {
    
    psimin = input$psi[1]
    psimax = input$psi[2] 
    var_d = input$max.slopevar
  
    alfa = input$alfa
    power = input$power
    
    Zalfa <- qnorm(1-(alfa/2),0,1)
    Zbeta <- qnorm(power,0,1)
    d = input$ES
   
    c = input$c.C
    sa = input$c.sA
    sb = input$c.sB

 
    if ( (sa/sb) > psimax) {psi = psimax
       } else {
       if ( (sa/sb) < psimin) {psi = psimin
          } else { 
          psi = (sa/sb) } } 

    nammd = sqrt(c/sa)*sqrt(2/var_d*psi/(1+psi))
    nbmmd = sqrt(c/sb)*sqrt(2/var_d*1/(1+psi))
    nammd = round(nammd)
    nbmmd = round(nbmmd)
    
    if (nammd == 0 & nbmmd == 0) {
   
    list(Ka = "NA", Kb = "NA", na = "0, set to 1 and run fixed center sizes in A and B", nb = "0, set to 1 and run fixed cluster sizes in A and B", budget = "NA", power = "NA")
    
    } else {

    if (nammd == 0)  {
 
    list(Ka = "NA", Kb = "NA", na = "0, set to 1 and run fixed center sizes in A and B", nb = nbmmd, budget = "NA", power = "NA")
    
    } else {

    if (nbmmd == 0)  {

    list(Ka = "NA", Kb = "NA", na = nammd , nb = "0, set to 1 and run fixed center sizes in A and B", budget = "NA", power = "NA")
  
    } else {
     
     # determine worst case value for psi after rounding of na and nb
  
    if ( 1 >= nammd/nbmmd) {psi = psimax
       } else {
       psi = psimin}

    Kmmd = (Zbeta + Zalfa)^2/d^2*(var_d + 2/nammd*psi/(1+psi) + 2/nbmmd*1/(1+psi))
    eps = 0.0000001
    Kmmd = ceiling(Kmmd-eps)

    # budget of (unrestricted) maximin design 
    Bmmd = Kmmd*(c+nammd*sa+nbmmd*sb)

    # power of (unrestricted) maximin design
    Zpower_num = d*sqrt(Kmmd)
    Zpower_denom = var_d + 2/nammd*psi/(psi+1) + 2/nbmmd/(psi+1) 
    Zpower = Zpower_num/sqrt(Zpower_denom) - Zalfa
    power = pnorm(Zpower,0,1)

    list(K = Kmmd, na = nammd, nb = nbmmd, budget = Bmmd, power = power)
    
     } } }    

       } else if (input$choice == "Fixed total center size") {

       psimin = input$psi[1]
       psimax = input$psi[2] 
       var_d = input$max.slopevar
  
       alfa = input$alfa
       power = input$power
    
       Zalfa <- qnorm(1-(alfa/2),0,1)
       Zbeta <- qnorm(power,0,1)
       d = input$ES
   
       nlb = input$n.min
       nub = input$n.max

       c = input$c.C
       sa = input$c.sA
       sb = input$c.sB   
          
         
       n_set = seq(nlb, nub, by = 1)

       for (n in n_set) { 

          K = rep(-99, times = n-1)
          b = rep(-99, times = n-1)
          power = rep(-99, times = n-1)
          na = rep(-99, times = n-1)
          nb = rep(-99, times = n-1)
          psi = rep(-99, times = n-1)

          index = seq(1,n-1, by =1)
  
      for (i in index) { 
          na[i] = i
          nb[i] = n-i

          if ( 1  >= na[i]/nb[i])  {psi[i] = psimax}
          else {psi[i] = psimin}

          eps = 0.0000001
          Khulp = ((Zalfa + Zbeta)/d)^2*(var_d  +  2/na[i]*psi[i]/(psi[i]+1) + 2/nb[i]*1/(psi[i]+1))
          K[i] = ceiling(Khulp-eps)

          # calculate power for K rounded up to the nearest integer

          Zpower_num = d*sqrt(K[i])
          Zpower_denom = var_d + 2/na[i]*psi[i]/(psi[i]+1) + 2/nb[i]/(psi[i]+1) 
          Zpower = Zpower_num/sqrt(Zpower_denom) - Zalfa
          power[i] = pnorm(Zpower,0,1)

          # Calculate budget
          b[i]= K[i]*(c + na[i]*sa + nb[i]*sb) }

     # Save the required budgets for the worst-case for each design for a fixed n
     result <- data.frame(K,na,nb,b,power)
     minbudget <- min(result$b)
     result <- result[result$b <= minbudget, ]
     maxpower <- max(result$power)
     result <- result[result$power >= maxpower, ]

     if (n == nlb) {
        budget <- minbudget   
        mmd <- result
        } else if (minbudget < budget) {
        budget <- minbudget
        mmd <- result }  
    }
        
         ## give resulting maximin design
         list(K= mmd$K, na= mmd$na, nb = mmd$nb, budget = mmd$b, power = mmd$power)      
       }
      
    else if (input$choice == "Fixed number of centers") { 
      
       psimin = input$psi[1]
       psimax = input$psi[2] 
       var_d = input$max.slopevar
  
       alfa = input$alfa
       power = input$power
    
       Zalfa <- qnorm(1-(alfa/2),0,1)
       Zbeta <- qnorm(power,0,1)
       d = input$ES
   
       Klb = input$K.min
       Kub = input$K.max

       c = input$c.C
       sa = input$c.sA
       sb = input$c.sB   

       K_set = seq(Klb, Kub, by = 1)
       nrK = length(K_set)

       b = rep(-99, times = nrK)
       na = rep(-99, times = nrK)
       nb = rep(-99, times = nrK)
       power = rep(-99, times = nrK)

       K_index = 1

       for (K in K_set) { 

       ## first check whether K is large enough to realize a required power level

       if (max(K_set) < (Zbeta + Zalfa)^2/d^2*var_d) { 
       
          list(K = "maxmimum K is too small, increase K", na = "NA", nb = "NA", budget = "NA", power = "NA")
         
           } else if (K >= (Zbeta + Zalfa)^2/d^2*var_d) {

           ### determine worst-case value for psi

           if ( (sa/sb) > psimax) {psi = psimax
              } else if ( (sa/sb) < psimin) {psi = psimin
              } else { 
              psi = (sa/sb) }  

           ### determine the required budget for required power 

           b_num = 2*(Zbeta + Zalfa)^2*K*(sqrt(psi*sa) +sqrt(sb))^2
           b_den = d^2*K*(psi+1) - (Zbeta + Zalfa)^2*var_d*(psi+1)
           b = b_num/b_den + K*c

           na.1 = (b-K*c)*sqrt(psi)/(K*sqrt(sa)*(sqrt(sa*psi) + sqrt(sb)) )                         
           nb.1 = (b-K*c)*1/(K*sqrt(sb)*(sqrt(sa*psi) + sqrt(sb)) ) 

           na[K_index] = ceiling(na.1)
           nb[K_index] = ceiling(nb.1)

           # determine worst case value for psi after rounding of na and nb
  
           if ( 1 >= na[K_index]/nb[K_index]) {psi = psimax
              } else {
              psi = psimin}

           # calculate the realized power  
           Zpower_num = d*sqrt(K)
           Zpower_denom = var_d + 2/na[K_index]*psi/(psi+1) + 2/nb[K_index]/(psi+1) 
           Zpower = Zpower_num/sqrt(Zpower_denom) - Zalfa
           power[K_index] = pnorm(Zpower,0,1)

           b[K_index] = K*(c+na[K_index]*sa+nb[K_index]*sb)

           K_index = K_index +1
              } }

      if (max(K_set) >= (Zbeta + Zalfa)^2/d^2*var_d) {

      result <- data.frame(K, na, nb, b, power)
      result <- result[result$power > -99, ]
      minbudget <- min(result$b)
      result <- result[result$b <= minbudget, ]
      maxpower <- max(result$power)
      mmd <- result[result$power >= maxpower, ]

      list(K= mmd$K, na= mmd$na, nb = mmd$nb, budget = mmd$b, power = mmd$power) 
    
      } else {
    
      list(K = "K is too small, increase K", na = "NA", nb = "NA", budget = "NA", power = "NA") }
                  
      } 
      
     else if (input$choice == "Fixed center size for A and B") {

          psimin = input$psi[1]
          psimax = input$psi[2] 
          var_d = input$max.slopevar
  
          alfa = input$alfa
          power = input$power
    
          Zalfa <- qnorm(1-(alfa/2),0,1)
          Zbeta <- qnorm(power,0,1)
          d = input$ES
   
          na = input$n.a
          nb = input$n.b

          c = input$c.C
          sa = input$c.sA
          sb = input$c.sB 

          if ( 1  >= na/nb)  {psi = psimax}
             else {psi = psimin}

          eps = 0.0000001
          Khulp = ((Zalfa + Zbeta)/d)^2*(var_d  +  2/na*psi/(psi+1) + 2/nb*1/(psi+1))
          K = ceiling(Khulp-eps)

          # calcuate power for K rounded up to the nearest integer

          Zpower_num = d*sqrt(K)
          Zpower_denom = var_d + 2/na*psi/(psi+1) + 2/nb/(psi+1) 
          Zpower = Zpower_num/sqrt(Zpower_denom) - Zalfa
          power = pnorm(Zpower,0,1)

          # Calculate budget
          b= K*(c + na*sa + nb*sb)

          list(K= K, na= na, nb = nb, budget = b, power = power) 
                
            }
     
      
    } )
 
  

  # Reactive value to track button click
  btn_click <- reactiveVal(FALSE)
  
  # Render results in Summary tab
  output$result1 <- renderText({
    paste("Number of centers:", reactiveResult1()$K)
    })
  
  output$result2 <- renderText({
    paste("Center size in A:", reactiveResult1()$na)
  })
  
  output$result3 <- renderText({
    paste("Center size in B:", reactiveResult1()$nb)
  })
  
  output$result4 <- renderText({
    paste("Budget:", reactiveResult1()$budget)
  })
    
  output$result5 <- renderText({
      paste("Power:", reactiveResult1()$power)   
  })
 
}

# Run the application
shinyApp(ui = ui, server = server)

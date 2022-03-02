#--------------------------------------
# Carl Schmertmann
# 28 Feb 2019
#
# interactive display of ASFR schedule
# in the quadratic spline model [Schmertmann 2003]
#
# C Schmertmann (2003)
# A system of model fertility schedules with 
# graphically intuitive parameters. 
# Demographic Research 9/5:81-110. 10 Oct 2003.
#--------------------------------------

library(shiny)
library(tidyverse)

QS = function( x, R, alpha, P, H ) {
  
  w = min(.75, .25+.025*(P-alpha))
  beta = max (  min(50, 4*H-3*P) , (4*H-P)/3 )
  
  D = P-20          # delay index
  C = (P+50)/2 - H  # control index
  
  knot    = vector("numeric",5)
  knot[1] = alpha
  knot[2] = alpha + w*(P-alpha)
  knot[3] = P
  knot[4] = (P+H)/2
  knot[5] = (H+beta)/2
  
  A      = matrix(NA,5,5)
  target = vector("numeric",5)
  
  # target value at P=1
  A[1,]     = (pmax(P-knot, 0))^2 
  target[1] = 1
  
  # target value at H=1/2
  A[2,]     = (pmax(H-knot, 0))^2
  target[2] = 1/2
  
  # target value at beta=0
  A[3,]     = (pmax(beta-knot, 0))^2
  target[3] = 0
  
  # target slope at P=0
  A[4,]     = 2*pmax(P-knot, 0)  
  target[4] = 0
  
  # target slope at beta=0
  A[5,]     = 2*pmax(beta-knot, 0)  
  target[5] = 0
  
  # calculate thetas
  theta = solve(A,target)
  
  tmp = (pmax( outer(x,knot,"-") , 0))^2
  
  y = (x >= alpha) * (x <= beta) * R * (tmp %*% theta)  # ASFRs
  
  return(list(R=R,alpha=alpha,P=P,H=H,
              theta=theta,knot=knot,w=w,beta=beta,delay=D,control=C,
              x=as.vector(x) ,y=as.vector(y) ))
  
} #QS


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Schmertmann 2003 Quadratic Spline Fertility Model"),
   
   
   sidebarLayout(
      sidebarPanel(
         sliderInput(inputId='R_slider',
                     label = "R (max fertility rate)",
                     min = .05,
                     max = .25,
                     step = .01,
                     value = .15),

         sliderInput(inputId='a_slider',
                     label = "alpha (age at menarche)",
                     min = 12,
                     max = 18,
                     step = 0.5,
                     value = 14),
         
         sliderInput(inputId='P_slider',
                     label = "P (age of peak fertility)",
                     min = 18,
                     max = 35,
                     step = 1,
                     value = 28),
         
         sliderInput(inputId='H_slider',
                     label = "H (age at which f(x)= f(P)/2)",
                     min = 25,
                     max = 45,
                     step = 1,
                     value = 35)
         

      ),  #sidebarPanel

      # Show a plot of log mortality
      mainPanel(
         plotOutput(outputId="asfr_plot")
      )
 
   ) # sidebarLayout
) # fluid

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  
  output$asfr_plot <- renderPlot({

    R = input$R_slider
    a = input$a_slider
    P = input$P_slider
    H = input$H_slider
    
    legend_text = c( 
                   paste0('baseline\n',
                          '(',sprintf("%2.2f", .15),
                          ',',sprintf("%3.1f",  14),
                          ',',sprintf("%2.0f",  28),
                          ',',sprintf("%2.0f",  35),
                          ')'),
                   paste0('selected\n',
                          '(',sprintf("%2.2f", R),
                          ',',sprintf("%3.1f", a),
                          ',',sprintf("%2.0f", P),
                          ',',sprintf("%2.0f", H),
                          ')')
                    ) 
                   
    
    # calculate ASFRs
    
    age = seq(from=10.25, to=49.75, by=.50)
    
    this_frate = QS(age, R, a, P, H)$y
    base_frate = QS(age, .15, 14, 28, 35)$y
    
    this_TFR = sum(this_frate)/2
    this_title = paste0('Age-Specific Fertility Rates\n',
                        'Quadratic Spline Model with (R,a,P,H) = ',
                        paste0('(',sprintf("%2.2f", R),
                               ', ',sprintf("%3.1f", a),
                               ', ',sprintf("%2.0f", P),
                               ', ',sprintf("%2.0f", H),
                               ')'),
                        '; TFR = ',sprintf("%4.2f", this_TFR) )
    
    df = data.frame( age= rep(age,2),
                         sched = rep(legend_text,rep(length(age),2)),
                        frate = c(base_frate, this_frate))
        
       G =  ggplot(data=df, aes(x=age, y=frate, 
                            color = sched, 
                            alpha = sched)) +
          geom_point(size=4) +
          geom_line(lwd=2) +
          scale_alpha_manual(guide=FALSE,
                             values=c(1.0,0.7)) +
          scale_color_manual(values=c(grey(.70),'seagreen')) +
          scale_x_continuous(limits=c(10,50),
                             breaks=seq(10,50,5)) +
          scale_y_continuous(limits=c(0,.25)) +
          labs(title=this_title) +
          theme_bw() +
          theme(legend.text = element_text(family='mono')) +
          theme(panel.grid.major = element_line(colour = "darkgrey"))
        

    print(G)
    
   }) # renderPlot
   
} # server

# Run the application 
shinyApp(ui = ui, server = server)


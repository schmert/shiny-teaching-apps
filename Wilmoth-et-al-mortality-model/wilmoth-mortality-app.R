#--------------------------------------
# Carl Schmertmann
# 27 Feb 2019
#
# interactive display of 
#     log mortality rates
#     survival probabilities (lx)
#     death fractions (dx)
# for any given (q5,k) pair in the
# Wilmoth et al. (2012) mortality model
#
# J Wilmoth, S Zureick, V Canudas-Romo, M Inoue, & C Sawyer (2012)
# A flexible two-dimensional mortality model for use in 
# indirect estimation. Population Studies, 66(1):1-28
#--------------------------------------

library(shiny)
library(tidyverse)

calc_logm = function(q5,k,sex) {
  
  wilmoth_constants = 
    read.csv(text = '
             age,am,bm,cm,vm,af,bf,cf,vf
             0,  -0.5101, 0.8164,-0.0245,     0,-0.6619, 0.7684,-0.0277,     0
             1,      -99,    -99,    -99,   -99,    -99,    -99,    -99,   -99
             5,  -3.0435, 1.5270, 0.0817,0.1720,-2.5608, 1.7937, 0.1082,0.2788
             10, -3.9554, 1.2390, 0.0638,0.1683,-3.2435, 1.6653, 0.1088,0.3423
             15, -3.9374, 1.0425, 0.0750,0.2161,-3.1099, 1.5797, 0.1147,0.4007
             20, -3.4165, 1.1651, 0.0945,0.3022,-2.9789, 1.5053, 0.1011,0.4133
             25, -3.4237, 1.1444, 0.0905,0.3624,-3.0185, 1.3729, 0.0815,0.3884
             30, -3.4438, 1.0682, 0.0814,0.3848,-3.0201, 1.2879, 0.0778,0.3391
             35, -3.4198, 0.9620, 0.0714,0.3779,-3.1487, 1.1071, 0.0637,0.2829
             40, -3.3829, 0.8337, 0.0609,0.3530,-3.2690, 0.9339, 0.0533,0.2246
             45, -3.4456, 0.6039, 0.0362,0.3060,-3.5202, 0.6642, 0.0289,0.1774
             50, -3.4217, 0.4001, 0.0138,0.2564,-3.4076, 0.5556, 0.0208,0.1429
             55, -3.4144, 0.1760,-0.0128,0.2017,-3.2587, 0.4461, 0.0101,0.1190
             60, -3.1402, 0.0921,-0.0216,0.1616,-2.8907, 0.3988, 0.0042,0.0807
             65, -2.8565, 0.0217,-0.0283,0.1216,-2.6608, 0.2591,-0.0135,0.0571
             70, -2.4114, 0.0388,-0.0235,0.0864,-2.2949, 0.1759,-0.0229,0.0295
             75, -2.0411, 0.0093,-0.0252,0.0537,-2.0414, 0.0481,-0.0354,0.0114
             80, -1.6456, 0.0085,-0.0221,0.0316,-1.7308,-0.0064,-0.0347,0.0033
             85, -1.3203,-0.0183,-0.0219,0.0061,-1.4473,-0.0531,-0.0327,0.0040
             90, -1.0368,-0.0314,-0.0184,     0,-1.1582,-0.0617,-0.0259,     0
             95, -0.7310,-0.0170,-0.0133,     0,-0.8655,-0.0598,-0.0198,     0
             100,-0.5024,-0.0081,-0.0086,     0,-0.6294,-0.0513,-0.0134,     0
             105,-0.3275,-0.0001,-0.0048,     0,-0.4282,-0.0341,-0.0075,     0
             110,-0.2212,-0.0028,-0.0027,     0,-0.2966,-0.0229,-0.0041,     0')
  
    colz = paste0(c('a','b','c','v'),
                  ifelse( sex=='Female','f','m'))
    
    h     = log(q5/1000)    # note: input q5 is in PER 1000
    theta = c(1,h,h^2,k)
    
    logm = as.matrix(wilmoth_constants[,colz]) %*% theta
    m0_1 = exp(logm[1])
    
    # fix [1,5) rate to match q5
    m1_5 = 1/4 * (- log(1-q5/1000) - m0_1)
    logm[2] = log( m1_5)

    return(list(age=c(0,1,seq(5,110,5)),logm= logm))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Wilmoth et al. 2012 Mortality Model"),
   
   
  
   sidebarLayout(
      sidebarPanel(
         sliderInput(inputId='q5_slider',
                     label = "q5 per 1000",
                     min = 5,
                     max = 400,
                     step = 5,
                     value = 50),

      sliderInput(inputId='k_slider',
                      label = "k",
                      min = -2,
                      max =  2,
                      step = 0.1,
                      value = 0),
      
      
      selectInput(inputId="plot_select", 
                  label="Select data to plot",
                  choices = c('logmu','lx','dx'),
                  selected='logmu'),
      
      selectInput(inputId="sex_select", 
                  label="Sex",
                  choices = c('Female','Male'),
                  selected='Female')
      
      ),  #sidebarPanel

      # Show a plot of log mortality
      mainPanel(
         plotOutput(outputId="logm_plot")
      )
 
   ) # sidebarLayout
) # fluid

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  
  output$logm_plot <- renderPlot({

    q5   = input$q5_slider
    k    = input$k_slider
    sex  = input$sex_select
    plot = input$plot_select
    
    legend_text = c( 
                   paste0('baseline\n(q5=',
                           sprintf("%3.0f", 50),
                          ', k=',
                          sprintf("%+2.1f", 0),')'),
                   paste0('selected\n(q5=',
                          sprintf("%3.0f", q5),
                          ', k=',
                          sprintf("%+2.1f", k),')')
                    ) 
                   
    
    # calculate log mortality and baseline
    lambda   = calc_logm(q5,k,sex)
    baseline = calc_logm(50,0,sex)
    
    n       = c(1,4,rep(5,22))
    base_lx = exp( - cumsum( c( 0, head( n * exp(baseline$logm),-1))  ))
    this_lx = exp( - cumsum( c( 0, head( n * exp(lambda$logm  ),-1))  ))
    
    age_dx  = baseline$age + n/2
    base_dx = c( -diff(base_lx), tail(base_lx,1))
    
    this_dx = c( -diff(this_lx), tail(this_lx,1))
    
    if (plot == 'logmu') {
    
      df = data.frame( age= rep(baseline$age,2),
                         sched = rep(legend_text,c(24,24)),
                         logm = c(baseline$logm,lambda$logm))
        
       G =  ggplot(data=df, aes(x=age, y=logm, 
                            color = sched, 
                            alpha = sched)) +
          geom_point(size=4) +
          geom_line(lwd=2) +
          scale_alpha_manual(guide=FALSE,
                             values=c(1.0,0.7)) +
          scale_color_manual(values=c('grey','red')) +
          scale_x_continuous(limits=c(0,110),
                             breaks=seq(0,110,10)) +
          scale_y_continuous(limits=c(-9,0)) +
          labs(title=paste(sex,'Log Mortality Rates by Age')) +
          theme_bw() +
          theme(legend.text = element_text(family='mono')) +
          theme(panel.grid.major = element_line(colour = "darkgrey"))
        
    } else if (plot == 'lx') {
      
      df = data.frame( age= rep(baseline$age,2),
                       sched = rep(legend_text,c(24,24)),
                       lx = c(base_lx, this_lx))
      
      G =  ggplot(data=df, aes(x=age, y=lx, 
                          color = sched, 
                          alpha = sched)) +
        geom_point(size=4) +
        geom_line(lwd=2) +
        scale_alpha_manual(guide=FALSE,
                           values=c(1.0,0.7)) +
        scale_color_manual(values=c('grey','limegreen')) +
        scale_x_continuous(limits=c(0,110),
                           breaks=seq(0,110,10)) +
        scale_y_continuous(limits=c(0,1)) +
        labs(title=paste(sex,'Survival Prob by Age')) +
        theme_bw() +
        theme(legend.text = element_text(family='mono')) +
        theme(panel.grid.major = element_line(colour = "darkgrey"))
      
            
    } else if (plot == 'dx') {  
      
      df = data.frame( age= age_dx,
                       sched = rep(legend_text,c(24,24)),
                       dx = c(base_dx, this_dx))
      
      G=  ggplot(data=df, aes(x=age, y=dx, 
                          color = sched, 
                          alpha = sched)) +
        geom_point(size=4) +
        geom_line(lwd=2) +
        scale_alpha_manual(guide=FALSE,
                           values=c(1.0,0.7)) +
        scale_color_manual(values=c('grey','purple')) +
        scale_x_continuous(limits=c(0,110),
                           breaks=seq(0,110,10)) +
        scale_y_continuous(limits=c(0,.25)) +
        labs(title=paste(sex,'Deaths by Age Group')) +
        theme_bw() +
        theme(legend.text = element_text(family='mono')) +
        theme(panel.grid.major = element_line(colour = "darkgrey"))
      
      
      }  # dx 
    
    print(G)
    
   }) # renderPlot
   
} # server

# Run the application 
shinyApp(ui = ui, server = server)


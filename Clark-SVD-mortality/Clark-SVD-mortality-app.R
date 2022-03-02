#--------------------------------------
# Carl Schmertmann
# 02 Mar 2019
#
# interactive display of 
#     log mortality rates
#     survival probabilities (lx)
#     death fractions (dx)
# for any given (5q0, 15q45) in the
# Clark SVD mortality model
#
#--------------------------------------

library(shiny)
library(tidyverse)
#library(devtools)

# install Sam Clark's svdComp5q0 package
#devtools::install_github(repo = "sinafala/svdComp5q0")
library(svdComp5q0)

calc_logm = function(am,sex) {
    qx   = predictLT(sex,am,out5=FALSE)[[1]] 
    logm = log( -log(1-qx) )
    return(list(age=0:109,logm= logm))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Clark SVD 5q0 Mortality Model"),
   
   
  
   sidebarLayout(
      sidebarPanel(
         sliderInput(inputId='q5_slider',
                     label = "q5 per 1000",
                     min = 5,
                     max = 300,
                     step = 5,
                     value = 50),


  
      selectInput(inputId="plot_select", 
                  label="Select data to plot",
                  choices = c('logmu','lx','dx'),
                  selected='logmu'),
      
      selectInput(inputId="sex_select", 
                  label="Sex",
                  choices = c('female','male'),
                  selected='female')
      
      ),  #sidebarPanel

      # Show a plot of log mortality
      mainPanel(
         plotOutput(outputId="main_plot")
      )
 
   ) # sidebarLayout
) # fluid

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  
  output$main_plot <- renderPlot({

    q5   = input$q5_slider
    sex  = input$sex_select
    plot = input$plot_select
    
    legend_text = c( 
                   paste0('baseline\n(q5=',
                           sprintf("%3.0f", 50),
                          ')'),
                   paste0('selected\n(q5=',
                          sprintf("%3.0f", q5),
                          ')')
                    ) 
                   
    
    # calculate log mortality and baseline
    lambda   = calc_logm(q5/1000,sex)
    baseline = calc_logm(50/1000,sex)
    
    n       = rep(1,110)
    base_lx = exp( - cumsum( c( 0, head( n * exp(baseline$logm),-1))  ))
    this_lx = exp( - cumsum( c( 0, head( n * exp(lambda$logm  ),-1))  ))
    
    this_e0 = sprintf("%3.1f", sum(this_lx))
    
    age_dx  = baseline$age + n/2
    base_dx = c( -diff(base_lx), tail(base_lx,1))
    
    this_dx = c( -diff(this_lx), tail(this_lx,1))
    
    if (plot == 'logmu') {
    
      df = data.frame( age= rep(baseline$age,2),
                         sched = rep(legend_text,c(110,110)),
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
          scale_y_continuous(limits=c(-10,0)) +
          labs(title=paste(toupper(sex),'Log Mortality Rates by Age',
                           'e0=',this_e0)) +
          theme_bw() +
          theme(legend.text = element_text(family='mono')) +
          theme(panel.grid.major = element_line(colour = "darkgrey"))
        
    } else if (plot == 'lx') {
      
      df = data.frame( age= rep(baseline$age,2),
                       sched = rep(legend_text,c(110,110)),
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
        labs(title=paste(toupper(sex),'Survival Prob by Age',
                         'e0=',this_e0)) +
        theme_bw() +
        theme(legend.text = element_text(family='mono')) +
        theme(panel.grid.major = element_line(colour = "darkgrey"))
      
            
    } else if (plot == 'dx') {  
      
      df = data.frame( age= age_dx,
                       sched = rep(legend_text,c(110,110)),
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
        scale_y_continuous(limits=c(0,.20)) +
        labs(title=paste(toupper(sex),'Deaths by Age Group',
                         'e0=',this_e0)) +
        theme_bw() +
        theme(legend.text = element_text(family='mono')) +
        theme(panel.grid.major = element_line(colour = "darkgrey"))
      
      
      }  # dx 
    
    print(G)
    
   }) # renderPlot
   
} # server

# Run the application 
shinyApp(ui = ui, server = server)


#--------------------------------------
# Carl Schmertmann
# created 27 Feb 2019
# edited  02 Mar 2022
#
# interactive display of 
#     log mortality rates
#     survival probabilities (lx)
#     death fractions (dx)
#     logits of survival probs (logit(lx))
# for any given (alpha,beta) pair in the
# Brass relational logit model
#
# The user can select one of three standard
# schedules from the HMD (all for both sexes combined): 
#    Sweden   1880 
#    Portugal 1950 
#    Japan    2000 
#--------------------------------------

library(shiny)
library(tidyverse)

df = structure(
       list(age = 0:110, 
        SWE1880 = c(      1, 0.88099, 0.84667, 0.82475, 0.80868, 
                    0.79612, 0.78653, 0.77875, 0.77221, 0.76657, 
                    0.76154, 0.75678, 0.75263, 0.74942, 0.74629, 
                    0.74329, 0.74017, 0.73693, 0.73341, 0.73005, 
                    0.72610, 0.72163, 0.71752, 0.71321, 0.70875, 
                    0.70404, 0.69973, 0.69487, 0.69026, 0.68547, 
                    0.68098, 0.67632, 0.67177, 0.66643, 0.66166, 
                    0.65641, 0.65126, 0.64607, 0.64125, 0.63589, 
                    0.63054, 0.62450, 0.61943, 0.61360, 0.60797,
                    0.60227, 0.59588, 0.58945, 0.58342, 0.57707, 
                    0.56988, 0.56241, 0.55486, 0.54706, 0.53908, 
                    0.53089, 0.52279, 0.51305, 0.50299, 0.49291, 
                    0.48195, 0.47125, 0.45898, 0.44577, 0.43210, 
                    0.41857, 0.40406, 0.38866, 0.37248, 0.35534, 
                    0.33685, 0.31747, 0.29794, 0.27803, 0.25774, 
                    0.23861, 0.21815, 0.19717, 0.17756, 0.15658, 
                    0.13644, 0.11876, 0.10101, 0.08497, 0.07101, 
                    0.05802, 0.04616, 0.03609, 0.02733, 0.02094, 
                    0.01550, 0.01121, 0.00792, 0.00546, 0.00367, 
                    0.00240, 0.00153, 0.00095, 0.00057, 0.00034, 
                    0.00019, 0.00011, 6e-05  , 3e-05,     2e-05, 
                    1e-05,   0,       0,       0,         0,   0   ), 
        PRT1950 = c(1      , 0.90210, 0.87546, 0.86422, 0.85949, 
                    0.85595, 0.85356, 0.85177,  0.8502, 0.84881, 
                    0.84753, 0.84641, 0.84532, 0.84427, 0.84329, 
                    0.84217, 0.84076, 0.83916, 0.83739, 0.83518, 
                    0.83288, 0.83027, 0.82768, 0.82473, 0.82176, 
                    0.81854, 0.81548, 0.81255, 0.80949, 0.80643, 
                    0.80335, 0.80015, 0.79711, 0.79433, 0.79113, 
                    0.78835, 0.78529, 0.78174, 0.77843, 0.77484, 
                    0.77143, 0.76708, 0.76355, 0.75904, 0.75462, 
                    0.75028, 0.74533, 0.74059, 0.73557, 0.73016, 
                    0.72462, 0.71816, 0.71224, 0.70578, 0.69863, 
                    0.69121, 0.68366, 0.67522, 0.66624, 0.65619, 
                    0.64772, 0.63546, 0.62408, 0.61115, 0.59709, 
                    0.58303, 0.56753, 0.55178, 0.53461, 0.51444, 
                    0.49878, 0.47306, 0.45142, 0.42516, 0.39789, 
                    0.37401, 0.34536, 0.31909, 0.29387, 0.26384, 
                    0.24179, 0.21147, 0.19145, 0.16697, 0.14538, 
                    0.12302, 0.10196, 0.08335, 0.06855, 0.05371, 
                    0.04347, 0.03208, 0.02495, 0.01910, 0.01439, 
                    0.01066, 0.00776, 0.00555, 0.00390, 0.00269, 
                    0.00182, 0.00120, 0.00078, 5e-04  , 0.00031, 
                    0.00019, 0.00011, 7e-05, 4e-05, 2e-05, 1e-05), 
        JPN2000 = c(1,       0.99672, 0.99621, 0.99592, 0.99570, 
                    0.99550, 0.99536, 0.99522, 0.99509, 0.99499, 
                    0.99489, 0.99480, 0.99471, 0.99461, 0.99448, 
                    0.99433, 0.99415, 0.99387, 0.99356, 0.99317, 
                    0.99274, 0.99229, 0.99186, 0.99137, 0.99088, 
                    0.99036, 0.98989, 0.98940, 0.98892, 0.98841, 
                    0.98788, 0.98731, 0.98668, 0.98603, 0.98538, 
                    0.98466, 0.98387, 0.98312, 0.98222, 0.98126, 
                    0.98025, 0.97915, 0.97796, 0.97661, 0.97525, 
                    0.97362, 0.97186, 0.96998, 0.96791, 0.96561, 
                    0.96309, 0.96022, 0.95712, 0.95371, 0.94999, 
                    0.94613, 0.94175, 0.93718, 0.93243, 0.92718, 
                    0.92153, 0.91552, 0.90922, 0.90222, 0.89439, 
                    0.88599, 0.87673, 0.86681, 0.85591, 0.84415, 
                    0.83155, 0.81783, 0.80315, 0.78744, 0.77049, 
                    0.75215, 0.73277, 0.71162, 0.68922, 0.66525, 
                    0.63921, 0.61065, 0.57978, 0.54703, 0.51279, 
                    0.47678, 0.43961, 0.40115, 0.36195, 0.32283, 
                    0.28410, 0.24665, 0.21012, 0.17622, 0.14505, 
                    0.11698, 0.09239, 0.07124, 0.05354, 0.03915, 
                    0.02781, 0.01916, 0.01278, 0.00825, 0.00514, 
                    0.00310, 0.00180, 0.00101, 0.00055, 0.00029, 0.00014)), 
       row.names = c(NA, -111L),
       class = "data.frame")

df = mutate(df, 
            logit_SWE1880 = log( SWE1880 / (1-SWE1880)),
            logit_PRT1950 = log( PRT1950 / (1-PRT1950)),
            logit_JPN2000 = log( JPN2000 / (1-JPN2000))
            )


Brass_transform = function(alpha,beta,std) {
    
    std_logit = df[[ paste0('logit_',std) ]]
    new_logit = alpha + beta * std_logit
    
    return(list( age=0:110, 
                 logit = new_logit,
                 lx = 1/(1+exp(-new_logit)) ) )
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Brass Relational Logit Model"),

   sidebarLayout(
      sidebarPanel(
        tags$div(HTML('Note: this version of the Brass model transforms the',
                  '<b> log odds of survival</b>',
                 ' from the standard table. <br/><br/>The classical version uses <b> one-half of the',
                 ' log odds of non-survival</b>.<br/><br/>Increasing &beta;',
                 ' above 1 in this app will "twist" the survival function clockwise at the median',
                 ' age of survival')),
        tags$hr(),
        
         sliderInput(inputId='alpha_slider',
                     label = "alpha",
                     min = -4,
                     max =  4,
                     step = 0.1,
                     value = 0),

      sliderInput(inputId='beta_slider',
                      label = "beta",
                      min = 0.2,
                      max = 1.8,
                      step = 0.1,
                      value = 1),
      
      
      selectInput(inputId="plot_select", 
                  label="Select data to plot",
                  choices = c('logmu','lx','dx','logit(lx)'),
                  selected='lx'),
      
      selectInput(inputId="std_select", 
                  label="Standard Schedule",
                  choices = c('SWE1880', 'PRT1950', 'JPN2000'),
                  selected='SWE1880')
      
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

    alpha = input$alpha_slider
    beta  = input$beta_slider
    std   = input$std_select
    plot =  input$plot_select
    
    legend_text = c( 
                   paste0('baseline\n(a=',
                           sprintf("%+2.1f", 0),
                          ', b=',
                          sprintf("% 2.1f", 1),')'),
                   paste0('selected\n(a=',
                          sprintf("%+2.1f", alpha),
                          ', b=',
                          sprintf("% 2.1f", beta),')')
                    ) 
                   
    
    # calculate log mortality and baseline
    transformed   = Brass_transform(alpha,beta,std)
    baseline      = Brass_transform(0,1,std)

    n              = rep(1,111)
    age_dx         = baseline$age + n/2
    
    baseline_dx    = c( -diff(baseline$lx   ), tail(baseline$lx,   1))
    transformed_dx = c( -diff(transformed$lx), tail(transformed$lx,1))
    
    baseline_logm    = log( -1/n * c( log( tail(baseline$lx,-1)    / head(baseline$lx,-1)), NaN) ) 
    transformed_logm = log( -1/n * c( log( tail(transformed$lx,-1) / head(transformed$lx,-1)), NaN) ) 
    
    if (plot == 'logmu') {
    
      df = data.frame( age= rep(baseline$age,2),
                         sched = rep(legend_text,c(111,111)),
                         logm = c(baseline_logm,transformed_logm))
        
       G =  ggplot(data=df, aes(x=age, y=logm, 
                            color = sched, 
                            alpha = sched)) +
          geom_point(size=4) +
          geom_line(lwd=2) +
          scale_alpha_manual(guide='none',
                             values=c(1.0,0.7)) +
          scale_color_manual(values=c('grey','red')) +
          scale_x_continuous(limits=c(0,110),
                             breaks=seq(0,110,10)) +
          scale_y_continuous(limits=c(-15,0)) +
          labs(title='Log Mortality Rates by Age',
               subtitle=paste('Standard Schedule =',std)) +
          theme_bw() +
          theme(legend.text = element_text(family='mono')) +
          theme(panel.grid.major = element_line(colour = "darkgrey"))
        
    } else if (plot == 'lx') {
      
      df = data.frame( age= rep(baseline$age,2),
                       sched = rep(legend_text,c(111,111)),
                       lx = c(baseline$lx, transformed$lx))
      
      G =  ggplot(data=df, aes(x=age, y=lx, 
                          color = sched, 
                          alpha = sched)) +
        geom_point(size=4) +
        geom_line(lwd=2) +
        scale_alpha_manual(guide='none',
                           values=c(1.0,0.7)) +
        scale_color_manual(values=c('grey','limegreen')) +
        scale_x_continuous(limits=c(0,110),
                           breaks=seq(0,110,10)) +
        scale_y_continuous(limits=c(0,1)) +
        labs(title='Survival Prob by Age',
             subtitle=paste('Standard Schedule =',std)) +
        theme_bw() +
        theme(legend.text = element_text(family='mono')) +
        theme(panel.grid.major = element_line(colour = "darkgrey"))

    } else if (plot == 'logit(lx)') {
      
      df = data.frame( age= rep(baseline$age,2),
                       sched = rep(legend_text,c(111,111)),
                       logit = c(baseline$logit, transformed$logit)) %>% 
               filter(is.finite(logit))
      
      G =  ggplot(data=df, aes(x=age, y=logit, 
                               color = sched, 
                               alpha = sched)) +
        geom_point(size=4) +
        geom_line(lwd=2) +
        scale_alpha_manual(guide='none',
                           values=c(1.0,0.7)) +
        scale_color_manual(values=c('grey','dodgerblue')) +
        scale_x_continuous(limits=c(0,110),
                           breaks=seq(0,110,10)) +
        scale_y_continuous(limits=c(-6,6)) +
        labs(title='Log Odds of Survival Prob by Age',
             subtitle=paste('Standard Schedule =',std)) +
        theme_bw() +
        theme(legend.text = element_text(family='mono')) +
        theme(panel.grid.major = element_line(colour = "darkgrey"))
      
            
    } else if (plot == 'dx') {  
      
      df = data.frame( age= age_dx,
                       sched = rep(legend_text,c(111,111)),
                       dx = c(baseline_dx, transformed_dx))
      
      G=  ggplot(data=df, aes(x=age, y=dx, 
                          color = sched, 
                          alpha = sched)) +
        geom_point(size=4) +
        geom_line(lwd=2) +
        scale_alpha_manual(guide='none',
                           values=c(1.0,0.7)) +
        scale_color_manual(values=c('grey','purple')) +
        scale_x_continuous(limits=c(0,110),
                           breaks=seq(0,110,10)) +
        scale_y_continuous(limits=c(0,.15)) +
        labs(title=paste('Deaths by Age'),
             subtitle=paste('Standard Schedule =',std)) +
        theme_bw() +
        theme(legend.text = element_text(family='mono')) +
        theme(panel.grid.major = element_line(colour = "darkgrey"))
      
      
      }  # dx 
    
    print(G)
    
   }) # renderPlot
   
} # server

# Run the application 
shinyApp(ui = ui, server = server)


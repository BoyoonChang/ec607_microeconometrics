###########################
#### A6 ###################
###########################

library(pacman)
library(broom)
library(ggplot2)
p_load(broom, ggplot2, 
       ggthemes, viridis, dplyr, magrittr, 
       knitr, parallel, tidyverse, janitor, 
       kableExtra, gridExtra, estimatr, huxtable)


## Data generating process

n = 100
t = 6

### time assignment
sim_fun = function(input_df){
  df = 
    map_df(
      1:nrow(input_df),
      function(i){
        print(i)
        beta0 = 15
        
        df = tibble(
          beta1 = input_df$beta1[i],
          beta2 = input_df$beta2[i],
          beta3 = input_df$beta3[i],
          alpha = input_df$alpha[i],
          TEffect = input_df$TEffect[i],
          
          t = input_df$t[i],
          ## creating time variable
          ## continuous time
          time1 = runif(n, min=0, max = t),
          time = as.integer(time1),
          ind_id = seq(1:n),
          error = rnorm(n, 0, 3),
          post = as.numeric(time1 >= t/2),
          
          # assignment of treatment
          treat = sample(c(0,1), n, prob=c(0.5, 0.5), replace = TRUE),
          
          # outcome variables
          ## pre-treatment period (parallel trends assumed)
          y_pre = alpha + beta1* time1 + beta0*treat + error,
          ## pre-treatment period (parallel trends violated)
          y_pre2 = alpha + beta1* time1*(1-post) - beta2*time1*post + beta0*treat + error,
          
          ## post-treatment period (assumed parallel trend by using the same beta1)
          y_post = alpha + beta1* time1 + beta0*treat + TEffect*treat + error,
          
          ## separating controls 
          y_pre_c = alpha + beta1* time1  + error,
          y_post_c = alpha + beta1* time1  + error,
          y_c = ifelse(post ==0, y_pre_c, y_post_c),
          
          ## separating treated
          y_pre_t = alpha +  beta1*time1 + beta0 + error,
          ## consider level changes in post-treatment periods
          y_post_t1 = alpha + TEffect + (beta1+beta3)*time1  + beta0 + error, 
          ## consider level as well as slope changes in post-treatment periods
          y_post_t2 = alpha + TEffect + (beta1+beta2)*time1 + beta0 +  error,
          ## create treated outcomes throughout the analysis periods(level diff)
          y_t1 = ifelse(post ==0, y_pre_t, y_post_t1),
          ## create treated outcomes throughout the analysis periods(level&slope diff)
          y_t2 = ifelse(post ==0, y_pre_t, y_post_t2),
          
          # y
          y1 = ifelse(treat==0, y_c, y_t1),
          y2 = ifelse(treat==0, y_c, y_t2),
          y3 = ifelse(post==0, y_pre, y_post),
          y4 = ifelse(post==0, y_pre2, y_post),
          
          ## taking the difference
          diff1 = y_t1-y_c,
          diff2 = y_t2-y_c
        )
        return(df)
        
      }
    )
}
input_df1 = expand.grid(t = 10, beta1 = 1:3, 
                        beta2 = -3:3, beta3 = -3:3, alpha = 7:10, TEffect=c(30,40,50))
one_iter = sim_fun(input_df1)
one_iter$TEffect = as.factor(one_iter$TEffect)

## Shiny App
#### Try out
library(shiny)
## building ui
ui = fluidPage(
  titlePanel("Test"),
  sidebarLayout(position="left",
    sidebarPanel(
             sliderInput(inputId = "t", label = "Time", min=6,  max=10, value=6),
             sliderInput(inputId = "beta3", label = "beta3", min=-3,  max=3, value=3),
             sliderInput(inputId = "beta1", label = "beta1", min=1,  max=3, value=2),
             sliderInput(inputId = "beta2", label = "beta2", min=-3,  max=3, value=3),
             sliderInput(inputId = "alpha", label = "Intercept", min=7,  max=10, value=7),
             sliderInput(inputId = "TEffect", label = "Treatment Effect", min=30,  max=50, value=30)
      ),
      mainPanel(
        plotOutput("Plot")
    )
  )
)


## data
data_c = one_iter %>% filter(treat==0)
data_t = one_iter %>% filter(treat==1)

# mpgData$am = factor(mpgData$am, labels = c("Automatic", "Manual"))
## define server
server = function(input, output){
  output$Plot = renderPlot({
    ggplot() + 
      geom_point(data = data_c %>% filter(t == input$t & beta3 == input$beta3
                                          & beta1 ==input$beta1
                                          & beta2 == input$beta2
                                          & alpha ==input$alpha
                                          & TEffect == input$TEffect), 
                 aes(x = time1, y = y_c), color = "steelblue") +
      geom_point(data = data_t %>% filter(t == input$t & beta3 == input$beta3
                                          & beta1 ==input$beta1
                                          & beta2 == input$beta2
                                          & alpha ==input$alpha
                                          & TEffect == input$TEffect),
                 aes(x=time1, y=y_t2), color = "darkred") +
      geom_point(data = data_t %>% filter(t == input$t & beta3 == input$beta3
                                          & beta1 ==input$beta1
                                          & beta2 == input$beta2
                                          & alpha ==input$alpha
                                          & TEffect == input$TEffect),
                 aes(x=time1, y=y_t1), color = "darkred", alpha = 0.2) +
            geom_vline(xintercept=input$t/2, linetype = "dashed")+
      xlab("Time")+
      ylab("Outcome Variable")+
      ylim(-100, 150)
    
  })
}
## combine ui with server
shinyApp(ui, server)
# runApp("~/shinyapp")

















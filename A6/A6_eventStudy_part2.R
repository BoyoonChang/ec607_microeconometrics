## Event Study

# **Part 1.** Simulate a DGP in which treatment is imposed on a fraction of units in a panel. 
# Adopting something like the DGP of A5 would be reasonable. 
# Produce an event study figure that represents the effect of treatment on outcomes.

## Step 0: Load the necessary packages
library(pacman)
p_load("data.table", "ggplot2", "collapse", "fixest", "shiny", "tidyverse", "dplyr", "broom")


## Step 1: Create a panel
### allocate individual id as well as time 

dgp_fun1 = function(input_df){
  df = map_df(
    1:nrow(input_df),
    function(i){
       print(i)    
       ind_totalID = input_df$ind_totalID[i]
       pre_totalT = input_df$pre_totalT[i]
       post_totalT = input_df$post_totalT[i] 
       a_pre = input_df$a_pre[i]
       a_post = input_df$a_post[i] 
       b_pre = input_df$b_pre[i]
       b_post = input_df$b_post[i]
       pre_trend = input_df$pre_trend[i]
       post_trend = input_df$post_trend[i]
       pre_slope = input_df$pre_slope[i]
       post_slope = input_df$post_slope[i]
  # setting the number of periods
  time = rep(1:(pre_totalT+post_totalT), each = ind_totalID)
  # setting the number of ids
  ind_ID = rep(1:ind_totalID, pre_totalT + post_totalT)
  # setting the number of observations
  n = 1:length(ind_ID)*length(time)
  # setting the error term 
  error = rnorm(n, 0, 1)
  # setting the treatment
  treat = sample(c(0,1), size = ind_totalID, replace=TRUE, prob=c(0.5, 0.5))

  # generating individual level variation (alpha)
  ## differ the level of variation between treated and control group
  alpha = ifelse(time < pre_totalT+1,
                 # pre
                 ifelse(treat == 0, 
                        # control gets a_pre
                        rnorm(ind_totalID, a_pre, 1),
                        # treated gets a_pre + pre_trend
                        rnorm(ind_totalID, a_pre + pre_trend, 1)),
                 # post
                 ifelse(treat == 0, 
                        # control gets a_post
                        rnorm(ind_totalID, a_post, 1), 
                        # treated gets a_post + post_trend
                        rnorm(ind_totalID, a_post + post_trend, 1)))
                #### Note if pre_trend = post_trend, this is parallel trend
  
  # generating period specific slope variation (beta) 
  beta = ifelse(time < pre_totalT+1, 
                # pre
                ifelse(treat == 0,
                       # control gets b_pre
                       rep(b_pre, each = pre_totalT*ind_totalID),
                       # treated gets b_pre + pre_slope
                       rep(b_pre+pre_slope, each = pre_totalT*ind_totalID)),
                # post
                ifelse(treat ==0, 
                       # control gets b_post
                       rep(sapply(1:post_totalT, function(x){rnorm(1, b_post*x, 1)}), each = ind_totalID),
                       # treated gets b_post + post_slope
                       rep(sapply(1:post_totalT, function(x){rnorm(1, (b_post+post_slope)*x, 1)}), each = ind_totalID)))
                #### Note if pre_slope =0, then parallel trend
                
  DT = data.table(treat, n, pre_totalT, post_totalT, time, 
                  ind_ID, alpha, beta, error, a_pre, a_post, b_pre, b_post, pre_trend, post_trend, pre_slope, post_slope)

  DT[, y := alpha + beta*treat + error][
          , ':=' (upper = max(y), lower = min(y)), by=time]
  
  return(DT)
    }
  )
}

dgp_fun2 = function(input_df){
  df = map_df(
    1:nrow(input_df),
    function(i){
      print(i)    
      ind_totalID = input_df$ind_totalID[i]
      pre_totalT = input_df$pre_totalT[i]
      post_totalT = input_df$post_totalT[i] 
      a_pre = input_df$a_pre[i]
      a_post = input_df$a_post[i] 
      b_pre = input_df$b_pre[i]
      b_post = input_df$b_post[i]
      pre_trend = input_df$pre_trend[i]
      post_trend = input_df$post_trend[i]
      pre_slope = input_df$pre_slope[i]
      post_slope = input_df$post_slope[i]
      # setting the number of periods
      time = rep(1:(pre_totalT+post_totalT), each = ind_totalID)
      # setting the number of ids
      ind_ID = rep(1:ind_totalID, pre_totalT + post_totalT)
      # setting the number of observations
      n = 1:length(ind_ID)*length(time)
      # setting the error term 
      error = rnorm(n, 0, 1)
      # setting the treatment
      treat = rep(sample(c(0,1), size = ind_totalID, replace=TRUE, prob=c(0.5, 0.5)), pre_totalT+post_totalT)
      
      # generating individual level variation (alpha)
      ## differ the level of variation between treated and control group
      alpha = ifelse(time < pre_totalT+1,
                     # pre
                     ifelse(treat == 0, 
                            # control gets a_pre
                            a_pre,
                            # treated gets a_pre + pre_trend
                            a_pre + pre_trend),
                     # post
                     ifelse(treat == 0, 
                            # control gets a_post
                            a_post, 
                            # treated gets a_post + post_trend
                            a_post+post_trend))
      #### Note if pre_trend = post_trend, this is parallel trend
      
      # generating period specific slope variation (beta) 
      # beta = 
      #### Note if pre_slope =0, then parallel trend
      
      DT = data.table(treat, n, pre_totalT, post_totalT, time, 
                      ind_ID, alpha, beta, error, 
                      a_pre, a_post, b_pre, b_post, 
                      pre_trend, post_trend, pre_slope, post_slope)
      
      DT[, beta := 
           ifelse(time < pre_totalT, 
                          ifelse(treat == 0, 
                                 b_pre*time, 
                                 (b_pre + pre_slope)*time),
                          ifelse(treat == 0, 
                                 b_post*(time-5), 
                                 (b_post + post_slope)*(time-5)))][
        , y := alpha + beta*treat + error][
        , ':=' (upper = max(y), lower = min(y)), by=time]
      
      return(DT)
    }
  )
}

if (time < pre_total){
  ifelse(treat == 0, 
         b_pre*time, 
         (b_pre + pre_slope)*time)
}else if (time == pre_total){
  
}

one_df = tibble(ind_totalID = 6, 
                pre_totalT = 5, 
                post_totalT = 5, 
                a_pre = -1:1,
                a_post = 2, 
                b_pre = 2,
                b_post = 2,
                pre_trend = 0,
                post_trend = 10,
                pre_slope = 0,
                post_slope = 0)
d = dgp_fun2(one_df)
d$time = relevel(factor(d$time), ref=max(d$pre_totalT))
est = feols(y ~ as.factor(time)*treat
      |ind_ID, data = d) 
est_sum = est %>% tidy() %>% filter(str_detect(term, coll(":treat")))
est_sum$time = as.numeric(str_sub(est_sum$term, 16, -7))
t_cv = qt(0.025, df = (max(d$n)-2), lower.tail = FALSE)
est_sum$ci_l = est_sum$estimate - t_cv*est_sum$std.error 
est_sum$ci_h = est_sum$estimate + t_cv*est_sum$std.error 

ggplot(est_sum, aes(x=factor(time), y = estimate)) + 
  geom_linerange(aes(ymin=ci_l, ymax=ci_h))+
  geom_pointrange(aes(ymin=ci_l, ymax=ci_h))+
  geom_errorbar(aes(ymin=ci_l, ymax=ci_h)) 

## Shiny App
# input df
input_df = expand.grid(ind_totalID = 20, 
                       pre_totalT = 5, 
                       post_totalT = 5:7, 
                       a_pre = 1,
                       a_post = -1:1, 
                       b_pre = -1:1,
                       b_post = -1:1,
                       pre_trend = -1:1,
                       post_trend = -2:2,
                       pre_slope = -1:1,
                       post_slope = -1:1)
one_iter = dgp_fun(input_df)
two_iter = dgp_fun2(input_df)

two_df = expand.grid(ind_totalID = 20, 
                     pre_totalT = 5, 
                     post_totalT = 5:7, 
                     a_pre = 5,
                     a_post = 10, 
                     b_pre = 1,
                     b_post = 1,
                     pre_trend = -1:1,
                     post_trend = -1:1,
                     pre_slope = -1:1,
                     post_slope = -1:1)
two_iter = dgp_fun2(two_df)
View(two_iter)
## Shiny App
#### Try out
## building ui
ui = fluidPage(
  titlePanel("Test"),
  sidebarLayout(position="left",
                sidebarPanel(
                  # sliderInput(inputId = "ind_totalID", label = "Number of id", min=6,  max=10, value=6),
                  # sliderInput(inputId = "pre_totalT", label = "Number of pre-periods", min=-3,  max=3, value=3),
                  sliderInput(inputId = "post_totalT", label = "post_totalT", min=5,  max=7, value=5),
                  # sliderInput(inputId = "a_pre", label = "a_pre", min=5,  max=5, value=5),
                  # sliderInput(inputId = "a_post", label = "a_post", min=5,  max=5, value=5),
                  # sliderInput(inputId = "b_pre", label = "b_pre", min=-1,  max=1, value=1),
                  # sliderInput(inputId = "b_post", label = "b_post", min=-1,  max=1, value=1),
                  sliderInput(inputId = "pre_trend", label = "pre_trend", min=-1,  max=1, value=1),
                  sliderInput(inputId = "post_trend", label = "post_trend", min=-1,  max=1, value=1),
                  sliderInput(inputId = "pre_slope", label = "pre_slope", min=-1,  max=1, value=1),
                  sliderInput(inputId = "post_slope", label = "post_slope", min=-1,  max=1, value=1)
                  # sliderInput(inputId = "post_totalT", label = "Number of post-periods", min=5,  max=7, value=5),
                  # sliderInput(inputId = "a_pre", label = "Level pre-periods", min=-1,  max=1, value=1),
                  # sliderInput(inputId = "a_post", label = "Level post-periods", min=-1,  max=1, value=1),
                  # sliderInput(inputId = "b_pre", label = "Slope pre-periods", min=-1,  max=1, value=1),
                  # sliderInput(inputId = "b_post", label = "Slope post-periods", min=-1,  max=1, value=1),
                  # sliderInput(inputId = "pre_trend", label = "Trend pre-periods", min=-1,  max=1, value=1),
                  # # sliderInput(inputId = "post_trend", label = "Trend post-periods", min=-2,  max=2, value=2),
                  # sliderInput(inputId = "pre_slope", label = "Slope pre-periods", min=-1,  max=1, value=1),
                  # sliderInput(inputId = "post_slope", label = "Slope post-periods", min=-1,  max=1, value=1)
                  # 
                ),
                mainPanel(
                  plotOutput("Plot")
                )
  )
)


## data
data_c = two_iter %>% filter(treat==0)
data_t = two_iter %>% filter(treat==1)

# mpgData$am = factor(mpgData$am, labels = c("Automatic", "Manual"))
## define server
server = function(input, output){

  output$Plot = renderPlot({
    data_c_f = data_c %>% filter(post_totalT == input$post_totalT
                                 # & a_pre ==input$a_pre
                                 # & a_post == input$a_post 
                                 # & b_pre == input$b_pre
                                 # & b_post == input$b_post
                                 & pre_trend == input$pre_trend
                                 & post_trend == input$post_trend
                                 & pre_slope == input$pre_slope
                                 & post_slope == input$post_slope
                                 )
    data_t_f = data_t %>% filter(post_totalT == input$post_totalT
                                 # & a_pre ==input$a_pre
                                 # & a_post == input$a_post 
                                 # & b_pre == input$b_pre
                                 # & b_post == input$b_post
                                 & pre_trend == input$pre_trend
                                 & post_trend == input$post_trend
                                 & pre_slope == input$pre_slope
                                 & post_slope == input$post_slope
                                 )
    mean_y=mean(data_t_f[time>pre_totalT,]$y)
    ggplot() + 
      geom_jitter(data = data_c_f, 
                 aes(x = time, y = y), color = "steelblue") +
      geom_jitter(data = data_t_f,
                 aes(x=time, y=y), color = "darkred") +
      geom_jitter(data = data_t_f %>% filter(time>pre_totalT),
                 aes(x=time, y=y), color = "darkred", alpha = 0.2) +
      geom_vline(xintercept=5, linetype = "dashed")+
      xlab("Time")+
      ylab("Outcome Variable") +
      # geom_hline(yintercept = mean(data_t_f[time>pre_totalT,]$y), color = "darkred")+
      # geom_hline(yintercept = mean(data_t_f[time<=pre_totalT,]$y), color = "darkred", alpha = 0.3)+
      # geom_hline(yintercept = mean(data_c_f[time>pre_totalT,]$y), color = "steelblue")+
      # geom_hline(yintercept = mean(data_c_f[time<=pre_totalT,]$y), color = "steelblue", alpha = 0.3)+
      geom_segment(data = data_c_f, aes(x = 5, xend = 6+input$post_totalT,
                                        y =mean(data_c_f[time >5&treat==0]$y), 
                                        yend=mean(data_c_f[time >5&treat==0]$y)), 
                  color = "steelblue")+
      geom_segment(data = data_t_f, aes(x = 5, xend = 6+input$post_totalT,
                                        y =mean(data_t_f[time >5&treat==1]$y), 
                                        yend=mean(data_t_f[time >5&treat==1]$y)), 
                   color = "darkred")+
      geom_segment(data = data_c_f, aes(x = 1, xend = 5,
                                        y =mean(data_c_f[time <=5&treat==0]$y), 
                                        yend=mean(data_c_f[time <=5&treat==0]$y)), 
                   color = "steelblue")+
      geom_segment(data = data_t_f, aes(x = 1, xend = 5,
                                        y =mean(data_t_f[time <=5&treat==1]$y), 
                                        yend=mean(data_t_f[time <=5&treat==1]$y)), 
                   color = "darkred")+
      ylim(0,50)
      # stat_summary(data = data_t_f, aes(x = time, y = data_c_f[time<=pre_totalT,]$y),fun="mean", color="black", geom="line")
  })
}
## combine ui with server
shinyApp(ui, server)


## tryout using one_iter

ggplot(d) + 
  geom_jitter(aes(x = time, y = y, color = qF(treat)))+
  geom_segment(aes(x = 5, xend=10, 
                   y = mean(d[time >5&treat==0]$y), 
                   yend=mean(d[time>5&treat==0]$y)), color = "blue")
  


########################
dgp_fun = function(ind_totalID = 6, 
                   pre_totalT = 5, 
                   post_totalT = 5, 
                   a_pre = 2,
                   a_post = 2, 
                   b_pre = 2,
                   b_post = 2,
                   pre_trend = 2,
                   post_trend = 10,
                   pre_slope = 0,
                   post_slope = 0)
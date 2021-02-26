library(shiny)

shiny_fun = function(input_df){
  df = map_df(
    1:nrow(input_df),
    function(i) {
      print(i)
      ind_totalID = input_df$ind_totalID[i] 
      pre_totalT = input_df$pre_totalT[i]
      post_totalT = input_df$post_totalT[i]
      c_intercept = input_df$c_intercept[i]
      c_slope = input_df$c_slope[i]
      t_slope = input_df$t_slope[i]
      dgp_fun2(ind_totalID,
               pre_totalT,
               post_totalT,
               c_intercept,
               c_slope,
               t_slope)
    }
  )
}

input_df = expand.grid(ind_totalID = 100, 
                       pre_totalT = 5,
                       post_totalT = 5,
                       c_intercept = 0:5,
                       c_slope = 0:5,
                       t_slope = 0:5)
one_iter = shiny_fun(input_df)

## building ui
ui = fluidPage(
  titlePanel("Event Study"),
  sidebarLayout(position="left",
                sidebarPanel(
                  selectInput(inputId = "pre_totalT", label = "pre_totalT", choices = 5),
                  selectInput(inputId = "post_totalT", label = "post_totalT", choices = 5),
                  sliderInput(inputId = "c_intercept", label = "c_intercept", min=0,  max=5, value=0),
                  sliderInput(inputId = "c_slope", label = "c_slope", min=0,  max=5, value=2),
                  sliderInput(inputId = "t_slope", label = "t_slope", min=0,  max=5, value=4),
                  selectInput(inputId = "case", label = "case",
                              choices = list("y_t1" = 1, "y_t2" = 2,
                                             "y_t3" = 3, "y_t4" = 4), selected = 1)
                ),
                mainPanel(
                  plotOutput("Plot"),
                  plotOutput("Plot2")
                )
  )
)


## data
a = one_iter %>% pivot_longer(y_t1:y_t4, names_to = "case", values_to = "y_t")
# mpgData$am = factor(mpgData$am, labels = c("Automatic", "Manual"))
## define server
server = function(input, output){
  output$Plot = renderPlot({
    # ggplot_fun(input$case)
    b= a %>% filter(    pre_totalT == input$pre_totalT,
                        post_totalT == input$post_totalT,
                        c_intercept == input$c_intercept,
                        c_slope == input$c_slope,
                        t_slope == input$t_slope) 
    ggplot() +
      geom_jitter(data = b[b$treat==1 & b$case==paste0("y_t", input$case),],
                  aes(x = time, y = y_t, color = case), alpha=0.2)  +
      stat_summary(data = b[b$treat==1 & b$case==paste0("y_t", input$case),],
                   aes(x= time, y = y_t, color = case),fun=mean, geom="line", alpha = 1,size = 1) +
      geom_jitter(data = b[b$treat==0,],
                  aes(x = time, y = y_c), color = "steelblue", alpha = 0.2, size = 1)+
      stat_summary(data = b[b$treat==0,], aes(x=time, y = y_c),color = "steelblue",
                   fun=mean, geom="line", alpha = 1,size = 1)+
      stat_summary(data = b[b$treat==1 ,],
                   aes(x= time, y = y_cf), color = "black", fun=mean, geom="line", 
                   linetype = "dotted", alpha = 1,size = 1) +
      geom_vline(xintercept = 6, linetype="dashed")+
      ylab("y")+
      xlab("Time") +
      scale_color_viridis_d() +
      theme(legend.position="none")+
      ylim(0,50)
  })
  output$Plot2 = renderPlot({
    b= a %>% filter(  pre_totalT == input$pre_totalT,
                      post_totalT == input$post_totalT,
                      c_intercept == input$c_intercept,
                      c_slope == input$c_slope,
                      t_slope == input$t_slope) 
    b$time = relevel(factor(b$time), ref="5")
    est1 = feols(y1~as.factor(time)*treat, data = b) %>% 
      tidy() %>% filter(str_detect(term, coll(":treat")))
    est2 = feols(y2~as.factor(time)*treat, data = b) %>% 
      tidy() %>% filter(str_detect(term, coll(":treat")))
    est3 = feols(y3~as.factor(time)*treat, data = b) %>%
      tidy() %>% filter(str_detect(term, coll(":treat")))
    est4 = feols(y4~as.factor(time)*treat, data = b) %>% 
      tidy() %>% filter(str_detect(term, coll(":treat")))
    sim_data = cbind(rbind(est1, est2, est3, est4), rep)
    sim_data$time = as.numeric(str_sub(sim_data$term, 16, -7))
    t_cv = qt(0.025, df = (max(b$n)-2), lower.tail = FALSE)
    sim_data$ci_l = sim_data$estimate - t_cv*sim_data$std.error 
    sim_data$ci_h = sim_data$estimate + t_cv*sim_data$std.error 
    ggplot(sim_data %>% filter(rep==input$case), aes(x=factor(time), y = estimate)) + 
      geom_linerange(aes(ymin=ci_l, ymax=ci_h))+
      geom_pointrange(aes(ymin=ci_l, ymax=ci_h))+
      geom_errorbar(aes(ymin=ci_l, ymax=ci_h)) +
      xlab("Time") + ylab("Estimates") + 
      ylim(-30, 120)
    
  })
}


## combine ui with server
shinyApp(ui, server)


dgp_fun2 = function(ind_totalID = 6, 
                    pre_totalT = 5, 
                    post_totalT = 5, 
                    a_pre = -1:1,
                    a_post = 2, 
                    b_pre = 2,
                    b_post = 2,
                    pre_trend = 2,
                    post_trend = 10,
                    pre_slope = 0,
                    post_slope = 5){
      # setting the number of periods
      time = rep(1:(pre_totalT+post_totalT), each = ind_totalID)
      t=time
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
      # beta = ifelse(time < pre_totalT+1, 
      #               # pre
      #               ifelse(treat == 0,
      #                      # control gets b_pre
      #                      b_pre,
      #                      # treated gets b_pre + pre_slope
      #                      b_pre+pre_slope),
      #               # post
      #               ifelse(treat ==0, 
      #                      # control gets b_post
      #                      seq(1,5),
      #                      # treated gets b_post + post_slope
      #                      (b_post+post_slope)*time))
      #### Note if pre_slope =0, then parallel trend
      
      DT = data.table(treat, n, pre_totalT, post_totalT, time, 
                      ind_ID, alpha, beta, error, a_pre, a_post, b_pre, b_post, pre_trend, post_trend, pre_slope, post_slope)
      
      DT[, beta := ifelse(time < pre_totalT+1, 
                          # pre
                          ifelse(treat == 0,
                                 # control gets b_pre
                                 b_pre,
                                 # treated gets b_pre + pre_slope
                                 b_pre+pre_slope),
                          # post
                          ifelse(treat ==0, 
                                 # control gets b_post
                                 b_post*time,
                                 # treated gets b_post + post_slope
                                 (b_post+post_slope)*time))][, y := alpha + beta*treat + error][
                                   , ':=' (upper = max(y), lower = min(y)), by=time]
      
      return(DT)
    }
d= dgp_fun2()
cbind(d[d$treat==0 & d$time >=6,]$time, 
      d[d$treat==0 & d$time >=6,]$beta,
      d[d$treat==1 & d$time >=6,]$beta,
      d[d$treat==0 & d$time <6,]$beta,
      d[d$treat==1 & d$time <6,]$beta,
      d[d$treat==0 & d$time >=6,]$alpha,
      d[d$treat==1 & d$time >=6,]$alpha,
      d[d$treat==0 & d$time <6,]$alpha,
      d[d$treat==1 & d$time <6,]$alpha
)
ggplot(d) + geom_jitter(data= d[d$treat==0, ], aes(x=time, y=y), color="steelblue") +
  geom_jitter(data=d[d$treat==1,], aes(x=time, y=y), color="darkred")


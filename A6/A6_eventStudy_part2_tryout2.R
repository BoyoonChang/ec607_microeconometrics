dgp_fun2 = function(ind_totalID = 100, 
                    pre_totalT = 5, 
                    post_totalT = 5){
  # setting the number of periods
  time = rep(1:(pre_totalT+post_totalT), each = ind_totalID)
  t=time
  # setting the number of ids
  ind_ID = rep(1:ind_totalID, pre_totalT + post_totalT)
  # setting the number of observations
  n = 1:length(time)
  # setting the error term 
  error = rnorm(n, 0, 1)
  # setting the treatment
  treat = sample(c(0,1), size = ind_totalID, replace=TRUE, prob=c(0.5, 0.5))
  # individual level
  alpha_noise_c = rnorm(ind_totalID, 0, 1)
  alpha_noise_t = rnorm(ind_totalID, 4, 1)
  alpha = ifelse(treat==0, alpha_noise_c, alpha_noise_t)
  c_intercept = 0
  c_slope = 2
  # time = 1:10
  t_slope = 5
  # # # control
  y_c = c_intercept + alpha_noise_c + c_slope * time +error
  # 
  # # treated
  y_t = c_intercept + alpha_noise_t + ifelse(time<=5, 5+c_slope*time, 10+t_slope*time ) + error
  # # counterfactual 1
  y_cf= c_intercept + 5 + alpha_noise_t + c_slope * time + error
  ## counterfactual 2
  y_cf1 = c_intercept + 5 + alpha_noise_t + ifelse(time<=5, c_slope*time, c_slope*5 )
  y_cf2 = c_intercept + 5 + alpha_noise_t + ifelse(time<=5, c_slope*time, (t_slope-1)*time )
  y_cf3 = c_intercept + 5 + alpha_noise_t + ifelse(time<=5, ifelse(time == 5, 0, c_slope*time), c_slope*time )
  y_cf4 = c_intercept + 5 + alpha_noise_t + ifelse(time<=5, -c_slope*time, c_slope*time )
  y = c_intercept + alpha_noise_c*(1-treat)+alpha_noise_t*treat + treat*ifelse(time<=5, 5+c_slope*time, 10+t_slope*time )+error
  # DT = data.table(time, ind_ID, n, error, treat, 
  #                 y_c, y_t, y, y_cf)
  DT = data.table(time, ind_ID, n, error, treat, alpha, alpha_noise_c, alpha_noise_t,
                  y_c, y_t, y, 
                  y_cf, y_cf1,
                  y_cf2,
                  y_cf3,
                  y_cf4
                  )
  
  return(DT)
}
a =dgp_fun2()
b = a %>% pivot_longer(y_cf:y_cf4, names_to = "counter_factual", values_to = "y_cf")

View(b)
ggplot() + 
  # geom_jitter(data = a[a$treat==1,], aes(x=time, y = y_t), color = "darkred")+
  # geom_jitter(data = a[a$treat==1,], aes(x=time, y = y_cf), color = "darkred", alpha = 0.3)+
  # geom_jitter(data = a[a$treat==0,], aes(x=time, y= y_c), color = "steelblue") +
  # geom_jitter(data = a[a$treat==1,], aes(x=time, y = y_cf1), color = "black", alpha = 0.3) +
  # geom_jitter(data = a[a$treat==1,], aes(x=time, y = y_cf2), color = "black", alpha = 0.5) +
  # geom_jitter(data = a[a$treat==1,], aes(x=time, y = y_cf3), color = "purple", alpha = 0.7)+
  # geom_jitter(data = a[a$treat==1,], aes(x=time, y = y_cf4), color = "plum", alpha = 0.7)+
 
  geom_point(data = a[a$treat==1,], aes(x=time, y = y_t), color = "darkred")+
  geom_point(data = a[a$treat==1,], aes(x=time, y = y_cf), color = "darkred", alpha = 0.3)+
  geom_point(data = a[a$treat==0,], aes(x=time, y= y_c), color = "steelblue") +
  geom_point(data = a[a$treat==1,], aes(x=time, y = y_cf1), color = "black", alpha = 0.3) +
  geom_point(data = a[a$treat==1,], aes(x=time, y = y_cf2), color = "black", alpha = 0.5) +
  geom_point(data = a[a$treat==1,], aes(x=time, y = y_cf3), color = "purple", alpha = 0.7)+
  geom_point(data = a[a$treat==1,], aes(x=time, y = y_cf4), color = "plum", alpha = 0.7)+

  stat_summary(data = a[a$treat==1,], aes(x=time, y = y_t), fun=mean, color = "darkred", geom="line")+
  stat_summary(data = a[a$treat==1,], aes(x=time, y = y_cf),  fun=mean, color = "darkred", geom="line")+
  stat_summary(data = a[a$treat==0,], aes(x=time, y= y_c),  fun=mean, color = "steelblue", geom="line") +
  stat_summary(data = a[a$treat==1,], aes(x=time, y = y_cf1),  fun=mean, color = "black", geom="line") +
  stat_summary(data = a[a$treat==1,], aes(x=time, y = y_cf2),  fun=mean, color = "black", geom="line") +
  stat_summary(data = a[a$treat==1,], aes(x=time, y = y_cf3),  fun=mean, color = "purple", geom="line")+
  stat_summary(data = a[a$treat==1,], aes(x=time, y = y_cf4),  fun=mean, color = "plum", geom="line")

## event study
library(fixest)
a$time = relevel(factor(a$time), ref=5)
est = feols(y ~ as.factor(time)*treat
            |ind_ID, data = a) 
est_sum = est %>% tidy() %>% filter(str_detect(term, coll(":treat")))
est_sum$time = as.numeric(str_sub(est_sum$term, 16, -7))
t_cv = qt(0.025, df = (max(d$n)-2), lower.tail = FALSE)
est_sum$ci_l = est_sum$estimate - t_cv*est_sum$std.error 
est_sum$ci_h = est_sum$estimate + t_cv*est_sum$std.error 
ggplot(est_sum, aes(x=factor(time), y = estimate)) + 
  geom_linerange(aes(ymin=ci_l, ymax=ci_h))+
  geom_pointrange(aes(ymin=ci_l, ymax=ci_h))+
  geom_errorbar(aes(ymin=ci_l, ymax=ci_h)) 

## simulation
simulation = 



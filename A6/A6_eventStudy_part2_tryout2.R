dgp_fun2 = function(ind_totalID = 100, 
                    pre_totalT = 5, 
                    post_totalT = 5,
                    c_intercept = 0,
                    c_slope = 2,
                    t_slope = 5){
  # setting the number of periods
  time = rep(1:(pre_totalT+post_totalT), each = ind_totalID)
  post = (time > pre_totalT)
  # setting the number of ids
  ind_ID = rep(1:ind_totalID, pre_totalT + post_totalT)
  # setting the number of observations
  n = 1:length(time)
  # setting the error term 
  error = rnorm(n, 0, 5)
  # setting the treatment
  treat = sample(c(0,1), size = ind_totalID, replace=TRUE, prob=c(0.5, 0.5))
  # individual level
  alpha_noise_c = rnorm(ind_totalID, 0, 5)
  alpha_noise_t = rnorm(ind_totalID, 4, 5)
  alpha = ifelse(treat==0, alpha_noise_c, alpha_noise_t)
  # control intercept
    # c_intercept = 0
    # # control slope
    # c_slope = 2
    # # treated slope
    # t_slope = 5
  # control outcome
  y_c = c_intercept + alpha_noise_c + c_slope * time +error
  # treated outcome
  y_t = c_intercept + alpha_noise_t + ifelse(time<=5, 5+c_slope*time, 10+t_slope*time ) + error
  # counterfactual outcome
  ## parallel trend
  y_cf= c_intercept + 5 + alpha_noise_t + c_slope * time + error
  ## non-parallel trend
  y_cf1 = c_intercept + 5 + alpha_noise_t + ifelse(time<=5, c_slope*time, c_slope*6 )+error
  ## diverging trend
  y_cf2 = c_intercept + 5 + alpha_noise_t + ifelse(time<=5, c_slope*time, (t_slope-1)*time )+error
  ## Aschenfelter dip
  y_cf3 = c_intercept + 5 + alpha_noise_t + ifelse(time<=6, ifelse(time == 6, 0, c_slope*time), c_slope*time )+error
  ## Anticipated streatment
  y_cf4 = c_intercept + 5 + alpha_noise_t + ifelse(time<=5, -c_slope*time, c_slope*time )+error
  ## y 
  # y = c_intercept + 
  #   alpha_noise_c*(1-treat)+alpha_noise_t*treat + 
  #   treat*ifelse(time<=5, 5+c_slope*time, 10+t_slope*time )+error
  y = c_intercept + alpha_noise_c*(1-treat) + c_slope * time *(1-treat) + 
      alpha_noise_t*treat + 
      treat*(1-post)*(5+c_slope*time)+treat*post*(10+t_slope*time)+error
  
  DT = data.table(time, post, ind_ID, n, error, treat, alpha, alpha_noise_c, alpha_noise_t,
                  y_c, y_t, y, 
                  y_cf, y_cf1,
                  y_cf2,
                  y_cf3,
                  y_cf4
                  )
  
  return(DT)
}
a =dgp_fun2(ind_totalID = 100, 
            pre_totalT = 5, 
            post_totalT = 5,
            c_intercept = 0,
            c_slope = 5,
            t_slope = 10)
b = a %>% pivot_longer(y_cf:y_cf4, names_to = "counter_factual", values_to = "y_cf")

ggplot() + 
  geom_jitter(data = b[b$treat==1,], aes(x = time, y = y_t), color = "darkred", alpha = 0.2, size = 1)+
  stat_summary(data = b[b$treat==1,], aes(x= time, y = y_t),color = "darkred",fun=mean, geom="line", alpha = 1,size = 2) +
    geom_jitter(data = b[b$treat==1,], aes(x= time, y = y_cf, color = counter_factual),alpha = 0.2, size = 1) +
  scale_color_viridis_d()+
  stat_summary(data = b, aes(x=time, y = y_cf, color = counter_factual),fun=mean, geom="line", alpha = 1,size = 2) +
  geom_jitter(data = b[b$treat==0,], aes(x = time, y = y_c), color = "steelblue", alpha = 0.2, size = 1)+
  stat_summary(data = b[b$treat==0,], aes(x=time, y = y_c),color = "steelblue",fun=mean, geom="line", alpha = 1,size = 2)+
  geom_vline(xintercept = 6, linetype="dashed")
  
  

# View(b)
ggplot() + 
  geom_jitter(data = a[a$treat==1,], aes(x=time, y = y_t), color = "darkred")+
  geom_jitter(data = a[a$treat==1,], aes(x=time, y = y_cf), color = "darkred", alpha = 0.3)+
  geom_jitter(data = a[a$treat==0,], aes(x=time, y= y_c), color = "steelblue") +
  geom_jitter(data = a[a$treat==1,], aes(x=time, y = y_cf1), color = "black", alpha = 0.3) +
  geom_jitter(data = a[a$treat==1,], aes(x=time, y = y_cf2), color = "black", alpha = 0.5) +
  geom_jitter(data = a[a$treat==1,], aes(x=time, y = y_cf3), color = "purple", alpha = 0.7)+
  geom_jitter(data = a[a$treat==1,], aes(x=time, y = y_cf4), color = "plum", alpha = 0.7)+
 
  # geom_point(data = a[a$treat==1,], aes(x=time, y = y_t), color = "darkred")+
  # geom_point(data = a[a$treat==1,], aes(x=time, y = y_cf), color = "darkred", alpha = 0.3)+
  # geom_point(data = a[a$treat==0,], aes(x=time, y= y_c), color = "steelblue") +
  # geom_point(data = a[a$treat==1,], aes(x=time, y = y_cf1), color = "black", alpha = 0.3) +
  # geom_point(data = a[a$treat==1,], aes(x=time, y = y_cf2), color = "black", alpha = 0.5) +
  # geom_point(data = a[a$treat==1,], aes(x=time, y = y_cf3), color = "purple", alpha = 0.7)+
  # geom_point(data = a[a$treat==1,], aes(x=time, y = y_cf4), color = "plum", alpha = 0.7)+

  stat_summary(data = a[a$treat==1,], aes(x=time, y = y_t), fun=mean, color = "darkred", geom="line")+
  stat_summary(data = a[a$treat==1,], aes(x=time, y = y_cf),  fun=mean, color = "darkred", geom="line")+
  stat_summary(data = a[a$treat==0,], aes(x=time, y= y_c),  fun=mean, color = "steelblue", geom="line") +
  stat_summary(data = a[a$treat==1,], aes(x=time, y = y_cf1),  fun=mean, color = "black", geom="line") +
  stat_summary(data = a[a$treat==1,], aes(x=time, y = y_cf2),  fun=mean, color = "black", geom="line") +
  stat_summary(data = a[a$treat==1,], aes(x=time, y = y_cf3),  fun=mean, color = "purple", geom="line")+
  stat_summary(data = a[a$treat==1,], aes(x=time, y = y_cf4),  fun=mean, color = "plum", geom="line")

## event study
library(fixest)
a$time = relevel(factor(a$time), ref="5")
est = feols(y ~ as.factor(time)*treat, data = a) 
# a$treat = qF(a$treat)
# est = feols(y ~ i(treat, time, 5)|ind_ID + time, data = a) 
# coefplot(est)
est_sum = est %>% tidy() %>% filter(str_detect(term, coll(":treat")))
est_sum$time = as.numeric(str_sub(est_sum$term, 16, -7))
t_cv = qt(0.025, df = (max(a$n)-2), lower.tail = FALSE)
est_sum$ci_l = est_sum$estimate - t_cv*est_sum$std.error 
est_sum$ci_h = est_sum$estimate + t_cv*est_sum$std.error 
ggplot(est_sum, aes(x=factor(time), y = estimate)) + 
  geom_linerange(aes(ymin=ci_l, ymax=ci_h))+
  geom_pointrange(aes(ymin=ci_l, ymax=ci_h))+
  geom_errorbar(aes(ymin=ci_l, ymax=ci_h)) 

## Shiny App

View(a)

## Estimation

#### did estimate
### parallel trend
lm(y~post*treat, data = a)
(mean(a$y_t[a$post == 1]) - mean(a$y_c[a$post == 1])) -
  (mean(a$y_t[a$post == 0]) - mean(a$y_c[a$post == 0]))
### parallel trend
(mean(a$y_t[a$post == 1]) - mean(a$y_c[a$post == 1])) -
  (mean(a$y_cf[a$post == 1]) - mean(a$y_c[a$post == 1]))
## 1
(mean(a$y_t[a$post == 1]) - mean(a$y_c[a$post == 1])) -
  (mean(a$y_cf1[a$post == 1]) - mean(a$y_c[a$post == 1]))
## 2
(mean(a$y_t[a$post == 1]) - mean(a$y_c[a$post == 1])) -
  (mean(a$y_cf2[a$post == 1]) - mean(a$y_c[a$post == 1]))
## 3
(mean(a$y_t[a$post == 1]) - mean(a$y_c[a$post == 1])) -
  (mean(a$y_cf3[a$post == 1]) - mean(a$y_c[a$post == 1]))
## 4
(mean(a$y_t[a$post == 1]) - mean(a$y_c[a$post == 1])) -
  (mean(a$y_cf4[a$post == 0]) - mean(a$y_c[a$post == 0]))

## Simulation?
model_sim= function(ind_totalID = 100, 
                    pre_totalT = 5, 
                    post_totalT = 5,
                    c_intercept = 0,
                    c_slope = 5,
                    t_slope = 10){
  a = dgp_fun2(ind_totalID , 
              pre_totalT , 
              post_totalT ,
              c_intercept ,
              c_slope,
              t_slope )
  t_est0 = (mean(a$y_t[a$post == 1]) - mean(a$y_c[a$post == 1])) -
            (mean(a$y_cf[a$post == 1]) - mean(a$y_c[a$post == 1]))
  t_est1 = (mean(a$y_t[a$post == 1]) - mean(a$y_c[a$post == 1])) -
    (mean(a$y_cf1[a$post == 1]) - mean(a$y_c[a$post == 1]))
  ## 2
  t_est2 = (mean(a$y_t[a$post == 1]) - mean(a$y_c[a$post == 1])) -
    (mean(a$y_cf2[a$post == 1]) - mean(a$y_c[a$post == 1]))
  ## 3
  t_est3 = (mean(a$y_t[a$post == 1]) - mean(a$y_c[a$post == 1])) -
    (mean(a$y_cf3[a$post == 1]) - mean(a$y_c[a$post == 1]))
  ## 4
  t_est4 =(mean(a$y_t[a$post == 1]) - mean(a$y_c[a$post == 1])) -
    (mean(a$y_cf4[a$post == 0]) - mean(a$y_c[a$post == 0]))
  return(c(t_est0, t_est1, t_est2, t_est3, t_est4))
}

n_sims=1000
sim_data = replicate(n_sims, model_sim(ind_totalID = 100, 
                                       pre_totalT = 5, 
                                       post_totalT = 5,
                                       c_intercept = 0,
                                       c_slope = 5,
                                       t_slope = 10))
# View(sim_data)
sim_data = as.data.frame(t(sim_data))
colnames(sim_data) = c("t_est0", "t_est1", "t_est2", "t_est3", "t_est4")
sim_data$id = seq(1:nrow(sim_data))

sim_data = sim_data %>% pivot_longer(t_est0:t_est4, names_to="case", values_to = "estimates")

ggplot(sim_data[sim_data$case!="t_est0",]) + 
  geom_density(aes(estimates, fill = case)) + 
  scale_fill_viridis_d() +
  xlim(10, 80)
  
  
#   
# ?geom_density
# ggplot(sim_data)+
#   # geom_density(aes(t_est0),sim_data, fill = "darkred")+
#   geom_vline(xintercept=mean(sim_data$t_est0), color = "red", linetype = "dashed") +
#   geom_density(aes(t_est1), sim_data, fill = "plum")+
#   geom_vline(xintercept=mean(sim_data$t_est1), linetype = "dashed")+
#   geom_density(aes(t_est2), sim_data, fill = "purple")+
#   geom_vline(xintercept=mean(sim_data$t_est2), linetype = "dashed") +
#   geom_density(aes(t_est3), sim_data, fill = "purple")+
#   geom_vline(xintercept=mean(sim_data$t_est3), linetype = "dashed") +
#   geom_density(aes(t_est4), sim_data, fill = "purple")+
#   geom_vline(xintercept=mean(sim_data$t_est4), linetype = "dashed") +
#   xlab("Distribution of DD Estimates")
# 


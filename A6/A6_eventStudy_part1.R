## Event Study

# **Part 1.** Simulate a DGP in which treatment is imposed on a fraction of units in a panel. 
# Adopting something like the DGP of A5 would be reasonable. 
# Produce an event study figure that represents the effect of treatment on outcomes.

## Step 0: Load the necessary packages
library(pacman)
p_load("data.table", "ggplot2", "collapse")


## Step 1: Create a panel
### allocate individual id as well as time 

dgp_fun = function(ind_totalID = 6, 
                   pre_totalT = 5, 
                   post_totalT = 5, 
                   alpha = 10,
                   beta = 40){
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
  alpha_noise = rnorm(ind_totalID, 0,1)
  ## no variation of slope across pre-treatment period (parallel trend pre period)
  beta_noise_pre = rep(0, each=pre_totalT*ind_totalID)
  ## variation of slope across post-treatment period (non-parallel trend post period)
  # beta_noise_post = rep(rnorm(post_totalT , 2*as.numeric(time), sd = 0), each =ind_totalID )
  beta_noise_post = rep(sapply(1:post_totalT, function(x){rnorm(1, 2*x, 3*x)}), each = ind_totalID)
  
  beta_noise = c(beta_noise_pre, beta_noise_post)
  DT = data.table(treat, n, pre_totalT, time, ind_ID, alpha, 
                  alpha_noise, beta, beta_noise, error)
  DT[, ':=' (alpha_ind = alpha + alpha_noise,
             beta_ind = beta + beta_noise), by = ind_ID][
        , y := alpha_ind + beta_ind*treat + error][
          , ':=' (upper = max(y), lower = min(y)), by=time]
  return(DT)
}


d = dgp_fun()
# d[, treat:=sample(c(0,1), size = length(d$time), replace = TRUE, prob=c(0.5,0.5)), by = ind_ID]
# summary(felm(y~))
library(fixest)
d$time = relevel(factor(d$time), ref=max(d$pre_totalT)+1)
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



coefplot(est_sum)
geom_linerange(coef)
p = ggplot(data = d, aes(x = time, y = y))
p+ geom_linerange(aes(ymin=lower, ymax = upper)) 

ggplot(data =d) +
  geom_linerange(aes(x=time, y = y))
?geom_pointrange
## one_iter
ind_totalID=4
pre_totalT=2
post_totalT=3
alpha = 10
beta = 5
# preT = 1:pre_totalT
# postT=1:post_totalT
time = rep(1:(pre_totalT+post_totalT), each = ind_totalID)
ind_ID = rep(1:ind_totalID, pre_totalT + post_totalT)
n = 1:length(ind_ID)
treat = sample(c(0,1), size = ind_totalID, replace=TRUE, prob=c(0.5, 0.5))


alpha_noise = rnorm(ind_totalID, 0,1)
beta_noise_pre = rep(0, each=pre_totalT*ind_totalID)
# beta_noise_post = rep(seq(1,10, length.out=post_totalT), 
#                  each = ind_totalID)
beta_noise_post = rep(sapply(1:post_totalT, function(x){rnorm(1, 2*x, 0)}), each = ind_totalID)
# 2*time
# beta_noise_post = rep(sample(c(-1, 2, 5)))
beta_noise = c(beta_noise_pre, beta_noise_post)
DT = data.table(n, treat, time, ind_ID, alpha, 
                alpha_noise, beta, beta_noise, beta_noise_post)
DT[, c("alpha_ind", "beta_ind") := 
     .(alpha+alpha_noise, beta+beta_noise), by = ind_ID]
DT[,var2 := beta+rnorm(1, 2*time, 3*time),by = qF(DT$time)]
DT

# Step 2: Produce an event study figure

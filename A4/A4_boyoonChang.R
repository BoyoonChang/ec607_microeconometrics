if (!require(SimDesign)) install.packages("SimDesign")
library(SimDesign)
install.packages("clusterSEs")
library(clusterSEs)
install.packages('multiwayvcov')
library(multiwayvcov)
install.packages("miceadds")
library(miceadds)

###############################
# before formulating a function
###############################
rho = 0
Sigma_i = matrix(c(1, 0, 0, 1 - rho), ncol = 2)
Sigma_i
n = 30
values_i = rmvnorm(n = n, mean =c(0,0), sigma = Sigma_i)# create n number of two vars with Sigma_i
var(values_i[,1]) # the variance of the first variable
mean(values_i[,1]) # mean of the first variable
var(values_i[,2]) # variance of the second var
mean(values_i[,2]) # mean of the second var
cov(values_i[,1], values_i[,2]) # covariance between first and second var
mean((values_i[,1]-mean(values_i[,1]))*(values_i[,2]-mean(values_i[,2])))
# what does rmvnorm do?
?rmvnorm
sigma <- matrix(c(3,2,2,6), 2, 2)
mu <- c(5,10)
x <- rmvnorm(1000, mean = mu, sigma = sigma)
head(x)
summary(x)
plot(x[,1], x[,2])
#### it creates two variables whose variance and covariance is sigma

# cluster level
rho = 1
n_cluster=10
cluster_name = rep(1:n_cluster, each = n / n_cluster)
id_in_cluster = sequence(rle(cluster_name)$lengths)
Sigma_cl = matrix(c(1, 0, 0, rho), ncol = 2)
values_cl = rmvnorm(n = n_cluster, mean = c(0,0), sigma = Sigma_cl)
values_cl
var(values_cl[,1]) # the variance of the first variable
mean(values_cl[,1]) # mean of the first variable
var(values_cl[,2]) # variance of the second var
mean(values_cl[,2]) # mean of the second var
cov(values_cl[,1], values_cl[,2]) # covariance between first and second var

# variables for i and c respectively
cbind(values_i, rep(values_cl[,1], each = n / n_cluster), rep(values_cl[,2], each = n/n_cluster))
# error consists of individual- and cluster-level components
x <- values_i[ , 1] + rep(values_cl[ , 1], each = n / n_cluster)
error <- values_i[ , 2] + rep(values_cl[ , 2], each = n / n_cluster)
cbind(values_i, rep(values_cl[,1], each = n / n_cluster), rep(values_cl[,2], each = n/n_cluster), x, error)

# treatment varying across i within c
t_i = sample(c(1,0), n, replace = TRUE)
# treatment varying across c 
t_c = rep(sample(c(1,0), n_cluster, replace = TRUE), each = n/n_cluster)
cbind(values_i, 
      rep(values_cl[,1], each = n / n_cluster), 
      rep(values_cl[,2], each = n/n_cluster), 
      x, 
      error,
      t_i,
      t_c,
      cluster_name,
      id_in_cluster)

# data generating process
test_data= data.frame(var1_ind = values_i[,1],
                      var2_ind = values_i[,2],
                      var1_cl = rep(values_cl[,1], each = n / n_cluster),
                      var2_cl = rep(values_cl[,2], each = n/n_cluster),
                      x,
                      error,
                      t_i,
                      t_c,
                      cluster = cluster_name,
                      id_in_cluster,
                      y = 1 + x*t_i + error 
)

m = lm(y ~ t_i, data = test_data)
a = coeftest(m)[2,2] # std error of beta
coeftest(m, vcov=cluster.vcov(m, test_data$cluster, df_correction = F))[2,2]
###############################
###############################

###############################
##  formulating a function ####
###############################

# function to generate a cluster
gen_cluster = function(rho) {
  n = 100
  n_cluster=20
  # individual level 
  Sigma_i = matrix(c(1, 0, 0, 1 - rho), ncol = 2)
  values_i = rmvnorm(n = n, mean = c(1,0), sigma = Sigma_i)
  # cluster level
  cluster_name = rep(1:n_cluster, each = n / n_cluster)
  id_in_cluster = sequence(rle(cluster_name)$lengths)
  Sigma_cl = matrix(c(1, 0, 0, rho), ncol = 2)
  values_cl = rmvnorm(n = n_cluster, mean = c(0,0), sigma = Sigma_cl)
  # error consists of individual- and cluster-level components
  x <- values_i[ , 1] + rep(values_cl[ , 1], each = n / n_cluster)
  error <- values_i[ , 2] + rep(values_cl[ , 2], each = n / n_cluster)
  # data generating process
  # treatment varying across i within c
  t_i = sample(c(1,0), n, replace = TRUE)
  # treatment varying across c 
  t_c = rep(sample(c(1,0), n_cluster, replace = TRUE), each = n/n_cluster)
  y = 1 + error + 2 * x * t_i
  df = data.frame(var1_ind = values_i[,1],
                  var2_ind = values_i[,2],
                  var1_cl = rep(values_cl[,1], each = n / n_cluster),
                  var2_cl = rep(values_cl[,2], each = n/n_cluster),
                  x,
                  error,
                  t_i,
                  t_c,
                  cluster = cluster_name,
                  id_in_cluster,
                  y)
  return(df)
}


cluster_list=sapply(seq(0,1, by =0.1), gen_cluster)

a = gen_cluster(rho=1)
b = gen_cluster(rho=0.5)
c = gen_cluster(rho=0)

ols_a = lm(y ~ t_i, data = a)
ols_a
ols_b = lm(y ~ t_i, data = b)
ols_c = lm(y ~ t_i, data = c)

coeftest(ols_a, vcov=cluster.vcov(ols_a, ))

coeftest(m, vcov=cluster.vcov(m, test_data$cluster, df_correction = F))


cluster_sim = function(rho){
  d = gen_cluster(rho=rho)
  m = lm(y~t_i, data =d)
  m_c = lm.cluster(y~t_i, cluster='cluster', data = d)
  b1 = coef(m)[2]
  b2 = coef(m_c)[2]
  # standard error + ci without adjustment
  se_nc = coeftest(m)[2,2]
  b1_ci_nc = confint(m)[2,]
  # clustered standard error + ci
  se_c = coeftest(m, vcov=cluster.vcov(m, d$cluster, df_correction = F))[2,2]
  t_cv = qt(0.025, df = n-2, lower.tail = FALSE)
  lower = b2 - t_cv*se_c
  upper = b2 + t_cv*se_c
  b1_ci_c = c(lower, upper)
  return(c( beta = b1, beta_c = b2, se_nc = se_nc, b1_ci_nc, se_c = se_c, b1_ci_c))
}

sapply(seq(0,1, by =0.1), cluster_sim)
df = replicate(100, sapply(seq(0,1, by =0.1), cluster_sim))

df = replicate(100, cluster_sim(rho = 1))

run_cluster_sim <- function(rho) {
  n_sims=1000
  df = replicate(n_sims, cluster_sim(rho = rho))
  df = as.data.frame(t(df))
  names(df) <- c('beta', 'beta_cluster', 
                 'se', 'ci95_lower', 'ci95_upper', 
                 'se_cluster', 'ci95_lower_cluster', 'ci95_upper_cluster')
  df = df %>% mutate(id = 1:n())
  return(df)
}

list = sapply(seq(0,1, by =0.1), run_cluster_sim)
# p_list=list()
# for (i in 1:ncol(list)){
#   beta = list[,i]$beta
#   p_list[[i]] = hist(beta)
# }
# p_list
# test= do.call(cbind, list)
# install.packages("reshape2")
# library(reshape2)
# test.melt  = melt(test)
data = tibble(beta = unlist(list[,1]$beta), 
              se = unlist(list[,1]$se),
              ci_l = unlist(list[,1]$ci95_lower),
              ci_h = unlist(list[,1]$ci95_upper),
              se_c = unlist(list[,1]$se_cluster),
              ci_lc = unlist(list[,1]$ci95_lower_cluster),
              ci_hc = unlist(list[,1]$ci95_upper_cluster),
              id = unlist(list[,1]$id),
              i = 0)
for (i in 2:ncol(list)){
  extra_data = tibble(beta = unlist(list[,i]$beta),
                      se = unlist(list[,i]$se),
                      ci_l = unlist(list[,i]$ci95_lower),
                      ci_h = unlist(list[,i]$ci95_upper),
                      se_c = unlist(list[,i]$se_cluster),
                      ci_lc = unlist(list[,i]$ci95_lower_cluster),
                      ci_hc = unlist(list[,i]$ci95_upper_cluster),
                      id = unlist(list[,i]$id),
                      i = (i-1)*0.1) # rho = 0
  data = rbind(data, extra_data)
}
data$i = as.factor(data$i)

# beta
ggplot(data, aes(beta, color = i)) + geom_density() + geom_vline(xintercept = 2, linetype = "dashed")

# confidence interval
library(ggthemes)
sample_n(data %>% group_by(i),100)
ggplot(sample_n(data %>% group_by(i),100))+
  aes(x = reorder(id, beta), y = beta,
      ymin= ci_l, ymax=ci_h, color = i)+
  geom_pointrange() + coord_flip()
ggplot(sample_n(data %>% group_by(i), 100)) +
  geom_hline(yintercept = 2, linetype="dashed")+
  geom_pointrange(aes(x = reorder(id,beta), y = beta,
                      ymin= ci_l, ymax=ci_h), color = "red", alpha = 0.3) + 
  geom_pointrange(aes(x = reorder(id, beta), y = beta, 
                      ymin= ci_lc, ymax = ci_hc), color = "blue", alpha = 0.3) +
  theme(axis.text.y=element_blank(), axis.ticks = element_blank())+
  coord_flip() + facet_wrap(vars(i)) + xlab("")

# stat
sample = sample_n(data %>% group_by(i),100)
flag = list()
flag_c= list()
for (n in 1:11){
  a = (n-1)*0.1
  sub_sample = sample %>% filter(i == a) 
  flag[[n]] = 1-mean(ifelse(sub_sample$ci_l <= 2 & sub_sample$ci_h >=2, 1, 0))
  flag_c[[n]] = 1- mean(ifelse(sub_sample$ci_lc <= 2 & sub_sample$ci_hc >=2, 1, 0))
  
}
flag
flac_c

# sample$flag = ifelse(sample$ci_l <= 2 & sample$ci_h >=2, 1, 0)
# 1-mean(sample$flag)
# sample$flag_c = ifelse(sample$ci_lc <= 2 & sample$ci_hc >=2, 1, 0)
# 1-mean(sample$flag_c)
# 
# 
# # plotting
# cluster_rho0=run_cluster_sim(n_sim=1000, rho=0)
# cluster_rho0.5=run_cluster_sim(n_sim=1000, rho=0.5)
# cluster_rho1=run_cluster_sim(n_sim=1000, rho=1)
# ggplot(cluster_rho0, aes(x=beta))+
#   geom_histogram(color='black')
# ggplot(cluster_rho0.5, aes(x=beta))+
#   geom_histogram(color='black')
# ggplot(cluster_rho1, aes(x=beta))+
#   geom_histogram(color='black')
# 
# 
# run_cluster_sim(n_sim=50, rho=0)
# ggplot(run_cluster_sim(n_sim=50, rho=0),
#        aes(x = reorder(id, b1), y = b1, 
#            ymin = ci95_lower, ymax = ci95_upper,
#            color = param_caught)) +
#   geom_hline(yintercept = sim_params[2], linetype = 'dashed') +
#   geom_pointrange() +
#   labs(x = 'sim ID', y = 'b1', title = 'Randomly Chosen 100 95% CIs') +
#   scale_color_discrete(name = 'True param value', labels = c('missed', 'hit')) +
#   coord_flip()
# run_cluster_sim(n_sim=50, rho=0.5)
# run_cluster_sim(n_sim=50, rho=1)
# 
# 
# # cluster_sim = function(rho, cluster_robust=FALSE){
# #   d = gen_cluster(rho=rho)
#   m = lm(y~t_i, data =d)
#   # m_nc = m
#   m_c = lm.cluster(y~t_i, cluster='cluster', data = d)
#   b1 = coef(m)[2]
#   b2 = coef(m_c)[2]
#   coeftest(m)
#   coeftest(m, vcov=cluster.vcov(m, d$cluster, df_correction = F))
#   if (cluster_robust==FALSE){
#     Sigma = vcov(m)
#     se = sqrt(diag(Sigma)[2])
#     b1_ci = confint(m)[2,]
#   } else{
#     Sigma = cluster.vcov(m, ~cluster)
#     se = sqrt(diag(Sigma)[2])
#     t_cv = qt(0.025, df = n-2, lower.tail = FALSE)
#     lower = b1 - t_cv*se
#     upper = b1 + t_cv*se
#     b1_ci = c(lower, upper)
#   }
#   return(c(beta_c = b2, beta = b1, se = se, b1_ci))
# }

# d = gen_cluster(rho=0.5)
# mod = lm(y~t, data = d)
# lm.cluster(y~t, cluster='cluster', data =d)
# b1=coef(mod)[2]
# se=sqrt(diag(vcov(mod))[2])
# b1_ci = confint(mod)[2,]
# c(beta = b1, se = se, b1_ci)
# vcov(mod)
# cluster.vcov(mod, ~cluster)
# 
# 

# cluster_sim(rho=1, cluster_robust=FALSE)
# lapply(10, cluster_sim(rho=1, cluster_robust=FALSE))
# replicate(10, cluster_sim(rho=1, cluster_robust=FALSE))
# replicate(10, cluster_sim(rho=0.5, cluster_robust=FALSE))
# replicate(10, cluster_sim(rho=0, cluster_robust=FALSE))
# 

# df = replicate(100, cluster_sim(rho = 1, cluster_robust = FALSE))
# 
# run_cluster_sim <- function(n_sims = 100, rho, cluster_robust = FALSE) {
#   df = replicate(n_sims, cluster_sim(rho = rho, cluster_robust = cluster_robust))
#   df = as.data.frame(t(df))
#   names(df) <- c('beta', 'beta_cluster', 'se', 'ci95_lower', 'ci95_upper')
#   return(df)
# }

# sim_df_nc = run_cluster_sim(n_sim=100, rho=0.5, cluster_robust=FALSE)
# run_cluster_sim(n_sim=100, rho=0, cluster_robust=FALSE)
# run_cluster_sim(n_sim=100, rho=0.5, cluster_robust=FALSE)
# run_cluster_sim(n_sim=100, rho=1, cluster_robust=FALSE)
# run_cluster_sim(n_sim=100, rho=0, cluster_robust=TRUE)
# run_cluster_sim(n_sim=100, rho=0.5, cluster_robust=TRUE)
# run_cluster_sim(n_sim=100, rho=1, cluster_robust=TRUE)
# 
# library(ggplot2)
# ggplot() + 
#   geom_density(aes(sim_df_nc$beta), color = 'black', data = sim_df_nc) +
#   geom_density(aes(sim_df_cl$beta), color = 'blue', data = sim_df_cl)
# sim_df_cl = run_cluster_sim(n_sim=100, rho=0.5, cluster_robust=TRUE)
# 
# replicate(100, cluster_sim(n = 100, rho = 0.5,
#                               n_cluster = 20,
#                               cluster_robust = cluster_robust))
# ci95_nocluster <- ggplot(sample_n(sim_nocluster, 100),
#                          aes(x = reorder(id, b1), y = b1, 
#                              ymin = ci95_lower, ymax = ci95_upper,
#                              color = param_caught)) +
#   geom_hline(yintercept = sim_params[2], linetype = 'dashed') +
#   geom_pointrange() +
#   labs(x = 'sim ID', y = 'b1', title = 'Randomly Chosen 100 95% CIs') +
#   scale_color_discrete(name = 'True param value', labels = c('missed', 'hit')) +
#   coord_flip()
# print(ci95_nocluster)
# 
# ## Are coefficients same?
# 
# gen_cluster = function(n = 100, n_cluster = 20, rho1, rho2) {
#   # individual level 
#   Sigma_i1 = matrix(c(1, 0, 0, 1 - rho1), ncol = 2)
#   Sigma_i2 = matrix(c(1, 0, 0, 1 - rho2), ncol = 2)
#   values_i1 = rmvnorm(n = n, sigma = Sigma_i1)
#   values_i2 = rmvnorm(n = n, sigma = Sigma_i2)
#   # cluster level
#   n_cluster=10
#   cluster_name = rep(1:n_cluster, each = n / n_cluster)
#   id_in_cluster = sequence(rle(cluster_name)$lengths)
#   Sigma_cl1 = matrix(c(1, 0, 0, rho1), ncol = 2)
#   values_cl1 = rmvnorm(n = n_cluster, sigma = Sigma_cl1)
#   Sigma_cl2 = matrix(c(1, 0, 0, rho2), ncol = 2)
#   values_cl2 = rmvnorm(n = n_cluster, sigma = Sigma_cl2)
#   
#   # error consists of individual- and cluster-level components
#   # e1 <- values_i[ , 1] + rep(values_cl[ , 1], each = n / n_cluster)
#   e21 <- values_i1[ , 2] + rep(values_cl1[ , 2], each = n / n_cluster)
#   e22 <- values_i2[ , 2] + rep(values_cl2[ , 2], each = n / n_cluster)
#   
#   # data generating process
#   # y <- 1 + e1 + e2 
#   # treatment varying across i within c
#   t_i = sample(c(1,0), n, replace = TRUE)
#   # treatment varying across c 
#   t_c = rep(sample(c(1,0), n_cluster, replace = TRUE), each = n/n_cluster)
#   # # control
#   # y_c <- y
#   # # treated
#   # y_t <- 
#   # # assignment of treatment
#   # y_n = ifelse(d=1, y_t, y_c)
#   # # dataframe
#   # # 
#   t = t_i*t_c
#   y1 = 1 + e21 + t*10
#   y2 = 1 + e22 + t*10
#   df = data.frame(cluster = cluster_name, e21, e22, y1, y2,t, id_in_cluster)
#   return(df)
# }
# 
# df = gen_cluster(rho1=0, rho2=1)
# lm(y1~t, data =df)
# lm(y2~t, data =df)
# 
# 
# # simulation
# gen_cluster = function(n, n_cluster){
#   cluster_name = rep(1:n_cluster, each = n/n_cluster)
#   id = sequence(rle(cluster_name)$lengths)
#   df <- data.frame(cluster=cluster_name, id)
#   return(df)
# }
# gen_cluster(10,2)

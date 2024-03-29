---
title: "Assignment 4"
author: "Boyoon Chang"
date: "March 27, 2021"
output: 
  html_document: 
    code_folding: hide
    theme: flatly
    highlight: tango
    toc_float:
      collapsed: yes
      smooth_scroll: yes
    keep_md: true
---



## A4 - Standard errors and clustering

> Due date: 9 February 2021

---

First, simulate a DGP in which there is a panel of economic units (_e.g._, firms, students, countries) that can be organized into "clusters" of some kind (_e.g._, industry, classrooms, years). In addition to the _iid_ variation in _y_ across _ic_, also give yourself the ability to control sources of co-variation in _y_ that are specific to economic units _i_ and/or specific to clusters _c_. 

Here's a bit of code that might have some harvestable stuff in it to get you started: 

```r
gen_cluster <- function(n = 1000, n_cluster = 20, rho) {
  # individual level 
  Sigma_i <- matrix(c(1, 0, 0, 1 - rho), ncol = 2)
  values_i <- rmvnorm(n = n, sigma = Sigma_i)
  # cluster level
  cluster_name <- rep(1:n_cluster, each = n / n_cluster)
  Sigma_cl <- matrix(c(1, 0, 0, rho), ncol = 2)
  values_cl <- rmvnorm(n = n_cluster, sigma = Sigma_cl)
  # error consists of individual- and cluster-level components
  e1 <- values_i[ , 1] + rep(values_cl[ , 1], each = n / n_cluster)
  e2 <- values_i[ , 2] + rep(values_cl[ , 2], each = n / n_cluster)
  # data generating process
  y <- 1 + e1 + e2 
  df <- data.frame(cluster = cluster_name, e1, e2, y)
  return(df)
}
```

Second, introduce the potential for treatment (i) at the individual level (varying across _i_ within _c_), and (ii) at the cluster level (varying across _c_). As part of that DGP, assign `rho` as the intra-cluster correlation with a parameter. (Thus, when `rho=1`, all units within a cluster are identical, and the effective sample size is reduced to the number of clusters. Likewise, when `rho=0`, there is no correlation of units within a cluster, and all observations are independent of each other.)

Third, demonstrate that $\hat\beta$ in the following 

$$y_{ic} = \alpha + \beta \mathbb{1}(T_{ic}=1) + e_{ic}~,$$
is not sensitive to `rho` but that the confidence intervals are, and are corrected by estimating cluster-robust standard errors. 

<br>

#### Model and Introducing Treatment

I have formulated a model where true $\beta$, i.e. treatment effect, varies across individual-level and cluster-level but the average treatment effect for the sample equals 2. Treatment is introduced either at the individual level or at the cluster level. The probability of an observation or a cluster being assigned to treatment is 0.5. The sample size is 100 with 20 clusters; thus, there are 50 observations for each cluster. I have performed 1000 iterations to see the asymptotic properties of the point estimate of $\beta$. See below for the results.

<br>

#### Distribution of parameter estimate


```r
if (!require(SimDesign)) install.packages("SimDesign")
if (!require(clusterSEs)) install.packages("clusterSEs")
if (!require(multiwayvcov)) install.packages("multiwayvcov")
if (!require(miceadds)) install.packages("miceadds")
library(clusterSEs)
library(SimDesign)
library(miceadds)
library(multiwayvcov)
library(ggthemes)
library(ggplot2)
library(gridExtra)

n = 100
n_cluster=20

gen_cluster = function(rho) {
  # individual level 
  Sigma_i = matrix(c(1, 0, 0, 1 - rho), ncol = 2)
  values_i = rmvnorm(n = n, mean = c(1,0), sigma = Sigma_i)
  # cluster level
  cluster_name = rep(1:n_cluster, each = n / n_cluster)
  id_in_cluster = sequence(rle(cluster_name)$lengths)
  Sigma_cl = matrix(c(1, 0, 0, rho), ncol = 2)
  values_cl = rmvnorm(n = n_cluster, mean = c(0,0), sigma = Sigma_cl)
  # x and error consists of individual- and cluster-level components
  x <- values_i[ , 1] + rep(values_cl[ , 1], each = n / n_cluster)
  error <- values_i[ , 2] + rep(values_cl[ , 2], each = n / n_cluster)
  # data generating process
  # treatment varying across i within c
  t_i = sample(c(1,0), size = n, prob=c(0.5, 0.5), replace = TRUE)
  # treatment varying only across c 
  t_c = rep(sample(c(1,0), size = n_cluster, prob=c(0.5, 0.5), replace = TRUE), each = n/n_cluster)
  # outcome variable depending on treatment
  y = 1 + error + 2 * x * t_i
  y_tc = 1 + error + 2 * x * t_c
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
                  y,
                  y_tc)
  return(df)
}
model_sim = function(rho){
  d = gen_cluster(rho=rho)
  m = lm(y~t_i, data =d)
  m_c = lm.cluster(y~t_i, cluster='cluster', data = d)
  m_tc = lm(y_tc ~ t_c, data = d)
  m_c_tc = lm.cluster(y_tc ~ t_c, cluster='cluster', data = d)
  b1 = coef(m)[2]
  b2 = coef(m_c)[2]
  b3 = coef(m_tc)[2]
  b4 = coef(m_c_tc)[2]
  # standard error + ci without adjustment
  se_nc = coeftest(m)[2,2]
  b1_ci_nc = confint(m)[2,]
  se_nc_tc = coeftest(m_tc)[2,2]
  b1_ci_nc_tc = confint(m_tc)[2,]
  # clustered standard error + ci
  se_c = coeftest(m, vcov=cluster.vcov(m, d$cluster, df_correction = F))[2,2]
  se_c_tc = coeftest(m_tc, vcov=cluster.vcov(m_tc, d$cluster, df_correction = F))[2,2]
  t_cv = qt(0.025, df = (n-2), lower.tail = FALSE)
  lower = b2 - t_cv*se_c
  upper = b2 + t_cv*se_c
  lower_tc = b4 - t_cv*se_c_tc
  upper_tc = b4 + t_cv*se_c_tc
  b1_ci_c = c(lower, upper)
  b1_ci_c_tc = c(lower_tc, upper_tc)
  return(c( beta = b1, beta_c = b2, 
            se_nc = se_nc, b1_ci_nc, 
            se_c = se_c, b1_ci_c,
            beta_tc = b3, beta_c_tc = b4,
            se_nc_tc = se_nc_tc, b1_ci_nc_tc = b1_ci_nc_tc,
            se_c_tc = se_c_tc, b1_ci_c_tc))
}

iter_sim <- function(rho) {
  n_sims=1000
  df = replicate(n_sims, model_sim(rho = rho))
  df = as.data.frame(t(df))
  names(df) <- c('beta', 'beta_cluster', 
                 'se', 'ci95_lower', 'ci95_upper', 
                 'se_cluster', 'ci95_lower_cluster', 
                 'ci95_upper_cluster',
                 'beta_tc', 'beta_cluster_tc',
                 'se_tc', 'ci95_lower_tc', 'ci95_upper_tc', 
                 'se_cluster_tc', 'ci95_lower_cluster_tc', 
                 'ci95_upper_cluster_tc')
  df = df %>% mutate(id = 1:n())
  return(df)
}
list = sapply(seq(0,1, by =0.1), iter_sim)
clusterdata = tibble(beta = unlist(list[,1]$beta), 
              se = unlist(list[,1]$se),
              ci_l = unlist(list[,1]$ci95_lower),
              ci_h = unlist(list[,1]$ci95_upper),
              se_c = unlist(list[,1]$se_cluster),
              ci_lc = unlist(list[,1]$ci95_lower_cluster),
              ci_hc = unlist(list[,1]$ci95_upper_cluster),
              beta_tc = unlist(list[,1]$beta_tc), 
              se_tc = unlist(list[,1]$se_tc),
              ci_l_tc = unlist(list[,1]$ci95_lower_tc),
              ci_h_tc = unlist(list[,1]$ci95_upper_tc),
              se_c_tc = unlist(list[,1]$se_cluster_tc),
              ci_lc_tc = unlist(list[,1]$ci95_lower_cluster_tc),
              ci_hc_tc = unlist(list[,1]$ci95_upper_cluster_tc),
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
                      beta_tc = unlist(list[,i]$beta_tc), 
                      se_tc = unlist(list[,i]$se_tc),
                      ci_l_tc = unlist(list[,i]$ci95_lower_tc),
                      ci_h_tc = unlist(list[,i]$ci95_upper_tc),
                      se_c_tc = unlist(list[,i]$se_cluster_tc),
                      ci_lc_tc = unlist(list[,i]$ci95_lower_cluster_tc),
                      ci_hc_tc = unlist(list[,i]$ci95_upper_cluster_tc),
                      id = unlist(list[,i]$id),
                      i = (i-1)*0.1) # rho = 0
  clusterdata = rbind(clusterdata, extra_data)
}
clusterdata$i = as.factor(clusterdata$i)
```



```r
# beta
p1_beta=ggplot(clusterdata, aes(beta, color = i)) + 
  geom_density() + 
  geom_vline(xintercept = 2, linetype = "dashed") + labs(fill="rho")+ 
  scale_color_discrete(name = "rho") +
  ggtitle("Treatment at Individual Level")+
  xlim(-1,5) + ylim(0, 0.8)
# beta
p2_beta=ggplot(clusterdata, aes(beta_tc, color = i)) + 
  geom_density() + 
  geom_vline(xintercept = 2, linetype = "dashed") + labs(fill="rho") + 
  scale_color_discrete(name = "rho")+
  ggtitle("Treatment at Cluster Level")+
  xlim(-1,5)+ ylim(0, 0.8)
grid.arrange(p1_beta, p2_beta, ncol=2)
```

<img src="A4_boyoonChang_files/figure-html/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

These graphs denote the density of the point estimates of $\beta$, i.e. $\hat{\beta}$ from 1000 iterations. I have varied $\rho$ to see if the degree of correlations across observations within a cluster changes the consistency of the $\hat{\beta}$. As can be seen from the graphs, it seems that the changes in $\rho$ does not have a significant influence over the unbiasedness of $\hat{\beta}$. That is, the varying degree of $\rho$ does not change the shape of the distribution nor the spike in the distribution at a value of 2. Such results are consistent with the theory. 

However, the variation of the $\hat{\beta}$ seems to be greater when the treatment falls at a cluster level than when the treatment falls at an individual level. In other words, although $\hat{\beta}$ is still unbiased, it is more likely that $\hat{\beta}$ is further away from the true $\beta$ if the treatment is introduced at a cluster level. This could be related to the way I introduced treatment effect. Recall that I designed a model to have treatment effect vary across individuals within cluster as well as across clusters. However, if selection into treatment happens only at cluster level, some of the variations that occur within cluster cannot be captured by the model estimation. Therefore, we see higher risk of seeing point estimates different from the true parameter.  

<br>

#### Ordinary Standard Error



```r
# confidence interval
p11_ci=ggplot(sample_n(clusterdata %>% filter(i %in% c(0,0.5,1)),100))+
  aes(x = reorder(id, beta), y = beta,
      ymin= ci_l, ymax=ci_h, color = i)+
  geom_pointrange() + coord_flip() +
  theme(axis.text.y=element_blank()) + xlab("")+
  geom_hline(yintercept = 2, linetype="dashed")+ labs(fill="rho") +
  ylim(-1,5)+ 
  scale_color_discrete(name = "rho")+
  ggtitle("Treatment at Individual Level")
p12_ci=ggplot(sample_n(clusterdata %>% group_by(i), 100)) +
  geom_hline(yintercept = 2, linetype="dashed")+
  geom_pointrange(aes(x = reorder(id,beta), y = beta,
                      ymin= ci_l, ymax=ci_h), color = "red", alpha = 0.3) + 
  geom_pointrange(aes(x = reorder(id, beta), y = beta, 
                      ymin= ci_lc, ymax = ci_hc), color = "blue", alpha = 0.3) +
  theme(axis.text.y=element_blank(), axis.ticks = element_blank())+
  coord_flip() + facet_wrap(vars(i), ncol=2) + xlab("")+
  ylim(-2.5,7.5)+ 
  scale_color_discrete(name = "rho")+
  ggtitle("Treatment at Individual Level")
p21_ci=ggplot(sample_n(clusterdata %>% filter(i %in% c(0,0.5,1)),100))+
  aes(x = reorder(id, beta_tc), y = beta_tc,
      ymin= ci_l_tc, ymax=ci_h_tc, color = i)+
  geom_pointrange() + coord_flip() +
  theme(axis.text.y=element_blank()) + xlab("")+
  geom_hline(yintercept = 2, linetype="dashed")+ labs(fill="rho")+ 
  scale_color_discrete(name = "rho")+
  ggtitle("Treatment at Cluster Level")
p22_ci=ggplot(sample_n(clusterdata %>% group_by(i), 100)) +
  geom_hline(yintercept = 2, linetype="dashed")+
  geom_pointrange(aes(x = reorder(id,beta_tc), y = beta_tc,
                      ymin= ci_l_tc, ymax=ci_h_tc), color = "red", alpha = 0.3) + 
  geom_pointrange(aes(x = reorder(id, beta_tc), y = beta_tc, 
                      ymin= ci_lc_tc, ymax = ci_hc_tc), color = "blue", alpha = 0.3) +
  theme(axis.text.y=element_blank(), axis.ticks = element_blank())+
  coord_flip() + facet_wrap(vars(i), ncol=2) + xlab("")+
  ylim(-2.5,7.5)+ 
  scale_color_discrete(name = "rho")+
  ggtitle("Treatment at Cluster Level")
grid.arrange(p11_ci, p21_ci, ncol=2)
```

<img src="A4_boyoonChang_files/figure-html/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />
Here, I have randomly picked 100 iterations from the total of 1000 simulations and plotted them on graphs. These graphs indicate the confidence interval for each point estimate from the randomly picked 100 simulations. It could also be seen from these two graphs that the variations of the mid points of the confidence interval, i.e. the point estimates from the simulations, are greater when the treatment falls at a cluster level than when the treatment falls at an individual level. This is consistent with the earlier density graphs.

As it is hard to tell the difference of the confidence interval across differing $\rho$, I have plotted graphs below for each differing value of $\rho$. However, I could not find an unilateral pattern in ordinary standard error either decreasing or increasing associated with an increase in $\rho$. 



```r
grid.arrange(p12_ci, p22_ci, ncol=2)
```

<img src="A4_boyoonChang_files/figure-html/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

<br>

#### Ordinary Standard Error and Clustered Standard Error


```r
# stat
sample = sample_n(clusterdata %>% group_by(i),100)
flag = list()
flag_c= list()
flag_tc = list()
flag_c_tc= list()
for (n in 1:11){
  a = (n-1)*0.1
  sub_sample = sample %>% filter(i == a) 
  flag[[n]] = 1-mean(ifelse(sub_sample$ci_l <= 2 & sub_sample$ci_h >=2, 1, 0))
  flag_c[[n]] = 1- mean(ifelse(sub_sample$ci_lc <= 2 & sub_sample$ci_hc >=2, 1, 0))
  flag_tc[[n]] = 1-mean(ifelse(sub_sample$ci_l_tc <= 2 & sub_sample$ci_h_tc >=2, 1, 0))
  flag_c_tc[[n]] = 1- mean(ifelse(sub_sample$ci_lc_tc <= 2 & sub_sample$ci_hc_tc >=2, 1, 0))

}
reject_rate = tibble(reject_rate = unlist(flag), 
                     reject_rate_cluster=unlist(flag_c),
                     rejection_rate_tc=unlist(flag_tc),
                     rejection_rate_cluster_tc=unlist(flag_c_tc),
                     rho = seq(0, 1, by=0.1))
kable(reject_rate[,c(1,2,5)],
      caption = "Rejection Rate \n(Treatment at individual level)",
      col.names = c("Ordinary SE", "Clustered SE", "rho")) %>%
  kable_styling(bootstrap_options="striped", full_width = FALSE)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Rejection Rate 
(Treatment at individual level)</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> Ordinary SE </th>
   <th style="text-align:right;"> Clustered SE </th>
   <th style="text-align:right;"> rho </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0.10 </td>
   <td style="text-align:right;"> 0.06 </td>
   <td style="text-align:right;"> 0.0 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> 0.09 </td>
   <td style="text-align:right;"> 0.1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.19 </td>
   <td style="text-align:right;"> 0.06 </td>
   <td style="text-align:right;"> 0.2 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.18 </td>
   <td style="text-align:right;"> 0.11 </td>
   <td style="text-align:right;"> 0.3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.17 </td>
   <td style="text-align:right;"> 0.12 </td>
   <td style="text-align:right;"> 0.4 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.21 </td>
   <td style="text-align:right;"> 0.12 </td>
   <td style="text-align:right;"> 0.5 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.14 </td>
   <td style="text-align:right;"> 0.09 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.12 </td>
   <td style="text-align:right;"> 0.06 </td>
   <td style="text-align:right;"> 0.7 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.14 </td>
   <td style="text-align:right;"> 0.11 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.19 </td>
   <td style="text-align:right;"> 0.07 </td>
   <td style="text-align:right;"> 0.9 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.16 </td>
   <td style="text-align:right;"> 0.10 </td>
   <td style="text-align:right;"> 1.0 </td>
  </tr>
</tbody>
</table>

```r
kable(reject_rate[,c(3,4,5)],
      caption = "Rejection Rate \n(Treatment at cluster level)",
      col.names = c("Ordinary SE", "Clustered SE", "rho")) %>%
  kable_styling(bootstrap_options="striped", full_width = FALSE)
```

<table class="table table-striped" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Rejection Rate 
(Treatment at cluster level)</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> Ordinary SE </th>
   <th style="text-align:right;"> Clustered SE </th>
   <th style="text-align:right;"> rho </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0.19 </td>
   <td style="text-align:right;"> 0.09 </td>
   <td style="text-align:right;"> 0.0 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.22 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.24 </td>
   <td style="text-align:right;"> 0.11 </td>
   <td style="text-align:right;"> 0.2 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.30 </td>
   <td style="text-align:right;"> 0.12 </td>
   <td style="text-align:right;"> 0.3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.26 </td>
   <td style="text-align:right;"> 0.08 </td>
   <td style="text-align:right;"> 0.4 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.33 </td>
   <td style="text-align:right;"> 0.11 </td>
   <td style="text-align:right;"> 0.5 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.21 </td>
   <td style="text-align:right;"> 0.04 </td>
   <td style="text-align:right;"> 0.6 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.32 </td>
   <td style="text-align:right;"> 0.05 </td>
   <td style="text-align:right;"> 0.7 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.33 </td>
   <td style="text-align:right;"> 0.10 </td>
   <td style="text-align:right;"> 0.8 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.30 </td>
   <td style="text-align:right;"> 0.10 </td>
   <td style="text-align:right;"> 0.9 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.35 </td>
   <td style="text-align:right;"> 0.15 </td>
   <td style="text-align:right;"> 1.0 </td>
  </tr>
</tbody>
</table>

When comparing two extreme cases of $\rho=0$, i.e. no correlation between individuals within cluster, and $\rho=1$, i.e. perfect correlation between individuals within cluster, it seems that the rate of true parameter being outside of the confidence interval is higher under $\rho=1$ using ordinary standard error. However, such inflated standard error of the point estimate is well adjusted for by using clustered standard error. As can be seen from the above tables, the rate in which the confidence interval includes true parameter is now around 95$\%$, as has been initially specified. (The above tables show the rate that true parameter falls outside the confidence interval.)






<br>

---

[GlenWaddell.com](http://GlenWaddell.com)



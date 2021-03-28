---
title: "Micro-metrics, [Glen Waddell](https://glenwaddell.com)"
author: Boyoon Chang
date: "Winter 2020"
#date: "<br>27 March 2021"
header-includes:
  - \usepackage{mathtools}
  - \DeclarePairedDelimiter\floor{\lfloor}{\rfloor}
  - \usepackage{amssymb}
output: 
  html_document:
    toc: false
    toc_depth: 3  
    number_sections: false
    theme: flatly
    highlight: tango  
    toc_float:
      collapsed: true
      smooth_scroll: true
    keep_md: true
---




> Can I propose something like each of is having a master file... like this one... that we each use to collect our thoughts as we work through the next ten weeks? 



# {.tabset .tabset-fade .tabset-pills}


## A2 - Estimators

> Due date: 22 January 2020

_For each scenario below, first describe the intuition of the estimator---what is being compared, what problems it fixes, why it works. Second, the identification strategy, inclusive of the assumption required for the identification of a causal parameter.  Third, provide an example of the estimator in use, inclusive of the specific assumptions that would be operational in that example. (Examples can be from existing literature, or from your own research programme.) Fourth, comment on any particularly relevant considerations to be made with respect to the estimation of standard errors in each environment. (There may not be.) Responses will be evaluated based on accuracy, completeness and clarity._

---

A difference estimate of the effect of $\mathbb{1}(T_i=1)$ on $Y_i$.

- The intuition behind it: Suppose random selection of subjects to treatment group and control group. The difference estimator compares the average outcome of the treated group with the average outcome of the control group to get the average causal treatment effect. It is the difference in average outcome between the two groups.  

$$
\begin{aligned}
\textrm{A difference estimator}
&= 
E(Y_{1i}|T_i=1) - E(Y_{0i}|T_i=0)\\
&= E(Y_{1i}|T_i=1) - E(Y_{0i}|T_i=1) + E(Y_{0i}|T_i=1)- E(Y_{0i}|T_i=0)\\
&= E(Y_{1i}-Y_{0i}|T_i=1) + E(Y_{0i}|T_i=1)- E(Y_{0i}|T_i=0)\\
&= \textrm{average treatment effect on the treated} + \textrm{selection bias (pre-treatment)}\\
\end{aligned}
$$

- The identification assumption(s): 
(a)  There is no selection bias in the absence of the treatment. Some individual characteristics do not govern the placement of the treatment, i.e., individuals are randomly selected to the treated or control group. This allows for the control group to be used as the counter-factual for the treated group ($E(Y_{0i}|T_i=1)=E(Y_{0i}|T_i=0)$). 
(b)  It assumes that there are no other reasons except the treatment for the change in mean outcomes before and after the treatment. That is, the treatment stands for the impact of all factors that are different between the two groups. 
(c)  The average outcome of the treated group remains constant throughout earlier and later periods of the treatment. The average outcome of the control group remains constant before and after the treatment. We don't consider the time component in difference estimator.  


- An example: Provided that the control group and treatment group select into treatment randomly, difference estimator in RCT setting could provide average causal treatment effect.

- Anything particular about standard errors: We may want to adjust for standard error if we see the variation of the outcome in treatment group different from the variation of the outcome in control group. However, here we may have too few clusters (two clusters), which could be problematic. 


<br>
A matching-type estimator of the effect of  $\mathbb{1}(T_i=1)$ on $Y_i$.

- The intuition behind it: Suppose the selection into treatment is non-random but is well approximated by observable covariates ($X$) or by $f(X)$. A matching-type estimator first pairs up the individuals from the treated and control group that have similar distributions on the covariates. Then for each matching pair, treatment effect is calculated by taking the difference in the outcome. Lastly, by taking the average of those differences, we can reasonably argue that a matching-type estimate of the treatment effect is the average causal treatment effect. A matching-type estimator helps to mitigate the fundamental problem of causal inference by using an individual with similar characteristics as a counterfactual for the treated individual. 

- The identification assumption(s): 
(a) Treated and control group share the same covariates and these covariates are observable.
(b) The selection into treatment is non-random and is well approximated by $X$ or $f(X)$.
(c) Conditional independence assumption (CIA): Conditional on observed characteristics $X$, the assignment of treatment is independent of the treatment-specific outcomes so that selection bias disappears. Recall CIA is satisfied if $X$ includes all variables that affect both selection into treatment and outcomes. 
(d) Overlap assumption: Conditional on observed characteristics $X$, the probability of being assigned to treated group is in between 0 and 1 ($Pr(T=1|X=x') \in (0,1)$). This implies that the probability of observing individuals in control group at each level of $X$ is positive. Notice that if $Pr(T=1|X=x')=1$, the there is no good matching individual in control group to be used as a counterfactual.

- An example: Nearest-neighbor matching, propensity score methods are some of the matching estimators. 

- Anything particular about standard errors: If variation in outcome across matching groups are different, we may account for it by using clustered standard error.


<br>
A difference-in-difference estimate of the effect of  $\mathbb{1}(T_i=1)$ on $Y_i$. 

- The intuition behind it: Difference-in-difference estimator is the time dimension added version of difference estimator and is used to estimate the average treatment effect. By including time dimension, the estimator addresses serial correlation of the outcomes. A difference-in-difference estimator compares the differences in average outcome in the treated group and control group across time. Suppose that the treated and control group share the same time trend. The difference-in-difference estimator isolates such trend component from the outcomes of the treated individuals. It does so by subtracting the differences in post-treatment and pre-treatment means of the control group from the difference in the post-treatment and pre-treatment means of the treated group. Note that this process also removes unobservable characteristics that are fixed across time within each group that could have contributed to the difference in outcome of the two groups.   

- The identification assumption(s): All the OLS model assumptions apply for DiD if we run a regression for DiD estimator. In addition, DiD assumes parallel trend assumption. This implies that both treatment and control group share the same trend over time and thus their difference in outcome is constant if treatment did not occur. 

- An example: Card and Krueger (1994) article about change in minimum wage on employment in the fast food sector. Having the change in employment in Pennsylvania to serve as a base, it controls for any bias caused by variables common to New Jersey and Pennsylvania. A simple statistical formulation of the model is $Y_{it} = \beta_0 + \beta_1 S_t + \beta_2 T_i + \beta_3 (S_t\times T_i) + \varepsilon_{it}$, where $S_t$ is time dummy ( $S_t = 0$ if $t=1$ and $S_t=1$ if $t=2$), and $T_i$ is the treatment dummy. The difference-in-difference estimate would then be $\beta_4$ which represents $(E(Y_{i2}|T_i=1)- E(Y_{i1}|T_i=1))-(E(Y_{i2}|T_i=0)- E(Y_{i1}|T_i=0))$. 

- Anything particular about standard errors: It is helpful to use robust standard errors to account for autocorrelation or correlation within identical group before and after the treatment. 


<br>
Instrumenting for an endogenous regressor, $X_i$, with an instrument, $Z_i$, in order to retrieve an estimate of the causal effect of $X_i$ on $Y_i$. 

- The intuition behind it: We control for the bad variation of $X_i$ by regressing it with respect to $Z_i$. We use the fitted $X_i$ to estimate the causal effect of $X_i$ on $Y_i$. IV methods solve for bias from measurement error in regression models. Recall that a regression coefficients is biased toward zero if the regressors contain large noise or measurement error. IV also solves for omitted variable bias in a sense that it isolates irrelevant variation of the endogenous regressors. 

- The identification assumption(s): The instruments should be relevant in that its variation must be related to the variation in the instrumented variable $X_i$. The instruments have no effect on $Y_i$ except through $X_i$. The instruments should be exogenous or predetermined in that they are uncorrelated with the disturbances.  

- An example: Consider a case where we are interested in the returns on schooling. The outcome variable is wage and the variable of interest is education. Here, education is defined to be years of schooling. Suppose education is endogenous, i.e. it is correlated with the error term. OLS estimator no longer produces consistent estimate since exogeneity assumption is violated. Thus we introduce an instrument variable, arguably a valid one to isolate bad variation from education. In general, 2SLS is used for IV estimator so at the first stage, education is regressed on the instrument to get the fitted value of education. Then on the second stage, wage is regressed on the fitted value of education. As long as the bad variation in education is well controlled for at the first stage, 2SLS produces consistent estimate for the average treatment effect. 

- Anything particular about standard errors: Standard error in IV is reported to be in general larger than OLS standard error although IV estimators generate a consistent estimate in contrast to OLS with endogenous regressors. If the instruments are weak, i.e. they have very low correlation with the regressors, the instrumental variable estimators or 2SLS estimators could lead to large or inconsistent standard errors. 


<br>
A regression-discontinuity approach to estimating the effect of  $\mathbb{1}(T_i=1)$ on $Y_i$.

- The intuition behind it: Suppose researchers have some knowledge that the non-random treatment is governed at least partly by some threshold value ($c$) of an observed covariate $X_i$ and that passing this threshold induces a change in potential outcome $Y_i$. RD then regards the discontinuity of the mean outcome along the covariate at the cutoff value as causal effect of treatment. Regression discontinuity comes in sharp and fuzzy. In sharp RD, we look at the discontinuity in conditional expectation of the outcome given the covariate to calculate an average causal effect of the treatment. The probability of the treatment changes from 0 to 1 (either not treated or treated) as $X_i$ moves across some threshold $c$. Then RD estimate the local average treatment effect by comparing the mean of the outcome that is right above and right below that threshold $c$. In fuzzy RD, the probability of the treatment that is strictly less than 1 changes as $X_i$ crosses the cutoff. This implies that the effect of $X_i$ crossing the cutoff has influence on the outcome as well as the probability of treatment. Therefore the treatment effect is defined by the ratio of these two effects. Fuzzy RD is similar to IV in a sense that it estimates a change in probability of treatment for variable $X_i-c$ and use the fitted probability of treatment to estimate the change in outcome.  If we extrapolate $E(Y_{0i}|X_i)$ and $E(Y_{1i}|X_i)$ not only upon the threshold $c$ but for the entire $X_i$ horizon, and compare the pre- and post-treatment differences in outcome means for those below the threshold to those above it, regression discontinuity is similar to the difference-in-difference estimator. The fuzzy RD estimates the local average treatment effect of the compliers.

- The identification assumption(s):
(a) Researchers know the assignment mechanism is some function of observable variable $X$. 
(b) Conditional independence assumption: Conditional on the covariates, there is no variation in the treatment.
(c) We observe a discontinuous change in the probability of treatment at some cut-off point. 
(d) $E(Y_{1i}|X_i=x)$ and $E(Y_{0i}|X_i=x)$ are continuous in $x$. This assumption is necessary to extrapolate $E(Y_{0i}|X_i=x)$ around the neighborhood of the discontinuity to use it for counterfactual of the average outcome of treated individuals around that threshold.
(e) Monotonicity assumption: Suppose $T_i(X=x^\star)$ denotes the potential treatment of $i$ with threshold $x^{\star}$. In fuzzy RD, $T_i(X)$ is non-increasing in $x^\star$ at $x^\star = c$. That is, increasing $x^\star$ marginally from $c$ to $c+\varepsilon$ doesn't make someone more likely to be treated, i.e. there is no defiers.   

- An example: Suppose that we are interested in looking at the effect of Ph.D. degree on wage. For the sake of simplicity, suppose that students with GRE test score above 160 get the degree and those below the score don't. The basic idea is that RD splits individuals into two groups below and above the score of 160 and get the difference in the outcome to estimate the average treatment effect. Thus the underlying assumption is that the ability and covariates of these two groups are similar enough that the wage comparison for those on either side of the GRE score of 160 gives us high internal validity. However, recall that the estimator is only so useful to estimate "local treatment effect" around the cutoff, otherwise the control group's outcome wouldn't be a good counterfactual to the treatment group. For example, comparing wages of individuals with GRE score very much far away from the cutoff, say those with score of 130 with those with 170, does not tell anything about whether that person holds a Ph.D. degree.  

- Anything particular about standard errors: Bandwidth selection could affect the estimates and standard errors. 





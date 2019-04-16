####################################
###### testing mean--variance ######
####################################
rm(list = ls())
library(brms)
library(dplyr)
library(ggplot2)
library(cowplot)
library(reshape)
# read the data

# times font
windowsFonts(Times = windowsFont("Times New Roman"))

ur <- "https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/contexteffects/FlankerStroopSimon/cleaning.R"
devtools::source_url(ur)



# incongruent intercept only
fit_1 <- brm(bf(rt ~ 1 + (1|c|ID), sigma ~ 1 + (1|c|ID)), 
             data = stroop %>% filter(congruency == "incongruent"),
             inits = 0, cores = 2, 
             chains = 2, iter = 2000, 
             warmup = 1000)

# check to easy worry of ``over-fitting''
# remove scale model
fit_2 <- brm(bf(rt ~ 1 + (1|c|ID)), 
             data = stroop %>% filter(congruency == "incongruent"),
             inits = 0, cores = 2, 
             chains = 2, iter = 2000, 
             warmup = 1000)

# congruent intercept only
fit_3 <- brm(bf(rt ~ 1 + (1|c|ID), sigma ~ 1 + (1|c|ID)), 
             data = stroop %>% filter(congruency == "congruent"),
             inits = 0, cores = 2, 
             chains = 2, iter = 2000, 
             warmup = 1000)


# check to easy worry of ``over-fitting''
# remove scale model
fit_4 <- brm(bf(rt ~ 1 + (1|c|ID)), 
             data = stroop %>% filter(congruency == "congruent"),
             inits = 0, cores = 2, 
             chains = 2, iter = 2000, 
             warmup = 1000)


###############################
######## plot 1 ###############
###############################

###############################
####### incongruent ###########
###############################

###########
## sigma ##
###########
re_sigma_in <- fit_1 %>% 
  data.frame() %>% 
  select(contains("r_ID__sigma")) 

fe_sigma_in <- fit_1 %>% 
  data.frame() %>% 
  select(contains("b_sigma_Intercept")) 

re_sigma_in <- exp(re_sigma_in + fe_sigma_in[,1]) 
colnames(re_sigma_in) <- 1:121

# empirical estimates
emp_est_in <- stroop %>% 
  filter(congruency == "incongruent") %>% 
  group_by(ID) %>% 
  summarise(sd_emp = sd(rt), mean_emp = mean(rt))

set.seed(1)
random_draw <- sample(1:121, 60, replace = F)

plot_1a <- melt(re_sigma_in) %>% 
  group_by(variable) %>% 
  # compute mean and intervals
  summarise(mu_sigma = mean(value), 
            low = quantile(value, 0.05),
            up = quantile(value, 0.95)) %>% 
  # order small to large
  mutate(sd_emp = emp_est_in$sd_emp) %>%
  arrange(mu_sigma) %>%
  mutate(index = as.factor(1:121), 
         dist_param = "SD", 
         outcome = "Incongruent") %>%
  filter(index %in% random_draw) %>%
  ggplot() +
  # fixed effect line
  geom_hline(yintercept = round(exp(posterior_summary(fit_1, "b_")[2,1]),2), 
             linetype = "twodash",
             alpha = 0.50) +
  # error bars
  geom_errorbar(aes(x = index, ymin = low, ymax = up),width = 0.05) +
  # model based estimates
  geom_point(aes(x = index, y = mu_sigma), size = 2, 
             color = "#0072B2", 
             alpha = 0.75) +
  # empirical estimates
  geom_point(aes(x = index, 
                 y = sd_emp), 
             size = 2, 
             color = "#D55E00", 
             alpha = 0.75) +
  # times font
  theme_bw(base_family = "Times") +
  # plot options
  theme(panel.grid.major.x =   element_blank(), 
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 14),
        title = element_text(size = 16)) +
  scale_x_discrete(expand = c(0.01, 0.01)) +
  ylab("Standard Deviation") +
  xlab("Ascending Index") +
  ggtitle("")
  
  
 
###########
## mean ##
###########
re_mean_in <- fit_1 %>% 
  data.frame() %>% 
  select(contains("r_ID")) %>% 
  select(-contains("sigma"))

fe_mean_in <- fit_1 %>% 
  data.frame() %>% 
  select(contains("b_Intercept")) 

re_mean_in <- (re_mean_in + fe_mean_in[,1]) 
colnames(re_mean_in) <- 1:121

# empirical estimates
# emp_est <- stroop %>% 
#   filter(congruency == "incongruent") %>% 
#   group_by(ID) %>% 
#   summarise(sd_emp = sd(rt), mean_emp = mean(rt))

set.seed(1)
random_draw <- sample(1:121, 60, replace = F)

plot_1b <- melt(re_mean_in) %>% 
  group_by(variable) %>% 
  # compute mean and intervals
  summarise(mu_mu = mean(value), 
            low = quantile(value, 0.05),
            up = quantile(value, 0.95)) %>% 
  # order small to large
  mutate(mu_emp = emp_est_in$mean_emp) %>%
  arrange(mu_mu) %>%
  mutate(index = as.factor(1:121), 
         dist_param = "mean", 
         outcome = "Incongruent") %>%
  filter(index %in% random_draw) %>%
  ggplot() +
  # fixed effect line
  geom_hline(yintercept = round(posterior_summary(fit_1, "b_")[1,1],2), 
             linetype = "twodash",
             alpha = 0.50) +
  # error bars
  geom_errorbar(aes(x = index, 
                    ymin = low, 
                    ymax = up), 
                width = 0.05) +
  # model based estimates
  geom_point(aes(x = index, 
                 y = mu_mu), 
             size = 2, 
             color = "#0072B2", 
             alpha = 0.75) +
  # empirical estimates
  geom_point(aes(x = index, 
                 y = mu_emp), 
             size = 2, 
             color = "#D55E00",
             alpha = 0.75) +
  # times font
  theme_bw(base_family = "Times") +
  # plot options
  theme(panel.grid.major.x =   element_blank(), 
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 14),
        title = element_text(size = 14)) +
  scale_x_discrete(expand = c(0.01, 0.01)) +
  ylab("Mean") +
  xlab("Ascending Index") +
  ggtitle("Incongruent") +
  scale_y_continuous(breaks = c(0.6, 0.7, 0.77, 0.9, 1))


# plot correlations
dat_model_in <- data.frame(type = "Hierarchical",  
                           mean = colMeans(re_mean_in), 
                           sd = colMeans(re_sigma_in))
                        

dat_data_in <- data.frame(type = "Empirical",  
                          mean = emp_est_in$mean_emp, 
                          sd = emp_est_in$sd_emp)


dat_plt_in <- rbind.data.frame(dat_model_in, dat_data_in)

plot_1c <- ggplot(dat_plt, aes(y = sd, x = mean, color = type)) +
  geom_point(size = 2, alpha = 0.75) +
  geom_smooth(method = "lm", se = F, show.legend = F) +
  scale_color_manual(values =c( "#0072B2", "#D55E00")) +
  theme_bw(base_family = "Times") +
  # plot options
  theme(panel.grid.minor.x =   element_blank(), 
        panel.grid.minor.y = element_blank(),
        axis.title = element_text(size = 14),
        title = element_text(size = 14),
         legend.text = element_text(size = 13),
        legend.justification=c(1,0), 
        legend.position=c(1,0),
        legend.title = element_blank(), 
        legend.background = element_rect(color = "black"),
        legend.margin=margin(c(1,1,1,1))) +
  xlab("Mean") +
  ylab("Standard Deviation") +
  guides(colour = guide_legend(override.aes = list(alpha=1))) +
  ggtitle("") 



plot_in <- plot_grid(plot_1b, plot_1a, plot_1c, nrow = 1)



###############################
####### congruent #############
###############################

###########
## sigma ##
###########
re_sigma_con <- fit_3 %>% 
  data.frame() %>% 
  select(contains("r_ID__sigma")) 

fe_sigma_con <- fit_3 %>% 
  data.frame() %>% 
  select(contains("b_sigma_Intercept")) 

re_sigma_con <- exp(re_sigma_con + fe_sigma_con[,1]) 
colnames(re_sigma_con) <- 1:121

# empirical estimates
emp_est_con <- stroop %>% 
  filter(congruency == "congruent") %>% 
  group_by(ID) %>% 
  summarise(sd_emp = sd(rt), mean_emp = mean(rt))

set.seed(1)
random_draw <- sample(1:121, 60, replace = F)

plot_2a <- melt(re_sigma_con) %>% 
  group_by(variable) %>% 
  # compute mean and intervals
  summarise(mu_sigma = mean(value), 
            low = quantile(value, 0.05),
            up = quantile(value, 0.95)) %>% 
  # order small to large
  mutate(sd_emp = emp_est_con$sd_emp) %>%
  arrange(mu_sigma) %>%
  mutate(index = as.factor(1:121), 
         dist_param = "SD", 
         outcome = "Congruent") %>%
  filter(index %in% random_draw) %>%
  ggplot() +
  # fixed effect line
  geom_hline(yintercept = round(exp(posterior_summary(fit_3, "b_")[2,1]),2), 
             linetype = "twodash",
             alpha = 0.50) +
  # error bars
  geom_errorbar(aes(x = index, ymin = low, ymax = up),width = 0.05) +
  # model based estimates
  geom_point(aes(x = index, y = mu_sigma), size = 2, 
             color = "#0072B2", 
             alpha = 0.75) +
  # empirical estimates
  geom_point(aes(x = index, 
                 y = sd_emp), 
             size = 2, 
             color = "#D55E00", 
             alpha = 0.75) +
  # times font
  theme_bw(base_family = "Times") +
  # plot options
  theme(panel.grid.major.x =   element_blank(), 
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 14),
        title = element_text(size = 14)) +
  scale_x_discrete(expand = c(0.01, 0.01)) +
  ylab("Standard Deviation") +
  xlab("Ascending Index") +
  ggtitle("") +
  scale_y_continuous(breaks = c(0.1, 0.17, 0.3))



###########
## mean ##
###########
re_mean_con <- fit_3 %>% 
  data.frame() %>% 
  select(contains("r_ID")) %>% 
  select(-contains("sigma"))

fe_mean_con <- fit_3 %>% 
  data.frame() %>% 
  select(contains("b_Intercept")) 

re_mean_con <- (re_mean_con + fe_mean_con[,1]) 
colnames(re_mean_con) <- 1:121

# empirical estimates
# emp_est <- stroop %>% 
#   filter(congruency == "incongruent") %>% 
#   group_by(ID) %>% 
#   summarise(sd_emp = sd(rt), mean_emp = mean(rt))

set.seed(1)
random_draw <- sample(1:121, 60, replace = F)

plot_2b <- melt(re_mean_con) %>% 
  group_by(variable) %>% 
  # compute mean and intervals
  summarise(mu_mu = mean(value), 
            low = quantile(value, 0.05),
            up = quantile(value, 0.95)) %>% 
  # order small to large
  mutate(mu_emp = emp_est_con$mean_emp) %>%
  arrange(mu_mu) %>%
  mutate(index = as.factor(1:121), 
         dist_param = "mean", 
         outcome = "Congruent") %>%
  filter(index %in% random_draw) %>%
  ggplot() +
  # fixed effect line
  geom_hline(yintercept = round(posterior_summary(fit_3, "b_")[1,1],2), 
             linetype = "twodash",
             alpha = 0.50) +
  # error bars
  geom_errorbar(aes(x = index, 
                    ymin = low, 
                    ymax = up), 
                width = 0.05) +
  
  # model based estimates
  geom_point(aes(x = index, 
                 y = mu_mu), 
             size = 2, 
             color = "#0072B2", 
             alpha = 0.75) +
  
  # empirical estimates
  geom_point(aes(x = index, 
                 y = mu_emp), 
             size = 2, 
             color = "#D55E00",
             alpha = 0.75) +
  
  # times font
  theme_bw(base_family = "Times") +
  
  # plot options
  theme(panel.grid.major.x =   element_blank(), 
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 14),
        title = element_text(size = 14)) +
  scale_x_discrete(expand = c(0.01, 0.01)) +
  ylab("Mean") +
  xlab("Ascending Index") +
  ggtitle("Congruent") +
  scale_y_continuous(breaks = c(0.6, 0.71, 0.8, 0.9, 1))



# plot correlations

dat_model_con <- data.frame(type = "Hierarchical",  
                        mean = colMeans(re_mean_con), 
                        sd = colMeans(re_sigma_con))


dat_data_con <- data.frame(type = "Empirical",  
                       mean = emp_est_con$mean_emp, 
                       sd = emp_est_con$sd_emp)


dat_plt_con <- rbind.data.frame(dat_model_con, dat_data_con)

plot_2c <- ggplot(dat_plt_con, aes(y = sd, 
                                   x = mean, 
                                   color = type)) +
  # points
  geom_point(size = 2, 
             alpha = 0.75) +
  # add fitted line
  geom_smooth(method = "lm", 
              se = F, 
              show.legend = F) +
  # color blind pallette
  scale_color_manual(values =c( "#0072B2", "#D55E00"), 
                     name = "") +
  # fav theme with times font
  theme_bw(base_family = "Times") +
  # plot options
  theme(panel.grid.minor.x =   element_blank(), 
        panel.grid.minor.y = element_blank(),
        axis.title = element_text(size = 14),
        title = element_text(size = 16),
        legend.position = "none") +
  xlab("Mean") +
  ylab("Standard Deviation") +
  # remove transparency from legend points
  guides(colour = guide_legend(override.aes = list(alpha=1))) +
  ggtitle("") 


plot_con <- plot_grid(plot_2b, plot_2a, plot_2c, nrow = 1)


plot_grid(plot_in, plot_con, nrow = 2)


##############################
###### compare WAIC ##########
##############################
WAIC(fit_1, fit_2)
WAIC(fit_3, fit_4)

##############################
##### bootstrapping cors #####
##############################
round(cor(dat_data_in[,2:3]),3)[1,2]
round(mean(posterior_samples(fit_1, pars = "cor")[,1]),3)

round(cor(dat_data_con[,2:3]),3)[1,2]
round(mean(posterior_samples(fit_3, pars = "cor")[,1]),3)


boot_in <- replicate(2000, cor(dat_data_in[sample(1:121, 121, replace = T),2:3])[1,2])
boot_con <- replicate(2000, cor(dat_data_con[sample(1:121, 121, replace = T),2:3])[1,2])


post_in <- posterior_samples(fit_1, pars = "cor")[,1]
post_con <- posterior_samples(fit_3, pars = "cor")[,1]


##############################
#### lkj marginals ###########
##############################
# nu 1
nu_1 <- rethinking::rlkjcorr(1000000, 
                             K = 4, 
                             eta = 1)[,,1][,2]
hdi_1 <- HDInterval::hdi(nu_1, 0.50)

# nu 2
nu_2  <- rethinking::rlkjcorr(1000000, 
                              K = 4, 
                              eta = 2)[,,1][,2]
hdi_2 <- HDInterval::hdi(nu_2, 0.50)

# nu 3
nu_3  <- rethinking::rlkjcorr(1000000, 
                              K = 4, 
                              eta = 3)[,,1][,2]
hdi_3 <- HDInterval::hdi(nu_3, 0.50)

# nu 4
nu_4  <- rethinking::rlkjcorr(1000000, 
                              K = 4, 
                              eta = 4)[,,1][,2]
hdi_4 <- HDInterval::hdi(nu_4, 0.50)

# data for plotting
lkj_dat <- data.frame(nu = as.factor(rep(1:4, each = 10000)), 
                      sample = c(nu_1[1:10000], 
                                 nu_2[1:10000],
                                 nu_3[1:10000], 
                                 nu_4[1:10000]))

lkj_dat %>% 
  ggplot() +
  # for linetype
  geom_line(stat = "density", 
            aes(linetype = nu, x = sample), 
            adjust = 1.5) +
  theme_bw(base_family = "Times") +
  # plot options
  theme(panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(),
        axis.title = element_text(size = 14),
        # top right legend
        legend.justification=c(1,1), 
        legend.position=c(1,1),
        legend.background = element_rect(color = "black")) +
  xlab( expression("Marginal Prior Distribution"~ italic(rho[i][j]))) +
  ylab("Density") +
  scale_linetype_manual(name = expression("  "~italic(nu)), 
                        values = c("solid", "longdash", "dotted", "dotdash"))
  


##########################
##### full models ########
##########################

# stroop MELSM
form_stroop <- brmsformula(rt ~ congruency + (congruency |c| ID), 
                           sigma  ~ congruency + (congruency |c| ID))



prior_stroop <- c(
  # correlations
  set_prior("lkj(2)", class = "cor"),
  # location random SD
  set_prior("normal(0,0.25)", 
            class = "sd", 
            coef = "congruencyincongruent", 
            group = "ID"),
  # location random SD
  set_prior("normal(0,0.25)",
            class = "sd", 
            coef = "Intercept",
            group = "ID"),
  # scale random SD
  set_prior("normal(0,1)", 
            class = "sd",
            coef = "congruencyincongruent", 
            group = "ID", 
            dpar = "sigma"), 
  # scale random SD
  set_prior("normal(0,1)", 
            class = "sd", 
            coef = "Intercept", 
            group = "ID", 
            dpar = "sigma"), 
  # fixed effect priors
  set_prior("normal(0,5)", 
            class = "b"),
  set_prior("normal(0,5)", 
            class = "b", 
            dpar = "sigma"),
  set_prior("normal(0,5)", 
            class = "Intercept"),
  set_prior("normal(0,5)", 
            class = "Intercept", 
            dpar = "sigma"))

fit_stroop <- brm(form_stroop, data = stroop, 
                  inits = 0, cores = 4, 
                  chains = 4, iter = 3500, 
                  warmup = 1000, prior = prior_stroop)

# save(fit_stroop, file = "fit_stroop.Rdata")

# fixed effects
round(posterior_summary(fit_stroop, pars = "b_", probs = c(0.05, 0.95)),2)

# random effects
round(posterior_summary(fit_stroop, pars = "sd_", probs = c(0.05, 0.95)),2)


###################################
##### fit mixed effects model #####
###################################
# stroop ME
form_stroop_me <- brmsformula(rt ~ congruency + (congruency |c| ID))


# ME priors 
prior_stroop_me <- c(
  set_prior("lkj(2)", class = "cor"),
  # location random SD
  set_prior("normal(0,0.25)", 
            class = "sd", 
            coef = "Intercept", 
            group = "ID"),
  set_prior("normal(0,0.25)", 
            class = "sd",
            coef = "congruencyincongruent", 
            group = "ID"), 
                  # fixed effect priors
                  set_prior("normal(0,5)", 
                            class = "b"),
                  set_prior("normal(0,5)", 
                            class = "Intercept"))

fit_stroop_me <- brm(form_stroop_me, data = stroop, 
                  inits = 0, cores = 4, 
                  chains = 4, iter = 3500, 
                  warmup = 1000, prior = prior_stroop_me)

#save(fit_stroop_me, file = "fit_stroop_me.Rdata")
#WAIC(fit_stroop, fit_stroop_me)

###############################
## mixed effects model plots ##
###############################
# intercept
me_int <- round(posterior_summary(fit_stroop_me, pars = "b_")[1,1], 2)
# stroop effect
me_stroop <- round(posterior_summary(fit_stroop_me, pars = "b_")[2,1], 3)

# intercept plot
me_int_plot <- coef(fit_stroop_me, probs = c(0.05, 0.95))$ID[,,1] %>% 
  data.frame() %>%
  arrange(Estimate) %>%
  mutate(sig = as.factor(ifelse(Q5 < me_int & Q95 > me_int, 0, 1)),
         index = as.factor(1:121),
         type = "MEM") %>%
  ggplot() +
  facet_grid(~ type ) +
  geom_errorbar(aes(x = index, 
                    ymin = Q5, 
                    ymax = Q95, 
                    color = sig), 
                show.legend = F,
                width = 0) +
  geom_hline(yintercept = me_int,
             alpha = 0.50,
             linetype = "twodash") +
  geom_point(aes(x = index, 
                 y = Estimate,
                 group = sig),
             size = 2, 
             alpha = 0.75) +
 theme_bw(base_family = "Times") +
  # plot options
  theme(panel.grid.major.x =   element_blank(), 
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 14),
        title = element_text(size = 14),
        strip.background = element_rect(fill = "grey94")) +
  ylab(expression(italic(beta[0])* " + " *italic(u[0][i]))) +
  scale_color_manual(values = c("#009E73", "#CC79A7")) +
  xlab("") +
  scale_y_continuous(breaks = seq(0.6, 1, .1)) +
  scale_x_discrete(expand = c(0.015, 0.015)) 



# stroop plot
me_stroop_plot <- coef(fit_stroop_me, probs = c(0.05, 0.95))$ID[,,2] %>% 
  data.frame() %>%
  arrange(Estimate) %>%
  mutate(sig = as.factor(ifelse(Q5 < me_stroop & Q95 > me_stroop, 0, 1)),
         index = as.factor(1:121),
         type = "MEM") %>%
  ggplot() +
  facet_grid(~ type ) +
  geom_errorbar(aes(x = index, 
                    ymin = Q5, 
                    ymax = Q95, 
                    color = sig), 
                show.legend = F,
                width = 0) +
  geom_hline(yintercept = me_stroop,
             alpha = 0.50,
             linetype = "twodash") +
  geom_hline(yintercept = 0,
             alpha = 0.50,
             linetype = "dotted") +
  
  geom_point(aes(x = index, 
                 y = Estimate,
                 group = sig),
             size = 2, 
             alpha = 0.75) +
  theme_bw(base_family = "Times") +
  # plot options
  theme(panel.grid.major.x =   element_blank(), 
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 14),
        title = element_text(size = 14),
        strip.background = element_rect(fill = "grey94")) +
  ylab(expression(italic(beta[1])* " + " *italic(u[1][i]))) +
  scale_color_manual(values = c("#009E73", "#CC79A7")) +
  xlab("") +
  scale_y_continuous(breaks = seq(0, .15, .05), 
                     labels = seq(0, .15, .05), 
                     limits = c(-0.02, 0.16)) +
  scale_x_discrete(expand = c(0.015, 0.015)) 




###############################
## mixed effects LSM   plots ##
###############################
# intercept
melsm_int <- round(posterior_summary(fit_stroop, pars = "b_")[1,1], 2)
# stroop effect
melsm_stroop <- round(posterior_summary(fit_stroop, pars = "b_")[3,1], 3)


# intercept
melsm_scale_int <- round(posterior_summary(fit_stroop, pars = "b_")[2,1], 3)
# stroop effect
melsm_scale_stroop <- round(posterior_summary(fit_stroop, pars = "b_")[4,1], 3)





melsm_int_plot <- coef(fit_stroop, probs = c(0.05, 0.95))$ID[,,1] %>% 
  data.frame() %>%
  arrange(Estimate) %>%
  mutate(sig = as.factor(ifelse(Q5 < melsm_int & Q95 > melsm_int, 0, 1)),
         index = as.factor(1:121),
         type = "MELSM") %>%
  ggplot() +
  facet_grid(~ type ) +
  geom_errorbar(aes(x = index, 
                    ymin = Q5, 
                    ymax = Q95, 
                    color = sig), 
                show.legend = F,
                width = 0) +
  geom_hline(yintercept = melsm_int,
             alpha = 0.50,
             linetype = "twodash") +
  geom_point(aes(x = index, 
                 y = Estimate,
                 group = sig),
             size = 2, 
             alpha = 0.75) +
  theme_bw(base_family = "Times") +
  # plot options
  theme(panel.grid.major.x =   element_blank(), 
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 14),
        title = element_text(size = 14),
        strip.background = element_rect(fill = "grey94")) +
  ylab(expression(italic(beta[0])* " + " *italic(u[0][i]))) +
  scale_color_manual(values = c("#009E73", "#CC79A7")) +
  xlab("") +
  scale_y_continuous(breaks = seq(0.6, 1, .1)) +
  scale_x_discrete(expand = c(0.015, 0.015)) 


melsm_stroop_plot <- coef(fit_stroop, probs = c(0.05, 0.95))$ID[,,2] %>% 
  data.frame() %>%
  arrange(Estimate) %>%
  mutate(sig = as.factor(ifelse(Q5 < melsm_stroop & Q95 > melsm_stroop, 0, 1)),
         index = as.factor(1:121),
         type = "MELSM") %>%
  ggplot() +
  facet_grid(~ type ) +
  geom_errorbar(aes(x = index, 
                    ymin = Q5, 
                    ymax = Q95, 
                    color = sig), 
                show.legend = F,
                width = 0) +
  geom_hline(yintercept = melsm_stroop,
             alpha = 0.50,
             linetype = "twodash") +
  geom_hline(yintercept = 0,
             alpha = 0.50,
             linetype = "dotted") +
  geom_point(aes(x = index, 
                 y = Estimate,
                 group = sig),
             size = 2, 
             alpha = 0.75) +
  theme_bw(base_family = "Times") +
  # plot options
  theme(panel.grid.major.x =   element_blank(), 
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 14),
        title = element_text(size = 14),
        strip.background = element_rect(fill = "grey94")) +
  ylab(expression(italic(beta[1])* " + " *italic(u[1][i]))) +
  scale_color_manual(values = c("#009E73", "#CC79A7")) +
  xlab("") +
  scale_y_continuous(breaks = seq(0, .15, .05), 
                     labels = seq(0, .15, .05), 
                     limits = c(-0.02, 0.16)) +
  scale_x_discrete(expand = c(0.015, 0.015)) 






melsm_scale_int_plot <- coef(fit_stroop, probs = c(0.05, 0.95))$ID[,,3] %>% 
  data.frame() %>%
  arrange(Estimate) %>%
  mutate(sig = as.factor(ifelse(Q5 < melsm_scale_int & Q95 > melsm_scale_int, 0, 1)),
         index = as.factor(1:121),
         type = "MELSM") %>%
  ggplot() +
  facet_grid(~ type ) +
  geom_errorbar(aes(x = index, 
                    ymin = Q5, 
                    ymax = Q95, 
                    color = sig), 
                show.legend = F,
                width = 0) +
  geom_hline(yintercept = melsm_scale_int,
             alpha = 0.50,
             linetype = "twodash") +
  geom_point(aes(x = index, 
                 y = Estimate,
                 group = sig),
             size = 2, 
             alpha = 0.75) +
  theme_bw(base_family = "Times") +
  # plot options
  theme(panel.grid.major.x =   element_blank(), 
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 14),
        title = element_text(size = 14),
        strip.background = element_rect(fill = "grey94")) +
  ylab(expression(italic(eta[0])* " + " *italic(u[2][i]))) +
  scale_color_manual(values = c("#009E73", "#CC79A7")) +
  xlab("") +
  scale_x_discrete(expand = c(0.015, 0.015)) +
  xlab("Ascending Index") 


melsm_scale_stroop_plot <- coef(fit_stroop, probs = c(0.05, 0.95))$ID[,,4] %>% 
  data.frame() %>%
  arrange(Estimate) %>%
  mutate(sig = as.factor(ifelse(Q5 < melsm_scale_stroop & Q95 > melsm_scale_stroop, 0, 1)),
         index = as.factor(1:121),
         type = "MELSM") %>%
  ggplot() +
  facet_grid(~ type ) +
  geom_errorbar(aes(x = index, 
                    ymin = Q5, 
                    ymax = Q95, 
                    color = sig), 
                show.legend = F,
                width = 0) +
  geom_hline(yintercept = melsm_scale_stroop,
             alpha = 0.50,
             linetype = "twodash") +
  geom_hline(yintercept = 0,
             alpha = 0.50,
             linetype = "dotted") +
  geom_point(aes(x = index, 
                 y = Estimate,
                 group = sig),
             size = 2, 
             alpha = 0.75) +
  theme_bw(base_family = "Times") +
  # plot options
  theme(panel.grid.major.x =   element_blank(), 
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_text(size = 14),
        title = element_text(size = 14),
        strip.background = element_rect(fill = "grey94")) +
  ylab(expression(italic(eta[1])* " + " *italic(u[3][i]))) +
  scale_color_manual(values = c("#009E73", "#CC79A7")) +
  xlab("") +
  scale_x_discrete(expand = c(0.015, 0.015)) +
  xlab("Ascending Index") 
# melsm_scale_stroop_plot

mu_int_plot <- plot_grid(me_int_plot, 
                         melsm_int_plot, 
                         labels = c("a", "b" ), 
                         hjust = -7, 
                         vjust = 4)


mu_stroop_plot <- plot_grid(me_stroop_plot, 
                            melsm_stroop_plot,
                            labels = c("c", "d" ), 
                            hjust = -7, 
                            vjust = 4)

scale_plot <- plot_grid(melsm_scale_int_plot, 
                        melsm_scale_stroop_plot,
                        labels = c("e", "f" ), 
                        hjust = -7, 
                        vjust = 4)

plot_grid(mu_int_plot, 
          mu_stroop_plot, scale_plot, nrow = 3)



mean(coef(fit_stroop, probs = c(0.05, 0.95))$ID[,,4][,1] > 0)

# +
  # scale_y_continuous(breaks = seq(0.6, 1, .1)) +
  # scale_x_discrete(expand = c(0.015, 0.015)) 






# # melsm_stroop_plot
# # melsm__plot
# # me_stroop_plot
# 
# 
# # random effects mean
# re_mean_stroop <- fit_stroop  %>% 
#   data.frame() %>%
#   select( contains("r")) %>%
#   select(- contains("sigma")) %>%
#   select(-contains("Intercept")) %>%
#   select(-contains("b_")) %>%
#   select(-contains("sd_"))
# 
# # fixed effect mean
# fe_mean_stroop <- fit_stroop  %>% 
#   data.frame() %>% 
#   select(contains("b_congruencyincongruent"))
# 
# re_mean_stroop <- (re_mean_stroop + fe_mean_stroop[,1])
# colnames(re_mean_stroop) <- 1:121
# 
# plot_1a <- re_mean_stroop %>% 
#    melt() %>% 
#   group_by(variable) %>% 
#   summarise(mu = mean(value),
#             low = quantile(value, probs = 0.05),
#              up = quantile(value, probs = 0.95)) %>%
#   mutate(sig = as.factor(ifelse(low < round(mean(fe_mean_stroop[,1]), 2)  & 
#                                 up > round(mean(fe_mean_stroop[,1]), 2), 0, 1))) %>%
#   arrange(mu) %>%
#   mutate(index = as.factor(1:121)) %>%
#   ggplot() +
#   # line at average
#   geom_hline(yintercept = round(mean(fe_mean_stroop[,1]), 2),
#              alpha = 0.50,
#              linetype = "twodash") +
#   # line at 0
#   geom_hline(yintercept = 0,
#              alpha = 0.50) +
#   # 90 credible intervals
#   geom_errorbar(aes(x = index, 
#                     ymin = low, 
#                     ymax = up, 
#                     color = sig), 
#                 show.legend = F,
#                 width = 0) +
#   # points
#   geom_point(aes(x = index, 
#                  y = mu,
#                  group = sig), 
#              size = 2, 
#              alpha = 0.75) +
#   theme_bw(base_family = "Times") +
#   # plot options
#   theme(panel.grid.major.x =   element_blank(), 
#         panel.grid.minor.y = element_blank(),
#         axis.text.x = element_blank(),
#         axis.title = element_text(size = 14),
#         title = element_text(size = 14)) +
#   xlab("Ascending Index") +
#   scale_x_discrete(expand = c(0.015, 0.015)) +
#   ylab(expression(atop(italic(beta[0])* " + " *italic(u[0][i]),  
#                        mu * "  Congruency"))) +
#   scale_y_continuous(breaks = c(0, 0.05, 0.07, 0.10, 0.15)) +
#   scale_color_manual(values = c("#009E73", "#CC79A7"))
#     
# 
# 
# 
# # random effects scale
# re_sigma_stroop <- fit_stroop  %>% 
#   data.frame() %>%
#   select( contains("r")) %>%
#   select(contains("sigma")) %>%
#   select(-contains("Intercept")) %>%
#   select(-contains("b_")) %>%
#   select(-contains("sd_")) %>%
#   select(-contains("cor"))
# 
# # fixed effect mean
# fe_sigma_stroop <- fit_stroop  %>% 
#   data.frame() %>% 
#   select(contains("b_sigma_congruencyincongruent"))
# 
# re_sigma_stroop <- re_sigma_stroop + fe_sigma_stroop[,1]
# colnames(re_sigma_stroop) <- 1:121
# 
# 
# plot_2a <- re_sigma_stroop %>% 
#   melt() %>% 
#   group_by(variable) %>% 
#   summarise(mu = mean(value),
#             low = quantile(value, probs = 0.05),
#             up = quantile(value, probs = 0.95)) %>%
#   mutate(sig = as.factor(ifelse(low < round(mean((fe_sigma_stroop[,1])),2)  & 
#                                 up > round(mean((fe_sigma_stroop[,1])),2) , 0, 1))) %>%
#   arrange(mu) %>%
#   mutate(index = as.factor(1:121)) %>%
#   ggplot() +
#   # line at average
#   geom_hline(yintercept = round(mean((fe_sigma_stroop[,1])),2),
#              alpha = 0.50,
#              linetype = "twodash") +
#   # line at 0
#   geom_hline(yintercept = 0,
#   alpha = 0.50) +
#   # 90 credible intervals
#   geom_errorbar(aes(x = index, 
#                     ymin = low, 
#                     ymax = up, 
#                     color = sig), 
#                 show.legend = F,
#                 width = 0) +
#   # points
#   geom_point(aes(x = index, 
#                  y = mu,
#                  group = sig), 
#              size = 2, 
#              alpha = 0.75) +
#   theme_bw(base_family = "Times") +
#   # plot options
#   theme(panel.grid.major.x =   element_blank(), 
#         panel.grid.minor.y = element_blank(),
#         axis.text.x = element_blank(),
#         axis.title = element_text(size = 14),
#         title = element_text(size = 14)) +
# 
#   scale_x_discrete(expand = c(0.015, 0.015)) +
#   ylab(expression(atop(italic(eta[1])* " + " *italic(u[3][i]), 
#                        "log"*(sigma)* "  Congruency"))) +
#   scale_y_continuous(breaks = c(-0.5, 0, 0.5, 0.16, 0.5, 1)) +
#   scale_color_manual(values = c("#009E73", "#CC79A7"))
# 
# plot_1 <- plot_grid(plot_1a, plot_2a, ncol = 2)
# 
# 
# ####################################
# ####### correlation plots ##########
# ####################################
# 
# mu_con_stroop <- fit_stroop %>% 
#   coef() %>% 
#   .$ID %>% 
#   .[,,"Intercept"] %>% 
#   data.frame() %>% 
#   select(Estimate)
# 
# sigma_con_stroop <- fit_stroop %>% 
#   coef() %>% 
#   .$ID %>% 
#   .[,,"sigma_Intercept"] %>% 
#   data.frame() %>% 
#   select(Estimate)
# 
# 
# dat_31 <- data.frame(mu_con_stroop = mu_con_stroop[,1], 
#                      sigma_con_stroop = sigma_con_stroop[,1])
# 
# plot_31 <- dat_31 %>% 
#   ggplot(aes(x = sigma_con_stroop, 
#              y = mu_con_stroop)) +
#   theme_bw(base_family = "Times") +
#   theme(panel.grid = element_blank(),
#         axis.title = element_text(size = 14),
#         title = element_text(size = 14)) +
#   stat_density_2d(aes(fill = ..density..), 
#                   geom = "raster", 
#                   contour = FALSE, 
#                   alpha = .75, 
#                   show.legend = F) +
#   scale_fill_distiller(palette= "Spectral", 
#                        direction=1) +
#   geom_smooth(method = "lm", 
#               color = "white", 
#               se = FALSE) +
#   geom_point(aes(x = sigma_con_stroop, 
#                  y = mu_con_stroop), 
#              size = 2, 
#              color = "black", 
#              alpha = 0.5) +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   ylab(expression(atop(italic(beta[0])* " + " *italic(u[0][i]), 
#                        italic(mu)*"  Intercept"))) +
#   xlab(expression(atop(italic(eta[0])* " + " *italic(u[2][i]), 
#                        "log"*(sigma)* "  Intercept"))) 
#  
# 
# 
# 
# 
# sigma_effect_stroop <- fit_stroop %>% 
#   coef() %>% 
#   .$ID %>% 
#   .[,,"sigma_congruencyincongruent"] %>% 
#   data.frame() %>% 
#   select(Estimate)
# 
# 
# dat_41 <- data.frame(mu_con_stroop = mu_con_stroop[,1], 
#                      sigma_effect_stroop = sigma_effect_stroop[,1])
# 
# plot_41 <- 
#   dat_41 %>% 
#   ggplot(aes(x = sigma_effect_stroop, 
#              y = mu_con_stroop)) +
#   theme_bw(base_family = "Times") +
#   theme(panel.grid = element_blank(),
#         axis.title = element_text(size = 14),
#         title = element_text(size = 14)) +
#   stat_density_2d(aes(fill = ..density..), 
#                   geom = "raster", 
#                   contour = FALSE, 
#                   alpha = .75, 
#                   show.legend = F) +
#   # spectral gradient
#   scale_fill_distiller(palette = "Spectral", 
#                        direction = 1) +
#   # fitted line
#   geom_smooth(method = "lm", 
#               color = "white", 
#               # no standard error
#               se = FALSE) +
#   # add points
#   geom_point(aes(x = sigma_effect_stroop, 
#                  y = mu_con_stroop), 
#              size = 2, 
#              color = "black", 
#              alpha = 0.5) +
#   # more all space in plot
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   # y label
#   ylab(expression(atop(italic(beta[0])* " + " *italic(u[0][i]), 
#                        italic(mu)*"  Intercept"))) +
#   # x label
#   xlab(expression(atop(italic(eta[1])* " + " *italic(u[3][i]), 
#                        "log"*(sigma)* "  Congruency"))) 
# 
# 
# mu_effect_stroop <- fit_stroop %>% 
#   coef() %>% 
#   .$ID %>% 
#   .[,,"congruencyincongruent"] %>% 
#   data.frame() %>% 
#   select(Estimate)
# 
# 
# dat_32 <- data.frame(mu_effect_stroop = mu_effect_stroop[,1], 
#                      sigma_con_stroop = sigma_con_stroop[,1])
# 
# plot_32 <- dat_32 %>% 
#   ggplot(aes(x = mu_effect_stroop, 
#              y = sigma_con_stroop)) +
#   theme_bw(base_family = "Times") +
#   theme(panel.grid = element_blank(),
#         axis.title = element_text(size = 14),
#         title = element_text(size = 14)) +
#   stat_density_2d(aes(fill = ..density..), 
#                   geom = "raster", 
#                   contour = FALSE, 
#                   alpha = .75, 
#                   show.legend = F) +
#   scale_fill_distiller(palette= "Spectral", 
#                        direction=1) +
#     geom_smooth(method = "lm", 
#                 color = "white", 
#                 se = FALSE) +
#     geom_point(aes(x = mu_effect_stroop, 
#                  y = sigma_con_stroop), 
#                size = 2, 
#                color = "black", 
#                alpha = 0.5) +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   ylab(expression(atop(italic(beta[1])* " + " *italic(u[2][i]), 
#                        italic(mu)*"  Congruency"))) +
#   xlab(expression(atop(italic(eta[0])* " + " *italic(u[2][i]), 
#                        "log"*(sigma)* "  Intercept"))) 
# 
# 
# dat_42 <- data.frame(mu_effect_stroop = mu_effect_stroop[,1], 
#                      sigma_effect_stroop = sigma_effect_stroop[,1])
#                      
# 
# plot_42 <- dat_42 %>% 
#   ggplot(aes(x = mu_effect_stroop, 
#              y =  sigma_effect_stroop )) +
#   theme_bw(base_family = "Times") +
#   theme(panel.grid = element_blank(),
#         axis.title = element_text(size = 14),
#         title = element_text(size = 14)) +
#   stat_density_2d(aes(fill = ..density..), 
#                   geom = "raster", 
#                   contour = FALSE, 
#                   alpha = .75, 
#                   show.legend = F) +
#   scale_fill_distiller(palette= "Spectral", 
#                        direction=1) +
#   geom_smooth(method = "lm", 
#               color = "white", 
#               se = FALSE) +
#   geom_point(aes(x = mu_effect_stroop, 
#                  y = sigma_effect_stroop), 
#              size = 2, 
#              color = "black", 
#              alpha = 0.5) +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   xlab(expression(atop(italic(beta[1])* " + " *italic(u[2][i]), 
#                        italic(mu)*"  Congruency"))) +
#   ylab(expression(atop(italic(eta[1])* " + " *italic(u[2][i]), 
#                        "log"*(sigma)* "  Congruency"))) 
# 
# 
# left_cors_stroop <- plot_grid(plot_31, plot_41, ncol = 1)
# right_cors_stroop <- plot_grid(plot_32, plot_42, ncol = 1)
# cor_plot <- plot_grid(left_cors_stroop, right_cors_stroop)
# 
# plot_grid(plot_1, "", cor_plot, nrow = 3, rel_heights = c(1,.25,2.5) )
# 
# 
#   

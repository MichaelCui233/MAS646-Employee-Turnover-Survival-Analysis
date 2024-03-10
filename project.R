library(dplyr)
library(ggfortify)
library(ggplot2)
library(survival)
library(ranger)
library(tidyr)

employee = read.csv("employee.csv")
performance_quit = read.csv("performance_quit.csv")
performance_fired = read.csv("performance_fired.csv")

  
# clean data
employee = employee %>%
  mutate(
    gender = factor(gender),
    workerhasdegree = factor(workerhasdegree),
    segment = factor(segment),
    joblevel = factor(joblevel)
  )

employee_quit = employee %>%
  filter(
    reason != 'Fired'
  )%>%
  rename(quit = reason) %>%
  mutate(
    quit = ifelse(quit == "Quit", 1, 0),
    highestdegree = factor(ifelse(highestdegree == 'Bachelor of Arts (BA)', 'BA' , 
                           ifelse(highestdegree == 'Master of Business Administration (MBA)', 'MBA', 
                                  ifelse(highestdegree == 'Master of Science (MS)', 'MS','Others' 
                                                ))) )          
  )

employee_fired = employee %>%
  filter(
    reason != 'Quit'
  )%>%
  rename(fired = reason) %>%
  mutate(
    fired = ifelse(fired == "Fired", 1, 0),
    highestdegree = factor(ifelse(highestdegree == 'Bachelor of Arts (BA)', 'BA' , 
                           ifelse(highestdegree == 'Master of Business Administration (MBA)', 'MBA', 
                                  ifelse(highestdegree == 'Associate of Arts (AA)', 'AA',
                                         ifelse(highestdegree == 'Bachelor of Business Administration (BBA)', 'BBA','Others'
                                                )))))
  )
  




# Join the two datasets by id/employee_id
epf1 <- left_join(performance_fired, employee_fired, by = c("employee_id" = "id"))
epq1 <- left_join(performance_quit, employee_quit, by = c("employee_id" = "id"))

epf1$rating <- factor(epf1$rating, 
                   levels = c("Underperformance", "Inconsistent", 
                              "Solid", "Strong", "Outstanding"), 
                   ordered = TRUE)
epq1$rating <- factor(epq1$rating, 
                      levels = c("Underperformance", "Inconsistent", 
                                 "Solid", "Strong", "Outstanding"), 
                      ordered = TRUE)

epf2 <- epf1 %>%
  group_by(employee_id) %>%
  mutate(last_row = row_number() == n(),
         fired = if_else(fired == 0, 0,
                         if_else(last_row, 1, 0)),
         review_date = as.Date(review_date),
         tstart = as.integer(review_date - min(review_date)),
         tstop = as.integer(lead(review_date, default = max(review_date) + 30) - min(review_date) - 1)
  ) %>%
  select(-last_row)

epq2 <- epq1 %>%
  group_by(employee_id) %>%
  mutate(last_row = row_number() == n(),
         quit = if_else(quit == 0, 0,
                         if_else(last_row, 1, 0)),
         review_date = as.Date(review_date),
         tstart = as.integer(review_date - min(review_date)),
         tstop = as.integer(lead(review_date, default = max(review_date) + 30) - min(review_date) - 1)
  ) %>%
  select(-last_row)






# build model
# Quit
survivalFit1 = survfit(Surv(tenure, quit) ~ 1, data = employee_quit)
autoplot(survivalFit1)

#1. gender
survivalFit1.1 = survfit(Surv(tenure, quit) ~ gender, data = employee_quit)
plot(survivalFit1.1, ylim=c(0.2, 1), xlab='Days since Joining', ylab='Quit Probability', col=c('red', 'blue'))
legend('topright', legend = levels(employee_quit$gender), lty=1, col=c('red', 'blue'))
abline(v=180) 
abline(h=.5) 

survdiff(Surv(tenure, quit) ~ gender, data = employee_quit) # keep

#2. age
m2.1 = coxph(Surv(tenure, quit) ~ age, data = employee_quit)
summary(m2.1)

cox.zph(m2.1) 

#3. workerhasdegree
survivalFit3.1 = survfit(Surv(tenure, quit) ~ workerhasdegree, data = employee_quit)
plot(survivalFit3.1, ylim=c(0.2, 1), xlab='Days since Joining', ylab='Quit Probability', col=c('red', 'blue'))
legend('bottomleft', legend = levels(employee_quit$workerhasdegree), lty=1, col=c('red', 'blue'))


survdiff(Surv(tenure, quit) ~ workerhasdegree, data = employee_quit) # discard

#4. highestdegree 
survivalFit4.1 = survfit(Surv(tenure, quit) ~ highestdegree, data = employee_quit)
plot(survivalFit4.1, ylim=c(0.2, 1), xlab='Days since Joining', ylab='Quit Probability', col=c('red', 'blue','green','black'))
legend('topright', legend = levels(employee_quit$highestdegree), lty=1, col=c('red', 'blue','green','black'))
abline(v=180) 
abline(h=.5)

survdiff(Surv(tenure, quit) ~ highestdegree, data = employee_quit)


#5 segment
survivalFit5.1 = survfit(Surv(tenure, quit) ~ segment, data = employee_quit)
plot(survivalFit5.1, ylim=c(0.2, 1), xlab='Days since Joining', ylab='Quit Probability', col=c('red', 'blue','green','black'))
legend('topright', legend = levels(employee_quit$segment), lty=1, col=c('red', 'blue','green','black'))


survdiff(Surv(tenure, quit) ~ segment, data = employee_quit) # discard

#6 region
m6.1 = coxph(Surv(tenure, quit) ~ region, data = employee_quit)
summary(m6.1)

cox.zph(m6.1) #discard

#7 joblevel
survivalFit7.1 = survfit(Surv(tenure, quit) ~ joblevel, data = employee_quit)
plot(survivalFit7.1, ylim=c(0, 1), xlab='Days since Joining', ylab='Quit Probability', col=c('red', 'blue', 'green'))
legend('right', legend = levels(employee_quit$joblevel), lty=1, col=c('red', 'blue', 'green'))
abline(v=180) 
abline(h=.5)

survdiff(Surv(tenure, quit) ~ joblevel, data = employee_quit) # keep

#8 gender, age, highestdegree, joblevel

m8.1 = coxph(Surv(tenure, quit) ~ age + gender + highestdegree + joblevel, data = employee_quit)
summary(m8.1)

cox.zph(m8.1) 


#9 performance
m9.1 = coxph(Surv(tstart, tstop, quit) ~ rating + appointments + signups + ratio.of.signups.appointments,
      data =epq2, cluster = employee_id)
summary(m9.1)

cox.zph(m9.1)


#10 big model
m10.1 =  coxph(Surv(tstart, tstop, quit) ~ rating + appointments + signups + ratio.of.signups.appointments + age + gender + highestdegree + joblevel,
            data =epq2, cluster = employee_id)
summary(m10.1)

cox.zph(m10.1)





# Fired
survivalFit2 = survfit(Surv(tenure, fired) ~ 1, data = employee_fired)
autoplot(survivalFit2)

#1. gender
survivalFit1.2 = survfit(Surv(tenure, fired) ~ gender, data = employee_fired)
plot(survivalFit1.2, ylim=c(0.2, 1), xlab='Days since Joining', ylab='fired Probability', col=c('red', 'blue'))
legend('topright', legend = levels(employee_fired$gender), lty=1, col=c('red', 'blue'))
abline(v=180) 
abline(h=.5)

survdiff(Surv(tenure, fired) ~ gender, data = employee_fired) # keep

#2. age
m2.2 = coxph(Surv(tenure, fired) ~ age, data = employee_fired)
summary(m2.2)

cox.zph(m2.2) # keep

#3. workerhasdegree
survivalFit3.2 = survfit(Surv(tenure, fired) ~ workerhasdegree, data = employee_fired)
plot(survivalFit3.2, ylim=c(0.2, 1), xlab='Days since Joining', ylab='fired Probability', col=c('red', 'blue'))
legend('bottomleft', legend = levels(employee_fired$workerhasdegree), lty=1, col=c('red', 'blue'))


survdiff(Surv(tenure, fired) ~ workerhasdegree, data = employee_fired) # discard

#4. highestdegree 
survivalFit4.2 = survfit(Surv(tenure, fired) ~ highestdegree, data = employee_fired)
plot(survivalFit4.2, ylim=c(0.4, 1), xlab='Days since Joining', ylab='fired Probability', col=c('red', 'blue','green','black','pink'))
legend('topright', legend = levels(employee_fired$highestdegree), lty=1, col=c('red', 'blue','green','black','pink'))
abline(v=180) 
abline(h=.5)

survdiff(Surv(tenure, fired) ~ highestdegree, data = employee_fired)

#5 segment
survivalFit5.2 = survfit(Surv(tenure, fired) ~ segment, data = employee_fired)
plot(survivalFit5.2, ylim=c(0.2, 1), xlab='Days since Joining', ylab='fired Probability', col=c('red', 'blue','green','black'))
legend('topright', legend = levels(employee_fired$segment), lty=1, col=c('red', 'blue','green','black'))


survdiff(Surv(tenure, fired) ~ segment, data = employee_fired) # discard

#6 region
m6.2 = coxph(Surv(tenure, fired) ~ region, data = employee_fired)
summary(m6.2)

cox.zph(m6.2) #discard

#7 joblevel
survivalFit7.2 = survfit(Surv(tenure, fired) ~ joblevel, data = employee_fired)
plot(survivalFit7.2, ylim=c(0, 1), xlab='Days since Joining', ylab='fired Probability', col=c('red', 'blue', 'green'))
legend('bottomleft', legend = levels(employee_fired$joblevel), lty=1, col=c('red', 'blue', 'green'))
abline(v=180) 
abline(h=.5)

survdiff(Surv(tenure, fired) ~ joblevel, data = employee_fired) # keep

#8 gender, age, highestdegree, joblevel

m8.2 = coxph(Surv(tenure, fired) ~ age + gender + highestdegree + joblevel, data = employee_fired)
summary(m8.2)

cox.zph(m8.2) 



#9 performance
m9.2 = coxph(Surv(tstart, tstop, fired) ~ rating + appointments + signups + ratio.of.signups.appointments,
           data =epf2, cluster = employee_id)
summary(m9.2)

cox.zph(m9.2)


#10 big model
m10.2 =  coxph(Surv(tstart, tstop, fired) ~ rating + appointments + signups + ratio.of.signups.appointments + age + gender + highestdegree + joblevel,
            data =epf2, cluster = employee_id)
summary(m10.2)

cox.zph(m10.2)


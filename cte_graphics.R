library(tidyverse)

cte.table$occupation_title_3 <- stringr::str_sub(cte.table$occupation_title, 1, 18)
cte.table$program <- stringr::str_sub(cte.table$`Program Name`, 1, 10)

cte.table %>% filter(deg.type == "CP") %>% arrange(proj_ann_jobs) %>% 
  ggplot() + geom_point(aes(y = program, x= proj_ann_jobs, color = occupation_title_3)) + theme_minimal()

cte.table %>% filter(deg.type == "CP") %>% arrange(in_ann_med_wage) %>%
  ggplot() + geom_point(aes(y = program, x = in_ann_med_wage, color = occupation_title_3)) + theme_minimal()

cte.table %>% filter(deg.type == "CP") %>% arrange(in_ann_med_wage) %>%
  ggplot() + geom_point(aes(y = in_ann_med_wage, x = proj_ann_jobs, color = program)) + theme_minimal()


# CER_CC

cte.table %>% filter(deg.type == "CER_CC") %>% 
  ggplot() + geom_point(aes(y = program, x = proj_ann_jobs, color = occupation_title_3)) + theme_minimal()

cte.table %>% filter(deg.type == "CER_CC") %>% 
  ggplot() + geom_point(aes(y = program, x = in_ann_med_wage, color = occupation_title_3)) + theme_minimal()

cte.table %>% filter(deg.type == "CER_CC") %>% 
  ggplot() + geom_point(aes(y = in_ann_med_wage, x = proj_ann_jobs, color = program)) + theme_minimal()

cte.table %>% filter(deg.type == "CER_CC") %>% 
  ggplot() + geom_point(aes(y = in_ann_med_wage, x = proj_ann_jobs, color = occupation_title_3)) + theme_minimal()


# AAS and AS

cte.table %>% filter(deg.type == "AAS" | deg.type == "AS") %>% 
  ggplot() + geom_point(aes(y = program, x = proj_ann_jobs, color = occupation_title_3)) + guides(color = "none") + theme_minimal()

cte.table %>% filter(deg.type == "AAS" | deg.type == "AS") %>% 
  ggplot() + geom_point(aes(y = program, x = in_ann_med_wage, color = occupation_title_3)) + guides(color = "none") + theme_minimal()

cte.table %>% filter(deg.type == "AAS" | deg.type == "AS") %>% 
  ggplot() + geom_point(aes(y = in_ann_med_wage, x = proj_ann_jobs, color = occupation_title_3)) + guides(color = "none") + theme_minimal()

cte.table %>% filter(deg.type == "AAS" | deg.type == "AS") %>% 
  ggplot() + geom_point(aes(y = in_ann_med_wage, x = proj_ann_jobs, color = program))  + theme_minimal()

cte.table %>%  group_by(SOC) %>% tally() %>% arrange(desc(n)) %>%
  ggplot() + geom_point(aes(y = SOC, x = n))  + theme_minimal()

blah <- cte.table %>% filter(deg.type == "AAS" | deg.type == "AS") %>%  group_by(SOC) %>% tally()
blah.1 <- blah %>% filter(n == 1)

cte.table$simwages <- rnorm(231, 35000, sd= 7500)
cte.table$simratio <- abs((cte.table$simwages - cte.table$in_ann_med_wage)/((cte.table$simwages + cte.table$in_ann_med_wage)/2))

cte.table.2 <- cte.table %>% filter(simratio != 2)

cte.table.2 %>% ggplot() + geom_point(aes(x = in_ann_med_wage, y = simwages, color = simratio, fill = simratio)) + 
  guides(color = "none") + 
  geom_abline(intercept = 0, slope = 1) + 
  labs(title = "Simulated graphic of SLCC students to comparison SOC", x = "actual SOC inexperienced median wages", y = "simulated median SLCC wages by SOC") +
  coord_cartesian(xlim = c(0, 65000), ylim = c(0, 65000)) +
  theme_minimal()
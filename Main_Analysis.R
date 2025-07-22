## Baseline Table ----
data_processed <- fread("./data/processed_data.csv")

cat_vars <- c("dementia", "Profile", "sex", "race", "drink", "smoke", 
              "bmi_cat", "met_cat", "edu", "income", "diet_score_cat", 
              "cancer", "cvd", "diabetes")

cont_vars <- c("age", "bmi", "tdi", "diet_score")

table1 <- CreateTableOne(
  vars = c(cont_vars, cat_vars),
  strata = "dementia",           
  data = data_processed,          
  factorVars = cat_vars
)

tables1 <- CreateTableOne(
  vars = c(cont_vars, cat_vars),
  strata = "Profile",             
  data = data_processed,
  factorVars = cat_vars
)

## KM ----
fit = survfit(Surv(dementia_time,dementia)~Profile,data=data_processed)

ggsurvplot(fit,
           xlim = c(0,12),
           break.x.by = 1,
           ylim = c(0.997,1),
           pval = T,
           pval.size = 4,
           pval.coord = c(0,0),
           conf.int = F,
           conf.int.style='ribbon',
           title = "Outcome",
           xlab = "Time (years)",
           surv.scale="percent",
           surv.median.line = "hv",
           legend.labs=c("Health","Omnivore","Sweet-tooth"),
           legend.title='Gourp',
           legend = c(0.80,0.88),
           palette = "nejm",
           risk.table = "absolute",
           ggtheme = theme_classic(),
           tables.theme=theme_cleantable(),
           risk.table.fontsize= 4,
           risk.table.height =0.15)

## COX assumption ----
fit_zph = coxph(Surv(dementia_time,dementia) ~
                  Profile + age + sex + race + drink + smoke + bmi +
                  met + edu + income + diet_score + cancer + cvd + diabetes,
                data = data_processed)
summary(fit_zph)
cox.zph(fit_zph)

library(car)
vif(fit_zph)


## COX regression with Dementia ----
cox_dementia <- coxph(
  formula = Surv(dementia_time, dementia) ~ Profile + age + sex + race + drink + smoke + 
    bmi + met + edu + income + tdi + diet_score + cancer + cvd + diabetes,
  data = data_processed
)

cox_AD <- coxph(
  formula = Surv(AD_time, AD) ~ Profile + age + sex + race + drink + smoke + 
    bmi + met + edu + income + tdi + diet_score + cancer + cvd + diabetes,
  data = data_processed
)

cox_VD <- coxph(
  formula = Surv(VD_time, VD) ~ Profile + age + sex + race + drink + smoke + 
    bmi + met + edu + income + tdi + diet_score + cancer + cvd + diabetes,
  data = data_processed
)



## Linear regression with Brain structure ----
brain_data = fread("./data/brain_data.csv") %>% 
  mutate(
    brain_volume_s = scale(brain_volume),
    gm_volume_s = scale(gm_volume),
    wm_volume_s = scale(wm_volume),
    left_hippogm_volume_s = scale(left_hippogm_volume),
    right_hippogm_volume_s = scale(right_hippogm_volume),
    wmh_s = scale(wmh),  
    wmh_s_log = log(wmh_s + 1)
  ) %>% 
  select(eid, brain_volume_s, gm_volume_s, wm_volume_s, 
         left_hippogm_volume_s, right_hippogm_volume_s, wmh_s_log)

y_vars <- c("brain_volume_s", "gm_volume_s", "wm_volume_s", 
            "left_hippogm_volume_s", "right_hippogm_volume_s", "wmh_s_log")

covariates <- c("age", "sex", "race", "drink", "smoke", "bmi", "met",
                "edu", "income", "tdi", "diet_score", "cancer", "cvd", "diabetes")

formula_str <- paste("`%s` ~ Profile +", paste(covariates, collapse = " + "))

brain_results <- list()

for (y in y_vars) {
  current_formula <- as.formula(sprintf(formula_str, y))
  glm_model <- glm(
    formula = current_formula,
    data = brain_data,
    family = gaussian()
  )
  
  model_summary <- summary(glm_model)
  ci <- confint(glm_model)  
  
  result <- data.frame(
    Outcome = y,
    Variable = rownames(model_summary$coefficients),
    Beta = coef(glm_model),
    CI_lower = ci[, 1],
    CI_upper = ci[, 2],
    P_value = model_summary$coefficients[, "Pr(>|t|)"]
  ) %>% 
    filter(grepl("Profile", Variable))
  brain_results[[y]] <- result
}

brain_results_combined <- do.call(rbind, brain_results)


## Mediation Analysis ----
brain_med <- brain_data %>% 
  filter(Profile != "Omnivore") %>%
  mutate(Profile = factor(Profile, levels = c("Health", "Sweet-tooth"))) %>% 
  na.omit()

mediators <- c("brain_volume_s", "gm_volume_s", "wm_volume_s", 
               "left_hippogm_volume_s", "right_hippogm_volume_s", "wmh_s_log")

all_mediation_results <- list()

for (M in mediators) {
  formula_M <- as.formula(
    paste(M, "~ Profile +", paste(covariates, collapse = " + "))
  )
  model_M <- lm(formula_M, data = brain_med)
  
  formula_Y <- as.formula(
    paste("Surv(dementia_time, dementia) ~ Profile +", M, "+", paste(covariates, collapse = " + "))
  )
  model_Y <- survreg(
    formula = formula_Y,
    data = brain_med,
    dist = "lognormal"
  )
  
  set.seed(12345)
  med_fit <- mediate(
    model.m = model_M,        
    model.y = model_Y,        
    treat = "Profile",        
    mediator = M,             
    sims = 1000,              
    boot = TRUE,              
    conf.level = 0.95,        
    outcome = "dementia_time"
  )
  all_mediation_results[[M]] <- med_fit
}

## Interaction and Joint Analysis with Genetic Risk ----
# Apoe and PRS
genetic_data = fread("./data/genetic_data.csv")

median_prs_ad <- median(genetic_data$prs_ad, na.rm = TRUE)

genetic_data <- genetic_data %>%
  mutate(
    prs_ad_group = ifelse(prs_ad > median_prs_ad, "1", "0"),
    prs_ad_q = factor(prs_ad_group, levels = c("0", "1")),
    combine_prs = interaction(Profile, prs_ad_q, drop = TRUE)
  )


joint_dementia_prs <- coxph(
  formula = Surv(dementia_time, dementia) ~ combine_prs + age + sex + race + drink + smoke + 
    bmi + met + edu + income + tdi + diet_score + cancer + cvd + diabetes,
  data = genetic_data
)

joint_AD_prs <- coxph(
  formula = Surv(AD_time, AD) ~ combine_prs + age + sex + race + drink + smoke + 
    bmi + met + edu + income + tdi + diet_score + cancer + cvd + diabetes,
  data = genetic_data
)

joint_VD_prs <- coxph(
  formula = Surv(VD_time, VD) ~ combine_prs + age + sex + race + drink + smoke + 
    bmi + met + edu + income + tdi + diet_score + cancer + cvd + diabetes,
  data = genetic_data
)

base_model_dementia = coxph(
  formula = Surv(dementia_time, dementia) ~ Profile + age + sex + race + drink + smoke + 
    bmi + met + edu + income + tdi + diet_score + cancer + cvd + diabetes,
  data = genetic_data
)

gen_model_dementia = coxph(
  formula = Surv(dementia_time, dementia) ~ Profile*prs_ad + age + sex + race + drink + smoke + 
    bmi + met + edu + income + tdi + diet_score + cancer + cvd + diabetes,
  data = genetic_data
)

lmtest::lrtest(base_model_dementia,gen_model_dementia)









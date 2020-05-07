# ----------------------------------------------------
#   Data Cleaning
# ----------------------------------------------------


library("here")
library("magrittr")
library("tidyverse")
library("naniar")
library("broom")
library("tidylog")
library("stargazer")
library("nnet")
library("foreign")


pilot <- read_csv(here("data", "anes_pilot_2019.csv"))
pilot


slim_pilot <- as_tibble(pilot) %>%
  select(
    caseid,
    party_id = pid1d,
    birthyr,
    gender,
    health = health1a,
    bmi,
    smoke = smoker2,
    exercise,
    vote = vote20jb
  )



# ----------------------------------------------------
#   Codebook
# ----------------------------------------------------

# caseid: Case ID
  # n = 3165

# party_id: "Generally speaking, do you usually think of yourself as a Democrat,
#   a Republican, an independent, or what?"
  # Democrat (1), Republican (2), Independent (3), Something else (4), No answer (-7),
  # Inapplicable, legitimate skip (-1)

# birthyr: Birth Year
  # range: 1926 - 2000

# gender: Gender
  # Male (1), Female (2)

# health: "Would you say in general your health is:"
  # Poor (1), Fair (2), Good (3), Very good (4), Excellent (5),
  # Inapplicable, legitimate skip (-1)

# bmi: Body Mass Index, computed from height and weight
  # Range: 5.1355 - 3232.1361

# smoke: "Do you now smoke cigarettes every day, some days, or not at all?"
  # Every day (1), Some days (2), Not at all (3)

# exercise: "How often do you work out or exercise?"
  # Never (1), A few times a year (2), Once or twice a month (3), Several times a week (4),
  # Every day (5), No answer (-7), Inapplicable, legitimate skip (-1)

# vote: "If the 2020 presidential election were between Donald Trump for the Republicans
#   and Joe Biden for the Democrats, would you vote for Donald Trump, Joe Biden, someone else,
#   or probably not vote?"
  # Donald Trump (1), Joe Biden (2), someone else (3), probably not vote (4)



# ----------------------------------------------------
#   Recode Variables
# ----------------------------------------------------


na_strings <- c(-1, -7)


slim_pilot <- slim_pilot %>%
  replace_with_na_all(condition = ~.x %in% na_strings)


# Party

slim_pilot <- slim_pilot %>%
  mutate(
    party = case_when(
      party_id == 1 ~ "0",
      party_id == 2 ~ "1"
    )
  )


# Age

slim_pilot <- slim_pilot %>%
  mutate(
    age = 2018 - birthyr
  )



# ----------------------------------------------------
#   Statistical Models
# ----------------------------------------------------

# party_id: indicator
# age: discrete
# gender: indicator
# health: likert
# bmi: categorical
# smoke: indicator
# exercise: likert
# vote: unordered categorical (multinomial)


slim_pilot$vote <- as.factor(slim_pilot$vote)

table(slim_pilot$vote)

slim_pilot$vote2 = relevel(slim_pilot$vote, ref = 4)

table(slim_pilot$vote2)

reg <- multinom(vote2 ~ health + bmi + smoke + exercise + age + gender + party_id, data = slim_pilot)

reg_tidy <- tidy(reg, conf.int = TRUE)
reg_tidy

texreg::screenreg(list(reg))



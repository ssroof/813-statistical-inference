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
    hospital,
    bmi,
    hearing = disable1,
    sight = disable2,
    mental = disable3,
    mobility = disable4,
    dress = disable5,
    errands = disable6,
    smoke = smoker1,
    smoke_freq = smoker2,
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

# comp_health: "Compared to others your age, would you say that in general your health is:"
  # Poor (1), Fair (2), Good (3), Very good (4), Excellent (5),
  # Inapplicable, legitimate skip (-1)

# hospital: "Have you or a member of your immediate family spent the night in a hospital
#   in the past 12 months?"
  # This has happened in the past year (1)
  # This has not happened n the past year (2)
  # Inapplicable, legitimate skip (-1)

# bmi: Body Mass Index, computed from height and weight
  # Range: 5.1355 - 3232.1361

# "Which of the following conditions do you have?"

# - hearing: "Deafness or difficulty hearing."
  # I have this (1), I do not have this (2), No answer (-7), inapplicable, legitimate skip (-1)

# - sight: "Blindness or serious difficulty seeing even when wearing glasses."
  # I have this (1), I do not have this (2), No answer (-7), inapplicable, legitimate skip (-1)

# - mental: "Serious difficulty concentrating, remembering, or making decisions due to a physical,
#     mental, or emotional condition."
  # I have this (1), I do not have this (2), No answer (-7), inapplicable, legitimate skip (-1)

# - mobility: "Serious difficulty walking or climbing stairs."
  # I have this (1), I do not have this (2), No answer (-7), inapplicable, legitimate skip (-1)

# - dress: "Difficulty dressing or bathing."
  # I have this (1), I do not have this (2), No answer (-7), inapplicable, legitimate skip (-1)

# - errands: "Difficulty doing errands alone such as visiting a doctor's office or shopping due
#     to a physical, mental, or emotional condition."
  # I have this (1), I do not have this (2), No answer (-7), inapplicable, legitimate skip (-1)

# smoke: "Have you smoked at least 100 cigarettes in your entire life?"
  # Yes (1), No (2), Inapplicable, legitimate skip (-1)

# smoke_freq: "Do you smoke cigarettes every day, some days, or not at all?"
  # Every day (1), Some days (2), Not at all (3), Inapplicable, legitimate skip (-1)

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
# comp_health: likert
# hospital: indicator
# bmi: continuous
# hearing: indicator
# sight: indicator
# mental: indicator
# mobility: indicator
# dress: indicator
# errands: indicator
# smoke: indicator
# smoke_freq: likert
# exercise: likert
# vote: unordered categorical (multinomial)


slim_pilot$vote <- as.factor(slim_pilot$vote)

table(slim_pilot$vote)

slim_pilot$vote2 = relevel(slim_pilot$vote, ref = 4)

table(slim_pilot$vote2)

reg <- multinom(vote2 ~ health + hospital + bmi + hearing + sight + mobility + dress
                + errands + smoke + smoke_freq + exercise, data = slim_pilot)

reg_tidy <- tidy(reg, conf.int = TRUE)
reg_tidy

stargazer(reg, type = "text", out = "reg.htm")















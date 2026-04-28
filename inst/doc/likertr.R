## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5,
  # = "80%",
  fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(likertr)

## ----include = TRUE-----------------------------------------------------------
small_data <- data[c(1, 3:7, 11:15)]
str(small_data)

## ----include = TRUE-----------------------------------------------------------
likert_analysis <- likertr(small_data)

## ----eval = FALSE-------------------------------------------------------------
# summary(likert_analysis)

## ----include = TRUE-----------------------------------------------------------
summary(likert_analysis,
  efa_summary = FALSE,
  reliability_summary = FALSE, inference_summary = FALSE
)

## ----include = TRUE-----------------------------------------------------------
summary(likert_analysis,
  data_summary = FALSE,
  reliability_summary = FALSE, inference_summary = FALSE
)

## ----include = TRUE-----------------------------------------------------------
summary(likert_analysis,
  data_summary = FALSE, efa_summary = FALSE,
  inference_summary = FALSE
)

## ----include = TRUE-----------------------------------------------------------
summary(likert_analysis,
  data_summary = FALSE,
  efa_summary = FALSE, reliability_summary = FALSE
)

## ----include = TRUE-----------------------------------------------------------
plot(likert_analysis)

## ----echo = TRUE, results = 'hide'--------------------------------------------
clean_analysis <- likertr(small_data,
  na_drop = TRUE,
  ipsatize_decision = TRUE,
  small_n_drop = TRUE
)

summary(clean_analysis,
  efa_summary = FALSE,
  reliability_summary = FALSE, inference_summary = FALSE
)

## ----echo = TRUE, results = 'hide'--------------------------------------------
inference_analysis <- likertr(small_data,
  na_drop = TRUE,
  ipsatize_decision = TRUE,
  small_n_drop = TRUE,
  category = 1
)

summary(inference_analysis,
  data_summary = FALSE,
  efa_summary = FALSE, reliability_summary = FALSE
)


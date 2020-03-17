basic_model <- function(x, b, d) 1 / (1 + ((x - b) / d)^2)

arule_models <- function(model, x, b, b_low, b_hi, b1, b2, d) {
  if(model == 1) fuzz <- basic_model(x, b, d)
  if(model == 2) fuzz <- dplyr::case_when(x > b_low & x < b_hi ~ 1,
                                          x <= b_low ~ basic_model(x, b_low, d),
                                          x >= b_hi ~ basic_model(x, b_hi, d))
  if(model == 3) fuzz <- dplyr::case_when(x > b1 & x < b2 ~ 1,
                                          x <= b1 ~ basic_model(x, b1, d),
                                          x >= b2 ~ basic_model(x, b2, d))
  if(model == 4) fuzz <- dplyr::if_else(x > b, 1, basic_model(x, b, d))
  if(model == 5) fuzz <- dplyr::case_when(x < b, 1, basic_model(x, b, d))

  fuzz * 100
}





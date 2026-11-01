# #user bata input lina 
#Classwork: Mean, SD and CV functions with input validation
# dbh_cm <- readline(prompt= "enter dbh in cm: ")
# dbh_cm <- strsplit(dbh_cm, ",")[[1]]
# dbh_cm <- as.numeric(dbh_cm)


# if(condition){
# dbh_cm
# }

# #user input ko validity check garna 
# #first ma ta numeric ho ki nai
# if (is.na(dbh_cm)){
#   stop("dbh must be numeric so numeric value hala")
# }

# #number tara negative yata zero ho ki nai
# if dbh_cm <=0{
#     stop("dbh zero hudaina ani negative pani hudaina so positive value hala")
# }

# print(dbh_cm)

# #mean claculate garne function banauna
# sum(dbh_cm)
# length (dbh_cm)
# my_mean <- sum(dbh_cm)/length(dbh_cm)
# my_mean


# #standard deviation calcualte garne function banauna 
# #first ma ta each no ko mean sita ko difference chaiyo ani teshlai square garni ani sabai square lai jodni
# #numerator
# difference <- dbh_cm - my_mean
# difference
# squaring_difference <- difference^2
# squaring_difference
# sum_of_squares_numerator <- sum(squaring_difference)
# sum_of_squares_numerator 
# #denomenator ma n-1 hunxa
# n <- length(dbh_cm)
# n
# denomenator <- n-1
# denomenator
# #calculate garna
# standard_deviation <- sqrt(sum_of_squares_numerator/ denomenator )
# standard_deviation

# #coefficient of variation
# coefficient_of_variation <- ((standard_deviation/ my_mean)*100){
#     dhn
#     standard_deviation
#     my_mean
#     coefficient_of_variation
# }

dbh_cm <- readline(prompt= "enter dbh in cm: ")
dbh_cm <- strsplit(dbh_cm, ",")[[1]]
dbh_cm <- as.numeric(dbh_cm)



check_input <- function(x) {
  if (!is.numeric(x)) {
    stop("Input numeric hunu parxa (number matra halnus)")     
  }
  if (any(is.na(x))) {
    stop("Input ma NA wa number nabhayeko value xa")      
  }
  if (any(x <= 0)) {
    stop("Negative wa zero value hunu hudaina ")
  }
}
calculate_mean <- function(x) {
  check_input(x)
  sum(x) / length(x)
}

calculate_sd <- function(x) {
  check_input(x)
  data_mean <- calculate_mean(x)
  sum_of_squares <- sum((x - data_mean)^2)
  sqrt(sum_of_squares / (length(x) - 1))
}

calculate_mean(c(1,2,3,4,5))
calculate_mean(c(1,2,3,4,6,5,23,4))

calculate_sd(c(1,2,3,4,5))
calculate_sd(c(1,2,3,4,6,5,23,4))


calculate_cv <- function(x) {
  check_input(x)
  sd <- calculate_sd(x)
  mean <- calculate_mean(x)
  (sd / mean) * 100
}

calculate_cv(c(100,105,106,-107,108))
calculate_cv(c(5,55,56,5,65,54))
calculate_cv(c(10,20,30))


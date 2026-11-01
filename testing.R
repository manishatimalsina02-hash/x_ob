
# 01_data_types.R — basic data types in R

dbh_num     <- 23.5               # tree diameter (cm), a decimal number
tree_count  <- 12L                # number of trees; the L makes it an integer
species_chr <- "Shorea robusta"   # species name (sal), text
is_alive    <- TRUE               # TRUE/FALSE value

# Inspect types using class() and typeof()
class(dbh_num)        # "numeric"
typeof(dbh_num)       # "double"

class(tree_count)     # "integer"
typeof(tree_count)    # "integer"

class(species_chr)    # "character"
typeof(species_chr)   # "character"

class(is_alive)       # "logical"
typeof(is_alive)      # "logical"

#data type changes



x <- "25"               # this is text as it has quotes
class(x)                # "character"

as.numeric(x)           # 25   (aaba yo decimal number ma gayo)
as.integer(x)           # 25   (aaba yo whole number ma gayo)

#manau raw data ko dbh chai messy xa kunai data ma decinam xa kunai data ma whole number xa. so we will convert all dbh data into decimal number using as.numeric() function.


# Messy DBH values chai manu yo ho 
dbh_raw <- c("23.5", " 24 ", "18.2 cm", "30,5", "", "NA", "twenty", "-5", "450")

class(dbh_raw)            # "character", so you can't do maths yet kina ki data types nai xuttai ho ni ta 
dbh <- trimws(dbh_raw)                 # 1. extra spaces remove huncxa like  " 24 " -> "24"
dbh <- gsub("cm", "", dbh)             # 2. paxadi ko unit remove garnu paryo bhane "18.2 cm" -> "18.2 "
dbh <- trimws(dbh)                     #    tidy spaces hatauna feri
dbh <- gsub(",", ".", dbh)             # 3. comma as decimal: "30,5" -> "30.5"
dbh[dbh == ""] <- NA                   # 4. empty cells become missing (NA)
dbh <- suppressWarnings(as.numeric(dbh))  # 5. convert to numbers; "twenty" -> NA

dbh >20
height = 11
# 23.5 24.0 18.2 30.5 NA NA NA -5.0 450.0

plot_code_factor <- factor(c("101", "102", "103"))
as.numeric(plot_code_factor)
as.numeric(as.character(plot_code_factor))

#R ko characteristic as per data type hiearchy
mixed_vec <- c(28.8, TRUE, "Shorea robusta")
class(mixed_vec)  # "character"  (everything is coerced to character)

#reational (comparision)  operators:>, <, >=, <=, == equal , != not equal
species_chr == "Shorea robusta"  # TRUE
species_chr != "Shorea robusta"  # FALSE

(dbh >30) | (height >30)  # then it is tree 
(dbh >10) & (height >30) #then it is pole

#conditional decision making 
dbh <- c(23.5, 24.0, 18.2, 30.5, NA, NA, NA, -5.0, 450.0)
if (dbh > 30) {
  print("This is a tree")
} else if (dbh > 10) {
  print("This is a pole")
} else {
  print("This is a sapling")
}

dbh <- c(23, 24, 18, 30, 450)
(trees_ba <- pi * (dbh^2) / 40000)
dbh <- c(23, 24, 18, 30, 450)
ifelse(dbh > 30, yes= "big tree", no = "small tree")

height <- c(11.9, 12, 9, 15, 20, NA, 25)
mean(height, na.rm = TRUE)
is.na(height)
sum(is.na(height))

test_trees <- c(28.4, -12.0, 0, 999, NA)
test_trees <= 0
test_trees > 150 & !is.na(test_trees)

any (test_trees <=0, na.rm = TRUE) # NA HATAUXA ANI BAKI VALUE MA KUNIA NEGATIVE NO XA KI NAI HERXA RESULT M AJUN TRUE 


# FUNCTIONS
# Functions are a way to group code together and reuse it.

#CALCULATE BASAL AREA OF TREE
calculate_basal_area <- function (dbh_cm) {
    if (!is.numeric (dbh_cm)) {
        stop("dbh_cm must be numeric")
    }
    if (any(dbh_cm <= 0, na.rm = TRUE)) {
        stop("dbh_cm must be non-negative and non-zero")
    }
    if (any(dbh_cm > 150, na.rm = TRUE)) {
        warning("dbh_cm must be less than or equal to 150 cm")
    }
  ba_m2 <- pi * (dbh_cm^2) / 40000
  return(ba_m2)
}

calculate_volume <- function(dbh_cm, height_m, form_factor = 0.5) {
  if (!is.numeric(dbh_cm)) {
    stop("dbh lai numeric ma rakha na")
  }
  if (!is.numeric(height_m)) {
    stop("height lai numeric ma rakha na")
  }
  if (any(dbh_cm <= 0, na.rm = TRUE)) {
    stop("dbh negative ma hunna k ani zero pani ta huna mildaina ni ta")
  }
  if (any(height_m <= 0, na.rm = TRUE)) {
    stop("height pani ta negative hunna ni ani zero pani hunna")
  }
  volume_m3 <- calculate_basal_area(dbh_cm) * form_factor * height_m
  return(volume_m3)
}

calculate_basal_area(c(23,24,28,12,120))
calculate_volume (c(23,24,28,12,120), c(11,12,9,15,20))

trees <- read.csv("./data/raw/unit2_starter_trees.csv")
head(trees) #first ko 6 wota row matrai read garxa r ko feature nai ho.
head(trees, n=10) #jamma 10 wota xa ni ta data sabai read garxa ra tyo 10 bhanda badi number rakhe pani huna chai 10 wota nai read garxa)
nrow(trees) #no of rows calculate garxa
ncol(trees) #no of columns calculate garxa
str(trees) #structure dekhauxax ani data type dekhauxa
names(trees) # column names dekhauxa

#access a single column usning $ 
trees$dbh_cm

calc_cv <- function(x) {
  cv <- sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
  return(cv)
}
#appply custom function
calc_cv(trees$dbh_cm)
trees$ba_m2 <- calculate_basal_area(trees$dbh_cm)
head(trees)

trees$volume_m3 <- calculate_volume(trees$dbh_cm, trees$height_m)
head(trees)


#to show dbh values rthan are greater than 3
trees$dbh_cm[trees$dbh_cm > 30]
tree$species[trees$dbh_cm > 30]
which.max(trees$dbh_cm)
trees[which.max(trees$dbh_cm)]

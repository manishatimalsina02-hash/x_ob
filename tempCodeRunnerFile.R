
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

dbh
# 23.5 24.0 18.2 30.5 NA NA NA -5.0 450.0

plot_code_factor <- factor(c("101", "102", "103"))
as.numeric(plot_code_factor)
as.numeric(as.character(plot_code_factor))

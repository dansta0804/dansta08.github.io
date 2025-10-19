# nolint start

# Path declarations:
PROJECT     <- "./"
PATTERN <- "^[a-ząčęėįšųūž]+$"
PCODE <- "^[0-9]+$"
EMAIL <- "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
PHONE <- "^\\+370 ?[0-9]{3} ?[0-9]{5}$"
TEXTS <- "^[a-z0-9ąčęėįšųū\\.,\\-]+$"

# User name validations:
validate_name <- function(name) {
  errors <- c()
  hideFeedback("name")

	if (name == "") {
		errors <- c(errors, "Privaloma nurodyti vardą!")
	}

	if (grepl(PATTERN, name, ignore.case = TRUE) == FALSE && name != "") {
		errors <- c(errors, "Varde gali būti tik raidės!")
	}

	if (nchar(name) > 25) {
		errors <- c(errors, "Vardo negali sudaryti daugiau nei 25 simboliai!")
	}

	if (length(errors) > 0) {
		showFeedbackDanger("name", paste(errors, collapse = "<br>"),
						   color = "red")
		return(errors)
	}
}

# User surname validations:
validate_surname <- function(surname) {
	errors <- c()
	hideFeedback("surname")

	if (surname == "") {
		errors <- c(errors, "Privaloma nurodyti pavardę!")
	}

	if (grepl(PATTERN, surname, ignore.case = TRUE) == FALSE && surname != "") {
		errors <- c(errors, "Pavardėje gali būti tik raidės!")
	}

	if (nchar(surname) > 35) {
		errors <-
			c(errors, "Pavardės negali sudaryti daugiau nei 35 simboliai!")
	}

	if (length(errors) > 0) {
		showFeedbackDanger("surname", paste(errors, collapse = "<br>"),
						   color = "red")
		return(errors)
	}
}

# User personal code validations:
validate_personal_code <- function(personal_code) {
	errors <- c()
	hideFeedback("personal_code")

	if (personal_code == "") {
		errors <- c(errors, "Privaloma nurodyti asmens kodą!")
	}

	if (grepl(PCODE, personal_code) == FALSE && personal_code != "") {
		errors <- c(errors, "Asmens kodą gali sudaryti tik skaitmenys!")
	}

	if (nchar(personal_code) > 11) {
		errors <-
			c(errors, "Asmens kodo negali sudaryti daugiau nei 11 skaitmenų!")
	}

	if (length(errors) > 0) {
		showFeedbackDanger("personal_code", paste(errors, collapse = "<br>"),
						   color = "red")
		return(errors)
	}
}

# User e-mail address validations:
validate_email <- function(input_id, text_value) {
	errors <- c()
	hideFeedback(input_id)

	if (grepl(EMAIL, text_value) == FALSE && text_value != "") {
		errors <- c(errors, "Įvesti neteisingi el. pašto simboliai!")
	}

	if (nchar(text_value) > 50) {
		errors <-
			c(errors, "El. paštą negali sudaryti daugiau nei 50 simbolių!")
	}

	if (length(errors) > 0) {
		showFeedbackDanger(inputId = input_id, paste(errors, collapse = "<br>"),
						   color = "red")
		return(errors)
	}
}

# User phone number validations:
validate_phone <- function(input_id, text_value) {
	errors <- c()
	hideFeedback(input_id)

	if (text_value == "") {
		errors <- c(errors, "Privaloma nurodyti telefono numerį!")
	}

	if (grepl(PHONE, text_value) == FALSE && text_value != "") {
		errors <- c(errors, "Telefono numeris neatitinka formato!")
	}

	if (nchar(text_value) > 14) {
		errors <-
			c(errors, "Tel. numerio negali sudaryti daugiau nei 14 simbolių!")
	}

	if (length(errors) > 0) {
		showFeedbackDanger(inputId = input_id, paste(errors, collapse = "<br>"),
						   color = "red")
		return(errors)
	}
}

# User address validations:
validate_address <- function(address) {
	errors <- c()
	hideFeedback("address")

	if (nchar(address) > 40) {
		errors <- c(errors, "Adreso negali sudaryti daugiau nei 40 simbolių!")
	}

	if (length(errors) > 0) {
		showFeedbackDanger("address", paste(errors, collapse = "<br>"),
						   color = "red")
		return(errors)
	}
}

# User gender validations:
validate_gender <- function(gender) {
	errors <- c()
	hideFeedback("gender")

	if (gender == "NN") {
		errors <- c(errors, "Privaloma nurodyti lytį!")
	}

	if (length(errors) > 0) {
		showFeedbackDanger("gender", paste(errors, collapse = "<br>"),
						   color = "red")
		return(errors)
	}
}

# User role validations:
validate_role <- function(role) {
	errors <- c()
	hideFeedback("role")

	if (role == "NN") {
		errors <- c(errors, "Privaloma pasirinkti kategoriją!")
	}

	if (length(errors) > 0) {
		showFeedbackDanger("role", paste(errors, collapse = "<br>"),
						   color = "red")
		return(errors)
	}
}

# Doctor licence validations:
validate_licence <- function(licence) {
	errors <- c()
	hideFeedback("licence")

	if (nchar(licence) > 6) {
		errors <-
			c(errors, "Numerio negali sudaryti daugiau nei 6 simboliai!")
	}

	if (length(errors) > 0) {
		showFeedbackDanger("licence", paste(errors, collapse = "<br>"),
						   color = "red")
		return(errors)
	}
}

# Doctor/researcher institution validations:
validate_text <- function(input_id, text_value) {
	errors <- c()
	hideFeedback(input_id)

	if (grepl(TEXTS, text_value, ignore.case = TRUE) == FALSE &&
		text_value != "") {
		errors <- c(errors, "Pavadinime yra neleistinų simbolių!")
	}
	
	if (length(errors) > 0) {
		showFeedbackDanger(inputId = input_id, paste(errors, collapse = "<br>"),
						   color = "red")
		return(errors)
	}
}

phone_email_exists <- function(input_id, text_value) {
	errors <- c()
	hideFeedback(input_id)

	if (text_value == "") {
		errors <-
			c(errors, "Privaloma nurodyti telefono numerį arba el. pašto adresą!")
	}
	
	if (length(errors) > 0) {
		showFeedbackDanger(inputId = input_id, paste(errors, collapse = "<br>"),
						   color = "red")
		return(errors)
	}
}
# nolint end
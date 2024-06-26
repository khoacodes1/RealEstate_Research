library(mice)

#Applying MICE

imputed_data <- mice(cleaned_data, method = "pmm", m = 5)


# Access the imputed datasets

imputed_data <- complete(imputed_data)

write.csv(imputed_data, file = "imputed_data", row.names = FALSE)

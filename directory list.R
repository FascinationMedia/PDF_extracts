#get a list of all files in a directory
# Specify the directory path
directory_path <- "C:\DEV\R\data\pastpapers"

# Check if the directory exists
return(ifelse(dir.exists(directory_path),TRUE,FALSE)) 


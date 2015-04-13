# Main file which calls the convertEval function iteratively

# Preamble
library(stringr)
library(randomNames)

# Load function
source("convertEval.R")

# Get the list of pdf files in the current directory
fList <- list.files()[str_detect(list.files(),"pdf$")]

# Build up iteratively in O(n^2) time (note that this will take a really long 
# time so be patient)
for (i in 1:length(fList)) {
  if (i==1) {
    eval.df <- convertEval(fList[i], str_sub(fList[i],1,4))
  } else {
    eval.df <- rbind(eval.df,
                     convertEval(fList[i], str_sub(fList[i],1,4)))
  }
}

# 1119profs.pdf could not be parsed :(

# Clean up a bit
output.df <- eval.df
output.df$Course <- str_trim(output.df$Course) 
output.df <- subset(eval.df,
                    !grepl("^[0-9]+|[A-Z]+$",eval.df$Course) &
                    !is.na(eval.df$Course))
rm(eval.df,fList,i)

# Output as a csv (Optional)
# write.csv(output.df,"evalData.csv",row.names = FALSE)

# Randomize and output for Shiny (Optional)
random.df <- subset(output.df,select = c(-First.Name,-Last.Name))
vec.instructors <- unique(random.df$Instructor)
set.seed(7777)
vec.names.random <- randomNames(length(vec.instructors))
instructors.map <- data.frame(o_Instructor = vec.instructors,
                              Instructor.Random = vec.names.random)
random.df <- merge(random.df, instructors.map,
                   by.x = "Instructor", by.y="o_Instructor")
random.df <- subset(random.df, select=-Instructor)
save(random.df,file="shiny/evalData_random.RData")
rm(vec.instructors,vec.names.random)

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

# Order the factors
response.levels <- c("Excellent","Too High","Always","Very Interesting","90-100%",
                      "Very helpful","Too much","0-2 hours",
                    "Good","Somewhat too high","Most of the time","Interesting","75-90%",
                      "Helpful","Somewhat too much","3-6 hours",
                    "Satisfactory","Just right","Often enough","Not Interesting","50-75%",
                      "Not helpful","Okay","7-10 hours",
                    "Unsatisfactory","Somewhat too low","Not often enough","Very poor",
                      "No work assigned","No printed course notes","25-50%",
                    "No text required","Somewhat too little","11-15 hours",
                      "Too low","Never","< 25%","Too little","> 15 hours",
                    "I did not seek help" ,"No opinion")
order.df <- random.df
order.df$Response <- factor(order.df$Response,levels=response.levels) 

# Output and clean up
save(order.df,file="shiny/evalData_random.RData")
rm(output.df,response.levels)

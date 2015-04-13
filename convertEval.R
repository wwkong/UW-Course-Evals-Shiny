# ======================================================
# Name:         convertEval
# I/O :         PATH + Date String -> Data Frame OR NULL
# Description:  Takes in a path to the PDF file, as
#               well as a date stamp and creates a 
#               parsed data frame
# =====================================================

convertEval <- function (path, inDate) {
  
  # Keep track
  print(path)
  
  # ---- Preamble ----
  library(tm)
  library(stringr)
  library(tidyr)
  library(dplyr) 
  
  # ---- Read PDF ----
  pdf <- readPDF(engine="xpdf")
  dat <- pdf(elem = list(uri=path), language='en', id='id1')
  date <- inDate
#   dat <- pdf(elem = list(uri="1109profs.pdf"), language='en', id='id1')
#   date <- 1119
  text <- data.frame(lines=dat$content)
  
  # Case for when the input cannot be read
  if (nrow(text) == 1) return(NULL)
  
  # ---- Import and clean ----
  clean.text <- subset(text,  
                       
                         lines != "" &
                         !grepl("Fall",lines) &
                         !grepl("Spring",lines) &
                         !grepl("Winter",lines) &
                         !grepl("Course Evaluations",lines) &
                         !grepl("^[0-9]+\\)",lines) &
                         !grepl("\\bPage [0-9]+\\b",lines) &
                         !grepl("Characteristics", lines) &
                         !grepl("At what level", lines) &
                         !grepl("Was the instructor", lines) &
                         !grepl("Evaluate", lines) &
                         !grepl("proportion", lines) &
                         !grepl("course interesting", lines) &
                         TRUE
                       
                       )
  
  relevant.text <- data.frame(lines=as.character(
                    clean.text[1:(grep("\\bIndex by Course\\b",clean.text$lines)-1),]))
  
  relevant.text$stats.lines <- with(relevant.text,
                                      
                                      grepl("\\bExcellent", lines) |
                                      grepl("\\bToo High", lines) |
                                      grepl("\\bAlways", lines) |
                                      grepl("\\b90-100", lines) |
                                      grepl("\\bVery helpful", lines) |
                                      grepl("\\bVery interesting", lines) |
                                      grepl("\\bToo much", lines) |
                                      grepl("\\b0-2 hours", lines) |
                                      grepl("^[0-9]+\\%", lines) |
                                      FALSE
                                    
                                    )
  
  relevant.text$course.lines <- with(relevant.text,
                                    
                                      grepl("^[A-Z]+[ ]*[0-9/]+[A-Z]*", lines) |
                                      FALSE
                                    
  )
  
  relevant.text$instructor.lines <- with(relevant.text,
                                     
                                      !stats.lines &
                                      !course.lines  
                                     
  )
  relevant.text$not.stats.lines <- !relevant.text$stats.lines 
  
  # ---- Create Course IDs ----
  course.id <- c()
  iter.id <- 1
  is.new.section <- FALSE
  for (i in 1:(nrow(relevant.text))) {
    
    # Check for a break
    if(relevant.text$stats.lines[i] && 
       relevant.text$not.stats.lines[i+1] &&
       i < (nrow(relevant.text)-1)) {
      is.new.section <- TRUE
    }
    
    # Iterate
    if (!is.new.section) {
      course.id <- c(course.id,iter.id)
    } else {
      course.id <- c(course.id,iter.id)
      iter.id <- iter.id + 1
      is.new.section <- FALSE
    }
       
  }
  relevant.text$course.id <- as.factor(course.id)
  
  # Sort and clean
  sorted.text <- relevant.text[order(relevant.text$course.id,
                                     -relevant.text$instructor.lines),]
  sorted.text$lines <- as.character(sorted.text$lines)
  
  # ---- Group the Analysis by the Course ----
  
  # Custom function to parse line data 
  splitLineData <- function(str, index) {
    str.lst <-  sapply(str_split(str,",")[[1]], function(s) str_split(s,":"))
    str.out <- lapply(str.lst,function(v) as.numeric(str_trim(v[2])))
    for (i in 1:length(str.lst)) {
      names(str.out)[i] <- paste0("Q",index," ",str.lst[[i]][1])
    }
    return(str.out)
  }
  
  # Custom function to parse by course ID
  getCourseInfo <- function(df.sub) {
    
    # Subset the data for analysis
    instructor.info <- as.character(subset(df.sub, instructor.lines)$lines)
    course.info <- str_replace(subset(df.sub, course.lines)$lines,"[\\(UAE\\)]* Section [0-9]+","")
    stats.info <- subset(df.sub, stats.lines)
    
    # Base info
    out.lst <- list(instructors=instructor.info,
                    course=str_split(course.info,"/")[[1]])
  
    # Additional info pertaining to questions
    for (i in 1:(nrow(stats.info)-1)) {
      out.lst <- c(out.lst,splitLineData(stats.info$lines[i],i))
    }
  
    # The last statistic is a special case
    sample.info <- str_extract(stats.info$lines[nrow(stats.info)],"\\(.+\\)")
    sample.reply <- str_extract_all(sample.info,"[0-9]+")[[1]][1]
    sample.tot <- str_extract_all(sample.info,"[0-9]+")[[1]][2]
    out.lst <- c(out.lst, list(Sample.n=as.numeric(sample.reply),
                               Size.n=as.numeric(sample.tot)))
  }
  parsed.text <- by(sorted.text,course.id,getCourseInfo)
  
  # ---- Convert to vectors / DF ----
  cur.instructor <- "John Smith" # Placeholder
  for (i in 1:length(parsed.text)) {
    
    # Go through each section individually
    section.i <- parsed.text[[i]]
    
    # Some instructors may be missing
    if (length(section.i$instructors) == 0) 
      section.i$instructors <- cur.instructor
    
    # Build up the data
    cur.instructor <- section.i$instructors
    n.repeats <- length(section.i$instructors) * length(section.i$course) 
    # Restart from previous iteration
    vec.courses <- c()
    vec.instructors <- c()
    # Main cross loop
    for (j in 1:length(section.i$instructors)) {
      for (k in 1:length(section.i$course)) {
        vec.instructors <- c(vec.instructors,section.i$instructors[j])
        vec.courses <- c(vec.courses,section.i$course[k])
      }
    }
    df.row <- as.data.frame(section.i[3:length(section.i)])
    names(df.row) <- names(section.i[3:length(section.i)])
    df.rep <- df.row[rep(row.names(df.row), n.repeats),]
    df.pair <- cbind(Instructor=vec.instructors,
                     Course=vec.courses,
                     df.rep)
    
    # Handle the first observation separately
    if (i == 1) 
      df.text <- df.pair
    else 
      df.text <- rbind(df.text, df.pair)
  }
  
  # ---- Convert Wide to Long and Clean up ----
  
  # Melt
  df.long <- melt(df.text, 
                  id.vars=c("Instructor","Course","Sample.n","Size.n"),
                  value.name = "Response.n")
  # Add basic info
  df.clean <- df.long
  df.clean$Course.Prefix <- str_extract(df.clean$Course, "[A-Z]+") 
  df.clean$Course.Suffix <- str_extract(df.clean$Course, "[0-9]+") 
  df.clean$Last.Name <- str_trim(str_extract(df.clean$Instructor, "[A-z]+"))
  df.clean$First.Name <- str_trim(
                          str_replace(
                          str_replace(
                              df.clean$Instructor,",",""),
                              fixed(df.clean$Last.Name),""))
  df.clean$Question.ID <- str_trim(str_extract(df.clean$variable, "Q[0-9]+"))
  df.clean$Response <- str_trim(str_replace(df.clean$variable,fixed(df.clean$Question.ID),""))
  df.clean$Date <- date
  df.clean$Course.Level <- round(as.numeric(df.clean$Course.Suffix),-2)
  df.clean$Course <- str_replace(df.clean$Course," ","")
  
  # Create the dictionary of Questions manually
  df.questions <- data.frame(
    
    Question.ID=c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7",
                  "Q8", "Q9", "Q10","Q11","Q12","Q13","Q14","Q15"),
    
    Question.STR=c("Evaluate the organization and coherence of the lectures.",
                   "At what level were the instructor's explanations aimed?",
                   "Evaluate the instructor's treatment of students' questions.",
                   "Evaluate the effectiveness of the instructor's visual presentation (blackboard, overheads, etc.).",
                   "Evaluate the effectiveness of the instructor's oral presentation.",
                   "Was the instructor available for help outside class?",
                   "Did you find the course interesting?",
                   "Evaluate the overall effectiveness of the instructor as a teacher.",
                   "What proportion of the lectures did you attend in this course?",
                   "Was the assigned work (assignments, projects, etc) helpful in learning the course content?",
                   "Were the printed notes (if any) helpful in learning the course content?",
                   "Was the required textbook (if any) helpful in learning the course content?",
                   "Did the course introduce an appropriate amount of new material?",
                   "Was the amount of assigned work required for the course appropriate?",
                   "On average, how many hours per week did you spend on this course outside of lectures?"
                   
                   ))
  df.merge <- merge(df.clean, df.questions,by="Question.ID")
  
  # Output a select number of rows in a certain order
  df.out <- df.merge %>% select(Date,
                                Instructor, First.Name, Last.Name,
                                Course, Course.Prefix, Course.Suffix,
                                Question.ID, Question.STR,
                                Response, Response.n,
                                Sample.n, Size.n)
  df.out <- df.out[order(df.out$Last.Name,
                         df.out$First.Name),]
  # Return
  return(df.out)
}


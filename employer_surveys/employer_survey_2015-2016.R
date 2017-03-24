# R script to analyze employer survey data
# see 'employerSurvey2015.csv' file for working file

library(reshape2)
library(ggplot2)
library(dplyr)

employerSurvey2015 <- read.csv("employerSurvey2015.csv")

# Assign variables the correct classification
employerSurvey2015$RespondentID <- factor(employerSurvey2015$RespondentID)
employerSurvey2015$fundamentalPrinciples <- ordered(employerSurvey2015$fundamentalPrinciples,
                                                    levels = c("Strongly Agree",
                                                               "Agree",
                                                               "Neutral",
                                                               "Disagree",
                                                               "Strongly Disagree"))

employerSurvey2015$analyzeProblems <- ordered(employerSurvey2015$analyzeProblems,
                                                    levels = c("Strongly Agree",
                                                               "Agree",
                                                               "Neutral",
                                                               "Disagree",
                                                               "Strongly Disagree"))

employerSurvey2015$effectiveCommunicators <- ordered(employerSurvey2015$effectiveCommunicators,
                                                    levels = c("Strongly Agree",
                                                               "Agree",
                                                               "Neutral",
                                                               "Disagree",
                                                               "Strongly Disagree"))

employerSurvey2015$manageInfoResources <- ordered(employerSurvey2015$manageInfoResources,
                                                    levels = c("Strongly Agree",
                                                               "Agree",
                                                               "Neutral",
                                                               "Disagree",
                                                               "Strongly Disagree"))

employerSurvey2015$organizeInformation <- ordered(employerSurvey2015$organizeInformation,
                                                    levels = c("Strongly Agree",
                                                               "Agree",
                                                               "Neutral",
                                                               "Disagree",
                                                               "Strongly Disagree"))

employerSurvey2015$effectiveTechnology <- ordered(employerSurvey2015$effectiveTechnology,
                                                    levels = c("Strongly Agree",
                                                               "Agree",
                                                               "Neutral",
                                                               "Disagree",
                                                               "Strongly Disagree"))

employerSurvey2015$emergingTechnology <- ordered(employerSurvey2015$emergingTechnology,
                                                    levels = c("Strongly Agree",
                                                               "Agree",
                                                               "Neutral",
                                                               "Disagree",
                                                               "Strongly Disagree"))

employerSurvey2015$assistClients <- ordered(employerSurvey2015$assistClients,
                                                    levels = c("Strongly Agree",
                                                               "Agree",
                                                               "Neutral",
                                                               "Disagree",
                                                               "Strongly Disagree"))

employerSurvey2015$promoteEvaluateServices <- ordered(employerSurvey2015$promoteEvaluateServices,
                                                    levels = c("Strongly Agree",
                                                               "Agree",
                                                               "Neutral",
                                                               "Disagree",
                                                               "Strongly Disagree"))

employerSurvey2015$originalResearch <- ordered(employerSurvey2015$originalResearch,
                                                    levels = c("Strongly Agree",
                                                               "Agree",
                                                               "Neutral",
                                                               "Disagree",
                                                               "Strongly Disagree"))

employerSurvey2015$goodLearners <- ordered(employerSurvey2015$goodLearners,
                                                    levels = c("Strongly Agree",
                                                               "Agree",
                                                               "Neutral",
                                                               "Disagree",
                                                               "Strongly Disagree"))

employerSurvey2015$goodInstructors <- ordered(employerSurvey2015$goodInstructors,
                                                    levels = c("Strongly Agree",
                                                               "Agree",
                                                               "Neutral",
                                                               "Disagree",
                                                               "Strongly Disagree"))

employerSurvey2015$goodManagers <- ordered(employerSurvey2015$goodManagers,
                                                    levels = c("Strongly Agree",
                                                               "Agree",
                                                               "Neutral",
                                                               "Disagree",
                                                               "Strongly Disagree"))

employerSurvey2015$goodLeaders <- ordered(employerSurvey2015$goodLeaders,
                                                    levels = c("Strongly Agree",
                                                               "Agree",
                                                               "Neutral",
                                                               "Disagree",
                                                               "Strongly Disagree"))

employerSurvey2015$wellPrepared <- ordered(employerSurvey2015$wellPrepared,
                                                    levels = c("Strongly Agree",
                                                               "Agree",
                                                               "Neutral",
                                                               "Disagree",
                                                               "Strongly Disagree"))

# Select variables to analyze
empsurv <- select(employerSurvey2015, RespondentID, fundamentalPrinciples, analyzeProblems,
                  effectiveCommunicators, manageInfoResources, organizeInformation,
                  effectiveTechnology, emergingTechnology, assistClients,
                  promoteEvaluateServices, originalResearch, goodLearners,
                  goodInstructors, goodManagers, goodLeaders, wellPrepared,
                  PublicLibrary, AcademicLibraryMain, AcademicLibraryBranch,
                  CorporateLibrary, OtherEmployer)

empsurv$OtherEmployer <- gsub("Analytics in Higher Education", "Yes", empsurv$OtherEmployer)

# exported CSV to combine the employer columns in to one
write.csv(empsurv, file = "empsurv.csv", quote = TRUE, sep = ",", col.names = TRUE)
empsurv <- read.csv("~/Dropbox/workspace/planning2015/empsurv.csv")
empsurv$X <- NULL
empsurv <- melt(empsurv, id.vars = c("RespondentID", "PublicLibrary"))
empsurv <- filter(empsurv, value != "NA")
empsurv$value <- ordered(empsurv$value,
                         levels = c("Strongly Agree",
                                    "Agree",
                                    "Neutral",
                                    "Disagree",
                                    "Strongly Disagree"))
empsurv$Org <- ordered(empsurv$Org,
                       levels = c("PublicLibrary",
                                  "AcademicLibrary",
                                  "CorporateLibrary",
                                  "OtherEmployer"))
empsurv$Org <- empsurv$PublicLibrary
empsurv$PublicLibrary <- NULL

p <- ggplot(empsurv, aes(x = value, y = variable, shape = Org))
p + geom_jitter() +
        xlab("Response") +
        ylab("Qualities") +
        ggtitle("Fall 2015 Employer Survey") +
        theme(axis.text.y = element_text(colour = "black")) +
        theme(axis.text.x = element_text(colour = "black")) +
        scale_shape(labels = c("Public Library", "Academic Library",
                               "Corporate Library", "Other Employer"),
                    name = "Org Type")

tmpemp <- filter(empsurv, Org == "PublicLibrary")
tapply(tmpemp$value, tmpemp$variable, FUN = summary)

tmpemp <- filter(empsurv, Org == "AcademicLibrary")
tapply(tmpemp$value, tmpemp$variable, FUN = summary)

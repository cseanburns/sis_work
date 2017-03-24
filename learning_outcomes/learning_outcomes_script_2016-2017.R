# R script to analyze learning outcome essay ratings and interrater agreement
# Run script on learning_outcomes_essays.csv

library(irr)
library(reshape2)
library(ggplot2)
library(dplyr)

# Read data
learning_outcome_essays <- read.csv("learning_outcome_essays_2016-2017.csv")

# Classify variables appropriately
learning_outcome_essays$PLO <- ordered(learning_outcome_essays$PLO,
                                        levels = c("1.4","1.5",
                                                   "2.1",
                                                   "3.4","3.5",
                                                   "4.1","4.2",
                                                   "7.2","7.3",
                                                   "8.3","8.4"))

learning_outcome_essays$Student <- ordered(learning_outcome_essays$Student,
                                           levels = c("Student1","Student2",
                                                      "Student3", "Student4",
                                                      "Student5","Student6",
                                                      "Student7","Student8",
                                                      "Student9","Student10",
                                                      "Student11","Student12",
                                                      "Student13","Student14"))

learning_outcome_essays$Semester <- ordered(learning_outcome_essays$Semester,
                                            levels = c("Fall2015",
                                                       "Spring2016"))

learning_outcome_essays$Program <- ordered(learning_outcome_essays$Program,
                                           levels = c("Foundations", "Resources",
                                                      "Organization", "Technology",
                                                      "Continuing", "Management"))

learning_outcome_essays$Count <- NULL

#### ---- Rater Differences ---- ####

outcomes_anova.1 <- melt(learning_outcome_essays,
                         id = c("PLO", "Student", "Semester", "Program"))

# Oneway ANOVA
fit.1 <- aov(value ~ variable, data = outcomes_anova.1)
summary(fit.1)

model.tables(fit.1, type = "effects", se = TRUE)
model.tables(fit.1, type = "means")

tukey <- TukeyHSD(fit.1)
tukey

tukey.df <- data.frame(TukeyHSD(fit.1, which = "variable")$variable)
tukey.df$Comparison <- row.names(tukey.df) 

p <- ggplot(tukey.df, aes(Comparison, y = diff, ymin = lwr, ymax = upr))
p +  geom_pointrange() +
        geom_errorbar() +
        ylab("Difference in Mean Scores of Raters") +
        coord_flip() +
        theme_bw() +
        ggtitle("TukeyHSD")

rm(outcomes_anova.1, fit.1, tukey, tukey.df, p)

#### ---- Interrater Reliability and Agreement ---- ####

# IntraClass Coefficient (ICC) Unit of Analysis:
        # Single: the reliability of an individual judge
        # Average: the reliability of the mean rating of a group of judges

# ICC for specific learning outcomes
# x = learning outcome: "1.1" or "1.2", etc.
# y = "single" or "average"
inter_rater.outcomes <- function(x,y) {
        filter(learning_outcome_essays, PLO == x) %>%
                select(Kim, Joo, Burns) %>%
                icc(model = "twoway",
                    type = "agreement",
                    unit = y)
        }

inter_rater.outcomes("1.4", "single")
inter_rater.outcomes("1.4", "average")
inter_rater.outcomes("1.5", "single")
inter_rater.outcomes("1.5", "average")
inter_rater.outcomes("2.1", "single")
inter_rater.outcomes("2.1", "average")
inter_rater.outcomes("3.4", "single")
inter_rater.outcomes("3.4", "average")
inter_rater.outcomes("3.5", "single")
inter_rater.outcomes("3.5", "average")
inter_rater.outcomes("4.1", "single")
inter_rater.outcomes("4.1", "average")
inter_rater.outcomes("4.2", "single")
inter_rater.outcomes("4.2", "average")
inter_rater.outcomes("7.2", "single")
inter_rater.outcomes("7.2", "average")
inter_rater.outcomes("7.3", "single")
inter_rater.outcomes("7.3", "average")
inter_rater.outcomes("8.3", "single")
inter_rater.outcomes("8.3", "average")
inter_rater.outcomes("8.4", "single")
inter_rater.outcomes("8.4", "average")

# ICC for Major Core Competencies/Program Learning Outcomes
# x = name of Core competency: "Foundations" or "Organization", etc.
# y = "single" or "average"
inter_rater.programs <- function(x,y) {
        filter(learning_outcome_essays, Program == x) %>%
                select(Kim, Joo, Burns) %>%
                icc(model = "twoway",
                    type = "agreement",
                    unit = y)
}

inter_rater.programs("Foundations", "single")
inter_rater.programs("Foundations", "average")
inter_rater.programs("Resources", "single")
inter_rater.programs("Resources", "average")
inter_rater.programs("Organization", "single")
inter_rater.programs("Organization", "average")
inter_rater.programs("Technology", "single")
inter_rater.programs("Technology", "average")
inter_rater.programs("Continuing", "single")
inter_rater.programs("Continuing", "average")
inter_rater.programs("Management", "single")
inter_rater.programs("Management", "average")

rm(inter_rater.programs, inter_rater.outcomes)

# Mean scores by program for each rater
Kim   <- tapply(learning_outcome_essays[,2], learning_outcome_essays$Program, FUN = mean)
Joo   <- tapply(learning_outcome_essays[,3], learning_outcome_essays$Program, FUN = mean)
Burns <- tapply(learning_outcome_essays[,4], learning_outcome_essays$Program, FUN = mean)

mean_scores.program         <- melt(cbind(Kim, Joo, Burns))
names(mean_scores.program)  <- c("Program", "Rater", "Mean_Score")
mean_scores.program$Program <- ordered(mean_scores.program$Program,
                                       levels = c("Foundations", "Resources",
                                                  "Organization","Technology",
                                                  "Continuing", "Management"))

mean_scores.program

p <- ggplot(mean_scores.program, aes(x = Program, y = Mean_Score, group = Rater))
p + geom_point(colour = "blue") +
        geom_line() +
        facet_grid(Rater ~ .) +
        xlab("") +
        ylab("Mean Scores") +
        ggtitle("Program Learning Outcomes") +
        theme_bw() +
        theme(axis.text.y = element_text(size = 11,
                                         colour = "black")) +
        theme(axis.text.x = element_text(size = 11,
                                         colour = "black"))

rm(mean_scores.program)

# ----- Quality of Learning Outcome Essays -----

mean_scores.df            <- data.frame(cbind(Kim, Joo, Burns))
mean_scores.df$Program    <- row.names(mean_scores.df)
row.names(mean_scores.df) <- NULL
mean_scores.df$Program    <- ordered(mean_scores.df$Program,
                                     levels = c("Foundations", "Resources",
                                                "Organization", "Technology",
                                                "Continuing", "Management"))

mean_scores.df$Program_means  <- rowMeans(mean_scores.df[,1:3])
mean_scores.df$Raters        <- rep("Raters", 6) # creates a meaningless group so that plot with points and lines

p <- ggplot(mean_scores.df, aes(x = Program, y = Program_means, group = Raters))
p + geom_point(colour = "blue") +
        geom_line() +
        geom_hline(yintercept = mean(mean_scores.df$Program_means)) +
        annotate("text", x = 3, y = 3.1, label = "Grand Mean") +
        xlab("") +
        ylab("Mean Scores") +
        ggtitle("Program Learning Outcomes") +
        theme_bw() +
        theme(axis.text.y = element_text(size = 11,
                                         colour = "black")) +
        theme(axis.text.x = element_text(size = 11,
                                         colour = "black"))

rm(Kim, Joo, Burns, mean_scores.df, p)
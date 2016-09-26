# R script to analyze learning outcome essay ratings and interrater agreement
# Run script on learning_outcomes_essays.csv

library(irr)
library(reshape2)
library(ggplot2)
library(dplyr)

# Read data
learning_outcome_essays <- read.csv("LO Essay Assessment Clean Data - Sheet1.csv")

# Classify variables appropriately
learning_outcome_essays$PLOs <- ordered(learning_outcome_essays$PLOs,
                                        levels = c("1.1","1.2", "1.3","1.4","1.5",
                                                   "2.1","2.2",
                                                   "3.1","3.2","3.3","3.4","3.5",
                                                   "4.1","4.2","4.3",
                                                   "5.1","5.2","5.3","5.4","5.5",
                                                   "6.1","6.2","6.3",
                                                   "7.1","7.2","7.3",
                                                   "8.1","8.2","8.3","8.4",
                                                   "9.1","9.2","9.3"))

learning_outcome_essays$Student <- ordered(learning_outcome_essays$Student,
                                           levels = c("Student1","Student2",
                                                      "Student3", "Student4",
                                                      "Student5","Student6",
                                                      "Student7","Student8",
                                                      "Student9","Student10",
                                                      "Student11","Student12",
                                                      "Student13","Student14",
                                                      "Student15","Student16",
                                                      "Student17","Student18",
                                                      "Student19"))

learning_outcome_essays$Semester <- ordered(learning_outcome_essays$Semester,
                                            levels = c("Summer2015",
                                                       "Fall2015",
                                                       "Spring2016"))

Program <- c(rep("Foundations", 5), rep("Resources", 2),
             rep("Organization", 5), rep("Technology", 3),
             rep("Reference", 5), rep("Research", 3),
             rep("Continuing", 3), rep("Management", 4), rep("Other", 3))

learning_outcome_essays$Program <- rep(Program, 19)
learning_outcome_essays$Program <- factor(learning_outcome_essays$Program)
learning_outcome_essays$Program <- ordered(learning_outcome_essays$Program,
                                           levels = c("Foundations", "Resources",
                                                      "Organization", "Technology",
                                                      "Reference", "Research",
                                                      "Continuing", "Management",
                                                      "Other"))
rm(Program)

# Reorder data for analysis
loe_aov_1      <- melt(learning_outcome_essays,
                       id = c("PLOs", "Student", "Semester", "Program"))

# Oneway ANOVA
fit.1 <- aov(value ~ variable, data = loe_aov_1)
summary(fit.1)

# Oneway analysis of variance
loe_aov_2             <- loe_aov_1
loe_aov_2$PLOs        <- NULL
loe_aov_2$Student     <- NULL
loe_aov_2$Semester    <- NULL
# loe_stacked           <- stack(loe_aov_2)
# names(loe_stacked)    <- c("Scores", "Raters")

loe_tukey <- TukeyHSD(fit.1)
model.tables(fit.1, type = "effects", se = TRUE)
model.tables(fit.1, type = "means")

p <- ggplot(loe_aov_1, aes(x = variable, y = value))
p + geom_boxplot() +
        xlab("Raters") +
        ylab("Scores") +
        ggtitle("Aggregrate Rater Scores") +
        theme_bw()

loe.hsd            <- data.frame(TukeyHSD(fit.1, which = "variable")$variable)
loe.hsd$Comparison <- row.names(loe.hsd) 

p <- ggplot(loe.hsd, aes(Comparison, y = diff, ymin = lwr, ymax = upr))
p +  geom_pointrange() +
        geom_errorbar() +
        ylab("Difference in Mean Scores of Raters") +
        coord_flip() +
        theme_bw() +
        ggtitle("TukeyHSD")

# Twoway analysis of variance
fit.2 <- aov(value ~ variable * Semester, data = loe_aov_1)
summary(fit.2)

p <- ggplot(loe_aov_1,
            aes(x = variable:Semester, y = value))
p + geom_boxplot() +
        theme_bw() +
        xlab("Interaction between Rater and Semester") +
        ylab("Score") +
        theme(axis.text.y = element_text(size = 12,
                                         colour = "black")) +
        theme(axis.text.x = element_text(angle = 90,
                                         hjust = 1,
                                         size = 12,
                                         colour = "black"))
        
interaction.plot(loe_aov_1$Semester, loe_aov_1$variable, loe_aov_1$value,
                 trace.label = "Rater",
                 xlab = "",
                 ylab = "Mean Scores")

# ----- IntraClass Coefficient -----
loe_irr <- learning_outcome_essays

# ICC for specific learning outcomes
# Unit of Analysis:
# Single: the reliability of an individual judge
# Average: the reliability of the mean rating of a group of judges
# x = learning outcome: "1.1" or "1.2", etc.
# y = "single" or "average"
loe_irr_no <- function(x,y) {
        filter(loe_irr, PLOs == x) %>% select(Namjoo:Sean) %>%
                icc(model = "twoway",
                    type = "agreement",
                    unit = y)
        }

# ANOVAs
# loe_irr_1.1$PLOs  <- NULL
# loe_irr_1.1       <- melt(loe_irr_1.1, id = c("Student", "Semester"))
# fit.3             <- aov(value ~ Student + variable,
#                          data = loe_irr_1.1)
# summary(fit.3)

# ICC for Major Core Competencies/Program Learning Outcomes
# x = name of Core competency: "Foundations" or "Organization", etc.
# y = "single" or "average"
loe_irr_program <- function(x,y) {
        filter(learning_outcome_essays, Program == x) %>%
                select(Namjoo, Shannon, Sean) %>%
                icc(model = "twoway",
                    type = "agreement",
                    unit = y)
}

# ----- Descriptive Statistics -----

Namjoo  <- tapply(learning_outcome_essays[,2], learning_outcome_essays$Program, FUN = mean)
Shannon <- tapply(learning_outcome_essays[,3], learning_outcome_essays$Program, FUN = mean)
Sean    <- tapply(learning_outcome_essays[,4], learning_outcome_essays$Program, FUN = mean)

NSS_g         <- data.frame(Namjoo, Shannon, Sean)
names(NSS_g)  <- c("Namjoo", "Shannon", "Sean")
NSS_g$Program <- row.names(NSS_g)
NULL          -> row.names(NSS_g)
NSS_g$Program <- ordered(NSS_g$Program,
                         levels = c("Foundations", "Resources", "Organization",
                                    "Technology", "Reference", "Research",
                                    "Continuing", "Management", "Other"))

NSS_g$ProgramMeans  <- rowMeans(NSS_g[,1:3])
NSS_g$Raters        <- rep("Raters", 9) # creates a meaningless group so that plot with points and lines
p <- ggplot(NSS_g, aes(x = Program, y = ProgramMeans, group = Raters))
p + geom_point(colour = "blue") +
        geom_line() +
        geom_hline(yintercept = mean(NSS_g$ProgramMeans)) +
        annotate("text", x = 5, y = 2.1, label = "Grand Mean") +
        xlab("") +
        ylab("Mean Scores") +
        ggtitle("Program Learning Outcomes") +
        theme_bw() +
        theme(axis.text.y = element_text(size = 11,
                                         colour = "black")) +
        theme(axis.text.x = element_text(size = 11,
                                         colour = "black"))


NSS         <- melt(cbind(Namjoo, Shannon, Sean))
names(NSS)  <- c("Program", "Rater", "MeanScore")
NSS$Program <- ordered(NSS$Program,
                       levels = c("Foundations", "Resources", "Organization",
                                  "Technology", "Reference", "Research",
                                  "Continuing", "Management", "Other"))

p <- ggplot(NSS, aes(x = Program, y = MeanScore, group = Rater))
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

# Proportion of scores assigned 0 to scores assigned > 0
c0 <- tally(group_by(learning_outcome_essays, Program), Namjoo == 0)
c1 <- tally(group_by(learning_outcome_essays, Program), Namjoo > 0)
ct <- c0$n/c1$n

o0 <- tally(group_by(learning_outcome_essays, Program), Shannon == 0)
o1 <- tally(group_by(learning_outcome_essays, Program), Shannon > 0)
ot <- o0$n/o1$n

b0 <- tally(group_by(learning_outcome_essays, Program), Sean == 0)
b1 <- tally(group_by(learning_outcome_essays, Program), Sean > 0)
bt <- b0$n/b1$n

cob <- data.frame(ct, ot, bt)
names(cob) <- c("Namjoo", "Shannon", "Sean")

Program <- c("Foundations", "Resources", "Organization", "Technology",
             "Reference", "Research", "Continuing", "Management", "Other")

Rater   <- rep("Rater", 9)

cob$Program <- ordered(Program,
                       levels = c("Foundations", "Resources", "Organization",
                                  "Technology", "Reference", "Research",
                                  "Continuing", "Management", "Other"))

# cob$Rater <- Rater
cob_g <- melt(cob)

p <- ggplot(cob_g, aes(x = Program, y = value, group = variable))
p + geom_point(colour = "blue") +
        geom_line() +
        facet_grid(variable ~ .) +
        xlab("") +
        ylab("Proportion") +
        ggtitle("Proportion of Program Learning Outcomes Not Addressed") +
        theme_bw() +
        theme(axis.text.y = element_text(size = 11,
                                         colour = "black")) +
        theme(axis.text.x = element_text(size = 11,
                                         colour = "black"))


# Association between core competency mean scores and core competency proportion of 0s
cob$ProgramProp  <- rowMeans(cob[,1:3])
cor(NSS_g$ProgramMeans, cob$ProgramProp)
cor(cob$ProgramProp, NSS_g$ProgramMeans)
ncob <- data.frame(NSS_g$ProgramMeans, cob$ProgramProp, cob$Program)
names(ncob) <- c("ProgramMeans", "ProgramProportions", "Program")
p <- ggplot(ncob, aes(x = ProgramMeans, y = ProgramProportions, label = Program))
p + geom_point() + geom_smooth() + geom_text() +
        xlab("Core Competency Means") +
        ylab("Core Competency Proportions") +
        ggtitle("Correlation between Core Competency Means and Proportions of 0s") +
        theme_bw() +
        theme(axis.text.y = element_text(size = 11,
                                         colour = "black")) +
        theme(axis.text.x = element_text(size = 11,
                                         colour = "black"))
        

loe_non <- filter(learning_outcome_essays, Namjoo > 0 & Shannon > 0 & Sean > 0)

Namjoo  <- tapply(loe_non[,2], loe_non$Program, FUN = mean)
Shannon <- tapply(loe_non[,3], loe_non$Program, FUN = mean)
Sean    <- tapply(loe_non[,4], loe_non$Program, FUN = mean)
non_g   <- data.frame(Namjoo, Shannon, Sean)

non_g$Program <- row.names(non_g)
NULL          -> row.names(non_g)
non_g$Program <- ordered(non_g$Program,
                         levels = c("Foundations", "Resources", "Organization",
                                    "Technology", "Reference", "Research",
                                    "Continuing", "Management", "Other"))

non_g$ProgramMeans  <- rowMeans(non_g[,1:3])
non_g$Raters        <- rep("Raters", 9) # creates a meaningless group so that plot with points and lines
p <- ggplot(non_g, aes(x = Program, y = ProgramMeans, group = Raters))
p + geom_point(colour = "blue") +
        geom_line() +
        geom_hline(yintercept = mean(non_g$ProgramMeans)) +
        annotate("text", x = 6, y = 2.66, label = "Grand Mean") +
        xlab("") +
        ylab("Mean Scores") +
        ggtitle("Program Learning Outcomes, Not Addressed Removed") +
        theme_bw() +
        theme(axis.text.y = element_text(size = 11,
                                         colour = "black")) +
        theme(axis.text.x = element_text(size = 11,
                                         colour = "black"))

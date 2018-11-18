######################## Import, exclusions, setup ########################################

setwd("~/Desktop/HCI/ProjectM/")

## Read in data
data <- read.csv("Project M_ Needfinding 2_ Raw Results.csv")

library(RColorBrewer)

## Exclude age < 18, 1 case
data <- data[-which(data$Select.your.age. == "Under 18"),]

############## Distribution of age, courses taken, and specialization #######################
data$Select.your.age. <- factor(data$Select.your.age.)
ylim <- c(0, 1.2*max(table(data$Select.your.age.)))
waz_b <- barplot(table(data$Select.your.age.), main = "Age", 
                 xlab = "Age group (years)", ylim=ylim, ylab = "Persons",
                 col = brewer.pal(5, "Spectral"))
text(x = waz_b, y = table(data$Select.your.age.), label = table(data$Select.your.age.), pos = 3, cex = 0.8)

ylim <- c(0, 1.2*max(table(data$How.many.courses.have.you.taken.with.OMSCS.)))
waz_b <- barplot(table(data$How.many.courses.have.you.taken.with.OMSCS.), main = "Courses taken",
                 xlab = "Number of courses taken", ylim=ylim, ylab = "Persons",
                 col = brewer.pal(10, "PuOr"))
text(x = waz_b, y = table(data$How.many.courses.have.you.taken.with.OMSCS.), label = table(data$How.many.courses.have.you.taken.with.OMSCS.), pos = 3, cex = 0.8)

ylim <- c(0, 1.2*max(table(data$What.is.your.chosen.specialization.)))
waz_b <- barplot(table(data$What.is.your.chosen.specialization.), main = "Specialization",
                 xlab = "Number of courses taken", ylim=ylim, ylab = "Persons",
                 col = brewer.pal(5, "RdBu"), las=2)
text(x = waz_b, y = table(data$What.is.your.chosen.specialization.), label = table(data$What.is.your.chosen.specialization.), pos = 3, cex = 0.8)

########################### Summary of tools used #############################
remain <- data$What.tools.do.you.use.for.course.planning.
list <- c("Course catalog", "Course website", "Forums", "Spreadsheets", 
          "Email", "Pen and paper", "Other websites")

tools_count <- vector()
for(i in 1:length(list)){
    tools_count[i] <- length(grep(list[i], remain))
}
ylim <- c(0, 1.2*max(tools_count))
names(tools_count) <- list
waz_b <- barplot(tools_count, main = "Tools used",
                 xlab = "Tools", ylab = "Persons", ylim=ylim,
                 col = brewer.pal(length(list), "Spectral"), las=2)
text(x = waz_b, y = tools_count, label = tools_count, pos = 3, cex = 0.8)

# Number of tools used
tools_multi <- table(lengths(regmatches(data$What.tools.do.you.use.for.course.planning., gregexpr(";", data$What.tools.do.you.use.for.course.planning.)))+1)
ylim <- c(0, 1.2*max(tools_multi))
waz_b <- barplot(tools_multi, main = "Multiple tools",
                 xlab = "Number of tools", ylab = "Persons", ylim=ylim,
                 col = brewer.pal(length(list), "BrBG"))
text(x = waz_b, y = tools_multi, label = tools_multi, pos = 3, cex = 0.8)


######################## Motivation theme keying ###############################
remain <- data$What.is.are.your.main.reason.s..for.taking.OMSCS.

# Getting a degree
idx_degree <- grep("[Dd]egree|[Mm]aster|[Pp]h[Dd]", remain)

# Pricing
idx_price <- grep("cheap|[Ii]nexpensive|cost", remain)

# Knowledge
idx_knowledge <- grep("lern|[Ll]earn|learning|student|material|personal|Mental|Hobby|knowledge|skill", remain)

# Career advancement
idx_career <- grep("[Cc]areer|opportunity|field|salary|[Jj]ob|resume", remain)

# Format
idx_format <- grep("[Ff]lexibility|time", remain)

# Prestige
idx_prestige <- grep("respectable|school|expected", remain)

# Summary of themes
list <- c("Degree", "Price", "Knowledge", "Career", "Format", "Prestige")

count_theme <- c(length(idx_degree), length(idx_price), length(idx_knowledge), length(idx_career),
                 length(idx_format), length(idx_prestige))
ylim <- c(0, 1.2*max(count_theme))
names(count_theme) <- list
waz_b <- barplot(count_theme, main = "Motivations",
                 xlab = "Code key", ylab = "Persons", ylim=ylim,
                 col = brewer.pal(length(list), "Spectral"), las=2)
text(x = waz_b, y = count_theme, label = count_theme, pos = 3, cex = 0.8)

# Number of multiple themes selected
idx_all <- c(idx_degree, idx_price, idx_knowledge, idx_career, idx_format, idx_prestige)
theme_multi <- table(table(idx_all))
ylim <- c(0, 1.2*max(theme_multi))
waz_b <- barplot(theme_multi, main = "Multiple motivations",
                 xlab = "Number of motivations", ylab = "Persons", ylim=ylim,
                 col = brewer.pal(length(list), "BrBG"))
text(x = waz_b, y = theme_multi, label = theme_multi, pos = 3, cex = 0.8)

################################## Information sources #############################
remain <- data$Which.sites.or.information.sources.have.you.used.for.planning.and.registering.for.your.courses.
list <- c("DegreeWorks", "[Cc]entral", "[Gg]oogle", "Previous semester class websites", "OMSCS official website", "[Ss]lack", "[rR]eddit", "critique", "oscar")

info_count <- vector()
for(i in 1:length(list)){
    info_count[i] <- length(grep(list[i], remain))
}

add <- data$If.you.selected..Other..to.the.last.question..please.clarify.
for(i in 1:length(list)){
    info_count[i] <- info_count[i] + length(grep(list[i], add))
}

list <- c("DegreeWorks", "OMSCentral", "Google+/Hangouts", "Previous semester class websites", "OMSCS official website", "Slack", "Reddit", "critique.gatech.edu", "OSCAR")
ylim <- c(0, 1.2*max(info_count))
names(info_count) <- list
waz_b <- barplot(info_count, main = "Info Source",
                 ylab = "Persons", ylim=ylim,
                 col = brewer.pal(length(list), "Dark2"), las=2)
text(x = waz_b, y = info_count, label = info_count, pos = 3, cex = 0.8)

# Number of information sources
info_multi <- lengths(regmatches(data$Which.sites.or.information.sources.have.you.used.for.planning.and.registering.for.your.courses., gregexpr(";", data$Which.sites.or.information.sources.have.you.used.for.planning.and.registering.for.your.courses.)))+1
info_multi <- info_multi + lengths(regmatches(data$If.you.selected..Other..to.the.last.question..please.clarify., gregexpr(";|and|,", data$If.you.selected..Other..to.the.last.question..please.clarify.)))
info_multi <- table(info_multi)
ylim <- c(0, 1.2*max(info_multi))
waz_b <- barplot(info_multi, main = "Multiple info sources",
                 xlab = "Number of sources", ylab = "Persons", ylim=ylim,
                 col = brewer.pal(length(list), "PRGn"))
text(x = waz_b, y = info_multi, label = info_multi, pos = 3, cex = 0.8)

############################# Difficulty course selection theming ########################

# Theme coding
remain <- data$What.do.you.find.most.difficult.about.deciding.on.a.course.

# Workload
idx_workload <- grep("[Ww]ork|effort|time|family|interests|balance|responsibilities|long", remain)

# Difficulty
idx_diff <- grep("[Dd]ifficult[y ]|prereq|technical|compatible|new material", remain)

# Availability & registration limits
idx_avail <- grep("get in|waitlist|offered|10|wait-list|waiting|future semesters", remain)

# Material
idx_mat <- grep("useful|learn|info|quality|interesting|instructor|rewarding|coverage|content|layout|group", remain)

list <- c("Workload", "Difficulty", "Availability", "Material")

diff_theme <- c(length(idx_workload), length(idx_diff), length(idx_avail), length(idx_mat))
ylim <- c(0, 1.2*max(diff_theme))
names(diff_theme) <- list
waz_b <- barplot(diff_theme, main = "Selecting course obstacles",
                 xlab = "Code key", ylab = "Persons", ylim=ylim,
                 col = brewer.pal(length(list), "Set1"))
text(x = waz_b, y = diff_theme, label = diff_theme, pos = 3, cex = 0.8)

# Number of multiple themes selected
idx_all <- c(idx_workload, idx_diff, idx_avail, idx_mat)
theme_multi <- table(table(idx_all))
ylim <- c(0, 1.2*max(theme_multi))
waz_b <- barplot(theme_multi, main = "Multiple course obstacles",
                 xlab = "Number of difficult factors", ylab = "Persons", ylim=ylim,
                 col = brewer.pal(length(list), "BrBG"))
text(x = waz_b, y = theme_multi, label = theme_multi, pos = 3, cex = 0.8)

############################# Factor for selecting course ##############################

remain <- data$What.are.the.factors.that.contribute.to.deciding.to.take.a.class.or.not.

# Reviews
idx_review <- grep("[Rr]eviews|comments|wisdom from friends|rating", remain)

# Instructor & Class structure
idx_staff <- grep("[Ii]nstructor|teacher|professor|group project|structure", remain)

# Requirements
idx_req <- grep("specialization requirements|functional requirements|speciali[sz]ation|degree", remain)

# Workload
idx_workload <- grep("[Ww]orkload|balance|time|[Ss]chedule|hours", remain)

# Difficulty
idx_diff <- grep("[Dd]ifficulty|prereq|course requirement|Easiness|gpa", remain)

# Availability & registration limits
idx_avail <- grep("get in|[Ss]eat", remain)

# Material
idx_mat <- grep("[Qq]uality|math|interest|intesting|[Kk]nowledge|content|quality|useful", remain)

list <- c("Workload", "Difficulty", "Availability", "Material", "Reviews", "Staff&Structure", "Requirements")

diff_theme <- c(length(idx_workload), length(idx_diff), length(idx_avail), length(idx_mat),
                length(idx_review), length(idx_staff), length(idx_req))
ylim <- c(0, 1.2*max(diff_theme))
names(diff_theme) <- list
waz_b <- barplot(diff_theme, main = "Selecting course factors",
                 xlab = "Code key", ylab = "Persons", ylim=ylim,
                 col = brewer.pal(length(list), "Dark2"), las=2)
text(x = waz_b, y = diff_theme, label = diff_theme, pos = 3, cex = 0.8)

######################### Information not provided #############################

# Theming
remain <- data$What.information.is.not.provided.to.you.that.would.help.your.academic.planning.

# Staff & Structure
idx_staff <- grep("[Ss]yllab|staff|[Gg]rad|calendar|[Pp]roject", remain)

# Availability
idx_avail <- grep("offered|waitlist|fill up|time ticket|require a course every year|new course", remain)

# Better reviews
idx_review <- grep("CIOS|filter|reviews|Winter term", remain)

# Workload
idx_workload <- grep("effort|time|[Ww]orkload|ROI", remain)

# Mentioned
idx_mentioned <- grep("[Mm]ention|Same as above", remain)

# Nothing
idx_nothing <- grep("[Nn]/[Aa]|[Nn]one|all information|nothing|Thanks", remain)

list <- c("Workload", "Staff&Structure", "Availability", "Better reviews")

diff_theme <- c(length(idx_workload), length(idx_staff), length(idx_avail), length(idx_review))
ylim <- c(0, 1.2*max(diff_theme))
names(diff_theme) <- list
waz_b <- barplot(diff_theme, main = "Additional information",
                 xlab = "Code key", ylab = "Persons", ylim=ylim,
                 col = brewer.pal(length(list), "Set2"))
text(x = waz_b, y = diff_theme, label = diff_theme, pos = 3, cex = 0.8)


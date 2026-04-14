# ============================================================
# DATA SCIENCE PROJECT : STUDENT HABITS ANALYSIS
# ============================================================

# =========================
# 1. LOAD LIBRARIES
# =========================
packages <- c("ggplot2", "dplyr", "tidyr", "scales", "GGally", "e1071", "caret")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# =========================
# 2. LOAD DATASET
# =========================
student_habit <- read.csv(file.choose())

# Preview
head(student_habit)
str(student_habit)

# =========================
# NUMERIC CONVERSION
# =========================
num_cols <- c("age","study_hours_per_day","social_media_hours","netflix_hours",
              "attendance_percentage","sleep_hours","exercise_frequency",
              "mental_health_rating","exam_score")

for(col in num_cols){
  student_habit[[col]] <- as.numeric(as.character(student_habit[[col]]))
}

# ==============================
# 3. DETECT DATA ISSUES
# ==============================

cat("----------- DATA ISSUE DETECTION -----------\n")

# 3.1 Missing values (including blank, space, NA, ?, null)
student_habit[student_habit == "" | student_habit == " " | student_habit == "NA" | student_habit == "?"] <- NA

cat("Missing values in each column:\n")
print(colSums(is.na(student_habit)))


# 3.2 Duplicate rows
dup_rows <- sum(duplicated(student_habit))
cat("Duplicate rows found:", dup_rows, "\n")


# 3.3 Inconsistent categories
cat("Unique values in Gender column:\n")
print(unique(student_habit$gender))

cat("Unique values in Part Time Job column:\n")
print(unique(student_habit$part_time_job))


# 3.4 Invalid numeric values
num_cols <- names(student_habit)[sapply(student_habit, is.numeric)]

cat("Invalid numeric values check:\n")
for(col in num_cols){
  cat(col, ":\n")
  # Use proper thresholds
  if(col %in% c("study_hours_per_day","social_media_hours","netflix_hours","sleep_hours")){
    cat(" Negative values:", sum(student_habit[[col]] < 0, na.rm = TRUE), "\n")
    cat(" Values >", 24, ":", sum(student_habit[[col]] > 24, na.rm = TRUE), "\n\n")
  } else if(col == "exercise_frequency"){
    cat(" Negative values:", sum(student_habit[[col]] < 0, na.rm = TRUE), "\n")
    cat(" Values >", 7, ":", sum(student_habit[[col]] > 7, na.rm = TRUE), "\n\n")
  } else if(col %in% c("age","attendance_percentage","exam_score")){
    cat(" Negative values:", sum(student_habit[[col]] < 0, na.rm = TRUE), "\n")
    cat(" Values >", 100, ":", sum(student_habit[[col]] > 100, na.rm = TRUE), "\n\n")
  } else if(col == "mental_health_rating"){
    cat(" Negative values:", sum(student_habit[[col]] < 0, na.rm = TRUE), "\n")
    cat(" Values >", 10, ":", sum(student_habit[[col]] > 10, na.rm = TRUE), "\n\n")
  } else {
    cat(" Negative values:", sum(student_habit[[col]] < 0, na.rm = TRUE), "\n\n")
  }
}

# ==============================
# 4. DATA CLEANING
# ==============================

# 4.1 Remove duplicates
dup_count <- sum(duplicated(student_habit))
cat("Duplicate rows found:", dup_count, "\n")

student_habit <- unique(student_habit)
cat("Duplicates removed\n")


# 4.2 Handle missing values


cat("Handling Missing Values...\n")

get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

for(col in names(student_habit)) {
  if(is.numeric(student_habit[[col]])) {
    na_count <- sum(is.na(student_habit[[col]]))
    if(na_count > 0){
      cat("Replacing", na_count, "missing values in", col, "with mean\n")
      student_habit[[col]][is.na(student_habit[[col]])] <- mean(student_habit[[col]], na.rm = TRUE)
    }
  } else {
    na_count <- sum(is.na(student_habit[[col]]))
    if(na_count > 0){
      cat("Replacing", na_count, "missing values in", col, "with mode\n")
      student_habit[[col]][is.na(student_habit[[col]])] <- get_mode(student_habit[[col]])
    }
  }
}


# 4.3 Fix invalid numeric values
cat("Fixing Invalid Numeric Values...\n")

for(col in num_cols){
  # Define upper limits
  upper_limit <- ifelse(col %in% c("study_hours_per_day","social_media_hours","netflix_hours","sleep_hours"), 24,
                        ifelse(col == "exercise_frequency", 7,
                               ifelse(col %in% c("age","attendance_percentage","exam_score"), 100,
                                      ifelse(col=="mental_health_rating", 10, Inf))))
  
  # Compute mean using only valid values
  valid_mean <- mean(student_habit[[col]][student_habit[[col]] >= 0 & student_habit[[col]] <= upper_limit], na.rm = TRUE)
  
  # Replace invalid high values
  exceed_count <- sum(student_habit[[col]] > upper_limit, na.rm = TRUE)
  if(exceed_count > 0){
    cat("Column", col, "has", exceed_count, "values exceeding", upper_limit, ". Replacing with valid mean\n")
    student_habit[[col]][student_habit[[col]] > upper_limit] <- valid_mean
  }
  
  # Replace invalid negative values
  neg_count <- sum(student_habit[[col]] < 0, na.rm = TRUE)
  if(neg_count > 0){
    cat("Column", col, "has", neg_count, "invalid negative values. Replacing with valid mean\n")
    student_habit[[col]][student_habit[[col]] < 0] <- valid_mean
  }
}

# 4.4 Fix inconsistent text values
cat("Fixing inconsistent text values...\n")

# Gender: only allow "male","female","other"
student_habit$gender <- tolower(student_habit$gender)
student_habit$gender[!(student_habit$gender %in% c("male","female","other"))] <- "other"

# Part-time job: only allow "yes","no"
student_habit$part_time_job <- tolower(student_habit$part_time_job)
student_habit$part_time_job[!(student_habit$part_time_job %in% c("yes","no"))] <- "yes"
print(unique(student_habit$gender))
print(unique(student_habit$part_time_job))
cat("Data cleaning completed\n")



# ==================================
# 5. EXPLORATORY DATA ANALYSIS 
# ==================================

summary(student_habit)

num_cols <- names(student_habit)[sapply(student_habit, is.numeric)]
cat_cols <- names(student_habit)[sapply(student_habit, function(x) is.character(x) | is.factor(x))]
eda_data <- student_habit
# ===========================
# 5.1 Histogram (Study Hours)
# ===========================
if("study_hours_per_day" %in% names(eda_data)){
  print(
    ggplot(eda_data, aes(x = study_hours_per_day)) +
      geom_histogram(fill="skyblue", bins=20, color="black") +
      labs(
        title = "Distribution of Study Hours",
        x = "Study Hours per Day",
        y = "Frequency"
      ) +
      theme_minimal()
  )
}


# =========================
# 5.2 Boxplot (Exam Score)
# =========================
if("exam_score" %in% names(eda_data)){
  print(
    ggplot(eda_data, aes(y = exam_score)) +
      geom_boxplot(fill="orange") +
      labs(
        title = "Boxplot of Exam Score",
        y = "Exam Score"
      ) +
      theme_minimal()
  )
}


# =========================
# 5.3 Bar Chart (Gender)
# =========================
if("gender" %in% names(eda_data)){
  
  eda_data$gender <- as.factor(eda_data$gender)
  
  print(
    ggplot(eda_data, aes(x = gender)) +
      geom_bar(fill="steelblue") +
      labs(
        title = "Gender Distribution",
        x = "Gender",
        y = "Count"
      ) +
      theme_minimal()
  )
}


# =================================
# 5.4 Scatter Plot (Study vs Score) 
# =================================
if(all(c("study_hours_per_day","exam_score") %in% names(eda_data))){
  
  print(
    ggplot(eda_data, aes(x = study_hours_per_day, y = exam_score)) +
      geom_point(color="blue", alpha=0.6) +
      geom_smooth(method="lm", color="red") +
      labs(
        title = "Study Hours vs Exam Score",
        x = "Study Hours per Day",
        y = "Exam Score"
      ) +
      theme_minimal()
  )
}


# =========================
# 5.5 Boxplot (Score by Gender)
# =========================
if(all(c("gender","exam_score") %in% names(eda_data))){
  
  print(
    ggplot(eda_data, aes(x = gender, y = exam_score)) +
      geom_boxplot(fill="lightgreen") +
      labs(
        title = "Exam Score by Gender",
        x = "Gender",
        y = "Exam Score"
      ) +
      theme_minimal()
  )
}


# =========================
# 5.6 Correlation Heatmap 
# =========================
if(length(num_cols) > 1){
  
  cor_matrix <- cor(eda_data[, num_cols], use="complete.obs")
  
  cor_df <- as.data.frame(as.table(cor_matrix))
  
  print(
    ggplot(cor_df, aes(Var1, Var2, fill=Freq)) +
      geom_tile(color="white") +
      geom_text(aes(label=round(Freq,2)), size=3) +
      scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0) +
      labs(title = "Correlation Heatmap", x = "", y = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle=45, hjust=1))
  )
}
# ====================================
# 6. OUTLIER HANDLING (IQR METHOD)
# ====================================

for(col in num_cols){
  Q1 <- quantile(student_habit[[col]], 0.25)
  Q3 <- quantile(student_habit[[col]], 0.75)
  IQR_val <- Q3 - Q1
  
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  student_habit[[col]] <- ifelse(student_habit[[col]] < lower, lower,
                                 ifelse(student_habit[[col]] > upper, upper, student_habit[[col]]))
}

cat("Outliers handled\n")

# ============================================================
# 7. ENCODING
# ============================================================

cat_cols <- names(student_habit)[sapply(student_habit, function(x) is.character(x) | is.factor(x))]

for(col in cat_cols){
  student_habit[[col]] <- as.numeric(factor(student_habit[[col]]))
}

# ============================================================
# 8. NORMALIZATION
# ============================================================
summary(student_habit[num_cols])
student_habit[num_cols] <- lapply(student_habit[num_cols], function(x){
  (x - mean(x)) / sd(x)
})

cat("Normalization done\n")

# ============================================================
# 9. FEATURE SELECTION
# ============================================================

cor_matrix <- cor(student_habit)
target_cor <- cor_matrix[, "exam_score"]

important_features <- names(target_cor[abs(target_cor) > 0.2])

student_selected <- student_habit[, important_features]

print("Selected Features:")
print(important_features)

# ============================================================
# 10. TRAIN-TEST SPLIT
# ============================================================

set.seed(123)

train_index <- createDataPartition(student_selected$exam_score, p=0.8, list=FALSE)

train_data <- student_selected[train_index, ]
test_data <- student_selected[-train_index, ]

cat("Train rows:", nrow(train_data), "\n")
cat("Test rows:", nrow(test_data), "\n")




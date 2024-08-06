
###install.packages("reticulate")
library(reticulate)

use_python("C:/Users/Public/Videos/python.exe", required=TRUE) # Specify the path to the Python executable if not automatically detected
py_discover_config()
use_virtualenv("C:/Users/Public/newenv", required = TRUE)
reticulate::py_install("transformers")
reticulate::py_install("torch")
transformers <- import("transformers")
model_name <- "cardiffnlp/twitter-roberta-base-sentiment-latest"
tokenizer <- transformers$AutoTokenizer$from_pretrained(model_name)
model <- transformers$AutoModelForSequenceClassification$from_pretrained(model_name)

texts <-"shooting Black unarmed citizens"
inputs <- tokenizer$encode_plus(text, return_tensors = "pt", max_length =as.integer(512), truncation = TRUE,)

sentiment_analysis <- function(text) {
  inputs <- tokenizer$encode_plus(text, return_tensors = "pt", max_length = as.integer(512), truncation = TRUE, padding = "max_length")
  outputs <- model(inputs$input_ids)
  logits <- outputs$logits
  
  # Convert PyTorch tensor to R numeric vector
  logits_numeric <- as.numeric(py_to_r(logits$detach()$numpy()))
  
  # Apply softmax to logits
  softmax <- function(x) {
    e_x <- exp(x - max(x))
    return(e_x / sum(e_x))
  }
  probabilities <- softmax(logits_numeric)
  
  # Find the index of the highest probability
  predicted_class <- which.max(probabilities)
  class_labels <- c("Negative", "Neutral", "Positive")
  
  # Return the predicted sentiment and its probability
  list(sentiment = class_labels[predicted_class], probability = probabilities[predicted_class])
}
sentiment_analysis(texts)

# Convert the list of results into a data frame
results_df <- do.call(rbind, results_list)


##So we have to make a function
###Set text to be equal to i position in the DF of Words
###Then run Sentiment analysis on that
###Then grab results, badda bing badda boom.results_list1 <- list()
results_list1<-list()
for (i in 1:nrow(MackingSample)) {
  texty <- as.character(MackingSample$Words[i])
  result <- sentiment_analysis(texty) 
  
  results_list1[[i]] <- result
}
MackingSampleResults<- do.call(rbind, results_list1)
MackingSampleResults<-as.data.frame(MackingSampleResults)
MackingSampleDone<-bind_cols(MackingSample,MackingSampleResults)
MackingSampleResults<-factor()
nrow(MackingSampleResults)

results_list2<-list()
for (i in 1:nrow(ProblemChildren)) {
  texty <- as.character(ProblemChildren$Words[i])
  result <- sentiment_analysis(texty) 
  
  results_list2[[i]] <- result
}
ProblemChildrenResults<- do.call(rbind, results_list2)
ProblemChildrenResults<-as.data.frame(ProblemChildrenResults)
ProblemChildrenDone<-bind_cols(ProblemChildren,ProblemChildrenResults)
MackingSampleResults<-factor()
nrow(MackingSampleResults)

levels(MackingSampleDone$Prompt) <- levels(MackingSampleDone$sentiment) <- c('Positive', 'Neutral', 'Negative')
MackingSampleDone$Prompt <- factor(MackingSampleDone$Prompt, levels = c('Positive', 'Neutral', 'Negative'))
MackingSampleDone$Actual <- factor(MackingSampleDone$Actual, levels = c('Positive', 'Neutral', 'Negative'))
MackingSampleConfuse<-confusionMatrix(MackingSampleDone$sentiment,MackingSampleDone$Prompt)
#####
library(caret)

# Ensure 'sentiment' column is a factor with the correct levels
MackingSampleDone$sentiment <- factor(MackingSampleDone$sentiment, levels = c('Positive', 'Neutral', 'Negative'))

# Ensure 'Prompt' column is a factor with the correct levels
MackingSampleDone$Prompt <- factor(MackingSampleDone$Prompt, levels = c('Positive', 'Neutral', 'Negative'))

# Ensure 'Actual' column is also correctly factored if needed
MackingSampleDone$Actual <- factor(MackingSampleDone$Actual, levels = c('Positive', 'Neutral', 'Negative'))

# Compute the confusion matrix
MackingSampleConfuse <- confusionMatrix(MackingSampleDone$sentiment, MackingSampleDone$Prompt)

# Print the confusion matrix
print(MackingSampleConfuse)


MackingSampleConfuse2 <- confusionMatrix(MackingSampleDone$sentiment, MackingSampleDone$Actual)
print(MackingSampleConfuse2)




PoliceConf<-confusionMatrix(ValencePartyConflict$Prompt,ValencePartyConflict$Actual)



# Define the number of parts
n_parts <- 10

# Calculate the size of each part 
part_size <- 72

# Create a factor to split the data frame
group_factor <- rep(1:n_parts, each = part_size)

# Split the data frame into 10 parts 
list_of_parts <- split(RobertaLovesYou, group_factor)


for(i in 1:length(list_of_parts)) {
  assign(paste("RobertaV", i, sep = ""), list_of_parts[[i]])
  
}

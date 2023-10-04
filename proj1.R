## Maxmillan Ries - S2585467 Wanying Liang -S2503745 Kexin Yan -S2520957
## Due to the difficulty of splitting statistical programming code amongst different people, 
## we opted to do the assignment more or less individually (helping each other when stuck, such as making split_punct more efficient),
## and combinig our solutions at the end, making sure we all learnt and understood what was required. 
## To do this, we sat in several Teams calls and shared our screen in a form of pair-programming.
## Since we all did the full assignment separately, we roughly had equal contributions.

## Question 1-3
## change the current working directory and read the file

setwd("F:/ED-semaster1/stastistical programming/group1") ## comment it
a <- scan("4300-0.txt",what="character",skip=73,nlines=32858-73)
a <- gsub("_(","",a,fixed=TRUE) ## remove "_("

## Question 4
## split_punct: takes a vector of words as input along with a punctuation makes. split the words and punctuation into two independent vectors
## use grep, rep and gsub for this work. grep and gsub can interpret the sample punctuation
## input a vector and out put a vector
split_punct <- function(punct, x){
  ii <- grep(punct,x, value=FALSE, fixed = TRUE) ## which elements of x have this punctuation?
  xs <- rep("",length(ii)+length(x)) ## vector to store the punctuation
  iis <-ii+1:length(ii) ## where should the separated punctuation mark go in xs?
  xs[iis] <- punct ## insert the punctuation mark
  xs[-iis] <- gsub(punct, "", x, fixed = TRUE) ## insert the resident word
  return(xs)
}

## Question 5
## separate the punctuation marks ",",".",";","!",":","?"
punctuation <- c(",",".",";","!",":","?")
a_word <- a
for (p in punctuation){
  a_word <- split_punct(p,a_word) ## text after separation
}


## Question 6
a_lower <- tolower(a_word) ## turn capital letters to lower case letters
a_unique <- unique(a_lower) 
a_match <- match(a_lower, a_unique)
a_fre <- tabulate(a_match) ## how many time each unique word occurs

a_1000 <- a_fre[a_fre>26] 
a_1000_ind <- which(a_fre>26)
b <- a_unique[a_1000_ind] ## a vector of the most m ≈ 1000 common words
b_fre <- a_fre[a_1000_ind]
b_pro <- b_fre/(sum(b_fre)) # probability on each common word appears 


## Question 7
b_match <- match(a_lower, b) 
b_match_1 <- which(b_match>0)
b_match_2 <- b_match_1 + 1  
b_match_3 <- b_match_1 + 2
b_combine <- cbind(b_match_1, b_match_2, b_match_3)

T_value <- matrix(nrow = nrow(b_combine), ncol = ncol(b_combine))
for (i in 1:nrow(b_combine)) {
  for (j in 1:ncol(b_combine)) {
    index <- b_combine[i, j]
    if (index > length(b_match)){
      T_value[i, j] <- NA
    }else{
      new_value <- b_match[[index]]
      T_value[i, j] <- new_value 
    }
  }
}

# drop the word triplets contains an NA
na_sums <- rowSums(is.na(T_value)) # Calculate number of NA's in each row
T <- T_value[na_sums == 0, ] # Use na_sums indices where there are no NA values

# Make the matrices of pairs
P_value <- T_value[,-3]
P_with_na <- apply(P_value, 1, function(row) any(is.na(row)))
P <- P_value[!P_with_na,]
na_sums <- rowSums(is.na(P_value)) # Calculate number of NA's in each row
P <- P_value[na_sums == 0, ] # Use na_sums indices where there are no NA values


## Question 8
simulate_text <- function(T,P,a,num_words_to_generate){
  generated_text <- character(0)
  start_word1 <- sample(1:length(b), 1, prob = b_pro) ## k[i]
  start_word2 <- sample(1:length(b), 1, prob = b_pro) ## k[j]
  
  for (i in 1:num_words_to_generate){
    sub_matrix <- T[T[, 1] == start_word1 & T[, 2] == start_word2, ]
    # check if the sub-matrix has row
    if ((nrow(sub_matrix)>0) && !is.null(nrow(sub_matrix))){
      next_word_idx <- sample(sub_matrix[, 3], 1)
    }else{
      sub_matrix_P <- P[P[, 1] == start_word2, ]
      if ((nrow(sub_matrix_P)>0) && !is.null(nrow(sub_matrix_P))){
        next_word_idx <- sample(sub_matrix_P[, 2], 1)
      }else{
        next_word_idx <- sample(1:length(b), 1, prob = b_pro)
      }
    }
    
    # Add next word to generated text
    # generated_text <- c(generated_text, b[next_word_idx])
    if (i == 1 || generated_text[length(generated_text)] == "." || generated_text[length(generated_text)] == "?" || generated_text[length(generated_text)] == "!"){
      generated_text <- c(generated_text, paste0(toupper(substring(b[next_word_idx], 1, 1)), substring(b[next_word_idx], 2)) )
    }else{
      generated_text <- c(generated_text, b[next_word_idx])
    }
    
    
    # Update the starting words for the next iteration
    start_word1 <- start_word2
    start_word2 <- next_word_idx
    
    
  }
  
  return(generated_text)
  
}

## Simulate 50-word section
num_words_to_generate <- 50
generated_text <- simulate_text(T, P, b, num_words_to_generate)

## Print the generated text
## 50-word generate for question 8 with Markov Model
cat(generated_text, sep = " ")


## Question 9
simulate_text_only_b <- function(b,num_words_to_generate){
  generated_text <- character(0)
  
  for (i in 1:num_words_to_generate){
    next_word_idx <- sample(1:length(b), 1, prob = b_pro) ## k[i]
    
    # Add next word to generated word
    generated_text <- c(generated_text, b[next_word_idx])
    
  }
  
  return(generated_text)
  
}

num_words_to_generate <- 50
generated_text_b <- simulate_text_only_b(b, num_words_to_generate)

## Print the generated text
## 50-word generate for question 9 for comparison, based on common word frequencies
cat(generated_text_b, sep = " ")

## Question 10 

b_print <- character(0)
for (word in b){
  uppercase_word <- paste0(toupper(substr(word, 1, 1)), substring(word, 2))
  lowercase_word <- word
  
  upper_match <- tabulate(match(a_word, uppercase_word))
  lower_match <- tabulate(match(a_word, lowercase_word))
  
  if (upper_match < lower_match){
    b_print <- c(b_print, lowercase_word)
  }else{
    b_print <- c(b_print, uppercase_word)
  }

}

num_words_to_generate <- 50
generated_text <- simulate_text(T, P, b_print, num_words_to_generate)

# Print the generated text
cat(generated_text, sep = " ")

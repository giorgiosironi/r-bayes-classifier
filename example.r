contains <- function(a_list, key) {
    return (length(which(names(a_list) == key)) > 0)
}

# training set
training <- list()
training[[1]] <- list(c("buy", "drugs", "online", "from", "our", "pharma"),
                    c("buy", "insurance", "at", "low", "prices"))
training[[2]] <- list(c("newsletter", "from", "your", "favorite", "website"),
                    c("I", "was", "writing", "for", "php", "advice"),
                   c("good", "article", "on", "php"))
categories <- length(training)
## priors
priors <- c()
total = 0
for (category in 1:categories) {
    total <- total + length(training[[category]])
}
for (category in 1:categories) {
    priors[category] <- length(training[[category]]) / total
}
## features
features <- list();
zeroOccurrences = list()
for (category in 1:categories) {
    categoryFeatures <- list();
    singleOccurrence = 1 / length(training[[category]])
    zeroOccurrences[[category]] = singleOccurrence
    for (sampleMail in training[[category]]) {
        for (word in sampleMail) {
            if (contains(categoryFeatures, word)) {
                categoryFeatures[[word]] = categoryFeatures[[word]] + singleOccurrence
            } else {
                categoryFeatures[[word]] = zeroOccurrences[[category]] + singleOccurrence
            }
        }
    }
    features[[category]] <- categoryFeatures
}

# classifier
score <- function (test_mail, category) {
    score <- priors[category]
    categoryFeatures = features[[category]]
    for (word in test_mail) {
        if (contains(categoryFeatures, word)) {
            score <- score * categoryFeatures[[word]]
        } else {
            score <- score * zeroOccurrences[[category]]
        }
    }
    return(score)
}

classify <- function(test_mail) {
    scores <- c()
    for (i in 1:categories) {
        scores[i] <- score(test_mail, i)
    }
    print(scores)
    which(scores==max(scores))
}

# validation set
print(classify(c("php", "object", "oriented")))
print(classify(c("free", "drugs")))
print(classify(c("r", "article")))

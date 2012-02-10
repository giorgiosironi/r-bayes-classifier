# training set
spam <- list(c("buy", "drugs", "online", "from", "our", "pharma"),
            c("buy", "insurance", "at", "low", "prices"))
legitimate <- list(c("newsletter", "from", "your", "favorite", "website"),
                   c("I", "was", "writing", "for", "php", "advice"),
                   c("good", "article", "on", "php"))
# training
categories = 2
priors <- c()
total <- length(spam) + length(legitimate)
priors[1] <- length(spam) / total
priors[2] <- length(legitimate) / total

# classifier
score <- function (test_mail, category) {
    priors[category]
}

classify <- function(test_mail) {
    scores = c()
    for (i in 1:categories) {
        scores[i] = score(test_mail, i)
    }
    print(scores)
    which(scores==max(scores))
}

# validation set
print(classify(c("php", "object", "oriented")))
print(classify(c("free", "drugs")))
print(classify(c("r", "article")))

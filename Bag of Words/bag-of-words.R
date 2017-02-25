install.packages("glmnet")
library(glmnet)
install.packages("text2vec",dependencies = TRUE)
library(text2vec)


sms <- read.csv(file = "SMSSpamCollection", header = FALSE, quote ="", sep = "\t")

names(sms)<-c("Class","text")

sms$text<-as.character(sms$text)

set.seed(42L)
it<-itoken(sms$text,process_function = tolower, tokenizer = word_tokenizer)
sw <- c( "wif","a","about","above","across","after","again","against","all","almost","alone","along","already","also","although","always","am","among","an","and","another","any","anybody","anyone","anything","anywhere","are","area","areas","aren't","around","as","ask","asked","asking","asks","at","away","b","back","backed","backing","backs","be","became","because","become","becomes","been","before","began","behind","being","beings","below","best","better","between","big","both","but","by","c","came","can","cannot","can't","case","cases","certain","certainly","clear","clearly","come","could","couldn't","d","did","didn't","differ","different","differently","do","does","doesn't","doing","done","don't","down","downed","downing","downs","during","e","each","early","either","end","ended","ending","ends","enough","even","evenly","ever","every","everybody","everyone","everything","everywhere","f","face","faces","fact","facts","far","felt","few","find","finds","first","for","four","from","full","fully","further","furthered","furthering","furthers","g","gave","general","generally","get","gets","give","given","gives","go","going","good","goods","got","great","greater","greatest","group","grouped","grouping","groups","h","had","hadn't","has","hasn't","have","haven't","having","he","he'd","he'll","her","here","here's","hers","herself","he's","high","higher","highest","him","himself","his","how","however","how's","i","i'd","if","i'll","i'm","important","in","interest","interested","interesting","interests","into","is","isn't","it","its","it's","itself","i've","j","just","k","keep","keeps","kind","knew","know","known","knows","l","large","largely","last","later","latest","least","less","let","lets","let's","like","likely","long","longer","longest","m","made","make","making","man","many","may","me","member","members","men","might","more","most","mostly","mr","mrs","much","must","mustn't","my","myself","n","necessary","need","needed","needing","needs","never","new","newer","newest","next","no","nobody","non","noone","nor","not","nothing","now","nowhere","number","numbers","o","of","off","often","old","older","oldest","on","once","one","only","open","opened","opening","opens","or","order","ordered","ordering","orders","other","others","ought","our","ours","ourselves","out","over","own","p","part","parted","parting","parts","per","perhaps","place","places","point","pointed","pointing","points","possible","present","presented","presenting","presents","problem","problems","put","puts","q","quite","r","rather","really","right","room","rooms","s","said","same","saw","say","says","second","seconds","see","seem","seemed","seeming","seems","sees","several","shall","shan't","she","she'd","she'll","she's","should","shouldn't","show","showed","showing","shows","side","sides","since","small","smaller","smallest","so","some","somebody","someone","something","somewhere","state","states","still","such","sure","t","take","taken","than","that","that's","the","their","theirs","them","themselves","then","there","therefore","there's","these","they","they'd","they'll","they're","they've","thing","things","think","thinks","this","those","though","thought","thoughts","three","through","thus","to","today","together","too","took","toward","turn","turned","turning","turns","two","u","under","until","up","upon","us","use","used","uses","v","very","w","want","wanted","wanting","wants","was","wasn't","way","ways","we","we'd","well","we'll","wells","went","were","we're","weren't","we've","what","what's","when","when's","where","where's","whether","which","while","who","whole","whom","who's","whose","why","why's","will","with","within","without","won't","work","worked","working","works","would","wouldn't","x","y","year","years","yes","yet","you","you'd","you'll","young","younger","youngest","your","you're","yours","yourself","yourselves","you've")
vocab <- create_vocabulary(it, stopwords = sw)
it <- itoken(sms$text, tolower, word_tokenizer)
Vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(it,Vectorizer)
dim(dtm)
dtm_tfidf = transform_tfidf(dtm)
dim(dtm_tfidf)

fit=cv.glmnet(x=dtm, y=sms[['Class']],family = 'binomial', alpha = 1, type.measure = "auc", nfolds = 5, thresh = 1e-3, maxit = 1e3)
pred=predict(fit,dtm)
summary(pred)
pred[pred>0]="spam"
pred[pred<=0]="ham"
table(pred,sms$Class)

fit=cv.glmnet(x=dtm_tfidf, y=sms[['Class']],family = 'binomial', alpha = 1, type.measure = "auc", nfolds = 5, thresh = 1e-3, maxit = 1e3)
pred=predict(fit,dtm_tfidf)
summary(pred)
pred[pred>0]="spam"
pred[pred<=0]="ham"
table(pred,sms$Class)

##
fit <- cv.glmnet(x = dtm, y = sms[['Class']],family = 'binomial',type.measure = "auc",alpha = 1,nfolds = 5,thresh = 1e-3,maxit = 1e3)
plot(fit)
round(max(fit$cvm), 4)
bestlam = fit$lambda.min
bestlam
pruned_vocab <- prune_vocabulary(vocab, term_count_min = 10, doc_proportion_max = 0.5, doc_proportion_min = 0.001)
tokens <- sms$text %>%
  tolower() %>%
  word_tokenizer()
it <- itoken(tokens)
vectorizer <- vocab_vectorizer(pruned_vocab)
dtm <- create_dtm(it, vectorizer)
dim(dtm)
dtm <- dtm %>% transform_tfidf()
fit <- cv.glmnet(x = dtm, y = sms[['Class']],family = 'binomial',type.measure = "auc",alpha = 1,nfolds = 5,thresh = 1e-3,maxit = 1e3)
plot(fit)
round(max(fit$cvm), 4)
bestlam = fit$lambda.min
bestlam

###
train = sample(1:5574, 4574)
x.train = dtm[train,]
y_vals = sms[['Class']]
y.train = y_vals[train]
x.test = dtm[-train,]
y.test = y_vals[-train]
lasso.mod = glmnet(x.train,y.train,alpha=1,family = "binomial")
lasso.probs = predict(lasso.mod,s=bestlam,newx = x.test, response = "response")
lasso.pred = rep(0, 1000)
lasso.pred[lasso.probs > .5] = "spam"
lasso.pred[lasso.probs <= .5] = "ham"
table(lasso.pred, y.test)
mean(lasso.pred == y.test)

###
it <- itoken(tokens)
vocab <- create_vocabulary(it, ngram = c(2L, 2L)) %>%
  prune_vocabulary(term_count_min = 10,doc_proportion_max = 0.5,doc_proportion_min = 0.001)
vectorizer <- vocab_vectorizer(vocab)
dtm <- tokens %>%
  itoken() %>%
  create_dtm(vectorizer) %>%
  transform_tfidf()
dim(dtm)
fit <- cv.glmnet(x = dtm, y = sms[['Class']],family = 'binomial',alpha = 1,type.measure = "auc",nfolds = 5,thresh = 1e-3,maxit = 1e3)
plot(fit)
round(max(fit$cvm), 4)
bestlam = fit$lambda.min
bestlam

###
x.train = dtm[train,]
x.test = dtm[-train,]
lasso.mod = glmnet(x.train,y.train,alpha=1,family = "binomial")
lasso.probs = predict(lasso.mod,s=bestlam,newx = x.test, response = "response")
lasso.pred = rep(0, 1000)
lasso.pred[lasso.probs > .5] = "spam"
lasso.pred[lasso.probs <= .5] = "ham"
table(lasso.pred, y.test)
mean(lasso.pred == y.test)


#Cross Validation
k=10
err=seq(1:10)
set.seed(1)
folds=sample(1:k,nrow(dtm),replace = TRUE)
for(i in 1:k)
{
  train=dtm_tfidf[folds!=i,]
  test=dtm_tfidf[folds==i,]
  train.y=sms[folds!=i,]$Class
  test.y=sms[folds==i,]$Class
  fit=cv.glmnet(x=train, y=train.y,family = 'binomial', alpha = 1, type.measure = "auc", nfolds = 5, thresh = 1e-3, maxit = 1e3)
  
  pred=predict(fit,test)
  pred[pred>0]="spam"
  pred[pred<=0]="ham"
  
  countequal=sum(pred==test.y)
  n=length(test.y)
  accuracy=countequal/n
  err[i]=accuracy
}
err
ave=mean(err)
ave


k=10
i=1
err=seq(1:10)
set.seed(1)
folds=sample(1:k,nrow(dtm),replace = TRUE)
for(i in 1:k)
{
  train=dtm[folds!=i,]
  test=dtm[folds==i,]
  train.y=sms[folds!=i,]$Class
  test.y=sms[folds==i,]$Class
  fit=cv.glmnet(x=train, y=train.y,family = 'binomial', alpha = 1, type.measure = "auc", nfolds = 5, thresh = 1e-3, maxit = 1e3)
  
  pred=predict(fit,test)
  pred[pred>0]="spam"
  pred[pred<=0]="ham"
  
  countequal=sum(pred==test.y)
  n=length(test.y)
  accuracy=countequal/n
  err[i]=accuracy
}
err
ave=mean(err)
ave
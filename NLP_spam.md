NLP: Spam data
================
Li-Hsin Chien
2024-03-23

# 1 下載資料

資料來源網址:
<https://archive.ics.uci.edu/dataset/228/sms+spam+collection>

R 程式碼來源: <https://rpubs.com/Seun/455974>

``` r
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00228/smsspamcollection.zip"

if (!file.exists("smsspamcollection.zip")) 
{
  download.file(url=url, destfile="smsspamcollection.zip", method="curl")
}
unzip("smsspamcollection.zip")

data_text <- read.delim("SMSSpamCollection", sep="\t", header=F, colClasses="character", quote="")

# rename the column names
colnames(data_text) <- c("Class", "Text")

str(data_text) #5574 obs, 2 variables
```

    ## 'data.frame':    5574 obs. of  2 variables:
    ##  $ Class: chr  "ham" "ham" "spam" "ham" ...
    ##  $ Text : chr  "Go until jurong point, crazy.. Available only in bugis n great world la e buffet... Cine there got amore wat..." "Ok lar... Joking wif u oni..." "Free entry in 2 a wkly comp to win FA Cup final tkts 21st May 2005. Text FA to 87121 to receive entry question("| __truncated__ "U dun say so early hor... U c already then say..." ...

共 5574 個簡訊(樣本)，2 個變數。

``` r
table(data_text[,1])
```

    ## 
    ##  ham spam 
    ## 4827  747

5574 簡訊中，有 747 個垃圾簡訊(spam)，4827 個非垃圾簡訊(ham)。

第一筆簡訊的內容如下:

``` r
data_text[1,2]
```

    ## [1] "Go until jurong point, crazy.. Available only in bugis n great world la e buffet... Cine there got amore wat..."

# 2 資料清理 (data cleaning)

``` r
#install.packages("tm")
#install.packages("SnowballC")
library(tm) #text mining package
#library(SnowballC)
```

定義語音庫(corpus)

``` r
corpus <- VCorpus(VectorSource(data_text$Text)) #tm; corpus(文集/語文庫)
as.character(corpus[[3]])
```

    ## [1] "Free entry in 2 a wkly comp to win FA Cup final tkts 21st May 2005. Text FA to 87121 to receive entry question(std txt rate)T&C's apply 08452810075over18's"

## 2.1 將字都改成小寫 (put the words in lowercase)

``` r
corpus = tm_map(corpus, content_transformer(tolower))
as.character(corpus[[3]])
```

    ## [1] "free entry in 2 a wkly comp to win fa cup final tkts 21st may 2005. text fa to 87121 to receive entry question(std txt rate)t&c's apply 08452810075over18's"

## 2.2 去除 0~9 (removeNumber)

``` r
corpus = tm_map(corpus, removeNumbers)
as.character(corpus[[3]])
```

    ## [1] "free entry in  a wkly comp to win fa cup final tkts st may . text fa to  to receive entry question(std txt rate)t&c's apply over's"

## 2.3 去除標點符號 (Remove punctuation)

removePunctuation:Remove punctuation marks from a text document.

``` r
corpus = tm_map(corpus, removePunctuation)
as.character(corpus[[3]])
```

    ## [1] "free entry in  a wkly comp to win fa cup final tkts st may  text fa to  to receive entry questionstd txt ratetcs apply overs"

## 2.4 去除 stop words

Stop words: a, an, and, are, as, at, be, but, by, for, if, in, into, is,
it, no, not, of, on, or, such, that, the, their, then, there, these,
they, this, to, was, will and with…

``` r
corpus = tm_map(corpus, removeWords, stopwords("english"))
as.character(corpus[[3]])
```

    ## [1] "free entry    wkly comp  win fa cup final tkts st may  text fa    receive entry questionstd txt ratetcs apply overs"

## 2.5 去除多餘空白

``` r
corpus = tm_map(corpus, stripWhitespace)
as.character(corpus[[3]])
```

    ## [1] "free entry wkly comp win fa cup final tkts st may text fa receive entry questionstd txt ratetcs apply overs"

## 2.6 替換成詞幹 (Stem words)

``` r
corpus = tm_map(corpus, stemDocument)
as.character(corpus[[3]])
```

    ## [1] "free entri wkli comp win fa cup final tkts st may text fa receiv entri questionstd txt ratetc appli over"

# 3 製作文件矩陣 (term-document matrix)

文件矩陣（term-document matrix）:
用來表示各個單詞在整個語料庫中之於文件的重要性。這裡用 term
frequency，即 $x_{ij}$ 代表第 $j$ 個term 在第 $i$ 個簡訊裡出現的次數。

``` r
dtm = DocumentTermMatrix(corpus)
dtm
```

    ## <<DocumentTermMatrix (documents: 5574, terms: 6956)>>
    ## Non-/sparse entries: 43745/38728999
    ## Sparsity           : 100%
    ## Maximal term length: 40
    ## Weighting          : term frequency (tf)

語音庫(corpus)中共有 5574 個簡訊 (document,row)，總共有 6956 個單詞
(term, column)。

接下來去掉出現頻率低的單詞:

``` r
dtm = removeSparseTerms(dtm, 0.999)
dim(dtm)
```

    ## [1] 5574 1208

單詞量減少至 1208 個。

將 term-document matrix 轉為矩陣格式:

``` r
mat<-as.matrix(dtm)
dim(mat)
```

    ## [1] 5574 1208

``` r
mat[3,mat[3,]>0]
```

    ##  appli   comp    cup  entri  final   free    may receiv   text    txt    win 
    ##      1      1      1      2      1      1      1      1      1      1      1 
    ##   wkli 
    ##      1

``` r
as.character(corpus[[3]])
```

    ## [1] "free entri wkli comp win fa cup final tkts st may text fa receiv entri questionstd txt ratetc appli over"

# 4 用文字雲比較 spam/ham 的字頻

``` r
y<-data_text$Class

mat2<-apply(mat,2,function(x) ifelse(x>0,1,0))
mat2[3,mat2[3,]>0]
```

    ##  appli   comp    cup  entri  final   free    may receiv   text    txt    win 
    ##      1      1      1      1      1      1      1      1      1      1      1 
    ##   wkli 
    ##      1

``` r
mat_spam<-mat2[y=="spam",]
mat_ham<-mat2[y=="ham",]

freq.spam<- sort(colSums(mat_spam), decreasing=TRUE)
freq.ham.tmp<- round(sort(colSums(mat_ham), decreasing=TRUE),digits=0)
freq.ham<-round(freq.ham.tmp*sum(freq.spam)/sum(freq.ham.tmp),digits=0)
head(freq.spam)
```

    ##  call   now  free   txt  text mobil 
    ##   328   181   169   142   122   118

``` r
head(freq.ham)
```

    ##  can  get will just  now come 
    ##   99   98   91   81   81   80

``` r
wf.spam<- data.frame(word=names(freq.spam), freq=freq.spam)
wf.ham<- data.frame(word=names(freq.ham), freq=freq.ham)

library("wordcloud2")
```

    ## Warning: 套件 'wordcloud2' 是用 R 版本 4.3.2 來建造的

``` r
wordcloud2(wf.spam[1:200,], 
           minSize = 0, size = .8)
```

![](NLP_spam_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
wordcloud2(wf.ham[1:200,], 
           minSize = 0, size = .5)
```

![](NLP_spam_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
barplot(wf.spam[1:200,2],ylim=c(0,350),main="spam")
```

![](NLP_spam_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
barplot(wf.ham[1:200,2],ylim=c(0,350),main="ham")
```

![](NLP_spam_files/figure-gfm/unnamed-chunk-20-2.png)<!-- -->

``` r
head(data.frame(wf.spam,wf.ham))
```

    ##        word freq word.1 freq.1
    ## call   call  328    can     99
    ## now     now  181    get     98
    ## free   free  169   will     91
    ## txt     txt  142   just     81
    ## text   text  122    now     81
    ## mobil mobil  118   come     80

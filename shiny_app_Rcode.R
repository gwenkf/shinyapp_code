library(data.table)
library(ggplot2)
library(ggthemes)
library(tm)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)

setwd("/sqlite/")
complete_nyt = fread("complete_nyt.csv")
complete_nyt = complete_nyt[-c(1),]

complete_nyt = as.data.table(complete_nyt, keep.rownames=TRUE)

#Articles on relief efforts/aid provision
provide_aid = complete_nyt[root_code == 07&countryname == "USA", ,]
type_provide_aid = provide_aid[ ,.(.N) ,by = .(year,code)][order(-year)]
(ggplot(data = type_provide_aid, aes(x = year, y=N, fill=code)) + geom_col(color="black") 
  + scale_fill_brewer(name = "Type of Aid", labels = c("Unspecified", "Economic Aid", "Military Aid", "Humanitarian Aid", "Military Protection or Peacekeeping", "Granting Asylum"),
                      breaks = c("70", "71", "72", "73", "74", "75"), palette = "Dark2") 
  + theme_base()
  + xlab("Year") + ylab("Number of NYT articles published relating to relief efforts and aid provision"))
unspecified_aid = provide_aid[code == 70, list(year, joined_issues) ,by= .(year,joined_issues)][order(-year)]

#Extracting and exporting issues falling under unspecified aid
issues_unspec_aid = provide_aid[code == 70, list(joined_issues), ]
issues_unspec_aid = as.character(issues_unspec_aid)
issues_unspec_aid = paste(issues_unspec_aid, sep=" ", collapse="")

write.table(issues_unspec_aid, "/Users/gfern/git_proj/test/shinyapp_code/shinyapp_code/issues_unspec_aid.txt", sep="\t")

#Text mining and word cloud creation for issues under unspecified aid
setwd("/Users/gfern/git_proj/test/shinyapp_code/shinyapp_code/")
keywords_unspec_aid = readLines(file.choose("issues_unspec_aid.txt"))
keywords_unspec_aid = Corpus(VectorSource(keywords_unspec_aid))

clean = content_transformer(function (x , pattern ) gsub(pattern, " ", x))
keywords_unspec_aid = tm_map(keywords_unspec_aid, clean, "/")
keywords_unspec_aid = tm_map(keywords_unspec_aid, clean, "@")
keywords_unspec_aid = tm_map(keywords_unspec_aid, clean, "\\|")
keywords_unspec_aid = tm_map(keywords_unspec_aid, clean, "\\\\")
keywords_unspec_aid = tm_map(keywords_unspec_aid, content_transformer(tolower))
keywords_unspec_aid = tm_map(keywords_unspec_aid, removePunctuation)
keywords_unspec_aid = tm_map(keywords_unspec_aid, removeNumbers)
keywords_unspec_aid = tm_map(keywords_unspec_aid, stripWhitespace)

unspec_aid_tm = TermDocumentMatrix(keywords_unspec_aid)
unspec_aid_matrix = as.matrix(unspec_aid_tm)
unspec_aid_rowsums = sort(rowSums(unspec_aid_matrix), decreasing = TRUE)
unspec_aid_df = data.frame(word = names(unspec_aid_rowsums), freq=unspec_aid_rowsums)
head(unspec_aid_df, 20)

set.seed(1234)
wordcloud(words = unspec_aid_df$word, freq = unspec_aid_df$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Paired"))

#Extracting and exporting article titles
titles_provide_aid = provide_aid[ , list(title), ]
titles_provide_aid = as.character(titles_provide_aid)
titles_provide_aid = paste(titles_provide_aid, sep= " ", collapse = "")

write.table(titles_provide_aid, "/Users/gfern/git_proj/test/shinyapp_code/shinyapp_code/provide_aid_titles.txt", sep="\t")

#Text mining and word cloud creation
setwd("/Users/gfern/git_proj/test/shinyapp_code/shinyapp_code/")
provide_aid_titles = readLines(file.choose("provide_aid_titles.txt"))
provide_aid_titles = Corpus(VectorSource(provide_aid_titles))

clean = content_transformer(function (x , pattern ) gsub(pattern, " ", x))
provide_aid_titles = tm_map(provide_aid_titles, clean, "/")
provide_aid_titles = tm_map(provide_aid_titles, clean, "@")
provide_aid_titles = tm_map(provide_aid_titles, clean, "\\|")
provide_aid_titles = tm_map(provide_aid_titles, clean, "\\\\")
provide_aid_titles = tm_map(provide_aid_titles, content_transformer(tolower))
provide_aid_titles = tm_map(provide_aid_titles, removePunctuation)
provide_aid_titles = tm_map(provide_aid_titles, stripWhitespace)
provide_aid_titles = tm_map(provide_aid_titles, removeWords, stopwords("english"))

provide_aid_tm = TermDocumentMatrix(provide_aid_titles)
provide_aid_matrix = as.matrix(provide_aid_tm)
provide_aid_rowsums = sort(rowSums(provide_aid_matrix), decreasing = TRUE)
provide_aid_df = data.frame(word = names(provide_aid_rowsums), freq=provide_aid_rowsums)

set.seed(1234)
wordcloud(words = provide_aid_df$word, freq = provide_aid_df$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

###############پردازش تصویر
#کتابخانه مورد نظر را نصب می کنیم
library(magick)
#عکس را وارد می کنیم
image <- image_read("C:/Users/user/Pictures/masoumeh.jpg")
print(image)
#این دستور اندازه تصویر را تغییر میدهد
image_scale(image,"600")
#این دستور ارتفاع تصویر را به 600 پیکسل تنظیم میکند و عرض متناسب با آن تغییر میکند (با حفظ نسبت ابعاد).
image_scale(image,"x600")
#این خط خروجی مقیاس بندی شده را به همان متغیر image اختصاص می دهد، به این معنا که تصویر اصلی در متغیر image با نسخه تغییر اندازه یافته جایگزین میی شود.
image<-image_scale(image,"x600")
#تنظیمات روشنایی و طیف رنگی تصویر
image<-image_modulate(image,brightness =100 ,saturation =90,hue = 100)
#نوشتن متن روی تصویر
image_annotate(image,"Masoumeh",size=30,color = "brown",boxcolor = "ivory", font = "Forte",strokecolor = "black",degrees = 0,location = "+300+0")
###############ابرکلمات
#پکیج های مورد نیاز را فراخوانی می کنیم
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("wordcloud2")
text <- readLines(file.choose())
docs <- Corpus(VectorSource(text))
#حروف بزرگ را به حروف کوچک تبدیل می کند
docs <- tm_map(docs, content_transformer(tolower))
#علائم نگارشی را حذف می کند
docs <- tm_map(docs, removePunctuation)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, c("and","the","these","over","have","also","one","are",
                                    "for","that","can","all","with","has","froms","from"))
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(200)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=150, random.order=FALSE, rot.per=0.5, 
          colors=brewer.pal(8, "Dark2"))
#با استفاده از کد بالا یک ابر کلمه ایجاد می کنیم که تا صدوپنجاه کلمه نمایش داده می شود  کلمات با توجه به فراوانی مرتب میشوند (بزرگترین کلمه در مرکز قرار میگیرد).
#نیمی از کلمات به صورت چرخیده و نیمی دیگر افقی هستند.
#رنگهای متنوع از پالت رنگی Dark2 برای زیبایی بیشتر استفاده میشود.
#با استفاده از کد زیر می توان ابر کلمات را به صورت یک ستاره در اورد که کلمات در ان چرخش نداشته باشند
wordcloud2(data= d, size = 0.7, shape = "star", color = "white", backgroundColor="blue", minRotation = -pi/6, maxRotation = -pi/6,rotateRatio = 0)
#با استفاده از کد زیر می توان ابر کلمات را به صورت یک قلب در اورد که کلمات در ان چرخش داشته باشند
wordcloud2(data= d, size = 0.7, shape = "cardioid", color = "red", backgroundColor="white", minRotation = -pi/6, maxRotation = -pi/6,rotateRatio = 1)
#در دو کد بالا محدوده زاویه چرخش کلمات منفی سی درجه است
###############تحلیل شبکه
library(igraph)
library("readxl")
#فراخوانی داده ها
data <- read_excel("C:/Users/user/Documents/data.xlsx")
x <- data.frame(data$Name, data$City)
#در اینجا کدی میزنیم تا مشخص شود که داده ها برای یک گراف است
net <- graph.data.frame(x, directed=T)
#لیست رئوس و یال های یک گراف را نمایش می دهد
V(net)
E(net)
#برچسب گذاری می کنیم
V(net)$label <- V(net)$name
V(net)$degree <- degree(net)
#هیستوگرام رسم می کنیم
hist(V(net)$degree,
     col = 'blue',
     main = 'Histogram of Node Degree',
     ylab = 'Frequency',
     xlab = 'Degree of Vertices')
#نمودار افقی تعداد یال هایی است که از یک گره خارج می شود
#نمودار عمودی فراوانی است
#گراف را رسم می کنیم
set.seed(100)
plot(net,
     vertex.color = 'red',
     vertext.size = 2,
     edge.arrow.size = 0.5,
     vertex.label.cex = 0.8)
#درجه ها را برجسته می کنیم
plot(net,
     vertex.color = rainbow(52),
     vertex.size = V(net)$degree*3,
     edge.arrow.size = 1,
     layout=layout.fruchterman.reingold)
#می توان مرکزیت شبکه را تشخیص داد
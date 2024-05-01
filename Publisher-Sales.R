```{r setup, message=FALSE}

library(tidyverse)
library(janitor)
library(gridExtra)
library(Hmisc)
library(emmeans)
library(dplyr)
options(width=100)
```
# publisher_sales

```{r}
bookdata <- read_csv('publisher_sales.csv')
bookdata <- bookdata %>% clean_names()
str(bookdata)
summary(bookdata)
bookdata$genre <- as.factor(bookdata$genre)
levels(bookdata$genre)
```

```{r}
bookdata <- bookdata %>% filter(daily_sales > 0)
summary(bookdata)
```

### Average Daily sales of books on different genres

```{r}

genre_sales <- bookdata %>% group_by(genre) %>% summarise(avg_sales = round(mean(daily_sales),2))

ggplot(genre_sales, aes(x=genre, y=avg_sales)) +geom_bar(stat = "identity") +geom_text(aes(label = avg_sales), vjust = -0.2) + labs(x="Genre", y="Average Sales", title = "Average e-Book Sales for each Genre")
```

### Book Sales depending upon their average review scores and total number of reviews

```{r}
rcorr(as.matrix(select(bookdata, avg_review, total_reviews, daily_sales)))

grid.arrange(ggplot(bookdata, aes(x=daily_sales, y=avg_review)) + geom_point() + geom_smooth() + labs(y="Average Review", x="Daily Sales"), ggplot(bookdata, aes(x=daily_sales, y=total_reviews)) + geom_point() + geom_smooth() + labs(y="Total Review", x="Daily Sales"), ggplot(bookdata, aes(x=avg_review, y=total_reviews)) + geom_point() + geom_smooth() + labs(y="Total Review", x="Average Review"), nrow=3, top="Correlation - Average Review/Total Review/Daily sales")

```

```{r}
ggplot(data = bookdata, aes(x=total_reviews)) +
  geom_histogram(binwidth = 5) +
  labs(x="Total Reviews", y="Number of e-Books", title="Distribution of Total Reviews of e-Books")
```

```{r}
salesbyreview <- lm(daily_sales ~ avg_review + total_reviews, data = bookdata)
summary(salesbyreview)
```

```{r}
index_avgreview <- which(bookdata$avg_review == 0) 
index_totalreview <- which(bookdata$total_reviews == 0)

index_avgreview
index_totalreview
```

```{r}
bookdatawithoutNull  <-  bookdata %>% filter( total_reviews!=0)

salesbyreview0 <- lm(daily_sales ~ avg_review + total_reviews, data = bookdatawithoutNull)
summary(salesbyreview0)
```

```{r}
cbind(coefficient=coef(salesbyreview0), confint(salesbyreview))
```

```{r}
summarytable <- bookdata %>% group_by(publisher_type) %>% summarise(count = n(),percentage = round(count/6000 * 100,2))

ggplot(summarytable, aes(x = publisher_type, y = percentage)) + geom_col() +geom_text(aes(label = percentage), vjust = -0.5) + theme_minimal() + labs(x="Publisher Type", y="Percentage ratio", title = "Percentage of books published by different types of publisher")
```

### Sales Price Depending upon Number of sales across different genres

```{r}
children <- bookdata %>% filter(genre == 'childrens')
fiction <- bookdata %>% filter(genre == 'fiction')
nonfiction <- bookdata %>% filter(genre == 'non_fiction')
```

```{r}
rcorr(as.matrix(children %>% select(daily_sales , sale_price)))
```

```{r}
rcorr(as.matrix(fiction %>% select(daily_sales , sale_price)))
```

```{r}
rcorr(as.matrix(nonfiction %>% select(daily_sales , sale_price)))
```

Visual Plot for different genres
```{r}
x <- ggplot(data = children, aes(x = daily_sales, y = sale_price)) + geom_point() + geom_smooth(method = lm) + labs(x= "Daily Sales", y = "Sales Price",title = "children")
```

```{r}
y <- ggplot(data = fiction, aes(x = daily_sales, y = sale_price)) + geom_point() + geom_smooth(method = lm) + labs(x= "Daily Sales", y = "Sales Price", tile = "fiction")
```

```{r}
z <- ggplot(data = nonfiction, aes(x = daily_sales, y = sale_price)) + geom_point() + geom_smooth(method = lm) + labs(x= "Daily Sales", y = "Sales Price" , title = "nonfiction")
```
```{r}
grid.arrange(x,y,z, ncol=3)
```

```{r, echo=FALSE, message=FALSE, warning=FALSE}
ggplot(genre_sales, aes(x=genre, y=avg_sales)) +geom_bar(stat = "identity") +geom_text(aes(label = avg_sales), vjust = -0.2) + labs(x="Genre", y="Average Sales", title = "Average e-Book Sales for each Genre", caption = "Figure 1")
```

```{r, echo=FALSE, message=FALSE, warning=FALSE}
ggplot(summarytable, aes(x = publisher_type, y = percentage)) + geom_col() +geom_text(aes(label = percentage), vjust = -0.5) + theme_minimal() + labs(x="Publisher Type", y="Percentage ratio", title = "Percentage of books published by different types of publisher", caption = "Figure 2")
```

```{r, echo=FALSE, message=FALSE, warning=FALSE}
grid.arrange(ggplot(bookdata, aes(x=daily_sales, y=avg_review)) + geom_point() + geom_smooth() + labs(y="Average Review", x="Daily Sales"), ggplot(bookdata, aes(x=daily_sales, y=total_reviews)) + geom_point() + geom_smooth() + labs(y="Total Review", x="Daily Sales"), ggplot(bookdata, aes(x=avg_review, y=total_reviews)) + geom_point() + geom_smooth() + labs(y="Total Review", x="Average Review"), nrow=3, top="Correlation - Average Review/Total Review/Daily sales", bottom="Figure 3")
```

```{r, echo=FALSE, message=FALSE, warning=FALSE}
grid.arrange(x,y,z, ncol=3, bottom="Figure 4")
```
---
library(dplyr)
library(igraph)
library(networkD3)

# full data
seoul_full<-read.csv(file.choose())
head(seoul_full)

# what kind of artworks
hist(as.numeric(as.character(seoul_full$수집년도)))
data.frame(table(seoul_full$작품분류명))
ggplot(seoul_full, aes(x=작품분류명)) + geom_bar()

# when they collect
ggplot(seoul_full, aes(x=수집년도)) + geom_histogram(bins = 30)

ggplot(seoul_full, aes(x=수집년도, fill=작품분류명, color=작품분류명)) +
  geom_histogram(bins = 30) + facet_grid(. ~ 작품분류명)

# where they keep
data.frame(table(seoul_full$소장처분류명))
ggplot(seoul_full, aes(x=수집년도, fill=소장처분류명, color=소장처분류명)) + geom_histogram(bins = 30) + facet_grid(. ~ 작품분류명)

# when they made
temp_seoul_full <- subset(seoul_full,제작년도!=c('년도미상'))
temp_seoul_full <- subset(temp_seoul_full,nchar(as.character(제작년도))<=4) 
temp_seoul_full <- subset(temp_seoul_full,제작년도!=c('연도미상')) 

subset(seoul_full,nchar(as.character(제작년도))>4) %>% group_by(제작년도) %>% summarise(cnt=n()) %>% arrange(-cnt)

temp_seoul_full$제작년도
ggplot(temp_seoul_full, aes(x=as.numeric(as.character(제작년도)))) + geom_histogram(bins = 30)

with(temp_seoul_full, plot(as.numeric(as.character(제작년도)), as.numeric(as.character(수집년도))))
qplot(as.numeric(as.character(수집년도)), as.numeric(as.character(제작년도)), data=temp_seoul_full) + facet_grid(. ~ 작품분류명)

# what kind of tools?
art_tools<-str_split(as.character(seoul_full$재료.기법), "에|,|/|\\(|\\)| on | in ")
art_tools<-melt(art_tools)
art_tools$value <- gsub(" ", "", art_tools$value)
art_tools$value <- gsub("\\d", "", art_tools$value)
art_tools$value <- tolower(art_tools$value)

art_tools<-subset(art_tools, nchar(as.character(art_tools$value))>0)
art_tools_sum <- art_tools %>% group_by(value) %>% summarise(n=n()) %>% arrange(-n)

quantile(art_tools_sum$n, c(1:100)*.01)
# > 5 88%

art_tools<- left_join(art_tools, art_tools_sum, c('value'))
art_tools_top <- subset(art_tools, n>=5)

names(temp_seoul_full)[1] <- 'L1'
art_tools_top_group <- left_join(art_tools_top, temp_seoul_full, c('L1'))
art_tools_top_group <- art_tools_top_group[complete.cases(art_tools_top_group), ]
art_tools_top_group <- art_tools_top_group %>% group_by(작품분류명, value) %>% summarise(n=n()) %>% arrange(-n)
art_tools_top_group <- art_tools_top_group[!duplicated(art_tools_top_group[2]),]

# by year
art_tools_top_year <- left_join(art_tools_top, temp_seoul_full, c('L1'))
art_tools_top_year <- art_tools_top_year[complete.cases(art_tools_top_year), ]
art_tools_top_year <- art_tools_top_year[c('value','L1','제작년도')]
hist(as.numeric(as.character(art_tools_top_year$제작년도)))
art_tools_top_year$제작년도 <- as.numeric(as.character(art_tools_top_year$제작년도))
art_tools_top_year$year<-floor(art_tools_top_year$제작년도/10)*10

# https://rdatamining.wordpress.com/2012/05/17/an-example-of-social-network-analysis-with-r-using-package-igraph/
# https://christophergandrud.github.io/networkD3/

art_tools_top_n <- left_join(art_tools_top, temp_seoul_full, c('L1'))
art_tools_top_n <- art_tools_top_n[complete.cases(art_tools_top_n), ]

mt<-as.matrix(with(art_tools_top_n, table(value,L1)))
mt[mt>=1] <- 1
mtmt <- mt %*% t(mt)
mtmt[5:10, 5:10]

g <- graph.adjacency(mtmt, weighted=T, mode = 'undirected')
g <- simplify(g)
gd3<-igraph_to_networkD3(g)

name <- data.frame(gd3$nodes$name)
names(name)[1]<-'value'
name<- left_join(name, art_tools_top_group, c('value'))

gd3$nodes$name
gd3$nodes$group <- name$작품분류명

forceNetwork(Links = gd3$links, Nodes = gd3$nodes, Value = "value",
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group', fontSize = 16, zoom=T, legend = T, fontFamily = "Arial", opacityNoHover=T)

gd3_igraph <- data.frame(from=gd3$links$source,to=gd3$links$target)
gd3_igraph <- graph.data.frame(gd3_igraph, directed=F)
degree<-degree(gd3_igraph)
btw<-betweenness(gd3_igraph)

name_nodes<-data.frame(gd3$nodes$name)
name_nodes$seq<-1
name_nodes$seq<-cumsum(name_nodes$seq)-1

degree<-data.frame(degree)
degree<-data.frame(seq=rownames(degree), degree$degree)
degree<-merge(degree, name_nodes, c('seq'))
degree %>% arrange(-degree.degree) %>% head(20)

btw<-data.frame(btw)
btw<-data.frame(seq=rownames(btw), btw$btw)
btw<-merge(btw, name_nodes, c('seq'))
btw %>% arrange(-btw.btw) %>% head(20)

# with time
# 1980 1990 2000 2010

temp_seoul_full$제작년도 <- as.numeric(as.character(temp_seoul_full$제작년도))
temp_seoul_full$year<-floor(temp_seoul_full$제작년도/10)*10
table(temp_seoul_full$year)
table(temp_seoul_full$year, temp_seoul_full$작품분류명)

mt_80<-as.matrix(with(subset(art_tools_top_year, year==1980), table(value,L1)))
mt_90<-as.matrix(with(subset(art_tools_top_year, year==1990), table(value,L1)))
mt_00<-as.matrix(with(subset(art_tools_top_year, year==2000), table(value,L1)))
mt_10<-as.matrix(with(subset(art_tools_top_year, year==2010), table(value,L1)))

mt_ft<-function(mt){
  mt[mt>=1] <- 1
  mtmt <- mt %*% t(mt)

  g <- graph.adjacency(mtmt, weighted=T, mode = 'undirected')
  g <- simplify(g)
  gd3<-igraph_to_networkD3(g)

  name <- data.frame(gd3$nodes$name)
  names(name)[1]<-'value'
  name<- left_join(name, art_tools_top_group, c('value'))

  gd3$nodes$group <- name$작품분류명
  return(gd3)
}

gd3_80<- mt_ft(mt_80)
gd3_90<- mt_ft(mt_90)
gd3_00<- mt_ft(mt_00)
gd3_10<- mt_ft(mt_10)

forceNetwork(Links = gd3_80$links, Nodes = gd3_80$nodes, Value = "value",
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group', fontSize = 16, zoom=T, legend = T, fontFamily = "Arial", opacityNoHover=T)

forceNetwork(Links = gd3_90$links, Nodes = gd3_90$nodes, Value = "value",
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group', fontSize = 16, zoom=T, legend = T, fontFamily = "Arial", opacityNoHover=T)

forceNetwork(Links = gd3_00$links, Nodes = gd3_00$nodes, Value = "value",
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group', fontSize = 16, zoom=T, legend = T, fontFamily = "Arial", opacityNoHover=T)

forceNetwork(Links = gd3_10$links, Nodes = gd3_10$nodes, Value = "value",
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group', fontSize = 16, zoom=T, legend = T, fontFamily = "Arial", opacityNoHover=T)


library(rqog)

# Download a local coppy of the file
dat <- read_qog(which.data = "basic")
# Subset the data
dat2 <- dat[dat$cname %in% c("Russia", "China", "India", "Brazil"), ]
dat2 <- dat2[c("cname", "year", "undp_hdi", "fh_polity2")]
dat2 <- dat2[dat2$year %in% 1990:2010, ]
# melt to long format
library(reshape2)
dat.l <- melt(dat2, id.vars = c("cname", "year"))
dat.l <- dat.l[!is.na(dat.l$value), ]
library(ggplot2)
# Plot the data
ggplot(dat.l, aes(x = year, y = value, color = cname)) + 
    geom_point() + 
    geom_line() + 
    geom_text(data = merge(dat.l, 
                           aggregate(year ~ cname, dat.l, max), 
                           by = c("year","cname")), 
              aes(x = year, y = value, label = cname), 
              hjust = 1, vjust = -1, size = 3, alpha = 0.8) + facet_wrap(~variable, scales = "free") + theme(legend.position = "none")


## ------------------------------------------------------ ##

gdp.cur <- read.csv("~/workspace/russia/book_finance/knitr/gdp_current.csv", dec=",")
gdp.cons <- read.csv("~/workspace/russia/book_finance/knitr/gdp_constant.csv", dec=",")
gdp <- join(gdp.cur,gdp.cons,by="Year")
gdp.long <- melt(gdp, id.vars="Year")


library(XML)

url <- "http://www.gks.ru/bgd/regl/b12_12/IssWWW.exe/stg/d01/07-01.htm"
x <- readHTMLTable(url, dec=",", header=TRUE)
n.rows <- unlist(lapply(x, function(t) dim(t)[1]))
df <- x[[which.max(n.rows)]]

#n2 <- df[,1]
# tai siis muokatut
n <- c("Actual final consumption of households(at current prices)",
       "percentage of GDP",
       "percentage to previous year",
       "per capita","Average per capita money income of population (monthly)",
       "Real disposable money income, percentage to previous year",
       "Average accrued monthly wages, employed in the economy",
       "Real accrued wages of an employee, percentage to previous year",
       "Average fixed pension size",
       "Real fixed pension size percentage to previous year",
       "Subsistence minimum level (average per capita):",
       "Subsistence minimum level",
       "Subsistence minimum level to previous year",
       "Subsistence minimum to pensioner",
       "Correlation with subsistence minimum level, percentage:",
       "of per capita \r\nmoney income2)",
       "of average \r\nmonthly accrued \r\nwage",
       "of average fixed \r\npensions3)",
       "Population with money income below subsistence minimum level2):",
       "mln. persons",
       "percentage \r\nof the total \r\npopulation",
       "percentage \r\nto previous \r\nyear",
       "Deficit of money\r\nincome of indigent population 2):",
       "bln. RUR \r\n(before 2000 - \r\ntrln. RUR )",
       "percentage of the \r\ntotal money \r\nincome of \r\npopulation",
       "Coefficient of funds (coefficient of \r\ndifferentiation of \r\nincome)2), times",
       "Minimum wages (annual average)",
       "Real minimum wages, percentage to previous year")
#
df <- as.data.frame(t(df[,-1]))
names(df) <- n

df.plot <- subset(df, 
                  select=c("Average per capita money income of population (monthly)",
                           "Average accrued monthly wages, employed in the economy",
                           "Average fixed pension size",
                           "Subsistence minimum level",
                           "Minimum wages (annual average)"))

#names(df.plot) <- "Average.per.capita.monthly.income"
Year <- c(1992,1995,2000,2005,2007,2008,2009,2010,2011)
df.plot <- cbind(df.plot,Year)
df.plot.l <- melt(df.plot, id.vars="Year")

df.plot <- cbind(df.plot,Year)
df.plot.l <- melt(df.plot, id.vars="Year")
df.plot.l$value <- factor(df.plot.l$value)
df.plot.l$value <- recode(df.plot.l$value, "'12104)'='1210';
                          '30185)'='3018'")
df.plot.l$value.num <- as.numeric(levels(df.plot.l$value)[df.plot.l$value])
df.plot.l <- subset(df.plot.l, Year >= 2000)

wages.long <- df.plot.l

names(gdp.long) <- c("Year","variable.gdp","value.gdp")
df <- join(gdp.long,wages.long,by="Year")
df <- subset(df, variable.gdp %in% 'GDP.in.current.prices')
df <- subset(df, variable %in% 'Average accrued monthly wages, employed in the economy')
df <- subset(df, Year >= 2000)
#
pl1 <- ggplot(df, aes(x=value.gdp, y=value.num, label=Year)) + 
    geom_point(shape=1, size=1.5) + geom_path() + geom_text(size=2, hjust=0.0, vjust=-0.5) +
    #geom_smooth(method=lm, se=FALSE) +
    labs(x = "GDP in Billion US$", y = "Roubles per Month",title="GDP vs. average wages") +
    coord_cartesian(ylim = c(1000, 25000), xlim = c(5000, 60000)) +
    theme_minimal() +
    theme(axis.title.y = element_text(size=6)) +
    theme(axis.title.x = element_text(size=6)) +
    theme(axis.text.y = element_text(size=6)) +
    theme(axis.text.x = element_text(size=6)) +
    theme(title = element_text(size=7))
#
df <- join(gdp.long,wages.long,by="Year")
df <- subset(df, variable.gdp %in% 'GDP.in.current.prices')
df <- subset(df, variable %in% 'Average fixed pension size')
df <- subset(df, Year >= 2000)
#

pl2 <- ggplot(df, aes(x=value.gdp, y=value.num, label=Year)) + 
    geom_point(shape=1, size=1.5) + geom_path() + geom_text(size=2, hjust=0.0, vjust=-0.5) +
    #geom_smooth(method=lm, se=FALSE) +
    labs(x = "GDP in Billion US$", y = "Roubles per Month",title="GDP vs. average pension size") +
    coord_cartesian(ylim = c(0, 10000), xlim = c(5000, 60000))  +
    theme_minimal() +
    theme(axis.title.y = element_text(size=6)) +
    theme(axis.title.x = element_text(size=6)) +
    theme(axis.text.y = element_text(size=6)) +
    theme(axis.text.x = element_text(size=6)) +
    theme(title = element_text(size=7))
#
df <- join(gdp.long,wages.long,by="Year")
df <- subset(df, variable.gdp %in% 'GDP.in.current.prices')
df <- subset(df, variable %in% 'Minimum wages (annual average)')
df <- subset(df, Year >= 2000)
#
pl3 <- ggplot(df, aes(x=value.gdp, y=value.num, label=Year)) + 
    geom_point(shape=1, size=1.5) + geom_path() + geom_text(size=2, hjust=0.0, vjust=-0.5) +
    #geom_smooth(method=lm, se=FALSE) +
    labs(x = "GDP in Billion US$", y = "Roubles per Month",title="GDP vs. annual average minimun wage") +
    theme(axis.title.x = element_text(size=13),
          axis.title.y  = element_text(angle=90, size=13)) +
    coord_cartesian(ylim = c(0, 6500), xlim = c(5000, 60000)) +
    theme_minimal() +
    theme(axis.title.y = element_text(size=6)) +
    theme(axis.title.x = element_text(size=6)) +
    theme(axis.text.y = element_text(size=6)) +
    theme(axis.text.x = element_text(size=6)) +
    theme(title = element_text(size=7))
#
df <- join(gdp.long,wages.long,by="Year")
df <- subset(df, variable.gdp %in% 'GDP.in.current.prices')
df <- subset(df, variable %in% 'Subsistence minimum level')
df <- subset(df, Year >= 2000)
#
pl4 <- ggplot(df, aes(x=value.gdp, y=value.num, label=Year)) + 
    geom_point(shape=1, size=1.5) + geom_path() + geom_text(size=2, hjust=0.0, vjust=-0.5) +
    #geom_smooth(method=lm, se=FALSE) +
    labs(x = "GDP in Billion US$", y = "Roubles per Month",title="GDP vs. level of subsistence minimun") +
    coord_cartesian(ylim = c(0, 7500), xlim = c(5000, 60000)) +
    theme_minimal() +
    theme(axis.title.y = element_text(size=6)) +
    theme(axis.title.x = element_text(size=6)) +
    theme(axis.text.y = element_text(size=6)) +
    theme(axis.text.x = element_text(size=6)) +
    theme(title = element_text(size=7))

pdf("figure/macroplot.pdf", width=12/2.54, height=14/2.54)
library(grid)
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,2)))
print(pl1, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(pl2, vp=viewport(layout.pos.row=1, layout.pos.col=2))
print(pl3, vp=viewport(layout.pos.row=2, layout.pos.col=1))
print(pl4, vp=viewport(layout.pos.row=2, layout.pos.col=2))
dev.off()

#

ppi <- 300
png("figure/macroplot_slide1.png", width=12/2.54*ppi, height=7/2.54*ppi, res=ppi)
library(grid)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(pl1, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(pl2, vp=viewport(layout.pos.row=1, layout.pos.col=2))
dev.off()


ppi <- 300
png("figure/macroplot_slide2.png", width=12/2.54*ppi, height=7/2.54*ppi, res=ppi)
library(grid)
grid.newpage()
pushViewport(viewport(layout=grid.layout(1,2)))
print(pl3, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(pl4, vp=viewport(layout.pos.row=1, layout.pos.col=2))
dev.off()
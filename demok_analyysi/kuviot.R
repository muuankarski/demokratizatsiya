library(rqog)

# Download a local coppy of the file
dat <- read_qog(which.data = "basic")
# Subset the data
dat2 <- dat[dat$cname %in% c("Russia", 
                             "China", "India", 
                             "Brazil"), ]
dat2 <- dat2[c("cname", "year", "undp_hdi", "wdi_gdpc")]
dat2 <- dat2[dat2$year %in% 1995:2010, ]

# dat2 <- dat[dat$cname %in% c("Russia", "China", "India", "Brazil"), ]
# dat2 <- dat2[c("cname", "year", "undp_hdi", "wdi_gdpc")]
# dat2 <- dat2[dat2$year %in% 1990:2010, ]

dat2 <- dat2[!is.na(dat2$undp_hdi), ]
dat2 <- dat2[!is.na(dat2$wdi_gdpc), ]

brazil <- dat2[dat2$cname == "Brazil",]
russia <- dat2[dat2$cname == "Russia",]
india <- dat2[dat2$cname == "India",]
china <- dat2[dat2$cname == "China",]

brazil$wdi_gdpc_relative <-brazil$wdi_gdpc / 7716.368 * 100
russia$wdi_gdpc_relative <-russia$wdi_gdpc / 7850.836 * 100
india$wdi_gdpc_relative <-india$wdi_gdpc / 1404.231 * 100
china$wdi_gdpc_relative <-china$wdi_gdpc / 1849.153 * 100

brazil$undp_hdi_relative <-brazil$undp_hdi / 0.634 * 100
russia$undp_hdi_relative <-russia$undp_hdi / 0.675 * 100
india$undp_hdi_relative <-india$undp_hdi / 0.437 * 100
china$undp_hdi_relative <-china$undp_hdi / 0.541 * 100

dat2 <- rbind(brazil,russia,china,india)

# # melt to long format
library(reshape2)
dat.l <- melt(dat2, id.vars = c("cname", "year"), measure.vars=c("undp_hdi_relative",
                                                                 "wdi_gdpc_relative",
                                                                 "undp_hdi",
                                                                 "wdi_gdpc"))

library(ggplot2)
library(wesanderson)

# Plot the data
plz <- ggplot(dat.l, aes(x = year, y = value, 
                         color = cname, 
                         label=round(value,0))) + 
    geom_point(shape=1, size=1.5) + 
    geom_path() + 
    geom_text(data=merge(dat.l, aggregate(year ~ cname, dat.l, max),
                         by=c("year","cname")),
              aes(x=year,y=value,label=cname),
              hjust=-.1,vjust=1.5,size=3) +
    theme_minimal() +
    scale_color_manual(values = wes.palette(5, "Cavalcanti")) +
    theme(legend.position="none") +
    theme(legend.title=element_blank()) +
    labs(x = "", 
         y = "") +
    theme(axis.title.y = element_text(size=8)) +
    theme(axis.title.x = element_text(size=8)) +
    theme(axis.text.y = element_text(size=8)) +
    theme(axis.text.x = element_text(size=8)) +
    theme(legend.text = element_text(size=8)) +
    theme(title = element_text(size=8)) +
    facet_wrap(~variable, ncol=2, scales="free") +
    coord_cartesian(xlim=c(1995,2012))



ggplot(dat2, aes(x=gdp_bofit, 
                 y=value.num,
                 label=year)) + 
    geom_point(shape=1, size=1.5) + geom_path() + 
    geom_text(size=2, hjust=0.0, vjust=-0.5)


dat.l <- melt(dat2, id.vars = c("cname", "year"), measure.vars=c("undp_hdi_relative",
                                                                  "wdi_gdpc_relative"))
dat.l <- dat.l[dat.l$cname != "China",]
plz2 <- ggplot(dat.l, aes(x = year, y = value, 
                         color = variable, 
                         label=round(value,0))) + 
    geom_point(shape=1, size=1.5) + 
    geom_path() + 
    geom_text(data=merge(dat.l, 
                         aggregate(year ~ cname, dat.l, max),
                         by=c("year","cname")),
              aes(x=year,y=value,label=cname),
              hjust=0,vjust=1.5,size=3) +
    theme_minimal() +
    scale_color_manual(values = wes.palette(5, "Cavalcanti")) +
    theme(legend.position="top") +
    theme(legend.title=element_blank()) +
    labs(x = "", 
         y = "") +
    theme(axis.title.y = element_text(size=8)) +
    theme(axis.title.x = element_text(size=8)) +
    theme(axis.text.y = element_text(size=8)) +
    theme(axis.text.x = element_text(size=8)) +
    theme(legend.text = element_text(size=8)) +
    theme(title = element_text(size=8)) +
    coord_cartesian(xlim=c(1995,2012))

ggsave(file="../figure/gdp_life.png", width=10, height=6)


ppi <- 300
png("../figure/macroplot0.png", width=18/2.54*ppi, height=20/2.54*ppi, res=ppi)
plz
dev.off()


## ------------------------------------------------------ ##

# GDP

# qualirt of government - ei vuotta 2012
#stand <- read_qog(which.data = "standard")
# Subset the data
#stand2 <- stand[stand$cname %in% "Russia", c("cname", "year", "wdi_gdp")]


# Käsin bofitin sivuilta & ppt. pdf:stö

year <- c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012)
gdp_bofit <- c(7306,8944,10819,13208,17027,21610,26917,33248,41277,38807,46309,55967,62218)

dat_gdp <- data.frame(year,gdp_bofit)


library(XML)

# url <- "http://www.gks.ru/bgd/regl/b12_12/IssWWW.exe/stg/d01/07-01.htm" <- vanha
url <- "http://www.gks.ru/bgd/regl/b13_12/IssWWW.exe/stg/d01/7-01.htm"

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
df_rosstat <- as.data.frame(t(df[,-1]))
names(df_rosstat) <- n

df.plot <- subset(df_rosstat, 
                  select=c("Average per capita money income of population (monthly)",
                           "Average accrued monthly wages, employed in the economy",
                           "Average fixed pension size",
                           "Subsistence minimum level",
                           "Minimum wages (annual average)"))

#names(df.plot) <- "Average.per.capita.monthly.income"
year <- c(1992,2000,2005,2007,2008,2009,2010,2011,2012)

df.plot <- cbind(df.plot,year)
df.plot.l <- melt(df.plot, id.vars="year")
df.plot.l$value <- factor(df.plot.l$value)
library(stringr)
df.plot.l$value <- str_replace_all(df.plot.l$value, "[0-9])", "")
df.plot.l$value <- factor(df.plot.l$value)
df.plot.l$value.num <- as.numeric(levels(df.plot.l$value)[df.plot.l$value])
df.plot.l <- subset(df.plot.l, year >= 2000)

wages.long <- df.plot.l

df <- merge(dat_gdp,wages.long,by="year")
df <- subset(df, year >= 2000)
df1 <- subset(df, variable %in% 'Average accrued monthly wages, employed in the economy')
df2 <- subset(df, variable %in% 'Average fixed pension size')
df3 <- subset(df, variable %in% 'Minimum wages (annual average)')
df4 <- subset(df, variable %in% 'Subsistence minimum level')

plotX <- function(dat,title) {
    ggplot(dat, aes(x=gdp_bofit, y=value.num, label=year)) + 
        geom_point(shape=1, size=1.5) + geom_path() + 
        geom_text(size=2, hjust=0.0, vjust=-0.5) +
        scale_color_manual(values = wes.palette(5, "Cavalcanti")) +
        #geom_smooth(method=lm, se=FALSE) +
        labs(x = "GDP in RUB billion", 
             y = "Roubles per Month",
             title=title) +
        theme_bw() +
        coord_cartesian(xlim=c(0,70000)) #+
#         theme(axis.title.y = element_text(size=8)) +
#         theme(axis.title.x = element_text(size=8)) +
#         theme(axis.text.y = element_text(size=8)) +
#         theme(axis.text.x = element_text(size=8)) +
#         theme(title = element_text(size=7))
}

pl1 <- plotX(df1,"GDP vs. average wages")
pl2 <- plotX(df2,"GDP vs. average pension size")
pl3 <- plotX(df3,"GDP vs. annual average minimun wage")
pl4 <- plotX(df3,"GDP vs. level of subsistence minimun")

ppi <- 300
png("../figure/macroplot1.png", width=24/2.54*ppi, height=26/2.54*ppi, res=ppi)
library(grid)
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,2)))
print(pl1, vp=viewport(layout.pos.row=1, layout.pos.col=1))
print(pl2, vp=viewport(layout.pos.row=1, layout.pos.col=2))
print(pl3, vp=viewport(layout.pos.row=2, layout.pos.col=1))
print(pl4, vp=viewport(layout.pos.row=2, layout.pos.col=2))
dev.off()

## --------------------------------------------- ##


df.plot <- subset(df_rosstat, 
                  select=c("Average per capita money income of population (monthly)",
                           "Average accrued monthly wages, employed in the economy",
                           "Average fixed pension size",
                           "Subsistence minimum level",
                           "Minimum wages (annual average)"))

#names(df.plot) <- "Average.per.capita.monthly.income"
year <- c(1992,2000,2005,2006,2007,2008,2009,2010,2011,2012)

df.plot <- cbind(df.plot,year)

df.plot.l <- melt(df.plot, id.vars="year")

df.plot.l$value <- str_replace_all(df.plot.l$value, "[0-9])", "")
df.plot.l$value <- factor(df.plot.l$value)
df.plot.l$value.num <- as.numeric(levels(df.plot.l$value)[df.plot.l$value])
df.plot.l <- subset(df.plot.l, Year >= 2000)

library(wesanderson)

pl <- ggplot(df.plot.l, aes(x=year, y=value.num, 
                      label=value.num, color=variable)) + 
    geom_line() + geom_point() + 
    geom_text(size=3, hjust=1.2, vjust=-0.5) + 
    theme_bw() +
    scale_color_manual(values = wes.palette(5, "Cavalcanti")) +
    theme(legend.position="top") +
    guides(color = guide_legend(nrow = 5,keyheight =.5)) +
    theme(legend.title=element_blank()) +
    labs(x = "Year", y = "log of Billion US$") +
#     theme(axis.title.y = element_text(size=8)) +
#     theme(axis.title.x = element_text(size=8)) +
#     theme(axis.text.y = element_text(size=8)) +
#     theme(axis.text.x = element_text(size=8)) +
#     theme(legend.text = element_text(size=8)) +
#     theme(title = element_text(size=7)) +
    coord_cartesian(xlim=c(2000,2014)) +
    scale_y_log10()


ppi <- 300
png("../figure/macroplot2.png", width=24/2.54*ppi, height=20/2.54*ppi, res=ppi)
pl
dev.off()

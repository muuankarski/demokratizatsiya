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
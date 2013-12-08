# Use of ggplot2
# Ref. Grammar of Graphics by Leland Wilkinson
# www.ggplot.org

# workshorse function for ggplot is : qplot()  {quickplot}
# plots are made up of:
#     aesthetics (size, shape, color) and
#     geoms (point ,line)

library(ggplot2)
str(mpg)
qplot(displ, hwy, data = mpg, color = drv)

# Adding a geom
qplot(displ, hwy, data = mpg, geom=c("point", "smooth"))
qplot(displ, hwy, data = mpg, color = drv, geom=c("point", "smooth"))

# Histograms
qplot(hwy, data = mpg, fill = drv)

# Facets (like panels in lattice)
qplot(displ, hwy, data = mpg, facets = .~drv) # .~drv = row ~ columns
qplot(displ, hwy, data = mpg, facets = drv~., binwidth=2)

# MAACS Cohort:
# Asthma study publication: http://goo.gl/WqE9j8
str(maacs)
qplot(log(eno), data = maacs, fill = mopos)
qplot(log(eno), data = maacs, geom = 'density')
qplot(log(eno), data = maacs, geom = 'density', color = mopos)
qplot(log(pm25), log(eno), data = maacs, , color = mopos, geom=c("points", 'smooth', method='lm'))
qplot(log(pm25), log(eno), data = maacs, , color = mopos, geom=c("points", 'smooth', method='lm'), facets = .~mopos)




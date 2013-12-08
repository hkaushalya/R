#Interacting with Plots (Indentifying Points)
# Interacting with a scatterplot
attach(mydata)
plot(x, y) # scatterplot
identify(x, y, labels=row.names(mydata)) # identify points
coords <- locator(type="l") # add lines
coords # display list 
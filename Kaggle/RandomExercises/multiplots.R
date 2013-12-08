##  Open a new default device.

get( getOption( "device" ) )()

##  Set up plotting in two rows and three columns, plotting along rows first.

par( mfrow = c( 2, 3 ) )

##  The first plot is located in row 1, column 1:

plot( rnorm( n = 10 ), col = "red", main = "plot 1", cex.lab = 1.1 )

##  The second plot is located in row 1, column 2:

plot( runif( n = 10 ), col = "blue", main = "plot 2", cex.lab = 1.1 )

##  The third plot is located in row 1, column 3:

plot( rt( n = 10, df = 8 ), col = "springgreen4", main = "plot 3",
      cex.lab = 1.1 )

##  The fourth plot is located in row 2, column 1:

plot( rpois( n = 10, lambda = 2 ), col = "black", main = "plot 4",
      cex.lab = 1.1 )

##  plot.new() skips a position.

plot.new()

##  The fifth plot is located in row 2, column 3:

plot( rf( n = 10, df1 = 4, df2 = 8 ), col = "gray30", main = "plot 5",
      cex.lab = 1.1 )
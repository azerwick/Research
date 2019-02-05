#### Creating Uniform Distributions
m <- 1000000 

a <- runif( m, min=0, max=.6 )
b <- runif( m,  min=.2, max=.5 )
c <- runif( m, min=.1, max=.5 )
d <- .1*a +.3*b + .6*c


### Generative Model Function
genfun <- function(x)
{
sum( (a*x[1]+b*x[2]+c*x[3] - d)^2 )
}

### Proportions of the ancestry
xt <- c(1/4,1/2,1/4)
genfun(xt)
#2711.821

####Very similar value to Jordan's

xs <- c(1/3, 1/3, 1/3)
genfun(xs)
2597.113
#### Wonder why this value is smaller than the previous...


####Sanity Checks
head(a*xt[1])
head(b*xt[1])
head(c*xt[1])
head(d)
sum(head((a*xt[1] + b*xt[2] + c*xt[3] - d)^2))
#####


#### Grid Search Values
#### Creating Values for all possible pi's

##first ancestry will be 0 - 100% repeated 10x each
anc1 <- c(rep(seq(0,1,by=.01), each = 10))
rep(c("1","2","3"), times = 1:3)
length(anc1)

## Second Ancestry will be 0 - 100% repeated 10x
anc2 <- c(rep( seq(0,1,.01), times = 10 ))
length(anc2)

## Third Ancestry will be 100 - 0 (1-anc2) repeated 10x
anc3 <- c(1-anc2)
length(anc3)

ancestry <- data.frame(anc1,anc2,anc3)
dim(ancestry)
#### Time to do the actual grid search
loop <- c(1:dim(ancestry)[1])

grid <- c()
##Start Time to see how long it takes
start <- Sys.time()
for(i in loop)
{
grid <- c(grid, genfun(ancestry[i,]))
}
## End time again, to see how long it takes.
end <- Sys.time()

end - start
####Time difference of 2.773467 secs


length(grid)
#### Plot the things!!!
plot(anc1,grid)
plot(anc2,grid)
plot(anc3,grid)

#### OOOOOK so looking at the 2d graphs of these doesn't REALLY make terribly
#### much sense, there are 10 rows of all the grids (0-100 10x).... and....
#### So.... maybe look at this in a 3d plot


#### 3D plotter
install.packages(plotly)
####There's a lot to Download
library(plotly)
?plot_ly

#Make the scattergraph!!!!
p <- plot_ly(x=anc1, y=anc2, z=grid)
p

#### OMG IT WORKS and it's pretty


#### Where is the minimum? Which value gives it?
ancestry[which(grid == min(grid)),]

#### Which if you check on the plot_ly it checks out.

#### Check it
genfun(ancestry[which(grid == min(grid)),])

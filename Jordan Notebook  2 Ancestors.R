#Part 1 - Top Half of page

#Create Uniform Distribution Values
M= 1000000

a <- runif( M, min= 0, max=.6 )
b <- runif( M, min = .2, max=.5 )
c <- runif( M, min= 0, max=.5 )

head(a)
min(a)
max(a)

######The Objective function the Generative Model. From the slideshow.
###### Taking the observed probability - expected probability from the gnomAD
objfunc <- function(x)
{
  sum((a*x[1]+b*x[2] - c)^2)
}

#####These are the 2 possible ancestries, 30% from pop1 70% from pop2
xt <- c(.3, .7)

head(a*xt[1]+b*xt[2] -c)

objfunc(xt)

#####[1]  34408.7
#####Same as Jordan's, i just have no idea really what this means.



#Gradient of the Objective Function
gradientfun <- function(x)
{
  c( sum( 2*a*(a*x[1]+ b*x[2]-c)), sum(2*b*(a*x[1]+b*x[2]-c)) )
  
}
gradientfun(xt)
#69026.77 70032.76
#Same as Jordan's

####So the a,b,c are drawn from the gnomAD and basically just plug and chug.####

###Part 2 - Bottom Half of Page
###For two ancestries, one SNP, we have an analytic solution given by

####x^N = (\frac{c-b}{a-b} ,  \frac{a-c}{a-b})

xn <- c( ( (c-b)/(a-b) ),( (a-c) / (a-b)) )

head(xn)
head(a)
head(b)
head(c)

c[1]-b[1]
#0.08234326
a[1]-b[1]
# -0.1520188




objfunc(xn)
#[1] 95837.1

gradientfun(xn)

#####Making an array of ancester values from 0% - 100% for the 2 ancestries
anc1 <- seq(0,1, by = .01)
anc2 <- 1-anc1

#####Combining ancestry stuff created above into 1, to use with the objfunction
#####from above

anccomb<-data.frame(anc1,anc2)

##### Start Time for the loop
start <- Sys.time()
#####Grid Search For Loop
f <- c()
forloop <- c(seq(1,101))
for (i in forloop)
{
f<-c( f, objfunc(anccomb[i,]))
}
end <- Sys.time()

end-start
### 0.2959211 secs
### Looking at jordan's code I don't know if that's seconds or...? Either way
### The 2 ancestry grid search doesn't really take that much time
### as is expected. I would imagine 3 ancesters would take...... make 5 secs
### it's just an order of magnitude/dimension increased yeah?


#####Creates a parabolic shape with a clear minimum. 
plot(anc1,f)
par(new=TRUE)
plot(anc2,f)

####Creates a plot that is pretty much identical to the one Jordan Made

#####Determining which value gives the minimum value.
anccomb(which(f == min(f)))




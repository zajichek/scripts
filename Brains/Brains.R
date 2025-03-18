#Brain image
readVol <- function(fname, dim = c(181, 217, 181)) {
    f<-gzfile(fname, open="rb")
    on.exit(close(f))
    b<-readBin(f,"integer",prod(dim),size = 1, signed = FALSE)
    array(b, dim)
}
#(a)
brain <- readVol("/group/statsoft/data/Brainweb/images/T1/t1_icbm_normal_1mm_pn3_rf20.rawb.gz")
#Choosing slice at index 100 for each dimension
image(z = brain[100,,], xlab = "Y", ylab = "Z", main = "Index 100 in X direction")
image(z = brain[,100,], xlab = "X", ylab = "Z", main = "Index 100 in Y direction")
image(z = brain[,,100], xlab = "X", ylab = "Y", main = "Index 100 in Z direction")

#(b)
library(ggplot2)
mask <- readVol("/group/statsoft/data/Brainweb/images/mask.rawb.gz")
brain_only <- subset(brain, mask == 1)
ggplot() + geom_density(aes(brain_only, fill = "black")) + xlab("Intensity") + ylab("Density") + guides(fill = F) + annotate("text", label = "CSF", x = 41, y = .009) + annotate("text", label = "Gray", x = 85, y = .015) + annotate("text", label = "White", x = 135, y = .0175)

#(c)
EMmix1 <- function(x, theta) {
  mu <- theta$mu
  sigma <- theta$sigma
  p <- theta$p
  M <- length(mu)
  ## E step
  Ez <- outer(x, 1:M, function(x, i) p[i]*dnorm(x, mu[i], sigma[i]))
  Ez <- sweep(Ez, 1, rowSums(Ez), "/")
  colSums.Ez <- colSums(Ez)
  ## M step
  xp <- sweep(Ez, 1, x, "*")
  mu.new <- colSums(xp)/colSums.Ez
  sqRes <- outer(x, mu.new, function(x, m) (x - m)ˆ2)
  sigma.new <- sqrt(colSums(Ez*sqRes) / colSums.Ez)
  p.new <- colSums.Ez / sum(colSums.Ez)
  ## pack up result
  list(mu = mu.new, sigma = sigma.new, p = p.new)
}

#Obtaining reasonable starting values
csf <- brain_only[brain_only<=70]
gray <- brain_only[brain_only>70 & brain_only<=115]
white <- brain_only[brain_only>115]
p1 <- length(csf)/length(brain_only)
p2 <- length(gray)/length(brain_only)
p3 <- length(white)/length(brain_only)

theta <- list("mu" = c(mean(csf), mean(gray), mean(white)), "sigma" = c(sd(csf), sd(gray), sd(white)), "p" = c(p1, p2, p3))

EM <- EMmix1(brain_only, theta)
$mu
[1]  47.49701  97.22827 131.82621

$sigma
[1] 11.564902 10.715485  7.961161

$p
[1] 0.1916802 0.4775411 0.3307788


#(d)
EM <- list(mu = c(47.49701, 97.22827, 131.82621), sigma = c(11.564902, 10.715485, 7.961161), p = c(0.1916802, 0.4775411, 0.3307788))

classify_brain <- function(x, theta) {
  which.max(theta$p*dnorm(x, theta$mu, theta$sigma))
}
classifications <- sapply(brain_only, classify_brain, theta = EM)
mask2 <- mask
mask2[mask == 1] = classifications

image(z = mask2[,,100], col = c("black","yellow","gray","white"), xlab = "X", ylab = "Y", main = "Voxel classification in Z direction")
legend(0,1,legend=c("CSF","Gray","White"),col=c("yellow","gray","black"),pch=c(16,16,1))

#(e)
Rprof()
EMmix1(brain_only,theta) 
summaryRprof() #Spending most time in 'dnorm'

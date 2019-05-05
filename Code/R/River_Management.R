# River management
x <- seq(-10,10,0.1)
R1 <- x^2
R2 <- x-2
x2 <- seq(0.5,1.375,0.1)
Channel <- -x2+0.8
# pdf("../Results/Rivers.pdf",5,5)
plot(R1 ~ x, type="l", xlab="Distance (km)", ylab="Distance (km)", xlim=c(-5,5), ylim=c(-5,5), lty=1, lwd=3, col="blue")
points(R2 ~ x, type="l", lty=1, lwd=3, col="blue")
text(3, 0.3, pos=4, labels="y = x - 2")
text(2.5, 4.5, pos=4, labels=expression(paste("y = ", x^2)))
points(Channel ~ x2, type="l", lty=2, lwd=2, col="blue")
text(0.3,-1,pos=3, labels="Channel", cex=0.7)
points(x=1.375, y=-0.625)
points(x=0.5, y=0.25)
# dev.off()
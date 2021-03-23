crxdf <- read.table(crxdfr.txt)
segdf <- read.table(segdfr.txt)
segdf <- segdf[,-1433]

ROW <- nrow(crxdf)
COL <- ncol(segdf)

lcrdf <- as.data.frame(matrix(nrow = 530, ncol = 1432))
lcrdf[is.na(lcrdf)] <- 0

for(rowidx in 1:ROW) {
crxidx <- 1
segidx <- 1
lcridx <- 1
while((crxidx <= COL)  (segidx <= COL)  (lcridx <= COL)) {

if((segdf[rowidx,segidx])*(crxdf[rowidx,crxidx])==0) {
lcrdf[rowidx,lcridx] <- 0
lcridx <- lcridx+1
crxidx <- crxidx+1
}
else {
lcrdf[rowidx,lcridx] <- (segdf[rowidx,segidx])*(crxdf[rowidx,crxidx])
crxidx <- crxidx+1
segidx <- segidx+1
lcridx <- lcridx+1
		}
	}
}

nbkcount <- vector()
nbkcount <- c(nbkcount,1:1432)
nbkcount <- c(nbkcount %% 1)

lcrcount <- vector()
lcrcount <- c(lcrcount,1:1432)
lcrcount <- c(lcrcount %% 1)

for(j in 1:COL) {
for(i in 1:ROW) {
if(lcrdf[i,j] == 2)

lcrcount[j] <- lcrcount[j]+1
else {
if(lcrdf[i,j] == 1)
nbkcount[j] <- nbkcount[j]+1
		}
	}
}

total <- nbkcount + lcrcount

ratio1 <- lcrcount / nbkcount
jpeg(ratio1.jpg)
plot(ratio1,xlab=Creixell MSA index [1:1432],ylab=Ratio of LCR-count to Non-LCRs)
dev.off()

ratio2 <- lcrcount / total
jpeg(ratio2.jpg)
plot(ratio2,xlab=Creixell MSA index [1:1432],ylab=LCR-count to total Non-blanks ratio)
dev.off()

percent1 <- (lcrcount * nbkcount * 100) / ROW
jpeg(percent1.jpg)
plot(percent1,xlab=Creixell MSA index [1:1432],ylab=Ratio of LCR*Non-LCRs per alignment)
dev.off()

percent2 <- (lcrcount * total * 100) / ROW
jpeg(percent2.jpg)
plot(percent2,xlab=Creixell MSA index [1:1432],ylab=Ratio of LCR*Total Non-blanks per ALN)
dev.off()

lcrdens <- (lcrcount/ROW)
jpeg(lcrdens.jpg)
plot(lcrdens,xlab=Creixell MSA index [1:1432],ylab=LCR-density values)
dev.off()

jpeg(lcrcount.jpg)
plot(lcrcount,xlab=Creixell MSA index [1:1432],ylab=LCR-count values)
dev.off()


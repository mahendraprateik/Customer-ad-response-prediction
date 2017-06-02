#importing the big dataset
mlball <- read.csv("mlball.csv")
dim(mlball)

mlball <- mlball[,-c(1:36, 39)]
dim(mlball)

#Creating the correlation matrix
mlballcorr <- cor(mlball)*100
write.csv(mlballcorr, "mlballcorr.csv")

attach(mlball)

cor(H, AB) #79%
cor(PA, AB) #73%
cor(H, RBI) #71%
cor(H, AVG) #98%
cor(XBH, X2B) #71%
cor(HR, SLG) #82%
cor(HR, OPS) #71%
cor(TB, HR) #77%
cor(XBH, HR) #80%
cor(XBH, RBI) #85%
cor(TB, RBI) #90%
cor(OPS, RBI) #94%
cor(SLG, RBI) #90%
cor(OBP, RBI) #83%
cor(AVG, RBI) #71.8%
cor(BB, NP) #76%
cor(AVG, OBP) #82.23%
cor(AVG, OPS) #79%
cor(AVG, OBP) #82%
cor(OBP, SLG) #70%
cor(OBP, OPS) #85.23%
cor(OBP, TB) #72%
cor(PA, OBP) #80%
cor(SLG, OPS) #96%
cor(TB, SLG) #98%
cor(SLG, XBH) #93.9%
cor(XBH, OPS) #89%
cor(OPS, TB) #96%
cor(TB, XBH) #93%
cor(GO_AO, GO) #78%
cor(GO_AO, AO) #79%
cor(NP, PA) #74%

mlballcorr[upper.tri(mlballcorr)] <- 0
diag(mlballcorr) <- 0
data.new <- data[,!apply(mlballcorr,2,function(x) any(x > 0.7))]
head(data.new)
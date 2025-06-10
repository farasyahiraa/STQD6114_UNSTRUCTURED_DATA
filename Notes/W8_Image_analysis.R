
install.packages('imager')
library(imager)

# load image from imager package
file <- system.file('extdata/parrots.png',package='imager')
parrots <- load.image(file)
plot(parrots) #Parrots

#read the rose that you downloaded
rose <- load.image("Rose.png") #Load image
plot(rose) #Rose #xline menegak, y line melintang

save.image(rose,"Rose.jpeg") #save image

parrots.blurry <- isoblur(parrots,10) #Blurry parrots
parrots.xedges <- deriche(parrots,2,order=2,axis="x") #Edge detector along x-axis
parrots.yedges <- deriche(parrots,2,order=2,axis="y") #Edge detector along y-axis
gray.parrots <- grayscale(parrots) #Make grayscale
plot(gray.parrots)

#Denoising
# process. 1) to have the original image, make it blur.
#          2) remove background
#          3) denoise it
birds <- load.image(system.file('extdata/Leonardo_Birds.jpg',package='imager'))
birds.noisy <- (birds + .5*rnorm(prod(dim(birds)))) 
layout(t(1:2))
plot(birds.noisy,main="Original")
isoblur(birds.noisy,5) %>% plot(main="Blurred")
plot(birds.noisy,main="Original")
72
blur_anisotropic(birds.noisy,ampl=1e3,sharp=.3) %>% plot(main="Blurred (anisotropic)")
                                                         
#Resize, rotate, ext.
thmb <- resize(parrots,round(width(parrots)/10),round(height(parrots)/10))
plot(thmb,main="Thumbnail") #Pixellated parrots

thmb <- resize(parrots,-10,-10) #minus 10% from the original size (x,y)
imrotate(parrots,30) %>% plot(main="Rotating") #Rotate
imshift(parrots,40,20) %>% plot(main="Shifting") #Shift
imshift(parrots,100,100,boundary=2) %>% plot(main="Shifting (circular)") #Shift circular

#Histogram equalisations
plot(boats)
grayscale(boats) %>% hist(main="Luminance values in boats picture")
R(boats) %>% hist(main="Red channel values in boats picture")

install.packages('ggplot2')
library(ggplot2)

install.packages('dplyr')
library(dplyr)

bdf <- as.data.frame(boats)
head(bdf,3)
bdf <- mutate(bdf,channel=factor(cc,labels=c('R','G','B')))
ggplot(bdf,aes(value,col=channel))+geom_histogram(bins=30)+facet_wrap(~ channel)

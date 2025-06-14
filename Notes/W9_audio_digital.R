install.packages('tuneR')
library(tuneR)
install.packages('seewave')
library(seewave)

# let's make a sine wave and play it 
help(Wave)
sr = 8000                      #the sampling rate. 8000hz used for video analysis
t = seq(0, 2, 1/sr)            #times in secs if sample for 2 seconds at 8KHz
y = (2^15-1)*sin(2*pi*440*t)   # sine wave a 440 Hz scaled to fill out 16 bit range

#plot(y, type="l")
plot(y[1:500], type="l")
w = Wave(y, samp.rate = sr, bit = 16)      # make the Wave representation
play(w) 
#close the media player to analyse next audio

plot(y[1:500], type="l")
y1 = (2^15-1)*sin(2*pi*220*t)   #common func used to create signal wave. 220 can be change 
#y1 = (represent frequency)*sin(2*pi*changeable*t)

w1 = Wave(y1, samp.rate = sr, bit = 16)
play(w1)
#close the media player

#Compare w and w1
par(mfrow=c(2,1)) #layout parallel 
plot(y[1:500], type="l") 
plot(y1[1:500], type="l")

#Compare in 1 layout (make redundant graph for easier comparison)
par(mfrow=c(1,1))
plot(y[1:500], type="l") #plot y above
lines(y1[1:500], lty=3, col="red") #plot y1 in plot y

#Try create another
wsum = normalize(w+w1, unit='16') #must normalize to make it also 16hz
play(wsum)
#close media player

plot((y+y1)[1:500], type='l') #plot from 1 to 500
w3 <- normalize(bind(w,w1,wsum), unit='16')
play(w3)

y3 <- c(y,y1, y+y1)
plot(y3, type='l')
#The above to create secara manual. Next we will use the function in tuneR
# Other types of waveforms,
?sine
Au1 <- sine(500, duration=100000) #100=freq, duration if 100000, means if divide 44100, its in 2+ second only
play(Au1)

writeWave(Au1, 'Audio1.wav') #save data
Au11 <- readWave('Audio1.wav') #Read the audio
Au2 <- noise(duration = 100000) #look noise function from ?sine
play(Au2) #seems sound broken tv/radio

Au2 <- noise(kind='pink',duration = 100000) #Change to pink noise (correlated noise). 
play(Au2)

Au3 <- pulse(220, duration = 100000)
play(Au3)

#plot 3 signal above
par(mfrow=c(3,1))
plot(Au1[1:1000]) #The first 1000 signal for Au1
plot(Au2[1:1000])
plot(Au3[1:1000])

#other signal in ?sine
Au4 <- sawtooth(100, duration = 100000)
play(Au4)

plot(Au4[1:1000])
plot(Au4[1:2000])
Au5 <- square(200, duration = 100000)
play(Au5)

plot(Au5[1:2000])
#Combine all Au1 sampaiAu5
Asum = normalize(bind(Au1, Au2, Au3, Au4, Au5), unit='32')
play(Asum)

Au6 = normalize(bind(Au1+Au2), unit='32')
play(Au6)

#data tico from notes seewave
data(tico)
par(mfrow=c(1,1))
timer(tico, f=22050, threshold=5, msmooth=c(50,0))#f is freq, msmooth to smooth the line

?timer

timer(tico, f=22050, threshold=5, msmooth=c(100,0)) 
sr = 8000 
t = seq(0, 2, 1/sr) 
y = (2^15-1)*sin(2*pi*440*t)
plot(y[1:500], type="l")
spectrum(y, span=20, log=c("no"))
plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data <- cbind(0:(length(X.k)-1), Mod(X.k))
  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

Yk <- fft(y)
plot.frequency.spectrum(Yk)
plot.frequency.spectrum(Yk[1:5000])#look roughly where the signal is 
plot.frequency.spectrum(Yk[1:1000])#around 800+

data(tico)
Ticok <- fft(tico@left) #audio file have left channel and right channel, so we want only left channel 
plot.frequency.spectrum(Ticok[1:(length(Ticok)/2)])#mirror, focus only one side

install.packages('rpanel')
library(rpanel)

dynspec(tico, wl=1024, osc=T) #function seewave
spectro(tico) #give spectrogram
meanspec(tico) #obtain average for the whole time length 
z = readWave("babycry.wav")
play(z)

timer(z, f=22050, threshold=5, msmooth=c(100,0))
Zk <- fft(z@left)
plot.frequency.spectrum(Zk)
plot.frequency.spectrum(Zk[1:20000])
dynspec(z, wl=1024, osc=T) #function seewave
spectro(z) #give spectrogram
meanspec(z) #obtain average for the whole time length

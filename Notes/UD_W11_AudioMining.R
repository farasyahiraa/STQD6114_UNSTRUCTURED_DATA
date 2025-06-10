########################################################
############### Audio Mining ###########################
########################################################

#---------------#
# Load Packages #
#---------------#

# analyze music & speech
install.packages('tuneR') 
# analyzing, manipulating, displaying, editing and synthesizing time waves
install.packages('seewave')
# interactive control for R
install.packages('rpanel')

library(tuneR)
library(seewave)
library(rpanel)

#-----------------------------------#
# Create Wave - NO PACKAGE FUNCTION #
#-----------------------------------#

#1. Create Sine Wave

# Set sampling rate
sample.rate <- 8000 # 8000 = 8kHz

# Generate time vector (2 seconds)
time <- seq(0, 2, by = 1/sample.rate)

#---------------------------------------------------------------------------#

#1(a) 1st Sine Wave
# Create Sine wave at 400 Hz (frequency)
# Scaled to 16-bit Pulse-Code Modulation (PCM)
# 16-bit PCM is standard digital audio encoding
y <- (2^15 - 1) * sin(2 * pi * 400 * time)

# Convert to Wave object
sine.wave <- Wave(y,
                  samp.rate = sample.rate,
                  bit = 16)

# sine.wave <- Wave(left = as.integer(y), samp.rate = sample.rate, bit = 16)
# left -> left channel
# right -> right channel

# Plot the 1st 500 signals in line plot
plot(y[1:500], type="l")
# High frequency because wave-length is short

# Play wave
play(sine.wave)
# To continue analysis, need to close the .wmp pop out

#---------------------------------------------------------------------------#

#1(b) 2nd Sine Wave
# Create Sine wave at 220 Hz (frequency)
# Scaled to 16-bit Pulse-Code Modulation (PCM)
# 16-bit PCM is standard digital audio encoding
y1 <- (2^15 - 1) * sin(2 * pi * 220 * time)

# Convert to Wave object
sine.wave1 <- Wave(y1,
                  samp.rate = sample.rate,
                  bit = 16)

# sine.wave <- Wave(left = as.integer(y), samp.rate = sample.rate, bit = 16)
# left -> left channel
# right -> right channel

# Plot the 1st 500 signals in line plot
plot(y1[1:500], type="l")
# Low frequency because wave-length is long

# Play wave
play(sine.wave1)

#############################################################################

#2. Comparing both plots

#2(a) Comparing vertically
par(mfrow=c(2,1))
plot(y[1:500], type="l")
plot(y1[1:500], type="l")

#2(a) Comparing in combine plot
par(mfrow=c(1,1))
plot(y[1:500], type="l", col = "darkgreen")
lines(y1[1:500], type="l", col = "red")

#############################################################################

#3. Create Another Sine Wave

#3(a) Additive Sine Wave

# Create additive Sine wave
y.add <- y+y1

# Convert to Wave object
# Must normalize, so new wave can have 16 Hz
sine.wave.add <- normalize(sine.wave + sine.wave1, unit = "16")

# Plot the 1st 500 signals in line plot
plot(y.add[1:500], type="l")
# complex periodic signal, appearing to be a combination of multiple sinusoidal waves

# Play wave
play(sine.wave.add)

#---------------------------------------------------------------------------#

#3(b) Combine Sine Wave

# Create additive Sine wave
y.comb <- c(y,y1, y+y1)

# Convert to Wave object
# Must normalize, so new wave can have 16 Hz
sine.wave.comb <- normalize(bind(sine.wave, sine.wave1, sine.wave.add),
                            unit = "16")

# Plot the 1st 500 signals in line plot
plot(y.comb[1:500], type="l")
# combination of multiple sinusoidal waves

# Play wave
play(sine.wave.comb)

#--------------------------------#
# Create Wave - PACKAGE FUNCTION #
#--------------------------------#

#1. Create Sine Wave
# Create Sine wave at 500 Hz (frequency)
# 100,000 in duration indicate samples
# So actual duration is 100,000/44100 Hz = +- 2.27 seconds
# By default sample rate is 44100
aud.sine <- sine(500, duration = 100000)
play(aud.sine)

# Write audio file
writeWave(aud.sine, "Audio_Sine.wav")

# Read audio file (whether as new vector or just read)
readWave("Audio_Sine.wav")

#############################################################################

#2. Create Noise Wave
# No need to define the frequency
# 100,000 in duration indicate samples
# So actual duration is 100,000/44100 Hz = +- 2.27 seconds
# By default sample rate is 44100

#2(a) Equal intensity accross all frequency (white)
aud.noise.white <- noise(kind = "white", duration = 100000)
play(aud.noise.white)
# Sound like tv/radio without channel
# if kind not mention, by default it is "white" kind

#2(b) More energy in lower frequency (pink) (correlated noise)
aud.noise.pink <- noise(kind = "pink",duration = 100000)
play(aud.noise.pink)

#2(c) Emphasis on more lower frequency (red)
aud.noise.red <- noise(kind = "red",duration = 100000)
play(aud.noise.red)

#############################################################################

#3. Create Pulse Wave
# Create Pulse wave at 220 Hz (frequency)
# 100,000 in duration indicate samples
# So actual duration is 100,000/44100 Hz = +- 2.27 seconds
# By default sample rate is 44100
aud.pulse <- pulse(220, duration = 100000)
play(aud.pulse)

#############################################################################

#4. Create Sawtooth Wave
# Create Sawtooth wave at 220 Hz (frequency)
# 100,000 in duration indicate samples
# So actual duration is 100,000/44100 Hz = +- 2.27 seconds
# By default sample rate is 44100
aud.stooth <- sawtooth(220, duration = 100000)
play(aud.stooth)

#############################################################################

#5. Create Square Wave
# Create Square wave at 200 Hz (frequency)
# 100,000 in duration indicate samples
# So actual duration is 100,000/44100 Hz = +- 2.27 seconds
# By default sample rate is 44100
aud.square <- square(200, duration = 100000)
play(aud.square)

#############################################################################

#6. Create Combine Wave
# Must normalize, so new wave can have 32 Hz
aud.comb <- normalize(bind(aud.sine,
                           aud.noise.white,
                           aud.pulse,
                           aud.stooth,
                           aud.square),
                      unit = "32")
play(aud.comb)


#############################################################################

#7. Plotting

#7(a) Plot Sine, Noise & Pulse Wave
# Plot the 1st 1000 signals
par(mfrow=c(3,1))
plot(aud.sine[1:1000], main = "Sine Wave", col = "blue") 
plot(aud.noise.white[1:1000], main = "Noise Wave", col = "red")
plot(aud.pulse[1:1000], main = "Pulse Wave", col = "orange")

#---------------------------------------------------------------------------#

#7(b) Plot Sawtooth Wave
plot(aud.stooth[1:1000], main = "Sawtooth Wave - 1st 1000 signal")
plot(aud.stooth[1:2000], main = "Sawtooth Wave - 1st 2000 signal")

#---------------------------------------------------------------------------#

#7(c) Plot Square Wave
par(mfrow= c(1,1))
plot(aud.square[1:2000], main = "Square Wave")


#-------------------------------------#
# timer function from seewave package #
#-------------------------------------#
# timer function is use to identify segment with audio wave
# however, if using this function for audio analysis
# it may not be enough information
# same goes if we take the full audio 

#1. Load "tico" audio from seewave package
data(tico)
plot(tico) # wave spread in segment for 2 seconds
play(tico) # sound of bird chirping 

#############################################################################

#2. Check audio sample rate
tico@samp.rate # sample rate @ 22050

#############################################################################

#3. Smooth envelope 
# f = sample rate
# msmooth = smoothing envelope line at "x" ms window
# threshold = to determine start/end times of acoustic events (%)

# 50 ms window
par(mfrow = c(2,1))
timer(tico, f=22050, threshold=5, msmooth=c(50,0))
# 100 ms window
timer(tico, f=22050, threshold=5, msmooth=c(100,0))

# from timer() function, we can get result Onset and Offset Times
result.timer <- timer(tico, f=22050, threshold=5, msmooth=c(50,0))

segment.df <- data.frame(start = result.timer$s.start,
                         end = result.timer$s.end)

#############################################################################

#4. Extract audio into segment

segment.list <- list()

for (i in seq_len(nrow(segment.df))) {
  segment.list[[i]] <- cutw(tico,
                             from = segment.df$start[i],
                             to = segment.df$end[i],
                             output = "Wave")
}

#############################################################################

#5. Analyze Spectogram

# Spectrogram of segment 1
windows(10,10)
spectro(segment.list[[1]], f = 22050, main = "Segment 1")

# Or loop through all
windows(10,10)
for (i in seq_along(segment.list)) {
  spectro(segment.list[[i]], f = 22050, main = paste("Segment", i))
}

#############################################################################

#6. Analyze Spectrum

# Spectrum of segment 1
spec(segment.list[[1]], f = 22050, main = "Segment 1")

# Or loop through all
for (i in seq_along(segment.list)) {
  spec(segment.list[[i]], f = 22050, main = paste("Segment", i))
}

#############################################################################

#6. Analyze Average spectrum (average for the whole time length)

# Spectrum of segment 1
meanspec(segment.list[[1]], f = 22050, main = "Segment 1")

# Or loop through all
for (i in seq_along(segment.list)) {
  meanspec(segment.list[[i]], f = 22050, main = paste("Segment", i))
}


#---------------------------------------#
# Fast Discrete Fourier Transform (FFT) #
# Short-Time Fourier Transform (STFT)   #
#---------------------------------------#
# Compute the DFT on successive sections along the entire sound signal
# So no need to segmentize into window

# Set sampling rate
sample.rate <- 8000 # 8000 = 8kHz

# Generate time vector (2 seconds)
time <- seq(0, 2, by = 1/sample.rate)

# Create Sine Wave
# Create Sine wave at 440 Hz (frequency)
# Scaled to 16-bit Pulse-Code Modulation (PCM)
# 16-bit PCM is standard digital audio encoding
y2 <- (2^15 - 1) * sin(2 * pi * 440 * time)

#############################################################################

# Spectrum (amplitude distributed over frequency) 
spectrum(y2, span=20, log=c("no"))
# volume high, sound fast

#############################################################################

# plot frequency spectrum function
# show how the energy or amplitude of signal is distributed across different frequencies
plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data <- cbind(0:(length(X.k)-1), Mod(X.k))
  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2]
  plot(plot.data, t="h", lwd=2, main="",
       xlab="Frequency (Hz)", ylab="Strength",
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

# Plotting y2 wave
# Before plotting, data must be converted to FFT using fft() function
y.fft <- fft(y2)

par(mfrow=c(3,1))
plot.frequency.spectrum(y.fft)
plot.frequency.spectrum(y.fft[1:5000]) #look roughly where the signal is
plot.frequency.spectrum(y.fft[1:1000]) #around 800+


#############################################################################

# Plotting tico wave
tico.fft <- fft(tico@left)
# to check audio channel (audio/ audio@right / audio@left)
plot.frequency.spectrum(tico.fft[1:(length(tico.fft)/2)])
#mirror, focus only one side

# dynspec (STFT), so no need to separate into segment
# wl = window length
# osc = True, will show position on current spectrum
dynspec(tico, wl=1024, osc=T)

# Spectrogram
windows(10,10)
spectro(tico)

# Average spectrum (Average for the whole time length)
windows(10,10)
meanspec(tico) 


#----------#
# Exercise #
#----------#

cry = readWave("Audio_Data/babycry.wav")
play(cry)

# Check audio sample rate
cry@samp.rate #8000

# Show segment
windows(10,10)
timer(cry, f=8000, threshold=5, msmooth=c(100,0))

# Convert data to FFT
cry.fft <- fft(cry@left)

# Plot Frequency Spectrum
windows(10,10)
plot.frequency.spectrum(cry.fft)
plot.frequency.spectrum(cry.fft[1:20000])

# STFT
dynspec(cry, wl=1024, osc=T)

# Spectogram
spectro(cry)

# Average spectrum
meanspec(cry) 



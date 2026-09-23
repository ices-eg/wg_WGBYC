#define the most recent year of the data provided (aka the present year minus one as a practical guess)
currentyear <- as.numeric(substr(Sys.Date(), 1, 4)) - 1
print(currentyear)

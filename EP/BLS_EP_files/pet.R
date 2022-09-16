## Total volumes of distribution (VT) from a simulated PET study
## including a baseline scan, as well as two other scans after
## administration of a drug. Note that each row in the matrix
## represents a ROI, whilst each column represents a scan.
library(occ)
data(occ.example)
occ.example

m = occ(occ.example)

print(m)     # Prints the neuroreceptor occupancy coefficients

summary(m)   # Also prints the non-displaceable volume of

fitted(m)    # Prints the fitted values

residuals(m) # Prints the residuals

plot(m)      # Plots the estimated and observed volumes of
# distribution


library(oro.pet)
n <- 11
h <- seq(200, 150, length=n)
w <- seq(80, 120, length=n)
cbind(h, w, leanBodyMass(h, w, "male"), leanBodyMass(h, w, "female"))


leanBodyMass(height, weight, gender)

hotSpotSUV(suv, radius = 10, type = "3D")

totalSUV(suv, mask, z, bg, local = TRUE)






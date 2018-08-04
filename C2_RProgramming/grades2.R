grades <- function(filename){

# read data from file
data <- read.csv(filename,header=TRUE)

# Sort Data Descendingly
data <- data[order(-data[,2]),1:2]
# remove NAs
no_longer <- is.na(data[,2])
data <- data[!no_longer,]

marks <- data[,2]


# First Level

threshold <- trunc(mean(marks)*10^2)/10^2
C_more <- marks[marks>=threshold]
below_C <- marks[marks<threshold]


# Second Level / Below_C

threshold <- trunc(mean(below_C)*10^2)/10^2
Cm_Dp <- below_C[below_C>=threshold]
D_F <- below_C[below_C<threshold]

# Second Level / C_more

threshold <- trunc(mean(C_more)*10^2)/10^2
B_more <- C_more[C_more>=threshold]
below_B <- C_more[C_more<threshold]


#Third Level: F, D, Dp, Cm

threshold <- trunc(mean(Cm_Dp)*10^2)/10^2
Cm <- cbind(Cm_Dp[Cm_Dp>=threshold], 68)        #"C-"
Dp <- cbind(Cm_Dp[Cm_Dp<threshold], 65)         #"D+"

threshold <- trunc(mean(D_F)*10^2)/10^2
D <- cbind(D_F[D_F>=threshold], 62)             #"D"
F <- cbind(D_F[D_F<threshold], 55)              #"F"


#Third Level: Am_more, Bp_B | Cp_Bm, C

threshold <- trunc(mean(B_more)*10^2)/10^2
Am_more <- B_more[B_more>=threshold]
Bp_B <- B_more[B_more<threshold] 

threshold <- trunc(mean(below_B)*10^2)/10^2
Bm_Cp <- below_B[below_B>=threshold]
C <- cbind(below_B[below_B<threshold], 71)        #"C"

#Fourth Level: A_Ap, Am, Bp, B, Bm, Cp

threshold <- trunc(mean(Am_more)*10^2)/10^2
Ap_A <- Am_more[Am_more>=threshold]
Am <- cbind(Am_more[Am_more<threshold], 90)       #"A-"

threshold <- trunc(mean(Bp_B)*10^2)/10^2
Bp <- cbind(Bp_B[Bp_B>=threshold], 86)            #"B+"
B <- cbind(Bp_B[Bp_B<threshold], 82)              #"B"

threshold <- trunc(mean(Bm_Cp)*10^2)/10^2
Bm <- cbind(Bm_Cp[Bm_Cp>=threshold], 78)          #"B-"
Cp <- cbind(Bm_Cp[Bm_Cp<threshold], 74)           #"C+"


#Fifth Level: Ap, A
threshold <- trunc(mean(Ap_A)*10^2)/10^2
Ap <- cbind(Ap_A[Ap_A>=threshold], 100)           #"A+"
A <- cbind(Ap_A[Ap_A<threshold], 95)              #"A"

grades <- as.data.frame(cbind(data[,1],rbind(Ap, A, Am, Bp, B, Bm, Cp, C, Cm, Dp, D, F)))
grades

}
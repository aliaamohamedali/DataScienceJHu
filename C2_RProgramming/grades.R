grades <- function(data){

# First Level

threshold <- trunc(mean(data)*10^2)/10^2
C_more <- data[data>=threshold]
below_C <- data[data<threshold]


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
Cm <- Cm_Dp[Cm_Dp>=threshold]
Dp <- Cm_Dp[Cm_Dp<threshold]

threshold <- trunc(mean(D_F)*10^2)/10^2
D <- D_F[D_F>=threshold]
F <- D_F[D_F<threshold]


#Third Level: Am_more, Bp_B | Cp_Bm, C

threshold <- trunc(mean(B_more)*10^2)/10^2
Am_more <- B_more[B_more>=threshold]
Bp_B <- B_more[B_more<threshold] 

threshold <- trunc(mean(below_B)*10^2)/10^2
Bm_Cp <- below_B[below_B>=threshold]
C <- below_B[below_B<threshold]

#Fourth Level: A_Ap, Am, Bp, B, Bm, Cp

threshold <- trunc(mean(Am_more)*10^2)/10^2
Ap_A <- Am_more[Am_more>=threshold]
Am <- Am_more[Am_more<threshold]

threshold <- trunc(mean(Bp_B)*10^2)/10^2
Bp <- Bp_B[Bp_B>=threshold]
B <- Bp_B[Bp_B<threshold]

threshold <- trunc(mean(Bm_Cp)*10^2)/10^2
Bm <- Bm_Cp[Bm_Cp>=threshold]
Cp <- Bm_Cp[Bm_Cp<threshold]


#Fifth Level: Ap, A
threshold <- trunc(mean(Ap_A)*10^2)/10^2
Ap <- Ap_A[Ap_A>=threshold]
A <- Ap_A[Ap_A<threshold]

grades <- list(Ap, A, Am, Bp, B, Bm, Cp, C, Cm, Dp, D, F)
grades

}
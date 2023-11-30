
SUBROUTINE insert(Ncol,Nrow,Ndof,Ngrid,Jcore,Z,Dz,Temp,Dtemp,Ipr)
   IMPLICIT NONE
   INTEGER Ipr , Jcore , Ncol , Ndof , Ngrid , Nrow
   DOUBLE PRECISION Dtemp(9) , Dz(1)
   REAL Temp(9) , Z(1)
   INTEGER i1 , i2 , is1 , iz1 , iz2
!
! INSERT INSERTS MATRIX PARTITONS INTO OPEN CORE FOR IS2D8
!
!
   is1 = Ngrid*Ndof**2
!
! COMPUTE STARTING POINTS INTO OPEN CORE FOR THIS PARTITION AND ITS TRAN
!
   iz1 = is1*(Nrow-1) + Ndof*(Ncol-1) + Jcore - 1
   iz2 = is1*(Ncol-1) + Ndof*(Nrow-1) + Jcore - 1
!
! IZ1 GETS TEMP,  IZ2 GETS THE TRANSPOSE
!
   i1 = iz1
   i2 = iz2
!
   IF ( Ipr==2 ) THEN
!
!
! DO THE SAME IN DOUBLE PRECISION
!
      IF ( Ndof==1 ) THEN
!
         Dz(i1+1) = Dtemp(1)
         Dz(i2+1) = Dtemp(1)
      ELSE
!
         Dz(i1+1) = Dtemp(1)
         Dz(i2+1) = Dtemp(1)
         Dz(i1+2) = Dtemp(2)
         Dz(i2+25) = Dtemp(2)
         Dz(i1+3) = Dtemp(3)
         Dz(i2+49) = Dtemp(3)
         Dz(i1+25) = Dtemp(4)
         Dz(i2+2) = Dtemp(4)
         Dz(i1+26) = Dtemp(5)
         Dz(i2+26) = Dtemp(5)
         Dz(i1+27) = Dtemp(6)
         Dz(i2+50) = Dtemp(6)
         Dz(i1+49) = Dtemp(7)
         Dz(i2+3) = Dtemp(7)
         Dz(i1+50) = Dtemp(8)
         Dz(i2+27) = Dtemp(8)
         Dz(i1+51) = Dtemp(9)
         Dz(i2+51) = Dtemp(9)
      ENDIF
!
   ELSEIF ( Ndof==1 ) THEN
!
! 1 X 1 PARTITION
!
      Z(i1+1) = Temp(1)
      Z(i2+1) = Temp(1)
   ELSE
!
! 3 X 3 PARTITION
! I1 GETS TEMP. I2 GETS THE TRANSPOSE
! IF I1=I2, THEN HALF OF THE ENTRIES WILL BE DUPLICATED
! THAT-S OK SINCE THERE ARE NO ADDITIONS
!
      Z(i1+1) = Temp(1)
      Z(i2+1) = Temp(1)
      Z(i1+2) = Temp(2)
      Z(i2+25) = Temp(2)
      Z(i1+3) = Temp(3)
      Z(i2+49) = Temp(3)
      Z(i1+25) = Temp(4)
      Z(i2+2) = Temp(4)
      Z(i1+26) = Temp(5)
      Z(i2+26) = Temp(5)
      Z(i1+27) = Temp(6)
      Z(i2+50) = Temp(6)
      Z(i1+49) = Temp(7)
      Z(i2+3) = Temp(7)
      Z(i1+50) = Temp(8)
      Z(i2+27) = Temp(8)
      Z(i1+51) = Temp(9)
      Z(i2+51) = Temp(9)
   ENDIF
!
END SUBROUTINE insert
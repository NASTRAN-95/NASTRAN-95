
SUBROUTINE kflud4
   IMPLICIT NONE
   REAL Ecpt(100) , Ki , R(3) , Skip(34) , Sysbuf , Z(3)
   INTEGER Iaxif , Iopt1 , K1ggsw , Necpt(100) , Nj , Nneg , Nptj , Npvt , Out
   LOGICAL Nogo
   CHARACTER*23 Ufm
   COMMON /sma1cl/ Iopt1 , K1ggsw , Npvt
   COMMON /sma1dp/ R , Z , Nneg , Nj , Nptj , Ki
   COMMON /sma1et/ Ecpt
   COMMON /system/ Sysbuf , Out , Nogo , Skip , Iaxif
   COMMON /xmssg / Ufm
   INTEGER i , ip , iret , j
!
!     THIS ROUTINE IS USED FOR THE 4-SIDED FLUID ELEMENT. IT REARRANGES
!     THE DATA AND  CALLS THE KFLUD3 ROUTINE FOR EACH SUBELEMENT.
!
!     THE ECPT DATA FOR THE ELEMENT AND ITS SUBELEMENTS ARE
!
!        FIELD      SYMBOL(FLUID4)      SYMBOL(FLUID3)
!           1            ID                  ID
!           2            SIL1                SIL1
!           3            SIL2                SIL2
!           4            SIL3                SIL3
!           5            SIL4                RHO
!           6            RHO                 BULK
!           7            BULK                N
!           8            N                   CSF
!           9            CSF                 R1
!          10            R1                  Z1
!          11            Z1                  -
!          12            -                   CSF
!          13            CSF                 R2
!          14            R2                  Z2
!          15            Z2                  -
!          16            -                   CSF
!          17            CSF                 R3
!          18            R3                  Z3
!          19            Z3                  -
!          20            -
!          21            CSF
!          22            R4
!          23            Z4
!          24            -
!          25            -
!
   !>>>>EQUIVALENCE (Ecpt(1),Necpt(1))
!
   IF ( Necpt(6)<=0.0 ) RETURN
!
!     TEST FOR INTERIOR ANGLES GREATER THAN 180 DEGREES
!
   Nneg = 0
   ip = 0
   DO i = 1 , 4
      DO j = 1 , 3
         Nj = i + j - 1
         IF ( Nj>4 ) Nj = Nj - 4
         Nptj = 4*(Nj-1) + 10
         R(j) = Ecpt(Nptj)
         Z(j) = Ecpt(Nptj+1)
      ENDDO
      IF ( Npvt==Necpt(i+1) ) ip = ip + 1
      Ki = (R(2)-R(1))*(Z(3)-Z(1)) - (R(3)-R(1))*(Z(2)-Z(1))
      IF ( Ki<0 ) THEN
         Nneg = Nneg + 1
      ELSEIF ( Ki==0 ) THEN
         GOTO 100
      ENDIF
   ENDDO
   IF ( Nneg==1 .OR. Nneg==3 ) GOTO 100
   IF ( ip/=1 ) GOTO 100
   Ecpt(6) = Ecpt(6)*2.0
   DO i = 1 , 24
      Ecpt(i+50) = Ecpt(i)
   ENDDO
   DO i = 5 , 24
      Ecpt(i) = Ecpt(i+1)
   ENDDO
   iret = 1
   DO
!
      IF ( Necpt(2)==Npvt .OR. Necpt(3)==Npvt .OR. Necpt(4)==Npvt ) CALL kflud3
      IF ( iret==1 ) THEN
         Ecpt(4) = Ecpt(55)
         Ecpt(17) = Ecpt(72)
         Ecpt(18) = Ecpt(73)
         iret = 2
      ELSEIF ( iret==2 ) THEN
         Ecpt(13) = Ecpt(68)
         Ecpt(14) = Ecpt(69)
         Ecpt(3) = Ecpt(54)
         iret = 3
      ELSEIF ( iret==3 ) THEN
         Ecpt(9) = Ecpt(64)
         Ecpt(10) = Ecpt(65)
         Ecpt(2) = Ecpt(53)
         iret = 4
      ELSE
         RETURN
      ENDIF
   ENDDO
!
 100  Nj = Necpt(1)
   IF ( Iaxif/=0 ) Nj = Nj/1000
   WRITE (Out,99001) Ufm , Nj
99001 FORMAT (A23,' 5002, INTERIOR ANGLE GREATER THAN OR EQUAL TO 180 ','DEGREES FOR ELEMENT',I12)
   Nogo = .TRUE.
END SUBROUTINE kflud4
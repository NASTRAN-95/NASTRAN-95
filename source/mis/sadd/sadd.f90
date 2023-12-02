!*==sadd.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE sadd(Z,Dz)
   IMPLICIT NONE
   USE C_PACKX
   USE C_SADDX
   USE C_SYSTEM
   USE C_TYPE
   USE C_ZNTPKX
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: Z
   REAL*8 , DIMENSION(1) :: Dz
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: alph , amcb
   REAL*8 , DIMENSION(2) :: da
   REAL*8 , DIMENSION(10) :: dalph
   REAL*8 , DIMENSION(1) :: dmcb
   INTEGER :: end , hop , i , ibuf , j , jj , k , ll , ncol1 , nrow , ntype , num , prec , type
   INTEGER , DIMENSION(2) , SAVE :: name
!
! End of declarations rewritten by SPAG
!
!
!     TO COMPUTE MATRIX SUM WITH MULTIPLIERS
!         ACCEPTS 1 TO 5 MATRIX BLOCKS PASSED ON VIA /SADDX/
!     COMMON BLOCK /SADDX/ NOMAT,LCORE,MCBS(60),MC(7)
!         NOMAT - NUMBER OF MATRICES INPUT
!         LCORE - LENGTH OF Z ARRAY (OPEN CORE)
!         MCBS  - MATRIX CONTROL BLOCKS AND MULTIPLIERS
!                 (12 WORDS/MATRIX)
!
!                 1 - FILE NAME            7 - NOT USED
!                 2 - NUMBER OF COLUMN     8 - TYPE OF MULTIPLIER
!                 3 - NUMBER OF ROW        9 - MULTIPLIER   *  LENGTH
!                 4 - FORM OF MATRIX      10 - MULTIPLIER   *  DEPENDS
!                 5 - TYPE OF MATRIX      11 - MULTIPLIER   *  ON THE
!                 6 - MAXIMUM NUMBER OF   12 - MULTIPLIER   *  TYPE
!                     NON-ZERO ELEMENTS
!
!         MC    - MATRIX CONTROL BLOCK OF THE OUTPUT
!
   !>>>>EQUIVALENCE (Amcb(1),Mcbs(9)) , (alph(1),dalph(1)) , (Da(1),A(1)) , (Dmcb(1),Mcbs(9)) , (Ntype,Mc(5)) , (Nrow,Mc(3))
   DATA name/4HSADD , 4H    /
!
!
   end = (Nomat-1)*12 + 1
   prec = -Nomat*2
   type = -Nomat*2
!
!     DETERMINE PRECISION TO BE USED FOR CALCULATIONS
!
!     NOTE - PRC ARRAY IS DIMENSIONED ONLY TO 2
!            PRC(1) = 1, PRC(2) = 2, AND
!            PRC(3) = NWDS(1) = 1, PRC(4) = NWDS(2) = 2
!            WHERE 1 MEANS S.P., 2 D.P.
!          - RC ARRAY = 1,1,2,2, WHERE 1 MEANS REAL, 2 COMPLEX
!
   DO i = 1 , end , 12
      IF ( Mcbs(i)/=0 ) THEN
         j = Mcbs(i+4)
         prec = prec + Prc(j)
         type = type + Rc(j)
         j = Mcbs(i+7)
         prec = prec + Prc(j)
         type = type + Rc(j)
      ELSE
         prec = prec + 2
         type = type + 2
      ENDIF
   ENDDO
   Typin = 1
   IF ( type>0 ) Typin = 3
   IF ( prec>0 ) Typin = Typin + 1
   num = nrow*Nwds(Typin)
   IF ( Lcore<(Nomat+1)*Sysbuf+num+1 ) CALL mesage(-8,0,name)
!
!     MOVE AND CONVERT MULTIPLIERS
!
   IF ( prec>0 ) THEN
!
!     DOUBLE PRECISION
!
      j = 1
      DO i = 1 , end , 12
         k = Mcbs(i+7)
         IF ( Prc(k)==2 ) THEN
            k = i/2 + 1
            dalph(j) = dmcb(k)
            dalph(j+1) = dmcb(k+1)
         ELSE
            dalph(j) = amcb(i)
            dalph(j+1) = amcb(i+1)
         ENDIF
         j = j + 1
         IF ( type>0 ) j = j + 1
      ENDDO
      IF ( type<=0 ) dalph(j+1) = 0.0D+0
   ELSE
!
!     SINGLE PRECISION
!
      j = 1
      DO i = 1 , end , 12
         k = Mcbs(i+7)
         IF ( Prc(k)==2 ) THEN
            k = i/2 + 1
            alph(j) = dmcb(k)
            alph(j+1) = dmcb(k+1)
         ELSE
            alph(j) = amcb(i)
            alph(j+1) = amcb(i+1)
         ENDIF
         j = j + 1
         IF ( type>0 ) j = j + 1
      ENDDO
      IF ( type<=0 ) alph(j+1) = 0.0
   ENDIF
!
   IF ( Typin==2 ) THEN
      ASSIGN 60 TO hop
   ELSEIF ( Typin==3 ) THEN
      ASSIGN 80 TO hop
   ELSEIF ( Typin==4 ) THEN
      ASSIGN 100 TO hop
   ELSE
      ASSIGN 40 TO hop
   ENDIF
!
!     OPEN AND ASSIGN FILES
!
   ibuf = Lcore
   DO i = 1 , end , 12
      ibuf = ibuf - Sysbuf
      IF ( Mcbs(i)/=0 ) CALL gopen(Mcbs(i),Z(ibuf),0)
   ENDDO
   ibuf = ibuf - Sysbuf
   CALL gopen(Mc,Z(ibuf),1)
!
!     SETUP PACK PARAMETERS
!
   One = 1
   N = nrow
   Typout = ntype
   Incr = 1
   ncol1 = Mc(2)
   Mc(2) = 0
   Mc(6) = 0
   Mc(7) = 0
!
!     ADD MATRICES
!
   DO i = 1 , ncol1
!
!     CLEAR CORE
!
      DO j = 1 , num
         Z(j) = 0.0
      ENDDO
!
      One = N
      N = 1
      DO j = 1 , Nomat
         k = 12*(j-1) + 1
         IF ( Mcbs(k)==0 ) CYCLE
         IF ( Mcbs(k+1)<i ) CYCLE
         CALL intpk(*150,Mcbs(k),0,Typin,0)
!
!     READ IN NON ZERO ELEMENT
!
 20      CALL zntpki
         IF ( Ii>nrow ) GOTO 120
         One = min0(One,Ii)
         N = max0(N,Ii)
         GOTO hop
 40      Z(Ii) = Z(Ii) + alph(j)*A(1)
         GOTO 120
 60      Dz(Ii) = Dz(Ii) + dalph(j)*da(1)
         GOTO 120
 80      Ii = Ii + Ii - 1
         jj = j + j - 1
         Z(Ii) = Z(Ii) + alph(jj)*A(1) - alph(jj+1)*A(2)
         Z(Ii+1) = Z(Ii+1) + alph(jj)*A(2) + alph(jj+1)*A(1)
         GOTO 120
 100     Ii = Ii + Ii - 1
         jj = j + j - 1
         Dz(Ii) = Dz(Ii) + dalph(jj)*da(1) - dalph(jj+1)*da(2)
         Dz(Ii+1) = Dz(Ii+1) + dalph(jj)*da(2) + dalph(jj+1)*da(1)
 120     IF ( Eol==0 ) GOTO 20
 150  ENDDO
!
!     END OF COLUMN
!
      One = min0(One,N)
      ll = (One-1)*Nwds(Typin) + 1
      CALL pack(Z(ll),Mc(1),Mc)
   ENDDO
!
!     DONE - CLOSE FILES AND RETURN
!
   DO i = 1 , end , 12
      IF ( Mcbs(i)/=0 ) CALL close(Mcbs(i),1)
   ENDDO
   CALL close(Mc,1)
END SUBROUTINE sadd

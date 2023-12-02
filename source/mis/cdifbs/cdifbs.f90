!*==cdifbs.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE cdifbs(Dz,Buf)
   IMPLICIT NONE
   USE C_CDCMPX
   USE C_CINVPX
   USE C_NAMES
   USE C_ZNTPKX
!
! Dummy argument declarations rewritten by SPAG
!
   REAL*8 , DIMENSION(1) :: Dz
   REAL , DIMENSION(1) :: Buf
!
! Local variable declarations rewritten by SPAG
!
   REAL*8 :: dtemp
   INTEGER :: i , idiag , in1 , intchn , ioff , j , k , lowtri , nrow , nrow2 , uprtri
   INTEGER , DIMENSION(2) , SAVE :: name
!
! End of declarations rewritten by SPAG
!
!
!     SUBROUTINE TO DO THE FBS PASS TO FIND THE LEFT EIGENVECTOR FOR
!     THE TRANSPOSED MATRIX
!
!     COMMON   /DESCRP/  LENGTH    ,MAJOR
   !>>>>EQUIVALENCE (Scrfil(6),Uprtri) , (Scrfil(8),Lowtri) , (Idum(2),Nrow)
   DATA name/4HCDIF , 4HBS  /
!
   CALL sswtch(12,idiag)
!
!     BEGIN THE FORWARD PASS USING THE UPPER TRIANGLE
!
   ioff = Iof - 1
   CALL gopen(uprtri,Buf,Rdrew)
   nrow2 = nrow + nrow
   DO i = 1 , nrow
      j = i + i
      CALL intpk(*100,uprtri,0,Cdp,0)
      DO
         CALL zntpki
         IF ( Ii<i ) THEN
!
!     SUBTRACT OFF NORMAL TERM
!
            Dz(j-1) = Dz(j-1) - Dz(2*Ii-1)*Da(1) + Dz(2*Ii)*Da(2)
            Dz(j) = Dz(j) - Dz(2*Ii-1)*Da(2) - Dz(2*Ii)*Da(1)
         ELSEIF ( Ii==i ) THEN
!
!     DIVIDE BY THE DIAGONAL
!
            dtemp = (Dz(j-1)*Da(1)+Dz(j)*Da(2))/(Da(1)**2+Da(2)**2)
            Dz(j) = (Dz(j)*Da(1)-Dz(j-1)*Da(2))/(Da(1)**2+Da(2)**2)
            Dz(j-1) = dtemp
         ELSE
!
!     SUBTRACT OFF ACTIVE COLUMN TERMS
!
            k = (i-ioff)*2
            Dz(2*Ii-1) = Dz(2*Ii-1) - Dz(k-1)*Da(1) + Dz(k)*Da(2)
            Dz(2*Ii) = Dz(2*Ii) - Dz(k)*Da(1) - Dz(k-1)*Da(2)
         ENDIF
         IF ( Eol/=0 ) EXIT
      ENDDO
 100  ENDDO
   CALL close(uprtri,Rew)
!
!     BEGIN BACKWARD PASS USING THE LOWER TRIANGLE
!
   CALL gopen(lowtri,Buf,Rdrew)
   CALL skprec(lowtri,nrow)
   DO i = 1 , nrow
      CALL bckrec(lowtri)
      intchn = 0
      CALL intpk(*150,lowtri,0,Cdp,0)
      j = (nrow-i+1)*2
      DO
         CALL zntpki
         IF ( Ii/=nrow-i+1 ) THEN
            Dz(j-1) = Dz(j-1) - Dz(2*Ii-1)*Da(1) + Dz(2*Ii)*Da(2)
            Dz(j) = Dz(j) - Dz(2*Ii-1)*Da(2) - Dz(2*Ii)*Da(1)
         ELSE
            IF ( Ii<j/2 ) GOTO 200
!
!     PERFORM THE INTERCHANGE
!
            intchn = ifix(sngl(Da(1)))*2
            IF ( idiag/=0 ) WRITE (6,99001) i , intchn
99001       FORMAT (5H I = ,I5,10HINTCHNG = ,I5)
         ENDIF
         IF ( Eol/=0 ) THEN
            IF ( intchn>0 ) THEN
               in1 = j + intchn
               dtemp = Dz(j)
               Dz(j) = Dz(in1)
               Dz(in1) = dtemp
               dtemp = Dz(j-1)
               Dz(j-1) = Dz(in1-1)
               Dz(in1-1) = dtemp
            ENDIF
            EXIT
         ENDIF
      ENDDO
 150  CALL bckrec(lowtri)
   ENDDO
   CALL close(lowtri,Rew)
   RETURN
!
 200  CALL mesage(-7,lowtri,name)
END SUBROUTINE cdifbs

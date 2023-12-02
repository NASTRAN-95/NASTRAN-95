!*==cdifbs.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE cdifbs(Dz,Buf)
   IMPLICIT NONE
   USE c_cdcmpx
   USE c_cinvpx
   USE c_names
   USE c_zntpkx
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
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
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
   ioff = iof - 1
   CALL gopen(uprtri,Buf,rdrew)
   nrow2 = nrow + nrow
   DO i = 1 , nrow
      j = i + i
      CALL intpk(*100,uprtri,0,cdp,0)
      DO
         CALL zntpki
         IF ( ii<i ) THEN
!
!     SUBTRACT OFF NORMAL TERM
!
            Dz(j-1) = Dz(j-1) - Dz(2*ii-1)*da(1) + Dz(2*ii)*da(2)
            Dz(j) = Dz(j) - Dz(2*ii-1)*da(2) - Dz(2*ii)*da(1)
         ELSEIF ( ii==i ) THEN
!
!     DIVIDE BY THE DIAGONAL
!
            dtemp = (Dz(j-1)*da(1)+Dz(j)*da(2))/(da(1)**2+da(2)**2)
            Dz(j) = (Dz(j)*da(1)-Dz(j-1)*da(2))/(da(1)**2+da(2)**2)
            Dz(j-1) = dtemp
         ELSE
!
!     SUBTRACT OFF ACTIVE COLUMN TERMS
!
            k = (i-ioff)*2
            Dz(2*ii-1) = Dz(2*ii-1) - Dz(k-1)*da(1) + Dz(k)*da(2)
            Dz(2*ii) = Dz(2*ii) - Dz(k)*da(1) - Dz(k-1)*da(2)
         ENDIF
         IF ( eol/=0 ) EXIT
      ENDDO
 100  ENDDO
   CALL close(uprtri,rew)
!
!     BEGIN BACKWARD PASS USING THE LOWER TRIANGLE
!
   CALL gopen(lowtri,Buf,rdrew)
   CALL skprec(lowtri,nrow)
   DO i = 1 , nrow
      CALL bckrec(lowtri)
      intchn = 0
      CALL intpk(*150,lowtri,0,cdp,0)
      j = (nrow-i+1)*2
      DO
         CALL zntpki
         IF ( ii/=nrow-i+1 ) THEN
            Dz(j-1) = Dz(j-1) - Dz(2*ii-1)*da(1) + Dz(2*ii)*da(2)
            Dz(j) = Dz(j) - Dz(2*ii-1)*da(2) - Dz(2*ii)*da(1)
         ELSE
            IF ( ii<j/2 ) GOTO 200
!
!     PERFORM THE INTERCHANGE
!
            intchn = ifix(sngl(da(1)))*2
            IF ( idiag/=0 ) WRITE (6,99001) i , intchn
99001       FORMAT (5H I = ,I5,10HINTCHNG = ,I5)
         ENDIF
         IF ( eol/=0 ) THEN
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
   CALL close(lowtri,rew)
   RETURN
!
 200  CALL mesage(-7,lowtri,name)
END SUBROUTINE cdifbs


SUBROUTINE cdifbs(Dz,Buf)
   IMPLICIT NONE
   REAL Cdp , Csp , Eofnrw , Rd , Rdp , Rdrew , Rew , Rsp , Scrfil(11) , Wrt , Wrtrew
   DOUBLE PRECISION Da(2)
   INTEGER Eol , Idum(36) , Idumm(20) , Ii , Iof , Lowtri , Norew , Nrow , Uprtri
   COMMON /cdcmpx/ Idumm , Iof
   COMMON /cinvpx/ Idum , Scrfil
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp
   COMMON /zntpkx/ Da , Ii , Eol
   REAL Buf(1)
   DOUBLE PRECISION Dz(1)
   DOUBLE PRECISION dtemp
   INTEGER i , idiag , in1 , intchn , ioff , j , k , name(2) , nrow2
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
   CALL gopen(Uprtri,Buf,Rdrew)
   nrow2 = Nrow + Nrow
   DO i = 1 , Nrow
      j = i + i
      CALL intpk(*100,Uprtri,0,Cdp,0)
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
   CALL close(Uprtri,Rew)
!
!     BEGIN BACKWARD PASS USING THE LOWER TRIANGLE
!
   CALL gopen(Lowtri,Buf,Rdrew)
   CALL skprec(Lowtri,Nrow)
   DO i = 1 , Nrow
      CALL bckrec(Lowtri)
      intchn = 0
      CALL intpk(*200,Lowtri,0,Cdp,0)
      j = (Nrow-i+1)*2
 150  CALL zntpki
      IF ( Ii/=Nrow-i+1 ) THEN
         Dz(j-1) = Dz(j-1) - Dz(2*Ii-1)*Da(1) + Dz(2*Ii)*Da(2)
         Dz(j) = Dz(j) - Dz(2*Ii-1)*Da(2) - Dz(2*Ii)*Da(1)
      ELSE
         IF ( Ii<j/2 ) GOTO 300
!
!     PERFORM THE INTERCHANGE
!
         intchn = ifix(sngl(Da(1)))*2
         IF ( idiag/=0 ) WRITE (6,99001) i , intchn
99001    FORMAT (5H I = ,I5,10HINTCHNG = ,I5)
      ENDIF
      IF ( Eol==0 ) GOTO 150
      IF ( intchn>0 ) THEN
         in1 = j + intchn
         dtemp = Dz(j)
         Dz(j) = Dz(in1)
         Dz(in1) = dtemp
         dtemp = Dz(j-1)
         Dz(j-1) = Dz(in1-1)
         Dz(in1-1) = dtemp
      ENDIF
 200  CALL bckrec(Lowtri)
   ENDDO
   CALL close(Lowtri,Rew)
   RETURN
!
 300  CALL mesage(-7,Lowtri,name)
END SUBROUTINE cdifbs
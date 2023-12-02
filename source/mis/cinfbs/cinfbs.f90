!*==cinfbs.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE cinfbs(Dx,Dy,Iobuf)
   IMPLICIT NONE
   USE C_CINFBX
   USE C_NAMES
   USE C_ZNTPKX
!
! Dummy argument declarations rewritten by SPAG
!
   REAL*8 , DIMENSION(1) :: Dx
   REAL*8 , DIMENSION(1) :: Dy
   INTEGER , DIMENSION(1) :: Iobuf
!
! Local variable declarations rewritten by SPAG
!
   REAL*8 :: dtemp
   INTEGER :: i , ifile , in1 , in2 , ioff , j , k , nrow , nrow2 , typear
   INTEGER , DIMENSION(2) , SAVE :: name
!
! End of declarations rewritten by SPAG
!
!
!     CINVFB DOES THE FORWARD AND BACKWARD PASS FOR COMPLEX INVERSE POWE
!
!     COMMON   /DESCRP/  LENGTH    ,MAJOR
   !>>>>EQUIVALENCE (Ifill(3),Nrow)
   DATA name/4HCINF , 4HBS  /
!
!     TRANSFER THE LOAD VECTOR TO THE SOLUTION VECTOR
!
   typear = Cdp
   nrow2 = nrow + nrow
   DO i = 1 , nrow2
      Dy(i) = Dx(i)
   ENDDO
!
!     BEGIN FORWARD PASS
!
!     CALL GOPEN (IFILL(1),IOBUF,RDREW)
   j = 1
 100  CALL intpk(*400,Ifill(1),0,typear,0)
   DO WHILE ( Eol==0 )
      CALL zntpki
      IF ( j<Ii ) GOTO 300
      IF ( j==Ii ) THEN
!
!     PERFORM THE REQUIRED ROW INTERCHANGE
!
         in1 = (j+ifix(sngl(Da(1))))*2 - 1
         dtemp = Dy(2*j-1)
         Dy(2*j-1) = Dy(in1)
         Dy(in1) = dtemp
         dtemp = Dy(2*j)
         Dy(2*j) = Dy(in1+1)
         Dy(in1+1) = dtemp
         GOTO 200
      ENDIF
   ENDDO
!
   ifile = Ifill(1)
   GOTO 1000
 200  IF ( Eol/=0 ) GOTO 400
   CALL zntpki
 300  Dy(2*Ii-1) = Dy(2*Ii-1) - Dy(2*j-1)*Da(1) + Dy(2*j)*Da(2)
   Dy(2*Ii) = Dy(2*Ii) - Dy(2*j-1)*Da(2) - Dy(2*j)*Da(1)
   GOTO 200
 400  j = j + 1
   IF ( j<nrow ) GOTO 100
   CALL rewind(Ifill(1))
!
!     BEGIN BACKWARD PASS
!
   ioff = Ifilu(7) - 1
   j = nrow
 500  CALL intpk(*900,Ifilu(1),0,typear,0)
   IF ( Eol/=0 ) GOTO 900
 600  CALL zntpki
   i = nrow - Ii + 1
   IF ( i/=j ) GOTO 800
!
!     DIVIDE BY THE DIAGONAL
!
   dtemp = (Dy(2*i-1)*Da(1)+Dy(2*i)*Da(2))/(Da(1)**2+Da(2)**2)
   Dy(2*i) = (Dy(2*i)*Da(1)-Dy(2*i-1)*Da(2))/(Da(1)**2+Da(2)**2)
   Dy(2*i-1) = dtemp
!
!     SUBTRACT OFF REMAINING TERMS
!
 700  IF ( i>j ) GOTO 600
   IF ( Eol/=0 ) THEN
      j = j - 1
      IF ( j>0 ) GOTO 500
      CALL rewind(Ifilu(1))
      RETURN
   ELSE
      CALL zntpki
      i = nrow - Ii + 1
   ENDIF
 800  in1 = i
   in2 = j
   IF ( i>=j ) THEN
      k = in1
      in1 = in2 - ioff
      in2 = k
   ENDIF
   in1 = in1 + in1 - 1
   in2 = in2 + in2 - 1
   Dy(in1) = Dy(in1) - Dy(in2)*Da(1) + Dy(in2+1)*Da(2)
   Dy(in1+1) = Dy(in1+1) - Dy(in2)*Da(2) - Dy(in2+1)*Da(1)
   GOTO 700
 900  ifile = Ifilu(1)
 1000 CALL mesage(-5,ifile,name)
END SUBROUTINE cinfbs

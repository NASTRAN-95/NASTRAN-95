
SUBROUTINE cinfbs(Dx,Dy,Iobuf)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Cdp , Eol , Ifill(7) , Ifilu(7) , Ii , Norew , Nrow
   REAL Csp , Eofnrw , Rd , Rdp , Rdrew , Rew , Rsp , Wrt , Wrtrew
   DOUBLE PRECISION Da(2)
   COMMON /cinfbx/ Ifill , Ifilu
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp
   COMMON /zntpkx/ Da , Ii , Eol
!
! Dummy argument declarations
!
   DOUBLE PRECISION Dx(1) , Dy(1)
   INTEGER Iobuf(1)
!
! Local variable declarations
!
   DOUBLE PRECISION dtemp
   INTEGER i , ifile , in1 , in2 , ioff , j , k , name(2) , nrow2 , typear
!
! End of declarations
!
!
!     CINVFB DOES THE FORWARD AND BACKWARD PASS FOR COMPLEX INVERSE POWE
!
!     COMMON   /DESCRP/  LENGTH    ,MAJOR
   EQUIVALENCE (Ifill(3),Nrow)
   DATA name/4HCINF , 4HBS  /
!
!     TRANSFER THE LOAD VECTOR TO THE SOLUTION VECTOR
!
   typear = Cdp
   nrow2 = Nrow + Nrow
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
   IF ( j<Nrow ) GOTO 100
   CALL rewind(Ifill(1))
!
!     BEGIN BACKWARD PASS
!
   ioff = Ifilu(7) - 1
   j = Nrow
 500  CALL intpk(*900,Ifilu(1),0,typear,0)
   IF ( Eol/=0 ) GOTO 900
 600  CALL zntpki
   i = Nrow - Ii + 1
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
      i = Nrow - Ii + 1
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

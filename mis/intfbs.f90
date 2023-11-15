
SUBROUTINE intfbs(Dx,Dy,Iobuf)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL A(4) , Cdp , Csp , Da , Diag , Eofnrw , Rd , Rdrew , Rect , Rew , Row , Sqr , Sym , Uprtri , Wrt , Wrtrew
   INTEGER Eol , Filel(7) , Fileu(7) , Identy , Idummy(27) , Ii , Iopen , Lowtri , Norew , Nrow , Rdp , Rsp
   COMMON /infbsx/ Filel , Fileu
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp , Sqr , Rect , Diag , Lowtri , Uprtri , &
                 & Sym , Row , Identy
   COMMON /trdxx / Idummy , Iopen
   COMMON /zntpkx/ A , Ii , Eol
!
! Dummy argument declarations
!
   REAL Dx(1) , Dy(1)
   INTEGER Iobuf(1)
!
! Local variable declarations
!
   REAL dtemp
   INTEGER i , in1 , in2 , ioff , j , k , parm(4) , typear
!
! End of declarations
!
!
!     GIVEN THE TRIANGULAR FACTORS FOR A GENERAL MATRIX, INTFBS WILL
!     PERFORM THE FORWARD-BACKWARD SUBSTITUTION NECESSARY TO SOLVE
!     A SYSTEM OF EQUATIONS
!
!     DEFINITION OF INPUT PARAMETERS
!
!     FILEL    =  MATRIX CONTROL BLOCK FOR THE LOWER TRIANGLE L
!     FILEU    =  MATRIX CONTROL BLOCK FOR THE UPPER TRIANGLE U
!     DX       =  THE LOAD VECTOR B
!     DY       =  THE SOLUTION VECTOR X
!     IOBUF    =  THE INPUT BUFFER
!
!     NAMED COMMONS
!
   EQUIVALENCE (A(1),Da) , (Filel(3),Nrow)
   DATA parm(3) , parm(4)/4HINTF , 4HBS  /
!
!
!     TRANSFER THE LOAD VECTOR TO THE SOLUTION VECTOR
!
   DO i = 1 , Nrow
      Dy(i) = Dx(i)
   ENDDO
   typear = Rsp
!
!     OPEN FILE FOR THE LOWER TRIANGLE
!
   parm(2) = Filel(1)
   IF ( Iopen/=-10 ) THEN
      IF ( Iopen==0 ) CALL open(*700,Filel(1),Iobuf,Rdrew)
      CALL fwdrec(*800,Filel(1))
   ENDIF
!
!     BEGIN FORWARD PASS
!
   j = 1
   CALL intpk(*200,Filel(1),0,typear,0)
 100  DO WHILE ( Eol==0 )
      CALL zntpki
      IF ( j<Ii ) THEN
         Dy(Ii) = Dy(Ii) - Dy(j)*Da
      ELSEIF ( j==Ii ) THEN
!
!     PERFORM THE REQUIRED ROW INTERCHANGE
!
         in1 = j + ifix(A(1))
         dtemp = Dy(j)
         Dy(j) = Dy(in1)
         Dy(in1) = dtemp
      ELSE
         CYCLE
      ENDIF
      DO WHILE ( Eol==0 )
         CALL zntpki
         Dy(Ii) = Dy(Ii) - Dy(j)*Da
      ENDDO
      GOTO 200
   ENDDO
   GOTO 900
 200  j = j + 1
   IF ( j<Nrow ) THEN
      CALL intpk(*200,Filel(1),0,typear,0)
      GOTO 100
   ELSE
      CALL rewind(Filel(1))
      IF ( Iopen==0 ) CALL close(Filel(1),Rew)
      IF ( Iopen==-10 ) CALL skprec(Filel,1)
!
!     BEGIN BACKWARD PASS
!
      ioff = Fileu(7) - 1
      parm(2) = Fileu(1)
      IF ( Iopen/=-10 ) THEN
         IF ( Iopen==0 ) CALL open(*700,Fileu(1),Iobuf,Rdrew)
         CALL fwdrec(*800,Fileu(1))
      ENDIF
      j = Nrow
   ENDIF
 300  CALL intpk(*900,Fileu(1),0,typear,0)
   IF ( Eol/=0 ) GOTO 900
 400  CALL zntpki
   i = Nrow - Ii + 1
   IF ( i/=j ) GOTO 600
!
!     DIVIDE BY THE DIAGONAL
!
   Dy(i) = Dy(i)/Da
!
!     SUBTRACT OFF REMAINING TERMS
!
 500  IF ( i>j ) GOTO 400
   IF ( Eol/=0 ) THEN
      j = j - 1
      IF ( j>0 ) GOTO 300
      CALL rewind(Fileu(1))
      IF ( Iopen==0 ) CALL close(Fileu(1),Rew)
      IF ( Iopen==-10 ) CALL skprec(Fileu,1)
      RETURN
   ELSE
      CALL zntpki
      i = Nrow - Ii + 1
   ENDIF
 600  in1 = i
   in2 = j
   IF ( i>=j ) THEN
      k = in1
      in1 = in2 - ioff
      in2 = k
   ENDIF
   Dy(in1) = Dy(in1) - Dy(in2)*Da
   GOTO 500
!
 700  parm(1) = -1
   GOTO 1000
 800  parm(1) = -2
   GOTO 1000
 900  parm(1) = -5
 1000 CALL mesage(parm(1),parm(2),parm(3))
END SUBROUTINE intfbs

!*==intfbs.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE intfbs(Dx,Dy,Iobuf)
   IMPLICIT NONE
   USE c_infbsx
   USE c_names
   USE c_trdxx
   USE c_zntpkx
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: Dx
   REAL , DIMENSION(1) :: Dy
   INTEGER , DIMENSION(1) :: Iobuf
!
! Local variable declarations rewritten by SPAG
!
   REAL :: da , dtemp
   INTEGER :: i , in1 , in2 , ioff , j , k , nrow , typear
   INTEGER , DIMENSION(4) , SAVE :: parm
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
   !>>>>EQUIVALENCE (A(1),Da) , (Filel(3),Nrow)
   DATA parm(3) , parm(4)/4HINTF , 4HBS  /
!
!
!     TRANSFER THE LOAD VECTOR TO THE SOLUTION VECTOR
!
   DO i = 1 , nrow
      Dy(i) = Dx(i)
   ENDDO
   typear = rsp
!
!     OPEN FILE FOR THE LOWER TRIANGLE
!
   parm(2) = filel(1)
   IF ( iopen/=-10 ) THEN
      IF ( iopen==0 ) CALL open(*700,filel(1),Iobuf,rdrew)
      CALL fwdrec(*800,filel(1))
   ENDIF
!
!     BEGIN FORWARD PASS
!
   j = 1
   CALL intpk(*200,filel(1),0,typear,0)
 100  DO WHILE ( eol==0 )
      CALL zntpki
      IF ( j<ii ) THEN
         Dy(ii) = Dy(ii) - Dy(j)*da
      ELSEIF ( j==ii ) THEN
!
!     PERFORM THE REQUIRED ROW INTERCHANGE
!
         in1 = j + ifix(a(1))
         dtemp = Dy(j)
         Dy(j) = Dy(in1)
         Dy(in1) = dtemp
      ELSE
         CYCLE
      ENDIF
      DO WHILE ( eol==0 )
         CALL zntpki
         Dy(ii) = Dy(ii) - Dy(j)*da
      ENDDO
      GOTO 200
   ENDDO
   GOTO 900
 200  j = j + 1
   IF ( j<nrow ) THEN
      CALL intpk(*200,filel(1),0,typear,0)
      GOTO 100
   ELSE
      CALL rewind(filel(1))
      IF ( iopen==0 ) CALL close(filel(1),rew)
      IF ( iopen==-10 ) CALL skprec(filel,1)
!
!     BEGIN BACKWARD PASS
!
      ioff = fileu(7) - 1
      parm(2) = fileu(1)
      IF ( iopen/=-10 ) THEN
         IF ( iopen==0 ) CALL open(*700,fileu(1),Iobuf,rdrew)
         CALL fwdrec(*800,fileu(1))
      ENDIF
      j = nrow
   ENDIF
 300  CALL intpk(*900,fileu(1),0,typear,0)
   IF ( eol/=0 ) GOTO 900
 400  CALL zntpki
   i = nrow - ii + 1
   IF ( i/=j ) GOTO 600
!
!     DIVIDE BY THE DIAGONAL
!
   Dy(i) = Dy(i)/da
!
!     SUBTRACT OFF REMAINING TERMS
!
 500  IF ( i>j ) GOTO 400
   IF ( eol/=0 ) THEN
      j = j - 1
      IF ( j>0 ) GOTO 300
      CALL rewind(fileu(1))
      IF ( iopen==0 ) CALL close(fileu(1),rew)
      IF ( iopen==-10 ) CALL skprec(fileu,1)
      RETURN
   ELSE
      CALL zntpki
      i = nrow - ii + 1
   ENDIF
 600  in1 = i
   in2 = j
   IF ( i>=j ) THEN
      k = in1
      in1 = in2 - ioff
      in2 = k
   ENDIF
   Dy(in1) = Dy(in1) - Dy(in2)*da
   GOTO 500
!
 700  parm(1) = -1
   GOTO 1000
 800  parm(1) = -2
   GOTO 1000
 900  parm(1) = -5
 1000 CALL mesage(parm(1),parm(2),parm(3))
END SUBROUTINE intfbs

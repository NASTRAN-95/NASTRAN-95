
SUBROUTINE cdtfbs(Dx,Dy,Iobuf,Fileu,Nrow)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Cdp , Identy , Incrx , Itypex , Iunpak , Junpak , Lowtri , Norew
   REAL Csp , Diag , Eofnrw , Rd , Rdp , Rdrew , Rect , Rew , Row , Rsp , Sqr , Sym , Uprtri , Wrt , Wrtrew
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp , Sqr , Rect , Diag , Lowtri , Uprtri , &
                 & Sym , Row , Identy
   COMMON /unpakx/ Itypex , Iunpak , Junpak , Incrx
!
! Dummy argument declarations
!
   INTEGER Nrow
   DOUBLE PRECISION Dx(1) , Dy(1)
   INTEGER Fileu(7) , Iobuf(1)
!
! Local variable declarations
!
   DOUBLE PRECISION dtemp
   INTEGER i , ioff , ising , j , jj , jk , ju , k , parm(4) , typear
!
! End of declarations
!
!
!     CDTFBS IS A SPECIAL VERSION OF GFBS USED BY COMPLEX DETERMINATE
!     METHOD
!
!     DEFINITION OF INPUT PARAMETERS
!
!     FILEU = MATRIX CONTROL BLOCK FOR THE UPPER TRIANGLE U
!     DX    = THE LOAD VECTOR B
!     DY    = THE SOLUTION VECTOR X
!     IOBUF = THE INPUT BUFFER
!
   DATA parm(3) , parm(4)/4HDETF , 4HBS  /
!
   Incrx = 1
   Itypex = Cdp
   typear = Cdp
!
!     BEGIN BACKWARD PASS
!
   ioff = Fileu(7) - 1
   parm(2) = Fileu(1)
   CALL open(*100,Fileu,Iobuf,Rd)
   DO i = 1 , Nrow
      Iunpak = 0
      j = Nrow - i + 1
      jj = j + j
      CALL bckrec(Fileu)
      CALL unpack(*200,Fileu,Dy)
      CALL bckrec(Fileu)
      ising = 0
      k = (Junpak-Iunpak+1)*2
      ju = Junpak + Junpak
 50   IF ( Junpak<j ) THEN
         Dx(ju-1) = Dx(ju-1) - Dx(jj-1)*Dy(k-1) + Dx(jj)*Dy(k)
         Dx(ju) = Dx(ju) - Dx(jj)*Dy(k-1) - Dx(jj-1)*Dy(k)
      ELSEIF ( Junpak==j ) THEN
         ising = 1
!
!     DIVIDE BY THE DIAGONAL
!
         dtemp = (Dx(jj)*Dy(k-1)-Dx(jj-1)*Dy(k))/(Dy(k)**2+Dy(k-1)**2)
         Dx(jj-1) = (Dx(jj-1)*Dy(k-1)+Dx(jj)*Dy(k))/(Dy(k)**2+Dy(k-1)**2)
         Dx(jj) = dtemp
      ELSE
         jk = (j-ioff)*2
         Dx(jk-1) = Dx(jk-1) - Dx(ju-1)*Dy(k-1) + Dx(ju)*Dy(k)
         Dx(jk) = Dx(jk) - Dx(ju)*Dy(k-1) - Dx(ju-1)*Dy(k)
      ENDIF
      DO
         k = k - 2
         ju = ju - 2
         Junpak = Junpak - 1
         IF ( k==0 ) THEN
            IF ( ising/=0 ) EXIT
            GOTO 200
         ELSEIF ( Dy(k)/=0.D0 .OR. Dy(k-1)/=0.D0 ) THEN
            GOTO 50
         ENDIF
      ENDDO
   ENDDO
   CALL close(Fileu,Rew)
   RETURN
!
 100  parm(1) = -1
   GOTO 300
 200  parm(1) = -5
 300  CALL mesage(parm(1),parm(2),parm(3))
END SUBROUTINE cdtfbs

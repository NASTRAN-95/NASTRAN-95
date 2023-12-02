!*==cdtfbs.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cdtfbs(Dx,Dy,Iobuf,Fileu,Nrow)
   USE c_names
   USE c_unpakx
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: Dx
   REAL(REAL64) , DIMENSION(1) :: Dy
   INTEGER , DIMENSION(1) :: Iobuf
   INTEGER , DIMENSION(7) :: Fileu
   INTEGER :: Nrow
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: dtemp
   INTEGER :: i , ioff , ising , j , jj , jk , ju , k , typear
   INTEGER , DIMENSION(4) , SAVE :: parm
   EXTERNAL bckrec , close , mesage , open , unpack
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         incrx = 1
         itypex = cdp
         typear = cdp
!
!     BEGIN BACKWARD PASS
!
         ioff = Fileu(7) - 1
         parm(2) = Fileu(1)
         CALL open(*20,Fileu,Iobuf,rd)
         DO i = 1 , Nrow
            iunpak = 0
            j = Nrow - i + 1
            jj = j + j
            CALL bckrec(Fileu)
            CALL unpack(*40,Fileu,Dy)
            CALL bckrec(Fileu)
            ising = 0
            k = (junpak-iunpak+1)*2
            ju = junpak + junpak
            SPAG_Loop_2_2: DO
               IF ( junpak<j ) THEN
                  Dx(ju-1) = Dx(ju-1) - Dx(jj-1)*Dy(k-1) + Dx(jj)*Dy(k)
                  Dx(ju) = Dx(ju) - Dx(jj)*Dy(k-1) - Dx(jj-1)*Dy(k)
               ELSEIF ( junpak==j ) THEN
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
               SPAG_Loop_3_1: DO
                  k = k - 2
                  ju = ju - 2
                  junpak = junpak - 1
                  IF ( k==0 ) THEN
                     IF ( ising/=0 ) EXIT SPAG_Loop_3_1
                     GOTO 40
                  ELSEIF ( Dy(k)/=0.D0 .OR. Dy(k-1)/=0.D0 ) THEN
                     CYCLE SPAG_Loop_2_2
                  ENDIF
               ENDDO SPAG_Loop_3_1
               EXIT SPAG_Loop_2_2
            ENDDO SPAG_Loop_2_2
         ENDDO
         CALL close(Fileu,rew)
         RETURN
!
 20      parm(1) = -1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 40      parm(1) = -5
         spag_nextblock_1 = 2
      CASE (2)
         CALL mesage(parm(1),parm(2),parm(3))
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE cdtfbs

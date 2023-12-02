!*==gpwg1b.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE gpwg1b(Mo,Ogpwg,Wtmass,Ipoint)
USE C_OUTPUT
USE C_SYSTEM
USE C_UNPAKX
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Mo
   INTEGER :: Ogpwg
   REAL :: Wtmass
   INTEGER :: Ipoint
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: delta , epsi
   REAL(REAL64) , DIMENSION(36) :: dz
   INTEGER :: i , ibuf , iflag , j , k , l , m
   REAL(REAL64) , DIMENSION(3,3) :: mr , mt , mtr , s , temp
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL :: r
   INTEGER , DIMENSION(150) :: z
   EXTERNAL close , clstab , gmmatd , gopen , gpwg1c , korsz , mesage , unpack , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     DOUBLE PRECISION VERSION, BY G.CHAN/UNISYS  8/86
!
!     THIS ROUTINE WRITES OGPWG--
!         HEADER
!         MO =  36 D.P.WORDS
!         S  =  9  D.P.WORDS
!         MX,XX,YX,ZX,MY,XY,YY,ZY,MZ,XZ,YZ,ZZ  = 12 D.P.WORDS
!         I  =  9  D.P.WORDS
!         I1P, I2P, I3P = 3 D.P.WORDS
!         Q  =  9  D.P.WORDS
!               78 D.P.WORDS (156 S.P.WORDS) TOTAL
!
   !>>>>EQUIVALENCE (dz(1),z(1),Iz(1))
!
!
   DATA name/4HGPWG , 4H1B  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     ASSIGN BUFFER
!     OPEN OGPWG, PUT ON OFP HEADER
!
         ibuf = korsz(z) - Sysbuf + 1
         CALL gopen(Mo,z(ibuf),0)
!
!     UNPACK MO  + MOVE TO PARTITIONS
!
         It1 = 2
         Incr = 1
         Jj = 6
         Ii = 1
         k = 1
         DO i = 1 , 6
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  CALL unpack(*2,Mo,dz(k))
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
 2                DO l = 1 , 6
                     m = l + k - 1
                     dz(m) = 0.0D0
                  ENDDO
                  spag_nextblock_2 = 2
               CASE (2)
                  k = k + 6
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         CALL close(Mo,1)
         delta = 1.D0/Wtmass
         DO i = 1 , 36
            dz(i) = dz(i)*delta
         ENDDO
!
!     OPEN OGPWG FOR OUTPUT
!
         CALL gopen(Ogpwg,z(ibuf),1)
         DO i = 104 , 150
            z(i) = 0
         ENDDO
         z(101) = 1
         z(102) = 13
         z(103) = Ipoint
         z(110) = 78*2
         CALL write(Ogpwg,z(101),50,0)
         CALL write(Ogpwg,Head,96,1)
!
!     PUT MO  ON OGPWG
!
         CALL write(Ogpwg,z(1),72,0)
!
!     PARTITION MO INTO MT, MTR, AND MR
!     AND CREATE DIAGONAL S MATRIX
!
         mt(1,1) = dz(1)
         mt(1,2) = dz(2)
         mt(1,3) = dz(3)
         mt(2,1) = dz(7)
         mt(2,2) = dz(8)
         mt(2,3) = dz(9)
         mt(3,1) = dz(13)
         mt(3,2) = dz(14)
         mt(3,3) = dz(15)
         mtr(1,1) = dz(4)
         mtr(2,1) = dz(5)
         mtr(3,1) = dz(6)
         mtr(1,2) = dz(10)
         mtr(2,2) = dz(11)
         mtr(3,2) = dz(12)
         mtr(1,3) = dz(16)
         mtr(2,3) = dz(17)
         mtr(3,3) = dz(18)
         mr(1,1) = dz(22)
         mr(1,2) = dz(23)
         mr(1,3) = dz(24)
         mr(2,1) = dz(28)
         mr(2,2) = dz(29)
         mr(2,3) = dz(30)
         mr(3,1) = dz(34)
         mr(3,2) = dz(35)
         mr(3,3) = dz(36)
         s(1,1) = 1.0D0
         s(1,2) = 0.0D0
         s(1,3) = 0.0D0
         s(2,1) = 0.0D0
         s(2,2) = 1.0D0
         s(2,3) = 0.0D0
         s(3,1) = 0.0D0
         s(3,2) = 0.0D0
         s(3,3) = 1.0D0
!
!     COMPUTE  DETERMINATE OF  MT
!
         delta = dsqrt(mt(1,1)**2+mt(2,2)**2+mt(3,3)**2)
         epsi = dsqrt(mt(2,1)**2+mt(3,1)**2+mt(3,2)**2)
         IF ( epsi/=0.0D0 ) THEN
            epsi = epsi/delta
            IF ( delta/=0.0D0 ) THEN
               IF ( epsi<1.0D-6 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
!
!     ROTATE COORDINATES
!
            r = epsi
            CALL mesage(42,r,name)
            DO i = 1 , 3
               DO j = 1 , 3
                  temp(i,j) = mt(i,j)
               ENDDO
            ENDDO
!
!     COMPUTE EIGENVECTORS OF  MT  BY JACOBY  METHOD
!
            CALL gpwg1c(temp,s,dz(1),iflag)
            IF ( iflag>0 ) CALL mesage(-7,0,name)
!
!     ORDER EIGENVECTORS  SUCH THAT
!
!     TRANSFORM  MT
!
            CALL gmmatd(mt,3,3,0,s,3,3,0,temp)
            CALL gmmatd(s,3,3,1,temp,3,3,0,mt)
!
!     TRANSFORM  MTR
!
            CALL gmmatd(mtr,3,3,0,s,3,3,0,temp)
            CALL gmmatd(s,3,3,1,temp,3,3,0,mtr)
!
!     TRANSFORM  MR
!
            CALL gmmatd(mr,3,3,0,s,3,3,0,temp)
            CALL gmmatd(s,3,3,1,temp,3,3,0,mr)
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     OUTPUT S
!
         CALL write(Ogpwg,s,18,0)
!
!     COMPUTE   MX,XX,YX,ZX
!
         dz(1) = mt(1,1)
         dz(2) = 0.0D0
         dz(3) = 0.0D0
         dz(4) = 0.0D0
         IF ( dz(1)/=0.0D0 ) THEN
            dz(2) = mtr(1,1)/dz(1)
            dz(3) = -mtr(3,1)/dz(1)
            dz(4) = mtr(2,1)/dz(1)
         ENDIF
         CALL write(Ogpwg,dz(1),8,0)
         dz(5) = mt(2,2)
         dz(6) = 0.0D0
         dz(7) = 0.0D0
         dz(8) = 0.0D0
         IF ( dz(5)/=0.0D0 ) THEN
            dz(6) = mtr(3,2)/dz(5)
            dz(7) = mtr(2,2)/dz(5)
            dz(8) = -mtr(1,2)/dz(5)
         ENDIF
         CALL write(Ogpwg,dz(5),8,0)
         dz(9) = mt(3,3)
         dz(10) = 0.0D0
         dz(11) = 0.0D0
         dz(12) = 0.0D0
         IF ( dz(9)/=0.0D0 ) THEN
            dz(10) = -mtr(2,3)/dz(9)
            dz(11) = mtr(1,3)/dz(9)
            dz(12) = mtr(3,3)/dz(9)
         ENDIF
         CALL write(Ogpwg,dz(9),8,0)
!
!     COMPUTE INERTIAS
!
         temp(1,1) = mr(1,1) - dz(5)*dz(8)*dz(8) - dz(9)*dz(11)*dz(11)
         temp(2,1) = -mr(1,2) - dz(9)*dz(10)*dz(11)
         temp(1,2) = temp(2,1)
         temp(1,3) = -mr(1,3) - dz(5)*dz(6)*dz(8)
         temp(3,1) = temp(1,3)
         temp(2,2) = mr(2,2) - dz(9)*dz(10)*dz(10) - dz(1)*dz(4)*dz(4)
         temp(2,3) = -mr(2,3) - dz(1)*dz(3)*dz(4)
         temp(3,2) = temp(2,3)
         temp(3,3) = mr(3,3) - dz(1)*dz(3)*dz(3) - dz(5)*dz(6)*dz(6)
         CALL write(Ogpwg,temp,18,0)
         CALL gpwg1c(temp,s,dz(1),iflag)
         IF ( iflag>0 ) CALL mesage(-7,0,name)
!
!     PUT OUT  PRINCIPLE INERTIA-S
!
         CALL write(Ogpwg,dz(1),6,0)
!
!     PUT  OUT  Q
!
         CALL write(Ogpwg,s,18,0)
         CALL clstab(Ogpwg,1)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE gpwg1b

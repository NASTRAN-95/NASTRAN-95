!*==ftube.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ftube
   USE c_condas
   USE c_emgdic
   USE c_emgest
   USE c_emgprm
   USE c_system
   USE c_xmssg
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(7) :: dict
   REAL :: dict5 , id1 , id2 , rhocp , vdot , xa , xb , ya , yb , za , zb
   INTEGER , DIMENSION(1) :: iest
   INTEGER :: ifil , irtn , isze
   REAL(REAL64) , DIMENSION(4) :: k
   REAL(REAL64) :: length
   REAL , DIMENSION(4) :: rk
   EXTERNAL emgout
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     THIS IS THE FLUID TUBE ELEMENT IN HEAT TRANSFER.
!     IT COMPUTES AND OUTPUTS THE CONDUCTIVITY AND/OR CAPACITY MATRICES
!     OF THE ELEMENT.
!
!     - SINGLE AND DOUBLE PRECISION VERSION -
!
!     EST ENTRY FOR -FTUBE- ELEMENT.
!     ==============================
!
!     EST( 1) = ELEMENT ID
!     EST( 2) = SIL-A
!     EST( 3) = SIL-B
!     EST( 4) = HEAT CAPACITY/UNIT VOLUME = RHO C
!     EST( 5) = VOLUME FLOW RATE = VDOT         P
!     EST( 6) = DIAMETER AT A
!     EST( 7) = DIAMETER AT B = DIAMETER AT A IF NOT DEFINED.
!     EST( 8) = CSID-A  NOT USED
!     EST( 9) = XA
!     EST(10) = YA
!     EST(11) = ZA
!     EST(12) = CSID-B  NOT USED
!     EST(13) = XB
!     EST(14) = YB
!     EST(15) = ZB
!     EST(16) = AVG TEMP OF ELEMENT.  NOT USED.
!
!
   !>>>>EQUIVALENCE (Iest(1),Est(1)) , (rk(1),k(1)) , (dict(5),dict5)
!
         IF ( .NOT.heat ) RETURN
         dict(1) = estid
         dict(2) = 1
         dict(3) = 2
         dict(4) = 1
         dict5 = 0.0
         IF ( kmb(1)==0 ) GOTO 20
!
!     CONDUCTIVITY
!
         rhocp = est(4)
         vdot = est(5)
!
!     STORE CONDUCTIVITY BY COLUMNS
!
         k(1) = dble(rhocp*vdot)
!
         k(2) = -k(1)
         k(3) = 0.0D0
         k(4) = 0.0D0
!
!     OUTPUT VIA EMGOUT THE FULL MATRIX IN GLOBAL, UNSYMETRIC
!
         ifil = 1
         isze = 4
         ASSIGN 20 TO irtn
         IF ( iprec==2 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         rk(1) = sngl(k(1))
         rk(2) = sngl(k(2))
         rk(3) = sngl(k(3))
         rk(4) = sngl(k(4))
         spag_nextblock_1 = 3
      CASE (3)
         CALL emgout(rk(1),k(1),isze,1,dict,ifil,iprec)
         GOTO irtn
!
!     CAPACITY MATRIX
!
 20      IF ( kmb(3)/=0 ) THEN
            rhocp = est(4)
            vdot = est(5)
            id1 = est(6)
            IF ( est(7)/=0 ) THEN
               id2 = est(7)
            ELSE
               id2 = id1
            ENDIF
            xa = est(9)
            ya = est(10)
            za = est(11)
            xb = est(13)
            yb = est(14)
            zb = est(15)
            length = dble((xb-xa))**2 + dble((yb-ya))**2 + dble((zb-za))**2
            IF ( length<=0.0D0 ) THEN
               length = dsqrt(length)
               WRITE (ioutpt,99001) uim , iest(1)
99001          FORMAT (A29,' FROM ELEMENT FTUBE -',/5X,'ELEMENT WITH ID =',I9,' HAS A ZERO LENGTH.')
               error = .TRUE.
            ENDIF
!
!     FILL AND OUTPUT CAPACITY MATRIX BY COLUMNS IN GLOBAL, SYMMETRIC.
!
            k(1) = (dble(rhocp*pi*(id1+id2)))**2*length/32.0D0
            k(2) = 0.0D0
            k(3) = 0.0D0
            k(4) = k(1)
            dict(2) = 2
            ifil = 3
            isze = 2
            ASSIGN 99999 TO irtn
            IF ( iprec<1 ) THEN
            ELSEIF ( iprec==1 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ELSE
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
99999 END SUBROUTINE ftube

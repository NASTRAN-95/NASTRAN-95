!*==locpt.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE locpt(N,P,M,S,K,Ks,Eps,Loc)
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: N
   REAL(REAL64) , DIMENSION(3,4) :: P
   INTEGER :: M
   REAL(REAL64) , DIMENSION(3,4) :: S
   INTEGER , DIMENSION(2,1) :: K
   REAL(REAL64) , DIMENSION(3) :: Ks
   REAL(REAL64) , DIMENSION(2) :: Eps
   INTEGER , DIMENSION(1) :: Loc
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: edotp , vdotk , vmag , vpmag
   INTEGER :: i , k1 , k2 , ne , np
   REAL(REAL64) , DIMENSION(3) :: v , vp
   REAL(REAL64) , DIMENSION(3,4) :: ve
   REAL(REAL64) , DIMENSION(4) :: vemag
   EXTERNAL dadotb , daxb , dvmag
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     DETERMINES POSITION OF EACH OF N POINTS (P) RELATIVE TO SURFACE
!        BOUNDED BY M POINTS (S)
!        ALL POINTS IN THE SAME COORDINATE SYSTEM
!     KS IS THE (UNIT) VECTOR NORMAL TO THE SURFACE
!     LOC(I) IS FLAG INDICATING POSITION OF POINT I RELATIVE TO SURFACE:
!        LOC= 1    WHEN POINT WITHIN SURFACE BOUNDRY
!        LOC= 0    WHEN POINT IS ON SURFACE BOUNDRY
!        LOC= -1   WHEN POINT OUTSIDE SURFACE BOUNDRY
!
!     MAXIMUM OF 4 POINTS MAY BE LOCATED RELATIVE
!        TO SURFACE WITH MAXIMUM OF 4 SIDES
!        WHOSE ENDPOINTS ARE IN K(2,M)
!
!
!
!
!     EPS ARRAY FOR SIGNIFICANCE TESTING
!        EPS(1) IS AREA, ANGLE LIMIT
!        EPS(2) IS LENGTH LIMIT
!
!
!
!     SET UP VECTORS ALONG EACH SURFACE EDGE
!
   DO ne = 1 , M
      k1 = K(1,ne)
      k2 = K(2,ne)
!
      DO i = 1 , 2
!     VE IS VECTOR ALONG SURFACE EDGE
         ve(i,ne) = S(i,k2) - S(i,k1)
      ENDDO
      ve(3,ne) = 0.D0
      vemag(ne) = dvmag(ve(1,ne),Eps(2))
   ENDDO
!
!      DETERMINE LOCATION OF POINT RELATIVE TO SURFACE
!
   DO np = 1 , N
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
!        (PRESET POINT FLAG TO INTERIOR CODE)
            Loc(np) = 1
!
            DO ne = 1 , M
               k1 = K(1,ne)
!
               DO i = 1 , 2
!     VP IS VECTOR FROM FIRST END OF EDGE VECTOR TO POINT
                  vp(i) = P(i,np) - S(i,k1)
               ENDDO
               vp(3) = 0.D0
               vpmag = dvmag(vp,Eps(2))
!
!     V= VE CROSS VP
               CALL daxb(ve(1,ne),vp,v)
               vmag = dvmag(v,Eps(1))
!     VDOTK= (VE CROSS VP) DOT K,  K NORMAL TO PLANE OF SURFACE
               vdotk = dadotb(v,Ks)
!     EDOTP IS VE DOT VP
               edotp = dadotb(ve(1,ne),vp)
               IF ( vpmag<=Eps(2) ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( vdotk<=Eps(1) ) THEN
!                                      INSIDE THIS EDGE
!                                                          OUTSIDE
                  IF ( (vdotk<-Eps(1)) .OR. (edotp<=Eps(1)) .OR. (vemag(ne)+Eps(2)<vpmag) ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!                  ON THIS EDGE
            ENDDO
!     POINT IS WITHIN SURFACE BOUNDRY IF NOT OUTSIDE ANY EDGE
!        AND NOT ON SURFACE BOUNDRY
            CYCLE
         CASE (2)
!
!     POINT IS OUTSIDE SURFACE BOUNDRY IF OUTSIDE ANY EDGE
            Loc(np) = -1
            CYCLE
         CASE (3)
!
!
!     POINT IS ON BOUNDRY WHEN ANGLE IS EFFECTIVELY ZERO
!        OR (EFFECTIVELY) COINCIDENT WITH EDGE POINT
            Loc(np) = 0
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
!
   ENDDO
END SUBROUTINE locpt

!*==kompnt.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
FUNCTION kompnt(Ig,Ic,Ideg,Iw,Icc,Jg)
   IMPLICIT NONE
   USE C_BANDS
!
! Function and Dummy argument declarations rewritten by SPAG
!
   INTEGER :: kompnt
   INTEGER , DIMENSION(1) :: Ig
   INTEGER , DIMENSION(1) :: Ic
   INTEGER , DIMENSION(1) :: Ideg
   INTEGER , DIMENSION(1) :: Iw
   INTEGER , DIMENSION(1) :: Icc
   INTEGER , DIMENSION(1) :: Jg
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ia , ii , is , ki , ko , n , nc
   EXTERNAL bunpak
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     THIS FUNCTION HAS AS ITS VALUE THE NUMBER OF COMPONENTS STORED
!     IN THE CONNECTION ARRAY IG.
!     ALSO, IC AND ICC ARE SET UP.
!     IC(I) =COMPONENT INDEX FOR NODE I
!     ICC(I)=THE STARTING POSITION TO BE USED FOR LABELS IN COMPONENT I
!     THUS, ICC(I+1)-ICC(I)= THE NUMBER OF NODES IN COMPONENT I
!
!     INTEGER          BUNPK
!
         DO i = 1 , Nn
            Icc(i) = 0
            Ic(i) = 0
         ENDDO
         nc = 0
         Icc(1) = 1
         spag_nextblock_1 = 2
      CASE (2)
         DO i = 1 , Nn
            IF ( Ic(i)==0 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            kompnt = nc
         ENDDO
         RETURN
      CASE (3)
         nc = nc + 1
         ki = 0
         ko = 1
         Iw(1) = i
         Ic(i) = nc
         IF ( nc>=1 ) THEN
            is = Icc(nc) + 1
            Icc(nc+1) = is
         ENDIF
         DO
            ki = ki + 1
            ii = Iw(ki)
            n = Ideg(ii)
            IF ( n==0 ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            CALL bunpak(Ig,ii,n,Jg)
            DO i = 1 , n
               ia = Jg(i)
               IF ( Ic(ia)==0 ) THEN
                  Ic(ia) = nc
                  ko = ko + 1
                  Iw(ko) = ia
                  is = Icc(nc+1) + 1
                  Icc(nc+1) = is
               ENDIF
            ENDDO
            IF ( ko<=ki ) THEN
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDDO
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END FUNCTION kompnt

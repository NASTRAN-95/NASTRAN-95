!*==amgb1d.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE amgb1d(Ajj,Tsonx,Tamach,Tredf)
   IMPLICIT NONE
   USE C_AMGMN
   USE C_BAMG1L
!
! Dummy argument declarations rewritten by SPAG
!
   COMPLEX , DIMENSION(Nstns,1) :: Ajj
   INTEGER , DIMENSION(1) :: Tsonx
   REAL , DIMENSION(1) :: Tamach
   REAL , DIMENSION(1) :: Tredf
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , nline , nline1 , nline2 , nnline , ns , numm
   EXTERNAL intert
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     THIS ROUTINE INTERPOLATES TRANSONIC AJJ MATRICES
!
!
!
!
!
   numm = 2*Nstns*Nstns
   DO nline = 1 , Nlines
      spag_nextblock_1 = 1
      SPAG_DispatchLoop_1: DO
         SELECT CASE (spag_nextblock_1)
         CASE (1)
            IF ( Tsonx(nline)==0 ) CYCLE
            ns = 0
            IF ( nline==1 ) THEN
!       SEARCH FOR 1ST--2--KNOWN STREAMLINES
               nline1 = 0
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( Tamach(nline)>=1.0 ) THEN
!        SUPERSONIC
               IF ( nline/=Nlines ) THEN
                  ns = 1
                  nline1 = 0
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSE
!       SUBSONIC
               IF ( nline==2 ) nline1 = 1
               IF ( nline==2 ) THEN
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDIF
            spag_nextblock_1 = 2
         CASE (2)
            nline1 = nline - 2
            nline2 = nline - 1
            CALL intert(nline,nline1,nline2,numm,Ajj,Tamach)
            CYCLE
         CASE (3)
            nline2 = 0
            nnline = nline + 1
            SPAG_Loop_2_1: DO i = nnline , Nlines
               IF ( nline2/=0 ) EXIT SPAG_Loop_2_1
               IF ( Tsonx(i)==0 ) THEN
                  IF ( nline1==0 ) nline1 = i
                  IF ( nline1/=i ) nline2 = i
               ENDIF
            ENDDO SPAG_Loop_2_1
            IF ( ns==0 ) THEN
               CALL intert(nline,nline1,nline2,numm,Ajj,Tamach)
            ELSE
               IF ( nline1==0 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( nline2==0 ) THEN
                  nline2 = nline1
                  nline1 = nline - 1
               ENDIF
               CALL intert(nline,nline1,nline2,numm,Ajj,Tamach)
            ENDIF
            EXIT SPAG_DispatchLoop_1
         END SELECT
      ENDDO SPAG_DispatchLoop_1
   ENDDO
END SUBROUTINE amgb1d

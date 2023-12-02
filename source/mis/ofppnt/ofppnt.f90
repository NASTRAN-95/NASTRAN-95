!*==ofppnt.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ofppnt(Out,Nwds,Fmt)
   IMPLICIT NONE
   USE C_MACHIN
   USE C_SYSTEM
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nwds
   INTEGER , DIMENSION(Nwds) :: Out
   CHARACTER(1) , DIMENSION(1200) :: Fmt
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: iosxx , k
   EXTERNAL forwrt
!
! End of declarations rewritten by SPAG
!
!
!WKBD LOGICAL         DEBUG
!WKBR INTEGER         OUT(NWDS), FMT(300)
!WKBI
!WKBD DATA    DEBUG / .FALSE. /
!
!WKBD IF (DEBUG) WRITE (L,10) (FMT(K),K=1,32)
99001 FORMAT (' FMT=',32A4)
!WKBR 5/95     IF ( MACHX.EQ.2 .OR. MACHX.EQ.5  )
   IF ( Machx==2 .OR. Machx==5 .OR. Machx==21 ) WRITE (L,Fmt,IOSTAT=iosxx) (Out(k),k=1,Nwds)
!WKBR 5/95      IF ( MACHX.NE.2 .AND. MACHX.NE.5 )
   IF ( Machx/=2 .AND. Machx/=5 .AND. Machx/=21 ) CALL forwrt(Fmt,Out,Nwds)
END SUBROUTINE ofppnt

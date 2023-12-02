!*==ofppnt.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ofppnt(Out,Nwds,Fmt)
   USE c_machin
   USE c_system
   IMPLICIT NONE
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
   IF ( machx==2 .OR. machx==5 .OR. machx==21 ) WRITE (l,Fmt,IOSTAT=iosxx) (Out(k),k=1,Nwds)
!WKBR 5/95      IF ( MACHX.NE.2 .AND. MACHX.NE.5 )
   IF ( machx/=2 .AND. machx/=5 .AND. machx/=21 ) CALL forwrt(Fmt,Out,Nwds)
END SUBROUTINE ofppnt

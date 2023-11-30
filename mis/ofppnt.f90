
SUBROUTINE ofppnt(Out,Nwds,Fmt)
   IMPLICIT NONE
   INTEGER L , Machx
   REAL Sysbuf
   COMMON /machin/ Machx
   COMMON /system/ Sysbuf , L
   INTEGER Nwds
   CHARACTER*1 Fmt(1200)
   INTEGER Out(Nwds)
   INTEGER iosxx , k
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

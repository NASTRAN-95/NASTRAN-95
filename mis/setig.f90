
SUBROUTINE setig(Kg1,Kg2,Ig,Norig)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dum2(2) , Dum3(3)
   INTEGER Ibuf , Maxdeg , Maxgrd , Mm , Nedge , Nn , Nout
   COMMON /bands / Nn , Mm , Dum2 , Maxgrd , Maxdeg , Dum3 , Nedge
   COMMON /system/ Ibuf , Nout
!
! Dummy argument declarations
!
   INTEGER Kg1 , Kg2
   INTEGER Ig(1) , Norig(1)
!
! Local variable declarations
!
   INTEGER bunpk
   INTEGER is , k , l , loop , m
   REAL sub(2)
!
! End of declarations
!
!
!     THIS ROUTINE IS USED ONLY IN BANDIT MODULE
!
!     THIS ROUTINE SETS IG(KG1,-)=KG2 AND IG(KG2,-)=KG1 IF THIS
!     CONNECTION HAS NOT ALREADY BEEN SET.
!     NEDGE = NUMBER OF UNIQUE EDGES.
!
   DATA sub/4HSETI , 4HG   /
!
   IF ( Kg1/=0 .AND. Kg2/=0 .AND. Kg1/=Kg2 ) THEN
      l = Kg1
      k = Kg2
      DO loop = 1 , 2
         IF ( loop/=1 ) THEN
            l = Kg2
            k = Kg1
         ENDIF
         m = 0
         DO
            m = m + 1
            IF ( m>Maxdeg ) GOTO 100
            is = bunpk(Ig,l,m)
            IF ( is==0 ) THEN
               CALL bpack(Ig,l,m,k)
               Mm = max0(Mm,m)
               IF ( loop==1 ) Nedge = Nedge + 1
               EXIT
            ELSEIF ( is==k ) THEN
               GOTO 99999
            ENDIF
         ENDDO
      ENDDO
   ENDIF
   GOTO 99999
!
 100  WRITE (Nout,99001) Norig(l) , Maxdeg
99001 FORMAT (34H0***  FATAL ERROR - - - GRID POINT,I10,48H  HAS DEGREE EXCEEDING THE NODAL DEGREE LIMIT OF,I8)
   CALL mesage(-8,0,sub)
99999 END SUBROUTINE setig

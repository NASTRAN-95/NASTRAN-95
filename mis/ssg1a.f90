
SUBROUTINE ssg1a(N1,Ilist,Nedt,Ntemp,Ncent,Casecc,Iharm)
   IMPLICIT NONE
   INTEGER Bgpdt , Core(138) , Cstm , Ecpt , Edt , Gptt , Icore(1) , Isil , Itherm , Lc , Loadnn , Lodc , Mass , Mpt , N(3) ,       &
         & Nobld , Nout , Nrowsp , Sil , Slt , System
   REAL Dum53(53) , Old
   COMMON /blank / Nrowsp , Loadnn
   COMMON /loadx / Lc , Slt , Bgpdt , Old , Cstm , Sil , Isil , Ecpt , Mpt , Gptt , Edt , N , Lodc , Mass , Nobld
   COMMON /system/ System , Nout , Dum53 , Itherm
   COMMON /zzzzzz/ Icore
   INTEGER Casecc , Iharm , N1 , Ncent , Nedt , Ntemp
   INTEGER Ilist(1)
   REAL flag
   INTEGER i , icomb(1080) , idefml(1080) , iflag , ifound , ifrst , ione , ip1 , ip2 , islt , ispcn , itempl(1080) , j , k , lc1 , &
         & llist , mpcn , name(2) , name1(2) , nogo
!
!     ROUTINE ANALIZES CASECC AND SLT TO BUILD LISTS OF SELECTED
!     LOADS
!
   EQUIVALENCE (Icore(1),Core(1))
   DATA name/4HSSG1 , 4HA   /
   DATA name1/4HSLT  , 4HSSG1/
!
!
!     INITIALIZE.
!
   Nedt = 0
   Ntemp = 0
   Ncent = 0
   ifound = 0
   N1 = 0
   lc1 = Lc - System
   islt = 0
   CALL open(*200,Slt,Core(lc1+1),0)
   islt = 1
   CALL read(*700,*100,Slt,Ilist(1),-2,0,N1)
   CALL read(*700,*100,Slt,Ilist(1),lc1,1,N1)
!
!     ALLOW FOR 360 LOADS
!
 100  IF ( N1>360 ) THEN
      name(2) = N1
      CALL mesage(-30,137,name)
   ENDIF
   lc1 = lc1 - System
   llist = N1
 200  CALL open(*900,Casecc,Core(lc1+1),0)
   ione = 0
   DO i = 1 , Loadnn
      CALL fwdrec(*900,Casecc)
   ENDDO
   ifrst = 0
   DO
      CALL read(*300,*900,Casecc,Core(1),166,1,flag)
      IF ( ifrst==0 ) THEN
         ifrst = 1
         ispcn = Core(3)
         mpcn = Core(2)
      ENDIF
!
!     TEST FOR SYMMETRY BUCKLING, OR DIFFERENTIAL STIFFNESS
!
      IF ( Core(16)==0 .AND. Core(5)==0 .AND. Core(138)==0 ) THEN
         IF ( Core(2)/=mpcn .OR. Core(3)/=ispcn ) EXIT
         Iharm = Core(136)
         ione = 1
         IF ( Core(6)/=0 ) THEN
!
!     SEE IF EL DEFORM LOAD ALREADY APPLIED
!
            IF ( Nedt/=0 ) THEN
               DO i = 1 , Nedt
                  IF ( idefml(i)==Core(6) ) GOTO 220
               ENDDO
            ENDIF
!
!     ADD TO LIST
!
            Nedt = Nedt + 1
            idefml(Nedt) = Core(6)
         ENDIF
 220     IF ( Core(7)/=0 ) THEN
!
!     SEE IF TEMP LOAD ALREADY APPLIED
!
            IF ( Itherm==0 ) THEN
               IF ( Ntemp/=0 ) THEN
                  DO i = 1 , Ntemp
                     IF ( itempl(i)==Core(7) ) GOTO 240
                  ENDDO
               ENDIF
               Ntemp = Ntemp + 1
               itempl(Ntemp) = Core(7)
            ENDIF
         ENDIF
 240     IF ( Core(4)/=0 ) THEN
            IF ( islt==0 ) CALL mesage(-31,Core(4),name1)
            IF ( N1/=0 ) THEN
               DO i = 1 , N1
                  IF ( Core(4)==iabs(Ilist(i)) ) GOTO 260
               ENDDO
            ENDIF
!
!     MUST LOOK AT LOAD CARDS
!
            ifound = ifound + 1
            icomb(ifound) = Core(4)
         ENDIF
         CYCLE
 260     Ilist(i) = -iabs(Ilist(i))
      ENDIF
   ENDDO
 300  CALL close(Casecc,1)
   IF ( ione==0 ) THEN
      WRITE (Nout,99001)
99001 FORMAT ('0*** MISSING LOAD CARD IN CASE CONTROL')
      CALL mesage(-7,0,name)
      GOTO 1000
   ELSE
      IF ( Ntemp/=0 ) THEN
         DO i = 1 , Ntemp
            j = N1 + i
            Ilist(j) = itempl(i)
         ENDDO
      ENDIF
      IF ( Nedt/=0 ) THEN
         DO i = 1 , Nedt
            j = N1 + Ntemp + i
            Ilist(j) = idefml(i)
         ENDDO
      ENDIF
      IF ( ifound==0 ) GOTO 600
!
!     LOOK AT LOAD CARDS
!
      DO i = 1 , N1
         CALL fwdrec(*700,Slt)
      ENDDO
      i = 1
      nogo = 0
      CALL read(*1000,*400,Slt,Core(1),lc1,1,iflag)
   ENDIF
 400  llist = N1 + Nedt + Ntemp
   IF ( llist==0 ) GOTO 1000
   DO i = 1 , ifound
      j = 1
      DO WHILE ( icomb(i)/=Core(j) )
         j = j + 6
         DO WHILE ( j-1<=iflag )
            IF ( Core(j-1)==-1 ) GOTO 450
            j = j + 2
         ENDDO
         GOTO 550
 450  ENDDO
      j = j + 3
      DO WHILE ( Core(j)/=-1 )
         DO k = 1 , llist
            IF ( Core(j)==iabs(Ilist(k)) ) THEN
               Ilist(k) = -iabs(Ilist(k))
               j = j + 2
               GOTO 500
            ENDIF
         ENDDO
         GOTO 550
 500  ENDDO
      CYCLE
 550  CALL mesage(31,icomb(i),name1)
      nogo = 1
   ENDDO
   IF ( nogo/=0 ) GOTO 1100
 600  IF ( islt/=0 ) CALL close(Slt,1)
   IF ( N1/=0 ) THEN
      DO i = 1 , N1
         IF ( Ilist(i)<0 ) THEN
            Ilist(i) = -Ilist(i)
         ELSEIF ( Ilist(i)/=0 ) THEN
            Ilist(i) = 0
         ENDIF
      ENDDO
   ENDIF
   RETURN
!
!     ERROR MESSAGES.
!
 700  ip1 = Slt
 800  ip2 = -1
   CALL mesage(ip2,ip1,name)
 900  ip1 = Casecc
   GOTO 800
 1000 ip2 = 31
   DO i = 1 , ifound
      ip1 = icomb(i)
      CALL mesage(ip2,ip1,name1)
   ENDDO
 1100 CALL mesage(-61,0,name)
!
END SUBROUTINE ssg1a

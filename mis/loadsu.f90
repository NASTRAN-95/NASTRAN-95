
SUBROUTINE loadsu
   IMPLICIT NONE
   INTEGER Buf2 , Hest , Iout , Ist , Iz(1) , Load , Mcore , Ng1 , Ng2 , Nslt , Ntot , Scr1 , Subcas
   LOGICAL Remfl
   REAL Sysbuf , X1 , X2 , Y1 , Y2 , Z(1) , Z1 , Z2
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /biot  / Ng1 , Ng2 , Ist , Subcas , X1 , Y1 , Z1 , X2 , Y2 , Z2 , Buf2 , Remfl , Mcore , Load , Nslt , Scr1 , Hest , Ntot
   COMMON /system/ Sysbuf , Iout
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Z
   REAL alls , factor , zl(2)
   INTEGER bgpdt , file , i , id , ido , iflag , iload , isimp , isub , isub1 , isub2 , iwords , j , ktype , l(2) , mcb(7) ,        &
         & mwords , n , nam(2) , ncards , nel , nloads , nobld , nrowsp , ns , ns2 , nsimp , nwords(19)
!
!     LOADSU SETS UP LOAD INFOTMATION FOR PROLAT FROM NSLT.
!     Z(IST)IS THE STARTING POINT FOR OPEN CORE,Z(MCORE) IS THE LAST
!     AVAILABLE WORD, NTOT IS THE NUMBER OF WORDS PUT INTO OPEN CORE
!     BY THIS ROUTINE. LOAD IS THE LOAD ID.
!
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (l(1),zl(1))
   DATA nam/4HLOAD , 4HSU  /
   DATA nwords/6 , 6 , 4 , 4 , 6 , 6 , 2 , 5 , 5 , 6 , 6 , 7 , 12 , 10 , 10 , 19 , 38 , 7 , 5/
!
   bgpdt = 103
   mcb(1) = bgpdt
   CALL rdtrl(mcb)
   nrowsp = mcb(2)
   mcb(1) = Hest
   CALL rdtrl(mcb)
   nel = mcb(2)
   nsimp = 0
   file = Nslt
   CALL open(*700,Nslt,Z(Buf2),0)
   CALL read(*800,*100,Nslt,Z(Ist+1),Mcore,0,iwords)
   GOTO 900
 100  nloads = iwords - 2
!
!     CHECK LOAD SELECTION AGAINST SIMPLE LOAD ID-S
!
   IF ( nloads/=0 ) THEN
      DO i = 1 , nloads
         IF ( Iz(Ist+2+i)==Load ) GOTO 200
      ENDDO
!
!     NOT A SIMPLE LOAD-MUST BEA LOAD COMBINATION. SKIP NLOADS RECORDS
!     AND SEARCH FOR PROPER LOAD ID
!
      DO i = 1 , nloads
         CALL fwdrec(*800,Nslt)
      ENDDO
   ENDIF
!
!     READ 2 WORDS AT A TIME -1,-1 SIGNIFIES END OF LOAD CARD
!
   iload = Ist + iwords
   DO
      CALL read(*800,*600,Nslt,l,2,0,iflag)
      IF ( l(1)==Load ) THEN
!
!     MATCH
!
         alls = zl(2)
         DO
            CALL fread(Nslt,l,2,0)
            IF ( l(1)==-1 .AND. l(2)==-1 ) GOTO 300
            nsimp = nsimp + 1
            IF ( iload+2*nsimp>Mcore ) GOTO 900
            isub = 2*nsimp - 1
            Z(iload+isub) = zl(1)
            Iz(iload+isub+1) = l(2)
         ENDDO
      ELSE
         DO
!
!     NO MATCH-SKIP TO -1-S
!
            CALL fread(Nslt,l,2,0)
            IF ( l(1)==-1 .AND. l(2)==-1 ) EXIT
         ENDDO
      ENDIF
   ENDDO
!
!     WE HAVE NSIMP SIMPLE LOADS. FOR ONE LOAD,SET PROPER PARAMETERS
!
 200  nsimp = 1
   alls = 1.
   iload = Ist + iwords
   Z(iload+1) = 1.
   Iz(iload+2) = Load
!
!     FOR EACH SIMPLE LOAD, FIND PROPER LOAD ID AND THEN POSITION TO
!     PROPER LOAD RECORD IN NSLT
!
 300  Ntot = 0
   isimp = iload + 2*nsimp
   DO ns = 1 , nsimp
!
      isub = iload + 2*ns - 1
      factor = Z(isub)
      id = Iz(isub+1)
      ncards = 0
      CALL rewind(Nslt)
      i = 1
      IF ( nloads/=0 ) THEN
         DO i = 1 , nloads
            IF ( id==Iz(Ist+2+i) ) GOTO 350
         ENDDO
         GOTO 500
      ENDIF
!
 350  DO j = 1 , i
         CALL fwdrec(*800,Nslt)
      ENDDO
      DO
!
         CALL read(*800,*400,Nslt,nobld,1,0,iflag)
         CALL fread(Nslt,ido,1,0)
         IF ( isimp+2>Mcore ) GOTO 900
         Iz(isimp+1) = nobld
         Iz(isimp+2) = ido
         isimp = isimp + 2
         Ntot = Ntot + 2
!
!     SKIP NOBLD=-20. IF NOBLD=24(REMFLUX), STORE ONLY NOBLD AND IDO,
!     BUT SKIP REMFLUX INFO ON NSLT
!
         IF ( nobld==-20 ) THEN
!
!     TYPE=-20    SKIP IT
!
            CALL fread(Nslt,Z,-(3*nrowsp),0)
         ELSEIF ( nobld<=19 ) THEN
!
!     NOT A MAGNETICS TYPE OF LOAD. - SKIP IT
!
            WRITE (Iout,99001) Uwm , Load
99001       FORMAT (A25,', IN FUNCTIONAL MODULE PROLATE, LOAD SET',I8,/5X,'CONTAINS A NONMAGNETIC LOAD TYPE. IT WILL BE IGNORED.')
            DO i = 1 , ido
               CALL fread(Nslt,Z,-nwords(nobld),0)
            ENDDO
            EXIT
         ELSE
            ktype = nobld - 19
            IF ( ktype==2 ) THEN
               mwords = 12
            ELSEIF ( ktype==3 ) THEN
               mwords = 48
            ELSEIF ( ktype==4 ) THEN
               mwords = 9
            ELSEIF ( ktype==5 ) THEN
               mwords = 3*nel
               mwords = -mwords
               GOTO 360
            ELSE
               mwords = 3*nrowsp
            ENDIF
!
            IF ( isimp+mwords*ido>Mcore ) GOTO 900
            Ntot = Ntot + mwords*ido
 360        DO j = 1 , ido
!
!     NCARDS TELLS HOW MANY SIMPLE LOAD CARDS HAVE THE PRESENT FACTOR
!     APPLIED TO IT
!
               ncards = ncards + 1
               CALL fread(Nslt,Z(isimp+1),mwords,0)
               IF ( nobld/=24 ) isimp = isimp + mwords
!
!     DONE WITH CARDS OF PRESENT TYPE-GET ANOTHER TYPE
!
            ENDDO
         ENDIF
      ENDDO
!
!     EOR ON NSLT-DONE WITH THIS SIMPLE LOAD-GET ANOTHER SIMPLE LOAD
!
!     SUBSTITUTE IN OPEN CORE NCARDS FOR THE SIMPLE LOAD ID. WE NO
!     LONGER NEED THE ID, BUT WE MUST SAVE NCARDS
!
 400  Iz(isub+1) = ncards
!
   ENDDO
!
!     DONE
!
!     STORE ALL THIS INFO BACK AT Z(IST) AS FOLLOWS
!
!     ALLS,NSIMP,(LOAD FACTOR,NCARDS) FOR EACH SIMPLE LOAD ID,
!     ALL LOAD INFO FOR EACH SIMPLE LOAD STARTING WITH NOBLD AND IDO
!
   Z(Ist+1) = alls
   Iz(Ist+2) = nsimp
   ns2 = 2*nsimp
   DO i = 1 , ns2
      Z(Ist+2+i) = Z(iload+i)
   ENDDO
   isub1 = Ist + ns2 + 2
   isub2 = iload + 2*nsimp
   DO i = 1 , Ntot
      Z(isub1+i) = Z(isub2+i)
   ENDDO
   Ntot = Ntot + 2*nsimp + 2
   CALL close(Nslt,1)
   RETURN
!
 500  Load = id
 600  WRITE (Iout,99002) Ufm , Load
99002 FORMAT (A23,', CANNOT FIND LOAD',I8,' ON NSLT IN BIOTSV')
   CALL mesage(-61,0,0)
!
 700  n = -1
   GOTO 1000
 800  n = -2
   GOTO 1000
 900  n = -8
   file = 0
 1000 CALL mesage(n,file,nam)
END SUBROUTINE loadsu
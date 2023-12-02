!*==loadsu.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE loadsu
   IMPLICIT NONE
   USE C_BIOT
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   REAL :: alls , factor
   INTEGER :: bgpdt , file , i , id , ido , iflag , iload , isimp , isub , isub1 , isub2 , iwords , j , ktype , mwords , n ,        &
            & ncards , nel , nloads , nobld , nrowsp , ns , ns2 , nsimp
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(2) :: l
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: nam
   INTEGER , DIMENSION(19) , SAVE :: nwords
   REAL , DIMENSION(2) :: zl
   EXTERNAL close , fread , fwdrec , mesage , open , rdtrl , read , rewind
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     LOADSU SETS UP LOAD INFOTMATION FOR PROLAT FROM NSLT.
!     Z(IST)IS THE STARTING POINT FOR OPEN CORE,Z(MCORE) IS THE LAST
!     AVAILABLE WORD, NTOT IS THE NUMBER OF WORDS PUT INTO OPEN CORE
!     BY THIS ROUTINE. LOAD IS THE LOAD ID.
!
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (l(1),zl(1))
   DATA nam/4HLOAD , 4HSU  /
   DATA nwords/6 , 6 , 4 , 4 , 6 , 6 , 2 , 5 , 5 , 6 , 6 , 7 , 12 , 10 , 10 , 19 , 38 , 7 , 5/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
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
         CALL open(*60,Nslt,Z(Buf2),0)
         CALL read(*80,*20,Nslt,Z(Ist+1),Mcore,0,iwords)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 20      nloads = iwords - 2
!
!     CHECK LOAD SELECTION AGAINST SIMPLE LOAD ID-S
!
         IF ( nloads/=0 ) THEN
            DO i = 1 , nloads
               IF ( iz(Ist+2+i)==Load ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
!
!     NOT A SIMPLE LOAD-MUST BEA LOAD COMBINATION. SKIP NLOADS RECORDS
!     AND SEARCH FOR PROPER LOAD ID
!
            DO i = 1 , nloads
               CALL fwdrec(*80,Nslt)
            ENDDO
         ENDIF
!
!     READ 2 WORDS AT A TIME -1,-1 SIGNIFIES END OF LOAD CARD
!
         iload = Ist + iwords
         DO
            CALL read(*80,*40,Nslt,l,2,0,iflag)
            IF ( l(1)==Load ) THEN
!
!     MATCH
!
               alls = zl(2)
               DO
                  CALL fread(Nslt,l,2,0)
                  IF ( l(1)==-1 .AND. l(2)==-1 ) THEN
                     spag_nextblock_1 = 3
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  nsimp = nsimp + 1
                  IF ( iload+2*nsimp>Mcore ) THEN
                     spag_nextblock_1 = 5
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  isub = 2*nsimp - 1
                  Z(iload+isub) = zl(1)
                  iz(iload+isub+1) = l(2)
               ENDDO
            ELSE
               SPAG_Loop_2_1: DO
!
!     NO MATCH-SKIP TO -1-S
!
                  CALL fread(Nslt,l,2,0)
                  IF ( l(1)==-1 .AND. l(2)==-1 ) EXIT SPAG_Loop_2_1
               ENDDO SPAG_Loop_2_1
            ENDIF
         ENDDO
         spag_nextblock_1 = 2
      CASE (2)
!
!     WE HAVE NSIMP SIMPLE LOADS. FOR ONE LOAD,SET PROPER PARAMETERS
!
         nsimp = 1
         alls = 1.
         iload = Ist + iwords
         Z(iload+1) = 1.
         iz(iload+2) = Load
         spag_nextblock_1 = 3
      CASE (3)
!
!     FOR EACH SIMPLE LOAD, FIND PROPER LOAD ID AND THEN POSITION TO
!     PROPER LOAD RECORD IN NSLT
!
         Ntot = 0
         isimp = iload + 2*nsimp
         DO ns = 1 , nsimp
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
!
                  isub = iload + 2*ns - 1
                  factor = Z(isub)
                  id = iz(isub+1)
                  ncards = 0
                  CALL rewind(Nslt)
                  i = 1
                  IF ( nloads/=0 ) THEN
                     DO i = 1 , nloads
                        IF ( id==iz(Ist+2+i) ) THEN
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                     ENDDO
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
                  spag_nextblock_2 = 2
               CASE (2)
!
                  DO j = 1 , i
                     CALL fwdrec(*80,Nslt)
                  ENDDO
                  SPAG_Loop_2_2: DO
!
                     CALL read(*80,*24,Nslt,nobld,1,0,iflag)
                     CALL fread(Nslt,ido,1,0)
                     IF ( isimp+2>Mcore ) THEN
                        spag_nextblock_1 = 5
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     iz(isimp+1) = nobld
                     iz(isimp+2) = ido
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
99001                   FORMAT (A25,', IN FUNCTIONAL MODULE PROLATE, LOAD SET',I8,/5X,                                              &
                               &'CONTAINS A NONMAGNETIC LOAD TYPE. IT WILL BE IGNORED.')
                        DO i = 1 , ido
                           CALL fread(Nslt,Z,-nwords(nobld),0)
                        ENDDO
                        EXIT SPAG_Loop_2_2
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
                           GOTO 22
                        ELSE
                           mwords = 3*nrowsp
                        ENDIF
!
                        IF ( isimp+mwords*ido>Mcore ) THEN
                           spag_nextblock_1 = 5
                           CYCLE SPAG_DispatchLoop_1
                        ENDIF
                        Ntot = Ntot + mwords*ido
 22                     DO j = 1 , ido
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
                  ENDDO SPAG_Loop_2_2
!
!     EOR ON NSLT-DONE WITH THIS SIMPLE LOAD-GET ANOTHER SIMPLE LOAD
!
!     SUBSTITUTE IN OPEN CORE NCARDS FOR THE SIMPLE LOAD ID. WE NO
!     LONGER NEED THE ID, BUT WE MUST SAVE NCARDS
!
 24               iz(isub+1) = ncards
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
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
         iz(Ist+2) = nsimp
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
      CASE (4)
!
         Load = id
 40      WRITE (Iout,99002) Ufm , Load
99002    FORMAT (A23,', CANNOT FIND LOAD',I8,' ON NSLT IN BIOTSV')
         CALL mesage(-61,0,0)
!
 60      n = -1
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 80      n = -2
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
      CASE (5)
         n = -8
         file = 0
         spag_nextblock_1 = 6
      CASE (6)
         CALL mesage(n,file,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE loadsu

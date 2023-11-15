
SUBROUTINE ifp1c(I81,Nz)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   LOGICAL Bit64
   INTEGER Blank , Core(1) , Corey(401) , Iben , Icc , Iecho , Ieor , Intp , Intra , Ipage , Is , Istr , Isub , Lencc , Line ,      &
         & Loadnn , Maxlin , Method , Mpcn , Ncpw4 , Nlpp , Nmodes , Nogo , Nset , Nsym , Nwpc , Otpe , Scr1
   REAL Casecc , Corex(1) , Date(3) , Equal , Skip(65) , Spcn , Splots , Stftem , Sysbuf , Tim , Tline , Zzzzbb
   COMMON /ifp1a / Scr1 , Casecc , Is , Nwpc , Ncpw4 , Nmodes , Icc , Nset , Nsym , Zzzzbb , Istr , Isub , Lencc , Iben , Equal ,   &
                 & Ieor
   COMMON /system/ Sysbuf , Otpe , Nogo , Intp , Mpcn , Spcn , Method , Loadnn , Nlpp , Stftem , Ipage , Line , Tline , Maxlin ,    &
                 & Date , Tim , Iecho , Splots , Skip , Intra
   COMMON /xifp1 / Blank , Bit64
   COMMON /zzzzzz/ Corex
!
! Dummy argument declarations
!
   INTEGER I81 , Nz
!
! Local variable declarations
!
   INTEGER exce , i , i81o , iacip , ial1 , ibk , ibk1 , iexcpt , ifwd , ifwd1 , il1 , ilset , iput , ireal , iret , iset , ithru , &
         & jexcpt , nifp1c(2) , thru
   REAL flag
!
! End of declarations
!
!
   EQUIVALENCE (Corex(1),Corey(1)) , (Core(1),Corey(401))
   DATA thru/4HTHRU/ , exce/4HEXCE/
   DATA nifp1c/4H IFP , 4H1C  /
!
   i81o = I81
   Core(I81+2) = Isub
   IF ( Core(I81+3)/=-1 ) THEN
!
!     NO NAME FOR SET
!
      CALL ifp1d(-615)
      GOTO 700
   ELSE
      Core(I81) = Core(I81+4)
      ilset = I81 + 1
      Core(ilset) = 0
!
!     FIND BEGINNING OF SET LIST
!
      I81 = I81 + 5
      IF ( Core(I81)==Ieor ) THEN
!
!     UNEXPECTED END OF RECORD
!
         CALL ifp1d(-623)
         GOTO 700
      ELSE
         ireal = 0
         IF ( Core(I81)>1 ) THEN
!
!     FOULED UP SET
!
            CALL ifp1d(-614)
            GOTO 700
         ELSE
            I81 = I81 + 3
            IF ( Core(I81)==Ieor ) THEN
               CALL ifp1d(-623)
               GOTO 700
            ELSE
               iput = ilset + 2
            ENDIF
         ENDIF
      ENDIF
   ENDIF
 100  ithru = 0
   iexcpt = 0
 200  ASSIGN 100 TO iret
   IF ( Core(I81)<0 ) THEN
      ithru = 0
      iexcpt = 0
   ELSEIF ( Core(I81)==0 ) THEN
      GOTO 400
   ELSE
      GOTO 500
   ENDIF
 300  IF ( iabs(Core(I81))/=1 ) ireal = 1
   Core(iput) = Core(I81+1)
   ibk1 = iabs(Core(I81+1))
   I81 = I81 + 2
   iput = iput + 1
   Core(ilset) = Core(ilset) + 1
   GOTO 200
!
!     CONTINUATION CARD
!
! ... ALLOW ON-LINE READ IF INTRA IS .GT. ZERO, SET BY ONLINS
!
 400  IF ( Intra<=0 ) THEN
      CALL read(*900,*900,Scr1,Core(1),Nwpc,0,flag)
      WRITE (Otpe,99001) Icc , (Core(i),i=1,Nwpc)
99001 FORMAT (11X,I8,6X,20A4)
      Icc = Icc + 1
      Line = Line + 1
      IF ( Line>=Nlpp ) CALL page
   ELSE
      CALL xread(*900,Core(1))
      Icc = Icc + 1
   ENDIF
   I81 = iput
   Nz = Nz - Core(ilset)
   CALL xrcard(Core(I81),Nz,Core(1))
   GOTO iret
!
!     THRU AND EXCEPT
!
 500  IF ( Core(I81)==Ieor ) THEN
!
!     END OF RECORD
!
      I81 = iput
      IF ( Core(ilset)<1 ) THEN
         CALL ifp1d(-614)
         GOTO 700
      ELSEIF ( Core(ilset)/=1 ) THEN
         IF ( ireal/=1 ) THEN
!
!     SORT LIST
!
            iset = Core(ilset)
            CALL ifp1s(Core(ilset+2),Core(I81),Core(ilset))
!
!     CORRECT FOR DELETIONS
!
            I81 = I81 + Core(ilset) - iset
         ENDIF
      ENDIF
      GOTO 800
   ELSE
      IF ( ireal==1 ) CALL ifp1d(-622)
      IF ( Bit64 ) CALL mvbits(Blank,0,32,Core(I81+1),0)
      IF ( Core(I81+1)/=thru ) THEN
!
!     EXCEPT
!
         IF ( Core(I81+1)/=exce ) THEN
            CALL ifp1d(-614)
            GOTO 700
         ELSEIF ( ithru==1 ) THEN
!
!     PROCESS EXCEPT CANDIDATES
!
            I81 = I81 + 3
            IF ( Core(I81)==Ieor ) THEN
               CALL ifp1d(-623)
               GOTO 700
            ELSEIF ( iexcpt==1 ) THEN
!
!     EXCEPT FOLLOWED BY THRU
!
               CALL ifp1d(-616)
               GOTO 700
            ELSE
               iexcpt = 1
               jexcpt = 0
            ENDIF
         ELSE
!
!     EXCEPT WITHOUT THRU
!
            CALL ifp1d(-613)
            GOTO 700
         ENDIF
      ELSEIF ( Core(ilset)==0 ) THEN
         CALL ifp1d(-614)
         GOTO 700
      ELSEIF ( Core(iput-1)<0 ) THEN
         CALL ifp1d(-616)
         GOTO 700
      ELSE
         I81 = I81 + 3
         IF ( Core(I81)==Ieor ) THEN
            CALL ifp1d(-623)
            GOTO 700
         ELSE
            ibk = ibk1
            ifwd = Core(I81+1)
            ifwd1 = ifwd
            IF ( ibk>=ifwd ) THEN
               CALL ifp1d(-614)
               GOTO 700
            ELSE
               ithru = 1
!     TEST FOR DEGENERATE THRU INTERVAL
               IF ( ifwd-ibk/=1 ) Core(I81+1) = -Core(I81+1)
               GOTO 300
            ENDIF
         ENDIF
      ENDIF
   ENDIF
 600  DO
      ASSIGN 600 TO iret
      IF ( Core(I81)<0 ) THEN
         IF ( Core(I81+1)>ifwd1 ) GOTO 100
         IF ( Core(I81+1)<ibk ) THEN
            CALL ifp1d(-614)
            EXIT
         ELSEIF ( Core(I81+1)<=Core(I81-1) .AND. jexcpt==1 .AND. (Core(I81+2)<=0 .OR. Core(I81+2)==Ieor) ) THEN
            CALL ifp1d(-626)
            I81 = I81 + 2
         ELSE
            jexcpt = 1
            IF ( Core(I81+1)==ibk ) THEN
!
!     EXCEPTING BEGINNING OF INTERVAL
!
               ibk = ibk + 1
               Core(iput-2) = ibk
               I81 = I81 + 2
               IF ( ifwd-ibk==1 ) Core(iput-1) = ifwd
               IF ( ibk==ifwd ) THEN
                  iput = iput - 1
                  Core(ilset) = Core(ilset) - 1
                  ibk = 0
                  ifwd = 0
               ENDIF
            ELSEIF ( Core(I81+1)==ifwd ) THEN
!
!     EXCEPT END OF INTERVAL
!
               ifwd = ifwd - 1
               Core(iput-1) = -ifwd
               I81 = I81 + 2
               IF ( ifwd-ibk==1 ) Core(iput-1) = ifwd
               IF ( ibk==ifwd ) THEN
                  iput = iput - 1
                  Core(ilset) = Core(ilset) - 1
               ENDIF
               GOTO 100
            ELSEIF ( Core(I81+1)==ifwd1 ) THEN
!
!     EXCEPT PAST OLD END OF INTERVAL
!
               I81 = I81 + 2
               iput = iput - 1
               Core(ilset) = Core(ilset) - 1
               GOTO 100
            ELSE
               IF ( Core(I81+1)-1==ibk ) THEN
!     EXCEPT ADJACENT TO BOTTOM OF INTERVAL
                  il1 = Core(iput-1)
                  ibk = ibk + 2
                  Core(iput-1) = ibk
                  ial1 = iabs(il1)
                  IF ( ial1-ibk==1 ) il1 = ial1
                  Core(iput) = il1
                  IF ( ibk==ial1 ) THEN
                     ibk = 0
                     ifwd = 0
                     I81 = I81 + 2
                     CYCLE
                  ENDIF
               ELSEIF ( Core(I81+1)+1==ifwd ) THEN
!     EXCEPT ADJACENT TO TOP OF INTERVAL
                  Core(iput) = iabs(Core(iput-1))
                  ifwd = ifwd - 2
                  Core(iput-1) = -ifwd
                  IF ( ifwd-ibk==1 ) Core(iput-1) = ifwd
               ELSE
!     EXCEPT IN MIDDLE OF INTERVAL
                  Core(iput-1) = -Core(I81+1) + 1
                  iacip = iabs(Core(iput-1))
                  IF ( iacip-ibk==1 ) Core(iput-1) = iacip
                  Core(iput) = Core(I81+1) + 1
                  Core(iput+1) = -ifwd
                  IF ( ifwd-Core(iput)==1 ) Core(iput+1) = ifwd
                  ibk = Core(iput)
                  I81 = I81 + 2
                  iput = iput + 2
                  Core(ilset) = Core(ilset) + 2
                  CYCLE
               ENDIF
               iput = iput + 1
               I81 = I81 + 2
               Core(ilset) = Core(ilset) + 1
            ENDIF
         ENDIF
      ELSEIF ( Core(I81)==0 ) THEN
         GOTO 400
      ELSE
         GOTO 500
      ENDIF
   ENDDO
 700  I81 = i81o
   Nset = Nset - 1
 800  RETURN
 900  DO
      CALL mesage(-1,Scr1,nifp1c)
   ENDDO
END SUBROUTINE ifp1c

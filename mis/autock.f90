
SUBROUTINE autock(Iadd)
   IMPLICIT NONE
   REAL Core(1)
   INTEGER Dmpcnt , Ihead , Iseqn , Junk(2) , Loscar , Nnames , Os(5) , Osbot , Oscar(1) , Ospnt , Osprc , Preflg , Prenam(100)
   COMMON /autocm/ Preflg , Nnames , Prenam
   COMMON /autohd/ Ihead
   COMMON /xgpi4 / Junk , Iseqn , Dmpcnt
   COMMON /zzzzzz/ Core
   INTEGER Iadd
   INTEGER casecc(2) , casei(2) , casess(2) , i , ib , iblank , ifin , incr , iop , ist , j , list(100) , mi , n2 , ndb , nlist ,   &
         & nopf , nwd , xchk(2)
   INTEGER lshift , rshift
   EXTERNAL lshift , rshift
!
!     THIS ROUTINE GENERATES A CHKPT OSCAR RECORD WHEN THE PRECHK
!     OPTION IS BEING USED, THE ADDRESS IADD IS THE STARTING
!     LOCATION OF THE OUTPUT FILE NAMES TO BE TESTED
!
   !>>>>EQUIVALENCE (Loscar,Core(1),Os(1)) , (Osprc,Os(2)) , (Osbot,Os(3)) , (Ospnt,Os(4)) , (Oscar(1),Os(5))
   DATA casess/4HCASE , 4HSS  /
   DATA casecc/4HCASE , 4HCC  /
   DATA casei/4HCASE , 4HI   /
   DATA xchk/4HXCHK , 4H    /
   DATA iblank/0/
!
   Ihead = 0
   iop = 0
   IF ( Preflg<=0 ) THEN
!
!     PURGE OR EQUIV DATA BLOCK LIST MUST BE CHECKED
!
      nwd = Oscar(Ospnt)
      mi = rshift(Oscar(Ospnt+2),16)
      ib = Ospnt + 6
      Preflg = iabs(Preflg)
      ndb = Oscar(ib)
      iop = 1
      IF ( mi==9 ) ist = ib + 1
      IF ( mi==10 ) ist = ib + 4
      IF ( mi==9 ) ifin = ist + 2*ndb - 1
      IF ( mi==10 ) ifin = ist + 2*ndb - 3
      nwd = nwd - 6
      incr = 2
      nlist = 0
      IF ( Preflg==1 ) THEN
      ELSEIF ( Preflg==2 ) THEN
         GOTO 200
      ELSEIF ( Preflg==3 ) THEN
         GOTO 300
      ELSE
         GOTO 500
      ENDIF
   ELSE
      Preflg = iabs(Preflg)
      nopf = Oscar(Iadd)
      nwd = 3*nopf
      ist = Iadd + 1
      ifin = ist + nwd - 1
      nlist = 0
      incr = 3
!
      IF ( Preflg==2 ) GOTO 200
      IF ( Preflg==3 ) GOTO 300
   ENDIF
!
!     CHECK OUTPUT FILE AGAINST LIST
!
 100  n2 = 2*Nnames
   DO i = ist , ifin , incr
      DO j = 1 , n2 , 2
         IF ( Prenam(j)/=casess(1) .OR. Prenam(j+1)/=casess(2) ) THEN
            IF ( Prenam(j)/=casecc(1) .OR. Prenam(j+1)/=casecc(2) ) THEN
               IF ( Prenam(j)/=casei(1) .OR. Prenam(j+1)/=casei(2) ) THEN
                  IF ( Prenam(j)==Oscar(i) .AND. Prenam(j+1)==Oscar(i+1) ) THEN
                     nlist = nlist + 1
                     list(2*nlist-1) = Oscar(i)
                     list(2*nlist) = Oscar(i+1)
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDDO
   ENDDO
   IF ( iop==1 ) GOTO 500
   IF ( nlist==0 ) RETURN
   GOTO 600
!
!     PREFLG=ALL OPTION, CHECKPOINT ALL OUTPUT DATA BLOCKS
!
 200  DO i = ist , ifin , incr
      IF ( Oscar(i)/=iblank .OR. Oscar(i+1)/=iblank ) THEN
         IF ( Oscar(i)/=casess(1) .OR. Oscar(i+1)/=casess(2) ) THEN
            IF ( Oscar(i)/=casecc(1) .OR. Oscar(i+1)/=casecc(2) ) THEN
               IF ( Oscar(i)/=casei(1) .OR. Oscar(i+1)/=casei(2) ) THEN
                  nlist = nlist + 1
                  list(2*nlist-1) = Oscar(i)
                  list(2*nlist) = Oscar(i+1)
               ENDIF
            ENDIF
         ENDIF
      ENDIF
   ENDDO
   IF ( iop/=1 ) GOTO 600
   GOTO 500
!
!     CHECK OUTPUT FILES EXCEPT THOSE IN LIST
!
 300  n2 = 2*Nnames
   DO i = ist , ifin , incr
      DO j = 1 , n2 , 2
         IF ( Prenam(j)==Oscar(i) .AND. Prenam(j+1)==Oscar(i+1) ) GOTO 400
      ENDDO
      IF ( Oscar(i)/=iblank .OR. Oscar(i+1)/=iblank ) THEN
         IF ( Oscar(i)/=casess(1) .OR. Oscar(i+1)/=casess(2) ) THEN
            IF ( Oscar(i)/=casecc(1) .OR. Oscar(i+1)/=casecc(2) ) THEN
               IF ( Oscar(i)/=casei(1) .OR. Oscar(i+1)/=casei(2) ) THEN
                  nlist = nlist + 1
                  list(2*nlist-1) = Oscar(i)
                  list(2*nlist) = Oscar(i+1)
               ENDIF
            ENDIF
         ENDIF
      ENDIF
 400  ENDDO
   IF ( iop/=1 ) THEN
      IF ( nlist==0 ) RETURN
      GOTO 600
   ENDIF
 500  nwd = nwd - 2*ndb - 2
   IF ( mi==10 ) nwd = nwd - 1
   IF ( nwd>0 .OR. nlist==0 ) THEN
      IF ( nwd<=0 .AND. nlist==0 ) THEN
         Ihead = 0
         GOTO 99999
      ELSE
         ndb = Oscar(ifin+2)
         IF ( mi==9 ) ist = ifin + 3
         IF ( mi==10 ) ist = ifin + 6
         ifin = ist + 2*ndb - 1
         IF ( mi==10 ) ifin = ifin - 2
         IF ( Preflg==1 ) GOTO 100
         IF ( Preflg==2 ) GOTO 200
         IF ( Preflg==3 ) GOTO 300
      ENDIF
   ENDIF
!
!     UPDATE OSCAR PARAMETERS
!
 600  Ihead = 1
   Osprc = Osbot
   Osbot = Oscar(Osbot) + Osbot
   Ospnt = Osbot
   Iseqn = Oscar(Osprc+1) + 1
!
!     LOAD HEADER
!
   Oscar(Ospnt) = 6
   Oscar(Ospnt+1) = Iseqn
   Oscar(Ospnt+2) = 4 + lshift(3,16)
   Oscar(Ospnt+3) = xchk(1)
   Oscar(Ospnt+4) = xchk(2)
   Oscar(Ospnt+5) = Dmpcnt
   IF ( iop==1 ) Oscar(Ospnt+5) = Oscar(Ospnt+5) - 1
   Oscar(Ospnt+6) = nlist
   CALL xlnkhd
   IF ( nlist/=0 ) THEN
!
!     LOAD CHKPNT INFORMATION
!
      nlist = 2*nlist
      DO i = 1 , nlist , 2
         Oscar(Ospnt+6+i) = list(i)
         Oscar(Ospnt+7+i) = list(i+1)
      ENDDO
   ENDIF
   Oscar(Ospnt) = Oscar(Ospnt) + nlist + 1
   Ihead = 0
99999 RETURN
END SUBROUTINE autock
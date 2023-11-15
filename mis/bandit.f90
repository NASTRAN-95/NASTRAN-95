
SUBROUTINE bandit
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Dum2w(2) , Dum3g(3) , Dum4w(4)
   INTEGER Geom1 , Geom2 , Geom4 , Hicore , I77 , Ib , Ibuf , Ibuf1 , Icrit , Ifl , Ih , Inp , Iop , Ipass , Irept , Is(97) , Kdim ,&
         & Kmod , Knew , Kore , Korig , Mach , Machin , Mask , Maxdeg , Maxgrd , Method , Mindeg , Mm , Nbitin , Nbpw , Ncm ,       &
         & Nedge , Nel , Neq , Neqr , Ngpts(2) , Ngrid , Nn , Nodep , Nogo , Nompc , Nopch , Norun , Nout , Nw , Nzero , Rd ,       &
         & Rdrew , Rew , Scr1 , Wrt , Wrtrew , Z(1)
   COMMON /banda / Ibuf1 , Nompc , Nodep , Nopch , Norun , Method , Icrit , Ngpts
   COMMON /bandb / Nbitin , Kore , Ifl , Ngrid , Ipass , Nw , Kdim , Nbpw , Irept
   COMMON /bandd / Korig , Knew , Iop , Inp , Ncm , Nzero , Nel , Neq , Neqr
   COMMON /bandg / Dum3g
   COMMON /bands / Nn , Mm , Ih , Ib , Maxgrd , Maxdeg , Kmod , Mach , Mindeg , Nedge , Mask
   COMMON /bandw / Dum4w , I77 , Dum2w
   COMMON /geomx / Geom1 , Geom2 , Geom4 , Scr1
   COMMON /machin/ Machin
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew
   COMMON /system/ Ibuf , Nout , Nogo , Is
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER end , ii3 , iquit , k1 , k2 , k3 , k4 , k5 , k6 , k7 , k8 , k9 , kdim4 , sub(3)
   INTEGER korsz
!
! End of declarations
!
!
!     BANDIT - A COMPUTER PROGRAM TO RE-SEQUENCE MATRIX BY BANDWIDTH,
!              PROFILE, AND WAVEFRONT METHODS FOR NASTRAN.
!
!     THIS PROGRAM GENERATES THE RE-SEQUENCE CARDS, SEQGP (AFTER GEOM1,
!     GEOM2, AND GEOM4 DATA BLOCKS ARE ASSEMBLED), AND ADD THESE CARDS
!     TO THE END OF GEOM1 FILE.
!
!     HOWEVER, IF THE ORIGINAL NASTRAN INPUT DECK CONTAINS ONE OR MORE
!     SEQGP CARD, BANDIT WILL BE AUTOMATICALLY SKIPPED.
!
!     ******************************************************************
!
!     ACKNOWLEDGEMENT:
!
!     THE ORIGINAL BANDIT PROGRAM (VERSION 9, DEC. 1978, DISTRIBUTED BY
!     COSMIC  NO. DOD-0034) WAS WRITTEN BY G. C. EVERTINE OF NAVAL SHIP
!     RESEARCH AND DEVELOPMENT CENTER (NSRDC), BETHESDA, MD.
!
!     THE FOLLOWING SUBROUTINES WERE WRITTEN BY E. CUTHILL AND J. MCKEE
!     OF NSRDC
!     - CTHMCK,DEGREE,DIAM,IDIST,KOMPNT,MAXDGR,MINDEG,RELABL
!
!     THE FOLLOWING SUBROUTINES WERE WRITTEN BY N. GIBBS, W. POOLE,
!     P. STOCKMEYER, AND H. CRANE OF THE COLLEGE OF WILLIAM AND MARY
!     - DGREE,FNDIAM,GIBSTK,NUMBER,PIKLVL,RSETUP,SORTDG,SORT2,TREE.
!     (THESE ROUTINES AND CTHMCK WERE MODIFIED BY G. C. EVERSTINE.)
!
!     ******************************************************************
!
!     ONLY HALF OF THE ORIGINAL BANDIT PROGRAM WAS ADOPTED IN THIS
!     NASTRAN VERSION BY G. C. CHAN OF SPERRY, HUNTSVILLE, AL., 1982
!
!     THE ORIGINAL BANDIT ROUTINES WERE UPDATED TO MEET NASTRAN
!     PROGRAMMING STYLE AND STANDARD.
!     NASTRAN GINO FILES AND GINO I/O ARE USED INSTEAD OF FORTRAN FILES
!     AND FORTRAN READ/WRITE
!     THE INTEGER PACK AND UNPACK ROUTINES, BPACK AND BUNPK, WERE RE-
!     WRITTEN TO ALLOW COMMON USAGE FOR IBM, CDC, UNIVAC AND VAX MACH.
!
!     ROUTINES BANDIT, SCHEME, BREAD, BGRID, BSEQGP, AND TIGER WERE
!     COMPLETELY RE-WRITTEN.
!     (SCHEME WAS FORMALLY CALLED NASNUM, AND CTHMCK WAS SCHEME)
!
!     ******************************************************************
!
!     THIS NASTRAN VERSION DOES NOT USE $-OPTION CARDS AS IN THE CASE OF
!     ORIGINAL BANDIT PROGRAM.
!
!     THE FOLLOWING 'OPTIONS' ARE PRE-SELECTED -
!
!        $ADD        (NOT USE)            $INSERT     (NOT USE)
!        $APPEND     (NOT USE)            $METHOD     (GPS    )
!        $CONFIG     (NOT USE)            $MPC        (NO     )
!        $CRITERION  (RMS    )            $NASTRAN    (NOT USE)
!        $DEGREE     (NOT USE)            $PLUS       (NOT USE)
!        $DIMENSION  (NOT USE)            $PRINT      (MIN    )
!        $ELEMENTS   (NOT USE)            $PUNCH      (NONE   )
!        $FRONTAL    (NOT USE)            $SEQUENCE   (YES    )
!        $GRID       (NOT USE)            $SPRING     (NO     )
!        $HICORE     (NOT USE)            $TABLE      (NO     )
!        $IGNORE     (NOT USE)            $START      (NOT USE)
!
!     ******************************************************************
!
!
!
   EQUIVALENCE (Hicore,Is(28))
   DATA sub/4HBAND , 4HIT   , 4HBEGN/
   DATA end , iquit/4HEND  , 4HQUIT/
!
!     INITIALIZE PROGRAM PARAMETERS
!
!     NOMPC =  0, MPC'S AND RIGID ELEM. ARE NOT USED IN BANDIT COMPUTATI
!           = +1, ONLY RIGID ELEMENTS ARE USED IN BANDIT RESEQUENCING
!           = +2, BOTH MPC'S  AND RIGID ELEM. ARE USED IN BANDIT
!           = +3, ONLY MPC'S, NOT RIGID ELEM. ARE USED IN BANDIT
!     NODEP = +1, MPC DEPENDENT PTS. ARE TO BE REMOVED FROM COMPUTATION
!           = -1, MPC DEPENDENT PTS. ARE NOT TO BE REMOVED.
!                 (NOTE - NODEP DICTATES ALSO THE DEPENDENT GRIDS OF
!                         THE RIGID ELEMENTS)
!     NOPCH = +1, PUNCH OUT SEQGP CARDS
!           = -1, NO SEQGP CARDS PUNCHED
!     NORUN = +1, BANDIT WILL RUN EVEN SEQGP CARDS ARE PRESENT
!           = -1, BANDIT IS SKIPPED IF ONE OR MORE SEQGP CARD IS
!                 PRESENT IN  THE INPUT DECK
!     METHOD= -1, CM METHOD ONLY
!           =  0, BOTHE CM AND GPS METHODS ARE USED
!           = +1, USE GPS METHOD ONLY
!     ICRIT =     RE-SEQUENCING CRITERION
!           =  1, RMS WAVEFRONT
!           =  2, BANDWIDTH
!           =  3, PROFILE
!           =  4, MAX WAVEFRONT
!
   Nzero = 0
   I77 = 77
   Nompc = 0
   Nodep = -1
   Nopch = -1
   Norun = -1
   Method = +1
   Kdim = 1
   Icrit = 1
   Irept = 0
!
!     THE ABOVE DEFAULT VALUES CAN BE RESET BY THE NASTRAN CARD.
!     (SEE SUBROUTINE NASCAR BANDIT FLAG FOR MORE DETAILS)
!     ******************************************************************
!
   CALL conmsg(sub,3,0)
   Nbpw = Is(37)
   Mach = Machin
   Kore = korsz(Z(1))
   Ibuf1 = Kore - Ibuf - 2
   Kore = Ibuf1 - 1
   DO
!
!     CALL BGRID TO GET THE NO. OF GRID POINTS IN THE PROBLEM, SET
!     THE INTEGER PACKING CONSTANT, NW, AND COMPUTE MAXGRD AND MAXDEG.
!     BANDIT QUITS IF PROBLEM IS TOO SMALL TO BE WORTHWHILE.
!
      Irept = Irept + 1
      CALL bgrid
      IF ( Ngrid<15 ) THEN
         sub(3) = iquit
         CALL conmsg(sub,3,0)
         EXIT
      ELSE
         kdim4 = Kdim*4
         ii3 = 2*Maxgrd
!
!     PARTITION OPEN CORE FOR SCHEME COMPUTATION.
!
         k2 = 1 + kdim4
         k3 = k2 + 2*ii3 + 2
         IF ( Method<=0 .AND. Maxdeg>Maxgrd ) k3 = k3 + Maxdeg - Maxgrd
         k4 = k3 + Maxgrd + 1
         k5 = k4 + Maxgrd
         k6 = k5 + Maxgrd + 1
         k7 = k6 + Maxgrd
         k8 = k7 + Maxdeg
         k1 = k8 + Maxdeg + Nw
         k9 = k1 + Maxgrd*Maxdeg/Nw
         IF ( k9>Kore ) CALL mesage(-8,k9-Kore,sub)
!
!     READ BULK DATA, SET UP CONNECTION TABLE, AND RESEQUENCE NODES.
!
         CALL scheme(Z(k1),Z(k2),ii3,Z(k3),Z(k4),Z(k5),Z(k6),Z(k7),Z(k8),Z)
         IF ( Ngrid==-1 ) CALL sptchk
         IF ( Irept/=2 ) THEN
            IF ( Ngrid<0 ) THEN
!
!     NO BANDIT RUN.
!
               Nogo = 1
            ELSEIF ( Ngrid/=0 ) THEN
!
!     JOB DONE.
!
               sub(3) = end
               CALL conmsg(sub,3,0)
               EXIT
            ENDIF
            sub(3) = iquit
            CALL conmsg(sub,3,0)
            EXIT
         ENDIF
      ENDIF
   ENDDO
END SUBROUTINE bandit

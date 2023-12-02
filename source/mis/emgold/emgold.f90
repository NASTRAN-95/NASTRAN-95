!*==emgold.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE emgold
   USE c_emg1bx
   USE c_emgdic
   USE c_emgest
   USE c_emgprm
   USE c_gpta1
   USE c_hydroe
   USE c_iemgod
   USE c_machin
   USE c_sma1bk
   USE c_sma1cl
   USE c_sma1dp
   USE c_sma1et
   USE c_sma1ht
   USE c_sma1io
   USE c_sma2bk
   USE c_sma2cl
   USE c_sma2dp
   USE c_sma2et
   USE c_sma2ht
   USE c_system
   USE c_xmssg
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(15) :: bdict , kdict , mdict
   INTEGER :: estwds , i , i1 , i2 , icount , if4gg , ifkgg , index , isil , izero , j , jltype , k , ktemp , l , local , mtemp ,   &
            & nbpw , outpt , qp
   EXTERNAL emg1b , hring , kcone2 , kconed , kconeq , kcones , kelbow , kflud2 , kflud3 , kflud4 , kqdmem , kqdplt , kslot ,       &
          & ksolid , ktetra , ktrapr , ktriqd , ktrirg , ktrplt , masstq , mcone , melbow , mflud2 , mflud3 , mflud4 , mfree ,      &
          & mqdplt , mring , mslot , msolid , mtrapr , mtriqd , mtrirg , mtrplt , page2
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     THIS IS A DRIVING ROUTINE OF THE -EMG- MODULE WHICH ALLOWS PIVOT-
!     POINT-LOGIC ELEMENT SUBROUTINES TO BE USED IN CONJUNCTION WITH THE
!     NON-PIVOT-POINT PROCESS.
!
   !>>>>EQUIVALENCE (Ksystm(2),Outpt) , (Ksystm(40),Nbpw) , (Smaio(11),Ifkgg) , (Smaio(13),If4gg)
!
         ktemp = knogo
         mtemp = mnogo
         qp = mod(lqro/100,10)
         jltype = 2*(eltype-1) + precis
         kheat = heat
         lheat = heat
         izero = incr*(eltype-1)
         IF ( eltype/=ltypes ) THEN
            CALL page2(3)
            index = izero
            IF ( heat ) THEN
               IF ( eltype==62 .OR. eltype==63 ) index = 15*incr
            ENDIF
            IF ( l38==1 ) WRITE (outpt,99001) sim , elem(index+1) , elem(index+2)
99001       FORMAT (A31,' 3107',/5X,'EMGOLD CALLED BY EMGPRO TO PROCESS ',2A4,' ELEMENTS.')
            ltypes = eltype
         ENDIF
!
         nsils = elem(izero+10)
         isil = elem(izero+13)
         IF ( elem(izero+9)/=0 ) isil = isil - 1
         estwds = elem(izero+12)
         i1 = isil
         i2 = isil + nsils - 1
         l = nsils
!
!     MOVE SILS TO SEPARATE ARRAY
!
!     SORT ARRAY OF SILS
!
!     POSITION VECTOR
!
         DO i = i1 , i2
            IF ( estbuf(i)==0 ) THEN
               k = l
               l = l - 1
            ELSE
               k = 1
               DO j = i1 , i2
                  IF ( estbuf(j)<estbuf(i) ) THEN
                  ELSEIF ( estbuf(j)==estbuf(i) ) THEN
                     IF ( j>=i ) CYCLE
                  ELSE
                     CYCLE
                  ENDIF
                  IF ( estbuf(j)/=0 ) k = k + 1
               ENDDO
            ENDIF
            posvec(k) = i - i1 + 1
            sil(k) = estbuf(i)
         ENDDO
!
!     ELIMINATE DUP SILS THAT MAY OCCUR,E.G. CHBDY WITH AMB.PTS.
!
         k = 1
         icount = 1
         DO i = 2 , nsils
            SPAG_Loop_2_1: DO
               k = k + 1
               IF ( k>nsils ) THEN
                  sil(i) = 0
                  posvec(i) = 0
                  EXIT SPAG_Loop_2_1
               ELSEIF ( sil(k)/=sil(k-1) ) THEN
                  sil(i) = sil(k)
                  IF ( sil(k)/=0 ) icount = icount + 1
                  posvec(i) = posvec(k)
                  EXIT SPAG_Loop_2_1
               ENDIF
            ENDDO SPAG_Loop_2_1
         ENDDO
         nsils = icount
!
!     SETUP VALUES AND DICTIONARY IN /EMG1BX/ FOR EMG1B USE
!
         dict(1) = estid
         dict(2) = 1
!
!     PSUEDO SMA1-SMA2 FILE NUMBERS
!
         ifkgg = 201
         if4gg = 202
!
!     DICT(4) WILL BE RESET TO EITHER 1 OR 63 BY EMG1B
!     BASED ON INCOMING DATA TO EMG1B
!
         DO i = 5 , 15
            dict(i) = 0
         ENDDO
!
!     CALL ELEMENT FOR EACH PIVOT ROW
!
         last = .FALSE.
         knogo = 0
         mnogo = 0
         DO i = 1 , nsils
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  IF ( i==nsils ) last = .TRUE.
!
!     STIFFNESS MATRIX
!
                  IF ( flags(1)/=0 ) THEN
!
!     RESTORE K-DICTIONARY IF NECESSARY
!
                     IF ( i/=1 ) THEN
                        DO l = 1 , 15
                           dict(l) = kdict(l)
                        ENDDO
                     ENDIF
                     filtyp = 1
!
!     IOPT4 IS TURNED ON SO THAT DAMPING CONSTANTS ARE SENT TO EMG1B
!     IN ALL AVAILABLE CASES BY ELEMENT ROUTINES.  MATRIX DATA WILL BE
!     IGNORED BY EMG1B ON EMG1B CALLS SENDING DAMPING CONSTANTS.
!     DAMPING CONSTANTS WILL BE PLACED IN 5TH WORD OF ELEMENT DICTIONARY
!     ENTRY.
!
                     iopt4 = 1
                     k4ggsw = 0
                     knpvt = sil(i)
!
!     FULL 6X6 MATRIX FORCED FOR STIFFNESS WITH OLD ELEMENT ROUTINES
!
                     dict(2) = 1
                     IF ( sil(i)/=0 ) THEN
                        DO l = 1 , estwds
                           kecpt(l) = estbuf(l)
                        ENDDO
                        hydro = .FALSE.
                        IF ( eltype>=76 .AND. eltype<=79 ) hydro = .TRUE.
!
!     CALL THE PROPER ELEMENT STIFFNESS ROUTINE
!
                        local = jltype - 100
                        IF ( local<=0 ) THEN
!
!     PAIRED -GO TO- ENTRIES PER ELEMENT SINGLE/DOUBLE PRECISION
!
!             1 CROD      2 C.....    3 CTUBE     4 CSHEAR    5 CTWIST
!
!             6 CTRIA1    7 CTRBSC    8 CTRPLT    9 CTRMEM   10 CONROD
!
!            11 ELAS1    12 ELAS2    13 ELAS3    14 ELAS4    15 CQDPLT
!
!            16 CQDMEM   17 CTRIA2   18 CQUAD2   19 CQUAD1   20 CDAMP1
!
!            21 CDAMP2   22 CDAMP3   23 CDAMP4   24 CVISC    25 CMASS1
!
!            26 CMASS2   27 CMASS3   28 CMASS4   29 CONM1    30 CONM2
!
!            31 PLOTEL   32 C.....   33 C.....   34 CBAR     35 CCONEAX
!
!            36 CTRIARG  37 CTRAPRG  38 CTORDRG  39 CTETRA   40 CWEDGE
!
!            41 CHEXA1   42 CHEXA2   43 CFLUID2  44 CFLUID3  45 CFLUID4
!
!            46 CFLMASS  47 CAXIF2   48 CAXIF3   49 CAXIF4   50 CSLOT3
!
                           IF ( jltype==1 .OR. jltype==2 .OR. jltype==3 .OR. jltype==4 .OR. jltype==5 .OR. jltype==6 .OR.           &
                              & jltype==7 .OR. jltype==8 .OR. jltype==9 .OR. jltype==10 .OR. jltype==13 .OR. jltype==14 .OR.        &
                              & jltype==17 .OR. jltype==18 .OR. jltype==19 .OR. jltype==20 .OR. jltype==21 .OR. jltype==22 .OR.     &
                              & jltype==23 .OR. jltype==24 .OR. jltype==25 .OR. jltype==26 .OR. jltype==27 .OR. jltype==28 .OR.     &
                              & jltype==39 .OR. jltype==40 .OR. jltype==41 .OR. jltype==42 .OR. jltype==43 .OR. jltype==44 .OR.     &
                              & jltype==45 .OR. jltype==46 .OR. jltype==47 .OR. jltype==48 .OR. jltype==49 .OR. jltype==50 .OR.     &
                              & jltype==51 .OR. jltype==52 .OR. jltype==53 .OR. jltype==54 .OR. jltype==55 .OR. jltype==56 .OR.     &
                              & jltype==57 .OR. jltype==58 .OR. jltype==59 .OR. jltype==60 .OR. jltype==63 .OR. jltype==64 .OR.     &
                              & jltype==65 .OR. jltype==66 .OR. jltype==67 .OR. jltype==68 .OR. jltype==75 .OR. jltype==76 ) THEN
                              spag_nextblock_1 = 2
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           IF ( jltype==11 .OR. jltype==12 ) THEN
!
!
!     IN -HEAT- FORMULATIONS SOME ELEMENTS ARE IGNORED (OPTION(1)=HEAT)
!     IN STRUCTURE PROBLEMS SOME ELEMENTS ARE IGNORED (OPTION(1)=STRUCT)
!
                              CALL ktriqd(1)
                              GOTO 2
                           ELSEIF ( jltype==15 .OR. jltype==16 ) THEN
                              CALL ktrplt
                              GOTO 2
                           ELSEIF ( jltype==29 .OR. jltype==30 ) THEN
                              CALL kqdplt
                              GOTO 2
                           ELSEIF ( jltype==31 .OR. jltype==32 ) THEN
                              CALL kqdmem
                              GOTO 2
                           ELSEIF ( jltype==33 .OR. jltype==34 ) THEN
                              CALL ktriqd(2)
                              GOTO 2
                           ELSEIF ( jltype==35 .OR. jltype==36 ) THEN
                              CALL ktriqd(4)
                              GOTO 2
                           ELSEIF ( jltype==37 .OR. jltype==38 ) THEN
                              CALL ktriqd(3)
                              GOTO 2
                           ELSEIF ( jltype==61 .OR. jltype==62 .OR. jltype==91 .OR. jltype==92 ) THEN
                              GOTO 2
                           ELSEIF ( jltype==69 ) THEN
                              CALL kcones
                              GOTO 2
                           ELSEIF ( jltype==70 ) THEN
                              IF ( mach==3 ) THEN
                                 CALL kcones
                              ELSEIF ( nbpw>=60 ) THEN
                                 CALL kcone2
                              ELSE
                                 IF ( qp==0 ) CALL kconed
                                 IF ( qp/=0 ) CALL kconeq
                              ENDIF
                              GOTO 2
                           ELSEIF ( jltype==71 .OR. jltype==72 ) THEN
                              IF ( heat ) THEN
                                 CALL hring(3)
                              ELSE
                                 IF ( knogo==2 ) CYCLE
                                 CALL ktrirg
                                 IF ( knogo==2 ) CYCLE
                              ENDIF
                              GOTO 2
                           ELSEIF ( jltype==73 .OR. jltype==74 ) THEN
                              IF ( heat ) THEN
                                 CALL hring(4)
                              ELSE
                                 CALL ktrapr
                              ENDIF
                              GOTO 2
                           ELSEIF ( jltype==77 .OR. jltype==78 ) THEN
                              CALL ktetra(0,0)
                              GOTO 2
                           ELSEIF ( jltype==79 .OR. jltype==80 ) THEN
                              CALL ksolid(1)
                              GOTO 2
                           ELSEIF ( jltype==81 .OR. jltype==82 ) THEN
                              CALL ksolid(2)
                              GOTO 2
                           ELSEIF ( jltype==83 .OR. jltype==84 ) THEN
                              CALL ksolid(3)
                              GOTO 2
                           ELSEIF ( jltype==85 .OR. jltype==86 .OR. jltype==93 .OR. jltype==94 ) THEN
                              CALL kflud2
                              GOTO 2
                           ELSEIF ( jltype==87 .OR. jltype==88 .OR. jltype==95 .OR. jltype==96 ) THEN
                              CALL kflud3
                              GOTO 2
                           ELSEIF ( jltype==89 .OR. jltype==90 .OR. jltype==97 .OR. jltype==98 ) THEN
                              CALL kflud4
                              GOTO 2
                           ELSEIF ( jltype==99 .OR. jltype==100 ) THEN
                              CALL kslot(0)
                              GOTO 2
                           ENDIF
                        ENDIF
!
!            51 CSLOT4   52 CHBDY    53 CDUM1    54 CDUM2    55 CDUM3
!
!            56 CDUM4    57 CDUM5    58 CDUM6    59 CDUM7    60 CDUM8
!
!            61 CDUM9    62 CQDMEM1  63 CQDMEM2  64 CQDMEM3  65 CIHEX1
!
!            66 CIHEX2   67 CIHEX3   68 CQUADTS  69 CTRIATS  70 CTRIAAX
!
!            71 CTRAPAX  72 CAERO1   73 CTRIM6   74 CTRPLT1  75 CTRSHL
!
!            76 CFHEX1   77 CFHEX2   78 CFTETRA  79 CFWEDGE  80 CIS2D8
!
!            81 CELBOW   82 FTUBE    83 CTRIA3   84 CPSE2    85 CPSE3
!
!            86 CPSE4
!
                        IF ( local==1 .OR. local==2 ) THEN
                           CALL kslot(1)
                        ELSEIF ( local==3 .OR. local==4 .OR. local==5 .OR. local==6 .OR. local==7 .OR. local==8 .OR. local==9 .OR.  &
                               & local==10 .OR. local==11 .OR. local==12 .OR. local==13 .OR. local==14 .OR. local==15 .OR.          &
                               & local==16 .OR. local==17 .OR. local==18 .OR. local==19 .OR. local==20 .OR. local==21 .OR.          &
                               & local==22 .OR. local==27 .OR. local==28 .OR. local==29 .OR. local==30 .OR. local==31 .OR.          &
                               & local==32 .OR. local==33 .OR. local==34 .OR. local==35 .OR. local==36 .OR. local==37 .OR.          &
                               & local==38 .OR. local==39 .OR. local==40 .OR. local==41 .OR. local==42 .OR. local==43 .OR.          &
                               & local==44 .OR. local==45 .OR. local==46 .OR. local==47 .OR. local==48 .OR. local==49 .OR.          &
                               & local==50 .OR. local==59 .OR. local==60 .OR. local==63 .OR. local==64 .OR. local==65 .OR.          &
                               & local==66 .OR. local==67 .OR. local==68 .OR. local==69 .OR. local==70 .OR. local==71 .OR.          &
                               & local==72 ) THEN
                           spag_nextblock_1 = 2
                           CYCLE SPAG_DispatchLoop_1
                        ELSEIF ( local==23 .OR. local==24 ) THEN
!
!     REPLACE ELEMENT TYPE CQDMEM1 BY ELEMENT TYPE CQDMEM
!     IN -HEAT- FORMULATION
!
                           IF ( .NOT.(heat) ) THEN
                              spag_nextblock_1 = 2
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           CALL kqdmem
                        ELSEIF ( local==25 ) THEN
!
!     REPLACE ELEMENT TYPE CQDMEM2 BY ELEMENT TYPE CQDMEM
!     IN -HEAT- FORMULATION
!
                           IF ( .NOT.(heat) ) THEN
                              spag_nextblock_1 = 2
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           CALL kqdmem
                        ELSEIF ( local==26 ) THEN
!
!     REPLACE ELEMENT TYPE CQDMEM2 BY ELEMENT TYPE CQDMEM
!     IN -HEAT- FORMULATION
!
                           IF ( .NOT.(heat) ) THEN
                              spag_nextblock_1 = 2
                              CYCLE SPAG_DispatchLoop_1
                           ENDIF
                           CALL kqdmem
                        ELSEIF ( local==51 .OR. local==52 ) THEN
                           CALL ksolid(2)
                        ELSEIF ( local==53 .OR. local==54 ) THEN
                           CALL ksolid(3)
                        ELSEIF ( local==55 .OR. local==56 ) THEN
                           CALL ktetra(0,0)
                        ELSEIF ( local==57 .OR. local==58 ) THEN
                           CALL ksolid(1)
                        ELSEIF ( local==61 .OR. local==62 ) THEN
                           CALL kelbow
                        ELSE
                           CALL ktriqd(1)
                        ENDIF
                     ELSE
                        CALL emg1b(dummy,0,1,1,0)
                     ENDIF
!
!     OUTPUT THE PIVOT ROW PARTITION NOW COMPLETED BY -EMG1B-
!
 2                   CALL emg1b(0.0D0,-1111111,0,0,0.0D0)
!
!     SAVE K-DICTIONARY
!
                     DO l = 1 , 15
                        kdict(l) = dict(l)
                     ENDDO
                  ENDIF
!
!     MASS MATRIX M
!
                  IF ( flags(2)==0 ) THEN
                     spag_nextblock_2 = 5
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  IF ( heat ) THEN
                     spag_nextblock_2 = 5
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
!
!     RESTORE M-DICTIONARY IF NECESSARY
!
                  IF ( i/=1 ) THEN
                     DO l = 1 , 15
                        dict(l) = mdict(l)
                     ENDDO
                  ENDIF
                  filtyp = 2
                  ioptb = 0
                  bggind = -1
                  mnpvt = sil(i)
                  dict(2) = 1
                  IF ( sil(i)/=0 ) THEN
                     DO l = 1 , estwds
                        mecpt(l) = estbuf(l)
                     ENDDO
                  ELSE
                     CALL emg1b(dummy,0,1,2,0)
                     spag_nextblock_2 = 4
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  spag_nextblock_2 = 2
               CASE (2)
!
!     CALL THE PROPER ELEMENT MASS ROUTINE.
!
                  local = jltype - 100
                  IF ( local<=0 ) THEN
!
!     PAIRED -GO TO- ENTRIES PER ELEMENT SINGLE/DOUBLE PRECISION
!
!             1 CROD      2 C.....    3 CTUBE     4 CSHEAR    5 CTWIST
!
!             6 CTRIA1    7 CTRBSC    8 CTRPLT    9 CTRMEM   10 CONROD
!
!            11 ELAS1    12 ELAS2    13 ELAS3    14 ELAS4    15 CQDPLT
!
!            16 CQDMEM   17 CTRIA2   18 CQUAD2   19 CQUAD1   20 CDAMP1
!
!            21 CDAMP2   22 CDAMP3   23 CDAMP4   24 CVISC    25 CMASS1
!
!            26 CMASS2   27 CMASS3   28 CMASS4   29 CONM1    30 CONM2
!
!            31 PLOTEL   32 C.....   33 C.....   34 CBAR     35 CCONEAX
!
!            36 CTRIARG  37 CTRAPRG  38 CTORDRG  39 CTETRA   40 CWEDGE
!
!            41 CHEXA1   42 CHEXA2   43 CFLUID2  44 CFLUID3  45 CFLUID4
!
!            46 CFLMASS  47 CAXIF2   48 CAXIF3   49 CAXIF4   50 CSLOT3
!
                     IF ( jltype==1 .OR. jltype==2 .OR. jltype==3 .OR. jltype==4 .OR. jltype==5 .OR. jltype==6 .OR. jltype==7 .OR.  &
                        & jltype==8 .OR. jltype==9 .OR. jltype==10 .OR. jltype==13 .OR. jltype==14 .OR. jltype==17 .OR.             &
                        & jltype==18 .OR. jltype==19 .OR. jltype==20 .OR. jltype==21 .OR. jltype==22 .OR. jltype==23 .OR.           &
                        & jltype==24 .OR. jltype==25 .OR. jltype==26 .OR. jltype==27 .OR. jltype==28 .OR. jltype==39 .OR.           &
                        & jltype==40 .OR. jltype==41 .OR. jltype==42 .OR. jltype==43 .OR. jltype==44 .OR. jltype==45 .OR.           &
                        & jltype==46 .OR. jltype==47 .OR. jltype==48 .OR. jltype==49 .OR. jltype==50 .OR. jltype==51 .OR.           &
                        & jltype==52 .OR. jltype==53 .OR. jltype==54 .OR. jltype==55 .OR. jltype==56 .OR. jltype==57 .OR.           &
                        & jltype==58 .OR. jltype==59 .OR. jltype==60 .OR. jltype==63 .OR. jltype==64 .OR. jltype==65 .OR.           &
                        & jltype==66 .OR. jltype==67 .OR. jltype==68 .OR. jltype==75 .OR. jltype==76 ) THEN
                        spag_nextblock_1 = 2
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     IF ( jltype==11 .OR. jltype==12 ) THEN
                        spag_nextblock_2 = 3
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                     IF ( jltype==15 .OR. jltype==16 ) THEN
!
                        IF ( icmbar<0 ) THEN
                           CALL masstq(3)
                        ELSE
                           CALL mtrplt
                        ENDIF
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ELSEIF ( jltype==29 .OR. jltype==30 ) THEN
!
                        IF ( icmbar<0 ) THEN
                           CALL masstq(7)
                        ELSE
                           CALL mqdplt
                        ENDIF
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ELSEIF ( jltype==31 .OR. jltype==32 ) THEN
                        CALL masstq(1)
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ELSEIF ( jltype==33 .OR. jltype==34 ) THEN
!
                        IF ( icmbar<0 ) THEN
                           CALL masstq(4)
                        ELSE
                           CALL mtriqd(2)
                        ENDIF
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ELSEIF ( jltype==35 .OR. jltype==36 ) THEN
!
                        IF ( icmbar<0 ) THEN
                           CALL masstq(1)
                        ELSE
                           CALL mtriqd(4)
                        ENDIF
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ELSEIF ( jltype==37 .OR. jltype==38 ) THEN
!
                        IF ( icmbar<0 ) THEN
                           CALL masstq(2)
                        ELSE
                           CALL mtriqd(3)
                        ENDIF
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ELSEIF ( jltype==61 .OR. jltype==62 ) THEN
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ELSEIF ( jltype==69 .OR. jltype==70 ) THEN
                        CALL mcone
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ELSEIF ( jltype==71 .OR. jltype==72 ) THEN
                        IF ( mnogo==2 ) CYCLE
                        IF ( heat ) THEN
                           CALL mring(3)
                           spag_nextblock_2 = 4
                        ELSE
                           CALL mtrirg
                           IF ( mnogo==2 ) CYCLE
                           spag_nextblock_2 = 4
                        ENDIF
                        CYCLE
                     ELSEIF ( jltype==73 .OR. jltype==74 ) THEN
                        IF ( heat ) THEN
                           CALL mring(4)
                        ELSE
                           CALL mtrapr
                        ENDIF
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ELSEIF ( jltype==77 .OR. jltype==78 ) THEN
                        CALL msolid(1)
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ELSEIF ( jltype==79 .OR. jltype==80 ) THEN
                        CALL msolid(2)
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ELSEIF ( jltype==81 .OR. jltype==82 ) THEN
                        CALL msolid(3)
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ELSEIF ( jltype==83 .OR. jltype==84 ) THEN
                        CALL msolid(4)
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ELSEIF ( jltype==85 .OR. jltype==86 .OR. jltype==93 .OR. jltype==94 ) THEN
                        CALL mflud2
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ELSEIF ( jltype==87 .OR. jltype==88 .OR. jltype==95 .OR. jltype==96 ) THEN
                        CALL mflud3
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ELSEIF ( jltype==89 .OR. jltype==90 .OR. jltype==97 .OR. jltype==98 ) THEN
                        CALL mflud4
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ELSEIF ( jltype==91 .OR. jltype==92 ) THEN
                        CALL mfree
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ELSEIF ( jltype==99 .OR. jltype==100 ) THEN
                        CALL mslot(0)
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDIF
!
!
!            51 CSLOT4   52 CHBDY    53 CDUM1    54 CDUM2    55 CDUM3
!
!            56 CDUM4    57 CDUM5    58 CDUM6    59 CDUM7    60 CDUM8
!
!            61 CDUM9    62 CQDMEM1  63 CQDMEM2  64 CQDMEM3  65 CIHEX1
!
!            66 CIHEX2   67 CIHEX3   68 CQUADTS  69 CTRIATS  70 CTRIAAX
!
!            71 CTRAPAX  72 CAERO1   73 CTRIM6   74 CTRPLT1  75 CTRSHL
!
!            76 CFHEX1   77 CFHEX2   78 CFTETRA  79 CFWEDGE  80 CIS2D8
!
!            81 CELBOW   82 FTUBE    83 CTRIA3   84 CPSE2    85 CPSE3
!
!            86 CPSE4
!
                  IF ( local==1 .OR. local==2 ) THEN
                     CALL mslot(1)
                  ELSEIF ( local==3 .OR. local==4 .OR. local==5 .OR. local==6 .OR. local==7 .OR. local==8 .OR. local==9 .OR.        &
                         & local==10 .OR. local==11 .OR. local==12 .OR. local==13 .OR. local==14 .OR. local==15 .OR. local==16 .OR. &
                         & local==17 .OR. local==18 .OR. local==19 .OR. local==20 .OR. local==21 .OR. local==22 .OR. local==23 .OR. &
                         & local==24 .OR. local==25 .OR. local==26 .OR. local==27 .OR. local==28 .OR. local==29 .OR. local==30 .OR. &
                         & local==31 .OR. local==32 .OR. local==33 .OR. local==34 .OR. local==35 .OR. local==36 .OR. local==37 .OR. &
                         & local==38 .OR. local==39 .OR. local==40 .OR. local==41 .OR. local==42 .OR. local==43 .OR. local==44 .OR. &
                         & local==45 .OR. local==46 .OR. local==47 .OR. local==48 .OR. local==49 .OR. local==50 .OR. local==59 .OR. &
                         & local==60 .OR. local==63 .OR. local==64 .OR. local==65 .OR. local==66 .OR. local==67 .OR. local==68 .OR. &
                         & local==69 .OR. local==70 .OR. local==71 .OR. local==72 ) THEN
                     spag_nextblock_1 = 2
                     CYCLE SPAG_DispatchLoop_1
                  ELSEIF ( local==51 .OR. local==52 .OR. local==53 .OR. local==54 .OR. local==55 .OR. local==56 .OR. local==57 .OR. &
                         & local==58 ) THEN
                  ELSEIF ( local==61 .OR. local==62 ) THEN
                     CALL melbow
                  ELSE
                     spag_nextblock_2 = 3
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
                  spag_nextblock_2 = 4
               CASE (3)
!
!
!     CONVENTIONAL MASS MATRIX GENERATION ROUTINE CALLED WHEN
!     ICMBAR .LT. 0
!     OTHERWISE CONSISTENT MASS MATRIX GENERATION ROUTINE CALLED
!
                  IF ( icmbar<0 ) THEN
                     CALL masstq(5)
                  ELSE
                     CALL mtriqd(1)
                  ENDIF
                  spag_nextblock_2 = 4
               CASE (4)
!
!     OUTPUT THE PIVOT ROW PARTITION NOW COMPLETED BY -EMG1B-
!
                  CALL emg1b(0.0D0,-1111111,0,0,0.0D0)
                  IF ( heat ) THEN
                     spag_nextblock_2 = 6
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
!
!     SAVE M-DICTIONARY
!
                  DO l = 1 , 15
                     mdict(l) = dict(l)
                  ENDDO
                  spag_nextblock_2 = 5
               CASE (5)
!
!     DAMPING MATRIX B
!
                  IF ( flags(3)==0 ) CYCLE
                  IF ( .NOT.heat ) CYCLE
!
!     RESTORE B-DICTIONARY IF NECESSARY
!
                  IF ( i/=1 ) THEN
                     DO l = 1 , 15
                        dict(l) = bdict(l)
                     ENDDO
                  ENDIF
                  filtyp = 3
                  ioptb = -1
                  bggind = -1
                  mnpvt = sil(i)
                  dict(2) = 1
                  IF ( sil(i)/=0 ) THEN
                     DO l = 1 , estwds
                        mecpt(l) = estbuf(l)
                     ENDDO
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ELSE
                     CALL emg1b(dummy,0,1,3,0)
!
!     OUTPUT THE PIVOT ROW PARTITION NOW COMPLETED BY -EMG1B-
!
                     CALL emg1b(0.0D0,-1111111,0,0,0.0D0)
                  ENDIF
                  spag_nextblock_2 = 6
               CASE (6)
!
!     SAVE DICTIONARY
!
                  DO l = 1 , 15
                     bdict(l) = dict(l)
                  ENDDO
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
!
         ENDDO
         IF ( knogo==0 ) knogo = ktemp
         IF ( mnogo==0 ) mnogo = mtemp
         RETURN
      CASE (2)
         WRITE (outpt,99002) swm , elid , elem(izero+1) , elem(izero+2)
99002    FORMAT (A27,' 3121, EMGOLD HAS RECEIVED A CALL FOR ELEMENT ID',I9,' (ELEMENT TYPE ',2A4,2H).,/5X,                          &
               & 'ELEMENT IGNORED AS THIS ','ELEMENT TYPE IS NOT HANDLED BY EMGOLD.')
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE emgold


SUBROUTINE emgold
   IMPLICIT NONE
!
! COMMON variable declarations
!
   REAL Bggind , Dum2(2) , S1dum(10) , S2dum(10) , Skip17(17) , Skip19(19) , Skip2(2) , Skipxx(15) , Smaio(36)
   INTEGER Dict(15) , Elem(1) , Elid , Eltype , Estbuf(100) , Estid , Filtyp , Flags(3) , Ibloc , Icmbar , If4gg , Ifkgg , Incr ,   &
         & Iopt4 , Ioptb , Irows , K4ggsw , Kecpt(100) , Kflags(3) , Knogo , Knpvt , Ksafe(200) , Ksystm(65) , Kwork(700) , L38 ,   &
         & Lcstm , Ldict , Lhmat , Lmat , Lqro , Ltypes , Mach , Mecpt(100) , Mnogo , Mnpvt , Msafe(200) , Mwork(700) , Nbloc ,     &
         & Nbpw , Nelems , Nlast , Nlocs , Nsils , Outpt , Posvec(10) , Precis , Sil(10)
   DOUBLE PRECISION Dummy
   LOGICAL Error , Heat , Hydro , Kheat , Last , Lheat
   CHARACTER*25 Sfm , Uwm
   CHARACTER*31 Sim
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /emg1bx/ Nsils , Posvec , Ibloc , Nbloc , Irows , Dict , Filtyp , Sil , Last
   COMMON /emgdic/ Eltype , Ldict , Nlocs , Elid , Estid
   COMMON /emgest/ Estbuf
   COMMON /emgprm/ Skipxx , Flags , Precis , Error , Heat , Icmbar , Lcstm , Lmat , Lhmat , Kflags , L38
   COMMON /gpta1 / Nelems , Nlast , Incr , Elem
   COMMON /hydroe/ Hydro
   COMMON /iemgod/ Dummy , Ltypes
   COMMON /machin/ Mach , Dum2 , Lqro
   COMMON /sma1bk/ S1dum
   COMMON /sma1cl/ Iopt4 , K4ggsw , Knpvt , Skip19 , Knogo , Ksafe
   COMMON /sma1dp/ Kwork
   COMMON /sma1et/ Kecpt
   COMMON /sma1ht/ Kheat
   COMMON /sma1io/ Smaio
   COMMON /sma2bk/ S2dum
   COMMON /sma2cl/ Ioptb , Bggind , Mnpvt , Skip17 , Mnogo , Skip2 , Msafe
   COMMON /sma2dp/ Mwork
   COMMON /sma2et/ Mecpt
   COMMON /sma2ht/ Lheat
   COMMON /system/ Ksystm
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm , Sim
!
! Local variable declarations
!
   INTEGER bdict(15) , estwds , i , i1 , i2 , icount , index , isil , izero , j , jltype , k , kdict(15) , ktemp , l , local ,      &
         & mdict(15) , mtemp , qp
!
! End of declarations
!
!
!     THIS IS A DRIVING ROUTINE OF THE -EMG- MODULE WHICH ALLOWS PIVOT-
!     POINT-LOGIC ELEMENT SUBROUTINES TO BE USED IN CONJUNCTION WITH THE
!     NON-PIVOT-POINT PROCESS.
!
   EQUIVALENCE (Ksystm(2),Outpt) , (Ksystm(40),Nbpw) , (Smaio(11),Ifkgg) , (Smaio(13),If4gg)
!
   ktemp = Knogo
   mtemp = Mnogo
   qp = mod(Lqro/100,10)
   jltype = 2*(Eltype-1) + Precis
   Kheat = Heat
   Lheat = Heat
   izero = Incr*(Eltype-1)
   IF ( Eltype/=Ltypes ) THEN
      CALL page2(3)
      index = izero
      IF ( Heat ) THEN
         IF ( Eltype==62 .OR. Eltype==63 ) index = 15*Incr
      ENDIF
      IF ( L38==1 ) WRITE (Outpt,99001) Sim , Elem(index+1) , Elem(index+2)
99001 FORMAT (A31,' 3107',/5X,'EMGOLD CALLED BY EMGPRO TO PROCESS ',2A4,' ELEMENTS.')
      Ltypes = Eltype
   ENDIF
!
   Nsils = Elem(izero+10)
   isil = Elem(izero+13)
   IF ( Elem(izero+9)/=0 ) isil = isil - 1
   estwds = Elem(izero+12)
   i1 = isil
   i2 = isil + Nsils - 1
   l = Nsils
!
!     MOVE SILS TO SEPARATE ARRAY
!
!     SORT ARRAY OF SILS
!
!     POSITION VECTOR
!
   DO i = i1 , i2
      IF ( Estbuf(i)==0 ) THEN
         k = l
         l = l - 1
      ELSE
         k = 1
         DO j = i1 , i2
            IF ( Estbuf(j)<Estbuf(i) ) THEN
            ELSEIF ( Estbuf(j)==Estbuf(i) ) THEN
               IF ( j>=i ) CYCLE
            ELSE
               CYCLE
            ENDIF
            IF ( Estbuf(j)/=0 ) k = k + 1
         ENDDO
      ENDIF
      Posvec(k) = i - i1 + 1
      Sil(k) = Estbuf(i)
   ENDDO
!
!     ELIMINATE DUP SILS THAT MAY OCCUR,E.G. CHBDY WITH AMB.PTS.
!
   k = 1
   icount = 1
   DO i = 2 , Nsils
      DO
         k = k + 1
         IF ( k>Nsils ) THEN
            Sil(i) = 0
            Posvec(i) = 0
            EXIT
         ELSEIF ( Sil(k)/=Sil(k-1) ) THEN
            Sil(i) = Sil(k)
            IF ( Sil(k)/=0 ) icount = icount + 1
            Posvec(i) = Posvec(k)
            EXIT
         ENDIF
      ENDDO
   ENDDO
   Nsils = icount
!
!     SETUP VALUES AND DICTIONARY IN /EMG1BX/ FOR EMG1B USE
!
   Dict(1) = Estid
   Dict(2) = 1
!
!     PSUEDO SMA1-SMA2 FILE NUMBERS
!
   Ifkgg = 201
   If4gg = 202
!
!     DICT(4) WILL BE RESET TO EITHER 1 OR 63 BY EMG1B
!     BASED ON INCOMING DATA TO EMG1B
!
   DO i = 5 , 15
      Dict(i) = 0
   ENDDO
!
!     CALL ELEMENT FOR EACH PIVOT ROW
!
   Last = .FALSE.
   Knogo = 0
   Mnogo = 0
   DO i = 1 , Nsils
      IF ( i==Nsils ) Last = .TRUE.
!
!     STIFFNESS MATRIX
!
      IF ( Flags(1)/=0 ) THEN
!
!     RESTORE K-DICTIONARY IF NECESSARY
!
         IF ( i/=1 ) THEN
            DO l = 1 , 15
               Dict(l) = kdict(l)
            ENDDO
         ENDIF
         Filtyp = 1
!
!     IOPT4 IS TURNED ON SO THAT DAMPING CONSTANTS ARE SENT TO EMG1B
!     IN ALL AVAILABLE CASES BY ELEMENT ROUTINES.  MATRIX DATA WILL BE
!     IGNORED BY EMG1B ON EMG1B CALLS SENDING DAMPING CONSTANTS.
!     DAMPING CONSTANTS WILL BE PLACED IN 5TH WORD OF ELEMENT DICTIONARY
!     ENTRY.
!
         Iopt4 = 1
         K4ggsw = 0
         Knpvt = Sil(i)
!
!     FULL 6X6 MATRIX FORCED FOR STIFFNESS WITH OLD ELEMENT ROUTINES
!
         Dict(2) = 1
         IF ( Sil(i)/=0 ) THEN
            DO l = 1 , estwds
               Kecpt(l) = Estbuf(l)
            ENDDO
            Hydro = .FALSE.
            IF ( Eltype>=76 .AND. Eltype<=79 ) Hydro = .TRUE.
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
               IF ( jltype==1 .OR. jltype==2 .OR. jltype==3 .OR. jltype==4 .OR. jltype==5 .OR. jltype==6 .OR. jltype==7 .OR.        &
                  & jltype==8 .OR. jltype==9 .OR. jltype==10 .OR. jltype==13 .OR. jltype==14 .OR. jltype==17 .OR. jltype==18 .OR.   &
                  & jltype==19 .OR. jltype==20 .OR. jltype==21 .OR. jltype==22 .OR. jltype==23 .OR. jltype==24 .OR. jltype==25 .OR. &
                  & jltype==26 .OR. jltype==27 .OR. jltype==28 .OR. jltype==39 .OR. jltype==40 .OR. jltype==41 .OR. jltype==42 .OR. &
                  & jltype==43 .OR. jltype==44 .OR. jltype==45 .OR. jltype==46 .OR. jltype==47 .OR. jltype==48 .OR. jltype==49 .OR. &
                  & jltype==50 .OR. jltype==51 .OR. jltype==52 .OR. jltype==53 .OR. jltype==54 .OR. jltype==55 .OR. jltype==56 .OR. &
                  & jltype==57 .OR. jltype==58 .OR. jltype==59 .OR. jltype==60 .OR. jltype==63 .OR. jltype==64 .OR. jltype==65 .OR. &
                  & jltype==66 .OR. jltype==67 .OR. jltype==68 .OR. jltype==75 .OR. jltype==76 ) GOTO 300
               IF ( jltype==11 .OR. jltype==12 ) THEN
!
!
!     IN -HEAT- FORMULATIONS SOME ELEMENTS ARE IGNORED (OPTION(1)=HEAT)
!     IN STRUCTURE PROBLEMS SOME ELEMENTS ARE IGNORED (OPTION(1)=STRUCT)
!
                  CALL ktriqd(1)
                  GOTO 20
               ELSEIF ( jltype==15 .OR. jltype==16 ) THEN
                  CALL ktrplt
                  GOTO 20
               ELSEIF ( jltype==29 .OR. jltype==30 ) THEN
                  CALL kqdplt
                  GOTO 20
               ELSEIF ( jltype==31 .OR. jltype==32 ) THEN
                  CALL kqdmem
                  GOTO 20
               ELSEIF ( jltype==33 .OR. jltype==34 ) THEN
                  CALL ktriqd(2)
                  GOTO 20
               ELSEIF ( jltype==35 .OR. jltype==36 ) THEN
                  CALL ktriqd(4)
                  GOTO 20
               ELSEIF ( jltype==37 .OR. jltype==38 ) THEN
                  CALL ktriqd(3)
                  GOTO 20
               ELSEIF ( jltype==61 .OR. jltype==62 .OR. jltype==91 .OR. jltype==92 ) THEN
                  GOTO 20
               ELSEIF ( jltype==69 ) THEN
                  CALL kcones
                  GOTO 20
               ELSEIF ( jltype==70 ) THEN
                  IF ( Mach==3 ) THEN
                     CALL kcones
                  ELSEIF ( Nbpw>=60 ) THEN
                     CALL kcone2
                  ELSE
                     IF ( qp==0 ) CALL kconed
                     IF ( qp/=0 ) CALL kconeq
                  ENDIF
                  GOTO 20
               ELSEIF ( jltype==71 .OR. jltype==72 ) THEN
                  IF ( Heat ) THEN
                     CALL hring(3)
                     GOTO 20
                  ELSE
                     IF ( Knogo==2 ) CYCLE
                     CALL ktrirg
                     IF ( Knogo/=2 ) GOTO 20
                     CYCLE
                  ENDIF
               ELSEIF ( jltype==73 .OR. jltype==74 ) THEN
                  IF ( Heat ) THEN
                     CALL hring(4)
                  ELSE
                     CALL ktrapr
                  ENDIF
                  GOTO 20
               ELSEIF ( jltype==77 .OR. jltype==78 ) THEN
                  CALL ktetra(0,0)
                  GOTO 20
               ELSEIF ( jltype==79 .OR. jltype==80 ) THEN
                  CALL ksolid(1)
                  GOTO 20
               ELSEIF ( jltype==81 .OR. jltype==82 ) THEN
                  CALL ksolid(2)
                  GOTO 20
               ELSEIF ( jltype==83 .OR. jltype==84 ) THEN
                  CALL ksolid(3)
                  GOTO 20
               ELSEIF ( jltype==85 .OR. jltype==86 .OR. jltype==93 .OR. jltype==94 ) THEN
                  CALL kflud2
                  GOTO 20
               ELSEIF ( jltype==87 .OR. jltype==88 .OR. jltype==95 .OR. jltype==96 ) THEN
                  CALL kflud3
                  GOTO 20
               ELSEIF ( jltype==89 .OR. jltype==90 .OR. jltype==97 .OR. jltype==98 ) THEN
                  CALL kflud4
                  GOTO 20
               ELSEIF ( jltype==99 .OR. jltype==100 ) THEN
                  CALL kslot(0)
                  GOTO 20
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
            ELSEIF ( local==3 .OR. local==4 .OR. local==5 .OR. local==6 .OR. local==7 .OR. local==8 .OR. local==9 .OR.              &
                   & local==10 .OR. local==11 .OR. local==12 .OR. local==13 .OR. local==14 .OR. local==15 .OR. local==16 .OR.       &
                   & local==17 .OR. local==18 .OR. local==19 .OR. local==20 .OR. local==21 .OR. local==22 .OR. local==27 .OR.       &
                   & local==28 .OR. local==29 .OR. local==30 .OR. local==31 .OR. local==32 .OR. local==33 .OR. local==34 .OR.       &
                   & local==35 .OR. local==36 .OR. local==37 .OR. local==38 .OR. local==39 .OR. local==40 .OR. local==41 .OR.       &
                   & local==42 .OR. local==43 .OR. local==44 .OR. local==45 .OR. local==46 .OR. local==47 .OR. local==48 .OR.       &
                   & local==49 .OR. local==50 .OR. local==59 .OR. local==60 .OR. local==63 .OR. local==64 .OR. local==65 .OR.       &
                   & local==66 .OR. local==67 .OR. local==68 .OR. local==69 .OR. local==70 .OR. local==71 .OR. local==72 ) THEN
               GOTO 300
            ELSEIF ( local==23 .OR. local==24 ) THEN
!
!     REPLACE ELEMENT TYPE CQDMEM1 BY ELEMENT TYPE CQDMEM
!     IN -HEAT- FORMULATION
!
               IF ( .NOT.(Heat) ) GOTO 300
               CALL kqdmem
            ELSEIF ( local==25 ) THEN
!
!     REPLACE ELEMENT TYPE CQDMEM2 BY ELEMENT TYPE CQDMEM
!     IN -HEAT- FORMULATION
!
               IF ( .NOT.(Heat) ) GOTO 300
               CALL kqdmem
            ELSEIF ( local==26 ) THEN
!
!     REPLACE ELEMENT TYPE CQDMEM2 BY ELEMENT TYPE CQDMEM
!     IN -HEAT- FORMULATION
!
               IF ( .NOT.(Heat) ) GOTO 300
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
            CALL emg1b(Dummy,0,1,1,0)
         ENDIF
!
!     OUTPUT THE PIVOT ROW PARTITION NOW COMPLETED BY -EMG1B-
!
 20      CALL emg1b(0.0D0,-1111111,0,0,0.0D0)
!
!     SAVE K-DICTIONARY
!
         DO l = 1 , 15
            kdict(l) = Dict(l)
         ENDDO
      ENDIF
!
!     MASS MATRIX M
!
      IF ( Flags(2)==0 ) GOTO 200
      IF ( Heat ) GOTO 200
!
!     RESTORE M-DICTIONARY IF NECESSARY
!
      IF ( i/=1 ) THEN
         DO l = 1 , 15
            Dict(l) = mdict(l)
         ENDDO
      ENDIF
      Filtyp = 2
      Ioptb = 0
      Bggind = -1
      Mnpvt = Sil(i)
      Dict(2) = 1
      IF ( Sil(i)/=0 ) THEN
         DO l = 1 , estwds
            Mecpt(l) = Estbuf(l)
         ENDDO
      ELSE
         CALL emg1b(Dummy,0,1,2,0)
         GOTO 150
      ENDIF
!
!     CALL THE PROPER ELEMENT MASS ROUTINE.
!
 50   local = jltype - 100
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
         IF ( jltype==1 .OR. jltype==2 .OR. jltype==3 .OR. jltype==4 .OR. jltype==5 .OR. jltype==6 .OR. jltype==7 .OR.              &
            & jltype==8 .OR. jltype==9 .OR. jltype==10 .OR. jltype==13 .OR. jltype==14 .OR. jltype==17 .OR. jltype==18 .OR.         &
            & jltype==19 .OR. jltype==20 .OR. jltype==21 .OR. jltype==22 .OR. jltype==23 .OR. jltype==24 .OR. jltype==25 .OR.       &
            & jltype==26 .OR. jltype==27 .OR. jltype==28 .OR. jltype==39 .OR. jltype==40 .OR. jltype==41 .OR. jltype==42 .OR.       &
            & jltype==43 .OR. jltype==44 .OR. jltype==45 .OR. jltype==46 .OR. jltype==47 .OR. jltype==48 .OR. jltype==49 .OR.       &
            & jltype==50 .OR. jltype==51 .OR. jltype==52 .OR. jltype==53 .OR. jltype==54 .OR. jltype==55 .OR. jltype==56 .OR.       &
            & jltype==57 .OR. jltype==58 .OR. jltype==59 .OR. jltype==60 .OR. jltype==63 .OR. jltype==64 .OR. jltype==65 .OR.       &
            & jltype==66 .OR. jltype==67 .OR. jltype==68 .OR. jltype==75 .OR. jltype==76 ) GOTO 300
         IF ( jltype==11 .OR. jltype==12 ) GOTO 100
         IF ( jltype==15 .OR. jltype==16 ) THEN
!
            IF ( Icmbar<0 ) THEN
               CALL masstq(3)
            ELSE
               CALL mtrplt
            ENDIF
            GOTO 150
         ELSEIF ( jltype==29 .OR. jltype==30 ) THEN
!
            IF ( Icmbar<0 ) THEN
               CALL masstq(7)
            ELSE
               CALL mqdplt
            ENDIF
            GOTO 150
         ELSEIF ( jltype==31 .OR. jltype==32 ) THEN
            CALL masstq(1)
            GOTO 150
         ELSEIF ( jltype==33 .OR. jltype==34 ) THEN
!
            IF ( Icmbar<0 ) THEN
               CALL masstq(4)
            ELSE
               CALL mtriqd(2)
            ENDIF
            GOTO 150
         ELSEIF ( jltype==35 .OR. jltype==36 ) THEN
!
            IF ( Icmbar<0 ) THEN
               CALL masstq(1)
            ELSE
               CALL mtriqd(4)
            ENDIF
            GOTO 150
         ELSEIF ( jltype==37 .OR. jltype==38 ) THEN
!
            IF ( Icmbar<0 ) THEN
               CALL masstq(2)
            ELSE
               CALL mtriqd(3)
            ENDIF
            GOTO 150
         ELSEIF ( jltype==61 .OR. jltype==62 ) THEN
            GOTO 150
         ELSEIF ( jltype==69 .OR. jltype==70 ) THEN
            CALL mcone
            GOTO 150
         ELSEIF ( jltype==71 .OR. jltype==72 ) THEN
            IF ( Mnogo==2 ) CYCLE
            IF ( Heat ) THEN
               CALL mring(3)
               GOTO 150
            ELSE
               CALL mtrirg
               IF ( Mnogo/=2 ) GOTO 150
               CYCLE
            ENDIF
         ELSEIF ( jltype==73 .OR. jltype==74 ) THEN
            IF ( Heat ) THEN
               CALL mring(4)
            ELSE
               CALL mtrapr
            ENDIF
            GOTO 150
         ELSEIF ( jltype==77 .OR. jltype==78 ) THEN
            CALL msolid(1)
            GOTO 150
         ELSEIF ( jltype==79 .OR. jltype==80 ) THEN
            CALL msolid(2)
            GOTO 150
         ELSEIF ( jltype==81 .OR. jltype==82 ) THEN
            CALL msolid(3)
            GOTO 150
         ELSEIF ( jltype==83 .OR. jltype==84 ) THEN
            CALL msolid(4)
            GOTO 150
         ELSEIF ( jltype==85 .OR. jltype==86 .OR. jltype==93 .OR. jltype==94 ) THEN
            CALL mflud2
            GOTO 150
         ELSEIF ( jltype==87 .OR. jltype==88 .OR. jltype==95 .OR. jltype==96 ) THEN
            CALL mflud3
            GOTO 150
         ELSEIF ( jltype==89 .OR. jltype==90 .OR. jltype==97 .OR. jltype==98 ) THEN
            CALL mflud4
            GOTO 150
         ELSEIF ( jltype==91 .OR. jltype==92 ) THEN
            CALL mfree
            GOTO 150
         ELSEIF ( jltype==99 .OR. jltype==100 ) THEN
            CALL mslot(0)
            GOTO 150
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
      ELSEIF ( local==3 .OR. local==4 .OR. local==5 .OR. local==6 .OR. local==7 .OR. local==8 .OR. local==9 .OR. local==10 .OR.     &
             & local==11 .OR. local==12 .OR. local==13 .OR. local==14 .OR. local==15 .OR. local==16 .OR. local==17 .OR.             &
             & local==18 .OR. local==19 .OR. local==20 .OR. local==21 .OR. local==22 .OR. local==23 .OR. local==24 .OR.             &
             & local==25 .OR. local==26 .OR. local==27 .OR. local==28 .OR. local==29 .OR. local==30 .OR. local==31 .OR.             &
             & local==32 .OR. local==33 .OR. local==34 .OR. local==35 .OR. local==36 .OR. local==37 .OR. local==38 .OR.             &
             & local==39 .OR. local==40 .OR. local==41 .OR. local==42 .OR. local==43 .OR. local==44 .OR. local==45 .OR.             &
             & local==46 .OR. local==47 .OR. local==48 .OR. local==49 .OR. local==50 .OR. local==59 .OR. local==60 .OR.             &
             & local==63 .OR. local==64 .OR. local==65 .OR. local==66 .OR. local==67 .OR. local==68 .OR. local==69 .OR.             &
             & local==70 .OR. local==71 .OR. local==72 ) THEN
         GOTO 300
      ELSEIF ( local==51 .OR. local==52 .OR. local==53 .OR. local==54 .OR. local==55 .OR. local==56 .OR. local==57 .OR. local==58 ) &
             & THEN
      ELSEIF ( local==61 .OR. local==62 ) THEN
         CALL melbow
      ELSE
         GOTO 100
      ENDIF
      GOTO 150
!
!
!     CONVENTIONAL MASS MATRIX GENERATION ROUTINE CALLED WHEN
!     ICMBAR .LT. 0
!     OTHERWISE CONSISTENT MASS MATRIX GENERATION ROUTINE CALLED
!
 100  IF ( Icmbar<0 ) THEN
         CALL masstq(5)
      ELSE
         CALL mtriqd(1)
      ENDIF
!
!     OUTPUT THE PIVOT ROW PARTITION NOW COMPLETED BY -EMG1B-
!
 150  CALL emg1b(0.0D0,-1111111,0,0,0.0D0)
      IF ( Heat ) GOTO 250
!
!     SAVE M-DICTIONARY
!
      DO l = 1 , 15
         mdict(l) = Dict(l)
      ENDDO
!
!     DAMPING MATRIX B
!
 200  IF ( Flags(3)==0 ) CYCLE
      IF ( .NOT.Heat ) CYCLE
!
!     RESTORE B-DICTIONARY IF NECESSARY
!
      IF ( i/=1 ) THEN
         DO l = 1 , 15
            Dict(l) = bdict(l)
         ENDDO
      ENDIF
      Filtyp = 3
      Ioptb = -1
      Bggind = -1
      Mnpvt = Sil(i)
      Dict(2) = 1
      IF ( Sil(i)/=0 ) THEN
         DO l = 1 , estwds
            Mecpt(l) = Estbuf(l)
         ENDDO
         GOTO 50
      ELSE
         CALL emg1b(Dummy,0,1,3,0)
!
!     OUTPUT THE PIVOT ROW PARTITION NOW COMPLETED BY -EMG1B-
!
         CALL emg1b(0.0D0,-1111111,0,0,0.0D0)
      ENDIF
!
!     SAVE DICTIONARY
!
 250  DO l = 1 , 15
         bdict(l) = Dict(l)
      ENDDO
!
   ENDDO
   IF ( Knogo==0 ) Knogo = ktemp
   IF ( Mnogo==0 ) Mnogo = mtemp
   GOTO 99999
 300  WRITE (Outpt,99002) Swm , Elid , Elem(izero+1) , Elem(izero+2)
99002 FORMAT (A27,' 3121, EMGOLD HAS RECEIVED A CALL FOR ELEMENT ID',I9,' (ELEMENT TYPE ',2A4,2H).,/5X,'ELEMENT IGNORED AS THIS ',  &
             &'ELEMENT TYPE IS NOT HANDLED BY EMGOLD.')
!
99999 RETURN
END SUBROUTINE emgold

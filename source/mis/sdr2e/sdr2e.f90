!*==sdr2e.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdr2e(Ieqex,Neqex) !HIDESTARS (*,Ieqex,Neqex)
!
!     THIS ROUTINE WHICH IS CALLED ONLY FROM SDR2D WILL PROCESS THE ESTA
!     FILE ONCE AND OUTPUT FORCE AND OR STRESS RESULTS ON OEF1 AND OR
!     OES1 WHICH ARE OPENED IN SDR2D.
!
   IMPLICIT NONE
   USE C_BLANK
   USE C_CLSTRS
   USE C_GPTA1
   USE C_ISAVE
   USE C_LHPWX
   USE C_NAMES
   USE C_SDR2C1
   USE C_SDR2DE
   USE C_SDR2X1
   USE C_SDR2X2
   USE C_SDR2X4
   USE C_SDR2X7
   USE C_SDR2X8
   USE C_SDR2X9
   USE C_SDRETT
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ieqex
   INTEGER :: Neqex
!
! Local variable declarations rewritten by SPAG
!
   LOGICAL :: acstic , again , heat , idforc , idlyfr , idlyst , idstrs , ok2wrt
   INTEGER , DIMENSION(50) , SAVE :: buf
   REAL , DIMENSION(1) :: bufr , zz
   INTEGER :: fphase , id , idelem , ifltyp , ijk , iout , ipr , iprec , isetfr , isetid , ist , istore , istrpt , isvsrc , itherm ,&
            & jcmplx , jcore , jltype , jout , jtemp , kk , lforce , local , lstres , ncmplx , nelhar , ngp , ngp1 , ngpx , nip ,   &
            & nip3 , npt , nptfor , nptstr , nsesta , nsetfr , nw , oharms , oldawd , opte , sphase
   INTEGER , SAVE :: ielchk , ielold
   LOGICAL , DIMENSION(4) :: ilogic
   INTEGER , DIMENSION(12) , SAVE :: platit
   EXTERNAL andf , fwdrec , int2al , magpha , mesage , read , rewind , saxif2 , sbar2 , sbspl2 , scone2 , scone3 , sdhtf2 , sdretd ,&
          & sdum12 , sdum22 , sdum32 , sdum42 , sdum52 , sdum62 , sdum72 , sdum82 , sdum92 , selas2 , selbo2 , sihex2 , spanl2 ,    &
          & sqdm12 , sqdm22 , squd42 , srod2 , ss2d82 , sslot2 , ssold2 , stord2 , stpax2 , stpax3 , stqme2 , strap2 , strax2 ,     &
          & strax3 , stri32 , strir2 , strm62 , strp12 , strqd2 , strsl2 , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   !>>>>EQUIVALENCE (Ksystm(2),Opte) , (Ksystm(55),Iprec) , (Ksystm(56),Itherm)
   !>>>>EQUIVALENCE (buf(1),bufr(1)) , (Z(1),Zz(1)) , (idstrs,ilogic(1)) , (idforc,ilogic(2)) , (idlyst,ilogic(3)) , (idlyfr,ilogic(4)) ,&
!>>>>    & (Temp,Jtemp) , (Nelhar,Elwork(155))
   DATA platit/4HLOAD , 4H FAC , 4HTOR  , 9*0/
   DATA buf/50*0/ , ielold/0/ , ielchk/0/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     INITIALIZE ESTA POINTERS.
!
         IF ( Stresx==0 .AND. Forcex==0 ) RETURN
         heat = .FALSE.
         IF ( itherm/=0 ) heat = .TRUE.
         Estawd = Iesta
         istore = 0
         Ix = Icc + Harms
         again = .FALSE.
         oharms = Z(Ix)
         IF ( oharms<0 ) oharms = Nharms
         Isave = Ivec
         isvsrc = Sorc
         Eltype = Z(Estawd)
         File = Esta
         Ix = Icc + Istr + 2
         sphase = iabs(Z(Ix))
         Ix = Icc + Ielf + 2
         fphase = iabs(Z(Ix))
         Twotop = alog10(2.0**Mtisa)
!
!     POSITION TO THE PROPER THERMAL RECORD IF NECESSARY.
!
         Record = .FALSE.
         IF ( Tloads/=0 ) THEN
            IF ( Tmprec/=0 ) THEN
               CALL rewind(Gptt)
               File = Gptt
               DO I = 1 , Tmprec
                  CALL fwdrec(*100,Gptt)
               ENDDO
!
!     READ AND VERIFY SET-ID  (FAILSAFE)
!
               CALL read(*100,*120,Gptt,isetid,1,0,Flag)
               IF ( Tloads/=isetid ) THEN
                  WRITE (opte,99001) Sfm , Tloads , isetid
99001             FORMAT (A25,' 4019, SDR2E DETECTS INVALID TEMPERATURE DATA FOR ','TEMPERATURE LOAD SET',2I10)
                  CALL mesage(-61,0,0)
               ENDIF
               Record = .TRUE.
!
!     INITIALIZE /SDRETT/ VARIABLES
!
               Oldeid = 0
               Oldel = 0
               Eorflg = .FALSE.
               Endid = .TRUE.
            ENDIF
         ENDIF
         Itemp = Tloads
         IF ( Nesta==0 ) THEN
            CALL rewind(Esta)
            CALL read(*80,*120,Esta,Eltype,1,0,Flag)
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     ELEMENT PARAMETERS FOR NEW ELEMENT TYPE
!
         Ielem = (Eltype-1)*Incr
         Ieltyp = Eltype
         ipr = iprec
         IF ( ipr/=1 ) ipr = 0
         jltype = 2*Eltype - ipr
         jcore = Icore
         IF ( heat .AND. Eltype/=82 ) THEN
!
            Nwdfor = 9
            Nwdstr = 0
            nptfor = 0
            nptstr = 0
            Nwdsa = 142
!
!     CHOP OFF 483 WORDS FROM OPEN CORE SPACE FOR CIHEX ELEMENTS
!
            IF ( Eltype>=65 .AND. Eltype<=67 ) Icore = Icore - 483
         ELSE
!                            FTUBE
            Nwdsa = Elem(Ielem+17)
            nptstr = Elem(Ielem+20)
            nptfor = Elem(Ielem+21)
            Nwdstr = Elem(Ielem+18)
            Nwdfor = Elem(Ielem+19)
         ENDIF
!
!
!     SETUP STRESS PRECISION CHECK.
!
         Nchk = Z(Icc+146)
         Fnchk = Nchk
!
!     SUBCASE ID
!
         Isub = Z(Icc+1)
!
!     DETERMINE LOAD/MODE, EIGENVALUE/FREQ/TIME HEADER
!
         Frtmei(1) = 0.
         Frtmei(2) = 0.
         IF ( Branch==5 .OR. Branch==6 ) THEN
!
!     FREQUENCY/TRANSIENT
!
            I = Icc + Idload
            Ild = Z(I)
            Frtmei(1) = zz(Jlist)
         ELSEIF ( Branch==2 .OR. Branch==8 .OR. Branch==9 ) THEN
!
!     EIGENVALUES
!
            Ild = Z(Jlist)
            Frtmei(1) = zz(Jlist+1)
            Frtmei(2) = zz(Jlist+2)
            IF ( Branch==2 ) THEN
               IF ( zz(Jlist+1)>0. ) Frtmei(1) = sqrt(zz(Jlist+1))/6.2831852
            ENDIF
         ELSE
!
!     STATICS
!
            I = Icc + Isload
            Ild = Z(I)
         ENDIF
         lstres = Nwdstr
         lforce = Nwdfor
         idstrs = .FALSE.
         idforc = .FALSE.
         idlyst = .FALSE.
         idlyfr = .FALSE.
         ok2wrt = .TRUE.
         IF ( Ktype==1 .OR. nptstr/=0 .OR. nptfor/=0 ) THEN
            IF ( Nwdstr+Nwdfor>0 ) THEN
!
!     OK SOME STRESS AND OR FORCE REQUESTS EXIST FOR THIS ELEMENT TYPE.
!     PROCESS INDIVIDUAL ELEMENTS REQUESTED
!
               IF ( Nesta/=0 ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Nwdsa<=Icore ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!
!     INSUFFICIENT CORE TO HOLD ESTA FOR 1 ELEMENT OF CURRENT ELEMENT
!     TYPE TRY PROCESSING THE OTHER ELEMENT TYPES IN AVAILABLE CORE.
!
               CALL mesage(8,0,Nam(1))
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     NO STRESS OR FORCE WORDS POSSIBLE FOR THIS ELEMENT TYPE IF FALL
!     HERE
!
         IF ( Nesta/=0 ) THEN
            DO
!
!     FIND END OF CURRENT ELEMEMT TYPE LIST IN CORE
!
               Estawd = Estawd + Nwdsa
               IF ( Z(Estawd+1)==0 ) THEN
                  spag_nextblock_1 = 36
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     FORWARD REC ON FILE TO NEXT ELEMENT TYPE
!
         CALL fwdrec(*100,Esta)
         CALL read(*80,*120,Esta,Eltype,1,0,Flag)
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
      CASE (4)
!
         CALL read(*100,*60,Esta,Z(Iesta),Nwdsa,0,Flag)
         Estawd = Iesta - 1
         spag_nextblock_1 = 5
      CASE (5)
!
!     DETERMINE IF THIS PARTICULAR ELEMENT OF THE CURRENT ELEMENT TYPE
!     HAS A STRESS OR FORCE REQUEST IN THE CURRENT CASE CONTROL RECORD.
!
         Elemid = Z(Estawd+1)
!
!     THE FOLLOWING CODE (THRU 93) IS FOR THE COMPLEX ANALYSIS OF IHEX
!     ELEMENTS ONLY (ELEM. TYPES 65,66,67)
!
         IF ( Ktype==2 .AND. Eltype>=65 .AND. Eltype<=67 ) THEN
            IF ( Ipart==2 .AND. istrpt==(nip3+ngp1+1) ) THEN
!
!     DONE FOR THIS IHEX ELEMENT, RESET CHECKING VARIABLES
!
               Ipart = 0
               ielold = 0
               ielchk = 0
!
!     FIRST INTEGRATION POINT FOR IMAGINARY RETULS FOR THIS IHEX ELEMENT
!     SAVE ELEMENT ID AND CURRENT ESTAWD
!
            ELSEIF ( Ipart/=1 .OR. istrpt/=1 ) THEN
!
!     FIRST INTEGRATION POINT FOR REAL RESULTS FOR THIS IHEX ELEMENT,
!     SAVE ELEMENT ID TO CHECK WITH EARLIER ELEMENT ID SAVED ABOVE
!
               IF ( Ipart==2 .AND. istrpt==1 ) ielchk = Elemid
            ELSE
               ielold = Elemid
               oldawd = Estawd - Nwdsa
            ENDIF
         ENDIF
!
!     END OF SPECIAL TREATMENT FOR IHEX ELEMENT
!
         idelem = Elemid
!
!     DECODE ELEMID TO FIND IT IN SET
!
         IF ( Axic ) THEN
            nelhar = Elemid - (Elemid/1000)*1000
            Elemid = Elemid/1000
         ENDIF
         Jstrs = 0
         Jforc = 0
         I = Isets
         IF ( Nwdstr/=0 ) THEN
            IF ( Stresx<0 ) THEN
               Jstrs = 1
            ELSEIF ( Stresx/=0 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 8
         CYCLE SPAG_DispatchLoop_1
      CASE (6)
         IF ( I/=Nsets ) THEN
            IF ( Z(I+1)<=0 ) THEN
               I = I + 1
               IF ( Elemid<Z(I-1) .OR. Elemid>-Z(I) ) THEN
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               Jstrs = 1
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( Elemid==Z(I) ) THEN
            Jstrs = 1
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         I = I + 1
         IF ( I<=Nsets ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
         I = Isetf
         IF ( Nwdfor/=0 ) THEN
            IF ( Forcex<0 ) THEN
               Jforc = 1
            ELSEIF ( Forcex/=0 ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 11
         CYCLE SPAG_DispatchLoop_1
      CASE (9)
         IF ( I/=Nsetf ) THEN
            IF ( Z(I+1)<=0 ) THEN
               I = I + 1
               IF ( Elemid<Z(I-1) .OR. Elemid>-Z(I) ) THEN
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               Jforc = 1
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( Elemid==Z(I) ) THEN
            Jforc = 1
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 10
      CASE (10)
         I = I + 1
         IF ( I<=Nsetf ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 11
      CASE (11)
         Jany = Jstrs + Jforc
         IF ( Jany==0 ) THEN
            IF ( Nesta==0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            Estawd = Estawd + Nwdsa
            spag_nextblock_1 = 34
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     OK FALL HERE AND A STRESS OR FORCE REQUEST EXISTS
!     IF THERMAL LOADING, GET THE ELEMENT THERMAL DATA.
!     IF ELEMENT DEFORMATIONS, LOOK UP THE DEFORMATION
!
!
!     ELEMENT TEMPERATURE
!
            IF ( Tloads==0 ) THEN
!
!     NORMALLY TGRID(1) WILL CONTAIN THE AVERAGE ELEMENT TEMPERATUE
!     AND IF GRID POINT TEMPERATURES ARE RETURNED THEY WILL BEGIN
!     IN TGRID(2).
!
               jtemp = -1
            ELSE
               N = Elem(Ielem+10)
!
!     IF NEW ELEMENTS ARE ADDED THAT HAVE SPECIAL BENDING THERMAL DATA
!     POSSIBLE THEN THE FOLLOWING TEST SHOULD BE EXPANDED TO INCLUDE
!     THEIR ELEMENT TYPE SO AS TO RECEIVE ZEROS AND ONLY THE AVERAGE
!     TEMPERATURE RATHER THAN SIMULATED GRID POINT TEMPERATURES IN THE
!     ABSENCE OF ANY USER SPECIFIED DATA.
!
               IF ( Ieltyp==34 .OR. Ieltyp==6 .OR. Ieltyp==7 .OR. Ieltyp==8 .OR. Ieltyp==15 .OR. Ieltyp==17 .OR. Ieltyp==18 .OR.    &
                  & Ieltyp==19 ) N = 0
               IF ( Ieltyp==74 .OR. Ieltyp==75 ) N = 0
               IF ( Ieltyp==64 .OR. Ieltyp==83 ) N = 0
!
!
               CALL sdretd(idelem,Tgrid,N)
!
!     SET THE AVERAGE ELEMENT TEMPERATURE CELL.
!
               Temp = Tgrid(1)
            ENDIF
!
!     ELEMENT DEFORMATION
!
            Deform = 0.0
            IF ( Eldef/=0 ) THEN
               DO I = Idef , Ndef , 2
                  IF ( Z(I)==Elemid ) GOTO 10
               ENDDO
            ENDIF
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
 10         Deform = zz(I+1)
         ENDIF
         spag_nextblock_1 = 12
      CASE (12)
!
!     WRITE ID FOR STRESSES IF NOT YET WRITTEN FOR THIS ELEMENT TYPE.
!
         IF ( Stress/=0 .AND. Nwdstr/=0 .AND. Jstrs/=0 ) THEN
            IF ( Comps==-1 .AND. Nstrop>1 ) THEN
!
               IF ( .NOT.(idlyst) ) THEN
                  Nlogic = 3
                  Ofile = Oes1l
                  Device = Sdest
                  ifltyp = 22
                  Irecx = Icc + Istr
                  Nwds = 10
                  jcmplx = 0
                  ok2wrt = .FALSE.
                  ASSIGN 20 TO Iretrn
                  spag_nextblock_1 = 37
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( .NOT.(idstrs) ) THEN
               Nlogic = 1
               Ofile = Oes1
               Device = Sdest
               Iseq = 4
               ifltyp = Dtype(Iseq)
               Irecx = Icc + Istr
               Nwds = Nwdstr
               jcmplx = nptstr
               ASSIGN 20 TO Iretrn
               spag_nextblock_1 = 37
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     WRITE ID FOR FORCES IF NOT YET WRITTEN FOR THIS ELEMENT TYPE.
!
 20      IF ( Force/=0 .AND. Nwdfor/=0 .AND. Jforc/=0 ) THEN
            IF ( Comps==-1 .AND. Nstrop>1 .AND. Stress/=0 ) THEN
!
               IF ( .NOT.(idlyfr) ) THEN
                  Nlogic = 4
                  Ofile = Oef1l
                  Device = Fdest
                  ifltyp = 23
                  Irecx = Icc + Ielf
                  Nwds = 9
                  jcmplx = 0
                  ok2wrt = .FALSE.
                  ASSIGN 40 TO Iretrn
                  spag_nextblock_1 = 37
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( .NOT.(idforc) ) THEN
               Nlogic = 2
               Ofile = Oef1
               Device = Fdest
               Iseq = 5
               ifltyp = Dtype(Iseq)
               Irecx = Icc + Ielf
               Nwds = Nwdfor
               jcmplx = nptfor
               ASSIGN 40 TO Iretrn
               spag_nextblock_1 = 37
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     MOVE ESTA DATA INTO /SDR2X7/
!
 40      nsesta = Estawd
         IF ( ielchk==0 .OR. Ipart<2 .OR. ielchk/=ielold ) THEN
            Ipart = 0
         ELSE
            Ipart = 1
         ENDIF
         spag_nextblock_1 = 13
      CASE (13)
         Ipart = Ipart + 1
         DO I = 1 , Nwdsa
            Estawd = Estawd + 1
            Elesta(I) = Z(Estawd)
         ENDDO
         acstic = .FALSE.
!
!     CALL APPROPRIATE ELEMENT ROUTINE FOR STRESS AND FORCE COMPUTATIONS
!
         IF ( heat ) THEN
!
!     PHASE TWO HEAT ONLY (ALL ELEMENTS)
!
            CALL sdhtf2(Ieqex,Neqex)
         ELSE
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
!            31 PLOTEL   32 C.....   33 C.....   34 CBAR     35 CCONE
!
!            36 CTRIARG  37 CTRAPRG  38 CTORDRG  39 CTETRA   40 CWEDGE
!
!            41 CHEXA1   42 CHEXA2   43 CFLUID2  44 CFLUID3  45 CFLUID4
!
!            46 CFLMASS  47 CAXIF2   48 CAXIF3   49 CAXIF4   50 CSLOT3
!
               IF ( jltype==1 .OR. jltype==2 .OR. jltype==5 .OR. jltype==6 .OR. jltype==19 .OR. jltype==20 ) THEN
!
                  CALL srod2
               ELSEIF ( jltype==3 .OR. jltype==4 .OR. jltype==39 .OR. jltype==40 .OR. jltype==41 .OR. jltype==42 .OR.               &
                      & jltype==43 .OR. jltype==44 .OR. jltype==45 .OR. jltype==46 .OR. jltype==47 .OR. jltype==48 .OR.             &
                      & jltype==49 .OR. jltype==50 .OR. jltype==51 .OR. jltype==52 .OR. jltype==53 .OR. jltype==54 .OR.             &
                      & jltype==55 .OR. jltype==56 .OR. jltype==57 .OR. jltype==58 .OR. jltype==59 .OR. jltype==60 .OR.             &
                      & jltype==61 .OR. jltype==62 .OR. jltype==63 .OR. jltype==64 .OR. jltype==65 .OR. jltype==66 .OR.             &
                      & jltype==85 .OR. jltype==86 .OR. jltype==87 .OR. jltype==88 .OR. jltype==89 .OR. jltype==90 .OR.             &
                      & jltype==91 .OR. jltype==92 ) THEN
                  spag_nextblock_1 = 34
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( jltype==7 .OR. jltype==8 ) THEN
                  K = 4
                  CALL spanl2(K)
               ELSEIF ( jltype==9 .OR. jltype==10 ) THEN
                  K = 5
                  CALL spanl2(K)
               ELSEIF ( jltype==11 .OR. jltype==12 .OR. jltype==33 .OR. jltype==34 ) THEN
                  K = 3
                  CALL strqd2(K,Tgrid(1))
               ELSEIF ( jltype==13 .OR. jltype==14 ) THEN
                  K = 0
                  CALL sbspl2(K,Tgrid(1))
               ELSEIF ( jltype==15 .OR. jltype==16 ) THEN
                  K = 3
                  CALL sbspl2(K,Tgrid(1))
               ELSEIF ( jltype==17 .OR. jltype==18 ) THEN
                  K = 1
                  CALL stqme2(K)
               ELSEIF ( jltype==21 .OR. jltype==22 .OR. jltype==23 .OR. jltype==24 .OR. jltype==25 .OR. jltype==26 .OR.             &
                      & jltype==27 .OR. jltype==28 ) THEN
                  CALL selas2
               ELSEIF ( jltype==29 .OR. jltype==30 ) THEN
                  K = 4
                  CALL sbspl2(K,Tgrid(1))
               ELSEIF ( jltype==31 .OR. jltype==32 ) THEN
                  K = 2
                  CALL stqme2(K)
               ELSEIF ( jltype==35 .OR. jltype==36 .OR. jltype==37 .OR. jltype==38 ) THEN
                  K = 4
                  CALL strqd2(K,Tgrid(1))
               ELSEIF ( jltype==67 .OR. jltype==68 ) THEN
                  CALL sbar2(Tgrid(1))
               ELSEIF ( jltype==69 .OR. jltype==70 ) THEN
                  again = .FALSE.
                  CALL scone2(Sorc)
               ELSEIF ( jltype==71 .OR. jltype==72 ) THEN
                  CALL strir2(Tgrid(2))
               ELSEIF ( jltype==73 .OR. jltype==74 ) THEN
                  CALL strap2(Tgrid(2))
               ELSEIF ( jltype==75 .OR. jltype==76 ) THEN
                  CALL stord2(Tgrid(2))
               ELSEIF ( jltype==77 .OR. jltype==78 ) THEN
                  CALL ssold2(1,Tgrid(2))
               ELSEIF ( jltype==79 .OR. jltype==80 ) THEN
                  CALL ssold2(2,Tgrid(2))
               ELSEIF ( jltype==81 .OR. jltype==82 ) THEN
                  CALL ssold2(3,Tgrid(2))
               ELSEIF ( jltype==83 .OR. jltype==84 ) THEN
                  CALL ssold2(4,Tgrid(2))
               ELSEIF ( jltype==93 .OR. jltype==94 ) THEN
                  kk = 0
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( jltype==95 .OR. jltype==96 ) THEN
                  kk = 1
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( jltype==97 .OR. jltype==98 ) THEN
                  kk = 2
                  spag_nextblock_1 = 14
                  CYCLE SPAG_DispatchLoop_1
               ELSEIF ( jltype==99 .OR. jltype==100 ) THEN
                  kk = 0
                  spag_nextblock_1 = 15
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  GOTO 50
               ENDIF
               spag_nextblock_1 = 17
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!            51 CSLOT4   52 CHBDY    53 CDUM1    54 CDUM2    55 CDUM3
!
!            56 CDUM4    57 CDUM5    58 CDUM6    59 CDUM7    60 CDUM8
!
!            61 CDUM9    62 CQDMEM1  63 CQDMEM2  64 CQUAD4   65 CIHEX1
!
!            66 CIHEX2   67 CIHEX3   68 CQUADTS  69 CTRIATS  70 CTRIAAX
!
!            71 CTRAPAX  72 CAERO1   73 CTRIM6   74 CTRPLT1  75 CTRSHL
!
!            76 CFHEX1   77 CFHEX2   78 CFTETRA  79 CFWEDGE  80 CIS2D8
!
!            81 CELBOW   82 CFTUBE   83 CTRIA3
!
 50         IF ( local==1 .OR. local==2 ) THEN
               kk = 1
               spag_nextblock_1 = 15
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( local==3 .OR. local==4 .OR. local==43 .OR. local==44 .OR. local==51 .OR. local==52 .OR. local==53 .OR.         &
                   & local==54 .OR. local==55 .OR. local==56 .OR. local==57 .OR. local==58 .OR. local==63 .OR. local==64 ) THEN
               spag_nextblock_1 = 34
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( local==5 .OR. local==6 ) THEN
               CALL sdum12
            ELSEIF ( local==7 .OR. local==8 ) THEN
               CALL sdum22
            ELSEIF ( local==9 .OR. local==10 ) THEN
               CALL sdum32
            ELSEIF ( local==11 .OR. local==12 ) THEN
               CALL sdum42
            ELSEIF ( local==13 .OR. local==14 ) THEN
               CALL sdum52
            ELSEIF ( local==15 .OR. local==16 ) THEN
               CALL sdum62
            ELSEIF ( local==17 .OR. local==18 ) THEN
               CALL sdum72
            ELSEIF ( local==19 .OR. local==20 ) THEN
               CALL sdum82
            ELSEIF ( local==21 .OR. local==22 ) THEN
               CALL sdum92
            ELSEIF ( local==23 .OR. local==24 ) THEN
               CALL sqdm12
            ELSEIF ( local==25 .OR. local==26 ) THEN
               CALL sqdm22
            ELSEIF ( local==27 .OR. local==28 ) THEN
               CALL squd42
            ELSEIF ( local==29 .OR. local==30 .OR. local==31 .OR. local==32 .OR. local==33 .OR. local==34 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ELSEIF ( local==35 .OR. local==36 ) THEN
            ELSEIF ( local==37 .OR. local==38 ) THEN
            ELSEIF ( local==39 .OR. local==40 ) THEN
               again = .FALSE.
               CALL strax2(Sorc,Tgrid(2))
            ELSEIF ( local==41 .OR. local==42 ) THEN
               again = .FALSE.
               CALL stpax2(Sorc,Tgrid(2))
            ELSEIF ( local==45 .OR. local==46 ) THEN
               CALL strm62(Tgrid(1))
            ELSEIF ( local==47 .OR. local==48 ) THEN
               CALL strp12(Tgrid(1))
            ELSEIF ( local==49 .OR. local==50 ) THEN
               CALL strsl2(Tgrid(1))
            ELSEIF ( local==59 .OR. local==60 ) THEN
               CALL ss2d82(Ieqex,Neqex,Tgrid(1))
            ELSEIF ( local==61 .OR. local==62 ) THEN
               CALL selbo2(Tgrid(1))
            ELSEIF ( local==65 .OR. local==66 ) THEN
!
               CALL stri32
            ELSE
               CALL srod2
            ENDIF
         ENDIF
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
      CASE (14)
         CALL saxif2(kk,Ipart,Branch,Z(Jlist))
         acstic = .TRUE.
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
      CASE (15)
         CALL sslot2(kk,Ipart,Branch,Z(Jlist))
         acstic = .TRUE.
         spag_nextblock_1 = 17
         CYCLE SPAG_DispatchLoop_1
      CASE (16)
         SPAG_Loop_1_1: DO
            CALL sihex2(Eltype-64,Tgrid(1),nip,istrpt,istore)
            ngp = 12*(Eltype-64) - 4
            ngp1 = ngp + 1
            IF ( Eltype==67 ) ngp1 = 21
            nip3 = nip**3
            IF ( istrpt<nip3+1 ) THEN
               spag_nextblock_1 = 35
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( istrpt/=nip3+1 ) THEN
               IF ( istrpt==nip3+1+ngp1 ) istore = 0
               IF ( Ktype==1 ) EXIT SPAG_Loop_1_1
               ngpx = istrpt - (nip3+1)
               nw = 22
               IF ( Eltype==67 ) nw = 23
               ist = nw*(ngpx-1)
               IF ( Ipart>=Ktype ) THEN
!
!     RETRIEVE IMAGINARY PARTS FOR THIS GRID (IHEX ELEMENTS)
!
                  ijk = ist + Icore
                  DO J = 1 , nw
                     Isaves(J) = Z(J+ijk)
                  ENDDO
                  EXIT SPAG_Loop_1_1
               ELSE
!
!     STORE IMARINARY PARTS FOR THIS GRID (IHEX ELEMENTS)
!
                  ijk = ist + Icore
                  DO J = 1 , nw
                     Z(J+ijk) = Bufa(J)
                  ENDDO
                  IF ( istore==0 ) THEN
                     Ivec = Midvec
                     Estawd = oldawd
                     spag_nextblock_1 = 13
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
            ENDIF
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 17
      CASE (17)
!
!     CALL ELEMENT TWO TIMES FOR COMPLEX VECTOR.  IMAGINARY FIRST, REAL
!     SECOND.  CALL ELEMENT ROUTINE TWICE IF AXIC PROBLEM
!     ONCE FOR EACH OF THE 2 VECTORS IN CORE
!
         IF ( .NOT.(Axic .AND. Midvec/=0 .AND. Ipart==1) ) THEN
            IF ( Ipart>=Ktype ) THEN
!
!     SPLIT OUTPUT FROM SECOND CALL FOR ACOUSTIC ELEMENTS
!     AXIF2, AXIF3, AXIF4, SLOT3, OR SLOT4.
!
               IF ( acstic ) THEN
                  IF ( Ipart>=2 ) THEN
                     DO I = 1 , 12
                        Isaves(I) = Bufa(I)
                        Bufa(I) = Bufa(I+12)
                     ENDDO
                  ENDIF
               ENDIF
!
!
!     OUTPUT ONLY FIRST N HARMONICS REQUESTED
!
               IF ( .NOT.Axic ) THEN
                  spag_nextblock_1 = 18
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( nelhar<0 .OR. nelhar>oharms ) THEN
                  spag_nextblock_1 = 34
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( Ipart==2 .AND. Ktype==1 ) THEN
                  spag_nextblock_1 = 34
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         Ivec = Midvec
!
!     FOR CONICAL SHELL ONLY
!
         IF ( .NOT.(Axic .AND. Ktype/=1) ) THEN
            Itemp = 1
            IF ( Sorc==1 ) Itemp = 2
            Sorc = Itemp
         ENDIF
         Estawd = nsesta
         IF ( .NOT.(Axic .AND. Ktype==1) ) THEN
!
!     SAVE IMAGINARY OUTPUTS  (NOT MORE THAN 75 STRESS OR FORCE WORDS)
!
            DO I = 1 , 75
               Isaves(I) = Bufa(I)
               Isavef(I) = Bufb(I)
            ENDDO
         ENDIF
         spag_nextblock_1 = 13
         CYCLE SPAG_DispatchLoop_1
      CASE (18)
!
!     OUTPUT STRESS RESULTS ON OES1 (IF REQUESTED)
!
         IF ( Jstrs==0 .OR. Nwdstr==0 ) THEN
            spag_nextblock_1 = 26
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Ktype==1 ) THEN
            spag_nextblock_1 = 22
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     COMBINE COMPLEX OUTPUT DESIRED PER FORMAT IN COMPLX ARRAY.
!          REAL PARTS ARE IN BUFA   BUFB
!          IMAG PARTS ARE IN ISAVES ISAVEF
!
!
!     COMPLEX STRESSES
!
         iout = 0
         I = nptstr
         spag_nextblock_1 = 19
      CASE (19)
         npt = Complx(I)
         IF ( npt<0 ) THEN
            npt = -npt
            IF ( sphase/=3 ) THEN
               spag_nextblock_1 = 21
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     COMPUTE MAGNITUDE/PHASE
!
            CALL magpha(Bufa(npt),Isaves(npt))
         ELSEIF ( npt==0 ) THEN
!
!     TRANSFER RESULTS TO BUFA
!
            DO I = 1 , iout
               Bufa(I) = Elwork(I)
            ENDDO
            Nwdstr = iout
            spag_nextblock_1 = 22
            CYCLE SPAG_DispatchLoop_1
         ELSE
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 20
      CASE (20)
         iout = iout + 1
         Elwork(iout) = Bufa(npt)
         I = I + 1
         spag_nextblock_1 = 19
         CYCLE SPAG_DispatchLoop_1
      CASE (21)
         IF ( npt<=lstres ) THEN
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         npt = npt - lstres
         iout = iout + 1
         Elwork(iout) = Isaves(npt)
         I = I + 1
         spag_nextblock_1 = 19
         CYCLE SPAG_DispatchLoop_1
      CASE (22)
!
!     WRITE STRESSES
!
!
!     DETERMINE DESTINATION FOR STRESS ENTRY
!
         IF ( Stress==0 ) THEN
            spag_nextblock_1 = 26
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( .NOT.ok2wrt ) THEN
            spag_nextblock_1 = 26
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         id = Bufa(1)
         Bufa(1) = 10*id + Sdest
         IF ( Xsetns<0 ) THEN
         ELSEIF ( Xsetns==0 ) THEN
            Bufa(1) = 10*id
         ELSE
            Ix = Ixsets
            spag_nextblock_1 = 23
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 25
         CYCLE SPAG_DispatchLoop_1
      CASE (23)
         IF ( Ix/=Nxsets ) THEN
            IF ( Z(Ix+1)<=0 ) THEN
               IF ( id>=Z(Ix) .AND. id<=(-Z(Ix+1)) ) THEN
                  spag_nextblock_1 = 25
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               Ix = Ix + 2
               spag_nextblock_1 = 24
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( id==Z(Ix) ) THEN
            spag_nextblock_1 = 25
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Ix = Ix + 1
         spag_nextblock_1 = 24
      CASE (24)
         IF ( Ix<=Nxsets ) THEN
            spag_nextblock_1 = 23
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Bufa(1) = 10*id
         spag_nextblock_1 = 25
      CASE (25)
!
!     NOW WRITE STRESS ENTRY
!
         CALL write(Oes1,Bufa(1),Nwdstr,0)
         Bufa(1) = id
         spag_nextblock_1 = 26
      CASE (26)
!
!     OUTPUT FORCE RESULTS ON OEF1 (IF REQUESTED)
!
         IF ( Jforc==0 .OR. Nwdfor==0 ) THEN
            spag_nextblock_1 = 34
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Ktype==1 ) THEN
            spag_nextblock_1 = 30
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     COMPLEX FORCES
!
         iout = 0
         I = nptfor
         spag_nextblock_1 = 27
      CASE (27)
         npt = Complx(I)
         IF ( npt<0 ) THEN
            npt = -npt
            IF ( fphase/=3 ) THEN
               spag_nextblock_1 = 29
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     COMPUTE MAGNITUDE/PHASE FOR FORCES
!
            CALL magpha(Bufb(npt),Isavef(npt))
         ELSEIF ( npt==0 ) THEN
!
!     TRANSFER RESULTS TO BUFB
!
            DO I = 1 , iout
               Bufb(I) = Elwork(I)
            ENDDO
            Nwdfor = iout
            spag_nextblock_1 = 30
            CYCLE SPAG_DispatchLoop_1
         ELSE
            spag_nextblock_1 = 29
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 28
      CASE (28)
         iout = iout + 1
         Elwork(iout) = Bufb(npt)
         I = I + 1
         spag_nextblock_1 = 27
         CYCLE SPAG_DispatchLoop_1
      CASE (29)
         IF ( npt<=lforce ) THEN
            spag_nextblock_1 = 28
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         npt = npt - lforce
         iout = iout + 1
         Elwork(iout) = Isavef(npt)
         I = I + 1
         spag_nextblock_1 = 27
         CYCLE SPAG_DispatchLoop_1
      CASE (30)
!
!     WRITE FORCES
!
!
!     DETERMINE DESTINATION FOR FORCE ENTRY
!
         IF ( Force==0 ) THEN
            spag_nextblock_1 = 34
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( .NOT.ok2wrt ) THEN
            spag_nextblock_1 = 34
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         id = Bufb(1)
         Bufb(1) = 10*id + Fdest
         IF ( Xsetnf<0 ) THEN
         ELSEIF ( Xsetnf==0 ) THEN
            Bufb(1) = 10*id
         ELSE
            Ix = Ixsetf
            spag_nextblock_1 = 31
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 33
         CYCLE SPAG_DispatchLoop_1
      CASE (31)
         IF ( Ix/=Nxsetf ) THEN
            IF ( Z(Ix+1)<=0 ) THEN
               IF ( id>=Z(Ix) .AND. id<=(-Z(Ix+1)) ) THEN
                  spag_nextblock_1 = 33
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               Ix = Ix + 2
               spag_nextblock_1 = 32
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( id==Z(Ix) ) THEN
            spag_nextblock_1 = 33
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Ix = Ix + 1
         spag_nextblock_1 = 32
      CASE (32)
         IF ( Ix<=Nxsetf ) THEN
            spag_nextblock_1 = 31
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Bufb(1) = 10*id
         spag_nextblock_1 = 33
      CASE (33)
!
!     NOW WRITE FORCE ENTRY
!
         CALL write(Oef1,Bufb(1),Nwdfor,0)
         Bufb(1) = id
         spag_nextblock_1 = 34
      CASE (34)
         IF ( .NOT.(again) ) THEN
            IF ( istore==1 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( Ktype/=1 .OR. (Axic .AND. Midvec/=0) ) Ivec = Isave
            IF ( Axic .AND. Midvec/=0 ) Sorc = isvsrc
            IF ( .NOT.Axic ) THEN
               spag_nextblock_1 = 35
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( nelhar/=Nharms ) THEN
               spag_nextblock_1 = 35
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( Eltype==35 ) CALL scone3(again)
         IF ( Eltype==70 ) CALL strax3(again)
         IF ( Eltype==71 ) CALL stpax3(again)
         nelhar = -1
         spag_nextblock_1 = 18
         CYCLE SPAG_DispatchLoop_1
      CASE (35)
         IF ( Nesta==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Z(Estawd+1)/=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     END OF ESTA FOR CURRENT ELEMENT TYPE
!
 60      IF ( idstrs ) CALL write(Oes1,0,0,1)
         IF ( idforc ) CALL write(Oef1,0,0,1)
         IF ( idlyst ) CALL write(Oes1l,0,0,1)
         IF ( idlyfr ) CALL write(Oef1l,0,0,1)
         IF ( Nesta==0 ) THEN
            CALL read(*80,*120,Esta,Eltype,1,0,Flag)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 36
      CASE (36)
         Estawd = Estawd + 2
         IF ( Estawd<Nesta ) THEN
            Eltype = Z(Estawd)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     END OF ESTA FILE HIT
!
 80      Ivec = Isave
         Icore = jcore
         RETURN
      CASE (37)
!
!     INTERNAL SUBROUTINE FOR WRITING ID RECORDS TO OUTPUT FILES
!
         DO I = 1 , 50
            buf(I) = 0
         ENDDO
!
!     IF THE ID IS BEING WRITTEN TO A FILE WITH COMPLEX DATA,
!     CHANGE THE NUMBER OF WORDS TO REFLECT THE ACTUAL COUNT
!     OF WORDS BEING PUT TOGETHER USING THE STRING OF NUMBERS
!     IN THE 'COMPLX' ARRAY.  (SEE FORTRAN LABELS 651 THRU 654
!     AND 951 THRU 954)
!
         IF ( Ktype/=1 ) THEN
            IF ( jcmplx==0 ) RETURN 1
            jout = 0
            I = jcmplx
            SPAG_Loop_1_2: DO
               ncmplx = Complx(I)
               IF ( ncmplx/=0 ) THEN
                  jout = jout + 1
                  I = I + 1
               ELSE
                  Nwds = jout
                  EXIT SPAG_Loop_1_2
               ENDIF
            ENDDO SPAG_Loop_1_2
         ENDIF
!
!     CHECK FOR VON MISES STRESS REQUEST.  SET WORD 11 IF
!     REQUEST IS FOUND.
!
         IF ( andf(Nstrop,1)/=0 ) buf(11) = 1
!
         IF ( Branch==2 .OR. Branch==8 .OR. Branch==9 ) THEN
!
!     EIGENVALUES OR BUCKLING PHASE 1.
!
            buf(2) = ifltyp + Ktypex
            buf(5) = Z(Jlist)
            buf(6) = Z(Jlist+1)
            buf(7) = Z(Jlist+2)
            buf(8) = 0
         ELSEIF ( Branch==5 ) THEN
!
!     FREQUENCY RESPONSE.
!
            Ix = Icc + Idload
            buf(8) = Z(Ix)
            buf(6) = 0
            buf(7) = 0
            buf(2) = ifltyp + Ktypex
            spag_nextblock_1 = 38
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( Branch==6 ) THEN
!
!     TRANSIENT RESPONSE.
!
            buf(5) = Z(Jlist)
            buf(2) = ifltyp
            Ix = Icc + Idload
            buf(8) = Z(Ix)
            buf(6) = 0
            buf(7) = 0
            spag_nextblock_1 = 38
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     NORMAL STATICS OR DIFF.STIFF. PHASE 0 OR 1 OR BUCKLING PHASE 0.
!
            buf(2) = ifltyp
            Ix = Icc + Isload
            buf(5) = Z(Icc+1)
            buf(6) = 0
            buf(7) = 0
            buf(8) = Z(Ix)
            IF ( Branch==10 ) THEN
               Ix = Icc + Ittl + 84
               Z(Ix) = platit(1)
               Z(Ix+1) = platit(2)
               Z(Ix+2) = platit(3)
               CALL int2al(Ugvvec-1,Z(Ix+3),platit(4))
            ENDIF
         ENDIF
         spag_nextblock_1 = 40
         CYCLE SPAG_DispatchLoop_1
      CASE (38)
!
!     FIRST TIME FOR THIS LOAD VECTOR ONLY - MATCH LIST OF
!
         IF ( Kfrq==0 ) THEN
!
!     USER REQUESTED FREQS WITH ACTUAL FREQS. MARK FOR
!     OUTPUT EACH ACTUAL FREQ WHICH IS CLOSEST TO USER REQUEST.
!
            Kfrq = 1
            Ix = Icc + Ifrout
            Fsetno = Z(Ix)
            IF ( Fsetno>0 ) THEN
               Ix = Icc + Ilsym
               Isetnf = Ix + Z(Ix) + 1
               SPAG_Loop_1_3: DO
                  isetfr = Isetnf + 2
                  nsetfr = Z(Isetnf+1) + isetfr - 1
                  IF ( Z(Isetnf)==Fsetno ) THEN
                     DO I = isetfr , nsetfr
                        K = 0
                        Diff = 1.E25
                        bufr(1) = zz(I)
                        DO J = Ilist , Nlist , 2
                           IF ( Z(J+1)==0 ) THEN
                              Diff1 = abs(zz(J)-bufr(1))
                              IF ( Diff1<Diff ) THEN
                                 Diff = Diff1
                                 K = J
                              ENDIF
                           ENDIF
                        ENDDO
                        IF ( K/=0 ) Z(K+1) = 1
                     ENDDO
                     spag_nextblock_1 = 39
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     Isetnf = nsetfr + 1
                     IF ( Isetnf>=Ivec ) THEN
                        Fsetno = -1
                        EXIT SPAG_Loop_1_3
                     ENDIF
                  ENDIF
               ENDDO SPAG_Loop_1_3
            ENDIF
            DO J = Ilist , Nlist , 2
               Z(J+1) = 1
            ENDDO
         ENDIF
         spag_nextblock_1 = 39
      CASE (39)
!
!     DETERMINE IF CURRENT FREQ IS MARKED FOR OUTPUT.
!
         IF ( Z(Jlist+1)==0 ) GOTO 80
         buf(5) = Z(Jlist)
         spag_nextblock_1 = 40
      CASE (40)
!
!     WRITE ID RECORD ON OUTPUT FILE.
!     (FOR MORE DETAIL, SEE OES1 FILE IN PROGRAMMER MANUAL P.2.3-130)
!
         buf(1) = Device + 10*Branch
         buf(3) = Eltype
!
!     CHECK FOR TRIA1, TRIA2, TRIA3, QUAD1, QUAD2, QUAD4  ELEMENTS
!
         IF ( Eltype==6 .OR. Eltype==17 .OR. Eltype==18 .OR. Eltype==19 .OR. Eltype==64 .OR. Eltype==83 ) THEN
!
!     CHECK FOR STRAIN OPTION
!
            IF ( buf(2)==5 .AND. Strain ) buf(2) = 21
         ENDIF
         buf(4) = Z(Icc+1)
         IF ( Ddrmm ) buf(4) = 9999
         buf(9) = iabs(Z(Irecx+2))
         IF ( buf(9)==1 .AND. Ktype==2 ) buf(9) = 2
         buf(10) = Nwds
         CALL write(Ofile,buf(1),50,0)
         Ix = Icc + Ittl
         CALL write(Ofile,Z(Ix),96,1)
         ilogic(Nlogic) = .TRUE.
         GOTO Iretrn
!
!     ERRORS
!
 100     N = 2
         spag_nextblock_1 = 41
         CYCLE SPAG_DispatchLoop_1
 120     N = 3
         spag_nextblock_1 = 41
      CASE (41)
         CALL mesage(N,File,Nam)
         RETURN 1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE sdr2e

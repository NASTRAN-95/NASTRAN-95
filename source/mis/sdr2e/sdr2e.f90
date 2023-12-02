!*==sdr2e.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sdr2e(*,Ieqex,Neqex)
!
!     THIS ROUTINE WHICH IS CALLED ONLY FROM SDR2D WILL PROCESS THE ESTA
!     FILE ONCE AND OUTPUT FORCE AND OR STRESS RESULTS ON OEF1 AND OR
!     OES1 WHICH ARE OPENED IN SDR2D.
!
   USE c_blank
   USE c_clstrs
   USE c_gpta1
   USE c_isave
   USE c_lhpwx
   USE c_names
   USE c_sdr2c1
   USE c_sdr2de
   USE c_sdr2x1
   USE c_sdr2x2
   USE c_sdr2x4
   USE c_sdr2x7
   USE c_sdr2x8
   USE c_sdr2x9
   USE c_sdrett
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
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
         IF ( stresx==0 .AND. forcex==0 ) RETURN
         heat = .FALSE.
         IF ( itherm/=0 ) heat = .TRUE.
         estawd = iesta
         istore = 0
         ix = icc + harms
         again = .FALSE.
         oharms = z(ix)
         IF ( oharms<0 ) oharms = nharms
         isave = ivec
         isvsrc = sorc
         eltype = z(estawd)
         file = esta
         ix = icc + istr + 2
         sphase = iabs(z(ix))
         ix = icc + ielf + 2
         fphase = iabs(z(ix))
         twotop = alog10(2.0**mtisa)
!
!     POSITION TO THE PROPER THERMAL RECORD IF NECESSARY.
!
         record = .FALSE.
         IF ( tloads/=0 ) THEN
            IF ( tmprec/=0 ) THEN
               CALL rewind(gptt)
               file = gptt
               DO i = 1 , tmprec
                  CALL fwdrec(*100,gptt)
               ENDDO
!
!     READ AND VERIFY SET-ID  (FAILSAFE)
!
               CALL read(*100,*120,gptt,isetid,1,0,flag)
               IF ( tloads/=isetid ) THEN
                  WRITE (opte,99001) sfm , tloads , isetid
99001             FORMAT (A25,' 4019, SDR2E DETECTS INVALID TEMPERATURE DATA FOR ','TEMPERATURE LOAD SET',2I10)
                  CALL mesage(-61,0,0)
               ENDIF
               record = .TRUE.
!
!     INITIALIZE /SDRETT/ VARIABLES
!
               oldeid = 0
               oldel = 0
               eorflg = .FALSE.
               endid = .TRUE.
            ENDIF
         ENDIF
         itemp = tloads
         IF ( nesta==0 ) THEN
            CALL rewind(esta)
            CALL read(*80,*120,esta,eltype,1,0,flag)
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     ELEMENT PARAMETERS FOR NEW ELEMENT TYPE
!
         ielem = (eltype-1)*incr
         ieltyp = eltype
         ipr = iprec
         IF ( ipr/=1 ) ipr = 0
         jltype = 2*eltype - ipr
         jcore = icore
         IF ( heat .AND. eltype/=82 ) THEN
!
            nwdfor = 9
            nwdstr = 0
            nptfor = 0
            nptstr = 0
            nwdsa = 142
!
!     CHOP OFF 483 WORDS FROM OPEN CORE SPACE FOR CIHEX ELEMENTS
!
            IF ( eltype>=65 .AND. eltype<=67 ) icore = icore - 483
         ELSE
!                            FTUBE
            nwdsa = elem(ielem+17)
            nptstr = elem(ielem+20)
            nptfor = elem(ielem+21)
            nwdstr = elem(ielem+18)
            nwdfor = elem(ielem+19)
         ENDIF
!
!
!     SETUP STRESS PRECISION CHECK.
!
         nchk = z(icc+146)
         fnchk = nchk
!
!     SUBCASE ID
!
         isub = z(icc+1)
!
!     DETERMINE LOAD/MODE, EIGENVALUE/FREQ/TIME HEADER
!
         frtmei(1) = 0.
         frtmei(2) = 0.
         IF ( branch==5 .OR. branch==6 ) THEN
!
!     FREQUENCY/TRANSIENT
!
            i = icc + idload
            ild = z(i)
            frtmei(1) = zz(jlist)
         ELSEIF ( branch==2 .OR. branch==8 .OR. branch==9 ) THEN
!
!     EIGENVALUES
!
            ild = z(jlist)
            frtmei(1) = zz(jlist+1)
            frtmei(2) = zz(jlist+2)
            IF ( branch==2 ) THEN
               IF ( zz(jlist+1)>0. ) frtmei(1) = sqrt(zz(jlist+1))/6.2831852
            ENDIF
         ELSE
!
!     STATICS
!
            i = icc + isload
            ild = z(i)
         ENDIF
         lstres = nwdstr
         lforce = nwdfor
         idstrs = .FALSE.
         idforc = .FALSE.
         idlyst = .FALSE.
         idlyfr = .FALSE.
         ok2wrt = .TRUE.
         IF ( ktype==1 .OR. nptstr/=0 .OR. nptfor/=0 ) THEN
            IF ( nwdstr+nwdfor>0 ) THEN
!
!     OK SOME STRESS AND OR FORCE REQUESTS EXIST FOR THIS ELEMENT TYPE.
!     PROCESS INDIVIDUAL ELEMENTS REQUESTED
!
               IF ( nesta/=0 ) THEN
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( nwdsa<=icore ) THEN
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
!
!
!     INSUFFICIENT CORE TO HOLD ESTA FOR 1 ELEMENT OF CURRENT ELEMENT
!     TYPE TRY PROCESSING THE OTHER ELEMENT TYPES IN AVAILABLE CORE.
!
               CALL mesage(8,0,nam(1))
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     NO STRESS OR FORCE WORDS POSSIBLE FOR THIS ELEMENT TYPE IF FALL
!     HERE
!
         IF ( nesta/=0 ) THEN
            DO
!
!     FIND END OF CURRENT ELEMEMT TYPE LIST IN CORE
!
               estawd = estawd + nwdsa
               IF ( z(estawd+1)==0 ) THEN
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
         CALL fwdrec(*100,esta)
         CALL read(*80,*120,esta,eltype,1,0,flag)
         spag_nextblock_1 = 2
      CASE (4)
!
         CALL read(*100,*60,esta,z(iesta),nwdsa,0,flag)
         estawd = iesta - 1
         spag_nextblock_1 = 5
      CASE (5)
!
!     DETERMINE IF THIS PARTICULAR ELEMENT OF THE CURRENT ELEMENT TYPE
!     HAS A STRESS OR FORCE REQUEST IN THE CURRENT CASE CONTROL RECORD.
!
         elemid = z(estawd+1)
!
!     THE FOLLOWING CODE (THRU 93) IS FOR THE COMPLEX ANALYSIS OF IHEX
!     ELEMENTS ONLY (ELEM. TYPES 65,66,67)
!
         IF ( ktype==2 .AND. eltype>=65 .AND. eltype<=67 ) THEN
            IF ( ipart==2 .AND. istrpt==(nip3+ngp1+1) ) THEN
!
!     DONE FOR THIS IHEX ELEMENT, RESET CHECKING VARIABLES
!
               ipart = 0
               ielold = 0
               ielchk = 0
!
!     FIRST INTEGRATION POINT FOR IMAGINARY RETULS FOR THIS IHEX ELEMENT
!     SAVE ELEMENT ID AND CURRENT ESTAWD
!
            ELSEIF ( ipart/=1 .OR. istrpt/=1 ) THEN
!
!     FIRST INTEGRATION POINT FOR REAL RESULTS FOR THIS IHEX ELEMENT,
!     SAVE ELEMENT ID TO CHECK WITH EARLIER ELEMENT ID SAVED ABOVE
!
               IF ( ipart==2 .AND. istrpt==1 ) ielchk = elemid
            ELSE
               ielold = elemid
               oldawd = estawd - nwdsa
            ENDIF
         ENDIF
!
!     END OF SPECIAL TREATMENT FOR IHEX ELEMENT
!
         idelem = elemid
!
!     DECODE ELEMID TO FIND IT IN SET
!
         IF ( axic ) THEN
            nelhar = elemid - (elemid/1000)*1000
            elemid = elemid/1000
         ENDIF
         jstrs = 0
         jforc = 0
         i = isets
         IF ( nwdstr/=0 ) THEN
            IF ( stresx<0 ) THEN
               jstrs = 1
            ELSEIF ( stresx/=0 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 8
      CASE (6)
         IF ( i/=nsets ) THEN
            IF ( z(i+1)<=0 ) THEN
               i = i + 1
               IF ( elemid<z(i-1) .OR. elemid>-z(i) ) THEN
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               jstrs = 1
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( elemid==z(i) ) THEN
            jstrs = 1
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         i = i + 1
         IF ( i<=nsets ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 8
      CASE (8)
         i = isetf
         IF ( nwdfor/=0 ) THEN
            IF ( forcex<0 ) THEN
               jforc = 1
            ELSEIF ( forcex/=0 ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         spag_nextblock_1 = 11
      CASE (9)
         IF ( i/=nsetf ) THEN
            IF ( z(i+1)<=0 ) THEN
               i = i + 1
               IF ( elemid<z(i-1) .OR. elemid>-z(i) ) THEN
                  spag_nextblock_1 = 10
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               jforc = 1
               spag_nextblock_1 = 11
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( elemid==z(i) ) THEN
            jforc = 1
            spag_nextblock_1 = 11
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 10
      CASE (10)
         i = i + 1
         IF ( i<=nsetf ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 11
      CASE (11)
         jany = jstrs + jforc
         IF ( jany==0 ) THEN
            IF ( nesta==0 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            estawd = estawd + nwdsa
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
            IF ( tloads==0 ) THEN
!
!     NORMALLY TGRID(1) WILL CONTAIN THE AVERAGE ELEMENT TEMPERATUE
!     AND IF GRID POINT TEMPERATURES ARE RETURNED THEY WILL BEGIN
!     IN TGRID(2).
!
               jtemp = -1
            ELSE
               n = elem(ielem+10)
!
!     IF NEW ELEMENTS ARE ADDED THAT HAVE SPECIAL BENDING THERMAL DATA
!     POSSIBLE THEN THE FOLLOWING TEST SHOULD BE EXPANDED TO INCLUDE
!     THEIR ELEMENT TYPE SO AS TO RECEIVE ZEROS AND ONLY THE AVERAGE
!     TEMPERATURE RATHER THAN SIMULATED GRID POINT TEMPERATURES IN THE
!     ABSENCE OF ANY USER SPECIFIED DATA.
!
               IF ( ieltyp==34 .OR. ieltyp==6 .OR. ieltyp==7 .OR. ieltyp==8 .OR. ieltyp==15 .OR. ieltyp==17 .OR. ieltyp==18 .OR.    &
                  & ieltyp==19 ) n = 0
               IF ( ieltyp==74 .OR. ieltyp==75 ) n = 0
               IF ( ieltyp==64 .OR. ieltyp==83 ) n = 0
!
!
               CALL sdretd(idelem,tgrid,n)
!
!     SET THE AVERAGE ELEMENT TEMPERATURE CELL.
!
               temp = tgrid(1)
            ENDIF
!
!     ELEMENT DEFORMATION
!
            deform = 0.0
            IF ( eldef/=0 ) THEN
               DO i = idef , ndef , 2
                  IF ( z(i)==elemid ) GOTO 10
               ENDDO
            ENDIF
            spag_nextblock_1 = 12
            CYCLE SPAG_DispatchLoop_1
 10         deform = zz(i+1)
         ENDIF
         spag_nextblock_1 = 12
      CASE (12)
!
!     WRITE ID FOR STRESSES IF NOT YET WRITTEN FOR THIS ELEMENT TYPE.
!
         IF ( stress/=0 .AND. nwdstr/=0 .AND. jstrs/=0 ) THEN
            IF ( comps==-1 .AND. nstrop>1 ) THEN
!
               IF ( .NOT.(idlyst) ) THEN
                  nlogic = 3
                  ofile = oes1l
                  device = sdest
                  ifltyp = 22
                  irecx = icc + istr
                  nwds = 10
                  jcmplx = 0
                  ok2wrt = .FALSE.
                  ASSIGN 20 TO iretrn
                  spag_nextblock_1 = 37
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( .NOT.(idstrs) ) THEN
               nlogic = 1
               ofile = oes1
               device = sdest
               iseq = 4
               ifltyp = dtype(iseq)
               irecx = icc + istr
               nwds = nwdstr
               jcmplx = nptstr
               ASSIGN 20 TO iretrn
               spag_nextblock_1 = 37
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     WRITE ID FOR FORCES IF NOT YET WRITTEN FOR THIS ELEMENT TYPE.
!
 20      IF ( force/=0 .AND. nwdfor/=0 .AND. jforc/=0 ) THEN
            IF ( comps==-1 .AND. nstrop>1 .AND. stress/=0 ) THEN
!
               IF ( .NOT.(idlyfr) ) THEN
                  nlogic = 4
                  ofile = oef1l
                  device = fdest
                  ifltyp = 23
                  irecx = icc + ielf
                  nwds = 9
                  jcmplx = 0
                  ok2wrt = .FALSE.
                  ASSIGN 40 TO iretrn
                  spag_nextblock_1 = 37
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ELSEIF ( .NOT.(idforc) ) THEN
               nlogic = 2
               ofile = oef1
               device = fdest
               iseq = 5
               ifltyp = dtype(iseq)
               irecx = icc + ielf
               nwds = nwdfor
               jcmplx = nptfor
               ASSIGN 40 TO iretrn
               spag_nextblock_1 = 37
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
!
!     MOVE ESTA DATA INTO /SDR2X7/
!
 40      nsesta = estawd
         IF ( ielchk==0 .OR. ipart<2 .OR. ielchk/=ielold ) THEN
            ipart = 0
         ELSE
            ipart = 1
         ENDIF
         spag_nextblock_1 = 13
      CASE (13)
         ipart = ipart + 1
         DO i = 1 , nwdsa
            estawd = estawd + 1
            elesta(i) = z(estawd)
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
                  k = 4
                  CALL spanl2(k)
               ELSEIF ( jltype==9 .OR. jltype==10 ) THEN
                  k = 5
                  CALL spanl2(k)
               ELSEIF ( jltype==11 .OR. jltype==12 .OR. jltype==33 .OR. jltype==34 ) THEN
                  k = 3
                  CALL strqd2(k,tgrid(1))
               ELSEIF ( jltype==13 .OR. jltype==14 ) THEN
                  k = 0
                  CALL sbspl2(k,tgrid(1))
               ELSEIF ( jltype==15 .OR. jltype==16 ) THEN
                  k = 3
                  CALL sbspl2(k,tgrid(1))
               ELSEIF ( jltype==17 .OR. jltype==18 ) THEN
                  k = 1
                  CALL stqme2(k)
               ELSEIF ( jltype==21 .OR. jltype==22 .OR. jltype==23 .OR. jltype==24 .OR. jltype==25 .OR. jltype==26 .OR.             &
                      & jltype==27 .OR. jltype==28 ) THEN
                  CALL selas2
               ELSEIF ( jltype==29 .OR. jltype==30 ) THEN
                  k = 4
                  CALL sbspl2(k,tgrid(1))
               ELSEIF ( jltype==31 .OR. jltype==32 ) THEN
                  k = 2
                  CALL stqme2(k)
               ELSEIF ( jltype==35 .OR. jltype==36 .OR. jltype==37 .OR. jltype==38 ) THEN
                  k = 4
                  CALL strqd2(k,tgrid(1))
               ELSEIF ( jltype==67 .OR. jltype==68 ) THEN
                  CALL sbar2(tgrid(1))
               ELSEIF ( jltype==69 .OR. jltype==70 ) THEN
                  again = .FALSE.
                  CALL scone2(sorc)
               ELSEIF ( jltype==71 .OR. jltype==72 ) THEN
                  CALL strir2(tgrid(2))
               ELSEIF ( jltype==73 .OR. jltype==74 ) THEN
                  CALL strap2(tgrid(2))
               ELSEIF ( jltype==75 .OR. jltype==76 ) THEN
                  CALL stord2(tgrid(2))
               ELSEIF ( jltype==77 .OR. jltype==78 ) THEN
                  CALL ssold2(1,tgrid(2))
               ELSEIF ( jltype==79 .OR. jltype==80 ) THEN
                  CALL ssold2(2,tgrid(2))
               ELSEIF ( jltype==81 .OR. jltype==82 ) THEN
                  CALL ssold2(3,tgrid(2))
               ELSEIF ( jltype==83 .OR. jltype==84 ) THEN
                  CALL ssold2(4,tgrid(2))
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
               CALL strax2(sorc,tgrid(2))
            ELSEIF ( local==41 .OR. local==42 ) THEN
               again = .FALSE.
               CALL stpax2(sorc,tgrid(2))
            ELSEIF ( local==45 .OR. local==46 ) THEN
               CALL strm62(tgrid(1))
            ELSEIF ( local==47 .OR. local==48 ) THEN
               CALL strp12(tgrid(1))
            ELSEIF ( local==49 .OR. local==50 ) THEN
               CALL strsl2(tgrid(1))
            ELSEIF ( local==59 .OR. local==60 ) THEN
               CALL ss2d82(Ieqex,Neqex,tgrid(1))
            ELSEIF ( local==61 .OR. local==62 ) THEN
               CALL selbo2(tgrid(1))
            ELSEIF ( local==65 .OR. local==66 ) THEN
!
               CALL stri32
            ELSE
               CALL srod2
            ENDIF
         ENDIF
         spag_nextblock_1 = 17
      CASE (14)
         CALL saxif2(kk,ipart,branch,z(jlist))
         acstic = .TRUE.
         spag_nextblock_1 = 17
      CASE (15)
         CALL sslot2(kk,ipart,branch,z(jlist))
         acstic = .TRUE.
         spag_nextblock_1 = 17
      CASE (16)
         SPAG_Loop_1_1: DO
            CALL sihex2(eltype-64,tgrid(1),nip,istrpt,istore)
            ngp = 12*(eltype-64) - 4
            ngp1 = ngp + 1
            IF ( eltype==67 ) ngp1 = 21
            nip3 = nip**3
            IF ( istrpt<nip3+1 ) THEN
               spag_nextblock_1 = 35
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( istrpt/=nip3+1 ) THEN
               IF ( istrpt==nip3+1+ngp1 ) istore = 0
               IF ( ktype==1 ) EXIT SPAG_Loop_1_1
               ngpx = istrpt - (nip3+1)
               nw = 22
               IF ( eltype==67 ) nw = 23
               ist = nw*(ngpx-1)
               IF ( ipart>=ktype ) THEN
!
!     RETRIEVE IMAGINARY PARTS FOR THIS GRID (IHEX ELEMENTS)
!
                  ijk = ist + icore
                  DO j = 1 , nw
                     isaves(j) = z(j+ijk)
                  ENDDO
                  EXIT SPAG_Loop_1_1
               ELSE
!
!     STORE IMARINARY PARTS FOR THIS GRID (IHEX ELEMENTS)
!
                  ijk = ist + icore
                  DO j = 1 , nw
                     z(j+ijk) = bufa(j)
                  ENDDO
                  IF ( istore==0 ) THEN
                     ivec = midvec
                     estawd = oldawd
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
         IF ( .NOT.(axic .AND. midvec/=0 .AND. ipart==1) ) THEN
            IF ( ipart>=ktype ) THEN
!
!     SPLIT OUTPUT FROM SECOND CALL FOR ACOUSTIC ELEMENTS
!     AXIF2, AXIF3, AXIF4, SLOT3, OR SLOT4.
!
               IF ( acstic ) THEN
                  IF ( ipart>=2 ) THEN
                     DO i = 1 , 12
                        isaves(i) = bufa(i)
                        bufa(i) = bufa(i+12)
                     ENDDO
                  ENDIF
               ENDIF
!
!
!     OUTPUT ONLY FIRST N HARMONICS REQUESTED
!
               IF ( .NOT.axic ) THEN
                  spag_nextblock_1 = 18
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( nelhar<0 .OR. nelhar>oharms ) THEN
                  spag_nextblock_1 = 34
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               IF ( ipart==2 .AND. ktype==1 ) THEN
                  spag_nextblock_1 = 34
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               spag_nextblock_1 = 18
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         ivec = midvec
!
!     FOR CONICAL SHELL ONLY
!
         IF ( .NOT.(axic .AND. ktype/=1) ) THEN
            itemp = 1
            IF ( sorc==1 ) itemp = 2
            sorc = itemp
         ENDIF
         estawd = nsesta
         IF ( .NOT.(axic .AND. ktype==1) ) THEN
!
!     SAVE IMAGINARY OUTPUTS  (NOT MORE THAN 75 STRESS OR FORCE WORDS)
!
            DO i = 1 , 75
               isaves(i) = bufa(i)
               isavef(i) = bufb(i)
            ENDDO
         ENDIF
         spag_nextblock_1 = 13
      CASE (18)
!
!     OUTPUT STRESS RESULTS ON OES1 (IF REQUESTED)
!
         IF ( jstrs==0 .OR. nwdstr==0 ) THEN
            spag_nextblock_1 = 26
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ktype==1 ) THEN
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
         i = nptstr
         spag_nextblock_1 = 19
      CASE (19)
         npt = complx(i)
         IF ( npt<0 ) THEN
            npt = -npt
            IF ( sphase/=3 ) THEN
               spag_nextblock_1 = 21
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     COMPUTE MAGNITUDE/PHASE
!
            CALL magpha(bufa(npt),isaves(npt))
         ELSEIF ( npt==0 ) THEN
!
!     TRANSFER RESULTS TO BUFA
!
            DO i = 1 , iout
               bufa(i) = elwork(i)
            ENDDO
            nwdstr = iout
            spag_nextblock_1 = 22
            CYCLE SPAG_DispatchLoop_1
         ELSE
            spag_nextblock_1 = 21
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 20
      CASE (20)
         iout = iout + 1
         elwork(iout) = bufa(npt)
         i = i + 1
         spag_nextblock_1 = 19
      CASE (21)
         IF ( npt<=lstres ) THEN
            spag_nextblock_1 = 20
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         npt = npt - lstres
         iout = iout + 1
         elwork(iout) = isaves(npt)
         i = i + 1
         spag_nextblock_1 = 19
      CASE (22)
!
!     WRITE STRESSES
!
!
!     DETERMINE DESTINATION FOR STRESS ENTRY
!
         IF ( stress==0 ) THEN
            spag_nextblock_1 = 26
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( .NOT.ok2wrt ) THEN
            spag_nextblock_1 = 26
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         id = bufa(1)
         bufa(1) = 10*id + sdest
         IF ( xsetns<0 ) THEN
         ELSEIF ( xsetns==0 ) THEN
            bufa(1) = 10*id
         ELSE
            ix = ixsets
            spag_nextblock_1 = 23
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 25
      CASE (23)
         IF ( ix/=nxsets ) THEN
            IF ( z(ix+1)<=0 ) THEN
               IF ( id>=z(ix) .AND. id<=(-z(ix+1)) ) THEN
                  spag_nextblock_1 = 25
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ix = ix + 2
               spag_nextblock_1 = 24
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( id==z(ix) ) THEN
            spag_nextblock_1 = 25
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ix = ix + 1
         spag_nextblock_1 = 24
      CASE (24)
         IF ( ix<=nxsets ) THEN
            spag_nextblock_1 = 23
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         bufa(1) = 10*id
         spag_nextblock_1 = 25
      CASE (25)
!
!     NOW WRITE STRESS ENTRY
!
         CALL write(oes1,bufa(1),nwdstr,0)
         bufa(1) = id
         spag_nextblock_1 = 26
      CASE (26)
!
!     OUTPUT FORCE RESULTS ON OEF1 (IF REQUESTED)
!
         IF ( jforc==0 .OR. nwdfor==0 ) THEN
            spag_nextblock_1 = 34
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ktype==1 ) THEN
            spag_nextblock_1 = 30
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     COMPLEX FORCES
!
         iout = 0
         i = nptfor
         spag_nextblock_1 = 27
      CASE (27)
         npt = complx(i)
         IF ( npt<0 ) THEN
            npt = -npt
            IF ( fphase/=3 ) THEN
               spag_nextblock_1 = 29
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     COMPUTE MAGNITUDE/PHASE FOR FORCES
!
            CALL magpha(bufb(npt),isavef(npt))
         ELSEIF ( npt==0 ) THEN
!
!     TRANSFER RESULTS TO BUFB
!
            DO i = 1 , iout
               bufb(i) = elwork(i)
            ENDDO
            nwdfor = iout
            spag_nextblock_1 = 30
            CYCLE SPAG_DispatchLoop_1
         ELSE
            spag_nextblock_1 = 29
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 28
      CASE (28)
         iout = iout + 1
         elwork(iout) = bufb(npt)
         i = i + 1
         spag_nextblock_1 = 27
      CASE (29)
         IF ( npt<=lforce ) THEN
            spag_nextblock_1 = 28
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         npt = npt - lforce
         iout = iout + 1
         elwork(iout) = isavef(npt)
         i = i + 1
         spag_nextblock_1 = 27
      CASE (30)
!
!     WRITE FORCES
!
!
!     DETERMINE DESTINATION FOR FORCE ENTRY
!
         IF ( force==0 ) THEN
            spag_nextblock_1 = 34
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( .NOT.ok2wrt ) THEN
            spag_nextblock_1 = 34
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         id = bufb(1)
         bufb(1) = 10*id + fdest
         IF ( xsetnf<0 ) THEN
         ELSEIF ( xsetnf==0 ) THEN
            bufb(1) = 10*id
         ELSE
            ix = ixsetf
            spag_nextblock_1 = 31
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 33
      CASE (31)
         IF ( ix/=nxsetf ) THEN
            IF ( z(ix+1)<=0 ) THEN
               IF ( id>=z(ix) .AND. id<=(-z(ix+1)) ) THEN
                  spag_nextblock_1 = 33
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               ix = ix + 2
               spag_nextblock_1 = 32
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( id==z(ix) ) THEN
            spag_nextblock_1 = 33
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ix = ix + 1
         spag_nextblock_1 = 32
      CASE (32)
         IF ( ix<=nxsetf ) THEN
            spag_nextblock_1 = 31
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         bufb(1) = 10*id
         spag_nextblock_1 = 33
      CASE (33)
!
!     NOW WRITE FORCE ENTRY
!
         CALL write(oef1,bufb(1),nwdfor,0)
         bufb(1) = id
         spag_nextblock_1 = 34
      CASE (34)
         IF ( .NOT.(again) ) THEN
            IF ( istore==1 ) THEN
               spag_nextblock_1 = 16
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( ktype/=1 .OR. (axic .AND. midvec/=0) ) ivec = isave
            IF ( axic .AND. midvec/=0 ) sorc = isvsrc
            IF ( .NOT.axic ) THEN
               spag_nextblock_1 = 35
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( nelhar/=nharms ) THEN
               spag_nextblock_1 = 35
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( eltype==35 ) CALL scone3(again)
         IF ( eltype==70 ) CALL strax3(again)
         IF ( eltype==71 ) CALL stpax3(again)
         nelhar = -1
         spag_nextblock_1 = 18
      CASE (35)
         IF ( nesta==0 ) THEN
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( z(estawd+1)/=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     END OF ESTA FOR CURRENT ELEMENT TYPE
!
 60      IF ( idstrs ) CALL write(oes1,0,0,1)
         IF ( idforc ) CALL write(oef1,0,0,1)
         IF ( idlyst ) CALL write(oes1l,0,0,1)
         IF ( idlyfr ) CALL write(oef1l,0,0,1)
         IF ( nesta==0 ) THEN
            CALL read(*80,*120,esta,eltype,1,0,flag)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 36
      CASE (36)
         estawd = estawd + 2
         IF ( estawd<nesta ) THEN
            eltype = z(estawd)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     END OF ESTA FILE HIT
!
 80      ivec = isave
         icore = jcore
         RETURN
      CASE (37)
!
!     INTERNAL SUBROUTINE FOR WRITING ID RECORDS TO OUTPUT FILES
!
         DO i = 1 , 50
            buf(i) = 0
         ENDDO
!
!     IF THE ID IS BEING WRITTEN TO A FILE WITH COMPLEX DATA,
!     CHANGE THE NUMBER OF WORDS TO REFLECT THE ACTUAL COUNT
!     OF WORDS BEING PUT TOGETHER USING THE STRING OF NUMBERS
!     IN THE 'COMPLX' ARRAY.  (SEE FORTRAN LABELS 651 THRU 654
!     AND 951 THRU 954)
!
         IF ( ktype/=1 ) THEN
            IF ( jcmplx==0 ) RETURN 1
            jout = 0
            i = jcmplx
            SPAG_Loop_1_2: DO
               ncmplx = complx(i)
               IF ( ncmplx/=0 ) THEN
                  jout = jout + 1
                  i = i + 1
               ELSE
                  nwds = jout
                  EXIT SPAG_Loop_1_2
               ENDIF
            ENDDO SPAG_Loop_1_2
         ENDIF
!
!     CHECK FOR VON MISES STRESS REQUEST.  SET WORD 11 IF
!     REQUEST IS FOUND.
!
         IF ( andf(nstrop,1)/=0 ) buf(11) = 1
!
         IF ( branch==2 .OR. branch==8 .OR. branch==9 ) THEN
!
!     EIGENVALUES OR BUCKLING PHASE 1.
!
            buf(2) = ifltyp + ktypex
            buf(5) = z(jlist)
            buf(6) = z(jlist+1)
            buf(7) = z(jlist+2)
            buf(8) = 0
         ELSEIF ( branch==5 ) THEN
!
!     FREQUENCY RESPONSE.
!
            ix = icc + idload
            buf(8) = z(ix)
            buf(6) = 0
            buf(7) = 0
            buf(2) = ifltyp + ktypex
            spag_nextblock_1 = 38
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( branch==6 ) THEN
!
!     TRANSIENT RESPONSE.
!
            buf(5) = z(jlist)
            buf(2) = ifltyp
            ix = icc + idload
            buf(8) = z(ix)
            buf(6) = 0
            buf(7) = 0
            spag_nextblock_1 = 38
            CYCLE SPAG_DispatchLoop_1
         ELSE
!
!     NORMAL STATICS OR DIFF.STIFF. PHASE 0 OR 1 OR BUCKLING PHASE 0.
!
            buf(2) = ifltyp
            ix = icc + isload
            buf(5) = z(icc+1)
            buf(6) = 0
            buf(7) = 0
            buf(8) = z(ix)
            IF ( branch==10 ) THEN
               ix = icc + ittl + 84
               z(ix) = platit(1)
               z(ix+1) = platit(2)
               z(ix+2) = platit(3)
               CALL int2al(ugvvec-1,z(ix+3),platit(4))
            ENDIF
         ENDIF
         spag_nextblock_1 = 40
      CASE (38)
!
!     FIRST TIME FOR THIS LOAD VECTOR ONLY - MATCH LIST OF
!
         IF ( kfrq==0 ) THEN
!
!     USER REQUESTED FREQS WITH ACTUAL FREQS. MARK FOR
!     OUTPUT EACH ACTUAL FREQ WHICH IS CLOSEST TO USER REQUEST.
!
            kfrq = 1
            ix = icc + ifrout
            fsetno = z(ix)
            IF ( fsetno>0 ) THEN
               ix = icc + ilsym
               isetnf = ix + z(ix) + 1
               SPAG_Loop_1_3: DO
                  isetfr = isetnf + 2
                  nsetfr = z(isetnf+1) + isetfr - 1
                  IF ( z(isetnf)==fsetno ) THEN
                     DO i = isetfr , nsetfr
                        k = 0
                        diff = 1.E25
                        bufr(1) = zz(i)
                        DO j = ilist , nlist , 2
                           IF ( z(j+1)==0 ) THEN
                              diff1 = abs(zz(j)-bufr(1))
                              IF ( diff1<diff ) THEN
                                 diff = diff1
                                 k = j
                              ENDIF
                           ENDIF
                        ENDDO
                        IF ( k/=0 ) z(k+1) = 1
                     ENDDO
                     spag_nextblock_1 = 39
                     CYCLE SPAG_DispatchLoop_1
                  ELSE
                     isetnf = nsetfr + 1
                     IF ( isetnf>=ivec ) THEN
                        fsetno = -1
                        EXIT SPAG_Loop_1_3
                     ENDIF
                  ENDIF
               ENDDO SPAG_Loop_1_3
            ENDIF
            DO j = ilist , nlist , 2
               z(j+1) = 1
            ENDDO
         ENDIF
         spag_nextblock_1 = 39
      CASE (39)
!
!     DETERMINE IF CURRENT FREQ IS MARKED FOR OUTPUT.
!
         IF ( z(jlist+1)==0 ) GOTO 80
         buf(5) = z(jlist)
         spag_nextblock_1 = 40
      CASE (40)
!
!     WRITE ID RECORD ON OUTPUT FILE.
!     (FOR MORE DETAIL, SEE OES1 FILE IN PROGRAMMER MANUAL P.2.3-130)
!
         buf(1) = device + 10*branch
         buf(3) = eltype
!
!     CHECK FOR TRIA1, TRIA2, TRIA3, QUAD1, QUAD2, QUAD4  ELEMENTS
!
         IF ( eltype==6 .OR. eltype==17 .OR. eltype==18 .OR. eltype==19 .OR. eltype==64 .OR. eltype==83 ) THEN
!
!     CHECK FOR STRAIN OPTION
!
            IF ( buf(2)==5 .AND. strain ) buf(2) = 21
         ENDIF
         buf(4) = z(icc+1)
         IF ( ddrmm ) buf(4) = 9999
         buf(9) = iabs(z(irecx+2))
         IF ( buf(9)==1 .AND. ktype==2 ) buf(9) = 2
         buf(10) = nwds
         CALL write(ofile,buf(1),50,0)
         ix = icc + ittl
         CALL write(ofile,z(ix),96,1)
         ilogic(nlogic) = .TRUE.
         GOTO iretrn
!
!     ERRORS
!
 100     n = 2
         spag_nextblock_1 = 41
         CYCLE SPAG_DispatchLoop_1
 120     n = 3
         spag_nextblock_1 = 41
      CASE (41)
         CALL mesage(n,file,nam)
         RETURN 1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE sdr2e

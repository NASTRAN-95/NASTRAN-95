!*==edtl.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE edtl(Nedt,Ilist,Pg)
!
!     THIS SUBROUTINE COMPUTES THE ELEMENT TEMPERATURE AND ENFORCED
!     DEFORMATION LOADS
!
   USE c_blank
   USE c_compst
   USE c_fpt
   USE c_gpta1
   USE c_loadx
   USE c_matin
   USE c_matout
   USE c_packx
   USE c_sgtmpd
   USE c_ssgett
   USE c_ssgwrk
   USE c_system
   USE c_tranx
   USE c_trimex
   USE c_xcstm
   USE c_xmssg
   USE c_zzzzzz
   USE C_BLANK
   USE C_COMPST
   USE C_FPT
   USE C_GPTA1
   USE C_LOADX
   USE C_MATIN
   USE C_MATOUT
   USE C_PACKX
   USE C_SGTMPD
   USE C_SSGETT
   USE C_SSGWRK
   USE C_SYSTEM
   USE C_TRANX
   USE C_TRIMEX
   USE C_XCSTM
   USE C_XMSSG
   USE C_ZZZZZZ
   IMPLICIT NONE
   INTEGER Alpha , Comps , Costh , Cstm , Dit , Dum(300) , E1 , Ecpt , Edt , Eltype , G , Ge , Gptt , icheck , Icm , Idefm , Ideft ,&
         & Idum1(14) , Iec , iflag , Igptt , Ii , Impt , Incr , Incur , Inflag , Iparam , Ipcmp , Ipcmp1 , Ipcmp2 , iprec , Itemp , &
         & ithrml , Itya , Ityb , Jj , Ksystm(64) , Last , Lcare , Lcore , Matid , Mecpt(200) , Mpt , N(3) , Ne(1) , Nelems ,       &
         & Ngptt , Nn(3) , Nnn , Npcmp , Npcmp1 , Npcmp2 , Nrowsp , Nsil , Nstart , Nu , Oldel , outpt , Rho , Sigmac , Sigmas ,    &
         & Sigmat , Sil , Sinth , Space(10) , Stress , sysbuf , Temp , Tgb(3,3) , To , To1
   LOGICAL Bufflg , Endid , Eorflg , Record
   REAL Core(1) , Ti(33)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Nrowsp , Iparam , Comps
   COMMON /compst/ Ipcmp , Npcmp , Ipcmp1 , Npcmp1 , Ipcmp2 , Npcmp2
   COMMON /fpt   / To , Nsil , Ngptt , Nstart , Lcore
   COMMON /gpta1 / Nelems , Last , Incr , Ne
   COMMON /loadx / Lcare , N , Cstm , Sil , Nnn , Ecpt , Mpt , Gptt , Edt , Impt , Igptt , Iec , Nn , Dit , Icm
   COMMON /matin / Matid , Inflag , Temp , Stress , Sinth , Costh
   COMMON /matout/ E1 , G , Nu , Rho , Alpha , To1 , Ge , Sigmat , Sigmac , Sigmas , Space
   COMMON /packx / Itya , Ityb , Ii , Jj , Incur
   COMMON /sgtmpd/ Ti
   COMMON /ssgett/ Eltype , Oldel , Eorflg , Endid , Bufflg , Itemp , Ideft , Idefm , Record
   COMMON /ssgwrk/ Dum
   COMMON /system/ Ksystm
   COMMON /tranx / Idum1
   COMMON /trimex/ Mecpt
   COMMON /xcstm / Tgb
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zzzzzz/ Core
   INTEGER Nedt , Ntemp
   INTEGER Ilist(1) , Pg(7)
   INTEGER buf1 , buf2 , buf3 , buf4 , buf5 , cbar , conrod , crod , ctube , dummy , flag , i , iddd , idx , iijj , illop , imat ,  &
         & iparm(2) , ipgtt , ipm , ipr , iti , j , jltype , kk , ldefm , local , lpcomp , n1 , name , ncstm , nloop , nmat ,       &
         & noedt , nogptt , npts , ntlist , nwords , pcomp(2) , pcomp1(2) , pcomp2(2) , pcomps , tlist(1080)
   INTEGER korsz
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Outpt) , (Ksystm(55),Iprec) , (Ksystm(56),Ithrml) , (Ti(7),Icheck) , (Ti(6),Iflag)
   DATA iparm , ipgtt/4HEDTL , 4H     , 4HGPTT/
   DATA crod , ctube , conrod , cbar , pcomps/1 , 3 , 10 , 34 , 112/
   DATA pcomp , pcomp1 , pcomp2/5502 , 55 , 5602 , 56 , 5702 , 57/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         Igptt = ipgtt
!
!     CHECK IF HEAT FORMULATION
!
         IF ( ithrml/=0 ) RETURN
!
         Itemp = 0
         Ideft = Nedt
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!
         ENTRY templ(Ntemp,Ilist,Pg)
!     ============================
!
         IF ( ithrml/=0 ) RETURN
         Ideft = 0
         Itemp = Ntemp
         spag_nextblock_1 = 2
      CASE (2)
!
!     START SEARCH POINTERS AT ZERO
!
         Itya = 1
         CALL delset
         Ityb = 1
         ipr = iprec
         IF ( ipr/=1 ) ipr = 0
         Ii = 1
         Jj = Nrowsp
         Incur = 1
         Nnn = 0
         nogptt = 0
         Idum1(1) = 0
         Icm = 1
         noedt = 0
         CALL delset
         lpcomp = 0
!
!     SET CORE SIZE AND BUFFERS
!
         Lcore = korsz(Core) - Nrowsp
         buf1 = Lcore - sysbuf - 2
         buf2 = buf1 - sysbuf - 2
         buf3 = buf2 - sysbuf - 2
         buf4 = buf3 - sysbuf - 2
         buf5 = buf4 - sysbuf - 2
!
!     OPEN FILES--
!
!     READ FILE PCOMPS INTO CORE ONLY IF PARAM COMPS = -1,
!     INDICATING THE PRESENCE OF LAMINATED COMPOSITE ELEMENTS
!
         IF ( Comps/=-1 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
         ipm = pcomps
         CALL preloc(*140,Core(buf2),pcomps)
!
         Ipcmp = Nrowsp + 1
         Ipcmp1 = Ipcmp
         Npcmp = 0
         Npcmp1 = 0
         Npcmp2 = 0
!
         Lcore = buf5 - Nrowsp - 1
!
!     LOCATE PCOMP DATA AND READ INTO CORE
!
         CALL locate(*40,Core(buf2),pcomp,flag)
!
         CALL read(*240,*20,pcomps,Core(Ipcmp),Lcore,0,Npcmp)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 20      Ipcmp1 = Ipcmp + Npcmp
         Lcore = Lcore - Npcmp
         IF ( Ipcmp1>=Lcore ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     LOCATE PCOMP1 DATA AND READ INTO CORE
!
 40      CALL locate(*80,Core(buf2),pcomp1,flag)
!
         Ipcmp1 = Ipcmp + Npcmp
         CALL read(*100,*60,pcomps,Core(Ipcmp1),Lcore,0,Npcmp1)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
 60      Ipcmp2 = Ipcmp1 + Npcmp1
         Lcore = Lcore - Npcmp1
         IF ( Ipcmp2>=Lcore ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
!     LOCATE PCOMP2 DATA AND READ INTO CORE
!
 80      CALL locate(*100,Core(buf2),pcomp2,flag)
!
         Ipcmp2 = Ipcmp1 + Npcmp1
         CALL read(*100,*100,pcomps,Core(Ipcmp2),Lcore,0,Npcmp2)
         spag_nextblock_1 = 5
         CYCLE SPAG_DispatchLoop_1
!
 100     lpcomp = Npcmp + Npcmp1 + Npcmp2
!
         Lcore = Lcore - Npcmp2
         IF ( Lcore<=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
!
         CALL close(pcomps,1)
         spag_nextblock_1 = 3
      CASE (3)
!
!
         CALL gopen(Ecpt,Core(buf2),0)
         IF ( Itemp/=0 ) THEN
            ipm = Gptt
            CALL open(*140,Gptt,Core(buf3),0)
!
!     BRING IN MAT ETC
!
            CALL read(*240,*180,Gptt,tlist(1),-2,0,ntlist)
            CALL read(*240,*120,Gptt,tlist(1),1080,1,ntlist)
            WRITE (outpt,99001) Ufm
99001       FORMAT (A23,' 4013, PROBLEM LIMITATION OF 360 TEMPERATURE SETS ',' HAS BEEN EXCEEDED.')
            n1 = -37
            spag_nextblock_1 = 4
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 120     IF ( Ideft/=0 ) CALL gopen(Edt,Core(buf4),0)
         nloop = Ideft + Itemp
         IF ( Ideft/=0 ) ldefm = 0
!
!     INITIALIZE MATERIAL ROUTINE
!
         imat = Nrowsp + lpcomp
         Lcore = buf5 - imat
         CALL premat(Core(imat+1),Core(imat+1),Core(buf5),Lcore,nmat,Mpt,Dit)
         Nstart = imat + nmat
         Lcore = Lcore - Nstart
         IF ( Lcore<=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( Ideft/=0 ) ldefm = 0
!
         DO illop = 1 , nloop
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
!
                  Idefm = Ilist(illop)
                  IF ( Itemp>0 ) CALL rewind(Gptt)
!
                  IF ( Nnn==1 ) THEN
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
!
!     BRING SIL INTO CORE
!
                  IF ( Lcore>=0 ) THEN
                     CALL gopen(Sil,Core(buf5),0)
                     ipm = Sil
                     CALL read(*240,*122,Sil,Core(Nstart+1),Lcore,1,Nsil)
                  ENDIF
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
 122              CALL close(Sil,1)
                  Lcore = Lcore - Nsil
                  Nstart = Nstart + Nsil
!
!     READ CSTM INTO OPEN CORE AND MAKE INITIAL CALLS TO PRETRD/PRETRS
!
                  IF ( Lcore>=0 ) THEN
                     CALL open(*126,Cstm,Core(buf5),0)
                     Icm = 0
                     CALL skprec(Cstm,1)
                     ipm = Cstm
                     CALL read(*240,*124,Cstm,Core(Nstart+1),Lcore,1,ncstm)
                  ENDIF
                  spag_nextblock_1 = 5
                  CYCLE SPAG_DispatchLoop_1
!
!     FOR THOSE SUBROUTINES WHICH USE BASGLB INSTEAD OF TRANSS/TRANSD,
!     WE NEED TO REPOSITION THE CSTM FILE AND LEAVE THE GINO BUFFER
!     AVAILABLE FOR LATER CALLS TO READ BY SUBROUTINE BASGLB.
!
 124              CALL rewind(Cstm)
                  CALL skprec(Cstm,1)
!
                  CALL pretrd(Core(Nstart+1),ncstm)
                  CALL pretrs(Core(Nstart+1),ncstm)
!
                  Lcore = Lcore - ncstm
                  Nstart = Nstart + ncstm
                  IF ( Lcore<=0 ) THEN
                     spag_nextblock_1 = 5
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
!
 126              Nnn = 1
                  spag_nextblock_2 = 2
               CASE (2)
                  IF ( Itemp<=0 ) THEN
                     spag_nextblock_2 = 4
                     CYCLE SPAG_DispatchLoop_2
                  ENDIF
!
                  DO i = 1 , ntlist , 3
                     IF ( Idefm==tlist(i) ) THEN
                        spag_nextblock_2 = 3
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDDO
!
!     THERMAL LOAD NOT FOUND IN GPTT
!
                  iparm(2) = iparm(1)
                  iparm(1) = Igptt
                  CALL mesage(-32,Idefm,iparm(1))
                  spag_nextblock_2 = 3
               CASE (3)
                  To = tlist(i+1)
                  IF ( tlist(i+2)==0 ) THEN
!
!     THE GPTT (ELEMENT TEMPERATURE TABLE) IS NOW POSITIONED TO THE
!     TEMPERATURE DATA FOR THE SET REQUESTED.  SUBROUTINE SSGETD WILL
!     READ THE DATA.
!
                     Record = .FALSE.
                     spag_nextblock_2 = 4
                     CYCLE SPAG_DispatchLoop_2
                  ELSE
                     i = tlist(i+2)
                     DO j = 1 , i
                        CALL fwdrec(*160,Gptt)
                     ENDDO
!
!     READ SETID AND VERIFY CORRECT RECORD.  FAILSAFE
!
                     CALL read(*128,*128,Gptt,iddd,1,0,dummy)
                     IF ( iddd==Idefm ) THEN
                        Record = .TRUE.
                        spag_nextblock_2 = 4
                        CYCLE SPAG_DispatchLoop_2
                     ENDIF
                  ENDIF
 128              WRITE (outpt,99002) Idefm
99002             FORMAT (98H0*** SYSTEM FATAL ERROR 4014, ROUTINE EDTL DETECTS BAD DATA ON TEMPERATURE DATA BLOCK FOR SET ID =,I9)
                  n1 = -61
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               CASE (4)
!
                  CALL close(Cstm,1)
                  CALL open(*130,Cstm,Core(buf5),0)
                  CALL skprec(Cstm,1)
                  Icm = 0
 130              DO i = 1 , Nrowsp
                     Core(i) = 0.0
                  ENDDO
!
!     INITIALIZE /SSGETT/ VARIABLES
!
                  Oldel = 0
                  Eorflg = .FALSE.
                  Endid = .TRUE.
                  Bufflg = .FALSE.
!
!     ELEMENT CALL PROCESSING
!
!
!     READ THE ELEMENT TYPE
!
 132              CALL read(*136,*200,Ecpt,Eltype,1,0,flag)
                  IF ( Eltype>=1 .AND. Eltype<=Nelems ) THEN
                     idx = (Eltype-1)*Incr
                     jltype = 2*Eltype - ipr
                     nwords = Ne(idx+12)
                     spag_nextblock_2 = 6
                     CYCLE SPAG_DispatchLoop_2
                  ELSE
                     CALL mesage(-7,0,name)
                  ENDIF
                  spag_nextblock_2 = 5
               CASE (5)
                  WRITE (outpt,99003) Swm , Eltype
99003             FORMAT (A27,' 4015, ELEMENT THERMAL AND DEFORMATION LOADING NOT ','COMPUTED FOR ILLEGAL ELEMENT TYPE',I9,/34X,    &
                         &'IN MODULE SSG1.')
!
!     NO LOAD, SKIP THE ECPT ENTRY ONLY
!
                  CALL fwdrec(*220,Ecpt)
                  GOTO 132
               CASE (6)
                  SPAG_Loop_2_1: DO
!
!     READ AN ENTRY FOR ONE ELEMENT FROM ECPT
!
                     CALL read(*220,*132,Ecpt,Mecpt(1),nwords,0,flag)
                     IF ( Itemp/=0 ) THEN
!
!     THERMAL LOAD
!
!
!     BRANCH TO THE DESIRED ELEMENT TYPE
!
                        local = jltype - 100
                        IF ( local<=0 ) THEN
!
!
!     PAIRED -GO TO- ENTRIES PER ELEMENT SINGLE/DOUBLE PRECISION
!
!             1 CROD      2 CBEAM     3 CTUBE     4 CSHEAR    5 CTWIST
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
!            31 PLOTEL   32 CREACT   33 CQUAD3   34 CBAR     35 CCONE
!
!            36 CTRIARG  37 CTRAPRG  38 CTORDRG  39 CTETRA   40 CWEDGE
!
!            41 CHEXA1   42 CHEXA2   43 CFLUID2  44 CFLUID3  45 CFLUID4
!
!            46 CFLMASS  47 CAXIF2   48 CAXIF3   49 CAXIF4   50 CSLOT3
!
                           IF ( jltype==1 .OR. jltype==2 ) THEN
!
!     ROD
!
                              CALL rod
                           ELSEIF ( jltype==3 .OR. jltype==4 .OR. jltype==63 .OR. jltype==64 .OR. jltype==65 .OR. jltype==66 ) THEN
                              spag_nextblock_2 = 5
                              CYCLE SPAG_DispatchLoop_2
                           ELSEIF ( jltype==5 .OR. jltype==6 ) THEN
                              CALL rod
                           ELSEIF ( jltype==7 .OR. jltype==8 ) THEN
!
!     SHEAR PANEL
!
                              CALL tshear
                           ELSEIF ( jltype==9 .OR. jltype==10 .OR. jltype==21 .OR. jltype==22 .OR. jltype==23 .OR. jltype==24 .OR.  &
                                  & jltype==25 .OR. jltype==26 .OR. jltype==27 .OR. jltype==28 .OR. jltype==39 .OR. jltype==40 .OR. &
                                  & jltype==41 .OR. jltype==42 .OR. jltype==43 .OR. jltype==44 .OR. jltype==45 .OR. jltype==46 .OR. &
                                  & jltype==47 .OR. jltype==48 .OR. jltype==49 .OR. jltype==50 .OR. jltype==51 .OR. jltype==52 .OR. &
                                  & jltype==53 .OR. jltype==54 .OR. jltype==55 .OR. jltype==56 .OR. jltype==57 .OR. jltype==58 .OR. &
                                  & jltype==59 .OR. jltype==60 .OR. jltype==61 .OR. jltype==62 .OR. jltype==85 .OR. jltype==86 .OR. &
                                  & jltype==87 .OR. jltype==88 .OR. jltype==89 .OR. jltype==90 .OR. jltype==91 .OR. jltype==92 .OR. &
                                  & jltype==93 .OR. jltype==94 .OR. jltype==95 .OR. jltype==96 .OR. jltype==97 .OR. jltype==98 .OR. &
                                  & jltype==99 .OR. jltype==100 ) THEN
                              CALL fwdrec(*220,Ecpt)
                              GOTO 132
                           ELSEIF ( jltype==11 .OR. jltype==12 ) THEN
!
!     TRIA1
!
                              kk = 1
                              EXIT SPAG_Loop_2_1
                           ELSEIF ( jltype==13 .OR. jltype==14 ) THEN
!
!     TRBSC
!
                              CALL ssgetd(Mecpt(1),Ti(1),0)
                              CALL trbsc(0,Ti)
                           ELSEIF ( jltype==15 .OR. jltype==16 ) THEN
!
!     TRPLT
!
                              CALL ssgetd(Mecpt(1),Ti(1),0)
                              CALL trplt(Ti)
                           ELSEIF ( jltype==17 .OR. jltype==18 ) THEN
!
!     TRMEM
!
                              CALL ssgetd(Mecpt(1),Ti(1),0)
                              CALL trimem(0,Ti,Core(1))
                           ELSEIF ( jltype==19 .OR. jltype==20 ) THEN
                              CALL rod
                           ELSEIF ( jltype==29 .OR. jltype==30 ) THEN
!
!     QDPLT
!
                              CALL ssgetd(Mecpt(1),Ti(1),0)
                              CALL qdplt(Ti)
                           ELSEIF ( jltype==31 .OR. jltype==32 ) THEN
!
!     QDMEM
!
                              CALL ssgetd(Mecpt(1),Ti(1),0)
                              CALL qdmem(Ti,Core(1))
                           ELSEIF ( jltype==33 .OR. jltype==34 ) THEN
!
!     TRIA2
!
                              kk = 2
                              EXIT SPAG_Loop_2_1
                           ELSEIF ( jltype==35 .OR. jltype==36 ) THEN
!
!     QUAD2
!
                              kk = 4
                              EXIT SPAG_Loop_2_1
                           ELSEIF ( jltype==37 .OR. jltype==38 ) THEN
!
!     QUAD1
!
                              kk = 3
                              EXIT SPAG_Loop_2_1
                           ELSEIF ( jltype==67 .OR. jltype==68 ) THEN
!
!     CONROD
!
!
!     TUBE
!
!
!     BAR
!
                              CALL bar(Core(1),Idefm,Itemp,Ideft)
                           ELSEIF ( jltype==69 .OR. jltype==70 ) THEN
!
!     CONE
!
                              CALL ssgetd(Mecpt(1),Ti(1),2)
                              CALL cone(Ti(2),Core(1))
                           ELSEIF ( jltype==71 .OR. jltype==72 ) THEN
!
!     TRIARG
!
                              CALL ssgetd(Mecpt(1),Ti(1),3)
                              CALL ttrirg(Ti(2),Core(1))
                           ELSEIF ( jltype==73 .OR. jltype==74 ) THEN
!
!     TRAPRG
!
                              CALL ssgetd(Mecpt(1),Ti(1),4)
                              CALL ttrapr(Ti(2),Core(1))
                           ELSEIF ( jltype==75 .OR. jltype==76 ) THEN
!
!     TORDRG
!
                              CALL ssgetd(Mecpt(1),Ti(1),2)
                              CALL ttordr(Ti(2),Core(1))
                           ELSEIF ( jltype==77 .OR. jltype==78 ) THEN
!
!     TETRA
!
                              CALL ssgetd(Mecpt(1),Ti(1),4)
                              CALL tetra(Ti(2),Core(1),0)
                           ELSEIF ( jltype==79 .OR. jltype==80 ) THEN
!
!     WEDGE
!
                              iijj = 1
                              npts = 6
                              spag_nextblock_2 = 7
                              CYCLE SPAG_DispatchLoop_2
                           ELSEIF ( jltype==81 .OR. jltype==82 ) THEN
!
!     HEXA1
!
                              iijj = 2
                              npts = 8
                              spag_nextblock_2 = 7
                              CYCLE SPAG_DispatchLoop_2
                           ELSEIF ( jltype==83 .OR. jltype==84 ) THEN
!
!     HEXA2
!
                              iijj = 3
                              npts = 8
                              spag_nextblock_2 = 7
                              CYCLE SPAG_DispatchLoop_2
                           ELSE
                              GOTO 134
                           ENDIF
                           CYCLE
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
!             71 CTRAPAX  72 CAERO1   73 CTRIM6   74 CTRPLT1  75 CTRSHL
!
!             76 CFHEX1   77 CFHEX2   78 CFTETRA  79 CFWEDGE  80 CIS2D8
!
!             81 CELBOW   82 FTUBE    83 TRIA3
!
 134                    IF ( local==1 .OR. local==2 .OR. local==3 .OR. local==4 .OR. local==63 .OR. local==64 ) THEN
                           CALL fwdrec(*220,Ecpt)
                           GOTO 132
                        ELSEIF ( local==5 .OR. local==6 ) THEN
!
!     DUMMY ELEMENTS
!
                           CALL dum1(Core(1))
                        ELSEIF ( local==7 .OR. local==8 ) THEN
                           CALL dum2(Core(1))
                        ELSEIF ( local==9 .OR. local==10 ) THEN
                           CALL dum3(Core(1))
                        ELSEIF ( local==11 .OR. local==12 ) THEN
                           CALL dum4(Core(1))
                        ELSEIF ( local==13 .OR. local==14 ) THEN
                           CALL dum5(Core(1))
                        ELSEIF ( local==15 .OR. local==16 ) THEN
                           CALL dum6(Core(1))
                        ELSEIF ( local==17 .OR. local==18 ) THEN
                           CALL dum7(Core(1))
                        ELSEIF ( local==19 .OR. local==20 ) THEN
                           CALL dum8(Core(1))
                        ELSEIF ( local==21 .OR. local==22 ) THEN
                           CALL dum9(Core(1))
                        ELSEIF ( local==23 .OR. local==24 ) THEN
!
!     QDMEM1
!
                           CALL ssgetd(Mecpt(1),Ti(1),0)
                           CALL qdmm1(Ti,Core(1))
                        ELSEIF ( local==25 .OR. local==26 ) THEN
!
!     QDMEM2
!
                           CALL ssgetd(Mecpt(1),Ti(1),0)
                           CALL qdmm2(Ti,Core(1))
                        ELSEIF ( local==27 .OR. local==28 ) THEN
!
!     QUAD4
!
                           DO iti = 1 , 7
                              Ti(iti) = 0.0
                           ENDDO
                           CALL ssgetd(Mecpt(1),Ti,4)
                           IF ( ipr/=0 ) CALL tlqd4s
                           IF ( ipr==0 ) CALL tlqd4d
                        ELSEIF ( local==29 .OR. local==30 .OR. local==31 .OR. local==32 .OR. local==33 .OR. local==34 ) THEN
!
!     IHEX1, IHEX2, IHEX3
!
                           npts = 12*(Eltype-64) - 4
                           CALL ssgetd(Mecpt(1),Ti(1),npts)
                           CALL ihex(Ti(1),Core(1),Eltype-64)
                        ELSEIF ( local==35 .OR. local==36 .OR. local==37 .OR. local==38 .OR. local==43 .OR. local==44 .OR.          &
                               & local==51 .OR. local==52 .OR. local==53 .OR. local==54 .OR. local==55 .OR. local==56 .OR.          &
                               & local==57 .OR. local==58 .OR. local==61 .OR. local==62 ) THEN
                           spag_nextblock_2 = 5
                           CYCLE SPAG_DispatchLoop_2
                        ELSEIF ( local==39 .OR. local==40 ) THEN
!
!     TRIAAX
!
                           CALL ssgetd(Mecpt,Ti,3)
                           CALL trttem(Ti(2),Core)
                        ELSEIF ( local==41 .OR. local==42 ) THEN
!
!     TRAPAX
!
                           CALL ssgetd(Mecpt,Ti,4)
                           CALL tpztem(Ti(2),Core)
                        ELSEIF ( local==45 .OR. local==46 ) THEN
!
!     TRIM6
!
                           CALL ssgetd(Mecpt(1),Ti,6)
                           CALL tlodm6(Ti(1))
                        ELSEIF ( local==47 .OR. local==48 ) THEN
!
!     TRPLT1
!
                           CALL ssgetd(Mecpt(1),Ti,0)
                           CALL tlodt1(Ti(1),Ti(1))
                        ELSEIF ( local==49 .OR. local==50 ) THEN
!
!     TRSHL
!
                           CALL ssgetd(Mecpt(1),Ti(1),0)
                           CALL tlodsl(Ti(1),Ti(1))
                        ELSEIF ( local==59 .OR. local==60 ) THEN
!
!     IS2D8
!
                           CALL ssgetd(Mecpt(1),Ti(1),8)
                           CALL tis2d8(Ti(2),Core)
                        ELSEIF ( local==65 .OR. local==66 ) THEN
!
!     TRIA3
!
                           DO iti = 1 , 7
                              Ti(iti) = 0.0
                           ENDDO
                           CALL ssgetd(Mecpt(1),Ti,3)
                           IF ( ipr/=0 ) CALL tltr3s
                           IF ( ipr==0 ) CALL tltr3d
                        ELSE
                           CALL rod
                        ENDIF
                     ELSE
!
!     ELEMENT DEFORMATION LOAD
!
                        IF ( Idefm/=ldefm ) CALL fedtst(Idefm)
                        ldefm = Idefm
                        IF ( Eltype==crod ) THEN
                           CALL rod
                        ELSEIF ( Eltype==ctube ) THEN
                           CALL rod
                        ELSEIF ( Eltype==conrod ) THEN
                           CALL rod
                        ELSEIF ( Eltype==cbar ) THEN
                           CALL bar(Core(1),Idefm,Itemp,Ideft)
                        ELSE
                           CALL fwdrec(*220,Ecpt)
                           GOTO 132
                        ENDIF
                     ENDIF
                  ENDDO SPAG_Loop_2_1
                  CALL ssgetd(Mecpt(1),Ti(1),0)
                  CALL triqd(kk,Ti(1))
                  spag_nextblock_2 = 6
               CASE (7)
                  CALL ssgetd(Mecpt(1),Ti(1),npts)
                  CALL solid(Ti(2),Core(1),iijj)
                  spag_nextblock_2 = 6
                  CYCLE SPAG_DispatchLoop_2
!
!     PACK THE LOAD VECTOR FROM CORE TO OUTPUT DATA BLOCK -PG-
!
 136              CALL pack(Core,Pg(1),Pg)
                  CALL rewind(Ecpt)
                  CALL fwdrec(*220,Ecpt)
                  IF ( Ideft/=0 .AND. Idefm/=0 ) CALL fedted(Idefm)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
!
         ENDDO
!
         IF ( noedt==0 ) CALL close(Edt,1)
         IF ( nogptt==0 ) CALL close(Gptt,1)
         IF ( Icm==0 ) CALL close(Cstm,1)
         CALL close(Ecpt,1)
         RETURN
!
 140     n1 = -1
         spag_nextblock_1 = 4
      CASE (4)
         CALL mesage(n1,ipm,iparm)
 160     ipm = Gptt
         GOTO 240
 180     n1 = -3
         spag_nextblock_1 = 4
      CASE (5)
         n1 = -8
         spag_nextblock_1 = 4
         CYCLE SPAG_DispatchLoop_1
 200     ipm = Ecpt
         GOTO 180
 220     ipm = Ecpt
 240     n1 = -2
         spag_nextblock_1 = 4
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE edtl

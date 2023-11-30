
SUBROUTINE edtl(Nedt,Ilist,Pg)
!
!     THIS SUBROUTINE COMPUTES THE ELEMENT TEMPERATURE AND ENFORCED
!     DEFORMATION LOADS
!
   IMPLICIT NONE
   INTEGER Alpha , Comps , Costh , Cstm , Dit , Dum(300) , E1 , Ecpt , Edt , Eltype , G , Ge , Gptt , Icheck , Icm , Idefm , Ideft ,&
         & Idum1(14) , Iec , Iflag , Igptt , Ii , Impt , Incr , Incur , Inflag , Iparam , Ipcmp , Ipcmp1 , Ipcmp2 , Iprec , Itemp , &
         & Ithrml , Itya , Ityb , Jj , Ksystm(64) , Last , Lcare , Lcore , Matid , Mecpt(200) , Mpt , N(3) , Ne(1) , Nelems ,       &
         & Ngptt , Nn(3) , Nnn , Npcmp , Npcmp1 , Npcmp2 , Nrowsp , Nsil , Nstart , Nu , Oldel , Outpt , Rho , Sigmac , Sigmas ,    &
         & Sigmat , Sil , Sinth , Space(10) , Stress , Sysbuf , Temp , Tgb(3,3) , To , To1
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
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Outpt) , (Ksystm(55),Iprec) , (Ksystm(56),Ithrml) , (Ti(7),Icheck) , (Ti(6),Iflag)
   DATA iparm , ipgtt/4HEDTL , 4H     , 4HGPTT/
   DATA crod , ctube , conrod , cbar , pcomps/1 , 3 , 10 , 34 , 112/
   DATA pcomp , pcomp1 , pcomp2/5502 , 55 , 5602 , 56 , 5702 , 57/
!
   Igptt = ipgtt
!
!     CHECK IF HEAT FORMULATION
!
   IF ( Ithrml/=0 ) RETURN
!
   Itemp = 0
   Ideft = Nedt
   GOTO 100
!
!
   ENTRY templ(Ntemp,Ilist,Pg)
!     ============================
!
   IF ( Ithrml/=0 ) RETURN
   Ideft = 0
   Itemp = Ntemp
!
!     START SEARCH POINTERS AT ZERO
!
 100  Itya = 1
   CALL delset
   Ityb = 1
   ipr = Iprec
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
   buf1 = Lcore - Sysbuf - 2
   buf2 = buf1 - Sysbuf - 2
   buf3 = buf2 - Sysbuf - 2
   buf4 = buf3 - Sysbuf - 2
   buf5 = buf4 - Sysbuf - 2
!
!     OPEN FILES--
!
!     READ FILE PCOMPS INTO CORE ONLY IF PARAM COMPS = -1,
!     INDICATING THE PRESENCE OF LAMINATED COMPOSITE ELEMENTS
!
   IF ( Comps/=-1 ) GOTO 700
!
   ipm = pcomps
   CALL preloc(*1500,Core(buf2),pcomps)
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
   CALL locate(*300,Core(buf2),pcomp,flag)
!
   CALL read(*2200,*200,pcomps,Core(Ipcmp),Lcore,0,Npcmp)
   GOTO 1900
 200  Ipcmp1 = Ipcmp + Npcmp
   Lcore = Lcore - Npcmp
   IF ( Ipcmp1>=Lcore ) GOTO 1900
!
!     LOCATE PCOMP1 DATA AND READ INTO CORE
!
 300  CALL locate(*500,Core(buf2),pcomp1,flag)
!
   Ipcmp1 = Ipcmp + Npcmp
   CALL read(*600,*400,pcomps,Core(Ipcmp1),Lcore,0,Npcmp1)
   GOTO 1900
 400  Ipcmp2 = Ipcmp1 + Npcmp1
   Lcore = Lcore - Npcmp1
   IF ( Ipcmp2>=Lcore ) GOTO 1900
!
!     LOCATE PCOMP2 DATA AND READ INTO CORE
!
 500  CALL locate(*600,Core(buf2),pcomp2,flag)
!
   Ipcmp2 = Ipcmp1 + Npcmp1
   CALL read(*600,*600,pcomps,Core(Ipcmp2),Lcore,0,Npcmp2)
   GOTO 1900
!
 600  lpcomp = Npcmp + Npcmp1 + Npcmp2
!
   Lcore = Lcore - Npcmp2
   IF ( Lcore<=0 ) GOTO 1900
!
   CALL close(pcomps,1)
!
!
 700  CALL gopen(Ecpt,Core(buf2),0)
   IF ( Itemp/=0 ) THEN
      ipm = Gptt
      CALL open(*1500,Gptt,Core(buf3),0)
!
!     BRING IN MAT ETC
!
      CALL read(*2200,*1800,Gptt,tlist(1),-2,0,ntlist)
      CALL read(*2200,*800,Gptt,tlist(1),1080,1,ntlist)
      WRITE (Outpt,99001) Ufm
99001 FORMAT (A23,' 4013, PROBLEM LIMITATION OF 360 TEMPERATURE SETS ',' HAS BEEN EXCEEDED.')
      n1 = -37
      GOTO 1600
   ENDIF
 800  IF ( Ideft/=0 ) CALL gopen(Edt,Core(buf4),0)
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
   IF ( Lcore<=0 ) GOTO 1900
   IF ( Ideft/=0 ) ldefm = 0
!
   DO illop = 1 , nloop
!
      Idefm = Ilist(illop)
      IF ( Itemp>0 ) CALL rewind(Gptt)
!
      IF ( Nnn==1 ) GOTO 1000
!
!     BRING SIL INTO CORE
!
      IF ( Lcore>=0 ) THEN
         CALL gopen(Sil,Core(buf5),0)
         ipm = Sil
         CALL read(*2200,*850,Sil,Core(Nstart+1),Lcore,1,Nsil)
      ENDIF
      GOTO 1900
 850  CALL close(Sil,1)
      Lcore = Lcore - Nsil
      Nstart = Nstart + Nsil
!
!     READ CSTM INTO OPEN CORE AND MAKE INITIAL CALLS TO PRETRD/PRETRS
!
      IF ( Lcore>=0 ) THEN
         CALL open(*950,Cstm,Core(buf5),0)
         Icm = 0
         CALL skprec(Cstm,1)
         ipm = Cstm
         CALL read(*2200,*900,Cstm,Core(Nstart+1),Lcore,1,ncstm)
      ENDIF
      GOTO 1900
!
!     FOR THOSE SUBROUTINES WHICH USE BASGLB INSTEAD OF TRANSS/TRANSD,
!     WE NEED TO REPOSITION THE CSTM FILE AND LEAVE THE GINO BUFFER
!     AVAILABLE FOR LATER CALLS TO READ BY SUBROUTINE BASGLB.
!
 900  CALL rewind(Cstm)
      CALL skprec(Cstm,1)
!
      CALL pretrd(Core(Nstart+1),ncstm)
      CALL pretrs(Core(Nstart+1),ncstm)
!
      Lcore = Lcore - ncstm
      Nstart = Nstart + ncstm
      IF ( Lcore<=0 ) GOTO 1900
!
 950  Nnn = 1
 1000 IF ( Itemp<=0 ) GOTO 1150
!
      DO i = 1 , ntlist , 3
         IF ( Idefm==tlist(i) ) GOTO 1050
      ENDDO
!
!     THERMAL LOAD NOT FOUND IN GPTT
!
      iparm(2) = iparm(1)
      iparm(1) = Igptt
      CALL mesage(-32,Idefm,iparm(1))
 1050 To = tlist(i+1)
      IF ( tlist(i+2)==0 ) THEN
!
!     THE GPTT (ELEMENT TEMPERATURE TABLE) IS NOW POSITIONED TO THE
!     TEMPERATURE DATA FOR THE SET REQUESTED.  SUBROUTINE SSGETD WILL
!     READ THE DATA.
!
         Record = .FALSE.
         GOTO 1150
      ELSE
         i = tlist(i+2)
         DO j = 1 , i
            CALL fwdrec(*1700,Gptt)
         ENDDO
!
!     READ SETID AND VERIFY CORRECT RECORD.  FAILSAFE
!
         CALL read(*1100,*1100,Gptt,iddd,1,0,dummy)
         IF ( iddd==Idefm ) THEN
            Record = .TRUE.
            GOTO 1150
         ENDIF
      ENDIF
 1100 WRITE (Outpt,99002) Idefm
99002 FORMAT (98H0*** SYSTEM FATAL ERROR 4014, ROUTINE EDTL DETECTS BAD DATA ON TEMPERATURE DATA BLOCK FOR SET ID =,I9)
      n1 = -61
      GOTO 1600
!
 1150 CALL close(Cstm,1)
      CALL open(*1200,Cstm,Core(buf5),0)
      CALL skprec(Cstm,1)
      Icm = 0
 1200 DO i = 1 , Nrowsp
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
 1250 CALL read(*1450,*2000,Ecpt,Eltype,1,0,flag)
      IF ( Eltype>=1 .AND. Eltype<=Nelems ) THEN
         idx = (Eltype-1)*Incr
         jltype = 2*Eltype - ipr
         nwords = Ne(idx+12)
         GOTO 1350
      ELSE
         CALL mesage(-7,0,name)
      ENDIF
 1300 WRITE (Outpt,99003) Swm , Eltype
99003 FORMAT (A27,' 4015, ELEMENT THERMAL AND DEFORMATION LOADING NOT ','COMPUTED FOR ILLEGAL ELEMENT TYPE',I9,/34X,                &
             &'IN MODULE SSG1.')
!
!     NO LOAD, SKIP THE ECPT ENTRY ONLY
!
      CALL fwdrec(*2100,Ecpt)
      GOTO 1250
 1350 DO
!
!     READ AN ENTRY FOR ONE ELEMENT FROM ECPT
!
         CALL read(*2100,*1250,Ecpt,Mecpt(1),nwords,0,flag)
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
                  GOTO 1300
               ELSEIF ( jltype==5 .OR. jltype==6 ) THEN
                  CALL rod
               ELSEIF ( jltype==7 .OR. jltype==8 ) THEN
!
!     SHEAR PANEL
!
                  CALL tshear
               ELSEIF ( jltype==9 .OR. jltype==10 .OR. jltype==21 .OR. jltype==22 .OR. jltype==23 .OR. jltype==24 .OR.              &
                      & jltype==25 .OR. jltype==26 .OR. jltype==27 .OR. jltype==28 .OR. jltype==39 .OR. jltype==40 .OR.             &
                      & jltype==41 .OR. jltype==42 .OR. jltype==43 .OR. jltype==44 .OR. jltype==45 .OR. jltype==46 .OR.             &
                      & jltype==47 .OR. jltype==48 .OR. jltype==49 .OR. jltype==50 .OR. jltype==51 .OR. jltype==52 .OR.             &
                      & jltype==53 .OR. jltype==54 .OR. jltype==55 .OR. jltype==56 .OR. jltype==57 .OR. jltype==58 .OR.             &
                      & jltype==59 .OR. jltype==60 .OR. jltype==61 .OR. jltype==62 .OR. jltype==85 .OR. jltype==86 .OR.             &
                      & jltype==87 .OR. jltype==88 .OR. jltype==89 .OR. jltype==90 .OR. jltype==91 .OR. jltype==92 .OR.             &
                      & jltype==93 .OR. jltype==94 .OR. jltype==95 .OR. jltype==96 .OR. jltype==97 .OR. jltype==98 .OR.             &
                      & jltype==99 .OR. jltype==100 ) THEN
                  CALL fwdrec(*2100,Ecpt)
                  GOTO 1250
               ELSEIF ( jltype==11 .OR. jltype==12 ) THEN
!
!     TRIA1
!
                  kk = 1
                  EXIT
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
                  EXIT
               ELSEIF ( jltype==35 .OR. jltype==36 ) THEN
!
!     QUAD2
!
                  kk = 4
                  EXIT
               ELSEIF ( jltype==37 .OR. jltype==38 ) THEN
!
!     QUAD1
!
                  kk = 3
                  EXIT
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
                  GOTO 1400
               ELSEIF ( jltype==81 .OR. jltype==82 ) THEN
!
!     HEXA1
!
                  iijj = 2
                  npts = 8
                  GOTO 1400
               ELSEIF ( jltype==83 .OR. jltype==84 ) THEN
!
!     HEXA2
!
                  iijj = 3
                  npts = 8
                  GOTO 1400
               ELSE
                  GOTO 1360
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
 1360       IF ( local==1 .OR. local==2 .OR. local==3 .OR. local==4 .OR. local==63 .OR. local==64 ) THEN
               CALL fwdrec(*2100,Ecpt)
               GOTO 1250
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
            ELSEIF ( local==35 .OR. local==36 .OR. local==37 .OR. local==38 .OR. local==43 .OR. local==44 .OR. local==51 .OR.       &
                   & local==52 .OR. local==53 .OR. local==54 .OR. local==55 .OR. local==56 .OR. local==57 .OR. local==58 .OR.       &
                   & local==61 .OR. local==62 ) THEN
               GOTO 1300
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
               CALL fwdrec(*2100,Ecpt)
               GOTO 1250
            ENDIF
         ENDIF
      ENDDO
      CALL ssgetd(Mecpt(1),Ti(1),0)
      CALL triqd(kk,Ti(1))
      GOTO 1350
 1400 CALL ssgetd(Mecpt(1),Ti(1),npts)
      CALL solid(Ti(2),Core(1),iijj)
      GOTO 1350
!
!     PACK THE LOAD VECTOR FROM CORE TO OUTPUT DATA BLOCK -PG-
!
 1450 CALL pack(Core,Pg(1),Pg)
      CALL rewind(Ecpt)
      CALL fwdrec(*2100,Ecpt)
      IF ( Ideft/=0 .AND. Idefm/=0 ) CALL fedted(Idefm)
!
   ENDDO
!
   IF ( noedt==0 ) CALL close(Edt,1)
   IF ( nogptt==0 ) CALL close(Gptt,1)
   IF ( Icm==0 ) CALL close(Cstm,1)
   CALL close(Ecpt,1)
   RETURN
!
 1500 n1 = -1
 1600 CALL mesage(n1,ipm,iparm)
 1700 ipm = Gptt
   GOTO 2200
 1800 n1 = -3
   GOTO 1600
 1900 n1 = -8
   GOTO 1600
 2000 ipm = Ecpt
   GOTO 1800
 2100 ipm = Ecpt
 2200 n1 = -2
   GOTO 1600
END SUBROUTINE edtl

SUBROUTINE ds1(Iarg)
   IMPLICIT NONE
   REAL Bufflg , Dscset , Fdfalt , Sysdum(25) , Xxx18(18) , Z(1)
   INTEGER Eltype , Iback , Ihalf , Incr , Incupk , Ioutpt , Isys , Itypeb , Iunpk , Iz(1) , Junpk , Last , Mach , Mn , Ndum(9) ,   &
         & Ne(1) , Nelems , Oldeid , Oldel , Tsetno
   LOGICAL Endid , Eorflg , Record
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Dscset
   COMMON /ds1ett/ Eltype , Oldel , Eorflg , Endid , Bufflg , Tsetno , Fdfalt , Iback , Record , Oldeid
   COMMON /gpta1 / Nelems , Last , Incr , Ne
   COMMON /machin/ Mach , Ihalf
   COMMON /system/ Isys , Sysdum , Mn , Xxx18 , Ndum
   COMMON /unpakx/ Itypeb , Iunpk , Junpk , Incupk
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Z
   INTEGER Iarg
   INTEGER buffr1 , buffr2 , buffr3 , bufloc , casecc , ccbuf(2) , clsrw , dfmset , dsetno , ecpt , ecptds , edt , edtbuf(3) ,      &
         & edtloc(2) , elid , eor , file , gptt , i , i1 , idfalt , idisp , idset , idx , iecpt(328) , iedt , iflag , iharm , iii , &
         & ileft , imhere , ind , index , inrw , isil , itemp , izmax , j , j1 , jsil(2) , kk , left , lim , low , mcbugv(7) ,      &
         & name(2) , nedt , neor , nogpts , npvt , nrngax , nsil , nskip , ntemp , nwds , outrw , recno , tmpset , ugv
   REAL deform , flag , gptbf3(3) , tgrid(33) , xecpt(328)
   LOGICAL dstype
   INTEGER korsz , rshift
   EXTERNAL rshift
!
!     THIS ROUTINE CREATES THE SCRATCH FILE ECPTDS BY APPENDING TO EACH
!     ELEMENT IN THE ECPT AN ELEMENT DEFORMATION, AN AVERAGE ELEMENT
!     LOADING TEMPERATURE, AND THE PROPER COMPONENTS OF THE DISPLACEMENT
!     VECTORS. SUBROUTINE DS1A READS THE ECPTDS IN THE SAME WAY AS SMA1A
!     READS THE ECPT IN ORDER TO CREATE A SECOND ORDER APPROXIMATION TO
!     THE KGG, WHICH IS CALLED KGGD.
!     IF DS1 CANNOT FIND ANY ELEMENTS IN THE ECPT WHICH ARE IN THE SET
!     OF ELEMENTS FOR WHICH DIFFERENTIAL STIFFNESS IS DEFINED, IARG IS
!     RETURNED CONTAINING A ZERO TO THE CALLING ROUTINE, DSMG1.
!
   !>>>>EQUIVALENCE (Z(1),Iz(1)) , (xecpt(1),iecpt(1)) , (gptbf3(1),tmpset) , (gptbf3(2),idfalt) , (gptbf3(3),recno) , (edtbuf(1),dfmset)&
!>>>>    & , (edtbuf(2),elid) , (edtbuf(3),deform) , (Ioutpt,Sysdum(1))
   DATA edtloc/104 , 1/ , nskip/137/
   DATA casecc , gptt , edt , ugv , ecpt , ecptds/101 , 102 , 104 , 105 , 108 , 301/
   DATA name/4HDS1  , 4H    /
   DATA inrw , outrw , eor , neor , clsrw/0 , 1 , 1 , 0 , 1/
!
!     SET IARG TO ZERO
!
   CALL delset
   Iarg = 0
!
!     DETERMINE SIZE OF AVAILABLE CORE, DEFINE 2 BUFFERS AND INITIALIZE
!     OPEN CORE POINTERS AND COUNTERS.
!
   izmax = korsz(Z)
   buffr1 = izmax - Isys
   buffr2 = buffr1 - Isys
   buffr3 = buffr2 - Isys
   bufloc = izmax - Isys - 3
   ileft = buffr3 - 1
   left = ileft - Nelems - 2
   isil = 0
   nsil = 0
   iedt = 0
   nedt = 0
!
!     SET DIFFERENTIAL STIFFNESS FLAGS FOR ALL ELEMENT TYPES TO ZERO
!
   DO i = 1 , Nelems
      Iz(left+i) = 0
   ENDDO
!
!     OPEN CASECC, SKIP HEADER, SKIP 5 WORDS AND READ DEFORMATION SET
!     NUMBER AND LOADING TEMPERATURE SET NUMBER.
!
   CALL gopen(casecc,Z(buffr1),inrw)
   CALL fread(casecc,0,-5,neor)
   CALL fread(casecc,ccbuf,2,neor)
   dsetno = ccbuf(1)
   Tsetno = ccbuf(2)
!
!     STORE THE DIFFERENTIAL STIFFNESS COEFFICIENT (BETA) SET NUMBER
!     IN COMMON.  THIS WORD IS THE 138TH WORD OF THE 2ND RECORD OF CASE
!     CONTROL.
!
   file = casecc
   CALL fwdrec(*800,casecc)
   CALL fread(casecc,0,-nskip,neor)
   CALL fread(casecc,Dscset,1,neor)
   CALL close(casecc,clsrw)
!
!     IS THERE A TEMPERATURE LOAD
!
   Record = .FALSE.
   Iback = 0
   IF ( Tsetno>0 ) THEN
!
!     THERE IS. OPEN THE GPTT, SKIP FIRST TWO WORDS OF THE HEADER RECORD
!     AND READ 3 WORD ENTRIES OF THE HEADER RECORD UNTIL A SET NUMBER
!     MATCHES THE SET NUMBER READ IN THE CASE CONTROL RECORD.
!
      file = gptt
      CALL open(*800,gptt,Z(buffr3),inrw)
      CALL fread(gptt,0,-2,neor)
      DO
         CALL fread(gptt,gptbf3,3,neor)
         IF ( tmpset==Tsetno ) THEN
            Fdfalt = gptbf3(2)
            IF ( recno/=0 ) THEN
!
!     POSITION GPTT TO DESIRED TEMPERATURE RECORD
!
               CALL rewind(gptt)
               DO i = 1 , recno
                  CALL fwdrec(*900,gptt)
               ENDDO
               Record = .TRUE.
!
!     READ SETID AND VERIFY FOR CORRECTNESS
!
               CALL fread(gptt,idset,1,0)
               IF ( Tsetno/=idset ) CALL mesage(-30,29,Tsetno)
!
!     INITIALIZE /DS1ETT/ VARIABLES
!
               Oldeid = 0
               Oldel = 0
               Eorflg = .FALSE.
               Endid = .TRUE.
            ELSE
               IF ( idfalt==-1 ) CALL mesage(-30,29,Tsetno)
               CALL close(gptt,clsrw)
            ENDIF
            EXIT
         ENDIF
      ENDDO
   ENDIF
!
!     DETERMINE IF AN ENFORCED DEFORMATION SET IS CALLED FOR.
!
   iedt = isil
   i = isil
   IF ( dsetno<=0 ) GOTO 200
   file = edt
   CALL preloc(*200,Z(bufloc),edt)
   CALL locate(*1200,Z(bufloc),edtloc,iflag)
   DO
      CALL read(*900,*100,edt,edtbuf,3,neor,iflag)
      IF ( dfmset==dsetno ) THEN
         Iz(i+1) = elid
         Z(i+2) = deform
         nedt = nedt + 2
         i = i + 2
         left = left - 2
         IF ( left<=0 ) CALL mesage(-8,0,name)
      ENDIF
   ENDDO
 100  CALL close(edt,clsrw)
   low = iedt + 1
   lim = iedt + nedt
!
!     READ THE UGV INTO CORE.
!
 200  CALL gopen(ugv,Z(buffr1),inrw)
   idisp = iedt + nedt
   mcbugv(1) = ugv
   CALL rdtrl(mcbugv(1))
   IF ( left<mcbugv(3) ) CALL mesage(-8,0,name(1))
   Itypeb = 1
   Iunpk = 1
   Junpk = mcbugv(3)
   Incupk = 1
   CALL unpack(*1300,ugv,Z(idisp+1))
   CALL close(ugv,clsrw)
!
!     OPEN THE ECPTDS AND ECPT FILES.
!
   CALL gopen(ecptds,Z(buffr2),outrw)
   CALL gopen(ecpt,Z(buffr1),inrw)
!
!     READ THE PIVOT POINT (1ST WORD).
!
 300  file = ecpt
   imhere = 100
   Eltype = -1
   j = -1
   CALL read(*700,*1100,ecpt,npvt,1,neor,iflag)
   ind = 0
 400  dstype = .FALSE.
!
!     READ ELEMENT TYPE (2ND WORD)
!
   CALL read(*900,*600,ecpt,Eltype,1,neor,iflag)
   IF ( Eltype<1 .OR. Eltype>Nelems ) GOTO 1500
!
!     READ ELEMENT ID (3RD WORD, BEGINNING OF J NO. OF WORDS)
!
   imhere = 115
   CALL read(*900,*1100,ecpt,iecpt,1,neor,iflag)
   IF ( Iback/=0 ) THEN
      IF ( Eltype==Oldel .AND. iecpt(1)>=Oldeid ) GOTO 500
      CALL bckrec(gptt)
!
!     RESET /DS1ETT/ VARIABLES
!
      Iback = 0
      Oldeid = 0
      Oldel = 0
      Eorflg = .FALSE.
      Endid = .TRUE.
      CALL read(*900,*1000,gptt,idset,1,0,flag)
      IF ( Tsetno/=idset ) CALL mesage(-30,29,Tsetno)
   ENDIF
!
   idx = (Eltype-1)*Incr
   ntemp = 1
!                IS2D8              IHEX1              IHEX3
   IF ( Eltype==80 .OR. (Eltype>=65 .AND. Eltype<=67) ) ntemp = Ne(idx+15) - 1
!
!     READ ECPT ENTRY FOR THIS ELEMENT (J-1 WORDS)
!
 500  j = Ne(idx+12)
   IF ( Ne(idx+24)/=0 ) dstype = .TRUE.
   imhere = 130
   CALL read(*900,*1100,ecpt,xecpt(2),j-1,neor,iflag)
!
!     IS THIS ELEMENT IN THE SET OF DS ELEMENTS.
!
   IF ( dstype ) THEN
      Iarg = 1
!
!     DETERMINE IF THE ELEMENT IS A CONE.  IF IT IS, IT MUST HAVE A
!     NONZERO MEMBRANE THICKNESS FOR IT TO BE ADMISSIBLE TO THE ECPTDS.
!
      IF ( Eltype==35 ) THEN
!                 CONEAX
         ntemp = 2
         IF ( xecpt(5)==0.0 ) GOTO 400
!
!     DETERMINE THE NUMBER OF RINGAX POINTS FROM THE 27TH WORD OF
!     /SYSTEM/.
!
         nrngax = rshift(Mn,Ihalf)
!
!     DETERMINE THE HARMONIC NUMBER, IHARM, FROM THE ELEMENT IDENT.
!     NUMBER, IECPT(1)
!
         itemp = iecpt(1)/1000
         iharm = iecpt(1) - itemp*1000 - 1
!
!     DETERMINE THE SIL NUMBERS, SIL(1) AND SIL(2), WHICH WILL BE USED
!     TO APPEND TEMPERATURES AND DISPLACEMENT VECTORS.
!
         IF ( iharm/=0 ) THEN
            itemp = 6*iharm*nrngax
            jsil(1) = iecpt(2) - itemp
            jsil(2) = iecpt(3) - itemp
         ELSE
            jsil(1) = iecpt(2)
            jsil(2) = iecpt(3)
         ENDIF
!
!     IF WE ARE DEALING WITH A TRIA1 OR QUAD1 ELEMENT, IT MUST HAVE A
!     NONZERO MEMBRANE THICKNESS FOR IT TO BE ADMISSIBLE TO THE ECPTDS.
!
      ELSEIF ( Eltype==6 .OR. Eltype==19 ) THEN
!               TRIA1              QUAD1
         kk = 7
         IF ( Eltype==19 ) kk = 8
!                  QUAD1
         IF ( xecpt(kk)==0.0 ) GOTO 400
      ENDIF
!
!     WRITE PIVOT POINT
!
      IF ( ind==0 ) CALL write(ecptds,npvt,1,neor)
      ind = 1
      IF ( Eltype==34 ) THEN
!                    BAR
!
!     THE ELEMENT IS A BAR.  THE ECPT ENTRY WILL BE REARRANGED SO THAT
!     THE DBAR SUBROUTINE MAY BE CALLED IN SUBROUTINE DS1A.
!
         Eltype = 2
!           BEAM
!
!     IF THE COUPLED MOMENT OF INERTIA TERM I12 (=ECPT(33)) IS NON-ZERO
!     SET I12 = 0.0, WRITE WARNING MESSAGE AND PROCEED.
!
         IF ( xecpt(33)/=0.0 ) THEN
            xecpt(33) = 0.0
            CALL mesage(30,111,iecpt(1))
         ENDIF
         xecpt(47) = xecpt(42)
         xecpt(46) = xecpt(41)
         xecpt(45) = xecpt(40)
         xecpt(44) = xecpt(39)
         xecpt(43) = xecpt(38)
         xecpt(42) = xecpt(37)
         xecpt(41) = xecpt(36)
         xecpt(40) = xecpt(35)
         xecpt(39) = xecpt(34)
         xecpt(29) = xecpt(31)
         xecpt(30) = xecpt(32)
         xecpt(28) = xecpt(21)
         xecpt(27) = xecpt(20)
         xecpt(25) = xecpt(19)
         xecpt(24) = xecpt(18)
         xecpt(21) = xecpt(17)
         xecpt(20) = xecpt(16)
         j = 47
      ENDIF
!
!     WRITE ELEMENT TYPE
!
      CALL write(ecptds,Eltype,1,neor)
!
!     ATTACH THE ELEMENT DEFORMATION TO THE XECPT ARRAY.
!
      j = j + 1
      nogpts = Ne(idx+10)
      xecpt(j) = 0.0
      IF ( dsetno>0 ) THEN
!
!     SEARCH THE EDT TO FIND AN ELEMENT NO. IN THE TABLE CORRESPONDING
!     TO THE CURRENT ELEMENT NO., IECPT(1).  IF IT CANNOT BE FOUND NOTE
!     THE ELEMENT DEFORMATION, IECPT(J), HAS BEEN SET TO ZERO.
!
         DO i = low , lim , 2
            IF ( Iz(i)==iecpt(1) ) THEN
               xecpt(j) = Z(i+1)
               EXIT
            ENDIF
         ENDDO
      ENDIF
!
!     APPEND THE LOADING TEMPERATURE(S) TO THE XECPT ARRAY
!
      IF ( Eltype==2 ) Eltype = 34
!                  BEAM          BAR
      CALL ds1etd(iecpt(1),tgrid,ntemp)
      IF ( Eltype==34 ) THEN
!                    BAR
         Eltype = 2
         IF ( Tsetno>0 ) tgrid(1) = (tgrid(1)+tgrid(2))*0.5
      ENDIF
      iii = 1
      IF ( Eltype==80 ) THEN
!                  IS2D8
         j = j + 1
         iecpt(j) = Tsetno
         iii = 2
      ENDIF
      DO i = iii , ntemp
         j = j + 1
         xecpt(j) = tgrid(i)
      ENDDO
!
!     NOW ATTACH THE DISPLACEMENT VECTORS
!
      j = j + 1
      IF ( Eltype==35 ) THEN
!
!     APPEND THE ZERO HARMONIC COMPONENTS OF THE DISPLACEMENT VECTOR.
!     NOTE THAT FOR A CONICAL SHELL ELEMENT DIRECT POINTERS INTO THE
!     DISPLACEMENT VECTOR ARE SIL(1) AND SIL(2).
!
         DO j1 = 1 , 2
            DO i = 1 , 6
               index = idisp + jsil(j1) + i - 1
               xecpt(j) = Z(index)
               j = j + 1
            ENDDO
         ENDDO
!
!     THE APPENDED ECPT, ECPTDS, IS NOW COMPLETE.
!
         CALL write(ecptds,xecpt,j-1,neor)
      ELSE
!                 CONEAX
         IF ( Eltype==2 .OR. Eltype==75 ) THEN
            nwds = 6
!                 BEAM             TRSHL
         ELSEIF ( Eltype<53 .OR. Eltype>61 ) THEN
            nwds = 3
!                 DUM1              DUM9
!
!
!     DUMMY ELEMENTS
!
         ELSEIF ( mod(Ndum(Eltype-52),10)==6 ) THEN
            nwds = 6
         ELSE
            nwds = 3
         ENDIF
         DO i = 1 , nogpts
            index = idisp + iecpt(i+1)
            DO i1 = 1 , nwds
               xecpt(j) = Z(index)
               index = index + 1
               j = j + 1
            ENDDO
         ENDDO
         CALL write(ecptds,xecpt,j-1,neor)
      ENDIF
   ELSEIF ( Iz(left+Eltype)/=1 ) THEN
      Iz(left+Eltype) = 1
      CALL page2(-2)
      WRITE (Ioutpt,99001) Uwm , Ne(idx+1) , Ne(idx+2) , Eltype
99001 FORMAT (A25,' 3117, DIFFERENTIAL STIFFNESS CAPABILITY NOT DEFINED',' FOR ',2A4,' ELEMENTS (ELEMENT TYPE ',I3,2H).)
   ENDIF
   GOTO 400
!
!    IF IND = 0, THEN NO ELEMENTS IN THE CURRENT ECPT RECORD ARE IN THE
!    DS ELEMENT SET.  WRITE A -1 FOR THIS PIVOT POINT.
!
 600  IF ( ind/=0 ) THEN
!
!     WRITE AN EOR ON THE ECPTDS FILE
!
      CALL write(ecptds,0,0,eor)
   ELSE
      CALL write(ecptds,-1,1,eor)
   ENDIF
   GOTO 300
!
!     CLOSE BOTH FILES
!
 700  CALL close(ecpt,clsrw)
   CALL close(gptt,clsrw)
   CALL close(ecptds,clsrw)
   RETURN
!
!     FATAL ERROR RETURNS
!
 800  j = -1
   GOTO 1400
 900  j = -2
   GOTO 1400
 1000 file = gptt
 1100 j = -3
   IF ( file==ecpt ) WRITE (Ioutpt,99002) imhere , Eltype , j
99002 FORMAT (/,'0*** DS1/IMHERE,ELTYPE,J = ',3I5)
   GOTO 1400
 1200 j = -4
   GOTO 1400
 1300 CALL mesage(-30,83,name(1))
 1400 CALL mesage(j,file,name)
 1500 WRITE (Ioutpt,99003) Sfm , Eltype
99003 FORMAT (A25,' 2147, ILLEGAL ELEMENT TYPE =',I10,' ENCOUNTERED BY DSMG1 MODULE.')
   CALL mesage(-61,0,name)
END SUBROUTINE ds1
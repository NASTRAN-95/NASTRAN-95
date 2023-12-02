!*==ds1.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE ds1(Iarg)
   IMPLICIT NONE
   USE c_blank
   USE c_ds1ett
   USE c_gpta1
   USE c_machin
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iarg
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buffr1 , buffr2 , buffr3 , bufloc , dfmset , dsetno , elid , file , i , i1 , idfalt , idisp , idset , idx , iedt ,    &
            & iflag , iharm , iii , ileft , imhere , ind , index , ioutpt , isil , itemp , izmax , j , j1 , kk , left , lim , low , &
            & nedt , nogpts , npvt , nrngax , nsil , ntemp , nwds , recno , tmpset
   INTEGER , SAVE :: casecc , clsrw , ecpt , ecptds , edt , eor , gptt , inrw , neor , nskip , outrw , ugv
   INTEGER , DIMENSION(2) :: ccbuf , jsil
   REAL :: deform , flag
   LOGICAL :: dstype
   INTEGER , DIMENSION(3) :: edtbuf
   INTEGER , DIMENSION(2) , SAVE :: edtloc , name
   REAL , DIMENSION(3) :: gptbf3
   INTEGER , DIMENSION(328) :: iecpt
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(7) :: mcbugv
   REAL , DIMENSION(33) :: tgrid
   REAL , DIMENSION(328) :: xecpt
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
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
   izmax = korsz(z)
   buffr1 = izmax - isys
   buffr2 = buffr1 - isys
   buffr3 = buffr2 - isys
   bufloc = izmax - isys - 3
   ileft = buffr3 - 1
   left = ileft - nelems - 2
   isil = 0
   nsil = 0
   iedt = 0
   nedt = 0
!
!     SET DIFFERENTIAL STIFFNESS FLAGS FOR ALL ELEMENT TYPES TO ZERO
!
   DO i = 1 , nelems
      iz(left+i) = 0
   ENDDO
!
!     OPEN CASECC, SKIP HEADER, SKIP 5 WORDS AND READ DEFORMATION SET
!     NUMBER AND LOADING TEMPERATURE SET NUMBER.
!
   CALL gopen(casecc,z(buffr1),inrw)
   CALL fread(casecc,0,-5,neor)
   CALL fread(casecc,ccbuf,2,neor)
   dsetno = ccbuf(1)
   tsetno = ccbuf(2)
!
!     STORE THE DIFFERENTIAL STIFFNESS COEFFICIENT (BETA) SET NUMBER
!     IN COMMON.  THIS WORD IS THE 138TH WORD OF THE 2ND RECORD OF CASE
!     CONTROL.
!
   file = casecc
   CALL fwdrec(*600,casecc)
   CALL fread(casecc,0,-nskip,neor)
   CALL fread(casecc,dscset,1,neor)
   CALL close(casecc,clsrw)
!
!     IS THERE A TEMPERATURE LOAD
!
   record = .FALSE.
   iback = 0
   IF ( tsetno>0 ) THEN
!
!     THERE IS. OPEN THE GPTT, SKIP FIRST TWO WORDS OF THE HEADER RECORD
!     AND READ 3 WORD ENTRIES OF THE HEADER RECORD UNTIL A SET NUMBER
!     MATCHES THE SET NUMBER READ IN THE CASE CONTROL RECORD.
!
      file = gptt
      CALL open(*600,gptt,z(buffr3),inrw)
      CALL fread(gptt,0,-2,neor)
      DO
         CALL fread(gptt,gptbf3,3,neor)
         IF ( tmpset==tsetno ) THEN
            fdfalt = gptbf3(2)
            IF ( recno/=0 ) THEN
!
!     POSITION GPTT TO DESIRED TEMPERATURE RECORD
!
               CALL rewind(gptt)
               DO i = 1 , recno
                  CALL fwdrec(*700,gptt)
               ENDDO
               record = .TRUE.
!
!     READ SETID AND VERIFY FOR CORRECTNESS
!
               CALL fread(gptt,idset,1,0)
               IF ( tsetno/=idset ) CALL mesage(-30,29,tsetno)
!
!     INITIALIZE /DS1ETT/ VARIABLES
!
               oldeid = 0
               oldel = 0
               eorflg = .FALSE.
               endid = .TRUE.
            ELSE
               IF ( idfalt==-1 ) CALL mesage(-30,29,tsetno)
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
   CALL preloc(*200,z(bufloc),edt)
   CALL locate(*1000,z(bufloc),edtloc,iflag)
   DO
      CALL read(*700,*100,edt,edtbuf,3,neor,iflag)
      IF ( dfmset==dsetno ) THEN
         iz(i+1) = elid
         z(i+2) = deform
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
 200  CALL gopen(ugv,z(buffr1),inrw)
   idisp = iedt + nedt
   mcbugv(1) = ugv
   CALL rdtrl(mcbugv(1))
   IF ( left<mcbugv(3) ) CALL mesage(-8,0,name(1))
   itypeb = 1
   iunpk = 1
   junpk = mcbugv(3)
   incupk = 1
   CALL unpack(*1100,ugv,z(idisp+1))
   CALL close(ugv,clsrw)
!
!     OPEN THE ECPTDS AND ECPT FILES.
!
   CALL gopen(ecptds,z(buffr2),outrw)
   CALL gopen(ecpt,z(buffr1),inrw)
!
!     READ THE PIVOT POINT (1ST WORD).
!
 300  file = ecpt
   imhere = 100
   eltype = -1
   j = -1
   CALL read(*500,*900,ecpt,npvt,1,neor,iflag)
   ind = 0
   DO
      dstype = .FALSE.
!
!     READ ELEMENT TYPE (2ND WORD)
!
      CALL read(*700,*400,ecpt,eltype,1,neor,iflag)
      IF ( eltype<1 .OR. eltype>nelems ) GOTO 1300
!
!     READ ELEMENT ID (3RD WORD, BEGINNING OF J NO. OF WORDS)
!
      imhere = 115
      CALL read(*700,*900,ecpt,iecpt,1,neor,iflag)
      IF ( iback/=0 ) THEN
         IF ( eltype==oldel .AND. iecpt(1)>=oldeid ) GOTO 350
         CALL bckrec(gptt)
!
!     RESET /DS1ETT/ VARIABLES
!
         iback = 0
         oldeid = 0
         oldel = 0
         eorflg = .FALSE.
         endid = .TRUE.
         CALL read(*700,*800,gptt,idset,1,0,flag)
         IF ( tsetno/=idset ) CALL mesage(-30,29,tsetno)
      ENDIF
!
      idx = (eltype-1)*incr
      ntemp = 1
!                IS2D8              IHEX1              IHEX3
      IF ( eltype==80 .OR. (eltype>=65 .AND. eltype<=67) ) ntemp = ne(idx+15) - 1
!
!     READ ECPT ENTRY FOR THIS ELEMENT (J-1 WORDS)
!
 350  j = ne(idx+12)
      IF ( ne(idx+24)/=0 ) dstype = .TRUE.
      imhere = 130
      CALL read(*700,*900,ecpt,xecpt(2),j-1,neor,iflag)
!
!     IS THIS ELEMENT IN THE SET OF DS ELEMENTS.
!
      IF ( dstype ) THEN
         Iarg = 1
!
!     DETERMINE IF THE ELEMENT IS A CONE.  IF IT IS, IT MUST HAVE A
!     NONZERO MEMBRANE THICKNESS FOR IT TO BE ADMISSIBLE TO THE ECPTDS.
!
         IF ( eltype==35 ) THEN
!                 CONEAX
            ntemp = 2
            IF ( xecpt(5)==0.0 ) CYCLE
!
!     DETERMINE THE NUMBER OF RINGAX POINTS FROM THE 27TH WORD OF
!     /SYSTEM/.
!
            nrngax = rshift(mn,ihalf)
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
         ELSEIF ( eltype==6 .OR. eltype==19 ) THEN
!               TRIA1              QUAD1
            kk = 7
            IF ( eltype==19 ) kk = 8
!                  QUAD1
            IF ( xecpt(kk)==0.0 ) CYCLE
         ENDIF
!
!     WRITE PIVOT POINT
!
         IF ( ind==0 ) CALL write(ecptds,npvt,1,neor)
         ind = 1
         IF ( eltype==34 ) THEN
!                    BAR
!
!     THE ELEMENT IS A BAR.  THE ECPT ENTRY WILL BE REARRANGED SO THAT
!     THE DBAR SUBROUTINE MAY BE CALLED IN SUBROUTINE DS1A.
!
            eltype = 2
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
         CALL write(ecptds,eltype,1,neor)
!
!     ATTACH THE ELEMENT DEFORMATION TO THE XECPT ARRAY.
!
         j = j + 1
         nogpts = ne(idx+10)
         xecpt(j) = 0.0
         IF ( dsetno>0 ) THEN
!
!     SEARCH THE EDT TO FIND AN ELEMENT NO. IN THE TABLE CORRESPONDING
!     TO THE CURRENT ELEMENT NO., IECPT(1).  IF IT CANNOT BE FOUND NOTE
!     THE ELEMENT DEFORMATION, IECPT(J), HAS BEEN SET TO ZERO.
!
            DO i = low , lim , 2
               IF ( iz(i)==iecpt(1) ) THEN
                  xecpt(j) = z(i+1)
                  EXIT
               ENDIF
            ENDDO
         ENDIF
!
!     APPEND THE LOADING TEMPERATURE(S) TO THE XECPT ARRAY
!
         IF ( eltype==2 ) eltype = 34
!                  BEAM          BAR
         CALL ds1etd(iecpt(1),tgrid,ntemp)
         IF ( eltype==34 ) THEN
!                    BAR
            eltype = 2
            IF ( tsetno>0 ) tgrid(1) = (tgrid(1)+tgrid(2))*0.5
         ENDIF
         iii = 1
         IF ( eltype==80 ) THEN
!                  IS2D8
            j = j + 1
            iecpt(j) = tsetno
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
         IF ( eltype==35 ) THEN
!
!     APPEND THE ZERO HARMONIC COMPONENTS OF THE DISPLACEMENT VECTOR.
!     NOTE THAT FOR A CONICAL SHELL ELEMENT DIRECT POINTERS INTO THE
!     DISPLACEMENT VECTOR ARE SIL(1) AND SIL(2).
!
            DO j1 = 1 , 2
               DO i = 1 , 6
                  index = idisp + jsil(j1) + i - 1
                  xecpt(j) = z(index)
                  j = j + 1
               ENDDO
            ENDDO
!
!     THE APPENDED ECPT, ECPTDS, IS NOW COMPLETE.
!
            CALL write(ecptds,xecpt,j-1,neor)
         ELSE
!                 CONEAX
            IF ( eltype==2 .OR. eltype==75 ) THEN
               nwds = 6
!                 BEAM             TRSHL
            ELSEIF ( eltype<53 .OR. eltype>61 ) THEN
               nwds = 3
!                 DUM1              DUM9
!
!
!     DUMMY ELEMENTS
!
            ELSEIF ( mod(ndum(eltype-52),10)==6 ) THEN
               nwds = 6
            ELSE
               nwds = 3
            ENDIF
            DO i = 1 , nogpts
               index = idisp + iecpt(i+1)
               DO i1 = 1 , nwds
                  xecpt(j) = z(index)
                  index = index + 1
                  j = j + 1
               ENDDO
            ENDDO
            CALL write(ecptds,xecpt,j-1,neor)
         ENDIF
      ELSEIF ( iz(left+eltype)/=1 ) THEN
         iz(left+eltype) = 1
         CALL page2(-2)
         WRITE (ioutpt,99001) uwm , ne(idx+1) , ne(idx+2) , eltype
99001    FORMAT (A25,' 3117, DIFFERENTIAL STIFFNESS CAPABILITY NOT DEFINED',' FOR ',2A4,' ELEMENTS (ELEMENT TYPE ',I3,2H).)
      ENDIF
   ENDDO
!
!    IF IND = 0, THEN NO ELEMENTS IN THE CURRENT ECPT RECORD ARE IN THE
!    DS ELEMENT SET.  WRITE A -1 FOR THIS PIVOT POINT.
!
 400  IF ( ind/=0 ) THEN
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
 500  CALL close(ecpt,clsrw)
   CALL close(gptt,clsrw)
   CALL close(ecptds,clsrw)
   RETURN
!
!     FATAL ERROR RETURNS
!
 600  j = -1
   GOTO 1200
 700  j = -2
   GOTO 1200
 800  file = gptt
 900  j = -3
   IF ( file==ecpt ) WRITE (ioutpt,99002) imhere , eltype , j
99002 FORMAT (/,'0*** DS1/IMHERE,ELTYPE,J = ',3I5)
   GOTO 1200
 1000 j = -4
   GOTO 1200
 1100 CALL mesage(-30,83,name(1))
 1200 CALL mesage(j,file,name)
 1300 WRITE (ioutpt,99003) sfm , eltype
99003 FORMAT (A25,' 2147, ILLEGAL ELEMENT TYPE =',I10,' ENCOUNTERED BY DSMG1 MODULE.')
   CALL mesage(-61,0,name)
END SUBROUTINE ds1

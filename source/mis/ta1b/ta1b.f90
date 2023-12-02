!*==ta1b.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE ta1b
   IMPLICIT NONE
   USE C_BLANK
   USE C_GPTA1
   USE C_NAMES
   USE C_SYSTEM
   USE C_TA1COM
   USE C_TA1ETT
   USE C_XMSSG
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: bar , cbar , ihex2 , ihex3 , plot , plotel , ppse , quadts , react , shear , triats , twist , two24
   INTEGER , DIMENSION(2) :: blk
   INTEGER , DIMENSION(50) :: buf
   INTEGER :: buf1 , buf2 , buf3 , buf4 , elemid , file , flag , i , id , idcntr , idptr , iecpt0 , ielem , igp , ii , ijk , ik ,   &
            & infile , ipass , irigd , is1 , is2 , iset , ist , itmpid , ix , izero , j , jscalr , jtemp , k , k1 , khi , kk , klo ,&
            & kscalr , ktwo24 , kx , l , last , length , lj , ll , llx , locbgp , locgpc , locsil , loctmp , locx , lq , lstprp ,   &
            & lx , m , maxel , mm , n , n2 , n21 , name , nbgp , ndx1 , ndx2 , neq1 , neq2 , nid , noect , nogpct , npvt , ntemp ,  &
            & ntmp , ntmpid , number , nx , op , oufile , outpt , ret , ret1 , ret2 , scri
   REAL , DIMENSION(50) :: bufr
   REAL :: deftmp
   INTEGER , DIMENSION(34) :: gpsav
   INTEGER , DIMENSION(2) , SAVE :: nam
   INTEGER :: scro , sysbuf , tempid
   REAL , DIMENSION(33) :: tgrid
   INTEGER , DIMENSION(4) , SAVE :: zeros
   REAL , DIMENSION(1) :: zz
!
! End of declarations rewritten by SPAG
!
!
!     TA1B BUILDS THE ELEMENT CONNECTION AND PROPERTIES TABLE (ECPT)
!     AND THE GRID POINT CONNECTION TABLE. THE ECPT CONTAINS ONE LOGICAL
!     RECORD FOR EACH GRID OR SCALAR POINT IN THE STRUCTURE.  EACH
!     LOGICAL RECORD CONTAINS EST TYPE DATA FOR ELEMENTS CONNECTED TO
!     THE GRID OR SCALAR POINT THE GPCT IS A SUMMARY OF THE ECPT.  EACH
!     LOGICAL RECORD CONTAINS ALL GRID POINTS CONNECTED TO THE PIVOT (BY
!     MEANS OF STRUCTURAL ELEMENTS).
!
   !>>>>EQUIVALENCE (Ksystm(1),Sysbuf) , (Ksystm(2),Outpt) , (Ksystm(10),Tempid) , (Idftmp,Deftmp)
   !>>>>EQUIVALENCE (blk(1),npvt) , (buf(1),bufr(1)) , (Z(1),Zz(1)) , (blk(2),n)
   DATA nam/4HTA1B , 3H   / , cbar/4HBAR / , plot/4HPLOT/
   DATA two24/8388608/ , zeros/0 , 0 , 0 , 0/ , ppse/4303/
   DATA plotel , react , shear , twist , ihex2 , ihex3 , quadts , triats , bar/5201 , 5251 , 4 , 5 , 66 , 67 , 68 , 69 , 34/
!
!     PERFORM GENERAL INITIALIZATION
!
   n2 = 2*Nelem - 1
   n21 = n2 + 1
   buf1 = korsz(Z) - sysbuf - 2
   buf2 = buf1 - sysbuf
   buf3 = buf2 - sysbuf
   buf4 = buf3 - sysbuf
   neq1 = Nsil + 1
   neq2 = 0
   kscalr = 0
!
!     THE GRID POINT COUNTER (GPC) HAS ONE ENTRY PER GRID OR SCALAR
!     POINT IN THE STRUCTURE. EACH ENTRY CONTAINS THE NUMBER OF
!     STRUCTURAL ELEMENTS CONNECTED TO THE POINT.
!
   DO i = 1 , Nsil
      Z(i+1) = 0
   ENDDO
!
!     OPEN THE ECT. INITIALIZE TO LOOP THRU BY ELEMENT TYPE.
!
   file = Ect
   CALL preloc(*2900,Z(buf1),Ect)
   noect = 1
   DO i = 1 , Jlast , Incr
!
!     IGNORE PLOTEL ELEMENTS.  OTHERWISE, LOCATE AN ELEMENT TYPE.
!     IF PRESENT, READ ALL ELEMENTS OF THAT TYPE AND INCREMENT THE GPC
!     ENTRY FOR EACH POINT TO WHICH THE ELEMENT IS CONNECTED.
!
      IF ( Elem(i)/=plot ) THEN
         CALL locate(*100,Z(buf1),Elem(i+3),flag)
         noect = 0
         lx = Elem(i+12)
         mm = lx + Elem(i+9) - 1
         m = Elem(i+5)
         IF ( Elem(i+10)==0 ) kscalr = 1
         DO
            CALL read(*3000,*100,Ect,buf,m,0,flag)
            DO l = lx , mm
               k = buf(l)
               IF ( k/=0 ) Z(k+1) = Z(k+1) + 1
            ENDDO
         ENDDO
      ENDIF
 100  ENDDO
   CALL close(Ect,Clsrew)
   IF ( noect/=0 ) THEN
      buf(1) = 0
      buf(2) = 0
      n = 13
      CALL mesage(-30,n,buf)
      CALL mesage(j,file,nam)
      GOTO 99999
   ELSE
!
!     REPLACE ENTRIES IN THE GPC BY A RUNNING SUM THUS CREATING POINTERS
!     INTO ECPT0.  QUEUE WARNING MESSAGES FOR GRID PTS. WITH NO ELEMENTS
!     CONNECTED.
!     (BRING IN EQEXIN AND ECHO OUT EXTERNAL GRID PT. ID  G.C/UNISYS 91)
!
      Z(1) = 1
      maxel = 0
      DO i = 1 , Nsil
         maxel = max0(maxel,Z(i+1))
         IF ( Z(i+1)/=0 ) GOTO 180
!
         j = 0
         IF ( neq2<0 ) GOTO 160
         IF ( neq2/=0 ) GOTO 140
         neq2 = -1
         Z(neq1) = Eqexin
         CALL rdtrl(Z(neq1))
         IF ( Z(neq1)<=0 ) GOTO 160
         file = Eqexin
!WKBR CALL GOPEN (EQEXIN,EQEXIN,Z(BUF1),RDREW)
         CALL gopen(Eqexin,Z(buf1),Rdrew)
         CALL read(*2900,*120,Eqexin,Z(neq1),buf4,1,neq2)
 120     CALL close(Eqexin,Clsrew)
         CALL sort(0,0,2,2,Z(neq1),neq2)
 140     j = Z((i-1)*2+neq1)
!
 160     buf(1) = i
         buf(2) = j
         CALL mesage(30,15,buf)
 180     Z(i+1) = Z(i) + Z(i+1)
      ENDDO
!
!     DETERMINE BAND OF ENTRIES IN ECPT0 WHICH WILL FIT IN CORE
!     NDX1 = POINTER IN GPC TO 1ST  ENTRY FOR CURRENT PASS.
!     NDX2 = POINTER IN GPC TO LAST ENTRY FOR CURRENT PASS.
!
      ndx1 = 1
      ndx2 = Nsil
      llx = 1
      iecpt0 = Nsil + 2
      length = buf1 - iecpt0
      op = Wrtrew
      DO
         IF ( Z(ndx2+1)-Z(ndx1)+2<=length ) THEN
!
!     PASS THE ECT. FOR EACH GRID PT IN RANGE ON THIS PASS,
!     STORE ELEMENT POINTER = 2**24 * J + WORD POSITION IN ECT RECORD.
!     WHERE J= (POINTER IN ELEM TABLE - 1)/INCR * 2 +1
!
            CALL preloc(*2900,Z(buf1),Ect)
            izero = Z(ndx1)
            j = 1
            DO i = 1 , Jlast , Incr
               IF ( Elem(i)/=plot ) THEN
                  idcntr = two24*j
                  CALL locate(*185,Z(buf1),Elem(i+3),flag)
                  m = Elem(i+5)
                  lx = Elem(i+12)
                  mm = lx + Elem(i+9) - 1
                  DO
                     CALL read(*3000,*185,Ect,buf,m,0,flag)
                     DO l = lx , mm
                        k = buf(l)
                        IF ( k>=ndx1 .AND. k<=ndx2 ) THEN
                           ix = Z(k) - izero + iecpt0
                           Z(ix) = idcntr
                           Z(k) = Z(k) + 1
                        ENDIF
                     ENDDO
                     idcntr = idcntr + m
                  ENDDO
               ENDIF
 185           j = j + 2
            ENDDO
            CALL close(Ect,Clsrew)
!
!     WRITE ECPT0 AND TEST FOR ADDITIONAL PASSES
!     ECPT0 CONTAINS ONE LOGICAL RECORD FOR EACH GRID OR SCALAR POINT.
!     EACH LOGICAL RECORD CONTAINS N PAIRS OF(-1,ELEMENT POINTER)WHERE
!     N= NUMBER OF ELEMENTS CONNECTED TO THE PIVOT.
!     IF NO ELEMENTS CONNECTED TO POINT, RECORD IS ONE WORD = 0.
!
            file = Scr1
            CALL open(*2900,Scr1,Z(buf1),op)
            buf(1) = -1
            lj = iecpt0 - 1
            DO i = ndx1 , ndx2
               m = Z(i) - llx
               IF ( m/=0 ) THEN
                  DO j = 1 , m
                     lj = lj + 1
                     buf(2) = Z(lj)
                     CALL write(Scr1,buf,2,0)
                  ENDDO
                  CALL write(Scr1,0,0,1)
               ELSE
                  CALL write(Scr1,0,1,1)
               ENDIF
               llx = Z(i)
            ENDDO
            IF ( ndx2>=Nsil ) THEN
!
!     READ AS MUCH OF ECT AS CORE CAN HOLD
!     FIRST N21 CELLS OF CORE CONTAIN A POINTER TABLE WHICH HAS TWO
!     ENTRIES PER ELEMENT TYPE. 1ST ENTRY HAS POINTER TO 1ST WORD OF
!     ECT DATA IN CORE FOR AN ELEMENT TYPE  2ND ENTRY HAS WORD POSITION
!     IN ECT RECORD OF THAT TYPE FOR LAST ENTRY READ ON PREVIOUS PASS.
!
               CALL close(Scr1,Clsrew)
               scri = Scr1
               scro = Scr2
               CALL preloc(*2900,Z(buf1),Ect)
               i = 1
               ielem = 1
               DO j = 1 , n21
                  Z(j) = 0
               ENDDO
               l = n21 + 1
               EXIT
            ELSE
               CALL close(Scr1,Cls)
               ndx1 = ndx2 + 1
               ndx2 = Nsil
               op = Wrt
            ENDIF
         ELSE
            ndx2 = ndx2 - 1
         ENDIF
      ENDDO
   ENDIF
 200  IF ( Elem(ielem+3)==plotel .OR. Elem(ielem+3)==react ) GOTO 400
   CALL locate(*400,Z(buf1),Elem(ielem+3),flag)
   Z(i) = l
   ll = 0
   m = Elem(ielem+5)
   last = buf3 - m
 300  DO WHILE ( l<=last )
      CALL read(*3000,*400,Ect,Z(l),m,0,flag)
      l = l + m
      ll = ll + m
   ENDDO
   GOTO 500
 400  i = i + 2
   ielem = ielem + Incr
   IF ( ielem<=Jlast ) GOTO 200
!
!     PASS ECPT0 ENTRIES LINE BY LINE
!     ATTACH EACH REFERENCED ECT ENTRY WHICH IS NOW IN CORE
!
 500  CALL open(*2900,scri,Z(buf2),Rdrew)
   CALL open(*2900,scro,Z(buf3),Wrtrew)
 600  DO
      CALL read(*800,*700,scri,buf,1,0,flag)
      IF ( buf(1)<0 ) THEN
         CALL read(*3000,*3100,scri,buf(2),1,0,flag)
         k = buf(2)/two24
         ktwo24 = k*two24
         idptr = buf(2) - ktwo24
         kk = Z(k) + idptr - Z(k+1)
         IF ( Z(k)==0 .OR. kk>last ) THEN
            CALL write(scro,buf,2,0)
         ELSE
            j = ((k-1)/2)*Incr + 1
            mm = Elem(j+5)
            buf(1) = mm
            buf(2) = Z(kk) + ktwo24
            CALL write(scro,buf,2,0)
            CALL write(scro,Z(kk+1),mm-1,0)
         ENDIF
      ELSEIF ( buf(1)==0 ) THEN
         CALL write(scro,0,1,1)
         CALL fwdrec(*3000,scri)
      ELSE
         CALL read(*3000,*3100,scri,buf(2),buf(1),0,flag)
         CALL write(scro,buf,buf(1)+1,0)
      ENDIF
   ENDDO
 700  CALL write(scro,0,0,1)
   GOTO 600
!
!     TEST FOR COMPLETION OF STEP
!     IF INCOMPLETE, SET FOR NEXT PASS
!
 800  CALL close(scri,Clsrew)
   CALL close(scro,Clsrew)
   IF ( i>n2 ) THEN
!
!     READ THE EPT INTO CORE (IF PRESENT)
!     FIRST N21 CELLS OF CORE CONTAINS PROPERTIES POINTER TABLE WHICH
!     HAS TWO WORDS PER ELEMENT TYPE, 1ST WORD HAS POINTER TO 1ST WORD
!     OF PROPERTY DATA FOR THAT ELEMENT TYPE. 2ND WORD HAS NUMBER OF
!     PROPERTY CARDS FOR THAT TYPE.
!
      CALL close(Ect,Clsrew)
      DO i = 1 , n21
         Z(i) = 0
      ENDDO
      l = 1
      CALL preloc(*900,Z(buf1),Ept)
      ielem = 1
      lstprp = 0
      l = n21 + 1
      DO ii = 1 , n2 , 2
         IF ( Elem(ielem+6)==lstprp .AND. lstprp/=ppse ) THEN
            n = 4
            IF ( Eltype==ihex2 .OR. Eltype==ihex3 ) n = 2
            Z(ii) = Z(ii-n)
            Z(ii+1) = Z(ii-n+1)
            GOTO 840
         ELSE
            CALL locate(*840,Z(buf1),Elem(ielem+6),flag)
            lstprp = Elem(ielem+6)
            m = Elem(ielem+8)
            Eltype = Elem(ielem+2)
            Z(ii) = l
            DO
               IF ( l+m>=buf3 ) CALL mesage(-8,0,nam)
               CALL read(*3000,*820,Ept,Z(l),m,0,flag)
               l = l + m
            ENDDO
         ENDIF
 820     n = l - Z(ii)
         Z(ii+1) = n/m
         IF ( Eltype/=shear .AND. Eltype/=twist ) THEN
            IF ( m>4 ) GOTO 840
         ENDIF
         i = Z(ii)
         CALL sort(0,0,m,1,Z(i),n)
 840     ielem = ielem + Incr
      ENDDO
      CALL close(Ept,Clsrew)
!
!     DETERMINE IF THE BGPDT AND SIL
!     WILL FIT IN CORE ON TOP OF THE EPT.
!
      number = 4*kscalr + 1
      Iback = 0
      length = buf4 - l - 4*maxel
      IF ( number*Nsil>length ) THEN
!
!     HERE IF ECPT CONSTRUCTION IS TWO PASSES.
!     PASS ECPT0 LINE BY LINE FOR EACH ECT ENTRY, ATTACH PROPERTY DATA
!     IF DEFINED
!
         CALL open(*2900,scro,Z(buf1),Rdrew)
         CALL open(*2900,scri,Z(buf2),Wrtrew)
         oufile = scri
         GOTO 1500
      ENDIF
   ELSE
      k = scri
      scri = scro
      scro = k
      l = n21 + 1
      DO j = 1 , n21
         Z(j) = 0
      ENDDO
      Z(i) = l
      Z(i+1) = ll
      GOTO 300
   ENDIF
!
!     IF YES, READ THE BGPDT,SIL AND GPTT INTO CORE
!
 900  ASSIGN 1000 TO ret
   ipass = 1
   GOTO 2200
!
!     PASS ECPT0 LINE BY LINE
!     FOR EACH ECT ENTRY, 1. ATTACH PROPERTY DATA (IF DEFINED)
!     2. ATTACH BASIC GRID POINT DATA (UNLESS SCALER ELEMENT), AND
!     3. CONVERT GRID PT NOS TO SIL VALUES
!     4. IF TEMPERATURE PROBLEM, ATTACH ELEMENT TEMP(UNLESS SCALAR ELEM)
!
 1000 infile = scro
   oufile = Ecpt
!
!     OPEN ECPT0, ECPT AND GPCT FILES
!
   GOTO 2600
!
!     WRITE PIVOT GRID POINT ON ECPT
!
 1100 IF ( ll-locsil>=Nsil ) THEN
!
!     CLOSE FILES, WRITE TRAILERS AND EXIT.
!
      CALL close(infile,Clsrew)
      CALL close(Gptt,Clsrew)
      CALL close(Ecpt,Clsrew)
      buf(1) = Ect
      CALL rdtrl(buf(1))
      buf(3) = 0
      k = 8192
      k1 = andf(buf(5),k)
      IF ( k1==k ) THEN
         buf(3) = 1
         irigd = 1
      ENDIF
      buf(1) = Ecpt
      DO i = 2 , 7
         buf(i) = 7
      ENDDO
      CALL wrttrl(buf)
      IF ( nogpct==0 ) RETURN
      CALL close(Gpct,Clsrew)
      buf(1) = Gpct
      CALL wrttrl(buf)
      RETURN
   ELSE
      IF ( Iback>0 ) THEN
         CALL bckrec(Gptt)
!
!     RESET /TA1ETT/ VARIABLES
!
         Iback = 0
         Oldeid = 0
         Oldel = 0
         Eorflg = .FALSE.
         Endid = .TRUE.
         CALL read(*3000,*3100,Gptt,iset,1,0,flag)
         IF ( iset/=tempid ) THEN
            WRITE (outpt,99001) Sfm , iset , tempid
            CALL mesage(-61,0,0)
         ENDIF
      ENDIF
      npvt = Z(ll)
      CALL write(Ecpt,npvt,1,0)
      IF ( Z(ll+1)-Z(ll)==1 ) npvt = -npvt
      i = locgpc
   ENDIF
!
!     READ AN ECT LINE FROM ECPT0. SET POINTERS AS A FUNCTION OF ELEM
!     TYPE.  IF ELEMENT IS BAR, PROCESS ORIENTATION VECTOR.  AXIS AND
!     THE STRESS AXIS DEFINITION BASED ON GRID POINTS MA AND SA.
!
 1200 CALL read(*3000,*1400,infile,buf(1),1,0,flag)
   IF ( buf(1)<0 ) GOTO 3300
   IF ( buf(1)==0 ) THEN
!
!     HERE IF NO ELEMENTS CONNECTED TO PIVOT.
!
      CALL write(Ecpt,0,0,1)
      IF ( nogpct/=0 ) CALL write(Gpct,npvt,1,1)
      ll = ll + 1
      CALL fwdrec(*3100,infile)
      GOTO 1100
   ELSE
      CALL read(*3000,*3100,infile,buf(2),buf(1),0,flag)
      ik = buf(2)/two24
      ii = ((ik-1)/2)*Incr + 1
      lx = Elem(ii+12) + 1
      m = Elem(ii+8)
      jscalr = Elem(ii+10)
      mm = lx + Elem(ii+9) - 1
      lq = 4
      IF ( m==0 ) lq = 3
      name = Elem(ii)
      jtemp = Elem(ii+13)
      Eltype = Elem(ii+2)
      ntemp = 1
      IF ( jtemp==4 ) ntemp = Elem(ii+14) - 1
      IF ( Eltype==quadts ) THEN
!
!     FOR QUADTS AND TRIATS ELEMENTS, STORE COORDINATES FOR MATERIAL
!     AND STRESS AXIS DEFINITION
!
         is1 = 12
      ELSEIF ( Eltype==triats ) THEN
         is1 = 10
      ELSE
         IF ( name==cbar ) THEN
!
!     FOR BAR ELEMENTS, STORE COORDINATES AND
!     COORDINATE SYSTEM ID FOR ORIENTATION VECTOR.
!
            kx = 4*(buf(4)-1) + locbgp
            IF ( buf(9)==1 ) THEN
               buf(9) = Z(kx)
            ELSE
               buf(9) = buf(6)
               IF ( buf(9)==0 ) THEN
                  buf(9) = Z(kx)
               ELSE
                  k = 4*(buf(9)-1) + locbgp
                  bufr(6) = zz(k+1) - zz(kx+1)
                  bufr(7) = zz(k+2) - zz(kx+2)
                  bufr(8) = zz(k+3) - zz(kx+3)
                  buf(9) = 0
               ENDIF
            ENDIF
!
!     SAVE INTERNAL GRID NOS AND CONVERT TO SIL NOS.
!
         ENDIF
         GOTO 1900
      ENDIF
      GOTO 2800
   ENDIF
!
!     IF ELEMENT IS NOT A SCALAR ELEMENT,
!     WRITE BGPDT AND ELEMENT TEMPERATURE SECTIONS OF ECPT LINE.
!
 1300 IF ( jscalr==0 ) THEN
!
!     CODE TO WRITE BGPDT AND ELEMENT TEMPERATURE SECTIONS OF ECAT LINE.
!
      DO l = lx , mm
         IF ( gpsav(l)==0 ) THEN
            CALL write(Ecpt,zeros,4,0)
         ELSE
            k = locbgp + 4*(gpsav(l)-1)
            CALL write(Ecpt,Z(k),4,0)
         ENDIF
      ENDDO
      CALL ta1etd(elemid,tgrid,ntemp)
      IF ( Eltype==bar ) tgrid(1) = (tgrid(1)+tgrid(2))/2.0
      CALL write(Ecpt,tgrid,ntemp,0)
   ENDIF
   GOTO 1200
!
!     CLOSE ECPT RECORD. WRITE GPCT RECORD.
!
 1400 CALL write(Ecpt,0,0,1)
!
!     INTERNAL ROUTINE TO SORT AND WRITE THE GPCT
!
   IF ( nogpct==0 ) THEN
      ll = ll + 1
      GOTO 1100
   ELSE
      n = i - locgpc
      CALL sort(0,0,1,1,Z(locgpc),n)
      Z(i) = 0
      j = locgpc
      ii = locgpc
      DO
         IF ( Z(ii)/=Z(ii+1) ) THEN
            nx = Z(ii)/2
            lx = Z(ii) - 2*nx
            IF ( lx/=0 ) nx = -nx
            Z(j) = nx
            j = j + 1
         ENDIF
         ii = ii + 1
         IF ( ii>=i ) THEN
            n = j - locgpc
            CALL write(Gpct,blk,2,0)
            CALL write(Gpct,Z(locgpc),n,1)
            ll = ll + 1
            GOTO 1100
         ENDIF
      ENDDO
      GOTO 2800
   ENDIF
 1500 DO
!
!     READ AN ECT LINE FROM ECT0. SET POINTERS AS FUNCTION OF ELEM TYPE.
!
      CALL read(*1700,*1600,scro,buf,1,0,flag)
      IF ( buf(1)<0 ) GOTO 3300
      IF ( buf(1)==0 ) THEN
!
!     HERE IF NO ELEMENTS CONNECTED TO PIVOT.
!
         CALL write(scri,0,1,1)
         CALL fwdrec(*3000,scro)
      ELSE
         CALL read(*3000,*3100,scro,buf(2),buf(1),0,flag)
         ik = buf(2)/two24
         ii = ((ik-1)/2)*Incr + 1
         m = Elem(ii+8)
         nx = buf(1) + 1
!
!     IF PROPERTY DATA IS DEFINED FOR ELEMENT, WRITE ECT DATA ON SCRI,
!     THEN LOOK UP AND WRITE EPT DATA ON SCRI.
!
         IF ( m==0 ) THEN
!
!     PROPERTY DATA NOT DEFINED. WRITE ECT LINE ON SCRI.
!
            CALL write(scri,buf,nx,0)
         ELSE
            id = buf(3)
            buf(1) = buf(1) + m - 1
            CALL write(scri,buf(1),nx,0)
            ASSIGN 1500 TO ret
            GOTO 2000
         ENDIF
      ENDIF
   ENDDO
!
!     CLOSE RECORD. RETURN FOR ANOTHER PIVOT.
!
 1600 CALL write(scri,0,0,1)
   GOTO 1500
!
!     ALL PIVOTS COMPLETE. CLOSE FILES.
!
 1700 CALL close(scro,Clsrew)
   CALL close(scri,Clsrew)
!
!     READ THE BGPDT, SIL AND, IF TEMPERATURE PROBLEM,
!     THE GPTT INTO CORE.
!
   l = 1
   ASSIGN 1800 TO ret
   GOTO 2200
!
!     SET POINTERS AND BRANCH TO COMMON CODE TO ASSEMBLE ECPT.
!
 1800 infile = scri
   oufile = Ecpt
   ipass = 2
   GOTO 2600
!
!
!     INTERNAL ROUTINE TO SAVE GRID PTS IN AN ECT LINE
!     AND CONVERT GRID PT NOS IN ECT LINE TO SIL VALUES
!
 1900 DO l = lx , mm
      gpsav(l) = 0
      IF ( buf(l)/=0 ) THEN
         gpsav(l) = buf(l)
         k = gpsav(l) + locsil - 1
         buf(l) = Z(k)
         ix = 0
         IF ( Z(k+1)-Z(k)==1 ) ix = 1
         Z(i) = 2*Z(k) + ix
         i = i + 1
      ENDIF
   ENDDO
   IF ( i>=buf3 ) CALL mesage(-8,0,nam)
!
!     IF ONE   PASS, WRITE ECT       SECTION  OF ECPT LINE.
!     IF TWO PASSES, WRITE ECT + EPT SECTIONS OF ECPT LINE.
!
   id = buf(3)
   nx = buf(1) + 2 - lq
   buf(1) = Elem(ii+2)
   buf(2) = buf(2) - ik*two24
   elemid = buf(2)
   CALL write(Ecpt,buf(1),2,0)
   CALL write(Ecpt,buf(lq),nx,0)
   IF ( ipass==2 ) GOTO 1300
!
!     IF PROPERTY DATA IS DEFINED, LOOK UP AND WRITE EPT SECTION OF ECPT
!
   IF ( m==0 ) GOTO 1300
   ASSIGN 1300 TO ret
!
!
!     INTERNAL ROUTINE TO ATTACH EPT DATA
!
 2000 locx = Z(ik)
   IF ( locx==0 ) THEN
      buf(1) = Elem(ii)
      buf(2) = Elem(ii+1)
      n = 11
      CALL mesage(-30,n,buf)
      CALL mesage(j,file,nam)
      GOTO 99999
   ELSE
      khi = Z(ik+1)
      ASSIGN 2100 TO ret1
      ASSIGN 3200 TO ret2
!
!
!     INTERNAL BINARY SEARCH ROUTINE
!
      klo = 1
      k = (klo+khi+1)/2
      DO
         kx = (k-1)*m + locx
         IF ( id<Z(kx) ) THEN
            khi = k
         ELSEIF ( id==Z(kx) ) THEN
            GOTO ret1
         ELSE
            klo = k
         ENDIF
         IF ( khi-klo<1 ) THEN
            GOTO ret2
         ELSEIF ( khi-klo==1 ) THEN
            IF ( k==klo ) THEN
               k = khi
            ELSE
               k = klo
            ENDIF
            klo = khi
         ELSE
            k = (klo+khi+1)/2
         ENDIF
      ENDDO
   ENDIF
 2100 CALL write(oufile,Z(kx+1),m-1,0)
   GOTO ret
!
!     INTERNAL ROUTINE TO READ THE BGPDT, SIL AND GPTT INTO CORE
!
 2200 nbgp = 0
   locbgp = l
   IF ( kscalr/=0 ) THEN
      CALL open(*2900,Bgpdt,Z(buf1),Rdrew)
      CALL fwdrec(*3000,Bgpdt)
      nbgp = 4*Nsil
      CALL read(*3000,*3100,Bgpdt,Z(locbgp),nbgp,1,flag)
      CALL close(Bgpdt,Clsrew)
   ENDIF
   l = l + nbgp
   CALL open(*2900,Sil,Z(buf1),Rdrew)
   CALL fwdrec(*3000,Sil)
   locsil = locbgp + nbgp
   CALL read(*3000,*3100,Sil,Z(locsil),Nsil,1,flag)
   CALL close(Sil,Clsrew)
   nx = locsil + Nsil
   Z(nx) = Luset + 1
   loctmp = nx + 1
   ntmp = loctmp - 1
   Record = .FALSE.
   Itemp = tempid
   Iback = 0
   IF ( tempid==0 ) GOTO 2500
   file = Gptt
   CALL open(*2900,Gptt,Z(buf4),Rdrew)
   CALL read(*3000,*2300,Gptt,Z(loctmp),buf3-loctmp,0,nid)
   CALL mesage(-8,0,nam)
 2300 itmpid = loctmp + 2
   ntmpid = loctmp + nid - 3
   DO ijk = itmpid , ntmpid , 3
      IF ( tempid==Z(ijk) ) GOTO 2400
   ENDDO
   buf(1) = tempid
   buf(2) = 0
   n = 44
   CALL mesage(-30,n,buf)
   CALL mesage(j,file,nam)
   GOTO 99999
 2400 Idftmp = Z(ijk+1)
   IF ( Idftmp/=-1 ) deftmp = zz(ijk+1)
   n = Z(ijk+2)
   IF ( n/=0 ) THEN
      Record = .TRUE.
      n = n - 1
      IF ( n/=0 ) THEN
         DO ijk = 1 , n
            CALL fwdrec(*3000,Gptt)
         ENDDO
      ENDIF
!
!     READ SET ID AND VERIFY FOR CORRECTNESS
!
      CALL read(*3000,*3100,Gptt,iset,1,0,flag)
      IF ( iset/=tempid ) THEN
         WRITE (outpt,99001) Sfm , iset , tempid
         CALL mesage(-61,0,nam)
      ENDIF
!
!     INITIALIZE /TA1ETT/ VARIABLES
!
      Oldeid = 0
      Oldel = 0
      Eorflg = .FALSE.
      Endid = .TRUE.
   ENDIF
 2500 GOTO ret
!
!
!     INTERNAL ROUTINE TO OPEN SCRATCH, ECPT AND GPCT FILES
!
 2600 CALL open(*2900,infile,Z(buf1),Rdrew)
   CALL open(*2900,Ecpt,Z(buf2),Wrtrew)
   CALL fname(Ecpt,buf)
   CALL write(Ecpt,buf,2,1)
   nogpct = 0
   CALL open(*2700,Gpct,Z(buf3),Wrtrew)
   nogpct = 1
   CALL fname(Gpct,buf)
   CALL write(Gpct,buf,2,1)
 2700 ll = locsil
   locgpc = ntmp + 1
   GOTO 1100
 2800 is2 = is1 + 9
   DO ist = is1 , is2 , 3
      igp = buf(ist)
      IF ( igp/=0 ) THEN
         k = 4*(igp-1) + locbgp
         bufr(ist) = zz(k+1)
         bufr(ist+1) = zz(k+2)
         bufr(ist+2) = zz(k+3)
      ENDIF
   ENDDO
   GOTO 1900
!
!     FATAL ERROR MESAGES
!
 2900 j = -1
   CALL mesage(j,file,nam)
   GOTO 99999
 3000 j = -2
   CALL mesage(j,file,nam)
   GOTO 99999
 3100 j = -3
   CALL mesage(j,file,nam)
   GOTO 99999
 3200 buf(1) = elemid
   buf(2) = id
   n = 10
   CALL mesage(-30,n,buf)
   CALL mesage(j,file,nam)
   GOTO 99999
 3300 buf(1) = 0
   buf(2) = 0
   n = 14
   CALL mesage(-30,n,buf)
   CALL mesage(j,file,nam)
99001 FORMAT (A25,' 4021, TA1B HAS PICKED UP TEMPERATURE SET',I9,' AND NOT THE REQUESTED SET',I9,1H.)
99999 END SUBROUTINE ta1b

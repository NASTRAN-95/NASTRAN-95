
SUBROUTINE adrprt(Casecc,Pkf,Spline,Sila,Useta,Freq,Nfreq,Ncore,Nload)
   IMPLICIT NONE
   REAL Dum1(6) , Head(96) , Z(1)
   INTEGER Ibit(64) , Ii , Incr , Ito , Itwo(32) , Iz(1) , Nlpp , Nn , Out , Sysbuf
   COMMON /bitpos/ Ibit
   COMMON /output/ Head
   COMMON /system/ Sysbuf , Out , Dum1 , Nlpp
   COMMON /two   / Itwo
   COMMON /unpakx/ Ito , Ii , Nn , Incr
   COMMON /zzzzzz/ Iz
   INTEGER Casecc , Ncore , Nfreq , Nload , Pkf , Sila , Spline , Useta
   REAL Freq(1)
   INTEGER all , extid , i , iaero , ibuf1 , ibuf2 , icc , id , iend , ipsil , ipuset , iret , iret1 , irow , iset , isetno ,       &
         & ismal , izsil , izspl , izuset , izvect , j , k , l , lcc , lcs , lsp(2) , m , mask , mm , n , nam(2) , nextra , nhfssu ,&
         & nlppp , nr , nset , nsil , nskip , nspl , nvect , nwr , setno , trl(7)
   INTEGER andf
   REAL buf(12) , dum , tsave(96)
   EXTERNAL andf
!
!     ADRPRT FORMATS PKF BY USER SET REQUEST FOR EACH FREQUENCY
!
   EQUIVALENCE (Z(1),Iz(1))
   DATA iaero/176/ , lcs/200/
   DATA nhfssu , nam/4HFSSU , 4HADRP , 4HRT  /
   DATA lsp/200 , 2/
!
!     CORE LAYOUT
!       FREQ LIST          NFREQ
!       SPLINE TRIPLETS    3*K POINTS
!       SILS FOR K POINTS  1 PER K
!       USET MASKS         6*K POINTS
!       CASECC RECORD      TRL(4) LONG
!       LOAD VECTOR        K SIZE
!       BUFFERS            2 * SYSBUF
!
   mask = Ibit(19)
   mask = Itwo(mask)
   DO i = 1 , 96
      tsave(i) = Head(i)
   ENDDO
   ibuf1 = Ncore - Sysbuf - 1
   ibuf2 = ibuf1 - Sysbuf
   izspl = Nfreq
   nr = ibuf2 - izspl
   CALL preloc(*500,Z(ibuf1),Spline)
   CALL locate(*500,Z(ibuf1),lsp,dum)
   CALL read(*500,*100,Spline,Z(izspl+1),nr,0,nwr)
!
!     ERROR MESSAGES
!
   CALL mesage(8,0,nam)
   GOTO 500
 100  nspl = nwr
   izsil = izspl + nwr
   CALL close(Spline,1)
!
!     FIND SMALLEST SILGA POINTER (-1+NEXTRA = NSKIP ON SILA)
!
   ismal = 1000000
   DO i = 1 , nspl , 3
      ismal = min0(ismal,Iz(izspl+i+1))
   ENDDO
   ismal = ismal - 1
   trl(1) = Sila
   CALL rdtrl(trl)
   IF ( trl(1)>=0 ) THEN
      nextra = trl(3)
      CALL gopen(Sila,Z(ibuf1),0)
      nskip = ismal + nextra
      nr = ibuf2 - izsil
      CALL read(*500,*500,Sila,Z(izsil+1),-nskip,0,nwr)
      CALL read(*500,*200,Sila,Z(izsil+1),nr,0,nwr)
      CALL mesage(8,0,nam)
   ENDIF
   GOTO 500
 200  nsil = nwr
   CALL close(Sila,1)
   izuset = izsil + nwr
   nr = ibuf2 - izuset
   nskip = Iz(izsil+1) - 1
   CALL open(*500,Useta,Z(ibuf1),0)
   CALL fwdrec(*500,Useta)
   CALL read(*500,*500,Useta,Z(izuset+1),-nskip,0,nwr)
   CALL read(*500,*300,Useta,Z(izuset+1),nr,0,nwr)
   CALL mesage(8,0,nam)
   GOTO 500
 300  icc = izuset + nwr
   CALL close(Useta,1)
!
!     ADJUST SILA AND USET POINTERS FOR SHRUNKEN LISTS
!
   DO i = 1 , nspl , 3
      Iz(izspl+i+1) = Iz(izspl+i+1) - ismal
   ENDDO
   DO i = 1 , nsil
      Iz(izsil+i) = Iz(izsil+i) - nskip
   ENDDO
   CALL bug(nhfssu,60,Z,icc)
   trl(1) = Casecc
   CALL rdtrl(trl)
   lcc = trl(4) + 1
   izvect = icc + lcc
   trl(1) = Pkf
   CALL rdtrl(trl)
   Ito = 3
   Ii = 1
   Nn = trl(3)
   Incr = 1
   nvect = trl(3)*2
   iend = izvect + nvect
   IF ( iend>ibuf2 ) THEN
      CALL mesage(8,0,nam)
   ELSE
      CALL open(*500,Casecc,Z(ibuf1),0)
      CALL fwdrec(*500,Casecc)
      CALL open(*500,Pkf,Z(ibuf2),0)
      CALL fwdrec(*500,Pkf)
!
!     LOOP OVER NLOAD (CASECC RECORDS)
!     THEN LOOP OVER NFREQ  (PKF COLUMNS)
!     OUTPUT K POINTS FOR SET LIST
!
      DO k = 1 , Nload
         CALL read(*500,*320,Casecc,Z(icc+1),lcc,1,nwr)
 320     setno = Iz(icc+iaero)
         all = 0
         DO i = 1 , 96
            Head(i) = Z(icc+i+38)
         ENDDO
         IF ( setno<0 ) THEN
            all = 1
         ELSEIF ( setno==0 ) THEN
            GOTO 440
         ELSE
            isetno = lcs + Iz(icc+lcs) + 1 + icc
            DO
               iset = isetno + 2
               nset = Iz(isetno+1) + iset - 1
               IF ( Iz(isetno)==setno ) EXIT
               isetno = nset + 1
               IF ( isetno>=izvect ) THEN
                  all = 1
                  EXIT
               ENDIF
            ENDDO
         ENDIF
         DO j = 1 , Nfreq
            nlppp = Nlpp
            CALL unpack(*330,Pkf,Z(izvect+1))
            GOTO 340
 330        CALL zeroc(Z(izvect+1),nvect)
!
!     PRINT LOOP
!
 340        IF ( all==0 ) THEN
               i = iset
               GOTO 360
            ELSE
               ASSIGN 350 TO iret
               l = 1
               GOTO 410
            ENDIF
 350        l = l + 3
            IF ( l<nspl ) GOTO 410
            CYCLE
 360        IF ( i==nset ) GOTO 380
            IF ( Iz(i+1)>0 ) GOTO 380
            id = Iz(i)
            n = -Iz(i+1)
            i = i + 1
            ASSIGN 370 TO iret1
            GOTO 400
 370        id = id + 1
            IF ( id>n ) GOTO 390
            GOTO 400
 380        id = Iz(i)
            ASSIGN 390 TO iret1
            GOTO 400
 390        i = i + 1
            IF ( i>nset ) CYCLE
            GOTO 360
!
!     LOCATE ELEMENT THEN  PRINT DATA
!
 400        ASSIGN 420 TO iret
            CALL bisloc(*420,id,Iz(izspl+1),3,nspl/3,l)
 410        extid = Iz(izspl+l)
            ipsil = Iz(izspl+l+1)
            irow = Iz(izspl+l+2)*2 - 1 + izvect
            ipuset = Iz(izsil+ipsil) + izuset - 1
!
!     PRINT
!
            IF ( nlppp>=Nlpp ) THEN
               CALL page1
               WRITE (Out,99001) j , Freq(j)
99001          FORMAT (44X,42HAERODYNAMIC LOADS  (UNIT DYNAMIC PRESSURE),/30X,7HVECTOR ,I8,10X,12HFREQUENCY = ,1P,E14.6,7H  HERTZ,/,&
                      &11H BOX OR    ,12X,7HT1 / R1,23X,7HT2 / R2,23X,7HT3 / R3,/,11H BODY ELMT.,3(4X,4HREAL,10X,12HIMAGINARY   ))
               nlppp = 1
            ENDIF
            DO m = 1 , 6
               mm = m*2 - 1
               buf(mm) = 0.0
               buf(mm+1) = 0.0
               IF ( andf(Iz(ipuset+m),mask)/=0 ) THEN
                  buf(mm) = Z(irow)
                  buf(mm+1) = Z(irow+1)
                  irow = irow + 2
               ENDIF
            ENDDO
            WRITE (Out,99002) extid , buf
99002       FORMAT (1H0,I10,6(1P,E15.6),/11X,6(1P,E15.6))
            nlppp = nlppp + 3
            GOTO iret
 420        GOTO iret1
         ENDDO
 440     IF ( k/=Nload ) THEN
            CALL rewind(Pkf)
            CALL skprec(Pkf,1)
         ENDIF
      ENDDO
   ENDIF
!
!     CLOSE UP AND RETURN
!
 500  CALL close(Casecc,1)
   CALL close(Pkf,1)
   CALL close(Sila,1)
   CALL close(Spline,1)
   DO i = 1 , 96
      Head(i) = tsave(i)
   ENDDO
   CALL page2(1)
   RETURN
END SUBROUTINE adrprt

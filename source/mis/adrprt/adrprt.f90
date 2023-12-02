!*==adrprt.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE adrprt(Casecc,Pkf,Spline,Sila,Useta,Freq,Nfreq,Ncore,Nload)
   USE c_bitpos
   USE c_output
   USE c_system
   USE c_two
   USE c_unpakx
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Casecc
   INTEGER :: Pkf
   INTEGER :: Spline
   INTEGER :: Sila
   INTEGER :: Useta
   REAL , DIMENSION(1) :: Freq
   INTEGER :: Nfreq
   INTEGER :: Ncore
   INTEGER :: Nload
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: all , extid , i , ibuf1 , ibuf2 , icc , id , iend , ipsil , ipuset , iret , iret1 , irow , iset , isetno , ismal ,    &
            & izsil , izspl , izuset , izvect , j , k , l , lcc , m , mask , mm , n , nextra , nlppp , nr , nset , nsil , nskip ,   &
            & nspl , nvect , nwr , setno
   REAL , DIMENSION(12) :: buf
   REAL :: dum
   INTEGER , SAVE :: iaero , lcs , nhfssu
   INTEGER , DIMENSION(2) , SAVE :: lsp , nam
   INTEGER , DIMENSION(7) :: trl
   REAL , DIMENSION(96) :: tsave
   REAL , DIMENSION(1) :: z
   EXTERNAL andf , bisloc , bug , close , fwdrec , gopen , locate , mesage , open , page1 , page2 , preloc , rdtrl , read , rewind ,&
          & skprec , unpack , zeroc
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     ADRPRT FORMATS PKF BY USER SET REQUEST FOR EACH FREQUENCY
!
   !>>>>EQUIVALENCE (Z(1),Iz(1))
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
   mask = ibit(19)
   mask = itwo(mask)
   DO i = 1 , 96
      tsave(i) = head(i)
   ENDDO
   ibuf1 = Ncore - sysbuf - 1
   ibuf2 = ibuf1 - sysbuf
   izspl = Nfreq
   nr = ibuf2 - izspl
   CALL preloc(*400,z(ibuf1),Spline)
   CALL locate(*400,z(ibuf1),lsp,dum)
   CALL read(*400,*100,Spline,z(izspl+1),nr,0,nwr)
!
!     ERROR MESSAGES
!
   CALL mesage(8,0,nam)
   GOTO 400
 100  nspl = nwr
   izsil = izspl + nwr
   CALL close(Spline,1)
!
!     FIND SMALLEST SILGA POINTER (-1+NEXTRA = NSKIP ON SILA)
!
   ismal = 1000000
   DO i = 1 , nspl , 3
      ismal = min0(ismal,iz(izspl+i+1))
   ENDDO
   ismal = ismal - 1
   trl(1) = Sila
   CALL rdtrl(trl)
   IF ( trl(1)>=0 ) THEN
      nextra = trl(3)
      CALL gopen(Sila,z(ibuf1),0)
      nskip = ismal + nextra
      nr = ibuf2 - izsil
      CALL read(*400,*400,Sila,z(izsil+1),-nskip,0,nwr)
      CALL read(*400,*200,Sila,z(izsil+1),nr,0,nwr)
      CALL mesage(8,0,nam)
   ENDIF
   GOTO 400
 200  nsil = nwr
   CALL close(Sila,1)
   izuset = izsil + nwr
   nr = ibuf2 - izuset
   nskip = iz(izsil+1) - 1
   CALL open(*400,Useta,z(ibuf1),0)
   CALL fwdrec(*400,Useta)
   CALL read(*400,*400,Useta,z(izuset+1),-nskip,0,nwr)
   CALL read(*400,*300,Useta,z(izuset+1),nr,0,nwr)
   CALL mesage(8,0,nam)
   GOTO 400
 300  icc = izuset + nwr
   CALL close(Useta,1)
!
!     ADJUST SILA AND USET POINTERS FOR SHRUNKEN LISTS
!
   DO i = 1 , nspl , 3
      iz(izspl+i+1) = iz(izspl+i+1) - ismal
   ENDDO
   DO i = 1 , nsil
      iz(izsil+i) = iz(izsil+i) - nskip
   ENDDO
   CALL bug(nhfssu,60,z,icc)
   trl(1) = Casecc
   CALL rdtrl(trl)
   lcc = trl(4) + 1
   izvect = icc + lcc
   trl(1) = Pkf
   CALL rdtrl(trl)
   ito = 3
   ii = 1
   nn = trl(3)
   incr = 1
   nvect = trl(3)*2
   iend = izvect + nvect
   IF ( iend>ibuf2 ) THEN
      CALL mesage(8,0,nam)
   ELSE
      CALL open(*400,Casecc,z(ibuf1),0)
      CALL fwdrec(*400,Casecc)
      CALL open(*400,Pkf,z(ibuf2),0)
      CALL fwdrec(*400,Pkf)
!
!     LOOP OVER NLOAD (CASECC RECORDS)
!     THEN LOOP OVER NFREQ  (PKF COLUMNS)
!     OUTPUT K POINTS FOR SET LIST
!
      DO k = 1 , Nload
         spag_nextblock_1 = 1
         SPAG_DispatchLoop_1: DO
            SELECT CASE (spag_nextblock_1)
            CASE (1)
               CALL read(*400,*305,Casecc,z(icc+1),lcc,1,nwr)
 305           setno = iz(icc+iaero)
               all = 0
               DO i = 1 , 96
                  head(i) = z(icc+i+38)
               ENDDO
               IF ( setno<0 ) THEN
                  all = 1
               ELSEIF ( setno==0 ) THEN
                  spag_nextblock_1 = 2
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  isetno = lcs + iz(icc+lcs) + 1 + icc
                  SPAG_Loop_2_1: DO
                     iset = isetno + 2
                     nset = iz(isetno+1) + iset - 1
                     IF ( iz(isetno)==setno ) EXIT SPAG_Loop_2_1
                     isetno = nset + 1
                     IF ( isetno>=izvect ) THEN
                        all = 1
                        EXIT SPAG_Loop_2_1
                     ENDIF
                  ENDDO SPAG_Loop_2_1
               ENDIF
               DO j = 1 , Nfreq
                  spag_nextblock_2 = 1
                  SPAG_DispatchLoop_2: DO
                     SELECT CASE (spag_nextblock_2)
                     CASE (1)
                        nlppp = nlpp
                        CALL unpack(*306,Pkf,z(izvect+1))
                        spag_nextblock_2 = 2
                        CYCLE SPAG_DispatchLoop_2
 306                    CALL zeroc(z(izvect+1),nvect)
                        spag_nextblock_2 = 2
                     CASE (2)
!
!     PRINT LOOP
!
                        IF ( all==0 ) THEN
                           i = iset
                           spag_nextblock_2 = 3
                        ELSE
                           ASSIGN 308 TO iret
                           l = 1
                           spag_nextblock_2 = 6
                        ENDIF
                        CYCLE
 308                    l = l + 3
                        IF ( l>=nspl ) CYCLE
                        spag_nextblock_2 = 6
                     CASE (3)
                        IF ( i==nset ) THEN
                           spag_nextblock_2 = 4
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        IF ( iz(i+1)>0 ) THEN
                           spag_nextblock_2 = 4
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        id = iz(i)
                        n = -iz(i+1)
                        i = i + 1
                        ASSIGN 310 TO iret1
                        spag_nextblock_2 = 5
                        CYCLE SPAG_DispatchLoop_2
 310                    id = id + 1
                        IF ( id<=n ) THEN
                           spag_nextblock_2 = 5
                           CYCLE SPAG_DispatchLoop_2
                        ENDIF
                        GOTO 312
                     CASE (4)
                        id = iz(i)
                        ASSIGN 312 TO iret1
                        spag_nextblock_2 = 5
                        CYCLE SPAG_DispatchLoop_2
 312                    i = i + 1
                        IF ( i>nset ) CYCLE
                        spag_nextblock_2 = 3
                     CASE (5)
!
!     LOCATE ELEMENT THEN  PRINT DATA
!
                        ASSIGN 314 TO iret
                        CALL bisloc(*314,id,iz(izspl+1),3,nspl/3,l)
                        spag_nextblock_2 = 6
                     CASE (6)
                        extid = iz(izspl+l)
                        ipsil = iz(izspl+l+1)
                        irow = iz(izspl+l+2)*2 - 1 + izvect
                        ipuset = iz(izsil+ipsil) + izuset - 1
!
!     PRINT
!
                        IF ( nlppp>=nlpp ) THEN
                           CALL page1
                           WRITE (out,99001) j , Freq(j)
99001                      FORMAT (44X,42HAERODYNAMIC LOADS  (UNIT DYNAMIC PRESSURE),/30X,7HVECTOR ,I8,10X,12HFREQUENCY = ,1P,E14.6,&
                                  &7H  HERTZ,/,11H BOX OR    ,12X,7HT1 / R1,23X,7HT2 / R2,23X,7HT3 / R3,/,11H BODY ELMT.,           &
                                 & 3(4X,4HREAL,10X,12HIMAGINARY   ))
                           nlppp = 1
                        ENDIF
                        DO m = 1 , 6
                           mm = m*2 - 1
                           buf(mm) = 0.0
                           buf(mm+1) = 0.0
                           IF ( andf(iz(ipuset+m),mask)/=0 ) THEN
                              buf(mm) = z(irow)
                              buf(mm+1) = z(irow+1)
                              irow = irow + 2
                           ENDIF
                        ENDDO
                        WRITE (out,99002) extid , buf
99002                   FORMAT (1H0,I10,6(1P,E15.6),/11X,6(1P,E15.6))
                        nlppp = nlppp + 3
                        GOTO iret
 314                    GOTO iret1
                     END SELECT
                  ENDDO SPAG_DispatchLoop_2
               ENDDO
               spag_nextblock_1 = 2
            CASE (2)
               IF ( k/=Nload ) THEN
                  CALL rewind(Pkf)
                  CALL skprec(Pkf,1)
               ENDIF
               EXIT SPAG_DispatchLoop_1
            END SELECT
         ENDDO SPAG_DispatchLoop_1
      ENDDO
   ENDIF
!
!     CLOSE UP AND RETURN
!
 400  CALL close(Casecc,1)
   CALL close(Pkf,1)
   CALL close(Sila,1)
   CALL close(Spline,1)
   DO i = 1 , 96
      head(i) = tsave(i)
   ENDDO
   CALL page2(1)
END SUBROUTINE adrprt

!*==outmsc.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE outmsc() !HIDESTARS (*,*)
   USE c_blank
   USE c_machin
   USE c_system
   USE c_type
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(20) :: block
   INTEGER :: buf1 , endfil , endrec , i , ii , input , iret , j , k , k1 , k2 , key , keyx , l , lcor , lend , nskip , nwd , out
   LOGICAL :: dp
   INTEGER , DIMENSION(3) :: dx
   REAL(REAL64) , DIMENSION(1) :: dxns
   INTEGER , DIMENSION(7) , SAVE :: hdr
   INTEGER , DIMENSION(7) :: hdrx , mcb
   INTEGER , DIMENSION(13) , SAVE :: inp
   CHARACTER(19) , SAVE :: mo2
   INTEGER , DIMENSION(2) :: name , tapcod , tmp
   INTEGER , DIMENSION(2) , SAVE :: none , sub
   REAL , DIMENSION(1) :: xns
   EXTERNAL close , endget , fname , getstr , korsz , mesage , open , page1 , rdtrl , read , rectyp
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     COPY DATA BLOCK(S) TO FORTRAN UNIT, IN MSC/OUTPUT2 COMPATIBLE
!     RECORD FORMATS.
!
!     DMAP CALL -
!     OUTPUT2  IN1,IN2,IN3,IN4,IN5/ /V,N,P1/V,N,P2/V,N,P3/V,N,P4/V,N,P5/
!                                    V,N,P6 $
!
!     THIS ROUTINE IS CALLED ONLY BY OUTPT2
!     SEE OUTPT2 FOR PARAMETERS P1,P2,...,P6. (P6 = *MSC*)
!
!     IF P1 .NE. -9, ALTERNATE RETURN 1, OTHERWISE RETURN 2.
!
!     WRITTEN BY G.CHAN/UNISYS  3/93
!
   !>>>>EQUIVALENCE (Xns(1),Z(1))
   !>>>>EQUIVALENCE (Xns(1),Dxns(1))
   DATA hdr/4HNAST , 4HRAN  , 4HFORT , 4H TAP , 4HE ID , 4H COD , 4HE - /
   DATA inp/4HUT1  , 4HUT2  , 4HUT3  , 4HINPT , 4HINP1 , 4HINP2 , 4HINP3 , 4HINP4 , 4HINP5 , 4HINP6 , 4HINP7 , 4HINP8 , 4HINP9/
   DATA mo2/'. MODULE OUTPUT2 - '/
   DATA none , sub/4H (NO , 4HNE)  , 4HOUTP , 4HUT2*/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         WRITE (nout,99001) uim
99001    FORMAT (A29,'. USER REQUESTED RECORDS IN MSC/OUTPUT2 COMPATIBLE',' RECORDS')
         endfil = 0
         endrec = 0
         lcor = korsz(z(1))
         buf1 = lcor - ibuf + 1
         IF ( buf1<=0 ) CALL mesage(-8,lcor,sub)
         lend = buf1 - 1
         out = p2
         tapcod(1) = p3(1)
         tapcod(2) = p3(2)
         IF ( p1==-9 ) THEN
!
!     FINAL CALL TO OUTPUT2, P1 = -9
!
            WRITE (out) endfil
            RETURN 2
         ELSEIF ( p1==-3 ) THEN
!
!     OBTAIN LIST OF DATA BLOCKS ON FORTRAN TAPE, P1 = -3
!
            REWIND out
            READ (out) key
            keyx = 3
            IF ( key/=keyx ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            READ (out) dx
            READ (out) key
            keyx = 7
            IF ( key/=keyx ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            READ (out) hdrx
            DO k = 1 , 7
               IF ( hdrx(k)/=hdr(k) ) THEN
                  spag_nextblock_1 = 7
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            READ (out) key
            keyx = 2
            IF ( key/=keyx ) THEN
               spag_nextblock_1 = 9
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            READ (out) tmp
            IF ( tmp(1)/=p3(1) .OR. tmp(2)/=p3(2) ) THEN
               WRITE (nout,99002) uwm , tmp , p3
99002          FORMAT (A25,' 4141. FORTRAN TAPE ID CODE - ',2A4,' DOES NOT MATCH OUTPUT2 THIRD PARAMETER NAME - ',2A4)
            ENDIF
            ASSIGN 60 TO iret
            nskip = 1
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( p1<=-2 ) THEN
            WRITE (nout,99003) ufm , mo2 , p1
99003       FORMAT (A23,' 4120',A19,'ILLEGAL FIRST PARAMETER ',I3)
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ELSE
            IF ( p1<=0 ) THEN
               spag_nextblock_1 = 3
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     SKIP FORWARD n DATA BLOCKS, P1 = n
!
            i = 1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
         READ (out) key
         keyx = 2
         IF ( key/=keyx ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         READ (out) tmp
         READ (out) key
         IF ( key>=0 ) THEN
            WRITE (nout,99004) sfm , key
99004       FORMAT (A25,' 2190. ILLEGAL VALUE FOR KEY =',I10)
            spag_nextblock_1 = 10
         ELSE
            ASSIGN 20 TO iret
            nskip = 1
            spag_nextblock_1 = 6
         ENDIF
         CYCLE
 20      i = i + 1
         IF ( i<=p1 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
         IF ( p1==-1 ) THEN
            REWIND out
            key = 3
            WRITE (out) key
            WRITE (out) d
            key = 7
            WRITE (out) key
            WRITE (out) hdr
            key = 2
            WRITE (out) key
            WRITE (out) p3
            endrec = endrec - 1
            WRITE (out) endrec
            WRITE (out) endfil
            endrec = 0
            WRITE (nout,99005) uim , p3
99005       FORMAT (A29,' FROM OUPUT2 MODULE.  THE LABEL IS ',2A4)
         ENDIF
!
 40      DO ii = 1 , 5
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  input = 100 + ii
                  mcb(1) = input
                  CALL rdtrl(mcb(1))
                  IF ( mcb(1)<=0 ) CYCLE
                  CALL fname(input,name)
                  IF ( name(1)==none(1) .AND. name(2)==none(2) ) CYCLE
                  block(1) = input
                  nwd = nwds(mcb(5))
                  dp = mcb(5)==2 .OR. mcb(5)==4
!
!     OPEN INPUT DATA BLOCK TO READ WITH REWIND
!
                  CALL open(*100,input,z(buf1),0)
                  key = 2
                  WRITE (out) key
                  WRITE (out) name
                  endrec = endrec - 1
                  WRITE (out) endrec
                  key = 7
                  WRITE (out) key
                  WRITE (out) mcb
                  endrec = endrec - 1
                  WRITE (out) endrec
                  spag_nextblock_2 = 2
               CASE (2)
!
!     COPY CONTENTS OF INPUT DATA BLOCK ONTO FILE
!
                  CALL rectyp(input,k)
                  key = 1
                  WRITE (out) key
                  WRITE (out) k
                  IF ( k==0 ) THEN
                     DO
!
!     NON-STRING RECORD
!     MAKE SURE EACH RECORD IS NOT LONGER THAN P4 WORDS
!
                        CALL read(*46,*42,input,z(1),lend,0,k1)
                        DO i = 1 , lend , p4
                           key = lend - i + 1
                           IF ( key>=p4 ) key = p4
                           k2 = i + key - 1
                           WRITE (out) key
                           WRITE (out) (z(k),k=i,k2)
                        ENDDO
                     ENDDO
                  ELSE
!
!     STRING RECORD
!     BLOCK(2) = STRING TYPE, 1,2,3 OR 4
!     BLOCK(4) = FIRST (OR LAST) ROW POSITION ON A MATRIX COLUMN
!     BLOCK(5) = POINTER TO STRING, W.R.T. XNS ARRAY
!     BLOCK(6) = NO. OF TERMS IN STRING
!
                     block(8) = -1
                  ENDIF
                  DO
                     CALL getstr(*44,block)
                     key = block(6)*nwd
                     WRITE (out) key
!
!     NEXT 3 LINES, ORIGINATED FROM MSC/OUTPUT2, DO NOT WORK FOR D.P.
!     DATA ON VAX, AND POSSIBLY SILICON-GRAPHICS. THEY ARE REPLACED BY
!     NEXT 8 LINES BELOW. BESIDE, TO WORK ON PROPER D.P. DATA BOUNDARY,
!     THE K1 IN THE FOLLOWING LINE SHOULD BE  K1 = (BLOCK(5)-1)*NWD+1
!
!     K1  = BLOCK(5)
!     K2  = K1 + KEY - 1
!     WRITE (OUT) BLOCK(4),(XNS(K),K=K1,K2)
!
                     k1 = block(5)*nwd
                     k2 = k1 + key - 1
                     IF ( dp ) THEN
                        k1 = k1/2
                        k2 = k2/2
                        WRITE (out) block(4) , (dxns(k),k=k1,k2)
                     ELSE
                        WRITE (out) block(4) , (xns(k),k=k1,k2)
                     ENDIF
!
                     CALL endget(block)
                  ENDDO
 42               DO i = 1 , k1 , p4
                     key = k1 - i + 1
                     IF ( key>=p4 ) key = p4
                     k2 = i + key - 1
                     WRITE (out) key
                     WRITE (out) (z(k),k=i,k2)
                  ENDDO
!
 44               endrec = endrec - 1
                  WRITE (out) endrec
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
!
!     CLOSE INPUT DATA BLOCK WITH REWIND
!
 46               CALL close(input,1)
                  WRITE (out) endfil
                  endrec = 0
                  WRITE (nout,99006) uim , name , out , inp(p2-10) , mcb
99006             FORMAT (A29,' 4144. DATA BLOCK ',2A4,' WRITTEN ON FORTRAN UNIT ',I3,2H (,A4,1H),/5X,'TRAILER =',6I7,I11)
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
!
         ENDDO
!
!     CLOSE FORTRAN TAPE WITHOUT END-OF-FILE AND WITHOUT REWIND
!
         RETURN 1
 60      k = 0
         spag_nextblock_1 = 4
      CASE (4)
         CALL page1
         WRITE (nout,99007) inp(p2-10) , out
99007    FORMAT (//42X,'CONTENTS OF ',A4,', FORTRAN UNIT',I3,/46X,'FILE',18X,'NAME',/)
         spag_nextblock_1 = 5
      CASE (5)
         READ (out) key
         IF ( key<0 ) THEN
            WRITE (nout,99008) sfm , mo2
99008       FORMAT (A25,' 4415',A19,'SHORT RECORD ENCOUNTERED')
            spag_nextblock_1 = 10
            CYCLE SPAG_DispatchLoop_1
         ELSEIF ( key==0 ) THEN
            ASSIGN 40 TO iret
            nskip = k + 1
            IF ( nskip>0 ) REWIND out
         ELSE
            READ (out) tmp
            ASSIGN 80 TO iret
            nskip = 1
         ENDIF
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 80      k = k + 1
         WRITE (nout,99009) k , tmp
99009    FORMAT (45X,I5,18X,2A4)
         IF ( mod(k,nlpp)/=0 ) THEN
            spag_nextblock_1 = 5
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 4
      CASE (6)
!
!     SKIP NSKIP FILES ON FORTRAN TAPE
!
         IF ( nskip/=0 ) THEN
            DO j = 1 , nskip
               SPAG_Loop_2_1: DO
                  READ (out) keyx
                  IF ( keyx<0 ) THEN
                  ELSEIF ( keyx==0 ) THEN
                     EXIT SPAG_Loop_2_1
                  ELSE
                     IF ( keyx>lcor ) THEN
                        spag_nextblock_1 = 8
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     READ (out) (z(l),l=1,keyx)
                  ENDIF
               ENDDO SPAG_Loop_2_1
            ENDDO
         ENDIF
         GOTO iret
!
!     ERRORS
!
 100     CALL fname(input,tmp)
         WRITE (nout,99010) sfm , mo2 , tmp
99010    FORMAT (A25,' 4116',A19,'UNABLE TO OPEN INPUT DATA BLOCK ',2A4)
         spag_nextblock_1 = 10
      CASE (7)
         WRITE (nout,99011) ufm , mo2 , hdrx
99011    FORMAT (A23,' 4130',A19,'ILLEGAL TAPE HEADER CODE ',7A4)
         spag_nextblock_1 = 10
      CASE (8)
         WRITE (nout,99012) ufm , lcor , key
99012    FORMAT (A23,' 2187. INSUFFICIENT WORKING CORE TO HOLD FORTRAN ','LOGICAL RECORD.',/5X,'LENGHT OF WORKING CORE =',I11,      &
                &'.   LENGTH OF FORTRAN LOGICAL RECORD =',I11)
         spag_nextblock_1 = 10
      CASE (9)
         WRITE (nout,99013) sfm , key , keyx
99013    FORMAT (A25,' 2190. ILLEGAL VLUE FOR KEY =',I10,1H.,5X,'EXPECTED VALUE =',I10)
         spag_nextblock_1 = 10
      CASE (10)
         CALL mesage(-61,0,sub)
         RETURN 1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE outmsc

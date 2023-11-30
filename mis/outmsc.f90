
SUBROUTINE outmsc(*,*)
   IMPLICIT NONE
   INTEGER D(3) , Ibuf , Idum4(6) , Idum5(5) , Idum6(2) , Mach , Nlpp , Nout , Nwds(4) , P1 , P2 , P3(2) , P4 , P5 , P6(2)
   DOUBLE PRECISION Dxns(1)
   CHARACTER*25 Sfm , Uwm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   REAL Xns(1) , Z(1)
   COMMON /blank / P1 , P2 , P3 , P4 , P5 , P6
   COMMON /machin/ Mach
   COMMON /system/ Ibuf , Nout , Idum4 , Nlpp , Idum5 , D
   COMMON /type  / Idum6 , Nwds
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm
   COMMON /zzzzzz/ Z
   INTEGER block(20) , buf1 , dx(3) , endfil , endrec , hdr(7) , hdrx(7) , i , ii , inp(13) , input , iret , j , k , k1 , k2 , key ,&
         & keyx , l , lcor , lend , mcb(7) , name(2) , none(2) , nskip , nwd , out , sub(2) , tapcod(2) , tmp(2)
   LOGICAL dp
   INTEGER korsz
   CHARACTER*19 mo2
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
!
   WRITE (Nout,99001) Uim
99001 FORMAT (A29,'. USER REQUESTED RECORDS IN MSC/OUTPUT2 COMPATIBLE',' RECORDS')
   endfil = 0
   endrec = 0
   lcor = korsz(Z(1))
   buf1 = lcor - Ibuf + 1
   IF ( buf1<=0 ) CALL mesage(-8,lcor,sub)
   lend = buf1 - 1
   out = P2
   tapcod(1) = P3(1)
   tapcod(2) = P3(2)
   IF ( P1==-9 ) THEN
!
!     FINAL CALL TO OUTPUT2, P1 = -9
!
      WRITE (out) endfil
      RETURN 2
   ELSEIF ( P1==-3 ) THEN
!
!     OBTAIN LIST OF DATA BLOCKS ON FORTRAN TAPE, P1 = -3
!
      REWIND out
      READ (out) key
      keyx = 3
      IF ( key/=keyx ) GOTO 1500
      READ (out) dx
      READ (out) key
      keyx = 7
      IF ( key/=keyx ) GOTO 1500
      READ (out) hdrx
      DO k = 1 , 7
         IF ( hdrx(k)/=hdr(k) ) GOTO 1300
      ENDDO
      READ (out) key
      keyx = 2
      IF ( key/=keyx ) GOTO 1500
      READ (out) tmp
      IF ( tmp(1)/=P3(1) .OR. tmp(2)/=P3(2) ) THEN
         WRITE (Nout,99002) Uwm , tmp , P3
99002    FORMAT (A25,' 4141. FORTRAN TAPE ID CODE - ',2A4,' DOES NOT MATCH OUTPUT2 THIRD PARAMETER NAME - ',2A4)
      ENDIF
      ASSIGN 700 TO iret
      nskip = 1
      GOTO 1100
   ELSEIF ( P1<=-2 ) THEN
      WRITE (Nout,99003) Ufm , mo2 , P1
99003 FORMAT (A23,' 4120',A19,'ILLEGAL FIRST PARAMETER ',I3)
      GOTO 1600
   ELSE
      IF ( P1<=0 ) GOTO 300
!
!     SKIP FORWARD n DATA BLOCKS, P1 = n
!
      i = 1
   ENDIF
 100  READ (out) key
   keyx = 2
   IF ( key/=keyx ) GOTO 1500
   READ (out) tmp
   READ (out) key
   IF ( key>=0 ) THEN
      WRITE (Nout,99004) Sfm , key
99004 FORMAT (A25,' 2190. ILLEGAL VALUE FOR KEY =',I10)
      GOTO 1600
   ELSE
      ASSIGN 200 TO iret
      nskip = 1
      GOTO 1100
   ENDIF
 200  i = i + 1
   IF ( i<=P1 ) GOTO 100
!
 300  IF ( P1==-1 ) THEN
      REWIND out
      key = 3
      WRITE (out) key
      WRITE (out) D
      key = 7
      WRITE (out) key
      WRITE (out) hdr
      key = 2
      WRITE (out) key
      WRITE (out) P3
      endrec = endrec - 1
      WRITE (out) endrec
      WRITE (out) endfil
      endrec = 0
      WRITE (Nout,99005) Uim , P3
99005 FORMAT (A29,' FROM OUPUT2 MODULE.  THE LABEL IS ',2A4)
   ENDIF
!
 400  DO ii = 1 , 5
      input = 100 + ii
      mcb(1) = input
      CALL rdtrl(mcb(1))
      IF ( mcb(1)<=0 ) CYCLE
      CALL fname(input,name)
      IF ( name(1)==none(1) .AND. name(2)==none(2) ) CYCLE
      block(1) = input
      nwd = Nwds(mcb(5))
      dp = mcb(5)==2 .OR. mcb(5)==4
!
!     OPEN INPUT DATA BLOCK TO READ WITH REWIND
!
      CALL open(*1200,input,Z(buf1),0)
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
!
!     COPY CONTENTS OF INPUT DATA BLOCK ONTO FILE
!
 450  CALL rectyp(input,k)
      key = 1
      WRITE (out) key
      WRITE (out) k
      IF ( k==0 ) THEN
         DO
!
!     NON-STRING RECORD
!     MAKE SURE EACH RECORD IS NOT LONGER THAN P4 WORDS
!
            CALL read(*600,*500,input,Z(1),lend,0,k1)
            DO i = 1 , lend , P4
               key = lend - i + 1
               IF ( key>=P4 ) key = P4
               k2 = i + key - 1
               WRITE (out) key
               WRITE (out) (Z(k),k=i,k2)
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
         CALL getstr(*550,block)
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
            WRITE (out) block(4) , (Dxns(k),k=k1,k2)
         ELSE
            WRITE (out) block(4) , (Xns(k),k=k1,k2)
         ENDIF
!
         CALL endget(block)
      ENDDO
 500  DO i = 1 , k1 , P4
         key = k1 - i + 1
         IF ( key>=P4 ) key = P4
         k2 = i + key - 1
         WRITE (out) key
         WRITE (out) (Z(k),k=i,k2)
      ENDDO
!
 550  endrec = endrec - 1
      WRITE (out) endrec
      GOTO 450
!
!     CLOSE INPUT DATA BLOCK WITH REWIND
!
 600  CALL close(input,1)
      WRITE (out) endfil
      endrec = 0
      WRITE (Nout,99006) Uim , name , out , inp(P2-10) , mcb
99006 FORMAT (A29,' 4144. DATA BLOCK ',2A4,' WRITTEN ON FORTRAN UNIT ',I3,2H (,A4,1H),/5X,'TRAILER =',6I7,I11)
!
   ENDDO
!
!     CLOSE FORTRAN TAPE WITHOUT END-OF-FILE AND WITHOUT REWIND
!
   RETURN 1
 700  k = 0
 800  CALL page1
   WRITE (Nout,99007) inp(P2-10) , out
99007 FORMAT (//42X,'CONTENTS OF ',A4,', FORTRAN UNIT',I3,/46X,'FILE',18X,'NAME',/)
 900  READ (out) key
   IF ( key<0 ) THEN
      WRITE (Nout,99008) Sfm , mo2
99008 FORMAT (A25,' 4415',A19,'SHORT RECORD ENCOUNTERED')
      GOTO 1600
   ELSEIF ( key==0 ) THEN
      ASSIGN 400 TO iret
      nskip = k + 1
      IF ( nskip>0 ) REWIND out
   ELSE
      READ (out) tmp
      ASSIGN 1000 TO iret
      nskip = 1
   ENDIF
   GOTO 1100
 1000 k = k + 1
   WRITE (Nout,99009) k , tmp
99009 FORMAT (45X,I5,18X,2A4)
   IF ( mod(k,Nlpp)==0 ) GOTO 800
   GOTO 900
!
!     SKIP NSKIP FILES ON FORTRAN TAPE
!
 1100 IF ( nskip/=0 ) THEN
      DO j = 1 , nskip
         DO
            READ (out) keyx
            IF ( keyx<0 ) THEN
            ELSEIF ( keyx==0 ) THEN
               EXIT
            ELSE
               IF ( keyx>lcor ) GOTO 1400
               READ (out) (Z(l),l=1,keyx)
            ENDIF
         ENDDO
      ENDDO
   ENDIF
   GOTO iret
!
!     ERRORS
!
 1200 CALL fname(input,tmp)
   WRITE (Nout,99010) Sfm , mo2 , tmp
99010 FORMAT (A25,' 4116',A19,'UNABLE TO OPEN INPUT DATA BLOCK ',2A4)
   GOTO 1600
 1300 WRITE (Nout,99011) Ufm , mo2 , hdrx
99011 FORMAT (A23,' 4130',A19,'ILLEGAL TAPE HEADER CODE ',7A4)
   GOTO 1600
 1400 WRITE (Nout,99012) Ufm , lcor , key
99012 FORMAT (A23,' 2187. INSUFFICIENT WORKING CORE TO HOLD FORTRAN ','LOGICAL RECORD.',/5X,'LENGHT OF WORKING CORE =',I11,         &
             &'.   LENGTH OF FORTRAN LOGICAL RECORD =',I11)
   GOTO 1600
 1500 WRITE (Nout,99013) Sfm , key , keyx
99013 FORMAT (A25,' 2190. ILLEGAL VLUE FOR KEY =',I10,1H.,5X,'EXPECTED VALUE =',I10)
 1600 CALL mesage(-61,0,sub)
   RETURN 1
!
END SUBROUTINE outmsc
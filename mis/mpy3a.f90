
SUBROUTINE mpy3a(Z,Iz,Dz)
   IMPLICIT NONE
   REAL A(2) , Buf4 , Code , Dum(2) , Dumcp(3) , E , Filec(7) , Filee(7) , Scr , Scr3(7)
   INTEGER Buf1 , Buf2 , Buf3 , Eol , Eor , Filea(7) , Fileb(7) , Iacols , Ibcols , Icore , Incr , Ipoint , Irow , Itrans , Itrl ,  &
         & Laend , Lcore , Lkore , M , N , Ncb , Npoint , Prec , Row1 , Rowm , Scr1 , Scr2 , Typin , Typout , Uincr , Urow1 ,       &
         & Urown , Utyp , Zpntrs(22)
   DOUBLE PRECISION Da
   COMMON /mpy3cp/ Itrl , Icore , N , Ncb , M , Dumcp , Zpntrs , Laend
   COMMON /mpy3tl/ Filea , Fileb , Filee , Filec , Scr1 , Scr2 , Scr , Lkore , Code , Prec , Lcore , Scr3 , Buf1 , Buf2 , Buf3 ,    &
                 & Buf4 , E
   COMMON /packx / Typin , Typout , Row1 , Rowm , Incr
   COMMON /unpakx/ Utyp , Urow1 , Urown , Uincr
   COMMON /zntpkx/ A , Dum , Irow , Eol , Eor
   DOUBLE PRECISION Dz(1)
   INTEGER Iz(1)
   REAL Z(1)
   INTEGER file , i , ib , ii , incrjj , j , jj , jj2 , jjc , jjt , k , l , mcb(7) , name(2) , nerr , precl , precn
!*****
!    PREPARES B AND A(T).
!*****
!
!
!
!
!
!
!
!
! FILES
! SUBROUTINE CALL PARAMETERS
! PACK
! UNPACK
! TERMWISE MATRIX READ
!
!
!
   EQUIVALENCE (Ipoint,Zpntrs(3)) , (Npoint,Zpntrs(4)) , (Iacols,Zpntrs(5)) , (Itrans,Zpntrs(7)) , (Ibcols,Zpntrs(11))
   EQUIVALENCE (A(1),Da)
!
!
!
   DATA name/4HMPY3 , 4HA   /
!*****
!    FILE OPENING.
!*****
   file = Scr1
   CALL open(*300,Scr1,Z(Buf2),1)
   file = Fileb(1)
   CALL open(*300,Fileb,Z(Buf3),0)
   CALL fwdrec(*400,Fileb)
!*****
!    UNPACK B AND PACK INTO SCRATCH FILE 1.
!*****
! PACK PARAMETERS
   Typin = Prec
   Typout = Prec
   Row1 = 1
   Rowm = N
   Incr = 1
! UNPACK PARAMETERS
   Utyp = Prec
   Urow1 = 1
   Urown = N
   Uincr = 1
   precn = Prec*N
   mcb(1) = 301
   mcb(2) = 0
   mcb(3) = N
   mcb(4) = 1
   mcb(5) = Prec
   mcb(6) = 0
   mcb(7) = 0
   DO k = 1 , Ncb
      CALL unpack(*50,Fileb,Z(Ibcols))
      GOTO 100
 50   ib = Ibcols - 1
      DO l = 1 , precn
         ib = ib + 1
         Z(ib) = 0.
      ENDDO
 100  CALL pack(Z(Ibcols),Scr1,mcb)
      CALL savpos(Scr1,Iz(k))
   ENDDO
   CALL close(Scr1,1)
   CALL close(Fileb,1)
   IF ( Icore/=1 ) THEN
!*****
!    INITIALIZE ARRAY CONTAINING POINTERS TO ROWS OF MATRIX A TO 0.
!*****
      DO l = Ipoint , Npoint
         Iz(l) = 0
      ENDDO
!*****
!    COUNT NO. OF NON-ZERO COLUMNS IN EACH ROW OF A.
!*****
      file = Filea(1)
      CALL open(*300,Filea,Z(Buf1),0)
      CALL fwdrec(*400,Filea)
      DO i = 1 , M
         CALL intpk(*150,Filea,0,Prec,0)
         DO
            CALL zntpki
            ii = Ipoint + Irow - 1
            Iz(ii) = Iz(ii) + 1
            IF ( Eol==1 ) EXIT
         ENDDO
 150  ENDDO
!*****
!    CALCULATE POINTERS TO ROWS OF MATRIX A.
!*****
      jj = 1
      DO l = Ipoint , Npoint
         IF ( Iz(l)/=0 ) THEN
            incrjj = Iz(l)
            Iz(l) = jj
            jj = jj + incrjj
         ENDIF
      ENDDO
      Laend = jj - 1
!*****
!    PROCESS A(T) MATRIX.
!*****
      file = Filea(1)
      CALL rewind(Filea)
      CALL fwdrec(*400,Filea)
      jj2 = Iacols + Laend - 1
      DO jj = Iacols , jj2
         Iz(jj) = 0
      ENDDO
      DO j = 1 , M
         CALL intpk(*200,Filea,0,Prec,0)
 160     CALL zntpki
         l = Ipoint + Irow - 1
         jj = Iz(l)
         jjc = Iacols + jj - 1
         DO WHILE ( Iz(jjc)/=0 )
            jj = jj + 1
            jjc = jjc + 1
         ENDDO
         Iz(jjc) = j
         IF ( Prec==2 ) THEN
            jjt = (Itrans-1)/2 + jj
            Dz(jjt) = Da
            IF ( Eol/=1 ) GOTO 160
         ELSE
            jjt = Itrans + jj - 1
            Z(jjt) = A(1)
            IF ( Eol/=1 ) GOTO 160
         ENDIF
 200  ENDDO
      precl = Prec*Laend
      CALL close(Filea,1)
   ENDIF
   GOTO 99999
!*****
!    ERROR MESSAGES.
!*****
 300  nerr = -1
   GOTO 500
 400  nerr = -2
 500  CALL mesage(nerr,file,name)
!
99999 RETURN
END SUBROUTINE mpy3a

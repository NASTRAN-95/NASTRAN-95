
SUBROUTINE mpy3ic(Z,Iz,Dz)
   IMPLICIT NONE
   REAL A(2) , Dum(2) , Scr , Scr2 , Sysbuf
   INTEGER Buf1 , Buf2 , Buf3 , Buf4 , Code , D , Eol , Eor , Filea(7) , Fileb(7) , Filec(7) , Filee(7) , I , Iacols , Iakj ,       &
         & Iantu , Ibcid , Ibcols , Ibntu , Ic , Icore , Iflag , Iktbp , Incr , Ipoint , Irow , Isavp , Itrans , Itrl , J , K , K2 ,&
         & Ka , Kcount , Laend , Lcore , Lkore , Ltbc , M , Maxa , N , Nacols , Nakj , Nantu , Nbcid , Nbcols , Nbntu , Nc , Ncb ,  &
         & Nk , Nktbp , Nout , Npoint , Nsavp , Ntbu , Ntrans , Prec , Row1 , Rowm , Scr1 , Scr3(7) , Typin , Typout , Uincr ,      &
         & Urow1 , Urown , Utyp , Zpntrs(22)
   LOGICAL E , First1 , First2
   COMMON /mpy3cp/ Itrl , Icore , N , Ncb , M , Nk , D , Maxa , Zpntrs , Laend , First1 , First2 , K , K2 , Kcount , Iflag , Ka ,   &
                 & Ltbc , J , I , Ntbu
   COMMON /mpy3tl/ Filea , Fileb , Filee , Filec , Scr1 , Scr2 , Scr , Lkore , Code , Prec , Lcore , Scr3 , Buf1 , Buf2 , Buf3 ,    &
                 & Buf4 , E
   COMMON /packx / Typin , Typout , Row1 , Rowm , Incr
   COMMON /system/ Sysbuf , Nout
   COMMON /unpakx/ Utyp , Urow1 , Urown , Uincr
   COMMON /zntpkx/ A , Dum , Irow , Eol , Eor
   DOUBLE PRECISION Dz(1)
   INTEGER Iz(1)
   REAL Z(1)
   DOUBLE PRECISION dd , mm , nn , pp
   INTEGER file , ia , ib , ibc , ii , ik , ix , name(2) , nerr , precm
!
!     IN-CORE PRODUCT.
!
   !>>>>EQUIVALENCE (Isavp,Zpntrs(1)) , (Nsavp,Zpntrs(2)) , (Ipoint,Zpntrs(3)) , (Npoint,Zpntrs(4)) , (Iacols,Zpntrs(5)) ,               &
!>>>>    & (Nacols,Zpntrs(6)) , (Itrans,Zpntrs(7)) , (Ntrans,Zpntrs(8)) , (Ic,Zpntrs(9)) , (Nc,Zpntrs(10)) , (Ibcols,Zpntrs(11)) ,       &
!>>>>    & (Nbcols,Zpntrs(12)) , (Ibcid,Zpntrs(13)) , (Nbcid,Zpntrs(14)) , (Ibntu,Zpntrs(15)) , (Nbntu,Zpntrs(16)) , (Iktbp,Zpntrs(17)) ,&
!>>>>    & (Nktbp,Zpntrs(18)) , (Iantu,Zpntrs(19)) , (Nantu,Zpntrs(20)) , (Iakj,Zpntrs(21)) , (Nakj,Zpntrs(22))
   DATA name/4HMPY3 , 4HIC  /
!
!
!     INITIALIZATION.
!
   First1 = .TRUE.
   First2 = .TRUE.
   dd = D
   nn = Ncb
   mm = M
   pp = Prec
!
!     OPEN CORE POINTERS
!
   Isavp = 1
   Nsavp = Ncb
   Ipoint = Nsavp + 1
   Npoint = Nsavp + Ncb
   Iacols = Npoint + 1
!     NACOLS = NPOINT + D*NCB*M/10000
   Nacols = Npoint + (dd*nn*mm/10000.D0+0.5D0)
   Itrans = Nacols + 1
   IF ( Prec/=1 .AND. mod(Itrans,2)/=1 ) Itrans = Itrans + 1
!     NTRANS = ITRANS + PREC*D*NCB*M/10000 - 1
   Ntrans = Itrans + (pp*dd*nn*mm/10000.D0+0.5D0) - 1
   Ic = Ntrans + 1
   IF ( Prec/=1 .AND. mod(Ic,2)/=1 ) Ic = Ic + 1
   Nc = Ic + Prec*M - 1
   Ibcols = Nc + 1
   Nbcols = Nc + Prec*N*Nk
   Ibcid = Nbcols + 1
   Nbcid = Nbcols + Nk
   Ibntu = Nbcid + 1
   Nbntu = Nbcid + Nk
   Iktbp = Nbntu + 1
   Nktbp = Nbntu + Maxa
   Iantu = Nktbp + 1
   Nantu = Nktbp + Maxa
   Iakj = Nantu + 1
   Nakj = Nantu + Prec*Maxa
!
!     PACK PARAMETERS
!
   Typin = Prec
   Typout = Prec
   Row1 = 1
   Incr = 1
!
!     UNPACK PARAMETERS
!
   Utyp = Prec
   Urow1 = 1
   Uincr = 1
!
!     PREPARE B AND A(T).
!
   CALL mpy3a(Z,Z,Z)
!
!     OPEN FILES AND CHECK EXISTENCE OF MATRIX E.
!
   IF ( E ) THEN
      file = Filee(1)
      CALL open(*200,Filee,Z(Buf4),2)
      CALL fwdrec(*300,Filee)
   ENDIF
   file = Filea(1)
   CALL open(*200,Filea,Z(Buf1),2)
   CALL fwdrec(*300,Filea)
   file = Scr1
   CALL open(*200,Scr1,Z(Buf2),0)
   file = Filec(1)
   CALL gopen(Filec,Z(Buf3),1)
   Rowm = Filec(3)
!
!     PROCESS COLUMNS OF C ONE BY ONE.
!
   DO J = 1 , M
!
!     INITIALIZE COLUMN OF C.
!
      DO ix = Ic , Nc
         Z(ix) = 0.
      ENDDO
      IF ( E ) THEN
         Urown = M
         CALL unpack(*50,Filee,Z(Ic))
      ENDIF
 50   precm = Prec*M
!
!     PROCESS A AND PERFORM FIRST PART OF PRODUCT.
!
      CALL mpy3b(Z,Z,Z)
!
!     TEST IF PROCESSING IS COMPLETE
!
      IF ( Iflag==0 ) GOTO 150
!
!     PROCESS REMAINING TERMS OF COLUMN J OF A.
!
!     TEST IF BCOLS IS FULL
!
 100  IF ( K2>=Nk ) THEN
!
!     CALCULATE NEXT TIME USED FOR COLUMNS OF B AND/OR TERMS OF A
!
         IF ( First2 ) THEN
            First2 = .FALSE.
            ibc = Ibcid - 1
            ib = Ibntu - 1
            DO ii = 1 , Nk
               ibc = ibc + 1
               I = Iz(ibc)
               CALL mpy3nu(Z)
               ib = ib + 1
               Iz(ib) = Ntbu
            ENDDO
         ENDIF
         ik = Iktbp - 1
         ia = Iantu - 1
         DO ii = 1 , K
            ik = ik + 1
            ia = ia + 1
            IF ( Iz(ik)==0 ) THEN
               Iz(ia) = 0
            ELSE
               I = Iz(ik)
               CALL mpy3nu(Z)
               Iz(ia) = Ntbu
            ENDIF
         ENDDO
      ENDIF
      DO
!
!     ADD OR REPLACE COLUMN OF B INTO CORE AND PERFORM COMPUTATION
!
         CALL mpy3c(Z,Z,Z)
         IF ( Kcount==K ) EXIT
         IF ( First2 ) GOTO 100
      ENDDO
!
!     PACK COLUMN OF C.
!
 150  CALL pack(Z(Ic),Filec,Filec)
   ENDDO
!
!     CLOSE FILES.
!
   CALL close(Filea,2)
   CALL close(Scr1,1)
   CALL close(Filec,1)
   IF ( E ) CALL close(Filee,2)
   GOTO 99999
!
!     ERROR MESSAGES.
!
 200  nerr = -1
   GOTO 400
 300  nerr = -2
 400  CALL mesage(nerr,file,name)
!
99999 RETURN
END SUBROUTINE mpy3ic
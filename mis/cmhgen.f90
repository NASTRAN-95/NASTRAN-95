
SUBROUTINE cmhgen
   IMPLICIT NONE
   INTEGER Buf1 , Buf2 , Buf3 , Buf4 , Cnam(2) , Combo(7,5) , Iauto , Iiii , Iin , Incr , Inpt , Iout , Lcore , Nipnew , Nnnn ,     &
         & Npsub , Scbdat , Scconn , Score , Scr1 , Scr3 , Scsfil , Z(1)
   REAL Buf5 , Casecc , Conset , Geom4 , Outt , Sccstm , Scmcon , Scr2 , Sctoc , Tdat(6) , Toler
   COMMON /cmb001/ Scr1 , Scr2 , Scbdat , Scsfil , Scconn , Scmcon , Sctoc , Geom4 , Casecc , Sccstm , Scr3
   COMMON /cmb002/ Buf1 , Buf2 , Buf3 , Buf4 , Buf5 , Score , Lcore , Inpt , Outt
   COMMON /cmb003/ Combo , Conset , Iauto , Toler , Npsub
   COMMON /cmb004/ Tdat , Nipnew , Cnam
   COMMON /packx / Iin , Iout , Iiii , Nnnn , Incr
   COMMON /zzzzzz/ Z
   INTEGER aaa(2) , bufex , ce(10) , i , i1 , i2 , iaddh , ic , icode , idh , idhm1 , ifile , ihead(2) , imsg , ioefil , ipnew ,    &
         & ipold , ir , ishptr , itest , j , j1 , j2 , lhptr , listn(32) , listo(32) , locipn , locipo , mcb(7) , nam(2) , ncn ,    &
         & nco , ncol , ncom , ncs , nheqss , nnn , nrow , nsilnw , nslold , nsub , ssil
   REAL colout(6) , t(6,6) , tid(6,6) , tp(6,6) , tpp(6,6) , ttran(6,6) , zero
   LOGICAL frsfil
!
!     THIS SUBROUTINE GENERATES THE (H) TRANSFORMATION MATRICES FOR
!     COMPONENT SUBSTRUCTURES IN A COMBINE OPERATION AND WRITES THEM
!     ON THE SOF
!
   DATA zero/0.0/ , aaa/4HCMHG , 4HEN  / , ihead/4HHORG , 4H    /
   DATA tid/1. , 0. , 0. , 0. , 0. , 0. , 0. , 1. , 0. , 0. , 0. , 0. , 0. , 0. , 1. , 0. , 0. , 0. , 0. , 0. , 0. , 1. , 0. , 0. , &
      & 0. , 0. , 0. , 0. , 1. , 0. , 0. , 0. , 0. , 0. , 0. , 1./
   DATA nheqss/4HEQSS/
!
!     READ SIL,C FROM SOF FOR COMBINED STRUCTURE
!
   Incr = 1
   bufex = Lcore - Buf2 + Buf3
   Lcore = bufex - 1
   IF ( Lcore<0 ) GOTO 700
   ioefil = 310
   CALL open(*500,ioefil,Z(bufex),0)
   mcb(1) = Scr1
   mcb(4) = 2
   mcb(5) = 1
   Iin = 1
   Iout = 1
   CALL sfetch(Cnam,nheqss,1,itest)
   nsub = 0
   DO i = 1 , Npsub
      nsub = nsub + Combo(i,5)
   ENDDO
   CALL sjump(nsub+1)
   CALL suread(Z(Score),-1,nsilnw,itest)
!
!     LOOP ON NUMBER OF PSEUDO-STRUCTURES BEING COMBINED
!
   ssil = Score + nsilnw
   Lcore = Lcore - nsilnw
   ifile = Scr3
   CALL open(*500,Scr3,Z(Buf1),0)
   ifile = Scsfil
   CALL open(*500,Scsfil,Z(Buf2),0)
!
   DO i = 1 , Npsub
      frsfil = .TRUE.
      mcb(2) = 0
      mcb(6) = 0
      mcb(7) = 0
!
!     READ SIL,C FOR COMPONENT SUBSTRUCTURE
!
      ncs = Combo(i,5) + 2
      DO j = 1 , ncs
         CALL fwdrec(*600,Scsfil)
      ENDDO
      ifile = ioefil
      CALL read(*600,*50,ioefil,Z(ssil),Lcore,1,nslold)
      GOTO 700
 50   ishptr = ssil + nslold
      ifile = Scsfil
      CALL read(*600,*100,Scsfil,Z(ishptr),Lcore,1,lhptr)
      GOTO 700
 100  CALL skpfil(Scsfil,1)
!
!     COMPUTE NUMBER OF ROWS IN MATRIX
!
      icode = Z(ssil+nslold-1)
      CALL decode(icode,listo,ncom)
      mcb(3) = Z(ssil+nslold-2) + ncom - 1
!
!     READ CONNECTION ENTRIES
!
!     READ TRANSFORMATION MATRIX FOR PSEUDOSTRUCTURE
!
      ifile = Scr3
      CALL read(*600,*150,Scr3,ttran,37,1,nnn)
 150  CALL skpfil(Scr3,-1)
      IF ( i/=1 ) CALL skpfil(Scr3,1)
      ifile = Scconn
      CALL open(*500,Scconn,Z(Buf3),0)
      ifile = Scr1
      CALL open(*500,Scr1,Z(Buf4),1)
      CALL write(Scr1,ihead,2,1)
      ipnew = 0
 200  CALL read(*400,*250,Scconn,ce,10,1,nnn)
 250  ipnew = ipnew + 1
      locipn = Score + 2*(ipnew-1) + 1
      IF ( ce(i+2)==0 ) THEN
         Iiii = 1
         Nnnn = 1
         icode = Z(locipn)
         CALL decode(icode,listn,ncn)
         DO i1 = 1 , ncn
            CALL pack(zero,Scr1,mcb)
         ENDDO
         GOTO 200
      ELSE
         ipold = ce(i+2)
         locipo = ssil + 2*(ipold-1) + 1
         icode = Z(locipn)
         CALL decode(icode,listn,ncn)
         icode = Z(locipo)
         CALL decode(icode,listo,nco)
!
         iaddh = ishptr + ipold - 1
         idh = Z(iaddh)
         IF ( idh<1 ) THEN
!
!     IDENTITY MATRIX
!
            DO i1 = 1 , 6
               DO i2 = 1 , 6
                  t(i1,i2) = tid(i1,i2)
               ENDDO
            ENDDO
         ELSEIF ( idh==1 ) THEN
!
!     TRANS MATRIX
!
            DO i1 = 1 , 6
               DO i2 = 1 , 6
                  t(i1,i2) = ttran(i1,i2)
               ENDDO
            ENDDO
         ELSE
!
!     MATRIX DUE TO GTRAN
!
            idhm1 = idh - 1
            DO i1 = 1 , idhm1
               CALL fwdrec(*600,Scr3)
            ENDDO
            CALL read(*600,*300,Scr3,t,37,1,nnn)
            GOTO 300
         ENDIF
         GOTO 350
      ENDIF
 300  DO i1 = 1 , idh
         CALL bckrec(Scr3)
      ENDDO
!
!     DELETE ROWS OF (T) FOR EACH COLD EQUAL TO ZERO
!
 350  DO j1 = 1 , nco
         ir = listo(j1) + 1
         DO j2 = 1 , 6
            tp(j1,j2) = t(ir,j2)
         ENDDO
      ENDDO
      nrow = nco
!
!     DELETE COLUMNS OF (T) FOR EACH CNEW EQUAL TO ZERO
!
      DO j1 = 1 , ncn
         ic = listn(j1) + 1
         DO j2 = 1 , nrow
            tpp(j2,j1) = tp(j2,ic)
         ENDDO
      ENDDO
      ncol = ncn
      DO i1 = 1 , ncol
         DO i2 = 1 , nrow
            colout(i2) = tpp(i2,i1)
         ENDDO
         Iiii = Z(locipo-1)
         Nnnn = Iiii + nrow - 1
         CALL pack(colout,Scr1,mcb)
      ENDDO
      GOTO 200
 400  CALL close(Scconn,1)
      CALL wrttrl(mcb)
      CALL close(Scr1,1)
      nam(1) = Combo(i,1)
      nam(2) = Combo(i,2)
      CALL mtrxo(Scr1,nam,ihead(1),Z(Buf4),itest)
      CALL skpfil(Scr3,1)
   ENDDO
!
   CALL close(Scsfil,1)
   CALL close(Scr3,1)
   CALL close(ioefil,1)
   Lcore = bufex + Buf2 - Buf3
   RETURN
!
 500  imsg = -1
   GOTO 800
 600  imsg = -2
   GOTO 800
 700  imsg = -8
 800  CALL mesage(imsg,ifile,aaa)
END SUBROUTINE cmhgen

SUBROUTINE cmauto
   IMPLICIT NONE
   INTEGER Buf1 , Buf2 , Combo(7,5) , Idat(3) , Idry , Ihead(96) , Inpt , Iot , Iprint , Isort , Isw , Ititl(96) , Junk(6) ,        &
         & Junk1(2) , Junk2(2) , Junk7(7) , Lcore , Line , Mcon , Nlpp , Npsub , Outt , Restct(7,7) , Scconn , Score , Scsfil , Z(1)
   REAL Buf3 , Buf4 , Buf5 , Casecc , Conect , Conset , Geom4 , Origin(7,3) , Rz(1) , Scbdat , Scmcon , Scr1 , Scr2 , Sctoc , Step ,&
      & Toler , Tran , Xxx
   LOGICAL Iauto , Tdat(6)
   CHARACTER*23 Ufm
   COMMON /blank / Step , Idry
   COMMON /cmb001/ Scr1 , Scr2 , Scbdat , Scsfil , Scconn , Scmcon , Sctoc , Geom4 , Casecc
   COMMON /cmb002/ Buf1 , Buf2 , Buf3 , Buf4 , Buf5 , Score , Lcore , Inpt , Outt
   COMMON /cmb003/ Combo , Conset , Iauto , Toler , Npsub , Conect , Tran , Mcon , Restct , Isort , Origin , Iprint
   COMMON /cmb004/ Tdat
   COMMON /output/ Ititl , Ihead
   COMMON /system/ Xxx , Iot , Junk , Nlpp , Junk1 , Line , Junk2 , Idat , Junk7 , Isw
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
   REAL a(3) , asej , b(3) , bsej , xsej
   INTEGER aaa(2) , ce(9) , i , ibits(2) , iblnk , ic1 , ic2 , idir , ifile , ihd(12) , imsg , isavl , isavs , j , jbits(2) , jj ,  &
         & k , kdh , kk , m1 , m2 , mm , name(14) , ncsub , ni , nipi , nipj , nlin , nout , np2 , nsil(8) , nwd(8) , snext(8) ,    &
         & snk , spk , ssil(8) , st , sts , svkk
   INTEGER andf , rshift
   LOGICAL back , found , print
   EXTERNAL andf , rshift
!
!     THIS SUBROUTINE PROCESSES THE AUTOMATIC CONNECTION OF
!     SUBSTRUCTURES IN THE COMB1 MODULE
!
   EQUIVALENCE (Z(1),Rz(1))
   DATA aaa/4HCMAU , 2HTO/ , iblnk/4H    /
   DATA ihd/4H SUM , 4HMARY , 4H OF  , 4H AUT , 4HOMAT , 4HICAL , 4HLY G , 4HENER , 4HATED , 4H CON , 4HNECT , 4HIONS/
!
   nlin = 1000
   found = .FALSE.
   print = .FALSE.
   IF ( andf(rshift(Iprint,10),1)==1 ) print = .TRUE.
   np2 = 2*Npsub
   DO i = 1 , np2 , 2
      j = i/2 + 1
      name(i) = Combo(j,1)
      name(i+1) = Combo(j,2)
   ENDDO
   DO i = 1 , 96
      Ihead(i) = iblnk
   ENDDO
   j = 1
   DO i = 75 , 86
      Ihead(i) = ihd(j)
      j = j + 1
   ENDDO
   isavs = Score
   isavl = Lcore
   ifile = Scconn
   CALL open(*1100,Scconn,Z(Buf2),3)
   IF ( Iauto ) THEN
!
      ifile = Scsfil
      CALL open(*1100,Scsfil,Z(Buf1),0)
      ssil(1) = Score
      nout = Npsub + 2
      idir = Isort + 1
      DO i = 1 , Npsub
         sts = ssil(i)
         ncsub = Combo(i,5)
         DO j = 1 , ncsub
            CALL fwdrec(*1200,Scsfil)
         ENDDO
!
!     READ SIL,C FOR THE I-TH PSEUDOSTRUCTURE
!
         CALL read(*1200,*20,Scsfil,Z(sts),Lcore,1,nsil(i))
         GOTO 1300
 20      Lcore = Lcore - nsil(i)
         snext(i) = Score + nsil(i)
         Score = Score + nsil(i)
         st = snext(i)
!
!     READ BGSS FOR THE I-TH PSEUDOSTRUCTURE
!
         CALL read(*1200,*40,Scsfil,Z(st),Lcore,1,nwd(i))
         GOTO 1300
 40      snext(i+1) = snext(i) + nwd(i)
         ssil(i+1) = snext(i) + nwd(i)
         Score = Score + nwd(i)
         CALL skpfil(Scsfil,1)
         Lcore = Lcore - nwd(i)
         ni = nwd(i) + st
!
!     WRITE THE IP NUMBER OVER THE CID IN THE BGSS
!     WILL BE USED AFTER SORTING
!
         DO j = st , ni , 4
            jj = (j-st+4)/4
            IF ( Z(j)+1/=0 ) THEN
               Z(j) = jj
            ELSE
               Z(j) = -jj
            ENDIF
         ENDDO
      ENDDO
!
!     SORT EACH BGSS IN THE SPECIFIED COORDINATE DIRECTION
!
      DO i = 1 , Npsub
         st = snext(i)
         CALL sortf(0,0,4,idir,Rz(st),nwd(i))
      ENDDO
      i = 1
   ELSE
      CALL close(Scconn,1)
      RETURN
   ENDIF
 100  k = 0
   kk = 0
   back = .FALSE.
   svkk = 0
   ic1 = ssil(i)
   nipi = nwd(i)/4
   j = i + 1
   IF ( Restct(i,j)/=1 ) GOTO 1000
 200  ic2 = ssil(j)
   nipj = nwd(j)/4
 300  spk = snext(i) + k + 1
   IF ( Z(spk-1)<0 ) GOTO 800
   a(1) = Rz(spk)
   a(2) = Rz(spk+1)
   a(3) = Rz(spk+2)
 400  snk = snext(j) + kk + 1
   IF ( Z(snk-1)<0 ) GOTO 900
   b(1) = Rz(snk)
   b(2) = Rz(snk+1)
   b(3) = Rz(snk+2)
   IF ( a(Isort)<b(Isort)-Toler ) GOTO 700
   IF ( b(Isort)<a(Isort)-Toler ) GOTO 900
   IF ( .NOT.(back) ) THEN
      back = .TRUE.
      svkk = kk
   ENDIF
   asej = a(Isort)
   bsej = b(Isort)
   xsej = asej - bsej
   DO mm = 1 , 3
      IF ( mm/=Isort ) THEN
         asej = a(mm)
         bsej = b(mm)
         xsej = a(mm) - b(mm)
         IF ( abs(xsej)>Toler ) GOTO 900
      ENDIF
   ENDDO
!
!     GENERATE THE NEW CONNECTION ENTRY
!
   DO kdh = 1 , 9
      ce(kdh) = 0
   ENDDO
   ce(2) = 2**(i-1) + 2**(j-1)
   ce(2+i) = iabs(Z(spk-1))
   ce(2+j) = iabs(Z(snk-1))
   m1 = iabs(Z(spk-1))
   m2 = iabs(Z(snk-1))
   ce(1) = andf(Z(ic1+2*m1-1),Z(ic2+2*m2-1))
   found = .TRUE.
!
!     WRITE THE CONNECTION ENTRY ON SCCONN
!
   IF ( ce(1)/=0 ) CALL write(Scconn,ce,nout,1)
   IF ( .NOT.print ) GOTO 900
   IF ( ce(1)==0 ) GOTO 900
   IF ( nlin<Nlpp ) GOTO 600
 500  nlin = 0
   CALL page
   WRITE (Outt,99001) (name(kdh),kdh=1,np2)
99001 FORMAT (/14X,22HCONNECTED   CONNECTION,29X,22HPSEUDOSTRUCTURE  NAMES,/17X,3HDOF,9X,4HCODE,3X,7(3X,2A4)//)
   nlin = nlin + 10
 600  CALL bitpat(ce(1),ibits)
   CALL bitpat(ce(2),jbits)
   nlin = nlin + 1
   IF ( nlin>Nlpp ) GOTO 500
   WRITE (Outt,99002) ibits(1) , ibits(2) , jbits(1) , jbits(2) , (ce(kdh+2),kdh=1,Npsub)
99002 FORMAT (16X,A4,A2,5X,A4,A3,2X,7(3X,I8))
   GOTO 900
 700  kk = svkk
   back = .FALSE.
 800  k = k + 4
   IF ( k/4<nipi ) GOTO 300
   k = 0
   kk = 0
   svkk = 0
   back = .FALSE.
   GOTO 1000
 900  kk = kk + 4
   IF ( kk/4>=nipj ) GOTO 700
   GOTO 400
 1000 j = j + 1
   IF ( j<=Npsub ) GOTO 200
   i = i + 1
   j = i
   IF ( i<Npsub ) GOTO 100
   WRITE (Outt,99003)
99003 FORMAT (//40X,'NOTE - GRID POINTS IN PSEUDOSTRUCTURE INTERNAL',' GRID NUMBERS')
   CALL close(Scconn,1)
   CALL close(Scsfil,1)
   Score = isavs
   Lcore = isavl
   IF ( found .OR. Tdat(1) .OR. Tdat(2) ) RETURN
!
   WRITE (Outt,99004) Ufm
99004 FORMAT (A23,' 6531, NO CONNECTIONS HAVE BEEN FOUND DURING ','AUTOMATIC CONNECTION PROCEDURE.')
   Idry = -2
   RETURN
!
 1100 imsg = -1
   GOTO 1400
 1200 imsg = -2
   GOTO 1400
 1300 imsg = -8
 1400 CALL mesage(imsg,ifile,aaa)
END SUBROUTINE cmauto

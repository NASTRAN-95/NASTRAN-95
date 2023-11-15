
SUBROUTINE cmrd2c(Iter)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Atrlr(7) , Attrlr(7) , B , Bbar , Chlsky , Dry , Gib , Gibfbs(7) , Gibt(7) , Him , Idum1 , Idum2(8) , Idum3(11) ,        &
         & Idum4(4) , Idum5 , Idum6(4) , Iprntr , Iscr(11) , Iscra , Iscrb , Iscrc , Iscrq(7) , Iscrth(8) , Isign , Kbi , Kib ,     &
         & Kibt(7) , Kigfbs(7) , Kii , Kiit(7) , Kiitc(7) , Korbgn , Korlen , Lcore , Ligfbs(7) , Lii , Liifbs(7) , Liit(7) ,       &
         & Liitc(7) , Lstzwd , Nscrth , Nx , Nzfbs , Nzgfbs , Nzsf , Oldnam(2) , Power , Powerc , Prec , Prec1 , Sbuf1 , Sbuf2 ,    &
         & Sbuf3 , Scr(3) , Sign , U(7) , Ugfbs(7) , Uii , Uiitc(7) , Z(1)
   DOUBLE PRECISION Det(2) , Deti , Detr , Dz(1) , Mindc , Mindia
   REAL Otfile(6)
   LOGICAL Rsave
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /blank / Idum1 , Dry , Idum4 , Sbuf1 , Sbuf2 , Sbuf3 , Idum3 , Otfile , Iscr , Korlen , Korbgn , Oldnam , Idum2 , Rsave , &
                 & Idum6 , Lstzwd
   COMMON /cdcmpx/ Kiitc , Liitc , Uiitc , Scr , Det , Powerc , Nx , Mindc , B , Bbar
   COMMON /fbsx  / Liifbs , U , Kibt , Gibt , Nzfbs , Prec , Sign
   COMMON /gfbsx / Ligfbs , Ugfbs , Kigfbs , Gibfbs , Nzgfbs , Prec1 , Isign
   COMMON /sfact / Kiit , Liit , Iscrq , Iscra , Iscrb , Nzsf , Detr , Deti , Power , Iscrc , Mindia , Chlsky
   COMMON /system/ Idum5 , Iprntr
   COMMON /trnspx/ Atrlr , Attrlr , Lcore , Nscrth , Iscrth
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Iter
!
! Local variable declarations
!
   INTEGER dblkor , i , ifile , imsg , iprc , item , itest , itmlst(3) , itrlr(7) , ityp , itype , j , lower , modnam(2) , upper
   REAL gibbar
   LOGICAL restor , symtry
!
! End of declarations
!
!
!     THIS SUBROUTINE PERFORMS THE GUYAN REDUCTION ON THE STRUCTURE
!     POINTS FOR THE CMRED2 MODULE.
!
!     INPUT  DATA
!     GINO - KII    - KII PARTITION MATRIX
!            KIB    - KIB KIB PARTITION MATRIX
!     SOF  - GIMS   - G TRANSFORMATION MATRIX FOR BOUNDARY POINTS OF
!                      ORIGINAL SUBSTRUCTURE
!
!     OUTPUT DATA
!     SOF  - LMTX   - LII PARTITION MATRIX
!            GIMS   - G TRANSFORMATION MATRIX FOR BOUNDARY POINTS OF
!                    ORIGINAL SUBSTRUCTURE
!
!     PARAMETERS
!     INPUT- GBUF   - GINO BUFFER
!            ISCR   - SCRATCH FILE NUMBER ARRAY
!            KORLEN - LENGTH OF OPEN CORE
!            KORBGN - BEGINNING ADDRESS OF OPEN CORE
!            OLDNAM - NAME OF SUBSTRUCTURE BEGING REDUCED
!            RSAVE  - DECOMPOSITION SAVE FLAG
!     OTHERS-KII    - KII PARTITION MATRIX FILE NUMBER
!            LII    - LII PARTITION MATRIX FILE NUMBER
!            SYMTRY - KII SYMMETRY FLAG
!
   EQUIVALENCE (Kib,Iscr(2)) , (Kbi,Iscr(3)) , (Kii,Iscr(4)) , (Lii,Iscr(8)) , (Uii,Iscr(9)) , (Him,Iscr(10)) , (Gib,Iscr(11)) ,    &
    & (Dz(1),Z(1))
   DATA modnam/4HCMRD , 4H2C  /
   DATA lower , upper/4 , 5/
   DATA itmlst/4HLMTX , 4HGIMS , 4HHORG/
!
!     PREFORM GUYAN REDUCTION
!
   IF ( Dry==-2 ) GOTO 800
   restor = .FALSE.
!
!     TRANSPOSE KII, KBI
!
   IF ( Iter==1 ) THEN
!
!     DECOMPOSE INTERIOR STIFFNESS MATRIX
!        (SYMMETRIC)
!
!                                 T
!        **   **   **   ** **   **
!        *     *   *     * *     *
!        * KII * = * LII * * LII *
!        *     *   *     * *     *
!        **   **   **   ** **   **
!
      CALL sofcls
      Kiit(1) = Kii
      CALL rdtrl(Kiit)
      IF ( Kiit(4)/=6 ) THEN
!
!     DECOMPOSE INTERIOR STIFFNESS MATRIX
!        (UNSYMMETRIC)
!
!        **   **   **   ** **   **
!        *     *   *     * *     *
!        * KII * = * LII * * UII *
!        *     *   *     * *     *
!        **   **   **   ** **   **
!
         symtry = .FALSE.
         GOTO 200
      ELSE
         symtry = .TRUE.
         iprc = 1
         ityp = 0
         IF ( Kiit(5)==2 .OR. Kiit(5)==4 ) iprc = 2
         IF ( Kiit(5)>=3 ) ityp = 2
         itype = iprc + ityp
         CALL makmcb(Liit,Lii,Kiit(3),lower,itype)
         Iscrq(1) = Iscr(5)
         Iscra = Iscr(6)
         Iscrb = Iscr(7)
         Iscrc = Iscr(9)
         Chlsky = 0
         Power = 1
         dblkor = (Korbgn/2) + 1
         Nzsf = Lstzwd - ((2*dblkor)-1)
         CALL sdcomp(*400,Dz(dblkor),Dz(dblkor),Dz(dblkor))
         CALL wrttrl(Liit)
         GOTO 300
      ENDIF
   ELSEIF ( symtry ) THEN
!
!     KII SYMMETRIC, GIBBAR = GIB
!
      item = itmlst(2)
      CALL mtrxi(gibbar,Oldnam,item,0,itest)
      IF ( itest==1 ) GOTO 800
      GOTO 700
   ENDIF
 100  dblkor = (Korbgn/2) + 1
   Lcore = Lstzwd - ((2*dblkor)-1)
   Nscrth = 5
   DO i = 1 , Nscrth
      Iscrth(i) = Iscr(4+i)
   ENDDO
   DO i = 1 , 2
      itrlr(1) = Kii
      IF ( i==2 ) itrlr(1) = Kbi
      CALL rdtrl(itrlr)
      DO j = 1 , 7
         Atrlr(j) = itrlr(j)
         Attrlr(j) = itrlr(j)
      ENDDO
      Attrlr(2) = itrlr(3)
      Attrlr(3) = itrlr(2)
      CALL trnsp(Dz(dblkor))
      CALL wrttrl(Attrlr)
   ENDDO
   IF ( restor ) CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
   IF ( restor ) GOTO 800
   restor = .TRUE.
   CALL sofcls
 200  Kiitc(1) = Kii
   CALL rdtrl(Kiitc)
   ityp = 0
   iprc = 1
   IF ( Kiitc(5)==2 .OR. Kiitc(5)==4 ) iprc = 2
   IF ( Kiitc(5)>=3 ) ityp = 2
   itype = iprc + ityp
   CALL makmcb(Liitc,Lii,Kiitc(3),lower,itype)
   CALL makmcb(Uiitc,Uii,Kiitc(3),upper,itype)
   Scr(1) = Iscr(5)
   Scr(2) = Iscr(6)
   Scr(3) = Iscr(7)
   B = 0
   Bbar = 0
   dblkor = (Korbgn/2) + 1
   Nx = Lstzwd - ((2*dblkor)-1)
   CALL cdcomp(*500,Dz(dblkor),Dz(dblkor),Dz(dblkor))
   CALL wrttrl(Liitc)
   CALL wrttrl(Uiitc)
!
!     SAVE LII AS LMTX ON SOF
!
 300  IF ( .NOT.(Iter==2 .OR. .NOT.Rsave) ) THEN
      CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
      ifile = Lii
      CALL mtrxo(Lii,Oldnam,itmlst(1),0,itest)
      item = itmlst(1)
      IF ( itest/=3 ) GOTO 700
      CALL sofcls
   ENDIF
!
!     SOLVE STRUCTURE REDUCTION TRANSFORMATION MATRIX
!        (SYMMETRIC)
!
!                       T
!        **   ** **   ** **   **    **   **
!        *     * *     * *     *    *     *
!        * LII * * LII * * GIB * = -* KIB *
!        *     * *     * *     *    *     *
!        **   ** **   ** **   **    **   **
!
   IF ( .NOT.symtry ) THEN
!
!     SOLVE STRUCTURE REDUCTION TRANSFORMATION MATRIX
!        (UNSYMMETRIC)
!
!        **   ** **   ** **   **    **   **
!        *     * *     * *     *    *     *
!        * LII * * UII * * GIB * = -* KIB *
!        *     * *     * *     *    *     *
!        **   ** **   ** **   **    **   **
!
      Kigfbs(1) = Kib
      IF ( Iter==2 ) Kigfbs(1) = Kbi
      CALL rdtrl(Kigfbs)
      DO i = 1 , 7
         Ligfbs(i) = Liitc(i)
         Ugfbs(i) = Uiitc(i)
      ENDDO
      iprc = 1
      ityp = 0
      IF ( Kigfbs(5)==2 .OR. Kigfbs(5)==4 ) iprc = 2
      IF ( Liitc(5)==2 .OR. Liitc(5)==4 ) iprc = 2
      IF ( Uiitc(5)==2 .OR. Uiitc(5)==4 ) iprc = 2
      IF ( Kigfbs(5)>=3 ) ityp = 2
      IF ( Liitc(5)>=3 ) ityp = 2
      IF ( Uiitc(5)>=3 ) ityp = 2
      itype = iprc + ityp
      CALL makmcb(Gibfbs,Gib,Kigfbs(3),Kigfbs(4),itype)
      Nzgfbs = Lstzwd - ((2*dblkor)-1)
      Prec1 = iprc
      Isign = -1
      CALL gfbs(Dz(dblkor),Dz(dblkor))
      CALL wrttrl(Gibfbs)
   ELSE
      Kibt(1) = Kib
      IF ( Iter==2 ) Kibt(1) = Kbi
      CALL rdtrl(Kibt)
      DO i = 1 , 7
         Liifbs(i) = Liit(i)
      ENDDO
      iprc = 1
      ityp = 0
      IF ( Kibt(5)==2 .OR. Kibt(5)==4 ) iprc = 2
      IF ( Liit(5)==2 .OR. Liit(5)==4 ) iprc = 2
      IF ( Kibt(5)>=3 ) ityp = 2
      IF ( Liit(5)>=3 ) ityp = 2
      itype = iprc + ityp
      CALL makmcb(Gibt,Gib,Kibt(3),Kibt(4),itype)
      Nzfbs = Lstzwd - ((2*dblkor)-1)
      Prec = Kibt(5) - 2
      Sign = -1
      CALL fbs(Dz(dblkor),Dz(dblkor))
      CALL wrttrl(Gibt)
   ENDIF
!
!     SAVE GIB AS GIMS ON SOF
!
   IF ( restor ) GOTO 100
   IF ( Iter==2 ) GOTO 800
   CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
   ifile = Gib
   CALL mtrxo(Gib,Oldnam,itmlst(2),0,itest)
   item = itmlst(2)
   IF ( itest==3 ) GOTO 800
   GOTO 700
!
!     PROCESS SYSTEM FATAL ERRORS
!
 400  WRITE (Iprntr,99001) Uwm , Oldnam
99001 FORMAT (A25,' 6311, SDCOMP DECOMPOSITION FAILED ON KII MATRIX ','FOR SUBSTRUCTURE ',2A4)
   GOTO 600
 500  WRITE (Iprntr,99002) Uwm , Oldnam
99002 FORMAT (A23,' 6635, CDCOMP DECOMPOSITION FAILED ON KII MATRIX ','FOR SUBSTRUCTURE ',2A4)
 600  imsg = -37
   ifile = 0
   CALL sofcls
   CALL mesage(imsg,ifile,modnam)
   GOTO 800
!
!     PROCESS MODULE FATAL ERRORS
!
 700  IF ( itest==2 ) THEN
!
      WRITE (Iprntr,99003) Ufm , modnam , item , Oldnam
99003 FORMAT (A23,' 6215, MODULE ',2A4,' - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' PSEUDO-EXISTS ONLY.')
      Dry = -2
   ELSEIF ( itest==3 ) THEN
!
      imsg = -1
      CALL smsg(imsg,item,Oldnam)
   ELSEIF ( itest==4 ) THEN
      imsg = -2
      CALL smsg(imsg,item,Oldnam)
   ELSEIF ( itest==5 ) THEN
      imsg = -3
      CALL smsg(imsg,item,Oldnam)
   ELSEIF ( itest==6 ) THEN
!
      WRITE (Iprntr,99004) Ufm , modnam , item , Oldnam
99004 FORMAT (A23,' 6632, MODULE ',2A4,' - NASTRAN MATRIX FILE FOR I/O',' OF SOF ITEM ',A4,', SUBSTRUCTURE ',2A4,', IS PURGED.')
      Dry = -2
   ELSE
      WRITE (Iprntr,99005) Ufm , modnam , item , Oldnam
!
99005 FORMAT (A23,' 6211, MODULE ',2A4,' - ITEM ',A4,' OF SUBSTRUCTURE ',2A4,' HAS ALREADY BEEN WRITTEN.')
      Dry = -2
   ENDIF
 800  RETURN
!
END SUBROUTINE cmrd2c

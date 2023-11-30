
SUBROUTINE mred2b
   IMPLICIT NONE
   LOGICAL Bounds , Rsave
   INTEGER Chlsky , Dmr , Dry , Gib , Gibt(7) , Idum1 , Idum2(8) , Idum3 , Idum4(4) , Idum5 , Idum6(6) , Idum7(4) , Infile(12) ,    &
         & Iprntr , Iscr(10) , Iscr11 , Iscra , Iscrb , Iscrc , Iscrq(7) , Kib , Kibt(7) , Kii , Kiit(7) , Korbgn , Korlen , Lii ,  &
         & Liifbs(7) , Liit(7) , Lstzwd , Nzfbs , Nzsf , Oldnam(2) , Power , Prec , Sbuf1 , Sbuf2 , Sbuf3 , Sign , U(7) , Z(1)
   DOUBLE PRECISION Deti , Detr , Dz(1) , Mindia
   CHARACTER*25 Sfm , Uwm
   CHARACTER*27 Swm
   CHARACTER*23 Ufm
   CHARACTER*29 Uim
   COMMON /blank / Idum1 , Dry , Idum4 , Sbuf1 , Sbuf2 , Sbuf3 , Infile , Idum6 , Iscr , Korlen , Korbgn , Oldnam , Idum2 , Bounds ,&
                 & Idum3 , Rsave , Idum7 , Lstzwd , Iscr11
   COMMON /fbsx  / Liifbs , U , Kibt , Gibt , Nzfbs , Prec , Sign
   COMMON /sfact / Kiit , Liit , Iscrq , Iscra , Iscrb , Nzsf , Detr , Deti , Power , Iscrc , Mindia , Chlsky
   COMMON /system/ Idum5 , Iprntr
   COMMON /xmssg / Ufm , Uwm , Uim , Sfm , Swm
   COMMON /zzzzzz/ Z
   INTEGER dblkor , i , ifile , imsg , item , itest , itmlst(2) , itrlr(7) , lower , modnam(2) , modsam
!
!     THIS SUBROUTINE PERFORMS THE GUYAN REDUCTION ON THE STRUCTURE
!     POINTS FOR THE MRED2 MODULE.
!
!     INPUT DATA
!     GINO   - KII    - KII PARTITION MATRIX
!     SOF    - GIMS   - G TRANSFORMATION MATRIX FOR BOUNDARY POINTS OF
!                       ORIGINAL SUBSTRUCTURE
!
!     OUTPUT DATA
!     GINO   - LII    - LII PARTITION MATRIX
!     SOF    - LMTX   - LII PARTITION MATRIX
!              GIMS   - G TRANSFORMATION MATRIX FOR BOUNDARY POINTS OF
!                       ORIGINAL SUBSTRUCTURE
!
!     PARAMETERS
!     INPUT  - GBUF   - GINO BUFFER
!              ISCR   - SCRATCH FILE NUMBER ARRAY
!              KORLEN - LENGTH OF OPEN CORE
!              KORBGN - BEGINNING ADDRESS OF OPEN CORE
!              OLDNAM - NAME OF SUBSTRUCTURE BEGING REDUCED
!              BOUNDS - OLDBOUNDS OPTION FLAG
!              RSAVE  - DECOMPOSITION SAVE FLAG
!     OTHERS - KIB    - KIB PARTITION MATRIX FILE NUMBER
!              KII    - KII PARTITION MATRIX FILE NUMBER
!              LII    - LII PARTITION MATRIX FILE NUMBER (ISCR11)
!
   !>>>>EQUIVALENCE (Dmr,Infile(11)) , (Gib,Iscr(6)) , (Dz(1),Z(1)) , (Kib,Iscr(2)) , (Kii,Iscr(3)) , (Lii,Iscr11)
   DATA modnam/4HMRED , 4H2B  /
   DATA lower/4/
   DATA itmlst/4HLMTX , 4HGIMS/
!
!     TEST FOR GUYAN REDUCTION
!
   IF ( Dry==-2 ) GOTO 99999
   IF ( Bounds ) THEN
      itrlr(1) = Dmr
      CALL rdtrl(itrlr)
      IF ( itrlr(1)<0 ) GOTO 99999
      item = itmlst(1)
      CALL softrl(Oldnam,item,itrlr)
      IF ( itrlr(1)==1 ) GOTO 99999
   ENDIF
!
!     DECOMPOSE INTERIOR STIFFNESS MATRIX
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
   CALL makmcb(Liit,Lii,Kiit(3),lower,Kiit(5))
   Iscrq(1) = Iscr(6)
   Iscra = Iscr(7)
   Iscrb = Iscr(8)
   Iscrc = Iscr(9)
   Power = 1
   Chlsky = 0
   dblkor = 1 + Korbgn/2
   Nzsf = Lstzwd - 2*dblkor - 1
   CALL sdcomp(*100,Dz(dblkor),Dz(dblkor),Dz(dblkor))
   CALL wrttrl(Liit)
!
!     SAVE LII AS LMTX ON SOF
!
   IF ( Rsave ) THEN
      CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
      ifile = Lii
      item = itmlst(1)
      CALL mtrxo(Lii,Oldnam,item,0,itest)
      IF ( itest/=3 ) GOTO 200
      IF ( Bounds ) GOTO 99999
      CALL sofcls
   ENDIF
!
!     SOLVE STRUCTURE REDUCTION TRANSFORMATION MATRIX
!
!                       T
!        **   ** **   ** **   **    **   **
!        *     * *     * *     *    *     *
!        * LII * * LII * * GIB * = -* KIB *
!        *     * *     * *     *    *     *
!        **   ** **   ** **   **    **   **
!
   IF ( Bounds ) THEN
      CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
      GOTO 99999
   ELSE
      Kibt(1) = Kib
      CALL rdtrl(Kibt)
      DO i = 1 , 7
         Liifbs(i) = Liit(i)
      ENDDO
      CALL makmcb(Gibt,Gib,Kibt(3),Kibt(4),Kibt(5))
      Nzfbs = Lstzwd - 2*dblkor
      Prec = Kibt(5)
      Sign = -1
      CALL fbs(Dz(dblkor),Dz(dblkor))
      CALL wrttrl(Gibt)
!
!     SAVE GIB AS GIMS ON SOF
!
      CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
      ifile = Gib
      item = itmlst(2)
      CALL mtrxo(Gib,Oldnam,item,0,itest)
      IF ( itest==3 ) GOTO 99999
      GOTO 200
   ENDIF
!
!     PROCESS SYSTEM FATAL ERRORS
!
 100  WRITE (Iprntr,99001) Swm , Oldnam
99001 FORMAT (A27,' 6311, SDCOMP DECOMPOSITION FAILED ON KII MATRIX ','FOR SUBSTRUCTURE ',2A4)
   imsg = -37
   ifile = 0
   CALL mesage(imsg,ifile,modnam)
   GOTO 99999
!
!     PROCESS MODULE FATAL ERRORS
!
 200  IF ( itest==4 ) THEN
      imsg = -2
      CALL smsg(imsg,item,Oldnam)
      GOTO 99999
   ELSEIF ( itest==5 ) THEN
      imsg = -3
      CALL smsg(imsg,item,Oldnam)
      GOTO 99999
   ELSEIF ( itest==6 ) THEN
      imsg = -10
   ELSE
      imsg = -9
   ENDIF
   Dry = -2
   CALL smsg1(imsg,item,Oldnam,modsam)
!
99999 RETURN
END SUBROUTINE mred2b
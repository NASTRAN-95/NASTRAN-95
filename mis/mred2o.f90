
SUBROUTINE mred2o(Nus)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Dry , Gbuf1 , Gbuf2 , Gs , Gszero , Hk , Hm , Icode , Idum1 , Idum2 , Idum3 , Idum4(2) , Idum5(14) , Incr , Infile(12) , &
         & Irow , Iscr(10) , Itrlra(7) , Itrlrb(7) , Itrlrc(7) , Itrlre(7) , Jscr(3) , Kmw2 , Korbgn , Korlen , Lamamr , Lkore ,    &
         & Lstzwd , M , Nmodes , Nrow , Otfile(6) , Prec , Rprtn , Sbuf1 , Sbuf2 , Sbuf3 , Typin , Typout , Z(1)
   REAL Dummy(13) , Rz(1)
   DOUBLE PRECISION Dz(1)
   COMMON /blank / Idum1 , Dry , Idum2 , Gbuf1 , Gbuf2 , Idum3 , Sbuf1 , Sbuf2 , Sbuf3 , Infile , Otfile , Iscr , Korlen , Korbgn , &
                 & Idum5 , Nmodes , Idum4 , Lstzwd
   COMMON /mpy3tl/ Itrlra , Itrlrb , Itrlre , Itrlrc , Jscr , Lkore , Icode , Prec , Dummy
   COMMON /packx / Typin , Typout , Irow , Nrow , Incr
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Nus
!
! Local variable declarations
!
   REAL block(11)
   INTEGER dblkor , i , ifile , iform , imsg , iop , iprc , isub(4) , itrlr1(7) , itrlr2(7) , ityp , itype , j , modnam(2) ,        &
         & nwdsrd , typea , typeb
!
! End of declarations
!
!
!     THIS SUBROUTINE FORMS THE M MATRIX FOR THE MRED2 MODULE.
!
   EQUIVALENCE (Lamamr,Infile(2)) , (Gs,Iscr(7)) , (Dz(1),Z(1)) , (Hk,Iscr(2)) , (Kmw2,Iscr(5)) , (Hm,Iscr(9)) , (Gszero,Iscr(10)) ,&
    & (M,Iscr(10)) , (Rprtn,Iscr(8)) , (Rz(1),Z(1)) , (typea,block(1)) , (typeb,block(7))
   DATA modnam/4HMRED , 4H2O  /
!
!     FORM HM MATRIX
!
!        **  **   **  **   **          **
!        *    *   *    *   *    .   .   *
!        * HM * = * HK * + * GS . 0 . 0 *
!        *    *   *    *   *    .   .   *
!        **  **   **  **   **          **
!
   IF ( Dry==-2 ) RETURN
   IF ( Nus==0 ) THEN
!
!     IF NO US POINTS
!
!        **  **   **  **
!        *    *   *    *
!        * HM * = * HK *
!        *    *   *    *
!        **  **   **  **
!
      Hm = Hk
      CALL sofcls
   ELSE
!
!     GENERATE ROW PARTITION VECTOR
!
      itrlr1(1) = Hk
      CALL rdtrl(itrlr1)
      itrlr2(1) = Gs
      CALL rdtrl(itrlr2)
      Typin = 1
      Typout = 1
      Irow = 1
      Nrow = itrlr1(2)
      Incr = 1
      DO i = 1 , Nrow
         Rz(Korbgn+i-1) = 0.0
         IF ( i>itrlr2(2) ) Rz(Korbgn+i-1) = 1.0
      ENDDO
      iform = 7
      CALL makmcb(itrlr2,Rprtn,Nrow,iform,Typin)
      CALL gopen(Rprtn,Z(Gbuf1),1)
      CALL pack(Z(Korbgn),Rprtn,itrlr2)
      CALL close(Rprtn,1)
      CALL wrttrl(itrlr2)
!
!     MERGE GS, ZERO MATRICES
!
      isub(1) = itrlr2(2)
      isub(2) = itrlr1(2) - itrlr2(2)
      CALL gmmerg(Gszero,Gs,0,0,0,Rprtn,0,isub,itrlr2(5),Z(Korbgn),Korlen)
!
!     FORM HM MATRIX
!
      itrlr2(1) = Gszero
      CALL rdtrl(itrlr2)
      DO i = 1 , 11
         block(i) = 0.0
      ENDDO
      block(2) = 1.0
      block(8) = 1.0
      typea = itrlr1(5)
      typeb = itrlr2(5)
      iop = 1
      CALL sofcls
      CALL ssg2c(Hk,Gszero,Hm,iop,block)
   ENDIF
!
!     FORM KMW2 = M  MATRIX
!                  I
!
   ifile = Lamamr
   CALL gopen(Lamamr,Z(Gbuf1),0)
   CALL fwdrec(*200,Lamamr)
   iform = 3
   itype = 1
   CALL makmcb(itrlr1,Kmw2,Nmodes,iform,itype)
   Typin = 1
   Typout = 1
   Irow = 1
   Nrow = Nmodes
   Incr = 1
   CALL gopen(Kmw2,Z(Gbuf2),1)
   DO i = 1 , Nmodes
      CALL read(*100,*200,Lamamr,Z(Korbgn),7,0,nwdsrd)
      DO j = 1 , Nmodes
         Rz(Korbgn+7+j-1) = 0.0
         IF ( j==i ) Rz(Korbgn+7+j-1) = Rz(Korbgn+5)
      ENDDO
      CALL pack(Z(Korbgn+7),Kmw2,itrlr1)
   ENDDO
   CALL close(Lamamr,1)
   CALL close(Kmw2,1)
   CALL wrttrl(itrlr1)
!
!     FORM M MATRIX
!
!                      T
!                **  ** **     ** **  **
!        ** **   *    * * .     * *    *
!        *   *   *    * *  .    * *    *
!        * M * = * HM * *   M   * * HM *     WHERE M = M
!        *   *   *    * *    .  * *    *                I
!        ** **   *    * *     . * *    *
!                **  ** **     ** **  **
!
   itrlr1(1) = Hm
   itrlr2(1) = Kmw2
   CALL rdtrl(itrlr1)
   CALL rdtrl(itrlr2)
   DO i = 1 , 7
      Itrlra(i) = itrlr1(i)
      Itrlrb(i) = itrlr2(i)
      Itrlre(i) = 0
   ENDDO
   iprc = 1
   ityp = 0
   IF ( (Itrlra(5)==2) .OR. (Itrlra(5)==4) ) iprc = 2
   IF ( (Itrlrb(5)==2) .OR. (Itrlrb(5)==4) ) iprc = 2
   IF ( Itrlra(5)>=3 ) ityp = 2
   IF ( Itrlrb(5)>=3 ) ityp = 2
   itype = iprc + ityp
   iform = 6
   CALL makmcb(Itrlrc,M,itrlr1(3),iform,itype)
   Jscr(1) = Iscr(7)
   Jscr(2) = Iscr(8)
   Jscr(3) = Iscr(6)
   Icode = 0
   Prec = 0
   dblkor = (Korbgn/2) + 1
   Lkore = Lstzwd - (2*dblkor-1)
   CALL mpy3dr(Dz(dblkor))
   CALL wrttrl(Itrlrc)
   CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
   RETURN
!
!     PROCESS SYSTEM FATAL ERRORS
!
 100  imsg = -2
   GOTO 300
 200  imsg = -3
 300  CALL sofcls
   CALL mesage(imsg,ifile,modnam)
END SUBROUTINE mred2o

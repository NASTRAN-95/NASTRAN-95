
SUBROUTINE mred2n
   IMPLICIT NONE
   INTEGER Dry , Gbuf1 , Gbuf2 , Hk , Icode , Idum1 , Idum2 , Idum3 , Idum4(14) , Idum5(4) , Idum6(2) , Incr , Infile(12) , Irow ,  &
         & Iscr(10) , Itrlra(7) , Itrlrb(7) , Itrlrc(7) , Itrlre(7) , Jscr(3) , K , Kmw2 , Korbgn , Korlen , Lamamr , Lkore ,       &
         & Lstzwd , Nmodes , Nrow , Otfile(6) , Prec , Sbuf1 , Sbuf2 , Sbuf3 , Typin , Typout
   REAL Dummy(13) , Forpi2 , Rz(1) , Z(1)
   DOUBLE PRECISION Dz(1)
   COMMON /blank / Idum1 , Dry , Idum2 , Gbuf1 , Gbuf2 , Idum3 , Sbuf1 , Sbuf2 , Sbuf3 , Infile , Otfile , Iscr , Korlen , Korbgn , &
                 & Idum4 , Nmodes , Idum6 , Lstzwd
   COMMON /condas/ Idum5 , Forpi2
   COMMON /mpy3tl/ Itrlra , Itrlrb , Itrlre , Itrlrc , Jscr , Lkore , Icode , Prec , Dummy
   COMMON /packx / Typin , Typout , Irow , Nrow , Incr
   COMMON /zzzzzz/ Z
   INTEGER dblkor , i , ifile , iform , imsg , iprc , itrlr1(7) , itrlr2(7) , ityp , itype , j , modnam(2) , nwdsrd
!
!     THIS SUBROUTINE CALCULATES THE K MATRIX FOR THE MRED2 MODULE.
!
   !>>>>EQUIVALENCE (Lamamr,Infile(2)) , (Rz(1),Z(1)) , (Dz(1),Z(1)) , (Hk,Iscr(2)) , (Kmw2,Iscr(5)) , (K,Iscr(3))
   DATA modnam/4HMRED , 4H2N  /
!
!                    2
!     FORM KMW2 = M W  MATRIX
!                  I I
!
   IF ( Dry==-2 ) RETURN
   IF ( Korbgn+7+Nmodes>=Korlen ) THEN
      imsg = -8
      ifile = 0
      GOTO 300
   ELSE
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
            IF ( j==i ) Rz(Korbgn+7+j-1) = Forpi2*Rz(Korbgn+5)*(Rz(Korbgn+4)**2)
         ENDDO
         CALL pack(Z(Korbgn+7),Kmw2,itrlr1)
      ENDDO
      CALL close(Lamamr,1)
      CALL close(Kmw2,1)
      CALL wrttrl(itrlr1)
!
!     FORM K MATRIX
!
!                      T
!                       **      **
!        ** **   **  ** * .    0 * **  **
!        *   *   *    * *  .     * *    *                  2
!        * K * = * HK * *   K    * * HK *     WHERE K = M W
!        *   *   *    * *    .   * *    *                I I
!        ** **   **  ** * 0   .  * **  **
!                       **      **
!
      itrlr2(1) = Hk
      CALL rdtrl(itrlr2)
      DO i = 1 , 7
         Itrlra(i) = itrlr2(i)
         Itrlrb(i) = itrlr1(i)
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
      CALL makmcb(Itrlrc,K,itrlr2(3),iform,itype)
      Jscr(1) = Iscr(8)
      Jscr(2) = Iscr(6)
      Jscr(3) = Iscr(2)
      Icode = 0
      Prec = 0
      dblkor = (Korbgn/2) + 1
      Lkore = Lstzwd - (2*dblkor-1)
      CALL sofcls
      CALL mpy3dr(Dz(dblkor))
      CALL wrttrl(Itrlrc)
      CALL sofopn(Z(Sbuf1),Z(Sbuf2),Z(Sbuf3))
      RETURN
   ENDIF
!
!     PROCESS SYSTEM FATAL ERRORS
!
 100  imsg = -2
   GOTO 300
 200  imsg = -3
 300  CALL sofcls
   CALL mesage(imsg,ifile,modnam)
END SUBROUTINE mred2n
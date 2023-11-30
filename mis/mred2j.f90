
SUBROUTINE mred2j(Nuf,N2)
   IMPLICIT NONE
   INTEGER Dry , Gbuf1 , Idum1 , Idum2(5) , Idum3(14) , Idum4 , Incr , Infile(12) , Irow , Iscr(10) , Korbgn , Korlen , Nmodes ,    &
         & Nrow , Otfile(6) , Phiss , Rprtn , Typin , Typout
   REAL Phiss1 , Phiss2 , Rz(1)
   COMMON /blank / Idum1 , Dry , Idum4 , Gbuf1 , Idum2 , Infile , Otfile , Iscr , Korlen , Korbgn , Idum3 , Nmodes
   COMMON /packx / Typin , Typout , Irow , Nrow , Incr
   COMMON /zzzzzz/ Rz
   INTEGER N2 , Nuf
   INTEGER i , ifile , iform , imsg , itrlr1(7) , kolumn , modnam(2)
!
!     THIS SUBROUTINE PARTITIONS THE PHISS MATRIX FOR THE MRED2 MODULE.
!
   !>>>>EQUIVALENCE (Phiss,Infile(3)) , (Phiss1,Iscr(8)) , (Phiss2,Iscr(9)) , (Rprtn,Iscr(10))
   DATA modnam/4HMRED , 4H2J  /
!
!     SET UP PARTITIONING VECTOR
!
   IF ( Dry==-2 ) RETURN
   Typin = 1
   Typout = 1
   Irow = 1
   Incr = 1
!
!     COMMENTS FROM G.CHAN/UNISYS    4/92
!     ORIGINALLY AT THIS POINT, THE FOLLOWING DO 20 LOOP IS IN ERROR
!       1. KOLUMN AND J ARE NOT DEFINED
!       2. NROW AND ITRLR1 ARE ALSO NOT YET DEFINED
!
!     MY BEST GUESS IS THE NEXT 10 LINES THAT FOLLOW
!
   ifile = Phiss
   itrlr1(1) = Phiss
   CALL rdtrl(itrlr1)
   IF ( itrlr1(1)<0 ) THEN
!
!     PROCESS SYSTEM FATAL ERRORS
!
      imsg = -1
      CALL sofcls
      CALL mesage(imsg,ifile,modnam)
      GOTO 99999
   ENDIF
   kolumn = itrlr1(2)
   Nrow = itrlr1(3)
   DO i = 1 , kolumn
      Rz(Korbgn+i-1) = 0.0
      IF ( i>Nuf ) Rz(Korbgn+i-1) = 1.0
   ENDDO
!
   iform = 7
   CALL makmcb(itrlr1,Rprtn,Nrow,iform,itrlr1(5))
   CALL gopen(Rprtn,Rz(Gbuf1),1)
   CALL pack(Rz(Korbgn),Rprtn,itrlr1)
   CALL close(Rprtn,1)
   CALL wrttrl(itrlr1)
!
!     PARTITION PHISS MATRIX
!
!        **     **   **               **
!        *       *   *        .        *
!        * PHISS * = * PHISS1 . PHISS2 *
!        *       *   *        .        *
!        **     **   **               **
!
   itrlr1(1) = Phiss
   CALL rdtrl(itrlr1)
   N2 = Nmodes - Nuf
   CALL gmprtn(Phiss,Phiss1,0,Phiss2,0,Rprtn,0,Nuf,N2,Rz(Korbgn),Korlen)
   RETURN
99999 RETURN
END SUBROUTINE mred2j
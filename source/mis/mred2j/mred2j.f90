!*==mred2j.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred2j(Nuf,N2)
   IMPLICIT NONE
   USE C_BLANK
   USE C_PACKX
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nuf
   INTEGER :: N2
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ifile , iform , imsg , kolumn , phiss , rprtn
   INTEGER , DIMENSION(7) :: itrlr1
   INTEGER , DIMENSION(2) , SAVE :: modnam
   REAL :: phiss1 , phiss2
   EXTERNAL close , gmprtn , gopen , makmcb , mesage , pack , rdtrl , sofcls , wrttrl
!
! End of declarations rewritten by SPAG
!
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
   ifile = phiss
   itrlr1(1) = phiss
   CALL rdtrl(itrlr1)
   IF ( itrlr1(1)<0 ) THEN
!
!     PROCESS SYSTEM FATAL ERRORS
!
      imsg = -1
      CALL sofcls
      CALL mesage(imsg,ifile,modnam)
      RETURN
   ENDIF
   kolumn = itrlr1(2)
   Nrow = itrlr1(3)
   DO i = 1 , kolumn
      Rz(Korbgn+i-1) = 0.0
      IF ( i>Nuf ) Rz(Korbgn+i-1) = 1.0
   ENDDO
!
   iform = 7
   CALL makmcb(itrlr1,rprtn,Nrow,iform,itrlr1(5))
   CALL gopen(rprtn,Rz(Gbuf1),1)
   CALL pack(Rz(Korbgn),rprtn,itrlr1)
   CALL close(rprtn,1)
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
   itrlr1(1) = phiss
   CALL rdtrl(itrlr1)
   N2 = Nmodes - Nuf
   CALL gmprtn(phiss,phiss1,0,phiss2,0,rprtn,0,Nuf,N2,Rz(Korbgn),Korlen)
   RETURN
END SUBROUTINE mred2j

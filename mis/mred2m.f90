
SUBROUTINE mred2m(Nuf,N2,Nus)
   IMPLICIT NONE
   INTEGER Cprtn , Dry , Gbuf1 , Hk , Hkpg , Ident , Idum1 , Idum2(17) , Idum3(14) , Idum4 , Incr , Irow , Iscr(10) , Korbgn ,      &
         & Korlen , Nmodes , Nrow , Otfile(6) , Phi12i , Rprtn , Typin , Typout , Z(1)
   REAL Rz(1)
   COMMON /blank / Idum1 , Dry , Idum4 , Gbuf1 , Idum2 , Otfile , Iscr , Korlen , Korbgn , Idum3 , Nmodes
   COMMON /packx / Typin , Typout , Irow , Nrow , Incr
   COMMON /zzzzzz/ Z
   INTEGER N2 , Nuf , Nus
   INTEGER i , ifile , iform , imsg , isub(4) , itrlr1(7) , itrlr2(7) , itype , j , modnam(2)
!
!     THIS SUBROUTINE FORMS THE HK MATRIX FOR THE MRED2 MODULE.
!
   !>>>>EQUIVALENCE (Hk,Iscr(2)) , (Ident,Iscr(8)) , (Hkpg,Iscr(3)) , (Phi12i,Iscr(8)) , (Cprtn,Iscr(9)) , (Rprtn,Iscr(9))
   !>>>>EQUIVALENCE (Rz(1),Z(1))
   DATA modnam/4HMRED , 4H2M  /
!
!     FORM HK MATRIX
!
!        **  **   **             **
!        *    *   *      .        *
!        * HK * = * HKPG . PHI12I *
!        *    *   *      .        *
!        **  **   **             **
!
   IF ( Dry==-2 ) RETURN
   IF ( Nuf==0 ) THEN
!
!     NO UF POINTS
!
!        **  **   **     **
!        *    *   *   .   *
!        * HK * = * 0 . I *
!        *    *   *   .   *
!        **  **   **     **
!
      Typin = 1
      Typout = 1
      Irow = 1
      Nrow = Nmodes
      Incr = 1
      iform = 8
      IF ( Korbgn+Nmodes<Korlen ) THEN
!
!     GENERATE IDENTITY MATRIX
!
         CALL makmcb(itrlr2,Ident,Nmodes,iform,Typin)
         CALL gopen(Ident,Z(Gbuf1),1)
         DO i = 1 , Nmodes
            DO j = 1 , Nmodes
               Rz(Korbgn+j-1) = 0.0
               IF ( j==i ) Rz(Korbgn+j-1) = 1.0
            ENDDO
            CALL pack(Z(Korbgn),Ident,itrlr2)
         ENDDO
         CALL close(Ident,1)
         CALL wrttrl(itrlr2)
!
!     GENERATE ROW PARTITIONING VECTOR
!
         Nrow = Nus + Nmodes
         IF ( Korbgn+Nrow<Korlen ) THEN
            j = Nrow
            DO i = 1 , j
               Rz(Korbgn+i-1) = 0.0
               IF ( i>Nus ) Rz(Korbgn+i-1) = 1.0
            ENDDO
            iform = 7
            CALL makmcb(itrlr2,Rprtn,Nrow,iform,Typin)
            CALL gopen(Rprtn,Z(Gbuf1),1)
            CALL pack(Z(Korbgn),Rprtn,itrlr2)
            CALL close(Rprtn,1)
            CALL wrttrl(itrlr2)
!
!     FORM HK MATRIX
!
            isub(1) = Nus
            isub(2) = Nmodes
            itype = 2
            CALL gmmerg(Hk,0,0,Ident,0,Rprtn,0,isub,itype,Z(Korbgn),Korlen)
            RETURN
         ENDIF
      ENDIF
!
!     PROCESS SYSTEM ERRORS
!
      imsg = -8
      ifile = 0
      CALL sofcls
      CALL mesage(imsg,ifile,modnam)
   ELSE
      itrlr1(1) = Hkpg
      CALL rdtrl(itrlr1)
      itrlr2(1) = Phi12i
      CALL rdtrl(itrlr2)
      Incr = 1
      Typin = 1
      Typout = 1
      Irow = 1
      Nrow = itrlr1(3) + itrlr2(3)
      isub(1) = itrlr1(3)
      isub(2) = itrlr2(3)
      DO i = 1 , Nrow
         Rz(Korbgn+i-1) = 0.0
         IF ( i>itrlr1(3) ) Rz(Korbgn+i-1) = 1.0
      ENDDO
      iform = 7
      CALL makmcb(itrlr2,Rprtn,Nrow,iform,Typin)
      CALL gopen(Rprtn,Z(Gbuf1),1)
      CALL pack(Z(Korbgn),Rprtn,itrlr2)
      CALL close(Rprtn,1)
      CALL wrttrl(itrlr2)
      itype = 2
      CALL gmmerg(Hk,Hkpg,0,Phi12i,0,Rprtn,0,isub,itype,Z(Korbgn),Korlen)
      RETURN
   ENDIF
!
END SUBROUTINE mred2m
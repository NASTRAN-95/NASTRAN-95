!*==mred2m.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred2m(Nuf,N2,Nus)
   IMPLICIT NONE
   USE C_BLANK
   USE C_PACKX
   USE C_ZZZZZZ
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nuf
   INTEGER :: N2
   INTEGER :: Nus
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: hk , hkpg , i , ident , ifile , iform , imsg , itype , j , phi12i , rprtn
   INTEGER , DIMENSION(4) :: isub
   INTEGER , DIMENSION(7) :: itrlr1 , itrlr2
   INTEGER , DIMENSION(2) , SAVE :: modnam
   REAL , DIMENSION(1) :: rz
   EXTERNAL close , gmmerg , gopen , makmcb , mesage , pack , rdtrl , sofcls , wrttrl
!
! End of declarations rewritten by SPAG
!
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
         CALL makmcb(itrlr2,ident,Nmodes,iform,Typin)
         CALL gopen(ident,Z(Gbuf1),1)
         DO i = 1 , Nmodes
            DO j = 1 , Nmodes
               rz(Korbgn+j-1) = 0.0
               IF ( j==i ) rz(Korbgn+j-1) = 1.0
            ENDDO
            CALL pack(Z(Korbgn),ident,itrlr2)
         ENDDO
         CALL close(ident,1)
         CALL wrttrl(itrlr2)
!
!     GENERATE ROW PARTITIONING VECTOR
!
         Nrow = Nus + Nmodes
         IF ( Korbgn+Nrow<Korlen ) THEN
            j = Nrow
            DO i = 1 , j
               rz(Korbgn+i-1) = 0.0
               IF ( i>Nus ) rz(Korbgn+i-1) = 1.0
            ENDDO
            iform = 7
            CALL makmcb(itrlr2,rprtn,Nrow,iform,Typin)
            CALL gopen(rprtn,Z(Gbuf1),1)
            CALL pack(Z(Korbgn),rprtn,itrlr2)
            CALL close(rprtn,1)
            CALL wrttrl(itrlr2)
!
!     FORM HK MATRIX
!
            isub(1) = Nus
            isub(2) = Nmodes
            itype = 2
            CALL gmmerg(hk,0,0,ident,0,rprtn,0,isub,itype,Z(Korbgn),Korlen)
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
      itrlr1(1) = hkpg
      CALL rdtrl(itrlr1)
      itrlr2(1) = phi12i
      CALL rdtrl(itrlr2)
      Incr = 1
      Typin = 1
      Typout = 1
      Irow = 1
      Nrow = itrlr1(3) + itrlr2(3)
      isub(1) = itrlr1(3)
      isub(2) = itrlr2(3)
      DO i = 1 , Nrow
         rz(Korbgn+i-1) = 0.0
         IF ( i>itrlr1(3) ) rz(Korbgn+i-1) = 1.0
      ENDDO
      iform = 7
      CALL makmcb(itrlr2,rprtn,Nrow,iform,Typin)
      CALL gopen(rprtn,Z(Gbuf1),1)
      CALL pack(Z(Korbgn),rprtn,itrlr2)
      CALL close(rprtn,1)
      CALL wrttrl(itrlr2)
      itype = 2
      CALL gmmerg(hk,hkpg,0,phi12i,0,rprtn,0,isub,itype,Z(Korbgn),Korlen)
      RETURN
   ENDIF
!
END SUBROUTINE mred2m

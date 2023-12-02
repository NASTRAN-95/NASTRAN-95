!*==mred2m.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mred2m(Nuf,N2,Nus)
   USE c_blank
   USE c_packx
   USE c_zzzzzz
   IMPLICIT NONE
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
   IF ( dry==-2 ) RETURN
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
      typin = 1
      typout = 1
      irow = 1
      nrow = nmodes
      incr = 1
      iform = 8
      IF ( korbgn+nmodes<korlen ) THEN
!
!     GENERATE IDENTITY MATRIX
!
         CALL makmcb(itrlr2,ident,nmodes,iform,typin)
         CALL gopen(ident,z(gbuf1),1)
         DO i = 1 , nmodes
            DO j = 1 , nmodes
               rz(korbgn+j-1) = 0.0
               IF ( j==i ) rz(korbgn+j-1) = 1.0
            ENDDO
            CALL pack(z(korbgn),ident,itrlr2)
         ENDDO
         CALL close(ident,1)
         CALL wrttrl(itrlr2)
!
!     GENERATE ROW PARTITIONING VECTOR
!
         nrow = Nus + nmodes
         IF ( korbgn+nrow<korlen ) THEN
            j = nrow
            DO i = 1 , j
               rz(korbgn+i-1) = 0.0
               IF ( i>Nus ) rz(korbgn+i-1) = 1.0
            ENDDO
            iform = 7
            CALL makmcb(itrlr2,rprtn,nrow,iform,typin)
            CALL gopen(rprtn,z(gbuf1),1)
            CALL pack(z(korbgn),rprtn,itrlr2)
            CALL close(rprtn,1)
            CALL wrttrl(itrlr2)
!
!     FORM HK MATRIX
!
            isub(1) = Nus
            isub(2) = nmodes
            itype = 2
            CALL gmmerg(hk,0,0,ident,0,rprtn,0,isub,itype,z(korbgn),korlen)
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
      incr = 1
      typin = 1
      typout = 1
      irow = 1
      nrow = itrlr1(3) + itrlr2(3)
      isub(1) = itrlr1(3)
      isub(2) = itrlr2(3)
      DO i = 1 , nrow
         rz(korbgn+i-1) = 0.0
         IF ( i>itrlr1(3) ) rz(korbgn+i-1) = 1.0
      ENDDO
      iform = 7
      CALL makmcb(itrlr2,rprtn,nrow,iform,typin)
      CALL gopen(rprtn,z(gbuf1),1)
      CALL pack(z(korbgn),rprtn,itrlr2)
      CALL close(rprtn,1)
      CALL wrttrl(itrlr2)
      itype = 2
      CALL gmmerg(hk,hkpg,0,phi12i,0,rprtn,0,isub,itype,z(korbgn),korlen)
      RETURN
   ENDIF
!
END SUBROUTINE mred2m

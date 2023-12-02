!*==read4.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE read4(Lama,Phi,Scr1,Eps,Mass)
   USE c_names
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_xmssg
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Lama
   INTEGER , DIMENSION(7) :: Phi
   INTEGER :: Scr1
   REAL :: Eps
   INTEGER :: Mass
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: dz
   REAL :: eps1 , eps2 , epsi , rmult
   INTEGER :: i , ibuf , ibuf1 , ibuf2 , iclos , idid , ifile , ii , ij , iout , ipos , ipr , isys , j , kore , n , ncol , no ,     &
            & nrow , num , nz
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(7) :: phi1
   EXTERNAL close , gopen , korsz , makmcb , mesage , ortck , pack , read , unpack , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     READ4 WILL TEST FOR CLOSE AND EQUAL ROOTS AND MAKE SURE THE
!     CORRESPONDING VECTORS ARE ORTHOGONAL
!
!WKBI ALPHA-OSF 9/94
   !>>>>EQUIVALENCE (Ksystm(1),Isys) , (Ksystm(2),Iout) , (Dz(1),Z(1))
   DATA name/4HREAD , 4H4   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         ncol = Phi(2)
         nrow = Phi(3)
         nz = korsz(z)
         ibuf = nz - isys
         ibuf1 = ibuf - isys
         ibuf2 = ibuf1 - isys
         iclos = 0
         idid = 0
         ipr = Phi(5)
         rmult = .01
         itype = rsp
         iunpak = 1
         junpak = nrow
         incr = 1
         itypa = rsp
         itypb = rsp
         ipak = 1
         jpak = nrow
         incrx = 1
         epsi = Eps
         IF ( Eps<=0. ) epsi = .0001
         nz = nz - isys - isys - 1 - isys
         CALL makmcb(phi1,Scr1,nrow,2,rsp)
         ifile = Lama
         CALL gopen(Lama,z(ibuf),0)
         CALL read(*40,*20,Lama,z(1),nz,1,n)
         spag_nextblock_1 = 6
         CYCLE SPAG_DispatchLoop_1
 20      CALL close(Lama,rew)
!
!     REJECT ALL BUT VALUES FOR WHICH VECTORS EXIST
!
         n = Phi(2)
         nz = nz - n
         IF ( nz<nrow ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         ifile = Phi(1)
         CALL gopen(Phi,z(ibuf),0)
         ipos = 1
         i = 1
         eps1 = rmult
         spag_nextblock_1 = 2
      CASE (2)
         IF ( abs(z(i))+abs(z(i+1))>=eps1 ) THEN
            IF ( z(i+1)==0.0 ) THEN
               spag_nextblock_1 = 5
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            IF ( abs(1.0-z(i)/z(i+1))>eps1 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         IF ( iclos==0 ) iclos = i
         spag_nextblock_1 = 5
      CASE (3)
         num = i - iclos + 1
         eps1 = rmult
!
!     NUM   = NUMBER OF CLOSE ROOTS IN THIS GROUP
!     ICLOS = THE INDEX OF THE FIRST CLOSE ROOT
!
         IF ( idid/=1 ) THEN
            idid = 1
            ifile = Scr1
            CALL gopen(Scr1,z(ibuf1),wrtrew)
         ENDIF
         ii = n + 1
         DO WHILE ( ipos/=iclos )
            ifile = Phi(1)
            CALL unpack(*60,Phi,z(ii))
            CALL pack(z(ii),Scr1,phi1)
            ipos = ipos + 1
         ENDDO
!
!     CHECK FOR CORE OVERFLOW
!     EIGENVALUES + EIGENVECTORS + GEN. MASS + ACCUM.
!
         kore = ii + num*nrow + num*num + n + n + 3
         IF ( kore>nz ) THEN
!
            eps2 = eps1/10.
            WRITE (iout,99001) uwm , num , i , eps1 , eps2
99001       FORMAT (A25,' 3142, INSUFFICIENT CORE STORAGE FOR EIGENVECTORS ','ASSOCIATED WITH',I4,                                  &
                   &' MULTIPLE EIGENVALUES STARTING WITH',/28X,'MODE NUMBER',I4,' USING CURRENT MULTIPLE ROOT ',                    &
                   &'CRITERIA. CRITERIA REDUCED FROM ',1P,E12.5,' TO ',E12.5)
            eps1 = eps2
            i = iclos
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ELSE
            DO j = 1 , num
               CALL unpack(*60,Phi,z(ii))
               ipos = ipos + 1
               ii = ii + nrow
               IF ( ii+nrow>=nz ) THEN
                  spag_nextblock_1 = 6
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
            ENDDO
            ij = ii + n + n + 3
            ii = ii/2 + 1
            CALL ortck(z(n+1),Mass,z(ibuf2),num,nrow,z(ij),dz(ii),epsi)
            ii = n + 1
            DO j = 1 , num
               CALL pack(z(ii),Scr1,phi1)
               ii = ii + nrow
            ENDDO
            iclos = 0
         ENDIF
         spag_nextblock_1 = 4
      CASE (4)
         IF ( iclos/=0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (5)
         i = i + 1
         IF ( i<n ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( iclos/=0 ) THEN
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( idid/=0 ) THEN
            IF ( ipos<=ncol ) THEN
               DO i = ipos , ncol
                  CALL unpack(*60,Phi,z)
                  CALL pack(z(1),Scr1,phi1)
               ENDDO
            ENDIF
            CALL wrttrl(phi1)
!
!     COPY VECTORS FROM SCR1 TO PHI
!
            CALL close(Phi,rew)
            CALL close(Scr1,rew)
            CALL gopen(Phi,z(ibuf),1)
            CALL gopen(Scr1,z(ibuf1),rdrew)
            CALL makmcb(Phi,Phi,nrow,2,ipr)
            itypb = ipr
            DO i = 1 , n
               CALL unpack(*60,Scr1,z)
               CALL pack(z,Phi,Phi)
            ENDDO
            CALL wrttrl(Phi)
            CALL close(Scr1,rew)
         ENDIF
         CALL close(Phi,rew)
         RETURN
 40      no = -2
         spag_nextblock_1 = 7
      CASE (6)
         no = -8
         spag_nextblock_1 = 7
         CYCLE SPAG_DispatchLoop_1
 60      no = -7
         spag_nextblock_1 = 7
      CASE (7)
         CALL mesage(no,ifile,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE read4

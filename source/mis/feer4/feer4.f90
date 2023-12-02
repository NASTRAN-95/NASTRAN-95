!*==feer4.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE feer4(It)
   USE c_feercx
   USE c_feerxx
   USE c_machin
   USE c_names
   USE c_opinv
   USE c_packx
   USE c_system
   USE c_unpakx
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: It
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(2) :: b
   REAL(REAL64) :: dsce , dsm
   REAL(REAL64) , DIMENSION(1) :: dz
   REAL :: erf , sce , sm
   INTEGER :: i , ibuf1 , ibuf2 , ibuf3 , icavl , icreq , iend , ier , iopn , isrv , iv1 , iv2 , iv3 , iv4 , iv5 , iv6 , iv7 , iv8 ,&
            & iv9 , ivr , ivw , ix1 , ix2 , ix3 , ix4 , ix5 , j , l , l26 , l43 , m , mdim , mflg , mred , nw , nz
   INTEGER , DIMENSION(2) , SAVE :: icr , name
   LOGICAL :: incore
   INTEGER , DIMENSION(1) :: iz
   INTEGER , DIMENSION(7) :: mcbc
   REAL , DIMENSION(2) :: sb
   EXTERNAL close , fqrw , fqrwv , gopen , korsz , makmcb , mesage , pack , page2 , read , rewind , skprec , sswtch , unpack , write
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     FEER4 OBTAINS FROM THE REDUCED TRIDIAGONAL MATRIX THE EIGENVALUES
!     AND EIGENVECTORS
!
!WKBNB NCL93007 11/94
!WKBNE NCL93007 11/94
   !>>>>EQUIVALENCE (Iz(1),Z(1),Dz(1)) , (sb(1),b(1)) , (dsce,sce)
   DATA name/4HFEER , 4H4   / , icr/4HPASS , 4HFAIL/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     SR4FLE CONTAINS THE EIGENVECTORS OF THE REDUCED PROBLEM
!     SR5FLE CONTAINS THE TRIDIAGONAL ELEMENTS AND SCRATCH IN FQRWV
!     SR6FLE CONTAINS THE G VECTORS
!     SR7FLE CONTAINS THE ORTHOGONAL VECTORS
!
         CALL sswtch(26,l26)
         mdim = mord + 1
         dsm = 10.0D+0**(-2*It/3)
         sm = dsm
         iprc = mcbrm(5)
         nz = korsz(z)
         CALL makmcb(mcbc(1),sr4fle,mdim,2,iprc)
         mcbc(2) = 0
         mcbc(6) = 0
         m = 0
!
!     INITIALIZE ALLOCATIONS
!
         ibuf1 = nz - sysbuf
         ibuf2 = ibuf1 - sysbuf
         ibuf3 = ibuf2 - sysbuf
         iv1 = 1
         iv2 = iv1 + mdim
         iv3 = iv2 + mdim
         iv4 = iv3 + mdim
         iv5 = iv4 + mdim
         iv6 = iv5 + mdim
         iv7 = iv6 + mdim
         iv8 = iv7 + mdim
         iv9 = iv8 + mdim
         ix3 = iv3 - 1
         ix4 = iv4 - 1
         iend = iprc*(8*mdim+1) + mdim
         IF ( iend>ibuf3 ) CALL mesage(-8,iend-ibuf3,name)
         CALL gopen(sr5fle,z(ibuf2),rdrew)
         IF ( iprc==2 ) dz(iv4+mord) = errc
         IF ( iprc==1 ) z(iv4+mord) = errc
         nw = iprc*2
         DO i = 1 , mord
            CALL read(*40,*60,sr5fle,b(1),nw,1,m)
            IF ( iprc==1 ) THEN
               z(ix3+i) = sb(1)
               z(ix4+i) = sb(2)
            ELSE
               dz(ix3+i) = b(1)
               dz(ix4+i) = b(2)
            ENDIF
         ENDDO
         CALL close(sr5fle,rew)
         CALL gopen(sr4fle,z(ibuf2),wrtrew)
         IF ( iprc==1 ) THEN
            CALL fqrw(mord,z(iv1),z(iv2),z(iv3),z(iv4),z(iv5),z(iv6),z(iv7),z(iv8),z(iv9),z(ibuf1),sr5fle,mcbc(1))
         ELSE
!                                                              SR4FLE
            CALL fqrwv(mord,dz(iv1),dz(iv2),dz(iv3),dz(iv4),dz(iv5),dz(iv6),dz(iv7),dz(iv8),dz(iv9),z(ibuf1),sr5fle,mcbc(1))
         ENDIF
!                                                          SR4FLE
         CALL close(sr4fle,norew)
!
!     RECONFIGURE VECTOR INDEX TO OBTAIN PHYSICAL EIGENVECTORS
!
         ix1 = iv1 - 1
         ix2 = iv2 - 1
         ix3 = iv3 - 1
         ix4 = iv4 - 1
         ix5 = ix4 + nord
         isrv = mcbrm(1)
         iflvec(1) = iflrvc
         iflelm(1) = iflrva
         IF ( nzero==0 ) THEN
!
!     PREPARE FILES WHEN NO RESTART AND/OR RIGID BODY VECTORS
!
            iflvec(2) = 0
            iflvec(6) = 0
            CALL gopen(iflrvc,z(ibuf3),wrtrew)
            CALL close(iflrvc,norew)
            CALL gopen(iflrva,z(ibuf3),wrtrew)
            CALL close(iflrva,norew)
         ENDIF
         itp1 = iprc
         itp2 = 1
         incrp = 1
         ii = 1
         CALL gopen(iflrva,z(ibuf1),wrt)
         mred = 0
         mflg = 1
         DO m = 1 , mord
            IF ( iprc==1 ) THEN
               sce = 1.0/z(ix1+m) - lambda
               IF ( l16/=0 ) THEN
                  erf = 0.0D+0
                  IF ( abs(sce)>sm ) erf = 100.0D+0*z(ix2+m)/dabs(1.0D+0-z(ix1+m)*lambda)
                  z(ix2+m) = sce
                  IF ( erf>critf ) mflg = 2
               ENDIF
            ELSE
               dsce = 1.0D+0/dz(ix1+m) - lambda
               IF ( l16/=0 ) THEN
                  erf = 0.0D+0
                  IF ( dabs(dsce)>dsm ) erf = 100.D0*dz(ix2+m)/dabs(1.D0-dz(ix1+m)*lambda)
                  dz(ix2+m) = dsce
                  IF ( erf>critf ) mflg = 2
               ENDIF
            ENDIF
            IF ( mflg/=2 ) THEN
               mred = mred + 1
               CALL write(iflrva,dsce,iprec,1)
            ENDIF
            IF ( l16/=0 ) THEN
               CALL page2(1)
               IF ( iprc==2 ) WRITE (io,99002) m , dsce , erf , icr(mflg)
               IF ( iprc==1 ) WRITE (io,99002) m , sce , erf , icr(mflg)
            ENDIF
         ENDDO
         CALL close(iflrva,eofnrw)
         IF ( mord==0 ) RETURN
!
         CALL gopen(isrv,z(ibuf1),rdrew)
         CALL gopen(sr4fle,z(ibuf2),rdrew)
         CALL gopen(iflrvc,z(ibuf3),wrt)
!WKBNB NCL93007 11/94
         incore = .FALSE.
         CALL sswtch(43,l43)
         IF ( l43==0 ) THEN
            ivw = ix5 + nord + 1
            icreq = nord*mord*iprc
            icavl = ibuf3 - ivw - 1
            IF ( icavl>icreq ) incore = .TRUE.
            IF ( incore ) THEN
               nn = nord
               DO i = 1 , mord
                  ivr = ivw + (i-1)*nord
                  IF ( iprc==1 ) CALL unpack(*5,isrv,z(ivr+1))
                  IF ( iprc==2 ) CALL unpack(*5,isrv,dz(ivr+1))
 5             ENDDO
            ENDIF
         ENDIF
!WKBNE NCL93007 11/94
!
!     IF DIAG 26 IS OFF, LIMIT EIGENSOLUTIONS TO NUMBER REQUESTED
!
         IF ( mred>=neig .AND. l26/=0 ) mred = neig
         IF ( iprc==1 ) THEN
            DO m = 1 , mred
               DO l = 1 , nord
                  z(ix5+l) = 0.0
               ENDDO
               nn = nord
               CALL unpack(*10,sr4fle,z(iv3))
               nn = nord
!WKBI NCL93007 11/94
               IF ( incore ) THEN
                  DO i = 1 , mord
                     ivr = ivw + (i-1)*nord
                     DO j = 1 , nord
                        z(ix5+j) = z(ix5+j) + z(ivr+j)*z(ix3+i)
                     ENDDO
                  ENDDO
               ELSE
                  DO i = 1 , mord
                     CALL unpack(*20,isrv,z(iv4))
                     DO j = 1 , nord
                        z(ix5+j) = z(ix5+j) + z(ix4+j)*z(ix3+i)
                     ENDDO
!WKBNB NCL93007 11/94
                  ENDDO
               ENDIF
!WKBNE NCL93007 11/94
 10            IF ( ioptf/=0 ) THEN
                  sce = 1.0/sqrt(abs(z(ix1+m)))
                  DO l = 1 , nord
                     z(ix5+l) = sce*z(ix5+l)
                  ENDDO
               ENDIF
               iip = 1
               nnp = nord
               CALL pack(z(ix5+1),iflrvc,iflvec(1))
!WKBI NCL93007 11/94
               IF ( .NOT.(incore) ) THEN
                  CALL rewind(mcbrm)
                  CALL skprec(mcbrm,1)
               ENDIF
 20         ENDDO
         ELSE
            DO m = 1 , mred
               DO l = 1 , nord
                  dz(ix5+l) = 0.0D+0
               ENDDO
               nn = mord
               CALL unpack(*25,sr4fle,dz(iv3))
               nn = nord
!WKBI NCL93007 11/94
               IF ( incore ) THEN
                  DO i = 1 , mord
                     ivr = ivw + (i-1)*nord
                     DO j = 1 , nord
                        dz(ix5+j) = dz(ix5+j) + dz(ivr+j)*dz(ix3+i)
                     ENDDO
                  ENDDO
               ELSE
                  DO i = 1 , mord
                     CALL unpack(*30,isrv,dz(iv4))
                     DO j = 1 , nord
                        dz(ix5+j) = dz(ix5+j) + dz(ix4+j)*dz(ix3+i)
                     ENDDO
!WKBNB NCL93007 11/94
                  ENDDO
               ENDIF
!WKBNE NCL93007 11/94
 25            IF ( ioptf/=0 ) THEN
                  dsce = 1.0D+0/dsqrt(dabs(dz(ix1+m)))
                  DO l = 1 , nord
                     dz(ix5+l) = dsce*dz(ix5+l)
                  ENDDO
               ENDIF
               iip = 1
               nnp = nord
               CALL pack(dz(ix5+1),iflrvc,iflvec(1))
!WKBI NCL93007 11/94
               IF ( .NOT.(incore) ) THEN
                  CALL rewind(mcbrm)
                  CALL skprec(mcbrm,1)
               ENDIF
 30         ENDDO
         ENDIF
!
         CALL close(iflrvc,eofnrw)
         CALL close(isrv,rew)
         CALL close(sr4fle,rew)
         mord = mred
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
 40      ier = 2
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 60      ier = 3
         spag_nextblock_1 = 2
      CASE (2)
         cndflg = 4
         CALL mesage(ier,sr5fle,name)
         spag_nextblock_1 = 3
      CASE (3)
         iopn = ibuf3 - iend
         IF ( l16==1 ) WRITE (io,99001) iopn , name
99001    FORMAT ('  OPEN CORE NOT USED',I10,2X,2A4)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99002 FORMAT (10X,'PHYSICAL EIGENVALUE',I5,1P,E16.8,'  THEOR ERROR ',E16.8,'  PERCENT',5X,A4)
END SUBROUTINE feer4

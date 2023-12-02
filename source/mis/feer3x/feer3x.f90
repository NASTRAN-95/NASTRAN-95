!*==feer3x.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE feer3x
   USE c_feercx
   USE c_feerxx
   USE c_names
   USE c_opinv
   USE c_packx
   USE c_reigkr
   USE c_system
   USE c_type
   USE c_unpakx
   USE c_zzzzzz
   USE iso_fortran_env
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: dashq
   REAL(REAL64) :: dsq
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER :: i , ibuf1 , ibuf2 , ibuf3 , ibuf4 , icrq , iend , ifl , ifn , ij , iv1 , iv2 , iv3 , iv4 , iv5 , ix2 , j , k , nwds , &
            & nz , sr10fl , sr9fle , srxfle
   INTEGER , DIMENSION(7) :: mcbscl
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL :: sq , ss
   EXTERNAL close , fnxtv , fnxtvc , fnxtvd , frmlta , frmltx , gopen , korsz , makmcb , mesage , pack , page2 , rdtrl , sswtch ,   &
          & unpack , unpscr
!
! End of declarations rewritten by SPAG
!
!                                                               T
!     FEER3 OBTAINS THE REDUCED TRIDIAGONAL MATRIX   (LI)*M*(LI)
!     WHERE M IS A SYMETRIC MATRIX AND L IS LOWER TRIANGULAR, AND (LI)
!     IS INVERSE OF L
!
!     THE TRANSFORMATION IS ALPHA = VT(L**(-1)M (L**-(1))TV
!     WHERE V IS A RECTANGULAR TRANSFORMATION.
!
!     LAST REVISED 11/91 BY G.CHAN/UNISYS, MAKE ROOM FOR NEW FBS METHOD
!
   !>>>>EQUIVALENCE (Iz(1),Z(1),Dz(1))
   DATA name/4HFEER , 4H3   / , dashq/4H-Q  /
!
!     SR5FLE CONTAINS THE TRIDIAGONAL ELEMENTS
!     SR6FLE CONTAINS THE G VECTORS
!     SR7FLE CONTAINS THE ORTHOGONAL VECTORS
!     SR8FLE CONTAINS THE CONDITIONED MAA OR KAAD MATRIX
!     SR9FLE CONTAINS MCBSMA DATA IN UNPACKED FORM = 309
!     SR10FL CONTAINS MCBLT  DATA IN UNPACKED FORM = 310
!                                              (OR = 308 IF IT IS FREE)
!     IFLVEC CONTAINS THE L OR C MATRIX FROM SDCOMP
!     IFLELM CONTAINS     KAA+ALPHA*MAA
!     IFLRVC CONTAINS THE RESTART AND/OR RIGID BODY VECTORS
!
   sr9fle = 309
   sr10fl = 308
   iprc = mcblt(5)
   nwds = iwords(iprc)
   nz = korsz(z)
   CALL makmcb(mcbvec(1),sr7fle,nord,2,iprc)
   mcbvec(2) = 0
   mcbvec(6) = 0
   CALL makmcb(mcbrm(1),sr6fle,mord,2,iprc)
   mcbrm(2) = 0
   mcbrm(6) = 0
   mcbscl(1) = iflrvc
   CALL rdtrl(mcbscl(1))
!
!     INITIALIZE ALLOCATIONS
!
   ibuf1 = nz - sysbuf
   ibuf2 = ibuf1 - sysbuf
   ibuf3 = ibuf2 - sysbuf
   ibuf4 = ibuf3 - sysbuf
   iv1 = 1
   iv2 = iv1 + nord
   iv3 = iv2 + nord
   iv4 = iv3 + nord
   iv5 = iv4 + nord
   nzv5 = ibuf4 - iv5*nwds - 2
   ix2 = iv2 - 1
   iend = nwds*(5*nord+1) + 2
   icrq = iend - ibuf4
   IF ( icrq>0 ) CALL mesage(-8,icrq,name)
   ifl = mcblt(1)
   srxfle = sr8fle
!
!     CALL UNPSCR TO MOVE MCBSMA DATA INTO SR9FLE, AND MCBLT INTO SR10FL
!     (ORIGINAL MCBSMA AND MCBLT TRAILER WORDS 4,5,6,7 WILL BE CHANGED)
!     NZV5 IS THE AVAILABE SIZE OF THE WORKING SPACE FOR NEW FBS METHOD
!     USED IN FRSW/2, FRBK/2, FRMLT/D, AND FRMLTX/A ROUTINES
!
!     IF KSYS94 IS 10000 OR DIAG 41 IS ON, NEW FBS METHODS AND UNPSCR
!     ARE NOT USED
!
   IF ( mod(ksys94,100000)/10000/=1 ) THEN
      CALL sswtch(41,i)
      IF ( i/=1 ) THEN
         srxfle = sr9fle
         CALL unpscr(mcbsma,srxfle,z,ibuf2,ibuf1,nzv5,0,1)
         j = 2
         IF ( ioptf==1 ) j = 3
         CALL unpscr(mcblt,sr10fl,z,ibuf2,ibuf1,nzv5,0,j)
         nzv5 = nzv5 + 1
         ifl = sr10fl
      ENDIF
   ENDIF
!
   CALL gopen(ifl,z(ibuf3),rdrew)
   CALL gopen(sr7fle,z(ibuf1),wrtrew)
   IF ( northo/=0 ) THEN
!
!     LOAD RESTART AND/OR RIGID BODY VECTORS
!
      CALL gopen(iflrvc,z(ibuf2),rdrew)
      incr = 1
      incrp = 1
      itp1 = iprc
      itp2 = iprc
!
      DO j = 1 , northo
         ii = 1
         nn = nord
         CALL unpack(*50,iflrvc,dz(1))
         iip = ii
         nnp = nn
         IF ( iprc==1 ) THEN
            IF ( ioptf/=0 ) THEN
               sq = 0.0
               CALL frmlta(mcblt(1),z(iv1),z(iv2),z(iv3))
               DO ij = 1 , nord
                  sq = sq + z(ix2+ij)**2
               ENDDO
               sq = 1.0/sqrt(sq)
               DO ij = 1 , nord
                  z(ij) = sq*z(ix2+ij)
               ENDDO
            ENDIF
            IF ( l16/=0 ) THEN
               CALL page2(2)
               WRITE (io,99003) iip , nnp , (z(i),i=1,nord)
            ENDIF
         ELSE
            IF ( ioptf/=0 ) THEN
               dsq = 0.D0
               CALL frmltx(mcblt(1),dz(iv1),dz(iv2),dz(iv3))
               DO ij = 1 , nord
                  dsq = dsq + dz(ix2+ij)**2
               ENDDO
               dsq = 1.D0/dsqrt(dsq)
               DO ij = 1 , nord
                  dz(ij) = dsq*dz(ix2+ij)
               ENDDO
            ENDIF
            IF ( l16/=0 ) THEN
               CALL page2(2)
               WRITE (io,99003) iip , nnp , (dz(i),i=1,nord)
            ENDIF
         ENDIF
         CALL pack(dz(1),sr7fle,mcbvec(1))
 50   ENDDO
!
      CALL close(iflrvc,norew)
      IF ( l16/=0 ) THEN
         CALL page2(1)
         WRITE (io,99001) northo , mcbvec
99001    FORMAT (5X,I5,16H ORTH VECTORS ON,I5,5H FILE,5I5,I14)
      ENDIF
   ENDIF
   k = northo
   CALL close(sr7fle,norew)
   j = k
   nonul = 0
   iter = 0
   CALL gopen(sr6fle,z(ibuf4),wrtrew)
   CALL close(sr6fle,norew)
   CALL gopen(srxfle,z(ibuf2),rdrew)
   CALL gopen(sr5fle,z(ibuf4),wrtrew)
   SPAG_Loop_1_1: DO
!
!     GENERATE SEED VECTOR
!
      k = k + 1
      j = k
      ifn = 0
!
!     GENERATE SEED VECTOR FOR LANCZOS
!
      ss = 1.0
      IF ( iprc==1 ) THEN
!
         DO i = 1 , nord
            ss = -ss
            j = j + 1
            sq = float(mod(j,3)+1)/(3.0*float((mod(j,13)+1)*(1+5*i/nord)))
            z(ix2+i) = sq*ss
         ENDDO
         IF ( optn2/=dashq ) CALL fnxtv(z(iv1),z(iv2),z(iv3),z(iv4),z(iv5),z(ibuf1),ifn)
         IF ( optn2==dashq ) CALL fnxtvd(z(iv1),z(iv2),z(iv3),z(iv4),z(iv5),z(ibuf1),ifn)
      ELSE
         DO i = 1 , nord
            ss = -ss
            j = j + 1
            dsq = float(mod(j,3)+1)/(3.0*float((mod(j,13)+1)*(1+5*i/nord)))
            dz(ix2+i) = dsq*ss
         ENDDO
         IF ( optn2/=dashq ) CALL fnxtvc(dz(iv1),dz(iv2),dz(iv3),dz(iv4),dz(iv5),z(ibuf1),ifn)
      ENDIF
!
      IF ( iter>mord ) THEN
         mord = northo - nzero
         cndflg = 3
         EXIT SPAG_Loop_1_1
!
      ELSEIF ( ifn>=mord ) THEN
         EXIT SPAG_Loop_1_1
      ENDIF
   ENDDO SPAG_Loop_1_1
   CALL close(sr5fle,norew)
   CALL close(srxfle,rew)
   CALL close(ifl,rew)
!
!     IF NEW FBS METHOD IS USED, SR9FLE AND SR10FL FILES COULD BE VERY
!     BIG. MAKE SURE THEY ARE PHYSICALLY REDUCED TO ZERO SIZE. THIS IS
!     IMPORTANT FOR A COMPUTER SYSTEM WITH LIMITED DISC SPACE
!
   IF ( ifl==sr10fl ) THEN
      CALL gopen(sr9fle,z(ibuf2),wrtrew)
      CALL gopen(sr10fl,z(ibuf3),wrtrew)
      CALL close(sr9fle,rew)
      CALL close(sr10fl,rew)
   ENDIF
!
   IF ( l16==0 ) RETURN
   CALL page2(1)
   i = ibuf4 - northo*nord*nwds - 2
   IF ( i<0 ) i = ibuf4 - iend
   WRITE (io,99002) i , name
99002 FORMAT (19H OPEN CORE NOT USED,I10,2X,2A4)
99003 FORMAT (10H ORTH VCT ,2I5,/(1X,8E16.8))
!
END SUBROUTINE feer3x

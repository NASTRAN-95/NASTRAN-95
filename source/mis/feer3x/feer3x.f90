!*==feer3x.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE feer3x
USE C_FEERCX
USE C_FEERXX
USE C_NAMES
USE C_OPINV
USE C_PACKX
USE C_REIGKR
USE C_SYSTEM
USE C_TYPE
USE C_UNPAKX
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
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
   Iprc = Mcblt(5)
   nwds = Iwords(Iprc)
   nz = korsz(Z)
   CALL makmcb(Mcbvec(1),Sr7fle,Nord,2,Iprc)
   Mcbvec(2) = 0
   Mcbvec(6) = 0
   CALL makmcb(Mcbrm(1),Sr6fle,Mord,2,Iprc)
   Mcbrm(2) = 0
   Mcbrm(6) = 0
   mcbscl(1) = Iflrvc
   CALL rdtrl(mcbscl(1))
!
!     INITIALIZE ALLOCATIONS
!
   ibuf1 = nz - Sysbuf
   ibuf2 = ibuf1 - Sysbuf
   ibuf3 = ibuf2 - Sysbuf
   ibuf4 = ibuf3 - Sysbuf
   iv1 = 1
   iv2 = iv1 + Nord
   iv3 = iv2 + Nord
   iv4 = iv3 + Nord
   iv5 = iv4 + Nord
   Nzv5 = ibuf4 - iv5*nwds - 2
   ix2 = iv2 - 1
   iend = nwds*(5*Nord+1) + 2
   icrq = iend - ibuf4
   IF ( icrq>0 ) CALL mesage(-8,icrq,name)
   ifl = Mcblt(1)
   srxfle = Sr8fle
!
!     CALL UNPSCR TO MOVE MCBSMA DATA INTO SR9FLE, AND MCBLT INTO SR10FL
!     (ORIGINAL MCBSMA AND MCBLT TRAILER WORDS 4,5,6,7 WILL BE CHANGED)
!     NZV5 IS THE AVAILABE SIZE OF THE WORKING SPACE FOR NEW FBS METHOD
!     USED IN FRSW/2, FRBK/2, FRMLT/D, AND FRMLTX/A ROUTINES
!
!     IF KSYS94 IS 10000 OR DIAG 41 IS ON, NEW FBS METHODS AND UNPSCR
!     ARE NOT USED
!
   IF ( mod(Ksys94,100000)/10000/=1 ) THEN
      CALL sswtch(41,i)
      IF ( i/=1 ) THEN
         srxfle = sr9fle
         CALL unpscr(Mcbsma,srxfle,Z,ibuf2,ibuf1,Nzv5,0,1)
         j = 2
         IF ( Ioptf==1 ) j = 3
         CALL unpscr(Mcblt,sr10fl,Z,ibuf2,ibuf1,Nzv5,0,j)
         Nzv5 = Nzv5 + 1
         ifl = sr10fl
      ENDIF
   ENDIF
!
   CALL gopen(ifl,Z(ibuf3),Rdrew)
   CALL gopen(Sr7fle,Z(ibuf1),Wrtrew)
   IF ( Northo/=0 ) THEN
!
!     LOAD RESTART AND/OR RIGID BODY VECTORS
!
      CALL gopen(Iflrvc,Z(ibuf2),Rdrew)
      Incr = 1
      Incrp = 1
      Itp1 = Iprc
      Itp2 = Iprc
!
      DO j = 1 , Northo
         Ii = 1
         Nn = Nord
         CALL unpack(*50,Iflrvc,dz(1))
         Iip = Ii
         Nnp = Nn
         IF ( Iprc==1 ) THEN
            IF ( Ioptf/=0 ) THEN
               sq = 0.0
               CALL frmlta(Mcblt(1),Z(iv1),Z(iv2),Z(iv3))
               DO ij = 1 , Nord
                  sq = sq + Z(ix2+ij)**2
               ENDDO
               sq = 1.0/sqrt(sq)
               DO ij = 1 , Nord
                  Z(ij) = sq*Z(ix2+ij)
               ENDDO
            ENDIF
            IF ( L16/=0 ) THEN
               CALL page2(2)
               WRITE (Io,99003) Iip , Nnp , (Z(i),i=1,Nord)
            ENDIF
         ELSE
            IF ( Ioptf/=0 ) THEN
               dsq = 0.D0
               CALL frmltx(Mcblt(1),dz(iv1),dz(iv2),dz(iv3))
               DO ij = 1 , Nord
                  dsq = dsq + dz(ix2+ij)**2
               ENDDO
               dsq = 1.D0/dsqrt(dsq)
               DO ij = 1 , Nord
                  dz(ij) = dsq*dz(ix2+ij)
               ENDDO
            ENDIF
            IF ( L16/=0 ) THEN
               CALL page2(2)
               WRITE (Io,99003) Iip , Nnp , (dz(i),i=1,Nord)
            ENDIF
         ENDIF
         CALL pack(dz(1),Sr7fle,Mcbvec(1))
 50   ENDDO
!
      CALL close(Iflrvc,Norew)
      IF ( L16/=0 ) THEN
         CALL page2(1)
         WRITE (Io,99001) Northo , Mcbvec
99001    FORMAT (5X,I5,16H ORTH VECTORS ON,I5,5H FILE,5I5,I14)
      ENDIF
   ENDIF
   k = Northo
   CALL close(Sr7fle,Norew)
   j = k
   Nonul = 0
   Iter = 0
   CALL gopen(Sr6fle,Z(ibuf4),Wrtrew)
   CALL close(Sr6fle,Norew)
   CALL gopen(srxfle,Z(ibuf2),Rdrew)
   CALL gopen(Sr5fle,Z(ibuf4),Wrtrew)
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
      IF ( Iprc==1 ) THEN
!
         DO i = 1 , Nord
            ss = -ss
            j = j + 1
            sq = float(mod(j,3)+1)/(3.0*float((mod(j,13)+1)*(1+5*i/Nord)))
            Z(ix2+i) = sq*ss
         ENDDO
         IF ( Optn2/=dashq ) CALL fnxtv(Z(iv1),Z(iv2),Z(iv3),Z(iv4),Z(iv5),Z(ibuf1),ifn)
         IF ( Optn2==dashq ) CALL fnxtvd(Z(iv1),Z(iv2),Z(iv3),Z(iv4),Z(iv5),Z(ibuf1),ifn)
      ELSE
         DO i = 1 , Nord
            ss = -ss
            j = j + 1
            dsq = float(mod(j,3)+1)/(3.0*float((mod(j,13)+1)*(1+5*i/Nord)))
            dz(ix2+i) = dsq*ss
         ENDDO
         IF ( Optn2/=dashq ) CALL fnxtvc(dz(iv1),dz(iv2),dz(iv3),dz(iv4),dz(iv5),Z(ibuf1),ifn)
      ENDIF
!
      IF ( Iter>Mord ) THEN
         Mord = Northo - Nzero
         Cndflg = 3
         EXIT SPAG_Loop_1_1
!
      ELSEIF ( ifn>=Mord ) THEN
         EXIT SPAG_Loop_1_1
      ENDIF
   ENDDO SPAG_Loop_1_1
   CALL close(Sr5fle,Norew)
   CALL close(srxfle,Rew)
   CALL close(ifl,Rew)
!
!     IF NEW FBS METHOD IS USED, SR9FLE AND SR10FL FILES COULD BE VERY
!     BIG. MAKE SURE THEY ARE PHYSICALLY REDUCED TO ZERO SIZE. THIS IS
!     IMPORTANT FOR A COMPUTER SYSTEM WITH LIMITED DISC SPACE
!
   IF ( ifl==sr10fl ) THEN
      CALL gopen(sr9fle,Z(ibuf2),Wrtrew)
      CALL gopen(sr10fl,Z(ibuf3),Wrtrew)
      CALL close(sr9fle,Rew)
      CALL close(sr10fl,Rew)
   ENDIF
!
   IF ( L16==0 ) RETURN
   CALL page2(1)
   i = ibuf4 - Northo*Nord*nwds - 2
   IF ( i<0 ) i = ibuf4 - iend
   WRITE (Io,99002) i , name
99002 FORMAT (19H OPEN CORE NOT USED,I10,2X,2A4)
99003 FORMAT (10H ORTH VCT ,2I5,/(1X,8E16.8))
!
END SUBROUTINE feer3x

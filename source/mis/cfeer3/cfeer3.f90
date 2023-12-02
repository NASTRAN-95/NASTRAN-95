!*==cfeer3.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE cfeer3
USE C_FEERAA
USE C_FEERXC
USE C_NAMES
USE C_SYSTEM
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER :: ibuf1 , ibuf2 , ibuf3 , iend , iopn , iprec , itop , iv1 , iv1l , iv2 , iv2l , iv3 , iv3l , iv4 , iv4l , iv5 , iv5l , &
            & jprec , nout , nz , sysbuf
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL cfer3d , cfer3s , close , gopen , korsz , makmcb , mesage
!
! End of declarations rewritten by SPAG
!
!
!     CFEER3 IS A DRIVER ROUTINE WHICH PERFORMS THE TRIDIAGONAL
!     REDUCTION FOR THE COMPLEX FEER METHOD
!
   !>>>>EQUIVALENCE (Dz(1),Z(1)) , (Ksystm(55),Iprec) , (Ksystm(1),Sysbuf) , (Ksystm(2),Nout)
   DATA name/4HCFEE , 4HR3  /
!
!     SCRATCH FILE AND BUFFER ALLOCATION
!
!     FILE  5  CONTAINS THE ELEMENTS OF REDUCED TRIDIAGONAL MATRIX
!     FILE  7  CONTAINS THE ORTHOGONAL VECTOR PAIRS (NUMBER OF
!              VECTOR PAIRS = NORTHO)
!
!     BUFFER Z(IBUF1) IS LOCAL SCRATCH BUFFER
!     BUFFER Z(IBUF2) IS LOCAL SCRATCH BUFFER
!     BUFFER Z(IBUF3) IS USED BY FILE 5
!
!
!     COMPUTE STORAGE ALLOCATIONS
!
   nz = korsz(Z)
   ibuf1 = nz - sysbuf
   ibuf2 = ibuf1 - sysbuf
   ibuf3 = ibuf2 - sysbuf
   itop = ibuf3
!
!     COMPUTE LOCATIONS OF RIGHT-HANDED VECTORS
!
   iv1 = 1
   iv2 = iv1 + Nord4
   iv3 = iv2 + Nord4
   iv4 = iv3 + Nord4
   iv5 = iv4 + Nord4
!
!     TEST FOR INSUFFICIENT CORE
!
   iend = iprec*(5*Nord4+1)
   IF ( iend>itop ) THEN
!
      iend = (iend-itop)/1000 + 1
      WRITE (nout,99001) iend
99001 FORMAT (5H0NEED,I4,17HK MORE CORE WORDS)
      CALL mesage(-8,0,name)
      RETURN
   ENDIF
!
!     COMPUTE LOCATIONS OF LEFT-HANDED VECTORS
!
   iv1l = iv1 + Nord2
   iv2l = iv2 + Nord2
   iv3l = iv3 + Nord2
   iv4l = iv4 + Nord2
   iv5l = iv5 + Nord2
   iopn = itop - iend
   IF ( Idiag/=0 ) WRITE (nout,99002) iopn
99002 FORMAT (1H ,I10,36H SINGLE PRECISION WORDS OF OPEN CORE,29H NOT USED (SUBROUTINE CFEER3))
   IF ( iopn<Minopn ) Minopn = iopn
!
!     INITIALIZE SCRATCH FILE TO CONTAIN TRIDIAGONAL ELEMENTS
!
   CALL gopen(Iscr(5),Z(ibuf3),Wrtrew)
!
!     GENERATE MATRIX CONTROL BLOCK FOR SCRATCH FILE TO CONTAIN
!     ORTHOGONAL VECTORS (LEFT VECTOR PACKED IMMEDIATELY AFTER
!     RIGHT, I. E., EACH COLUMN CONTAINS RIGHT VECTOR FOLLOWED BY
!     LEFT VECTOR)
!
   jprec = iprec + 2
   CALL makmcb(Mcbvec(1),Iscr(7),Nord2,2,jprec)
!
!     PERFORM DOUBLE PRECISION FEER
!
   IF ( iprec==2 ) CALL cfer3d(dz(iv1),dz(iv1l),dz(iv2),dz(iv2l),dz(iv3),dz(iv3l),dz(iv4),dz(iv4l),dz(iv5),dz(iv5l),Z(ibuf1),       &
                             & Z(ibuf2))
!
!     PERFORM SINGLE PRECISION FEER
!
   IF ( iprec/=2 ) CALL cfer3s(Z(iv1),Z(iv1l),Z(iv2),Z(iv2l),Z(iv3),Z(iv3l),Z(iv4),Z(iv4l),Z(iv5),Z(iv5l),Z(ibuf1),Z(ibuf2))
!
!     TERMINATE SCRATCH FILE CONTAINING TRIDIAGONAL ELEMENTS
!
   CALL close(Iscr(5),Norew)
   RETURN
END SUBROUTINE cfeer3

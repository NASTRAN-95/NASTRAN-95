
SUBROUTINE cfeer3
   IMPLICIT NONE
   INTEGER Cdp , Ib(7) , Idiag , Ik(7) , Ilam(7) , Im(7) , Iphi(7) , Iprec , Iscr(11) , Ksystm(65) , Mcbvec(7) , Minopn , Mreduc ,  &
         & Nord , Nord2 , Nord4 , Nordp1 , Norew , Northo , Nout , Sqr , Switch , Sysbuf
   REAL Csp , Dudxx , Dumaa(84) , Eofnrw , Epsdum(2) , Rd , Rdp , Rdrew , Rew , Rsp , Wrt , Wrtrew , Xcdum(12) , Z(1)
   DOUBLE PRECISION Dz(1) , Lambda(2)
   COMMON /feeraa/ Ik , Im , Ib , Ilam , Iphi , Dudxx , Iscr , Dumaa , Mcbvec
   COMMON /feerxc/ Lambda , Switch , Mreduc , Nord , Idiag , Epsdum , Northo , Nord2 , Nord4 , Nordp1 , Xcdum , Minopn
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp , Rdp , Csp , Cdp , Sqr
   COMMON /system/ Ksystm
   COMMON /zzzzzz/ Z
   INTEGER ibuf1 , ibuf2 , ibuf3 , iend , iopn , itop , iv1 , iv1l , iv2 , iv2l , iv3 , iv3l , iv4 , iv4l , iv5 , iv5l , jprec ,    &
         & name(2) , nz
   INTEGER korsz
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
   ibuf1 = nz - Sysbuf
   ibuf2 = ibuf1 - Sysbuf
   ibuf3 = ibuf2 - Sysbuf
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
   iend = Iprec*(5*Nord4+1)
   IF ( iend>itop ) THEN
!
      iend = (iend-itop)/1000 + 1
      WRITE (Nout,99001) iend
99001 FORMAT (5H0NEED,I4,17HK MORE CORE WORDS)
      CALL mesage(-8,0,name)
      GOTO 99999
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
   IF ( Idiag/=0 ) WRITE (Nout,99002) iopn
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
   jprec = Iprec + 2
   CALL makmcb(Mcbvec(1),Iscr(7),Nord2,2,jprec)
!
!     PERFORM DOUBLE PRECISION FEER
!
   IF ( Iprec==2 ) CALL cfer3d(Dz(iv1),Dz(iv1l),Dz(iv2),Dz(iv2l),Dz(iv3),Dz(iv3l),Dz(iv4),Dz(iv4l),Dz(iv5),Dz(iv5l),Z(ibuf1),       &
                             & Z(ibuf2))
!
!     PERFORM SINGLE PRECISION FEER
!
   IF ( Iprec/=2 ) CALL cfer3s(Z(iv1),Z(iv1l),Z(iv2),Z(iv2l),Z(iv3),Z(iv3l),Z(iv4),Z(iv4l),Z(iv5),Z(iv5l),Z(ibuf1),Z(ibuf2))
!
!     TERMINATE SCRATCH FILE CONTAINING TRIDIAGONAL ELEMENTS
!
   CALL close(Iscr(5),Norew)
   RETURN
99999 RETURN
END SUBROUTINE cfeer3
!*==feer3.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE feer3
   USE c_feercx
   USE c_feerim
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
   REAL(REAL64) :: dsq
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER :: i , ibuf1 , ibuf2 , ibuf3 , ibuf4 , iend , ifn , ij , irmem , itest , iv1 , iv2 , iv2m1 , iv3 , iv4 , iv5 , j , k ,   &
            & l43 , mavail , memlt , memort , memsma , memtot , minnee , ncols , nstrgs , nterms , nwds , nwdtrm , nz
   INTEGER , DIMENSION(7) :: mcbscl
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL :: sq , ss
   EXTERNAL close , dssize , feer3x , ferrdm , ferxtd , ferxts , frmlta , frmltx , gopen , korsz , makmcb , mesage , pack , rdtrl , &
          & sswtch , unpack
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
!  Comments to follow refer to updates made 11/94.
!  This is a new version of FEER3.  The old version has been renamed FEER3X.
!  Diag 43 may be used to force the use of the old version.  The new version
!  uses all of available open core for storage of the orthogonal vectors,
!  the lower triangular matrix from SDCOMP, and the SMA matrix.  If
!  insufficient memory is available, only part of the lower triangular
!
!     INTEGER            DASHQ
!
!  NIDSMA = IN-MEMORY INDEX FOR COLUMN DATA OF SMA MATRIX
!  NIDLT  = IN-MEMORY INDEX FOR LOWER TRIANGULAR MATRIX
!  NIDORV = IN-MEMORY INDEX FOR ORTHOGONAL VECTORS
!  NLTLI  = INDEX OF LAST STRING OF LOWER TRIANGULAR MATRIX HELD IN MEMORY
!  NSMALI = INDEX OF LAST STRING OF SMA MATRIX HELD IN MEMORY
!  IBFSMA = IN-MEMORY INDEX FOR BUFFER FOR OPENING SMA MATRIX
!  IBMLT  = IN-MEMORY INDEX FOR BUFFER FOR OPENING LOWER TRIANGULAR MATRIX
!  IBFORV = IN-MEMORY INDEX FOR BUFFER FOR ORTHOGONAL VECTORS
!  SMAPOS = POSITION OF RECORD FOLLOWING LAST RECORD READ INTO MEMORY
!           AND THE LAST RECORD OF MATRIX SMA (SEE SUBROUTINE DSCPOS)
!  LTPOS  = POSITION OF RECORD FOLLOWING LAST RECORD READ INTO MEMORY
!           AND THE LAST RECORD OF THE LOWER TRIANGULAR MATRIX
!
   !>>>>EQUIVALENCE (Iz(1),Z(1),Dz(1))
   DATA name/4HFEER , 4H3   /
!     DATA      DASHQ / 4H-Q    /
!
!     SR5FLE CONTAINS THE TRIDIAGONAL ELEMENTS
!     SR6FLE CONTAINS THE G VECTORS
!     SR7FLE CONTAINS THE ORTHOGONAL VECTORS
!     SR8FLE CONTAINS THE CONDITIONED MAA OR KAAD MATRIX
!     IFLVEC CONTAINS THE L OR C MATRIX FROM SDCOMP
!     IFLELM CONTAINS     KAA+ALPHA*MAA
!     IFLRVC CONTAINS THE RESTART AND/OR RIGID BODY VECTORS
!
   CALL sswtch(43,l43)
   IF ( l43==0 ) THEN
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
      ibforv = ibuf1
      ibflt = ibuf3
      ibfsma = ibuf2
      iv1 = 1
      iv2 = iv1 + nord
      iv2m1 = iv2 - 1
      iv3 = iv2 + nord
      iv4 = iv3 + nord
      iv5 = iv4 + nord
      iend = nwds*(5*nord+1) + 2
      mavail = iend - ibuf4
      IF ( mavail>0 ) CALL mesage(-8,mavail,name)
!
! COMPUTE THE MEMORY REQUIREMENT FOR ORTHOGONAL VECTORS
!
      memort = nord*(mord+northo)*iprc
!
! COMPUTE THE MEMORY REQUIREMENT FOR THE LOWER TRIANGULAR MATRIX
!
      CALL dssize(mcblt,ncols,nterms,nstrgs,nwdtrm)
      memlt = nterms*nwdtrm + nstrgs*4
!
! COMPUTE THE MEMORY REQUIREMENT FOR THE SMA MATRIX
!
      CALL dssize(mcbsma,ncols,nterms,nstrgs,nwdtrm)
      memsma = nterms*nwdtrm + nstrgs*4
      IF ( l16/=0 ) THEN
         minnee = iend + 4*sysbuf
         memtot = memort + memlt + memsma + minnee
         WRITE (nout,99001) minnee , memort , memsma , memlt , memtot , nz
99001    FORMAT (' FEER EIGENVALUE EXTRACTION NFORMATION',/,5X,' THE FOLLOWING GIVES OPEN CORE REQUIREMENTS FOR KEEPING',/,5X,      &
                &' VARIOUS MATRICES AND VECTORS IN CORE FOR THE FEER',/,5X,' EIGENVALUE EXTRACTION METHOD',/,10X,                   &
                &' MINIMUM NUMBER OF WORDS NEEDED IN OPEN CORE    =',I10,/,10X,' NUMBER OF WORDS FOR ORTHOGONAL VECTORS         =', &
               & I10,/,10X,' NUMBER OF WORDS FOR SMA MATRIX                 =',I10,/,10X,                                           &
                &' NUMBER OF WORDS FOR LOWER TRIANGULAR MATRIX    =',I10,/,10X,' TOTAL NUMBER OF WORDS NEEDED TO ELIMINATE I/O  =', &
               & I10,/,10X,' WORDS FOR OPEN CORE SPECIFIED IN THIS RUN      =',I10)
      ENDIF
! CHECK TO SEE IF MEMORY AVAILABLE FOR ORTHOGONAL VECTORS
      nidorv = 0
      itest = iend + memort
      IF ( itest<=ibuf4 ) THEN
         nidorv = iend
         nidorv = (nidorv/2)*2 + 1
         iend = iend + memort
      ENDIF
! CHECK TO SEE IF MEMORY AVAILABLE FOR SMA MATRIX
      irmem = ibuf4 - iend
      IF ( irmem<=10 ) THEN
         nidsma = 0
         memsma = 0
      ELSE
         nidsma = iend
         nidsma = (nidsma/2)*2 + 1
         memsma = memsma
         memsma = min0(memsma,irmem)
         iend = iend + memsma
      ENDIF
! CHECK TO SEE IF MEMORY AVAILABLE FOR LOWER TRIANGULAR MATRIX
      irmem = ibuf4 - iend
      IF ( irmem<=10 ) THEN
         nidlt = 0
         memlt = 0
      ELSE
         nidlt = iend
         nidlt = (nidlt/2)*2 + 1
         memlt = memlt
         memlt = min0(memlt,irmem)
         iend = iend + memlt
      ENDIF
      ltpos(4) = -1
      smapos(4) = -1
!      PRINT *,' FEER3, CALLING FERRDM,NIDSMA,NIDLT=',NIDSMA,NIDLT
      IF ( nidsma/=0 ) CALL ferrdm(mcbsma,nidsma,memsma,ibfsma,nsmali,smapos)
!      PRINT *,' RETURN FROM FERRDM,MEMSMA,NSMALI=',MEMSMA,NSMALI
!      PRINT *,' SMAPOS=',SMAPOS
      IF ( nidlt/=0 ) CALL ferrdm(mcblt,nidlt,memlt,ibflt,nltli,ltpos)
!      PRINT *,' RETURN FROM FERRDM,MEMLT,NLTLI=',MEMLT,NLTLI
!      PRINT *,' LTPOS=',LTPOS
      IF ( l16/=0 ) THEN
         WRITE (nout,99002) 'SMA' , smapos(1)
         WRITE (nout,99002) 'LT ' , ltpos(1)
      ENDIF
!      PRINT *,' SMAPOS=',SMAPOS
!      PRINT *,' LTPOS =',LTPOS
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
         DO j = 1 , northo
            ii = 1
            nn = nord
            CALL unpack(*20,iflrvc,dz(1))
            iip = ii
            nnp = nn
            IF ( iprc==1 ) THEN
               IF ( ioptf/=0 ) THEN
                  sq = 0.0
!      PRINT *,' FEER3 CALLING FRMLTA'
                  CALL frmlta(mcblt(1),z(iv1),z(iv2),z(iv3))
                  DO ij = 1 , nord
                     sq = sq + z(iv2m1+ij)**2
                  ENDDO
                  sq = 1.0/sqrt(sq)
                  DO ij = 1 , nord
                     z(ij) = sq*z(iv2m1+ij)
                  ENDDO
               ENDIF
            ELSEIF ( ioptf/=0 ) THEN
               dsq = 0.D0
!      PRINT *,' FERR3 CALLING FRMLTX'
               CALL frmltx(mcblt(1),dz(iv1),dz(iv2),dz(iv3))
               DO ij = 1 , nord
                  dsq = dsq + dz(iv2m1+ij)**2
               ENDDO
               dsq = 1.D0/dsqrt(dsq)
               DO ij = 1 , nord
                  dz(ij) = dsq*dz(iv2m1+ij)
               ENDDO
            ENDIF
            CALL pack(dz(1),sr7fle,mcbvec(1))
 20      ENDDO
         CALL close(iflrvc,norew)
      ENDIF
      k = northo
      CALL close(sr7fle,norew)
      j = k
      nonul = 0
      iter = 0
!      PRINT *,' FEER3,SR7FLE,IFLRVC,SR6FLE=',SR7FLE,IFLRVC,SR6FLE
!      PRINT *,' FEER3,SR6FLE,SR8FLE,SR5FLE=',SR6FLE,SR8FLE,SR5FLE
!      PRINT *,' FEER3,MCBSMA,MCBLT,MCBVEC=',MCBSMA(1),MCBLT(1),MCBSMA(1)
      CALL gopen(sr6fle,z(ibuf4),wrtrew)
      CALL close(sr6fle,norew)
      IF ( sr8fle==mcbsma(1) ) THEN
!      CALL GOPEN (SR8FLE,Z(IBUF2) ,RDREW )
         CALL gopen(sr5fle,z(ibuf4),wrtrew)
         CALL gopen(mcbsma,z(ibfsma),rdrew)
         CALL gopen(mcblt,z(ibflt),rdrew)
      ELSE
!      PRINT *,' PROBLEM IN FEER3, SR8FLE NE MCBSMA =',SR8FLE,MCBSMA(1)
         STOP
      ENDIF
   ELSE
      CALL feer3x
      RETURN
   ENDIF
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
         DO i = 1 , nord
            ss = -ss
            j = j + 1
            sq = float(mod(j,3)+1)/(3.0*float((mod(j,13)+1)*(1+5*i/nord)))
            z(iv2m1+i) = sq*ss
         ENDDO
!      IF (OPTN2 .EQ. DASHQ) GO TO 175
         CALL ferxts(z(iv1),z(iv2),z(iv3),z(iv4),z(iv5),z(ibuf1),ifn)
      ELSE
         DO i = 1 , nord
            ss = -ss
            j = j + 1
            dsq = float(mod(j,3)+1)/(3.0*float((mod(j,13)+1)*(1+5*i/nord)))
            dz(iv2m1+i) = dsq*ss
         ENDDO
!      PRINT *,' FEER3 CALLING FERXTD'
         CALL ferxtd(dz(iv1),dz(iv2),dz(iv3),dz(iv4),dz(iv5),z(ibuf1),ifn)
      ENDIF
!  175 CALL FERXTQ ( Z(IV1), Z(IV2)  , Z(IV3), Z(IV4 )
!     1,              Z(IV5), Z(IBUF1), IFN)
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
   CALL close(sr8fle,rew)
   CALL close(mcblt,rew)
99002 FORMAT (10X,' LAST COLUMN OF ',A3,' MATRIX IN MEMORY IS ',I4)
END SUBROUTINE feer3

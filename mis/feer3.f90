
SUBROUTINE feer3
   IMPLICIT NONE
   INTEGER Cndflg , Ibflt , Ibforv , Ibfsma , Ibk , Idiag , Ifkaa(7) , Iflelm(7) , Iflrva , Iflrvc , Iflvec(7) , Ifmaa(7) , Ifset , &
         & Ii , Iip , Incr , Incrp , Ind , Ioptf , Iprc , Iprec , Istart , Iter , Itp1 , Itp2 , Iwords(4) , Iz(1) , Ksys94 , L16 ,  &
         & Ltpos(7) , Mcblt(7) , Mcbrm(7) , Mcbsma(7) , Mcbvec(7) , Mord , Mrank , Neig , Nidlt , Nidorv , Nidsma , Nltli , Nn ,    &
         & Nnp , Nochng , Nonul , Nord , Norew , Northo , Nout , Nsmali , Nzero , Optn2 , Rdrew , Rew , Smapos(7) , Sr5fle ,        &
         & Sr6fle , Sr7fle , Sr8fle , Sysbuf , Wrtrew
   REAL Critf , Dmpfle , Eofnrw , Epx , Option , Rc(2) , Rd , Skip36(38) , Sr1fle , Sr2fle , Sr3fle , Sr4fle , Systm(52) , Timed ,  &
      & Wrt , Xlmbda , Z(1)
   DOUBLE PRECISION Dz(1) , Lambda , Lmbda
   COMMON /feercx/ Ifkaa , Ifmaa , Iflelm , Iflvec , Sr1fle , Sr2fle , Sr3fle , Sr4fle , Sr5fle , Sr6fle , Sr7fle , Sr8fle ,        &
                 & Dmpfle , Nord , Xlmbda , Neig , Mord , Ibk , Critf , Northo , Iflrva , Iflrvc
   COMMON /feerim/ Nidsma , Nidlt , Nidorv , Nltli , Nsmali , Ibfsma , Ibflt , Ibforv , Smapos , Ltpos
   COMMON /feerxx/ Lambda , Cndflg , Iter , Timed , L16 , Ioptf , Epx , Nochng , Ind , Lmbda , Ifset , Nzero , Nonul , Idiag ,      &
                 & Mrank , Istart
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw
   COMMON /opinv / Mcblt , Mcbsma , Mcbvec , Mcbrm
   COMMON /packx / Itp1 , Itp2 , Iip , Nnp , Incrp
   COMMON /reigkr/ Option , Optn2
   COMMON /system/ Sysbuf , Nout , Systm , Iprec , Skip36 , Ksys94
   COMMON /type  / Rc , Iwords
   COMMON /unpakx/ Iprc , Ii , Nn , Incr
   COMMON /zzzzzz/ Z
   DOUBLE PRECISION dsq
   INTEGER i , ibuf1 , ibuf2 , ibuf3 , ibuf4 , iend , ifn , ij , irmem , itest , iv1 , iv2 , iv2m1 , iv3 , iv4 , iv5 , j , k , l43 ,&
         & mavail , mcbscl(7) , memlt , memort , memsma , memtot , minnee , name(2) , ncols , nstrgs , nterms , nwds , nwdtrm , nz
   INTEGER korsz
   REAL sq , ss
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
      Ibforv = ibuf1
      Ibflt = ibuf3
      Ibfsma = ibuf2
      iv1 = 1
      iv2 = iv1 + Nord
      iv2m1 = iv2 - 1
      iv3 = iv2 + Nord
      iv4 = iv3 + Nord
      iv5 = iv4 + Nord
      iend = nwds*(5*Nord+1) + 2
      mavail = iend - ibuf4
      IF ( mavail>0 ) CALL mesage(-8,mavail,name)
!
! COMPUTE THE MEMORY REQUIREMENT FOR ORTHOGONAL VECTORS
!
      memort = Nord*(Mord+Northo)*Iprc
!
! COMPUTE THE MEMORY REQUIREMENT FOR THE LOWER TRIANGULAR MATRIX
!
      CALL dssize(Mcblt,ncols,nterms,nstrgs,nwdtrm)
      memlt = nterms*nwdtrm + nstrgs*4
!
! COMPUTE THE MEMORY REQUIREMENT FOR THE SMA MATRIX
!
      CALL dssize(Mcbsma,ncols,nterms,nstrgs,nwdtrm)
      memsma = nterms*nwdtrm + nstrgs*4
      IF ( L16/=0 ) THEN
         minnee = iend + 4*Sysbuf
         memtot = memort + memlt + memsma + minnee
         WRITE (Nout,99001) minnee , memort , memsma , memlt , memtot , nz
99001    FORMAT (' FEER EIGENVALUE EXTRACTION NFORMATION',/,5X,' THE FOLLOWING GIVES OPEN CORE REQUIREMENTS FOR KEEPING',/,5X,      &
                &' VARIOUS MATRICES AND VECTORS IN CORE FOR THE FEER',/,5X,' EIGENVALUE EXTRACTION METHOD',/,10X,                   &
                &' MINIMUM NUMBER OF WORDS NEEDED IN OPEN CORE    =',I10,/,10X,' NUMBER OF WORDS FOR ORTHOGONAL VECTORS         =', &
               & I10,/,10X,' NUMBER OF WORDS FOR SMA MATRIX                 =',I10,/,10X,                                           &
                &' NUMBER OF WORDS FOR LOWER TRIANGULAR MATRIX    =',I10,/,10X,' TOTAL NUMBER OF WORDS NEEDED TO ELIMINATE I/O  =', &
               & I10,/,10X,' WORDS FOR OPEN CORE SPECIFIED IN THIS RUN      =',I10)
      ENDIF
! CHECK TO SEE IF MEMORY AVAILABLE FOR ORTHOGONAL VECTORS
      Nidorv = 0
      itest = iend + memort
      IF ( itest<=ibuf4 ) THEN
         Nidorv = iend
         Nidorv = (Nidorv/2)*2 + 1
         iend = iend + memort
      ENDIF
! CHECK TO SEE IF MEMORY AVAILABLE FOR SMA MATRIX
      irmem = ibuf4 - iend
      IF ( irmem<=10 ) THEN
         Nidsma = 0
         memsma = 0
      ELSE
         Nidsma = iend
         Nidsma = (Nidsma/2)*2 + 1
         memsma = memsma
         memsma = min0(memsma,irmem)
         iend = iend + memsma
      ENDIF
! CHECK TO SEE IF MEMORY AVAILABLE FOR LOWER TRIANGULAR MATRIX
      irmem = ibuf4 - iend
      IF ( irmem<=10 ) THEN
         Nidlt = 0
         memlt = 0
      ELSE
         Nidlt = iend
         Nidlt = (Nidlt/2)*2 + 1
         memlt = memlt
         memlt = min0(memlt,irmem)
         iend = iend + memlt
      ENDIF
      Ltpos(4) = -1
      Smapos(4) = -1
!      PRINT *,' FEER3, CALLING FERRDM,NIDSMA,NIDLT=',NIDSMA,NIDLT
      IF ( Nidsma/=0 ) CALL ferrdm(Mcbsma,Nidsma,memsma,Ibfsma,Nsmali,Smapos)
!      PRINT *,' RETURN FROM FERRDM,MEMSMA,NSMALI=',MEMSMA,NSMALI
!      PRINT *,' SMAPOS=',SMAPOS
      IF ( Nidlt/=0 ) CALL ferrdm(Mcblt,Nidlt,memlt,Ibflt,Nltli,Ltpos)
!      PRINT *,' RETURN FROM FERRDM,MEMLT,NLTLI=',MEMLT,NLTLI
!      PRINT *,' LTPOS=',LTPOS
      IF ( L16/=0 ) THEN
         WRITE (Nout,99002) 'SMA' , Smapos(1)
         WRITE (Nout,99002) 'LT ' , Ltpos(1)
      ENDIF
!      PRINT *,' SMAPOS=',SMAPOS
!      PRINT *,' LTPOS =',LTPOS
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
         DO j = 1 , Northo
            Ii = 1
            Nn = Nord
            CALL unpack(*20,Iflrvc,Dz(1))
            Iip = Ii
            Nnp = Nn
            IF ( Iprc==1 ) THEN
               IF ( Ioptf/=0 ) THEN
                  sq = 0.0
!      PRINT *,' FEER3 CALLING FRMLTA'
                  CALL frmlta(Mcblt(1),Z(iv1),Z(iv2),Z(iv3))
                  DO ij = 1 , Nord
                     sq = sq + Z(iv2m1+ij)**2
                  ENDDO
                  sq = 1.0/sqrt(sq)
                  DO ij = 1 , Nord
                     Z(ij) = sq*Z(iv2m1+ij)
                  ENDDO
               ENDIF
            ELSEIF ( Ioptf/=0 ) THEN
               dsq = 0.D0
!      PRINT *,' FERR3 CALLING FRMLTX'
               CALL frmltx(Mcblt(1),Dz(iv1),Dz(iv2),Dz(iv3))
               DO ij = 1 , Nord
                  dsq = dsq + Dz(iv2m1+ij)**2
               ENDDO
               dsq = 1.D0/dsqrt(dsq)
               DO ij = 1 , Nord
                  Dz(ij) = dsq*Dz(iv2m1+ij)
               ENDDO
            ENDIF
            CALL pack(Dz(1),Sr7fle,Mcbvec(1))
 20      ENDDO
         CALL close(Iflrvc,Norew)
      ENDIF
      k = Northo
      CALL close(Sr7fle,Norew)
      j = k
      Nonul = 0
      Iter = 0
!      PRINT *,' FEER3,SR7FLE,IFLRVC,SR6FLE=',SR7FLE,IFLRVC,SR6FLE
!      PRINT *,' FEER3,SR6FLE,SR8FLE,SR5FLE=',SR6FLE,SR8FLE,SR5FLE
!      PRINT *,' FEER3,MCBSMA,MCBLT,MCBVEC=',MCBSMA(1),MCBLT(1),MCBSMA(1)
      CALL gopen(Sr6fle,Z(ibuf4),Wrtrew)
      CALL close(Sr6fle,Norew)
      IF ( Sr8fle==Mcbsma(1) ) THEN
!      CALL GOPEN (SR8FLE,Z(IBUF2) ,RDREW )
         CALL gopen(Sr5fle,Z(ibuf4),Wrtrew)
         CALL gopen(Mcbsma,Z(Ibfsma),Rdrew)
         CALL gopen(Mcblt,Z(Ibflt),Rdrew)
      ELSE
!      PRINT *,' PROBLEM IN FEER3, SR8FLE NE MCBSMA =',SR8FLE,MCBSMA(1)
         STOP
      ENDIF
   ELSE
      CALL feer3x
      GOTO 99999
   ENDIF
   DO
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
         DO i = 1 , Nord
            ss = -ss
            j = j + 1
            sq = float(mod(j,3)+1)/(3.0*float((mod(j,13)+1)*(1+5*i/Nord)))
            Z(iv2m1+i) = sq*ss
         ENDDO
!      IF (OPTN2 .EQ. DASHQ) GO TO 175
         CALL ferxts(Z(iv1),Z(iv2),Z(iv3),Z(iv4),Z(iv5),Z(ibuf1),ifn)
      ELSE
         DO i = 1 , Nord
            ss = -ss
            j = j + 1
            dsq = float(mod(j,3)+1)/(3.0*float((mod(j,13)+1)*(1+5*i/Nord)))
            Dz(iv2m1+i) = dsq*ss
         ENDDO
!      PRINT *,' FEER3 CALLING FERXTD'
         CALL ferxtd(Dz(iv1),Dz(iv2),Dz(iv3),Dz(iv4),Dz(iv5),Z(ibuf1),ifn)
      ENDIF
!  175 CALL FERXTQ ( Z(IV1), Z(IV2)  , Z(IV3), Z(IV4 )
!     1,              Z(IV5), Z(IBUF1), IFN)
      IF ( Iter>Mord ) THEN
         Mord = Northo - Nzero
         Cndflg = 3
         EXIT
!
      ELSEIF ( ifn>=Mord ) THEN
         EXIT
      ENDIF
   ENDDO
   CALL close(Sr5fle,Norew)
   CALL close(Sr8fle,Rew)
   CALL close(Mcblt,Rew)
99002 FORMAT (10X,' LAST COLUMN OF ',A3,' MATRIX IN MEMORY IS ',I4)
99999 RETURN
END SUBROUTINE feer3
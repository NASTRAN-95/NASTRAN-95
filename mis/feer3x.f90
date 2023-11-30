
SUBROUTINE feer3x
   IMPLICIT NONE
   INTEGER Cndflg , Ibk , Idiag , Ifkaa(7) , Iflelm(7) , Iflrva , Iflrvc , Iflvec(7) , Ifmaa(7) , Ifset , Ii , Iip , Incr , Incrp , &
         & Ind , Io , Ioptf , Iprc , Iprec , Istart , Iter , Itp1 , Itp2 , Iwords(4) , Iz(1) , Ksys94 , L16 , Mcblt(7) , Mcbrm(7) , &
         & Mcbsma(7) , Mcbvec(7) , Mord , Mrank , Neig , Nn , Nnp , Nochng , Nonul , Nord , Norew , Northo , Nzero , Nzv5 , Optn2 , &
         & Sr5fle , Sr6fle , Sr7fle , Sr8fle , Sysbuf
   REAL Critf , Dmpfle , Eofnrw , Epx , Option , Rc(2) , Rd , Rdrew , Rew , Skip36(38) , Sr1fle , Sr2fle , Sr3fle , Sr4fle ,        &
      & Systm(52) , Timed , Wrt , Wrtrew , Xlmbda , Z(1)
   DOUBLE PRECISION Dz(1) , Lambda , Lmbda
   COMMON /feercx/ Ifkaa , Ifmaa , Iflelm , Iflvec , Sr1fle , Sr2fle , Sr3fle , Sr4fle , Sr5fle , Sr6fle , Sr7fle , Sr8fle ,        &
                 & Dmpfle , Nord , Xlmbda , Neig , Mord , Ibk , Critf , Northo , Iflrva , Iflrvc
   COMMON /feerxx/ Lambda , Cndflg , Iter , Timed , L16 , Ioptf , Epx , Nochng , Ind , Lmbda , Ifset , Nzero , Nonul , Idiag ,      &
                 & Mrank , Istart , Nzv5
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw
   COMMON /opinv / Mcblt , Mcbsma , Mcbvec , Mcbrm
   COMMON /packx / Itp1 , Itp2 , Iip , Nnp , Incrp
   COMMON /reigkr/ Option , Optn2
   COMMON /system/ Sysbuf , Io , Systm , Iprec , Skip36 , Ksys94
   COMMON /type  / Rc , Iwords
   COMMON /unpakx/ Iprc , Ii , Nn , Incr
   COMMON /zzzzzz/ Z
   INTEGER dashq , i , ibuf1 , ibuf2 , ibuf3 , ibuf4 , icrq , iend , ifl , ifn , ij , iv1 , iv2 , iv3 , iv4 , iv5 , ix2 , j , k ,   &
         & mcbscl(7) , name(2) , nwds , nz , sr10fl , sr9fle , srxfle
   DOUBLE PRECISION dsq
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
         CALL unpack(*50,Iflrvc,Dz(1))
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
               CALL frmltx(Mcblt(1),Dz(iv1),Dz(iv2),Dz(iv3))
               DO ij = 1 , Nord
                  dsq = dsq + Dz(ix2+ij)**2
               ENDDO
               dsq = 1.D0/dsqrt(dsq)
               DO ij = 1 , Nord
                  Dz(ij) = dsq*Dz(ix2+ij)
               ENDDO
            ENDIF
            IF ( L16/=0 ) THEN
               CALL page2(2)
               WRITE (Io,99003) Iip , Nnp , (Dz(i),i=1,Nord)
            ENDIF
         ENDIF
         CALL pack(Dz(1),Sr7fle,Mcbvec(1))
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
            Dz(ix2+i) = dsq*ss
         ENDDO
         IF ( Optn2/=dashq ) CALL fnxtvc(Dz(iv1),Dz(iv2),Dz(iv3),Dz(iv4),Dz(iv5),Z(ibuf1),ifn)
      ENDIF
!
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

SUBROUTINE feer4(It)
   IMPLICIT NONE
   INTEGER Cndflg , Eofnrw , Ibk , Idiag , Ifkaa(7) , Iflelm(7) , Iflrva , Iflrvc , Iflvec(7) , Ifmaa(7) , Ifset , Ii , Iip , Incr ,&
         & Incrp , Ind , Io , Ioptf , Iprc , Iprec , Istart , Iter , Itp1 , Itp2 , Iz(1) , L16 , Mach , Mcblt(7) , Mcbrm(7) ,       &
         & Mcbsma(7) , Mcbvec(7) , Mord , Mrank , Neig , Nn , Nnp , Nonul , Nord , Norew , Northo , Nzero , Rdrew , Rew , Sr2fle ,  &
         & Sr4fle , Sr5fle , Sr6fle , Sr7fle , Sr8fle , Sysbuf , Wrt , Wrtrew
   REAL Critf , Dmpfle , Epx , Errc , Rd , Sr1fle , Sr3fle , Systm(52) , Timed , Xlmbda , Z(1)
   DOUBLE PRECISION Dz(1) , Lambda , Lmbda
   COMMON /feercx/ Ifkaa , Ifmaa , Iflelm , Iflvec , Sr1fle , Sr2fle , Sr3fle , Sr4fle , Sr5fle , Sr6fle , Sr7fle , Sr8fle ,        &
                 & Dmpfle , Nord , Xlmbda , Neig , Mord , Ibk , Critf , Northo , Iflrva , Iflrvc
   COMMON /feerxx/ Lambda , Cndflg , Iter , Timed , L16 , Ioptf , Epx , Errc , Ind , Lmbda , Ifset , Nzero , Nonul , Idiag , Mrank ,&
                 & Istart
   COMMON /machin/ Mach
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw
   COMMON /opinv / Mcblt , Mcbsma , Mcbvec , Mcbrm
   COMMON /packx / Itp1 , Itp2 , Iip , Nnp , Incrp
   COMMON /system/ Sysbuf , Io , Systm , Iprec
   COMMON /unpakx/ Iprc , Ii , Nn , Incr
   COMMON /zzzzzz/ Z
   INTEGER It
   DOUBLE PRECISION b(2) , dsce , dsm
   REAL erf , sb(2) , sce , sm
   INTEGER i , ibuf1 , ibuf2 , ibuf3 , icavl , icr(2) , icreq , iend , ier , iopn , isrv , iv1 , iv2 , iv3 , iv4 , iv5 , iv6 , iv7 ,&
         & iv8 , iv9 , ivr , ivw , ix1 , ix2 , ix3 , ix4 , ix5 , j , l , l26 , l43 , m , mcbc(7) , mdim , mflg , mred , name(2) ,   &
         & nw , nz
   LOGICAL incore
   INTEGER korsz
!
!     FEER4 OBTAINS FROM THE REDUCED TRIDIAGONAL MATRIX THE EIGENVALUES
!     AND EIGENVECTORS
!
!WKBNB NCL93007 11/94
!WKBNE NCL93007 11/94
   EQUIVALENCE (Iz(1),Z(1),Dz(1)) , (sb(1),b(1)) , (dsce,sce)
   DATA name/4HFEER , 4H4   / , icr/4HPASS , 4HFAIL/
!
!     SR4FLE CONTAINS THE EIGENVECTORS OF THE REDUCED PROBLEM
!     SR5FLE CONTAINS THE TRIDIAGONAL ELEMENTS AND SCRATCH IN FQRWV
!     SR6FLE CONTAINS THE G VECTORS
!     SR7FLE CONTAINS THE ORTHOGONAL VECTORS
!
   CALL sswtch(26,l26)
   mdim = Mord + 1
   dsm = 10.0D+0**(-2*It/3)
   sm = dsm
   Iprc = Mcbrm(5)
   nz = korsz(Z)
   CALL makmcb(mcbc(1),Sr4fle,mdim,2,Iprc)
   mcbc(2) = 0
   mcbc(6) = 0
   m = 0
!
!     INITIALIZE ALLOCATIONS
!
   ibuf1 = nz - Sysbuf
   ibuf2 = ibuf1 - Sysbuf
   ibuf3 = ibuf2 - Sysbuf
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
   iend = Iprc*(8*mdim+1) + mdim
   IF ( iend>ibuf3 ) CALL mesage(-8,iend-ibuf3,name)
   CALL gopen(Sr5fle,Z(ibuf2),Rdrew)
   IF ( Iprc==2 ) Dz(iv4+Mord) = Errc
   IF ( Iprc==1 ) Z(iv4+Mord) = Errc
   nw = Iprc*2
   DO i = 1 , Mord
      CALL read(*200,*300,Sr5fle,b(1),nw,1,m)
      IF ( Iprc==1 ) THEN
         Z(ix3+i) = sb(1)
         Z(ix4+i) = sb(2)
      ELSE
         Dz(ix3+i) = b(1)
         Dz(ix4+i) = b(2)
      ENDIF
   ENDDO
   CALL close(Sr5fle,Rew)
   CALL gopen(Sr4fle,Z(ibuf2),Wrtrew)
   IF ( Iprc==1 ) THEN
      CALL fqrw(Mord,Z(iv1),Z(iv2),Z(iv3),Z(iv4),Z(iv5),Z(iv6),Z(iv7),Z(iv8),Z(iv9),Z(ibuf1),Sr5fle,mcbc(1))
   ELSE
!                                                              SR4FLE
      CALL fqrwv(Mord,Dz(iv1),Dz(iv2),Dz(iv3),Dz(iv4),Dz(iv5),Dz(iv6),Dz(iv7),Dz(iv8),Dz(iv9),Z(ibuf1),Sr5fle,mcbc(1))
   ENDIF
!                                                          SR4FLE
   CALL close(Sr4fle,Norew)
!
!     RECONFIGURE VECTOR INDEX TO OBTAIN PHYSICAL EIGENVECTORS
!
   ix1 = iv1 - 1
   ix2 = iv2 - 1
   ix3 = iv3 - 1
   ix4 = iv4 - 1
   ix5 = ix4 + Nord
   isrv = Mcbrm(1)
   Iflvec(1) = Iflrvc
   Iflelm(1) = Iflrva
   IF ( Nzero==0 ) THEN
!
!     PREPARE FILES WHEN NO RESTART AND/OR RIGID BODY VECTORS
!
      Iflvec(2) = 0
      Iflvec(6) = 0
      CALL gopen(Iflrvc,Z(ibuf3),Wrtrew)
      CALL close(Iflrvc,Norew)
      CALL gopen(Iflrva,Z(ibuf3),Wrtrew)
      CALL close(Iflrva,Norew)
   ENDIF
   Itp1 = Iprc
   Itp2 = 1
   Incrp = 1
   Ii = 1
   CALL gopen(Iflrva,Z(ibuf1),Wrt)
   mred = 0
   mflg = 1
   DO m = 1 , Mord
      IF ( Iprc==1 ) THEN
         sce = 1.0/Z(ix1+m) - Lambda
         IF ( L16/=0 ) THEN
            erf = 0.0D+0
            IF ( abs(sce)>sm ) erf = 100.0D+0*Z(ix2+m)/dabs(1.0D+0-Z(ix1+m)*Lambda)
            Z(ix2+m) = sce
            IF ( erf>Critf ) mflg = 2
         ENDIF
      ELSE
         dsce = 1.0D+0/Dz(ix1+m) - Lambda
         IF ( L16/=0 ) THEN
            erf = 0.0D+0
            IF ( dabs(dsce)>dsm ) erf = 100.D0*Dz(ix2+m)/dabs(1.D0-Dz(ix1+m)*Lambda)
            Dz(ix2+m) = dsce
            IF ( erf>Critf ) mflg = 2
         ENDIF
      ENDIF
      IF ( mflg/=2 ) THEN
         mred = mred + 1
         CALL write(Iflrva,dsce,Iprec,1)
      ENDIF
      IF ( L16/=0 ) THEN
         CALL page2(1)
         IF ( Iprc==2 ) WRITE (Io,99002) m , dsce , erf , icr(mflg)
         IF ( Iprc==1 ) WRITE (Io,99002) m , sce , erf , icr(mflg)
      ENDIF
   ENDDO
   CALL close(Iflrva,Eofnrw)
   IF ( Mord==0 ) RETURN
!
   CALL gopen(isrv,Z(ibuf1),Rdrew)
   CALL gopen(Sr4fle,Z(ibuf2),Rdrew)
   CALL gopen(Iflrvc,Z(ibuf3),Wrt)
!WKBNB NCL93007 11/94
   incore = .FALSE.
   CALL sswtch(43,l43)
   IF ( l43==0 ) THEN
      ivw = ix5 + Nord + 1
      icreq = Nord*Mord*Iprc
      icavl = ibuf3 - ivw - 1
      IF ( icavl>icreq ) incore = .TRUE.
      IF ( incore ) THEN
         Nn = Nord
         DO i = 1 , Mord
            ivr = ivw + (i-1)*Nord
            IF ( Iprc==1 ) CALL unpack(*20,isrv,Z(ivr+1))
            IF ( Iprc==2 ) CALL unpack(*20,isrv,Dz(ivr+1))
 20      ENDDO
      ENDIF
   ENDIF
!WKBNE NCL93007 11/94
!
!     IF DIAG 26 IS OFF, LIMIT EIGENSOLUTIONS TO NUMBER REQUESTED
!
   IF ( mred>=Neig .AND. l26/=0 ) mred = Neig
   IF ( Iprc==1 ) THEN
      DO m = 1 , mred
         DO l = 1 , Nord
            Z(ix5+l) = 0.0
         ENDDO
         Nn = Nord
         CALL unpack(*40,Sr4fle,Z(iv3))
         Nn = Nord
!WKBI NCL93007 11/94
         IF ( incore ) THEN
            DO i = 1 , Mord
               ivr = ivw + (i-1)*Nord
               DO j = 1 , Nord
                  Z(ix5+j) = Z(ix5+j) + Z(ivr+j)*Z(ix3+i)
               ENDDO
            ENDDO
         ELSE
            DO i = 1 , Mord
               CALL unpack(*50,isrv,Z(iv4))
               DO j = 1 , Nord
                  Z(ix5+j) = Z(ix5+j) + Z(ix4+j)*Z(ix3+i)
               ENDDO
!WKBNB NCL93007 11/94
            ENDDO
         ENDIF
!WKBNE NCL93007 11/94
 40      IF ( Ioptf/=0 ) THEN
            sce = 1.0/sqrt(abs(Z(ix1+m)))
            DO l = 1 , Nord
               Z(ix5+l) = sce*Z(ix5+l)
            ENDDO
         ENDIF
         Iip = 1
         Nnp = Nord
         CALL pack(Z(ix5+1),Iflrvc,Iflvec(1))
!WKBI NCL93007 11/94
         IF ( .NOT.(incore) ) THEN
            CALL rewind(Mcbrm)
            CALL skprec(Mcbrm,1)
         ENDIF
 50   ENDDO
   ELSE
      DO m = 1 , mred
         DO l = 1 , Nord
            Dz(ix5+l) = 0.0D+0
         ENDDO
         Nn = Mord
         CALL unpack(*60,Sr4fle,Dz(iv3))
         Nn = Nord
!WKBI NCL93007 11/94
         IF ( incore ) THEN
            DO i = 1 , Mord
               ivr = ivw + (i-1)*Nord
               DO j = 1 , Nord
                  Dz(ix5+j) = Dz(ix5+j) + Dz(ivr+j)*Dz(ix3+i)
               ENDDO
            ENDDO
         ELSE
            DO i = 1 , Mord
               CALL unpack(*100,isrv,Dz(iv4))
               DO j = 1 , Nord
                  Dz(ix5+j) = Dz(ix5+j) + Dz(ix4+j)*Dz(ix3+i)
               ENDDO
!WKBNB NCL93007 11/94
            ENDDO
         ENDIF
!WKBNE NCL93007 11/94
 60      IF ( Ioptf/=0 ) THEN
            dsce = 1.0D+0/dsqrt(dabs(Dz(ix1+m)))
            DO l = 1 , Nord
               Dz(ix5+l) = dsce*Dz(ix5+l)
            ENDDO
         ENDIF
         Iip = 1
         Nnp = Nord
         CALL pack(Dz(ix5+1),Iflrvc,Iflvec(1))
!WKBI NCL93007 11/94
         IF ( .NOT.(incore) ) THEN
            CALL rewind(Mcbrm)
            CALL skprec(Mcbrm,1)
         ENDIF
 100  ENDDO
   ENDIF
!
   CALL close(Iflrvc,Eofnrw)
   CALL close(isrv,Rew)
   CALL close(Sr4fle,Rew)
   Mord = mred
   GOTO 500
 200  ier = 2
   GOTO 400
 300  ier = 3
 400  Cndflg = 4
   CALL mesage(ier,Sr5fle,name)
 500  iopn = ibuf3 - iend
   IF ( L16==1 ) WRITE (Io,99001) iopn , name
99001 FORMAT ('  OPEN CORE NOT USED',I10,2X,2A4)
99002 FORMAT (10X,'PHYSICAL EIGENVALUE',I5,1P,E16.8,'  THEOR ERROR ',E16.8,'  PERCENT',5X,A4)
END SUBROUTINE feer4

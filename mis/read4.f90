
SUBROUTINE read4(Lama,Phi,Scr1,Eps,Mass)
   IMPLICIT NONE
   DOUBLE PRECISION Dz(1)
   REAL Eofnrw , Rd , Rew , Wrt , Z(1)
   INTEGER Incr , Incrx , Iout , Ipak , Isys , Itypa , Itypb , Itype , Iunpak , Jpak , Junpak , Ksystm(65) , Norew , Rdrew , Rsp ,  &
         & Wrtrew
   CHARACTER*23 Ufm
   CHARACTER*25 Uwm
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Rew , Norew , Eofnrw , Rsp
   COMMON /packx / Itypa , Itypb , Ipak , Jpak , Incrx
   COMMON /system/ Ksystm
   COMMON /unpakx/ Itype , Iunpak , Junpak , Incr
   COMMON /xmssg / Ufm , Uwm
   COMMON /zzzzzz/ Z
   REAL Eps
   INTEGER Lama , Mass , Scr1
   INTEGER Phi(7)
   REAL eps1 , eps2 , epsi , rmult
   INTEGER i , ibuf , ibuf1 , ibuf2 , iclos , idid , ifile , ii , ij , ipos , ipr , j , kore , n , name(2) , ncol , no , nrow ,     &
         & num , nz , phi1(7)
   INTEGER korsz
!
!     READ4 WILL TEST FOR CLOSE AND EQUAL ROOTS AND MAKE SURE THE
!     CORRESPONDING VECTORS ARE ORTHOGONAL
!
!WKBI ALPHA-OSF 9/94
   EQUIVALENCE (Ksystm(1),Isys) , (Ksystm(2),Iout) , (Dz(1),Z(1))
   DATA name/4HREAD , 4H4   /
!
   ncol = Phi(2)
   nrow = Phi(3)
   nz = korsz(Z)
   ibuf = nz - Isys
   ibuf1 = ibuf - Isys
   ibuf2 = ibuf1 - Isys
   iclos = 0
   idid = 0
   ipr = Phi(5)
   rmult = .01
   Itype = Rsp
   Iunpak = 1
   Junpak = nrow
   Incr = 1
   Itypa = Rsp
   Itypb = Rsp
   Ipak = 1
   Jpak = nrow
   Incrx = 1
   epsi = Eps
   IF ( Eps<=0. ) epsi = .0001
   nz = nz - Isys - Isys - 1 - Isys
   CALL makmcb(phi1,Scr1,nrow,2,Rsp)
   ifile = Lama
   CALL gopen(Lama,Z(ibuf),0)
   CALL read(*600,*100,Lama,Z(1),nz,1,n)
   GOTO 700
 100  CALL close(Lama,Rew)
!
!     REJECT ALL BUT VALUES FOR WHICH VECTORS EXIST
!
   n = Phi(2)
   nz = nz - n
   IF ( nz<nrow ) GOTO 700
   ifile = Phi(1)
   CALL gopen(Phi,Z(ibuf),0)
   ipos = 1
   i = 1
   eps1 = rmult
 200  IF ( abs(Z(i))+abs(Z(i+1))>=eps1 ) THEN
      IF ( Z(i+1)==0.0 ) GOTO 500
      IF ( abs(1.0-Z(i)/Z(i+1))>eps1 ) GOTO 400
   ENDIF
   IF ( iclos==0 ) iclos = i
   GOTO 500
 300  num = i - iclos + 1
   eps1 = rmult
!
!     NUM   = NUMBER OF CLOSE ROOTS IN THIS GROUP
!     ICLOS = THE INDEX OF THE FIRST CLOSE ROOT
!
   IF ( idid/=1 ) THEN
      idid = 1
      ifile = Scr1
      CALL gopen(Scr1,Z(ibuf1),Wrtrew)
   ENDIF
   ii = n + 1
   DO WHILE ( ipos/=iclos )
      ifile = Phi(1)
      CALL unpack(*800,Phi,Z(ii))
      CALL pack(Z(ii),Scr1,phi1)
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
      WRITE (Iout,99001) Uwm , num , i , eps1 , eps2
99001 FORMAT (A25,' 3142, INSUFFICIENT CORE STORAGE FOR EIGENVECTORS ','ASSOCIATED WITH',I4,' MULTIPLE EIGENVALUES STARTING WITH',  &
            & /28X,'MODE NUMBER',I4,' USING CURRENT MULTIPLE ROOT ','CRITERIA. CRITERIA REDUCED FROM ',1P,E12.5,' TO ',E12.5)
      eps1 = eps2
      i = iclos
      GOTO 200
   ELSE
      DO j = 1 , num
         CALL unpack(*800,Phi,Z(ii))
         ipos = ipos + 1
         ii = ii + nrow
         IF ( ii+nrow>=nz ) GOTO 700
      ENDDO
      ij = ii + n + n + 3
      ii = ii/2 + 1
      CALL ortck(Z(n+1),Mass,Z(ibuf2),num,nrow,Z(ij),Dz(ii),epsi)
      ii = n + 1
      DO j = 1 , num
         CALL pack(Z(ii),Scr1,phi1)
         ii = ii + nrow
      ENDDO
      iclos = 0
   ENDIF
 400  IF ( iclos/=0 ) GOTO 300
 500  i = i + 1
   IF ( i<n ) GOTO 200
   IF ( iclos/=0 ) GOTO 300
   IF ( idid/=0 ) THEN
      IF ( ipos<=ncol ) THEN
         DO i = ipos , ncol
            CALL unpack(*800,Phi,Z)
            CALL pack(Z(1),Scr1,phi1)
         ENDDO
      ENDIF
      CALL wrttrl(phi1)
!
!     COPY VECTORS FROM SCR1 TO PHI
!
      CALL close(Phi,Rew)
      CALL close(Scr1,Rew)
      CALL gopen(Phi,Z(ibuf),1)
      CALL gopen(Scr1,Z(ibuf1),Rdrew)
      CALL makmcb(Phi,Phi,nrow,2,ipr)
      Itypb = ipr
      DO i = 1 , n
         CALL unpack(*800,Scr1,Z)
         CALL pack(Z,Phi,Phi)
      ENDDO
      CALL wrttrl(Phi)
      CALL close(Scr1,Rew)
   ENDIF
   CALL close(Phi,Rew)
   RETURN
 600  no = -2
   GOTO 900
 700  no = -8
   GOTO 900
 800  no = -7
 900  CALL mesage(no,ifile,name)
END SUBROUTINE read4

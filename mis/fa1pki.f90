
SUBROUTINE fa1pki(Fsave,Qhhl)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   DOUBLE PRECISION Dz(1)
   INTEGER Floop , Ia , Icp , Iflag , Ik , Imvr , Incr1 , Inn , Iout , Iq , Ncore , Nk , Nnn , Nout , Sysbuf
   CHARACTER*23 Ufm
   REAL Z(1)
   COMMON /blank / Floop
   COMMON /fa1pkc/ Ncore , Nk , Imvr , Ik , Ia , Iq , Icp , Iflag
   COMMON /system/ Sysbuf , Nout
   COMMON /unpakx/ Iout , Inn , Nnn , Incr1
   COMMON /xmssg / Ufm
   COMMON /zzzzzz/ Z
!
! Dummy argument declarations
!
   INTEGER Fsave , Qhhl
!
! Local variable declarations
!
   INTEGER buf1 , i , iad , icpd , ifle , in , ising , j , k , l , m , n , n2 , name(2) , ni , nk1 , nn , nrow , nwr , trl(7)
   DOUBLE PRECISION det , dx1 , dx2
   INTEGER korsz
   REAL newm , oldm , rmi , rms , rmx
!
! End of declarations
!
!
!     FA1PKI BUILDS AN INTERPOLATION MATRIX IN CORE FOR PK METHOD
!
!     LAST REVISED  2/91, BY J.PETKAS/LOOKHEED
!     TO ALLOW CALCULATION OF INTERPOLATION MATRIX IN D.P.
!
   EQUIVALENCE (Dz(1),Z(1))
   DATA oldm/ - 1.0/
   DATA name/4HFA1P , 4HKI  /
!
   Iflag = 0
   IF ( oldm/=-1.0 ) GOTO 200
   Ncore = korsz(Z)
   buf1 = Ncore - Sysbuf
   Imvr = 1
!
!     PUT M V   IN CORE ON SECOND LOOP RETURN IF SAME MACH
!
   ifle = Fsave
   CALL gopen(Fsave,Z(buf1),0)
   CALL read(*400,*100,Fsave,Z(Imvr),buf1,1,nwr)
 100  Ik = Imvr + nwr
   CALL close(Fsave,1)
 200  i = (Floop-1)*3 + Imvr
   newm = Z(i)
   IF ( oldm==newm ) GOTO 99999
   oldm = newm
   Iflag = 1
!
!     PUT LIST OF M K'S IN CORE FOR THIS MACH
!
   trl(1) = Qhhl
   CALL rdtrl(trl)
   nrow = trl(3)
   ni = (trl(2)/trl(3))*2
   Iout = 3
   Inn = 1
   Incr1 = 1
   Nnn = nrow
   n2 = nrow*2
   nn = nrow*nrow
   ifle = Qhhl
   CALL open(*400,Qhhl,Z(buf1),0)
   CALL read(*400,*400,Qhhl,Z,-3,0,nwr)
   CALL read(*400,*400,Qhhl,n,1,0,nwr)
   n = n + n
   ni = min0(ni,n)
   CALL read(*400,*400,Qhhl,Z(Ik),ni,1,nwr)
!
!     FIND M'S CLOSEST TO NEWM
!
   Ia = Ik + ni
   IF ( mod(Ia,2)==0 ) Ia = Ia + 1
   rmi = 1.E20
   rms = 0.0
   DO i = 1 , ni , 2
      rmx = abs(Z(Ik+i-1)-newm)
      rmi = amin1(rmi,rmx)
      IF ( rmx<=rmi ) rms = Z(Ik+i-1)
   ENDDO
   rmi = rms
!
!     COUNT K"S
!
   Nk = 0
   DO i = 1 , ni , 2
      IF ( Z(Ik+i-1)==rmi ) Nk = Nk + 1
   ENDDO
!
!     ALLOCATE CORE FOR A-1 AND Q.  THEN BUILD THEM.
!
   i = 2*(Nk+1)**2
   Iq = Ia + i
   Icp = Iq + nn*2*Nk
   IF ( mod(Icp,2)==0 ) Icp = Icp + 1
   IF ( Icp+Sysbuf+n2>Ncore ) CALL mesage(-8,0,name)
!
!     BUILD A
!
   j = 0
   DO i = 1 , ni , 2
      IF ( Z(Ik+i-1)==rmi ) THEN
         Z(Iq+j) = Z(Ik+i)
         j = j + 1
      ENDIF
   ENDDO
   nk1 = Nk + 1
   n = 0
   m = Iq - 1
   iad = Ia/2 + 1
   icpd = Icp/2 + 1
   DO i = 1 , nk1
      dx2 = Z(m+i)
      DO j = 1 , nk1
         IF ( i==nk1 .AND. j==nk1 ) GOTO 300
         IF ( j==nk1 .OR. i==nk1 ) THEN
            Dz(iad+n) = 1.0D+0
         ELSE
            dx1 = Z(m+j)
            Dz(iad+n) = dabs((dx1-dx2)**3) + (dx1+dx2)**3
         ENDIF
         n = n + 1
      ENDDO
   ENDDO
 300  Dz(iad+n) = 0.0D+0
!
!     MODIFICATION FOR LEVEL 17.7 UPDATE
!     REPLACE ALL CALLS TO INVAER WITH CALLS TO INVERS.
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
   ising = -1
   CALL inverd(nk1,Dz(iad),nk1,0,0,det,ising,Dz(icpd))
   IF ( ising==2 ) THEN
!
      WRITE (Nout,99001) Ufm , name
99001 FORMAT (A23,' 2427, SINGULAR MATRIX FOR INTERPOLATION IN ',2A4)
      CALL mesage(-61,0,0)
   ELSE
!
!     BUILD Q
!
      n = 0
      in = nn
      l = 0
      DO i = 1 , ni , 2
         IF ( Z(Ik+i-1)==rmi ) THEN
            DO j = 1 , nrow
               CALL unpack(*305,Qhhl,Z(Icp))
               GOTO 310
 305           CALL zeroc(Z(Icp),nrow*2)
!
!     SPLIT REAL AND IMAGINARY DIVIDE IMAGINARY BY K
!
 310           DO k = 1 , n2 , 2
                  Z(Iq+n) = Z(Icp+k-1)
                  n = n + 1
                  Z(Iq+in) = Z(Icp+k)/Z(Ik+i)
                  in = in + 1
               ENDDO
            ENDDO
            Z(Ik+l) = Z(Ik+i)
            l = l + 1
            n = n + nn
            in = in + nn
         ELSE
            CALL skprec(Qhhl,nrow)
         ENDIF
      ENDDO
      CALL close(Qhhl,1)
      GOTO 99999
   ENDIF
 400  CALL mesage(-2,ifle,name)
99999 END SUBROUTINE fa1pki

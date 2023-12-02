!*==fa1pki.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE fa1pki(Fsave,Qhhl)
USE C_BLANK
USE C_FA1PKC
USE C_SYSTEM
USE C_UNPAKX
USE C_XMSSG
USE C_ZZZZZZ
USE ISO_FORTRAN_ENV                 
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Fsave
   INTEGER :: Qhhl
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buf1 , i , iad , icpd , ifle , in , ising , j , k , l , m , n , n2 , ni , nk1 , nn , nrow , nwr
   REAL(REAL64) :: det , dx1 , dx2
   REAL(REAL64) , DIMENSION(1) :: dz
   INTEGER , DIMENSION(2) , SAVE :: name
   REAL :: newm , rmi , rms , rmx
   REAL , SAVE :: oldm
   INTEGER , DIMENSION(7) :: trl
   EXTERNAL close , gopen , inverd , korsz , mesage , open , rdtrl , read , skprec , unpack , zeroc
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!
!     FA1PKI BUILDS AN INTERPOLATION MATRIX IN CORE FOR PK METHOD
!
!     LAST REVISED  2/91, BY J.PETKAS/LOOKHEED
!     TO ALLOW CALCULATION OF INTERPOLATION MATRIX IN D.P.
!
   !>>>>EQUIVALENCE (Dz(1),Z(1))
   DATA oldm/ - 1.0/
   DATA name/4HFA1P , 4HKI  /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         Iflag = 0
         IF ( oldm/=-1.0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         Ncore = korsz(Z)
         buf1 = Ncore - Sysbuf
         Imvr = 1
!
!     PUT M V   IN CORE ON SECOND LOOP RETURN IF SAME MACH
!
         ifle = Fsave
         CALL gopen(Fsave,Z(buf1),0)
         CALL read(*40,*20,Fsave,Z(Imvr),buf1,1,nwr)
 20      Ik = Imvr + nwr
         CALL close(Fsave,1)
         spag_nextblock_1 = 2
      CASE (2)
         i = (Floop-1)*3 + Imvr
         newm = Z(i)
         IF ( oldm==newm ) RETURN
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
         CALL open(*40,Qhhl,Z(buf1),0)
         CALL read(*40,*40,Qhhl,Z,-3,0,nwr)
         CALL read(*40,*40,Qhhl,n,1,0,nwr)
         n = n + n
         ni = min0(ni,n)
         CALL read(*40,*40,Qhhl,Z(Ik),ni,1,nwr)
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
         SPAG_Loop_1_1: DO i = 1 , nk1
            dx2 = Z(m+i)
            DO j = 1 , nk1
               IF ( i==nk1 .AND. j==nk1 ) EXIT SPAG_Loop_1_1
               IF ( j==nk1 .OR. i==nk1 ) THEN
                  dz(iad+n) = 1.0D+0
               ELSE
                  dx1 = Z(m+j)
                  dz(iad+n) = dabs((dx1-dx2)**3) + (dx1+dx2)**3
               ENDIF
               n = n + 1
            ENDDO
         ENDDO SPAG_Loop_1_1
         dz(iad+n) = 0.0D+0
!
!     MODIFICATION FOR LEVEL 17.7 UPDATE
!     REPLACE ALL CALLS TO INVAER WITH CALLS TO INVERS.
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
!
         ising = -1
         CALL inverd(nk1,dz(iad),nk1,0,0,det,ising,dz(icpd))
         IF ( ising==2 ) THEN
!
            WRITE (Nout,99001) Ufm , name
99001       FORMAT (A23,' 2427, SINGULAR MATRIX FOR INTERPOLATION IN ',2A4)
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
                     spag_nextblock_2 = 1
                     SPAG_DispatchLoop_2: DO
                        SELECT CASE (spag_nextblock_2)
                        CASE (1)
                           CALL unpack(*22,Qhhl,Z(Icp))
                           spag_nextblock_2 = 2
                           CYCLE SPAG_DispatchLoop_2
 22                        CALL zeroc(Z(Icp),nrow*2)
                           spag_nextblock_2 = 2
                        CASE (2)
!
!     SPLIT REAL AND IMAGINARY DIVIDE IMAGINARY BY K
!
                           DO k = 1 , n2 , 2
                              Z(Iq+n) = Z(Icp+k-1)
                              n = n + 1
                              Z(Iq+in) = Z(Icp+k)/Z(Ik+i)
                              in = in + 1
                           ENDDO
                           EXIT SPAG_DispatchLoop_2
                        END SELECT
                     ENDDO SPAG_DispatchLoop_2
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
            RETURN
         ENDIF
 40      CALL mesage(-2,ifle,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE fa1pki

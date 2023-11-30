
SUBROUTINE clvec(Lamd,Nvect,Phidl,Ih,Ibuf,Ibuf1)
   IMPLICIT NONE
   INTEGER Clsrew , Fileb(7) , Filek(7) , Filem(7) , Ii , Inc , It1 , It2 , Jj , Norew , Nrow , Rdrew , Scr(11) , Switch , Sysbuf
   REAL Dum(15) , Dumdcp(30) , Rd , Wrt , Wrtrew , Z(1)
   DOUBLE PRECISION Dz(1) , Lambda(2) , Mindia
   COMMON /cdcmpx/ Dumdcp , Mindia
   COMMON /cinvpx/ Filek , Filem , Fileb , Dum , Scr
   COMMON /cinvxx/ Lambda , Switch
   COMMON /names / Rd , Rdrew , Wrt , Wrtrew , Clsrew , Norew
   COMMON /packx / It1 , It2 , Ii , Jj , Inc
   COMMON /system/ Sysbuf
   COMMON /zzzzzz/ Z
   INTEGER Ibuf , Ibuf1 , Lamd , Nvect , Phidl
   INTEGER Ih(7)
   REAL buf(6) , f , fi1 , fnrow
   DOUBLE PRECISION di1 , dnrow
   INTEGER flag , i , ibuf2 , j , j2 , n , name(2)
!*****
!     CLVEC CACLULATES THE LEFT EIGENVECTORS FOR THE DETERMINANT AND
!     UPPER HESSENBERG APPROACHES TO THE COMPLEX EIGENVALUE PROBLEM
!*****
   EQUIVALENCE (Nrow,Filek(3))
   EQUIVALENCE (Dz(1),Z(1))
   DATA name/4HCLVE , 4HC   /
!*****
!     INITIALIZATION
!*****
   ibuf2 = Ibuf1 - Sysbuf
   IF ( Fileb(1)<0 ) Fileb(1) = 0
   IF ( Fileb(6)==0 ) Fileb(1) = 0
   DO i = 1 , 11
      Scr(i) = 300 + i
   ENDDO
   Switch = -204
   fnrow = float(Nrow)
   dnrow = fnrow
!*****
!     OPEN SORTED EIGENVALUE FILE
!*****
   CALL gopen(Lamd,Z(Ibuf),Rdrew)
   CALL skprec(Lamd,1)
!*****
!     LOOP TO CALCULATE LEFT EIGENVECTORS
!*****
   DO i = 1 , Nvect
! READ EIGENVALUE
      CALL read(*200,*300,Lamd,buf,6,0,flag)
      Lambda(1) = buf(3)
      Lambda(2) = buf(4)
! CREATE DYNAMIC MATRIX
 50   CALL cinvp1
! DECOMPOSE DYNAMIC MATRIX
      CALL cinvp2(*100)
! BUILD LOAD FOR FBS
      fi1 = float(i-1)
      di1 = fi1
      j2 = 2*Nrow
      DO j = 1 , j2 , 2
         f = float((j+1)/2)
         Dz(j) = Mindia/(1.0D0+(1.0D0-f/dnrow)*di1)
         Dz(j+1) = 0.0D0
      ENDDO
! PERFORM FORWARD-BACKWARD SUBSTITUTION - U(T)*L(T)*PHI
      CALL cdifbs(Dz(1),Z(ibuf2))
! NORMALIZE LEFT EIGENVECTOR
      CALL cnorm1(Dz(1),Nrow)
! PACK LEFT EIGENVECTOR ONTO PHIDL
      It1 = 4
      It2 = 3
      Ii = 1
      Jj = Nrow
      Inc = 1
      CALL pack(Dz(1),Phidl,Ih)
      CYCLE
! ADJUST CURRENT EIGENVALUE
 100  Lambda(1) = 1.01D0*Lambda(1)
      Lambda(2) = 1.01D0*Lambda(2)
      GOTO 50
! END OF LOOP
   ENDDO
   CALL close(Lamd,Clsrew)
   RETURN
!*****
!     ERRORS
!*****
 200  n = -2
   GOTO 400
 300  n = -3
 400  CALL mesage(n,Lamd,name)
END SUBROUTINE clvec

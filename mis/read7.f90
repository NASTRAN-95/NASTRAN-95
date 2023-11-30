
SUBROUTINE read7(Nr1,Olama,Ophia,Nlama,Nphia)
   IMPLICIT NONE
   REAL Core(1)
   DOUBLE PRECISION Dcore(2)
   INTEGER Ii , Iip , Incrp , Incur , It1 , It2 , Itb , Jj , Jjp , Sysbuf
   COMMON /packx / It1 , It2 , Iip , Jjp , Incrp
   COMMON /system/ Sysbuf
   COMMON /unpakx/ Itb , Ii , Jj , Incur
   COMMON /zzzzzz/ Core
   INTEGER Nlama , Nphia , Nr1 , Olama , Ophia
   DOUBLE PRECISION dx
   INTEGER i , ibuf1 , ibuf2 , ibuf3 , ibuf4 , ifile , ifl , ix(7) , j , lc , name(2) , nn , nr , nrow , sgldbl
   INTEGER korsz
   REAL x(7)
!
!     READ7  COPIES NR VECTORS FROM OPHIA TO NPHIA -
!     IT ALSO PLACES THE EIGENVALUES ON NLAMA
!     THIS ROUTINE HANDLES BOTH SINGLE AND DOUBLE PRECISION
!
   EQUIVALENCE (Dcore(1),Core(1)) , (x(1),dx)
   DATA name/4HREAD , 4H7   /
!
!     GET ORGANIZED
!
   nr = Nr1
   lc = korsz(Core)
   ibuf1 = lc - Sysbuf + 1
   ibuf2 = ibuf1 - Sysbuf
   ibuf3 = ibuf2 - Sysbuf
   ibuf4 = ibuf3 - Sysbuf
   ix(1) = Ophia
   CALL rdtrl(ix)
   nrow = ix(3)
   Ii = 1
   Jj = nrow
   It1 = ix(5)
   It2 = It1
   Itb = It1
   Dcore(1) = 0.0D0
   Incrp = 1
   ASSIGN 50 TO sgldbl
   IF ( Itb==2 ) ASSIGN 100 TO sgldbl
   Incur = 1
!
!     OPEN OLD FILES
!
   CALL gopen(Olama,Core(ibuf1),0)
   CALL fwdrec(*300,Olama)
   CALL gopen(Ophia,Core(ibuf2),0)
!
!     OPEN NEW FILES TO WRITE
!
   CALL gopen(Nlama,Core(ibuf3),1)
   CALL gopen(Nphia,Core(ibuf4),1)
!
!     START COPY LOOP
!
   CALL makmcb(ix,Nphia,nrow,ix(4),It2)
   DO i = 1 , nr
      CALL read(*300,*500,Olama,x,7,0,ifl)
      Ii = 0
      CALL unpack(*250,Ophia,Dcore(2))
      GOTO sgldbl
 50   x(1) = sqrt(x(6))
      DO j = 1 , nrow
         Core(j+2) = Core(j+2)/x(1)
      ENDDO
      GOTO 150
 100  dx = sqrt(x(6))
      DO j = 1 , nrow
         Dcore(j+1) = Dcore(j+1)/dx
      ENDDO
 150  Iip = Ii
      Jjp = Jj
      CALL pack(Dcore(2),Nphia,ix)
 200  dx = x(3)
      CALL write(Nlama,dx,2,1)
      CYCLE
!
!     NULL COLUMN
!
 250  Iip = 1
      Jjp = 1
      CALL pack(Dcore,Nphia,ix)
      GOTO 200
   ENDDO
   CALL close(Olama,1)
   CALL close(Ophia,1)
   CALL close(Nlama,2)
   CALL close(Nphia,1)
   RETURN
!
!     ERRORS
!
 300  nn = -2
 400  ifile = Olama
   CALL mesage(nn,ifile,name)
   RETURN
 500  nn = -3
   GOTO 400
END SUBROUTINE read7

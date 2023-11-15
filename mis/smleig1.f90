
SUBROUTINE smleig1(D,O,Val)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Entry , Ii , Iii , Incr , Incr1 , It1 , It2 , It3 , Jj , Jjj , Lama , Md , Mo , N , Nfound , Phia , Sysbuf , Xentry
   REAL Title(150) , Vcom(30)
   COMMON /givn  / Title
   COMMON /packx / It1 , It2 , Ii , Jj , Incr
   COMMON /system/ Sysbuf
   COMMON /unpakx/ It3 , Iii , Jjj , Incr1
!
! Dummy argument declarations
!
   REAL D(2) , O(2) , Val(2)
!
! Local variable declarations
!
   INTEGER i , ibuf1 , im1 , itra , loc , mcb(7)
   INTEGER korsz
   REAL p , q , valx
!
! End of declarations
!
!
!     COMPUTES EIGENVALUES AND VECTORS FOR 1X1 AND 2X2
!
!
!
   EQUIVALENCE (Mo,Title(2)) , (Md,Title(3)) , (Entry,Title(11)) , (Xentry,Title(20)) , (Vcom(1),Title(101)) , (N,Vcom(1)) ,        &
    & (Lama,Vcom(6)) , (Phia,Vcom(12)) , (Nfound,Vcom(10))
!
   DATA mcb/7*0/
!
!     D        ARRAY OF DIAGONALS
!     O        ARRAY OF OFF DIAGONALS
!     VAL      ARRAY OF EIGENVALUES
!     LAMA     FILE OF EIGENVALUES--HEADER,VALUES,ORDER FOUND
!     PHIA     FILE OF  VECTORS   --......,VECTORS-D.P.
!     MO       RESTART TAPE FOR MORE EIGENVALUES
!     MD       INPUT MATRIX
!     N        ORDER OF  PROBLEM
!     NFOUND   NUMBER OF EIGENVALUES/VECTOR PREVIOUSLY FOUND
!
!WKBR 2/94      IBUF1 =(KORSZ(O) - SYSBUF +1 )/2  -1
   ibuf1 = (korsz(O)-Sysbuf+1) - 1
!
!     OPEN INPUT MATRIX
!
   CALL gopen(Md,O(ibuf1),0)
!
!     SETUP FOR UNPACK
!
   It3 = 1
   Iii = 1
   Jjj = N
   Incr1 = 1
   ASSIGN 100 TO itra
   CALL unpack(*400,Md,D)
 100  IF ( N==2 ) THEN
!
!     THE MATRIX IS A 2X2
!
      O(1) = D(2)
      O(2) = 0.0
      ASSIGN 200 TO itra
      Iii = 2
      CALL unpack(*400,Md,D(2))
   ELSE
!
!     THE MATRIX IS A 1X1
!
      O(1) = 0.0
      Val(1) = D(1)
      loc = 1
      GOTO 300
   ENDIF
 200  p = D(1) + D(2)
   q = sqrt(p*p-4.0*(D(1)*D(2)-O(1)**2))
   Val(1) = (p+q)/2.0
   Val(2) = (p-q)/2.0
   loc = 0
!
!     WRAP UP ROUTINE
!
 300  CALL close(Md,1)
!
!     COPY D,O,LOC ONTO MO FOR RESTART
!
   CALL gopen(Mo,O(ibuf1),1)
!
!     SETUP FOR PACK
!
   im1 = 1
   It1 = 1
   It2 = 1
   Ii = 1
   Jj = N
   Incr = 1
   CALL pack(D,Mo,mcb)
   CALL pack(O,Mo,mcb)
   CALL write(Mo,loc,1,1)
   CALL close(Mo,1)
   IF ( N==1 ) THEN
!
!     1X1 WRITE OUT VECTORS AND VALUES
!
      mcb(1) = Phia
      mcb(2) = 0
      mcb(3) = 1
      mcb(4) = 2
      mcb(5) = 2
      mcb(6) = 0
      CALL gopen(Phia,O(ibuf1),1)
      Jj = 1
      CALL pack(1.0,Phia,mcb)
      CALL close(Phia,1)
      CALL wrttrl(mcb(1))
      CALL gopen(Lama,O(ibuf1),1)
      IF ( Nfound/=0 ) THEN
         DO i = 1 , Nfound
            CALL write(Lama,0.0,1,0)
         ENDDO
      ENDIF
      valx = Val(1)
      CALL write(Lama,valx,1,1)
      IF ( Nfound/=0 ) THEN
         DO i = 1 , Nfound
            CALL write(Lama,i,1,0)
         ENDDO
      ENDIF
      CALL write(Lama,Nfound+1,1,1)
      CALL close(Lama,1)
      mcb(1) = Lama
      CALL wrttrl(mcb)
   ENDIF
   Xentry = -Entry
   RETURN
 400  DO i = Iii , Jjj
      D(i) = 0.0
   ENDDO
   GOTO itra
END SUBROUTINE smleig1

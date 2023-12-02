!*==lamx.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE lamx
   IMPLICIT NONE
   USE C_BLANK
   USE C_CONDAS
   USE C_OUTPUT
   USE C_PACKX
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   REAL :: a , b , c
   INTEGER :: bufa , bufb , bufe , i , icore , j , l , m , ncol , nloop , nwr
   REAL , DIMENSION(3) :: d
   INTEGER , SAVE :: edit , ied , iz2 , lama , lamb , lma , nam
   INTEGER , DIMENSION(10) , SAVE :: ist
   INTEGER , DIMENSION(7) :: trl
   REAL , DIMENSION(7) :: z
   EXTERNAL close , fwdrec , gopen , korsz , mesage , pack , rdtrl , read , unpack , write , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
!     LAMX MAKES OR EDITS THE LAMA DATA BLOCK
!
!     LAMX  EDIT,LAMA/LAMB/C,Y,NLAM=0 $
!     IF NLAM LT 0 MAKE LAMB A MATRIX OF 5 COLUMNS
!     LAMA  OMEGA FREQ GM GS
!     UNTIL GM = 0.0
!
!
!
!
!
   !>>>>EQUIVALENCE (d(1),a) , (d(2),b) , (d(3),c)
   !>>>>EQUIVALENCE (Z(1),Iz(1))
!
   DATA edit , lama , lamb/101 , 102 , 201/
   DATA ist/21 , 6 , 7*0 , 7/
   DATA lma/1/ , ied/1/ , iz2/2/
   DATA nam/4HLAMX/
!
!     INITILIZE AND DECIDE MODE OF OPERATIONS
!
   icore = korsz(z)
   trl(1) = lama
   CALL rdtrl(trl)
   IF ( trl(1)<0 ) lma = 0
   trl(1) = edit
   CALL rdtrl(trl)
   IF ( trl(1)<0 ) ied = 0
   ncol = trl(2)
   IF ( ncol==0 ) ied = 0
   IF ( lma==0 .AND. ied==0 ) RETURN
   Ito = 1
   Ii = 1
   Incr = 1
   Ie = trl(3)
   IF ( Ie>3 ) Ie = 3
   b = 0.0
   c = 0.0
   bufb = icore - Sysbuf
   CALL gopen(lamb,z(bufb),1)
   IF ( lma==0 ) THEN
!
!      MAKE A NEW LAMB
!
      bufe = bufb - Sysbuf
      CALL gopen(edit,z(bufe),0)
      IF ( Nlam>0 ) ncol = min0(ncol,Nlam)
!
!     WRITE HEADER
!
      CALL write(lamb,ist,50,0)
      CALL write(lamb,Hdg,96,1)
!
!     MAKE RECORDS
!
      DO i = 1 , ncol
         spag_nextblock_1 = 1
         SPAG_DispatchLoop_1: DO
            SELECT CASE (spag_nextblock_1)
            CASE (1)
               CALL unpack(*5,edit,d)
               spag_nextblock_1 = 2
               CYCLE SPAG_DispatchLoop_1
 5             d(1) = 0.0
               d(2) = 0.0
               d(3) = 0.0
               spag_nextblock_1 = 2
            CASE (2)
               Iz(1) = i
               Iz(iz2) = i
               z(5) = a
               z(4) = Twopi*a
               z(3) = z(4)*z(4)
               z(6) = c
               z(7) = c*z(3)
               CALL write(lamb,z,7,0)
               EXIT SPAG_DispatchLoop_1
            END SELECT
         ENDDO SPAG_DispatchLoop_1
      ENDDO
      j = ncol
      GOTO 300
   ELSE
      bufa = bufb - Sysbuf
      CALL gopen(lama,z(bufa),0)
      IF ( Nlam<0 ) THEN
!
!     BUILD LAMB AS A MATRIX
!
         trl(1) = lamb
         trl(2) = 0
         trl(4) = 1
         trl(5) = 1
         trl(6) = 0
         trl(7) = 0
         Ityin = 1
         Ityout = 1
         Iii = 1
         Incr1 = 7
         CALL fwdrec(*400,lama)
         CALL read(*400,*500,lama,z,bufa,0,nwr)
         CALL mesage(8,0,nam)
         GOTO 400
      ELSE
         bufe = bufa - Sysbuf
!
!      EDITING LAMA FROM EDIT
!
         IF ( ied/=0 ) CALL gopen(edit,z(bufe),0)
!
!     WRITE HEADER
!
         CALL read(*100,*100,lama,z,bufe,1,nwr)
      ENDIF
   ENDIF
 100  CALL write(lamb,z,nwr,1)
   IF ( ied==0 ) THEN
!
!     COPY LAMA TO LAMB FOR NLAM RECORDS
!
      IF ( Nlam==0 ) GOTO 400
      j = Nlam
      m = 7*Nlam
      CALL read(*300,*200,lama,z,m,0,nwr)
      CALL write(lamb,z,7*Nlam,0)
      GOTO 300
   ELSE
!
!     MAKE RECORDS
!
      j = 0
      SPAG_Loop_1_1: DO i = 1 , ncol
         CALL read(*300,*300,lama,z,7,0,nwr)
         CALL unpack(*120,edit,d)
         IF ( a/=0.0 .OR. b/=0.0 .OR. c/=0.0 ) THEN
            IF ( c<0.0 ) CYCLE
            z(5) = z(5)*(1.0+b) + a
            z(4) = z(5)*Twopi
            z(3) = z(4)*z(4)
            IF ( c/=0.0 ) z(6) = c
            z(7) = z(6)*z(3)
         ENDIF
 120     j = j + 1
         Iz(1) = j
         IF ( Nlam>0 ) THEN
            IF ( j>Nlam ) EXIT SPAG_Loop_1_1
         ENDIF
         CALL write(lamb,z,7,0)
      ENDDO SPAG_Loop_1_1
      GOTO 300
   ENDIF
 200  CALL write(lamb,z,nwr,0)
 300  trl(1) = lamb
   trl(2) = j
   CALL wrttrl(trl)
 400  CALL close(lama,1)
   CALL close(lamb,1)
   CALL close(edit,1)
   RETURN
 500  nloop = 0
   SPAG_Loop_1_2: DO i = 1 , nwr , 7
      IF ( z(i+5)==0.0 ) EXIT SPAG_Loop_1_2
      nloop = nloop + 1
   ENDDO SPAG_Loop_1_2
   IF ( nloop/=0 ) THEN
      trl(3) = nloop
      Nnn = nloop
      l = 3
      DO i = 1 , 5
         CALL pack(z(l),lamb,trl)
         l = l + 1
      ENDDO
      CALL wrttrl(trl)
   ENDIF
   GOTO 400
END SUBROUTINE lamx


SUBROUTINE modac2(Nv,Inp1,Iout)
   IMPLICIT NONE
   INTEGER Ii , Incr , Itc , Iz(1) , Jj , Nfn , Nfo , Nz , Sysbuf
   REAL Z(1)
   COMMON /modac3/ Nfo , Nfn , Nz
   COMMON /system/ Sysbuf
   COMMON /unpakx/ Itc , Ii , Jj , Incr
   COMMON /zzzzzz/ Iz
   INTEGER Inp1 , Iout , Nv
   INTEGER file , i , ibuf1 , ibuf2 , icol , ifn , ihd(2) , ikr , inv , ip1 , j , k , m , mcb(7) , name(2) , nload
!
!     MODAC2  REDUCES THE SIZE OF INP1 (BY REMOVING SELECTED COLUMNS)
!
!     CORE IS LAIDED OUT AS FOLLOWS
!
!         CONTENTS            LENGTH  TYPE   POINTER
!         --------            ------  ----   -------
!
!         NEW TIMES           NFN      R     IFN
!         KEEP/REMOVE         NFO      I     IKR
!         COPIED COLUMN       MCB(3)   R     ICOL
!
!         2  BUFFERS          SYSBUF   I     IBUF1
!                             SYSBUF   I     IBUF2
!
!     VARIABLES
!
!     NV       NUMBER OF COLUMS TO PROCESS TOGETHER (MINUS SAYS ADD HEAD
!     INP1     COPY FROM THIS FILE
!     IOUT     COPY TO  THIS  FILE
!
!
!
   EQUIVALENCE (Z(1),Iz(1))
   DATA name/4HMODA , 4HC2  /
!
!     ALLOCATE CORE
!
   mcb(1) = Iout
   CALL rdtrl(mcb)
   IF ( mcb(1)<=0 ) RETURN
   mcb(1) = Inp1
   CALL rdtrl(mcb)
   IF ( mcb(1)<=0 ) RETURN
   nload = mcb(2)/(Nfo*iabs(Nv))
   ifn = 1
   ikr = ifn + Nfn
   icol = ikr + Nfo
   ibuf1 = Nz - Sysbuf + 1
   ibuf2 = ibuf1 - Sysbuf
   IF ( icol+mcb(3)+2*Sysbuf>Nz ) CALL mesage(-8,0,name)
!
!     OPEN  FILES
!
   file = Inp1
   CALL gopen(Inp1,Iz(ibuf1),0)
   file = Iout
   CALL open(*100,Iout,Iz(ibuf2),1)
   CALL fname(Iout,ihd)
   CALL write(Iout,ihd,2,0)
   IF ( Nv<=0 ) CALL write(Iout,Z,Nfn,0)
   CALL write(Iout,0,0,1)
!
!     SET UP MATRIX TRAILER
!
   file = Inp1
   mcb(2) = 0
   mcb(6) = 0
   mcb(7) = 0
   mcb(1) = Iout
   Itc = mcb(5)
   Incr = 1
   inv = iabs(Nv)
   DO m = 1 , nload
      k = ikr - 1
      DO i = 1 , Nfo
         k = k + 1
         IF ( Iz(k)==0 ) THEN
!
!     SKIP COLUMN
!
            DO j = 1 , inv
               CALL fwdrec(*300,Inp1)
            ENDDO
         ELSE
!
!     KEEP COLUMN
!
            CALL cyct2b(Inp1,Iout,inv,Iz(icol),mcb)
         ENDIF
      ENDDO
   ENDDO
!
!     CLOSE  UP
!
   CALL close(Inp1,1)
   CALL close(Iout,1)
   CALL wrttrl(mcb)
   RETURN
!
!     ERROR MESSAGES
!
 100  ip1 = -1
 200  CALL mesage(ip1,file,name)
 300  ip1 = -2
   GOTO 200
END SUBROUTINE modac2


SUBROUTINE gpwg1a(Ip,Bgpdt,Cstm,Eqexin,D,Iscalr)
   IMPLICIT NONE
   INTEGER Ii , Incr , It1 , It2 , Iz(1) , Jj , Sysbuf
   REAL Z(5)
   COMMON /packx / It1 , It2 , Ii , Jj , Incr
   COMMON /system/ Sysbuf
   COMMON /zzzzzz/ Iz
   INTEGER Bgpdt , Cstm , D , Eqexin , Ip , Iscalr
   REAL dd(6,6) , r(3) , ti(3,3) , tr(3,3) , tt(3,3)
   INTEGER file , i , ibuf , ibuf1 , iflag , ip1 , j , mcb(7) , name(2) , ncstm , nz
   INTEGER korsz
!
!     ROUTINE FORMS D MATRIX (ACCTUALLY D TRANSPOSE)
!
   !>>>>EQUIVALENCE (Iz(1),Z(1))
   DATA name/4HGPWG , 4H1A  /
!
!     CONVERT  EXTERNAL IP TO INTERNAL IP
!
   ibuf = korsz(Z) - Sysbuf + 1
   file = Eqexin
   CALL gopen(Eqexin,Z(ibuf),0)
   CALL read(*700,*100,Eqexin,Iz(1),ibuf-1,0,iflag)
   GOTO 800
 100  CALL close(Eqexin,1)
   DO i = 1 , iflag , 2
      IF ( Iz(i)==Ip ) GOTO 200
   ENDDO
   CALL mesage(41,Ip,name)
   Ip = 0
   GOTO 300
 200  Ip = Iz(i+1)
!
!     FIND RZERO FOR  IP
!
 300  file = Bgpdt
   r(1) = 0.0
   r(2) = 0.0
   r(3) = 0.0
   CALL gopen(Bgpdt,Z(ibuf),0)
   IF ( Ip/=0 ) THEN
      i = (Ip-1)*4
      CALL fread(Bgpdt,Z,-i,0)
      CALL fread(Bgpdt,i,1,0)
      IF ( i==-1 ) THEN
!
!     SCALAR POINT
!
         CALL mesage(41,Ip,name)
      ELSE
         CALL fread(Bgpdt,r,3,0)
      ENDIF
      CALL rewind(Bgpdt)
      CALL skprec(Bgpdt,1)
   ENDIF
!
!     SET UP TO WRITE D
!
   ibuf1 = ibuf - Sysbuf
   nz = ibuf1 - 5
!
!     BRING IN CSTM
!
   file = Cstm
   CALL open(*500,Cstm,Z(ibuf1),0)
   CALL fwdrec(*700,Cstm)
   CALL read(*700,*400,Cstm,Z(5),nz,0,ncstm)
   GOTO 800
 400  CALL close(Cstm,1)
   CALL pretrs(Z(5),ncstm)
 500  CALL gopen(D,Z(ibuf1),1)
   CALL makmcb(mcb,D,6,2,1)
   Iscalr = 0
   Ii = 1
   Jj = 6
   It1 = 1
   It2 = 1
   Incr = 1
   DO
!
!     EXAMINE BGPDT
!
      CALL read(*700,*600,Bgpdt,Z(1),4,0,iflag)
      IF ( Iz(1)<0 ) THEN
!
!     SCALAR POINT
!
         DO i = 1 , 6
            dd(i,1) = 0.0
         ENDDO
         CALL pack(dd,D,mcb)
      ELSE
!
!     COMPUTE  TR
!
         Iscalr = 1
         tr(1,1) = 0.0
         tr(2,2) = 0.0
         tr(3,3) = 0.0
         tr(2,1) = Z(4) - r(3)
         tr(1,2) = -tr(2,1)
         tr(3,1) = r(2) - Z(3)
         tr(1,3) = -tr(3,1)
         tr(3,2) = Z(2) - r(1)
         tr(2,3) = -tr(3,2)
         DO i = 1 , 3
            DO j = 1 , 3
               ti(i,j) = 0.0
               IF ( i==j ) ti(i,j) = 1.0
            ENDDO
         ENDDO
         IF ( Iz(1)/=0 ) THEN
            CALL transs(Iz(1),ti)
            CALL gmmats(ti,3,3,1,tr,3,3,0,tt)
            DO i = 1 , 3
               DO j = 1 , 3
                  tr(i,j) = tt(i,j)
               ENDDO
            ENDDO
         ENDIF
!
!     MOVE STUFF INTO  DD
!
         DO i = 1 , 6
            DO j = 1 , 3
               IF ( i>3 ) THEN
                  dd(i,j) = tr(i-3,j)
                  dd(j,i) = 0.0
               ELSE
                  dd(i,j) = ti(j,i)
                  dd(i+3,j+3) = dd(i,j)
               ENDIF
            ENDDO
         ENDDO
         DO i = 1 , 6
            CALL pack(dd(1,i),D,mcb)
         ENDDO
      ENDIF
   ENDDO
!
!     END BGPDT
!
 600  CALL close(Bgpdt,1)
   CALL close(D,1)
   CALL wrttrl(mcb)
   RETURN
 700  DO
      ip1 = -2
!
!     ERROR MESAGES
!
      CALL mesage(ip1,file,name)
   ENDDO
 800  ip1 = -8
   CALL mesage(ip1,file,name)
   GOTO 700
END SUBROUTINE gpwg1a
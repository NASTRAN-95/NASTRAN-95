
SUBROUTINE ddampg
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ibuf(80) , Ii , Iii , Iin , Incr , Iout , Jncr , Jout , Ndir , Nmodes , Nn , Nnn
   REAL Z(1)
   COMMON /blank / Nmodes , Ndir
   COMMON /packx / Iin , Iout , Ii , Nn , Incr
   COMMON /system/ Ibuf
   COMMON /unpakx/ Jout , Iii , Nnn , Jncr
   COMMON /zzzzzz/ Z
!
! Local variable declarations
!
   INTEGER buf1 , buf2 , buf3 , file , ijk , isub , j , k , lcore , mcb(7) , mcb4 , mcb5 , mp , n , nam(2) , ncolmp , ncolpv ,      &
         & nrowmp , nrowpv , pg , pvw
   INTEGER korsz
!
! End of declarations
!
!
!     DDAMPG  MP,PVW/PG/V,N,NMODES/V,N,NDIR $
!
!     MP IS MGG*PHIG, PVW IS (PF)*SSDV*OMEGA, PARTICIPATION FACTORS X
!     SHOCK SPECTRUM DESIGN VALUES X RADIAN FREQUENCIES.
!     MP IS (NXM).  IF PVW IS A VECTOR (MX1), WE WANT TO MULTIPLY THE
!     ITH. TERM INTO THE ITH. COLUMN OF MP.  PG IS THEN NXM.
!     IF PVW IS A MATRIX (MXL), WE REPEAT THE PREVIOUS COMPUTATION FOR
!     EACH OF THE L VECTORS, MAKING PG (NX(MXL)).
!     NMODES IS NUMBER OF MODES. NDIR IS NUMBER OF SHOCK DIRECTIONS
!
   DATA mp , pvw , pg/101 , 102 , 201/
   DATA nam/4HDDAM , 4HPG  /
!
!     SET UP OPEN CORE AND BUFFERS
!
   lcore = korsz(Z)
   buf1 = lcore - Ibuf(1) + 1
   buf2 = buf1 - Ibuf(1)
   buf3 = buf2 - Ibuf(1)
   lcore = buf3 - 1
   IF ( lcore<=0 ) GOTO 200
!
!     PICK UP ROW AND COLUMN STATISTICS AND SET PACK/UNPACK PARAMETERS
!
   mcb(1) = mp
   CALL rdtrl(mcb)
   ncolmp = mcb(2)
   Nmodes = ncolmp
   nrowmp = mcb(3)
   mcb(1) = pvw
   CALL rdtrl(mcb)
   ncolpv = mcb(2)
   Ndir = ncolpv
   nrowpv = mcb(3)
   mcb4 = mcb(4)
   mcb5 = mcb(5)
!
!
   IF ( lcore<nrowpv+nrowmp ) GOTO 200
   IF ( ncolmp/=nrowpv ) THEN
      n = -7
      GOTO 300
   ELSE
      mcb(1) = pg
      mcb(2) = 0
      mcb(3) = nrowmp
      mcb(4) = mcb4
      mcb(5) = mcb5
      mcb(6) = 0
      mcb(7) = 0
!
      Jout = 1
      Iin = 1
      Iout = 1
      Ii = 1
      Iii = 1
      Nn = nrowmp
      Incr = 1
      Jncr = 1
!
      CALL gopen(mp,Z(buf1),0)
      CALL gopen(pvw,Z(buf2),0)
      CALL gopen(pg,Z(buf3),1)
!
      DO ijk = 1 , ncolpv
         Nnn = nrowpv
         CALL unpack(*40,pvw,Z(1))
!
         DO j = 1 , ncolmp
            Nnn = nrowmp
            CALL unpack(*10,mp,Z(nrowpv+1))
!
            DO k = 1 , nrowmp
               isub = nrowpv + k
               Z(isub) = Z(isub)*Z(j)
            ENDDO
            GOTO 20
!
 10         DO k = 1 , nrowmp
               Z(nrowpv+k) = 0.
            ENDDO
 20         CALL pack(Z(nrowpv+1),pg,mcb)
         ENDDO
         GOTO 60
!
!     NULL COLUMN FOR PVW-WRITE OUT NCOLMP ZERO COLUMNS OF LENGTH NROWMP
!
 40      DO k = 1 , nrowmp
            Z(k) = 0.
         ENDDO
         DO k = 1 , ncolmp
            CALL pack(Z,pg,mcb)
         ENDDO
 60      CALL rewind(mp)
         file = mp
         CALL fwdrec(*100,mp)
      ENDDO
!
      CALL wrttrl(mcb)
      CALL close(mp,1)
      CALL close(pvw,1)
      CALL close(pg,1)
!
      RETURN
   ENDIF
!
!     FATAL ERRORS
!
 100  n = -2
   GOTO 300
 200  n = -8
   file = 0
 300  CALL mesage(n,file,nam)
END SUBROUTINE ddampg

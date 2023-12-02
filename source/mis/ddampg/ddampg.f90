!*==ddampg.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE ddampg
   IMPLICIT NONE
   USE C_BLANK
   USE C_PACKX
   USE C_SYSTEM
   USE C_UNPAKX
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: buf1 , buf2 , buf3 , file , ijk , isub , j , k , lcore , mcb4 , mcb5 , n , ncolmp , ncolpv , nrowmp , nrowpv
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , SAVE :: mp , pg , pvw
   INTEGER , DIMENSION(2) , SAVE :: nam
   EXTERNAL close , fwdrec , gopen , korsz , mesage , pack , rdtrl , rewind , unpack , wrttrl
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
   INTEGER :: spag_nextblock_3
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
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     SET UP OPEN CORE AND BUFFERS
!
         lcore = korsz(Z)
         buf1 = lcore - Ibuf(1) + 1
         buf2 = buf1 - Ibuf(1)
         buf3 = buf2 - Ibuf(1)
         lcore = buf3 - 1
         IF ( lcore<=0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
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
         IF ( lcore<nrowpv+nrowmp ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( ncolmp/=nrowpv ) THEN
            n = -7
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
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
               spag_nextblock_2 = 1
               SPAG_DispatchLoop_2: DO
                  SELECT CASE (spag_nextblock_2)
                  CASE (1)
                     Nnn = nrowpv
                     CALL unpack(*4,pvw,Z(1))
!
                     DO j = 1 , ncolmp
                        spag_nextblock_3 = 1
                        SPAG_DispatchLoop_3: DO
                           SELECT CASE (spag_nextblock_3)
                           CASE (1)
                              Nnn = nrowmp
                              CALL unpack(*2,mp,Z(nrowpv+1))
!
                              DO k = 1 , nrowmp
                                 isub = nrowpv + k
                                 Z(isub) = Z(isub)*Z(j)
                              ENDDO
                              spag_nextblock_3 = 2
                              CYCLE SPAG_DispatchLoop_3
!
 2                            DO k = 1 , nrowmp
                                 Z(nrowpv+k) = 0.
                              ENDDO
                              spag_nextblock_3 = 2
                           CASE (2)
                              CALL pack(Z(nrowpv+1),pg,mcb)
                              EXIT SPAG_DispatchLoop_3
                           END SELECT
                        ENDDO SPAG_DispatchLoop_3
                     ENDDO
                     spag_nextblock_2 = 2
                     CYCLE SPAG_DispatchLoop_2
!
!     NULL COLUMN FOR PVW-WRITE OUT NCOLMP ZERO COLUMNS OF LENGTH NROWMP
!
 4                   DO k = 1 , nrowmp
                        Z(k) = 0.
                     ENDDO
                     DO k = 1 , ncolmp
                        CALL pack(Z,pg,mcb)
                     ENDDO
                     spag_nextblock_2 = 2
                  CASE (2)
                     CALL rewind(mp)
                     file = mp
                     CALL fwdrec(*20,mp)
                     EXIT SPAG_DispatchLoop_2
                  END SELECT
               ENDDO SPAG_DispatchLoop_2
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
 20      n = -2
         spag_nextblock_1 = 3
         CYCLE SPAG_DispatchLoop_1
      CASE (2)
         n = -8
         file = 0
         spag_nextblock_1 = 3
      CASE (3)
         CALL mesage(n,file,nam)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE ddampg

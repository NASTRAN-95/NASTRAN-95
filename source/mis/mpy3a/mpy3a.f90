!*==mpy3a.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE mpy3a(Z,Iz,Dz)
   USE c_mpy3cp
   USE c_mpy3tl
   USE c_packx
   USE c_unpakx
   USE c_zntpkx
   USE iso_fortran_env
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: Z
   INTEGER , DIMENSION(1) :: Iz
   REAL(REAL64) , DIMENSION(1) :: Dz
!
! Local variable declarations rewritten by SPAG
!
   REAL(REAL64) :: da
   INTEGER :: file , i , iacols , ib , ibcols , ii , incrjj , ipoint , itrans , j , jj , jj2 , jjc , jjt , k , l , nerr , npoint ,  &
            & precl , precn
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL close , fwdrec , intpk , mesage , open , pack , rewind , savpos , unpack , zntpki
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   INTEGER :: spag_nextblock_2
!*****
!    PREPARES B AND A(T).
!*****
!
!
!
!
!
!
!
!
! FILES
! SUBROUTINE CALL PARAMETERS
! PACK
! UNPACK
! TERMWISE MATRIX READ
!
!
!
   !>>>>EQUIVALENCE (Ipoint,Zpntrs(3)) , (Npoint,Zpntrs(4)) , (Iacols,Zpntrs(5)) , (Itrans,Zpntrs(7)) , (Ibcols,Zpntrs(11))
   !>>>>EQUIVALENCE (A(1),Da)
!
!
!
   DATA name/4HMPY3 , 4HA   /
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!*****
!    FILE OPENING.
!*****
         file = scr1
         CALL open(*40,scr1,Z(buf2),1)
         file = fileb(1)
         CALL open(*40,fileb,Z(buf3),0)
         CALL fwdrec(*60,fileb)
!*****
!    UNPACK B AND PACK INTO SCRATCH FILE 1.
!*****
! PACK PARAMETERS
         typin = prec
         typout = prec
         row1 = 1
         rowm = n
         incr = 1
! UNPACK PARAMETERS
         utyp = prec
         urow1 = 1
         urown = n
         uincr = 1
         precn = prec*n
         mcb(1) = 301
         mcb(2) = 0
         mcb(3) = n
         mcb(4) = 1
         mcb(5) = prec
         mcb(6) = 0
         mcb(7) = 0
         DO k = 1 , ncb
            spag_nextblock_2 = 1
            SPAG_DispatchLoop_2: DO
               SELECT CASE (spag_nextblock_2)
               CASE (1)
                  CALL unpack(*2,fileb,Z(ibcols))
                  spag_nextblock_2 = 2
                  CYCLE SPAG_DispatchLoop_2
 2                ib = ibcols - 1
                  DO l = 1 , precn
                     ib = ib + 1
                     Z(ib) = 0.
                  ENDDO
                  spag_nextblock_2 = 2
               CASE (2)
                  CALL pack(Z(ibcols),scr1,mcb)
                  CALL savpos(scr1,Iz(k))
                  EXIT SPAG_DispatchLoop_2
               END SELECT
            ENDDO SPAG_DispatchLoop_2
         ENDDO
         CALL close(scr1,1)
         CALL close(fileb,1)
         IF ( icore/=1 ) THEN
!*****
!    INITIALIZE ARRAY CONTAINING POINTERS TO ROWS OF MATRIX A TO 0.
!*****
            DO l = ipoint , npoint
               Iz(l) = 0
            ENDDO
!*****
!    COUNT NO. OF NON-ZERO COLUMNS IN EACH ROW OF A.
!*****
            file = filea(1)
            CALL open(*40,filea,Z(buf1),0)
            CALL fwdrec(*60,filea)
            DO i = 1 , m
               CALL intpk(*10,filea,0,prec,0)
               SPAG_Loop_2_1: DO
                  CALL zntpki
                  ii = ipoint + irow - 1
                  Iz(ii) = Iz(ii) + 1
                  IF ( eol==1 ) EXIT SPAG_Loop_2_1
               ENDDO SPAG_Loop_2_1
 10         ENDDO
!*****
!    CALCULATE POINTERS TO ROWS OF MATRIX A.
!*****
            jj = 1
            DO l = ipoint , npoint
               IF ( Iz(l)/=0 ) THEN
                  incrjj = Iz(l)
                  Iz(l) = jj
                  jj = jj + incrjj
               ENDIF
            ENDDO
            laend = jj - 1
!*****
!    PROCESS A(T) MATRIX.
!*****
            file = filea(1)
            CALL rewind(filea)
            CALL fwdrec(*60,filea)
            jj2 = iacols + laend - 1
            DO jj = iacols , jj2
               Iz(jj) = 0
            ENDDO
            DO j = 1 , m
               CALL intpk(*20,filea,0,prec,0)
               SPAG_Loop_2_2: DO
                  CALL zntpki
                  l = ipoint + irow - 1
                  jj = Iz(l)
                  jjc = iacols + jj - 1
                  DO WHILE ( Iz(jjc)/=0 )
                     jj = jj + 1
                     jjc = jjc + 1
                  ENDDO
                  Iz(jjc) = j
                  IF ( prec==2 ) THEN
                     jjt = (itrans-1)/2 + jj
                     Dz(jjt) = da
                     IF ( eol==1 ) EXIT SPAG_Loop_2_2
                  ELSE
                     jjt = itrans + jj - 1
                     Z(jjt) = a(1)
                     IF ( eol==1 ) EXIT SPAG_Loop_2_2
                  ENDIF
               ENDDO SPAG_Loop_2_2
 20         ENDDO
            precl = prec*laend
            CALL close(filea,1)
         ENDIF
         RETURN
!*****
!    ERROR MESSAGES.
!*****
 40      nerr = -1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
 60      nerr = -2
         spag_nextblock_1 = 2
      CASE (2)
         CALL mesage(nerr,file,name)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
!
END SUBROUTINE mpy3a

!*==linkup.f90 processed by SPAG 8.01RF 16:20  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE linkup(*,Name)
!
   USE c_lnklst
   USE c_machin
   USE c_zzzzzz
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(2) :: Name
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: ihash , ikind , itotal , k , l
   EXTERNAL andf , complf , lshift , orf , rshift
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
!     HASH INTO TABLE
!
         IF ( machx==4 ) THEN
!
!     60-BIT MACHINE
!
            itotal = rshift(Name(1),18) + rshift(Name(2),18)
         ELSEIF ( machx==5 .OR. machx==6 .OR. machx==8 .OR. machx==9 .OR. machx==10 .OR. machx==11 .OR. machx==16 .OR.              &
                & machx==17 .OR. machx==18 .OR. machx==19 .OR. machx==20 .OR. machx==21 .OR. machx==22 ) THEN
!
!     32-BIT MACHINES
!
            itotal = rshift(Name(1),1) + rshift(Name(2),1)
         ELSEIF ( machx==12 .OR. machx==14 .OR. machx==15 ) THEN
!
!     64-BIT MACHINES
!
            itotal = rshift(Name(1),32) + rshift(Name(2),32)
         ELSE
!
!     IBM AND UNIVAC
!
            itotal = Name(1) + Name(2)
         ENDIF
!
         ihash = 4*iabs(mod(itotal,250)) + 4
         k = andf(z(ihash),mask1)
         IF ( k==0 ) THEN
!
!     NO HASH CHAIN FOUND - CREATE CHAIN
!
            z(ihash) = z(ihash) + itop
            spag_nextblock_1 = 3
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 2
      CASE (2)
!
!     HASH CHAIN FOUND - CHECK PRESENCE OF NAME
!
         IF ( z(k)==Name(1) .AND. z(k+1)==Name(2) ) THEN
            ikind = rshift(andf(z(k+3),mask3),28)
            IF ( (ikind+1)/2==(kind+1)/2 ) THEN
               spag_nextblock_1 = 4
               CYCLE SPAG_DispatchLoop_1
            ENDIF
         ENDIF
         l = andf(z(k+3),mask2)
         IF ( l==0 ) THEN
            z(k+3) = z(k+3) + lshift(itop,14)
         ELSE
            k = rshift(l,14)
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
!
!     NO ENTRY FOUND - CREATE ENTRY
!
         z(itop) = Name(1)
         z(itop+1) = Name(2)
         z(itop+2) = lshift(itype,28)
         z(itop+3) = z(itop+3) + lshift(iabs(kind),28)
         itop = itop + 4
         IF ( itop>=ibot ) RETURN 1
         IF ( kind<0 ) RETURN
         k = itop - 4
         spag_nextblock_1 = 4
      CASE (4)
!
!     ADD STATEMENT NUMBER TO LIST
!
         l = andf(z(k+2),mask1)
         IF ( l/=0 ) THEN
!
!     CHAIN ENTRY ON LIST
!
            l = rshift(andf(z(k+2),mask2),14)
            z(l) = andf(z(l),complf(mask2))
            z(l) = orf(z(l),lshift(ibot,14))
         ELSE
!
!     LIST IS EMPTY - START LIST
!
            z(k+2) = z(k+2) + ibot
         ENDIF
!
!     ADD ENTRY TO LIST
!
         z(ibot) = orf(lshift(kind,28),isn)
         z(k+2) = andf(z(k+2),complf(mask2))
         z(k+2) = z(k+2) + lshift(ibot,14)
         ibot = ibot - 1
         IF ( itop>=ibot ) RETURN 1
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE linkup

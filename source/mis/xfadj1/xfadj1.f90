!*==xfadj1.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE xfadj1(Bf,shift,Sd)
!
!     XFADJ1 ADJUSTS 4 CHARACTER FIELDS, LEFT OR RIGHT, 2 OR 4 FIELDS
!     AT A TIME
!
!     BF    = ADDR OF LEFT MOST FIELD
!     SHIFT = LSHIFT OR RSHIFT
!     SD   = 0 SINGLE (2 FIELDS), 1 DOUBLE (4 FIELDS)
!     RIGHT SHIFTING CAUSES INSERTION OF LEADING ZEROS
!
   USE c_machin
   USE c_xsrtcm
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Bf
   INTEGER , EXTERNAL :: shift
   INTEGER :: Sd
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , DIMENSION(6) :: bk , mk
   INTEGER :: bkx , blk , blks , i1 , i2 , i3 , i4 , ihld , ii , j , n , n1 , n2 , n3
   LOGICAL :: dec
   INTEGER , DIMENSION(3) :: sft
   EXTERNAL andf , isft , khrfn1 , khrfn3 , khrfn4 , lshift , orf , rshift
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
   !>>>>EQUIVALENCE (Bk(1),Bkmsk1(2)) , (Mk(1),Bimsk1(1)) , (Sft(1),Shifts(2)) , (Blks,Bkmsk1(8)) , (Bkx,Bkmsk1(1))
!
!     DATA     BK   / 4H0000,4H0000,4H0000,4H000 ,4H00  ,4H0   /
!     DATA     (MK(I),I=1,6) /O777777000000,O777700000000,O770000000000,
!    1                        O000000770000,O000077770000,O007777770000/
!     DATA     (SFT(I),I=1,3)/6,12,18/
!     DATA     BLKS / 4H    /,    BKX/4H0000/
!
!
!     INITIALIZE ROUTINES
!
         dec = mach==5 .OR. mach==6 .OR. mach==21
         IF ( shift(mk(3),sft(1))/=0 ) THEN
!
!     RIGHT SHIFT REQUESTED
!
            blk = bkx
            j = 4
            IF ( Sd==0 ) THEN
!
!     SINGLE FIELD
!
               i1 = 2
               i2 = 1
            ELSE
!
!     DOUBLE FIELD
!
               i1 = 4
               i2 = 3
               i3 = 2
               i4 = 1
            ENDIF
         ELSE
!
!     LEFT SHIFT REQUESTED
!
            blk = blks
            i1 = 1
            i2 = 2
            i3 = 3
            i4 = 4
            j = 3
         ENDIF
!
!     TOTAL FIELD SHIFTS
!
         n = 0
         SPAG_Loop_1_1: DO
            IF ( j/=4 .OR. Bf(i1)==blks ) THEN
               IF ( j/=3 .OR. Bf(i1)==blks .OR. Bf(i1)==bkx ) THEN
                  Bf(i1) = Bf(i2)
                  Bf(i2) = blk
                  IF ( Sd/=0 ) THEN
                     n = n + 1
                     Bf(i2) = Bf(i3)
                     Bf(i3) = Bf(i4)
                     Bf(i4) = blk
                     IF ( n/=3 ) CYCLE
                  ENDIF
                  IF ( Bf(i1)==blks ) RETURN
               ENDIF
            ENDIF
!
!     CHARACTER SHIFTS BETWEEN FIELDS
!
            n = 0
            IF ( j==3 ) THEN
               spag_nextblock_1 = 6
               CYCLE SPAG_DispatchLoop_1
            ENDIF
!
!     RIGHT
!
            ii = i1
            EXIT SPAG_Loop_1_1
         ENDDO SPAG_Loop_1_1
         spag_nextblock_1 = 2
      CASE (2)
         IF ( Bf(ii)==blks ) THEN
            Bf(ii) = bkx
         ELSEIF ( Bf(ii)/=bkx ) THEN
            SPAG_Loop_1_2: DO
               IF ( .NOT.dec ) ihld = rshift(andf(mk(3),Bf(ii)),1)
               IF ( dec ) ihld = khrfn4(rshift(khrfn4(khrfn1(bkmsk2,1,Bf(ii),1)),1))
               IF ( ihld/=icon1 ) THEN
                  IF ( n==0 ) EXIT SPAG_Loop_1_2
                  spag_nextblock_1 = 4
                  CYCLE SPAG_DispatchLoop_1
               ELSE
                  n = n + 1
                  IF ( .NOT.dec ) Bf(ii) = lshift(Bf(ii),sft(1))
                  IF ( dec ) Bf(ii) = khrfn3(bkmsk2,Bf(ii),1,1)
                  IF ( n>=3 ) THEN
                     spag_nextblock_1 = 4
                     CYCLE SPAG_DispatchLoop_1
                  ENDIF
               ENDIF
            ENDDO SPAG_Loop_1_2
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         ii = ii - 1
         IF ( ii/=0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         n = 0
         spag_nextblock_1 = 5
      CASE (4)
         n2 = 4 - n
         IF ( .NOT.dec ) Bf(ii) = orf(rshift(Bf(ii),sft(n)),bk(n2))
         IF ( dec ) Bf(ii) = khrfn3(bk(n2),Bf(ii),n,0)
         n = 0
         spag_nextblock_1 = 3
      CASE (5)
!
!     RIGHT
!
         IF ( dec ) THEN
            IF ( khrfn1(mk(4),4,Bf(i1),4)/=khrfn1(mk(4),4,blks,4) ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 7
         ELSE
            IF ( andf(mk(4),Bf(i1))/=andf(mk(4),blks) ) THEN
               spag_nextblock_1 = 8
               CYCLE SPAG_DispatchLoop_1
            ENDIF
            spag_nextblock_1 = 7
         ENDIF
      CASE (6)
!
!     LEFT
!
         IF ( .NOT.dec ) ihld = rshift(andf(mk(3),Bf(i1)),1)
         IF ( dec ) ihld = khrfn4(rshift(khrfn4(khrfn1(bkmsk2,1,Bf(i1),1)),1))
         IF ( ihld/=icon1 .AND. ihld/=icon2 ) THEN
            spag_nextblock_1 = 8
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 7
      CASE (7)
         n = n + 1
         IF ( .NOT.dec ) Bf(i1) = shift(Bf(i1),sft(1))
         IF ( dec ) Bf(i1) = khrfn3(bkmsk2,Bf(i1),1,4-j)
         IF ( n>=3 ) THEN
            spag_nextblock_1 = 9
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         IF ( j==3 ) THEN
            spag_nextblock_1 = 6
            CYCLE SPAG_DispatchLoop_1
         ENDIF
         spag_nextblock_1 = 5
      CASE (8)
         IF ( n==0 ) RETURN
         spag_nextblock_1 = 9
      CASE (9)
         IF ( j==4 ) THEN
!
!     RIGHT SHIFTS
!
            n1 = 7 - n
            n2 = 4 - n
         ELSE
!
!     LEFT SHIFTS
!
            n1 = n
            n2 = n + 3
         ENDIF
         n3 = 4 - n
         IF ( .NOT.dec ) Bf(i1) = orf(andf(mk(n1),Bf(i1)),andf(mk(n2),isft(Bf(i2),sft(n3),j)))
         IF ( dec ) Bf(i1) = khrfn3(Bf(i1),Bf(i2),n3,j-3)
         Bf(i1) = orf(Bf(i1),bkmsk2)
         IF ( .NOT.dec ) Bf(i2) = orf(andf(mk(n1),shift(Bf(i2),sft(n))),bk(n2))
         IF ( dec ) Bf(i2) = khrfn3(bk(n2),Bf(i2),n,4-j)
         IF ( Sd==0 ) RETURN
!
         IF ( .NOT.dec ) Bf(i2) = orf(andf(mk(n1),Bf(i2)),andf(mk(n2),isft(Bf(i3),sft(n3),j)))
         IF ( dec ) Bf(i2) = khrfn3(Bf(i2),Bf(i3),n3,j-3)
         Bf(i2) = orf(Bf(i2),bkmsk2)
         IF ( Bf(i2)==blk ) RETURN
!
         IF ( .NOT.dec ) Bf(i3) = orf(andf(mk(n1),shift(Bf(i3),sft(n))),bk(n2))
         IF ( dec ) Bf(i3) = khrfn3(bk(n2),Bf(i3),n,4-j)
         IF ( .NOT.dec ) Bf(i3) = orf(andf(mk(n1),Bf(i3)),andf(mk(n2),isft(Bf(i4),sft(n3),j)))
         IF ( dec ) Bf(i3) = khrfn3(Bf(i3),Bf(i4),n3,j-3)
         Bf(i3) = orf(Bf(i3),bkmsk2)
         IF ( Bf(i3)==blk ) RETURN
!
         IF ( .NOT.dec ) Bf(i4) = orf(andf(mk(n1),shift(Bf(i4),sft(n))),bk(n2))
         IF ( dec ) Bf(i4) = khrfn3(bk(n2),Bf(i4),n,4-j)
         EXIT SPAG_DispatchLoop_1
      END SELECT
   ENDDO SPAG_DispatchLoop_1
END SUBROUTINE xfadj1

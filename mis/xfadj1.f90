
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
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Bimsk1(6) , Bimsk2(5) , Bimsk3(4) , Bimsk4(4) , Bimsk5(2) , Bimsk6 , Bk(6) , Bkmsk1(8) , Bkmsk2 , Bkx , Blank , Blks ,   &
         & Dollar , Icon1 , Icon2 , Is , Mach , Mask , Mbit4 , Mk(6) , Mka , Plus , Sft(3) , Sftm , Shifts(4) , Slash , Star , Starl
   COMMON /machin/ Mach
   COMMON /xsrtcm/ Bimsk1 , Bimsk2 , Bimsk3 , Bimsk4 , Bimsk5 , Bimsk6 , Bkmsk1 , Bkmsk2 , Shifts , Icon1 , Icon2 , Star , Plus ,   &
                 & Dollar , Starl , Slash , Sftm , Mask , Blank , Mka , Is , Mbit4
!
! Dummy argument declarations
!
   INTEGER Sd
   INTEGER Bf(1)
   INTEGER shift
!
! Local variable declarations
!
   INTEGER andf , isft , khrfn1 , khrfn3 , khrfn4 , lshift , orf , rshift
   INTEGER blk , i1 , i2 , i3 , i4 , ihld , ii , j , n , n1 , n2 , n3
   LOGICAL dec
   EXTERNAL andf , lshift , orf , rshift , shift
!
! End of declarations
!
   EQUIVALENCE (Bk(1),Bkmsk1(2)) , (Mk(1),Bimsk1(1)) , (Sft(1),Shifts(2)) , (Blks,Bkmsk1(8)) , (Bkx,Bkmsk1(1))
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
   dec = Mach==5 .OR. Mach==6 .OR. Mach==21
   IF ( shift(Mk(3),Sft(1))/=0 ) THEN
!
!     RIGHT SHIFT REQUESTED
!
      blk = Bkx
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
      blk = Blks
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
   DO
      IF ( j/=4 .OR. Bf(i1)==Blks ) THEN
         IF ( j/=3 .OR. Bf(i1)==Blks .OR. Bf(i1)==Bkx ) THEN
            Bf(i1) = Bf(i2)
            Bf(i2) = blk
            IF ( Sd/=0 ) THEN
               n = n + 1
               Bf(i2) = Bf(i3)
               Bf(i3) = Bf(i4)
               Bf(i4) = blk
               IF ( n/=3 ) CYCLE
            ENDIF
            IF ( Bf(i1)==Blks ) RETURN
         ENDIF
      ENDIF
!
!     CHARACTER SHIFTS BETWEEN FIELDS
!
      n = 0
      IF ( j==3 ) GOTO 500
!
!     RIGHT
!
      ii = i1
      EXIT
   ENDDO
 100  IF ( Bf(ii)==Blks ) THEN
      Bf(ii) = Bkx
   ELSEIF ( Bf(ii)/=Bkx ) THEN
      DO
         IF ( .NOT.dec ) ihld = rshift(andf(Mk(3),Bf(ii)),1)
         IF ( dec ) ihld = khrfn4(rshift(khrfn4(khrfn1(Bkmsk2,1,Bf(ii),1)),1))
         IF ( ihld/=Icon1 ) THEN
            IF ( n==0 ) EXIT
            GOTO 300
         ELSE
            n = n + 1
            IF ( .NOT.dec ) Bf(ii) = lshift(Bf(ii),Sft(1))
            IF ( dec ) Bf(ii) = khrfn3(Bkmsk2,Bf(ii),1,1)
            IF ( n>=3 ) GOTO 300
         ENDIF
      ENDDO
   ENDIF
 200  ii = ii - 1
   IF ( ii/=0 ) GOTO 100
   n = 0
   GOTO 400
 300  n2 = 4 - n
   IF ( .NOT.dec ) Bf(ii) = orf(rshift(Bf(ii),Sft(n)),Bk(n2))
   IF ( dec ) Bf(ii) = khrfn3(Bk(n2),Bf(ii),n,0)
   n = 0
   GOTO 200
!
!     RIGHT
!
 400  IF ( dec ) THEN
      IF ( khrfn1(Mk(4),4,Bf(i1),4)==khrfn1(Mk(4),4,Blks,4) ) GOTO 600
      GOTO 700
   ELSE
      IF ( andf(Mk(4),Bf(i1))==andf(Mk(4),Blks) ) GOTO 600
      GOTO 700
   ENDIF
!
!     LEFT
!
 500  IF ( .NOT.dec ) ihld = rshift(andf(Mk(3),Bf(i1)),1)
   IF ( dec ) ihld = khrfn4(rshift(khrfn4(khrfn1(Bkmsk2,1,Bf(i1),1)),1))
   IF ( ihld/=Icon1 .AND. ihld/=Icon2 ) GOTO 700
 600  n = n + 1
   IF ( .NOT.dec ) Bf(i1) = shift(Bf(i1),Sft(1))
   IF ( dec ) Bf(i1) = khrfn3(Bkmsk2,Bf(i1),1,4-j)
   IF ( n>=3 ) GOTO 800
   IF ( j/=3 ) GOTO 400
   GOTO 500
 700  IF ( n==0 ) RETURN
 800  IF ( j==4 ) THEN
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
   IF ( .NOT.dec ) Bf(i1) = orf(andf(Mk(n1),Bf(i1)),andf(Mk(n2),isft(Bf(i2),Sft(n3),j)))
   IF ( dec ) Bf(i1) = khrfn3(Bf(i1),Bf(i2),n3,j-3)
   Bf(i1) = orf(Bf(i1),Bkmsk2)
   IF ( .NOT.dec ) Bf(i2) = orf(andf(Mk(n1),shift(Bf(i2),Sft(n))),Bk(n2))
   IF ( dec ) Bf(i2) = khrfn3(Bk(n2),Bf(i2),n,4-j)
   IF ( Sd==0 ) RETURN
!
   IF ( .NOT.dec ) Bf(i2) = orf(andf(Mk(n1),Bf(i2)),andf(Mk(n2),isft(Bf(i3),Sft(n3),j)))
   IF ( dec ) Bf(i2) = khrfn3(Bf(i2),Bf(i3),n3,j-3)
   Bf(i2) = orf(Bf(i2),Bkmsk2)
   IF ( Bf(i2)==blk ) RETURN
!
   IF ( .NOT.dec ) Bf(i3) = orf(andf(Mk(n1),shift(Bf(i3),Sft(n))),Bk(n2))
   IF ( dec ) Bf(i3) = khrfn3(Bk(n2),Bf(i3),n,4-j)
   IF ( .NOT.dec ) Bf(i3) = orf(andf(Mk(n1),Bf(i3)),andf(Mk(n2),isft(Bf(i4),Sft(n3),j)))
   IF ( dec ) Bf(i3) = khrfn3(Bf(i3),Bf(i4),n3,j-3)
   Bf(i3) = orf(Bf(i3),Bkmsk2)
   IF ( Bf(i3)==blk ) RETURN
!
   IF ( .NOT.dec ) Bf(i4) = orf(andf(Mk(n1),shift(Bf(i4),Sft(n))),Bk(n2))
   IF ( dec ) Bf(i4) = khrfn3(Bk(n2),Bf(i4),n,4-j)
END SUBROUTINE xfadj1

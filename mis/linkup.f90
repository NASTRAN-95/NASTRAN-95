
SUBROUTINE linkup(*,Name)
!
   IMPLICIT NONE
   INTEGER Ibot , Isn , Itop , Itype , Kind , Machx , Mask1 , Mask2 , Mask3 , Z(1)
   COMMON /lnklst/ Itop , Ibot , Isn , Kind , Itype , Mask1 , Mask2 , Mask3
   COMMON /machin/ Machx
   COMMON /zzzzzz/ Z
   INTEGER Name(2)
   INTEGER andf , complf , lshift , orf , rshift
   INTEGER ihash , ikind , itotal , k , l
   EXTERNAL andf , complf , lshift , orf , rshift
!
!     HASH INTO TABLE
!
   IF ( Machx==4 ) THEN
!
!     60-BIT MACHINE
!
      itotal = rshift(Name(1),18) + rshift(Name(2),18)
   ELSEIF ( Machx==5 .OR. Machx==6 .OR. Machx==8 .OR. Machx==9 .OR. Machx==10 .OR. Machx==11 .OR. Machx==16 .OR. Machx==17 .OR.     &
          & Machx==18 .OR. Machx==19 .OR. Machx==20 .OR. Machx==21 .OR. Machx==22 ) THEN
!
!     32-BIT MACHINES
!
      itotal = rshift(Name(1),1) + rshift(Name(2),1)
   ELSEIF ( Machx==12 .OR. Machx==14 .OR. Machx==15 ) THEN
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
   k = andf(Z(ihash),Mask1)
   IF ( k==0 ) THEN
!
!     NO HASH CHAIN FOUND - CREATE CHAIN
!
      Z(ihash) = Z(ihash) + Itop
      GOTO 200
   ENDIF
!
!     HASH CHAIN FOUND - CHECK PRESENCE OF NAME
!
 100  IF ( Z(k)==Name(1) .AND. Z(k+1)==Name(2) ) THEN
      ikind = rshift(andf(Z(k+3),Mask3),28)
      IF ( (ikind+1)/2==(Kind+1)/2 ) GOTO 300
   ENDIF
   l = andf(Z(k+3),Mask2)
   IF ( l==0 ) THEN
      Z(k+3) = Z(k+3) + lshift(Itop,14)
   ELSE
      k = rshift(l,14)
      GOTO 100
   ENDIF
!
!     NO ENTRY FOUND - CREATE ENTRY
!
 200  Z(Itop) = Name(1)
   Z(Itop+1) = Name(2)
   Z(Itop+2) = lshift(Itype,28)
   Z(Itop+3) = Z(Itop+3) + lshift(iabs(Kind),28)
   Itop = Itop + 4
   IF ( Itop>=Ibot ) RETURN 1
   IF ( Kind<0 ) RETURN
   k = Itop - 4
!
!     ADD STATEMENT NUMBER TO LIST
!
 300  l = andf(Z(k+2),Mask1)
   IF ( l/=0 ) THEN
!
!     CHAIN ENTRY ON LIST
!
      l = rshift(andf(Z(k+2),Mask2),14)
      Z(l) = andf(Z(l),complf(Mask2))
      Z(l) = orf(Z(l),lshift(Ibot,14))
   ELSE
!
!     LIST IS EMPTY - START LIST
!
      Z(k+2) = Z(k+2) + Ibot
   ENDIF
!
!     ADD ENTRY TO LIST
!
   Z(Ibot) = orf(lshift(Kind,28),Isn)
   Z(k+2) = andf(Z(k+2),complf(Mask2))
   Z(k+2) = Z(k+2) + lshift(Ibot,14)
   Ibot = Ibot - 1
   IF ( Itop>=Ibot ) RETURN 1
END SUBROUTINE linkup
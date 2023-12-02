!*==typint.f90 processed by SPAG 8.01RF 14:46  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE typint(X,Y,Xyd,Num,Field,Opt)
   IMPLICIT NONE
   USE C_PLTDAT
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: X
   REAL :: Y
   INTEGER :: Xyd
   INTEGER :: Num
   INTEGER :: Field
   INTEGER :: Opt
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: aster , minus
   INTEGER , DIMENSION(11) :: d
   INTEGER :: dir , i , j , n , nd
   REAL :: xx , xy , yy
   EXTERNAL tipe , type10
!
! End of declarations rewritten by SPAG
!
!
!     (X,Y) = STARTING OR ENDING POINT OF THE NUMBER TO BE TYPED (ALWAYS
!             LEFT-TO-RIGHT OR TOP-TO-BOTTOM).
!     XYD   = NO ACTION IF OPT IS ZERO
!           = (+/-)1 IF X = STARTING OR ENDING POINT OF THE NUMBER.
!           = (+/-)2 IF Y = STARTING OR ENDING POINT OF THE NUMBER.
!     NUM   = INTEGER NUMBER TO BE TYPED (AT MOST 10 DIGITS).
!     FIELD = NO ACTION IF OPT IS ZERO
!           = 1 IF THE NUMBER IS TO BE CENTERED AT (X,Y). IF XYD=1 OR 2,
!             THE NUMBER WILL BE TYPED IN THE X OR Y DIRECTION.
!           = 0 OR -1 IF THE NUMBER IS TO BE TYPED STARTING OR ENDING AT
!             (X,Y). IF FIELD = -1, FIELD WILL BE SET TO THE NUMBER OF
!             DIGITS PRINTED.
!     OPT   =-1 TO INITIATE  THE TYPING MODE.
!           =+1 TO TERMINATE THE TYPING MODE.
!           = 0 TO TYPE A LINE.
!
   DATA aster , minus/41 , 40/
!
   IF ( Opt==0 ) THEN
!
!     SEPARATE THE DIGITS OF THE NUMBER (MAXIMUM OF 10).
!
      nd = -1
      IF ( Num<0 ) THEN
         nd = 0
         d(1) = minus
      ENDIF
      n = iabs(Num)
      DO i = 1 , 10
         j = n/10**(10-i)
         IF ( j/=0 .OR. nd>0 ) THEN
            IF ( j>9 ) j = aster - 1
            IF ( nd<=0 ) nd = nd + 1
            nd = nd + 1
            d(nd) = j + 1
            n = n - j*10**(10-i)
         ENDIF
      ENDDO
      IF ( nd<=0 ) THEN
         nd = 1
         d(1) = 1
      ENDIF
!
      xx = X
      yy = Y
      IF ( Field>0 .AND. nd>1 ) THEN
!
!     THE TYPED NUMBER MUST BE CENTERED AT (X,Y).
!
         xy = nd/2
         IF ( nd/2==(nd+1)/2 ) xy = xy - .5
         dir = max0(iabs(Xyd),1)
         IF ( dir==1 ) xx = X - xy*Cntx
         IF ( dir==2 ) yy = Y - xy*Cnty
      ELSE
!
!     THE TYPED NUMBER IS NOT TO BE CENTERED AT (X,Y).
!
         dir = Xyd
         IF ( Field<0 ) Field = nd
      ENDIF
!
!     TYPE THE NUMBER.
!
      CALL type10(xx,yy,dir,d,nd,0)
   ELSE
      CALL tipe(0,0,0,0,0,Opt)
   ENDIF
!
END SUBROUTINE typint

!*==drwchr.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE drwchr(X,Y,Xyd,Chr,Nn,Opt)
   IMPLICIT NONE
   USE C_CHRDRW
   USE C_PLTDAT
!
! Dummy argument declarations rewritten by SPAG
!
   REAL :: X
   REAL :: Y
   INTEGER :: Xyd
   INTEGER , DIMENSION(1) :: Chr
   INTEGER :: Nn
   INTEGER :: Opt
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: d , i , j , k , l , n , n1 , n2
   INTEGER , SAVE :: lstchr
   REAL :: s
   REAL , DIMENSION(2,2) :: save , xy , xyc
   EXTERNAL line
!
! End of declarations rewritten by SPAG
!
!
!     (X,Y)  = STARTING OR ENDING POINT OF THE LINE TO BE TYPED (ALWAYS
!              LEFT-TO-RIGHT OR TOP-TO-BOTTOM)
!     XYD    = (+/-)1 IF X = STARTING OR ENDING POINT OF THE LINE
!            = (+/-)2 IF Y = STARTING OR ENDING POINT OF THE LINE
!     CHR    = CHARACTERS TO BE DRAWN
!     NN     = NUMBER OF CHARACTERS
!     OPT    = -1 TO INITIATE  THE TYPING MODE
!            = +1 TO TERMINATE THE TYPING MODE
!            =  0 TO TYPE A LINE
!     CSCALE = SCALE FOR CHARACTER SIZE (REAL)
!
   DATA lstchr/48/
!
   IF ( Opt==0 ) THEN
!
      n = Nn
      IF ( n<=0 ) n = 1
      d = max0(iabs(Xyd),1)
      s = Cntchr(d)
      IF ( Xyd==-1 .OR. Xyd==2 ) s = -s
      xyc(1,1) = 3.0*Cscale
      xyc(2,1) = 3.0*Cscale
      xy(1,1) = X - xyc(1,1)
      xy(2,1) = Y - xyc(2,1)
      xy(1,2) = xy(1,1)
      xy(2,2) = xy(2,1)
      DO i = 1 , 2
         save(i,1) = Reg(i,1)
         Reg(i,1) = amax1(-Edge(i),Reg(i,1)-xyc(i,1))
         save(i,2) = Reg(i,2)
         Reg(i,2) = amin1(Xymax(i)+Edge(i),Reg(i,2)+xyc(i,1))
      ENDDO
!
!     TYPE THE LINE.
!
      DO j = 1 , n
         xy(d,2) = xy(d,1) + s*float(j-1)
!
!     MAKE SURE EACH CHARACTER IS A VALID CHARACTER.
!
         i = j
         IF ( Xyd<0 ) i = n - j + 1
         k = Chr(i)
         IF ( Nn==0 .OR. k<lstchr ) THEN
            IF ( k<=Lstind ) THEN
               SPAG_Loop_2_1: DO
!
!     DRAW THE CHARACTER.
!
                  n1 = Chrind(k)
                  IF ( n1>0 ) THEN
                     DO
                        n2 = Chrind(k+1)
                        IF ( n2>0 ) THEN
!
                           n2 = n2 - 1
                           DO l = n1 , n2
                              DO i = 1 , 2
                                 xyc(i,1) = xyc(i,2)
                                 xyc(i,2) = xy(i,2) + Cscale*float(iabs(Xychr(i,l)))
                              ENDDO
                              IF ( l/=n1 .AND. Xychr(1,l)>=0 .AND. Xychr(2,l)>=0 ) CALL line(xyc(1,1),xyc(2,1),xyc(1,2),xyc(2,2),1, &
                                 & 0)
                           ENDDO
                           EXIT SPAG_Loop_2_1
                        ELSE
                           k = k + 1
                        ENDIF
                     ENDDO
                  ELSE
                     k = -n1
                  ENDIF
               ENDDO SPAG_Loop_2_1
            ENDIF
         ENDIF
      ENDDO
!
      DO i = 1 , 2
         Reg(i,1) = save(i,1)
         Reg(i,2) = save(i,2)
      ENDDO
   ELSE
      CALL line(0,0,0,0,0,Opt)
   ENDIF
!
END SUBROUTINE drwchr

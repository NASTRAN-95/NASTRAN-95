!*==upcase.f90 processed by SPAG 8.01RF 16:18  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE upcase(Byte,N)
   USE c_machin
   USE c_upcasx
   USE c_xechox
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   CHARACTER(1) , DIMENSION(1) :: Byte
   INTEGER :: N
!
! Local variable declarations rewritten by SPAG
!
   CHARACTER(1) , SAVE :: bk1 , ic , il , ip , la , lz
   INTEGER :: i , j
   CHARACTER(56) , DIMENSION(5) , SAVE :: kc
   CHARACTER(1) , DIMENSION(256) :: lc
   INTEGER , DIMENSION(20) , SAVE :: tab
!
! End of declarations rewritten by SPAG
!
!
!     THIS ROUTINE CHANGES ALL LOWER CASE CHARACTERS INTO UPPER CASE.
!     IT ALSO CONVERTS BCD INPUT CODE TO EBCDIC FOR IBM MACHINE
!
   !>>>>EQUIVALENCE (kc(1),lc(1))
!
!                     TAB = UPPER CASE 'A' TO LOWER CASE 'a' SPAN
!
   DATA tab/ + 32 , -64 , +32 , +3968 , +32 , +32 , +32 , +32 , +32 , +32 , +32 , +32 , +32 , +32 , +32 , +32 , +32 , +32 , +32 ,   &
      & +32/
   DATA bk1 , la , lz , il , ic/' ' , 'A' , 'Z' , '(' , ','/
   DATA ip/'%'/
!
!     TAB IS DECIMAL VALUE BETWEEN UPPER CASE 'A' AND LOWER CASE 'a'
!     TAB IS POSITIVE IF LOWER CASE 'a' COMES AFTER UPPER CASE 'A' IN
!     MACHINE ASCII CHARACTER SET; OTHERWISE TAB IS NEGATIVE.
!
!     THE FOLLOWING KC TABLE MUST BE PUNCHED IN EBCDIC CODE (FOR IBM
!     ONLY)                          =======    ===========
!
   DATA kc/'                                                        ' , '                   .)(+ +          $*)  -/         ,(%  ' ,&
       &'           =''''=  ABCDEFGHI       JKLMNOPQR        STUVWX' , 'YZ                       ABCDEFGHI       JKLMNOPQR      ' , &
       &'  STUVWXYZ      0123456789      WRITTEN BY G.CHAN/UNISYS'/
!
   IF ( machx==2 ) THEN
!
!     IBM MACHINE ONLY, WHICH USES EBCDIC CODE
!
      DO i = 1 , N
         j = ichar(Byte(i))
         Byte(i) = lc(j+1)
      ENDDO
!
!     THE % SIGN MAY BE CHANGED TO ( IN BCD-EBCDIC CONVERSION,
!     CHANGE IT BACK TO %
!
      IF ( ffflag/=1234 .OR. N<5 ) RETURN
      DO i = 5 , N
         IF ( Byte(i)==il .AND. Byte(i+1)==il .AND. (Byte(i-1)==ic .OR. Byte(i-1)==bk1) ) Byte(i) = ip
      ENDDO
      RETURN
   ELSEIF ( .NOT.(flag) ) THEN
      flag = .TRUE.
      id = tab(machx)
      ia = ichar(la) + id
      iz = ichar(lz) + id
   ENDIF
!
   DO i = 1 , N
      IF ( Byte(i)/=bk1 ) THEN
         j = ichar(Byte(i))
         IF ( j>=ia .AND. j<=iz ) Byte(i) = char(j-id)
      ENDIF
   ENDDO
END SUBROUTINE upcase

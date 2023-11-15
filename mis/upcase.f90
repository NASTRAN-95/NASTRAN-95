
SUBROUTINE upcase(Byte,N)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Ffflag , Ia , Id , Iz , Machx
   LOGICAL Flag
   COMMON /machin/ Machx
   COMMON /upcasx/ Flag , Id , Ia , Iz
   COMMON /xechox/ Ffflag
!
! Dummy argument declarations
!
   INTEGER N
   CHARACTER*1 Byte(1)
!
! Local variable declarations
!
   CHARACTER*1 bk1 , ic , il , ip , la , lc(256) , lz
   INTEGER i , j , tab(20)
   CHARACTER*56 kc(5)
!
! End of declarations
!
!
!     THIS ROUTINE CHANGES ALL LOWER CASE CHARACTERS INTO UPPER CASE.
!     IT ALSO CONVERTS BCD INPUT CODE TO EBCDIC FOR IBM MACHINE
!
   EQUIVALENCE (kc(1),lc(1))
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
   IF ( Machx==2 ) THEN
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
      IF ( Ffflag/=1234 .OR. N<5 ) RETURN
      DO i = 5 , N
         IF ( Byte(i)==il .AND. Byte(i+1)==il .AND. (Byte(i-1)==ic .OR. Byte(i-1)==bk1) ) Byte(i) = ip
      ENDDO
      GOTO 99999
   ELSEIF ( .NOT.(Flag) ) THEN
      Flag = .TRUE.
      Id = tab(Machx)
      Ia = ichar(la) + Id
      Iz = ichar(lz) + Id
   ENDIF
!
   DO i = 1 , N
      IF ( Byte(i)/=bk1 ) THEN
         j = ichar(Byte(i))
         IF ( j>=Ia .AND. j<=Iz ) Byte(i) = char(j-Id)
      ENDIF
   ENDDO
   RETURN
99999 END SUBROUTINE upcase

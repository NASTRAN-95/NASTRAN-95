
SUBROUTINE ruler(Rule,Icp,Zrct,Onct,List,N,Buff,Iopt)
   IMPLICIT NONE
   REAL A1(4) , Two1(32)
   INTEGER Eol , L
   COMMON /two   / Two1
   COMMON /zntpkx/ A1 , L , Eol
   INTEGER Iopt , N , Onct , Rule , Zrct
   REAL Buff(1)
   INTEGER Icp(1) , List(1)
   REAL a , r
   INTEGER i , is , j , j1 , k , m , n1 , namcp , oct , zct
   INTEGER orf
   EXTERNAL orf
!
!     DETERMINES STRING OF ZEROS AND ONES IN LIST BY APPLYING RULE TO
!     CP.
!
!
!     PICK UP PARAMETERS
!
   Eol = 0
   r = Rule
   namcp = Icp(1)
   zct = 0
   oct = 0
   ASSIGN 300 TO is
   IF ( r>=0.0 ) ASSIGN 250 TO is
   r = abs(r)
   L = 0
   j1 = 0
   m = 0
   n1 = N
   IF ( namcp/=0 ) THEN
      CALL gopen(namcp,Buff,0)
      CALL intpk(*100,namcp,0,1,0)
      GOTO 200
   ENDIF
 100  m = n1
   Eol = 1
 200  DO i = 1 , n1
      j = (i+31)/32
      IF ( m<i ) THEN
         IF ( Eol==0 ) THEN
            CALL zntpki
         ELSE
            L = n1
            A1(1) = 0.0
         ENDIF
      ENDIF
      IF ( L==i ) THEN
         a = A1(1)
      ELSE
         m = L
         a = 0.0
      ENDIF
      IF ( Iopt/=1 .AND. j>j1 ) THEN
         j1 = j
         List(j) = 0
      ENDIF
      GOTO is
 250  IF ( a==r ) GOTO 400
      GOTO 350
 300  IF ( a<r ) THEN
      ELSEIF ( a==r ) THEN
         GOTO 400
      ELSE
         CYCLE
      ENDIF
 350  oct = oct + 1
      IF ( Iopt==1 ) THEN
         List(i) = oct
      ELSE
         k = i - ((i-1)/32)*32
         List(j) = orf(List(j),Two1(k))
      ENDIF
      CYCLE
 400  zct = zct + 1
      IF ( Iopt/=0 ) List(i) = -zct
   ENDDO
   Zrct = zct
   Onct = oct
   IF ( namcp/=0 ) CALL close(namcp,1)
END SUBROUTINE ruler
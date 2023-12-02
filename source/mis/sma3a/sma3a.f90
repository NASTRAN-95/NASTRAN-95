!*==sma3a.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE sma3a(Mcbcur)
   IMPLICIT NONE
   USE c_blank
   USE c_genely
   USE c_system
   USE c_zblpkx
   USE c_zzzzzz
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(7) :: Mcbcur
!
! Local variable declarations rewritten by SPAG
!
   REAL*8 :: det
   REAL*8 , DIMENSION(1) :: dq
   INTEGER :: i , ibuff , icol , idiff , idummy , ii , iloop , ind , ip , ippi , ising , ismall , izk , izrow , izscol , j , jcol , &
            & jrow , jud , jui , k , kk , l , lim , limjud , limjui , limk , low , m , max , mpn , msq , n , nsq , small
   INTEGER , DIMENSION(1) :: iq
   INTEGER , DIMENSION(7) :: mcb
   INTEGER , DIMENSION(2) , SAVE :: name
   LOGICAL :: zonly
!
! End of declarations rewritten by SPAG
!
!
! Dummy argument declarations rewritten by SPAG
!
!
! Local variable declarations rewritten by SPAG
!
!
! End of declarations rewritten by SPAG
!
!*****
! THIS ROUTINE BUILDS A GENERAL ELEMENT MATRIX (DOUBLE PRECISION AND
! SYMMETRIC) OF SIZE LUSET X LUSET.  MCBCUR IS THE MATRIX CONTROL BLOCK
! FOR THIS MATRIX.
!*****
!
!
!
   !>>>>EQUIVALENCE (Iq(1),Dq(1),Q(1)) , (Ibuff3(2),M) , (Ibuff3(3),N)
!
   DATA name(1)/4HSMA3/ , name(2)/4HA   /
!
! MAKE THE ARGUMENT A LOCAL VARIABLE
!
   DO i = 1 , 7
      mcb(i) = Mcbcur(i)
   ENDDO
!
! READ THE UI SET OF SCALAR INDEX NUMBERS INTO OPEN CORE.
!
   CALL fread(ifgei,iq(iui+1),m,0)
!
! IUD POINTS TO THE ZEROTH LOCATION OF THE UD ARRAY.
!
   iud = iui + m
   left = left - m
!
! SET UP ARITHMETIC CONSTANTS.
!
   mpn = m + n
   msq = m**2
   nsq = n**2
   zonly = .FALSE.
   IF ( n==0 ) zonly = .TRUE.
   IF ( .NOT.(zonly) ) THEN
!
! SINCE N .NE. 0, THE UD SET EXISTS.  READ IT INTO CORE.
!
      CALL fread(ifgei,iq(iud+1),n,0)
      left = left - n
   ENDIF
!
! BUILD THE ARRAY IQ(IP+1),IQ(IP+2),...,IQ(IP+MPN) SUCH THAT
! IQ(IP+K) = L IMPLIES IQ(IUI+L) IS THE K TH SMALLEST NUMBER OF THE
! SET OF NUMBERS IQ(IUI+1),...,IQ(IUD+N)
!
   ip = iui + mpn
   k = ip
   limk = ip + mpn
   low = iui + 2
   lim = iui + mpn
   DO
      small = iq(iui+1)
      ismall = iui + 1
      DO j = low , lim
         IF ( iq(j)<small ) THEN
            small = iq(j)
            ismall = j
         ENDIF
      ENDDO
      k = k + 1
      idiff = ismall - iui
      iq(k) = idiff
      iq(idiff) = iq(idiff) + luset
      IF ( k>=limk ) THEN
         low = iui + 1
         DO i = low , lim
            IF ( iq(i)<=luset ) CALL mesage(-30,28,5)
            iq(i) = iq(i) - luset
         ENDDO
!
! READ INDICATOR OF Z OR K MATRIX
!
         CALL fread(ifgei,izk,1,0)
!
! SET UP POINTERS TO THE ZEROTH LOCATION OF THE DOUBLE PRECISION ARRAYS
!       -1
! K  ORZ  AND S
!  E    E      E
!
         izi = (iui+2*mpn-1)/2 + 2
         is = izi + msq
!
! READ IN THE M**2 SINGLE PRECISION ELEMENTS OF THE SYMMETRIC Z OR K
! INTO A TEMPORARY BUFFER BEGINNING AT Q(IBUFF)
!
         ibuff = iui + 2*(mpn+msq)
!
! IF ALL OF Z OR K CANNOT FIT INTO THIS BUFFER, READ BLOCKS OF M WORDS
!
         IF ( ibuff+msq>left ) THEN
!
! READ Z OR K INTO THE BUFFER M WORDS AT A TIME AND STORE M WORDS
! AT A TIME
!
            ind = neor
            DO k = 1 , m
               IF ( k==m .AND. zonly ) ind = eor
               CALL fread(ifgei,q(ibuff+1),m,ind)
               i = izi + (k-1)*m
               j = ibuff
               lim = i + m
               DO
                  i = i + 1
                  IF ( i>lim ) EXIT
                  j = j + 1
                  dq(i) = q(j)
               ENDDO
            ENDDO
         ELSE
            ind = neor
            IF ( zonly ) ind = eor
            CALL fread(ifgei,iq(ibuff+1),msq,ind)
!
! STORE THE SINGLE PRECISION MATRIX IN ITS DOUBLE PRECISION LOCATION.
!
            lim = izi + msq
            i = izi
            j = ibuff
            DO
               i = i + 1
               IF ( i>lim ) EXIT
               j = j + 1
               dq(i) = q(j)
            ENDDO
         ENDIF
!
! IF K IS INPUT DO NOT COMPUTE INVERSE
!
         IF ( izk/=2 ) THEN
!*****
! COMPUTE THE INVERSE OF Z
!                        E
!*****
!
! THE 4TH ARGUMENT OF INVERD IS A DUMMY D.P. ARGUMENT WHILE 3 * M
! WORDS OF WORKING STORAGE ARE NEEDED FOR THE 8TH ARGUMENT OF SUBROUTINE
! INVERD.  SUBROUTINE INVERD WILL RETURN Z  INVERSE AT DQ(IZI+1)
!                                         E
!
            ibuff = iui + 2*(mpn+msq) + 5
            ii = ibuff + 2*m
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
            ising = -1
            CALL inverd(m,dq(izi+1),m,iq(ibuff+1),0,det,ising,iq(ii+1))
!
! ISING = 2 IMPLIES A SINGULAR Z
!                               E
!
            IF ( ising==2 ) CALL mesage(-5,idummy,name)
         ENDIF
!
! READ IN THE M*N ELEMENTS OF THE M X N  S MATRIX IF N .GT. 0.
! THIS MATRIX IS SINGLE PRECISION AND ROW STORED.
!
         IF ( .NOT.(zonly) ) THEN
            ibuff = mpn + 2*msq + 2*m*n + 5
            CALL fread(ifgei,q(ibuff+1),m*n,1)
!
! STORE THE S  MATRIX AT DQ(IS+1) MAKING S  DOUBLE PRECISION
!            E                            E
!
            low = is + 1
            lim = is + m*n
            j = ibuff
            DO i = low , lim
               j = j + 1
               dq(i) = q(j)
            ENDDO
!                  -1
! COMPUTE K S  OR Z  S AND STORE AT DQ(IZIS+1)
!          E E     E  E
!
            izis = is + m*n
            CALL gmmatd(dq(izi+1),m,m,0,dq(is+1),m,n,0,dq(izis+1))
!
!          T        T -1
! COMPUTE S K S OR S Z  S AND STORE AT DQ(ISTZIS+1)
!          E E E    E E  E
!
            istzis = izis + m*n
            CALL gmmatd(dq(is+1),m,n,1,dq(izis+1),m,n,0,dq(istzis+1))
!
!              -1
! SET K S  OR Z  S  NEGATIVE
!      E E     E  E
!
            low = izis + 1
            lim = izis + m*n
            DO i = low , lim
               dq(i) = -dq(i)
            ENDDO
         ENDIF
!*****
! AT THIS POINT ALL MATRICES HAVE BEEN COMPUTED
!*****
!
! INITIALIZE FOR OUTPUT ONTO THE FILE
!
         izrow = 1
         izscol = 1
         icol = 1
         limjui = iui + m
         limjud = iui + mpn
         jui = iui + 1
         jud = iud + 1
         EXIT
      ENDIF
   ENDDO
!******
! BEGIN OUTPUT LOOP
!******
 100  iloop = 0
   IF ( .NOT.(zonly) ) THEN
      IF ( iq(jui)<iq(jud) ) THEN
      ELSEIF ( iq(jui)==iq(jud) ) THEN
!
! FATAL ERROR MESSAGES
!
         CALL mesage(-30,28,1)
         CALL mesage(-30,28,2)
         CALL mesage(-30,28,3)
         CALL mesage(-30,28,4)
         GOTO 99999
      ELSE
         GOTO 400
      ENDIF
   ENDIF
!
! AT THIS POINT, WRITE OUT COLUMN(S) CORRESPONDING TO THE UI SET.
!
 200  IF ( iq(jui)<icol ) THEN
!
! A TRANSFER TO STATEMENT NO. 1115 WILL BE MADE IF THE MAXIMUM OF THE
! UD SET IS LESS THAN THE MINIMUM OF THE UI SET AND THE COLUMNS
! CORRESPONDING TO THE UD SET HAVE BEEN OUTPUT.
!
      IF ( iloop==1 .OR. zonly ) THEN
         CALL mesage(-30,28,2)
         CALL mesage(-30,28,3)
         CALL mesage(-30,28,4)
         GOTO 99999
      ELSE
         iloop = 1
         GOTO 400
      ENDIF
   ELSEIF ( iq(jui)/=icol ) THEN
!
! SINCE IQ(JUI) .GT. ICOL, IQ(JUI) - ICOL COLUMNS OF ZERO VECTORS MUST
! BE OUTPUT.
!
      lim = iq(jui) - icol
      DO i = 1 , lim
         CALL bldpk(2,iprec,mcb(1),0,0)
         CALL bldpkn(mcb(1),0,mcb)
      ENDDO
   ENDIF
!
! INITIALIZE FOR THE OUTPUT OF THE CURRENT COLUMN BY CALLING BLDPK
!
   CALL bldpk(2,iprec,mcb(1),0,0)
   DO i = 1 , mpn
      ippi = ip + i
      IF ( iq(ippi)>m ) THEN
!
! HERE WE ARE DEALING WITH A MEMBER OF THE UD SET.  HENCE AN ELEMENT OF
!                -1
! THE -K S  OR -Z  S  MATRIX MUST BE OUTPUT
!       E E      E  E
!
         jrow = izrow
         jcol = iq(ippi) - m
         k = (jrow-1)*n + jcol + izis
      ELSE
!
! SINCE IQ(IPPI).LE.M,OUTPUT AN ELEMENT OF K OR Z INVERSE
!
         jrow = izrow
         jcol = iq(ippi)
         k = (jrow-1)*m + jcol + izi
      ENDIF
!
! FILL ZBLPKI COMMON BLOCK
!
      kk = iq(ippi)
      index = iq(kk)
      dpword = dq(k)
      IF ( dpword/=0.0D0 ) CALL zblpki
   ENDDO
!
! THE CURRENT COLUMN IS COMPLETE.  CALL BLDPKN TO WRAP UP.
!
   CALL bldpkn(mcb(1),0,mcb)
   izrow = izrow + 1
   icol = iq(jui) + 1
   jui = jui + 1
   IF ( jui>limjui ) jui = limjui
 300  IF ( izrow<=m .OR. izscol<=n ) GOTO 100
!
! DETERMINE IF ZERO COLUMNS ARE TO BE OUTPUT.
!
   k = iui + m
   l = iud + n
   max = iq(k)
   IF ( iq(l)>max ) max = iq(l)
   lim = max - luset
   IF ( lim<0 ) THEN
!
! OUTPUT LIM ZERO COLUMNS
!
      lim = iabs(lim)
      DO i = 1 , lim
         CALL bldpk(2,iprec,mcb(1),0,0)
         CALL bldpkn(mcb(1),0,mcb)
      ENDDO
   ELSEIF ( lim/=0 ) THEN
      CALL mesage(-30,28,4)
      GOTO 99999
   ENDIF
   DO i = 1 , 7
      Mcbcur(i) = mcb(i)
   ENDDO
   RETURN
!
! AT THIS POINT WRITE OUT A COLUMN(S) USING THE UD SET.
!
 400  IF ( iq(jud)<icol ) THEN
!
! A TRANSFER TO STATEMENT NO. 1185 WILL BE MADE IF THE MAXIMUM OF THE
! UI SET IS LESS THAN THE MINIMUM OF THE UD SET AND THE COLUMNS
! CORRESPONDING TO THE UI SET HAVE BEEN OUTPUT.
!
      IF ( iloop==1 ) THEN
         CALL mesage(-30,28,3)
         CALL mesage(-30,28,4)
         GOTO 99999
      ELSE
         iloop = 1
         GOTO 200
      ENDIF
   ELSEIF ( iq(jud)/=icol ) THEN
!
! WRITE ZERO COLUMN(S).
!
      lim = iq(jud) - icol
      DO i = 1 , lim
         CALL bldpk(2,iprec,mcb(1),0,0)
         CALL bldpkn(mcb(1),0,mcb)
      ENDDO
   ENDIF
   CALL bldpk(2,iprec,mcb(1),0,0)
!
! OUTPUT A COLUMN WHOSE SIL NO. IS A MEMBER OF THE UD SET.
!
   DO i = 1 , mpn
      ippi = ip + i
      IF ( iq(ippi)>m ) THEN
!
!                       T         T -1
! OUTPUT AN ELEMENT OF S K S  OR S Z  S
!                       E E E     E E  E
!
         jrow = iq(ippi) - m
         jcol = izscol
         k = (jrow-1)*n + jcol + istzis
      ELSE
!
!                                           -1
! SINCE IQ(IPPI).LE.M,AN ELEMENT OF -KS OR -Z  S MUST BE OUTPUT
!
         jrow = iq(ippi)
         jcol = izscol
         k = (jrow-1)*n + jcol + izis
      ENDIF
!
! SET UP PARAMETERS IN ZBLPKI COMMON BLOCK
!
      kk = iq(ippi)
      index = iq(kk)
      dpword = dq(k)
      IF ( dpword/=0.0D0 ) CALL zblpki
   ENDDO
!
! WRAP UP THIS COLUMN.
!
   CALL bldpkn(mcb(1),0,mcb)
   izscol = izscol + 1
   icol = iq(jud) + 1
   jud = jud + 1
   IF ( jud>limjud ) jud = limjud
   GOTO 300
99999 END SUBROUTINE sma3a

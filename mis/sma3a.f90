
SUBROUTINE sma3a(Mcbcur)
   IMPLICIT NONE
!
! COMMON variable declarations
!
   INTEGER Clsnrw , Clsrw , Eor , Ibuff3(3) , Ifa , Ifb , Ifc , Ifd , Ife , Iff , Ifgei , Ifkggx , Ifout , Index , Inrw , Iprec ,   &
         & Iq(1) , Is , Istzis , Isys , Iud , Iui , Izi , Izis , Left , Luset , M , Mcba(7) , Mcbb(7) , Mcbc(7) , Mcbd(7) , Mcbe(7) &
         & , Mcbf(7) , Mcbkgg(7) , N , Neor , Ngenel , Noecpt , Outrw
   DOUBLE PRECISION Dpword , Dq(1)
   REAL Dum2(2) , Dummy(53) , Q(1)
   COMMON /blank / Luset , Ngenel , Noecpt
   COMMON /genely/ Ifgei , Ifkggx , Ifout , Ifa , Ifb , Ifc , Ifd , Ife , Iff , Inrw , Outrw , Clsrw , Clsnrw , Eor , Neor , Mcba , &
                 & Mcbb , Mcbc , Mcbd , Mcbe , Mcbf , Mcbkgg , Iui , Iud , Izi , Is , Izis , Istzis , Ibuff3 , Left
   COMMON /system/ Isys , Dummy , Iprec
   COMMON /zblpkx/ Dpword , Dum2 , Index
   COMMON /zzzzzz/ Q
!
! Dummy argument declarations
!
   INTEGER Mcbcur(7)
!
! Local variable declarations
!
   DOUBLE PRECISION det
   INTEGER i , ibuff , icol , idiff , idummy , ii , iloop , ind , ip , ippi , ising , ismall , izk , izrow , izscol , j , jcol ,    &
         & jrow , jud , jui , k , kk , l , lim , limjud , limjui , limk , low , max , mcb(7) , mpn , msq , name(2) , nsq , small
   LOGICAL zonly
!
! End of declarations
!
!*****
! THIS ROUTINE BUILDS A GENERAL ELEMENT MATRIX (DOUBLE PRECISION AND
! SYMMETRIC) OF SIZE LUSET X LUSET.  MCBCUR IS THE MATRIX CONTROL BLOCK
! FOR THIS MATRIX.
!*****
!
!
!
   EQUIVALENCE (Iq(1),Dq(1),Q(1)) , (Ibuff3(2),M) , (Ibuff3(3),N)
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
   CALL fread(Ifgei,Iq(Iui+1),M,0)
!
! IUD POINTS TO THE ZEROTH LOCATION OF THE UD ARRAY.
!
   Iud = Iui + M
   Left = Left - M
!
! SET UP ARITHMETIC CONSTANTS.
!
   mpn = M + N
   msq = M**2
   nsq = N**2
   zonly = .FALSE.
   IF ( N==0 ) zonly = .TRUE.
   IF ( .NOT.(zonly) ) THEN
!
! SINCE N .NE. 0, THE UD SET EXISTS.  READ IT INTO CORE.
!
      CALL fread(Ifgei,Iq(Iud+1),N,0)
      Left = Left - N
   ENDIF
!
! BUILD THE ARRAY IQ(IP+1),IQ(IP+2),...,IQ(IP+MPN) SUCH THAT
! IQ(IP+K) = L IMPLIES IQ(IUI+L) IS THE K TH SMALLEST NUMBER OF THE
! SET OF NUMBERS IQ(IUI+1),...,IQ(IUD+N)
!
   ip = Iui + mpn
   k = ip
   limk = ip + mpn
   low = Iui + 2
   lim = Iui + mpn
   DO
      small = Iq(Iui+1)
      ismall = Iui + 1
      DO j = low , lim
         IF ( Iq(j)<small ) THEN
            small = Iq(j)
            ismall = j
         ENDIF
      ENDDO
      k = k + 1
      idiff = ismall - Iui
      Iq(k) = idiff
      Iq(idiff) = Iq(idiff) + Luset
      IF ( k>=limk ) THEN
         low = Iui + 1
         DO i = low , lim
            IF ( Iq(i)<=Luset ) CALL mesage(-30,28,5)
            Iq(i) = Iq(i) - Luset
         ENDDO
!
! READ INDICATOR OF Z OR K MATRIX
!
         CALL fread(Ifgei,izk,1,0)
!
! SET UP POINTERS TO THE ZEROTH LOCATION OF THE DOUBLE PRECISION ARRAYS
!       -1
! K  ORZ  AND S
!  E    E      E
!
         Izi = (Iui+2*mpn-1)/2 + 2
         Is = Izi + msq
!
! READ IN THE M**2 SINGLE PRECISION ELEMENTS OF THE SYMMETRIC Z OR K
! INTO A TEMPORARY BUFFER BEGINNING AT Q(IBUFF)
!
         ibuff = Iui + 2*(mpn+msq)
!
! IF ALL OF Z OR K CANNOT FIT INTO THIS BUFFER, READ BLOCKS OF M WORDS
!
         IF ( ibuff+msq>Left ) THEN
!
! READ Z OR K INTO THE BUFFER M WORDS AT A TIME AND STORE M WORDS
! AT A TIME
!
            ind = Neor
            DO k = 1 , M
               IF ( k==M .AND. zonly ) ind = Eor
               CALL fread(Ifgei,Q(ibuff+1),M,ind)
               i = Izi + (k-1)*M
               j = ibuff
               lim = i + M
               DO
                  i = i + 1
                  IF ( i>lim ) EXIT
                  j = j + 1
                  Dq(i) = Q(j)
               ENDDO
            ENDDO
         ELSE
            ind = Neor
            IF ( zonly ) ind = Eor
            CALL fread(Ifgei,Iq(ibuff+1),msq,ind)
!
! STORE THE SINGLE PRECISION MATRIX IN ITS DOUBLE PRECISION LOCATION.
!
            lim = Izi + msq
            i = Izi
            j = ibuff
            DO
               i = i + 1
               IF ( i>lim ) EXIT
               j = j + 1
               Dq(i) = Q(j)
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
            ibuff = Iui + 2*(mpn+msq) + 5
            ii = ibuff + 2*M
!     NO NEED TO COMPUTE DETERMINANT SINCE IT IS NOT USED SUBSEQUENTLY.
            ising = -1
            CALL inverd(M,Dq(Izi+1),M,Iq(ibuff+1),0,det,ising,Iq(ii+1))
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
            ibuff = mpn + 2*msq + 2*M*N + 5
            CALL fread(Ifgei,Q(ibuff+1),M*N,1)
!
! STORE THE S  MATRIX AT DQ(IS+1) MAKING S  DOUBLE PRECISION
!            E                            E
!
            low = Is + 1
            lim = Is + M*N
            j = ibuff
            DO i = low , lim
               j = j + 1
               Dq(i) = Q(j)
            ENDDO
!                  -1
! COMPUTE K S  OR Z  S AND STORE AT DQ(IZIS+1)
!          E E     E  E
!
            Izis = Is + M*N
            CALL gmmatd(Dq(Izi+1),M,M,0,Dq(Is+1),M,N,0,Dq(Izis+1))
!
!          T        T -1
! COMPUTE S K S OR S Z  S AND STORE AT DQ(ISTZIS+1)
!          E E E    E E  E
!
            Istzis = Izis + M*N
            CALL gmmatd(Dq(Is+1),M,N,1,Dq(Izis+1),M,N,0,Dq(Istzis+1))
!
!              -1
! SET K S  OR Z  S  NEGATIVE
!      E E     E  E
!
            low = Izis + 1
            lim = Izis + M*N
            DO i = low , lim
               Dq(i) = -Dq(i)
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
         limjui = Iui + M
         limjud = Iui + mpn
         jui = Iui + 1
         jud = Iud + 1
         EXIT
      ENDIF
   ENDDO
!******
! BEGIN OUTPUT LOOP
!******
 100  iloop = 0
   IF ( .NOT.(zonly) ) THEN
      IF ( Iq(jui)<Iq(jud) ) THEN
      ELSEIF ( Iq(jui)==Iq(jud) ) THEN
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
 200  IF ( Iq(jui)<icol ) THEN
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
   ELSEIF ( Iq(jui)/=icol ) THEN
!
! SINCE IQ(JUI) .GT. ICOL, IQ(JUI) - ICOL COLUMNS OF ZERO VECTORS MUST
! BE OUTPUT.
!
      lim = Iq(jui) - icol
      DO i = 1 , lim
         CALL bldpk(2,Iprec,mcb(1),0,0)
         CALL bldpkn(mcb(1),0,mcb)
      ENDDO
   ENDIF
!
! INITIALIZE FOR THE OUTPUT OF THE CURRENT COLUMN BY CALLING BLDPK
!
   CALL bldpk(2,Iprec,mcb(1),0,0)
   DO i = 1 , mpn
      ippi = ip + i
      IF ( Iq(ippi)>M ) THEN
!
! HERE WE ARE DEALING WITH A MEMBER OF THE UD SET.  HENCE AN ELEMENT OF
!                -1
! THE -K S  OR -Z  S  MATRIX MUST BE OUTPUT
!       E E      E  E
!
         jrow = izrow
         jcol = Iq(ippi) - M
         k = (jrow-1)*N + jcol + Izis
      ELSE
!
! SINCE IQ(IPPI).LE.M,OUTPUT AN ELEMENT OF K OR Z INVERSE
!
         jrow = izrow
         jcol = Iq(ippi)
         k = (jrow-1)*M + jcol + Izi
      ENDIF
!
! FILL ZBLPKI COMMON BLOCK
!
      kk = Iq(ippi)
      Index = Iq(kk)
      Dpword = Dq(k)
      IF ( Dpword/=0.0D0 ) CALL zblpki
   ENDDO
!
! THE CURRENT COLUMN IS COMPLETE.  CALL BLDPKN TO WRAP UP.
!
   CALL bldpkn(mcb(1),0,mcb)
   izrow = izrow + 1
   icol = Iq(jui) + 1
   jui = jui + 1
   IF ( jui>limjui ) jui = limjui
 300  IF ( izrow<=M .OR. izscol<=N ) GOTO 100
!
! DETERMINE IF ZERO COLUMNS ARE TO BE OUTPUT.
!
   k = Iui + M
   l = Iud + N
   max = Iq(k)
   IF ( Iq(l)>max ) max = Iq(l)
   lim = max - Luset
   IF ( lim<0 ) THEN
!
! OUTPUT LIM ZERO COLUMNS
!
      lim = iabs(lim)
      DO i = 1 , lim
         CALL bldpk(2,Iprec,mcb(1),0,0)
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
 400  IF ( Iq(jud)<icol ) THEN
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
   ELSEIF ( Iq(jud)/=icol ) THEN
!
! WRITE ZERO COLUMN(S).
!
      lim = Iq(jud) - icol
      DO i = 1 , lim
         CALL bldpk(2,Iprec,mcb(1),0,0)
         CALL bldpkn(mcb(1),0,mcb)
      ENDDO
   ENDIF
   CALL bldpk(2,Iprec,mcb(1),0,0)
!
! OUTPUT A COLUMN WHOSE SIL NO. IS A MEMBER OF THE UD SET.
!
   DO i = 1 , mpn
      ippi = ip + i
      IF ( Iq(ippi)>M ) THEN
!
!                       T         T -1
! OUTPUT AN ELEMENT OF S K S  OR S Z  S
!                       E E E     E E  E
!
         jrow = Iq(ippi) - M
         jcol = izscol
         k = (jrow-1)*N + jcol + Istzis
      ELSE
!
!                                           -1
! SINCE IQ(IPPI).LE.M,AN ELEMENT OF -KS OR -Z  S MUST BE OUTPUT
!
         jrow = Iq(ippi)
         jcol = izscol
         k = (jrow-1)*N + jcol + Izis
      ENDIF
!
! SET UP PARAMETERS IN ZBLPKI COMMON BLOCK
!
      kk = Iq(ippi)
      Index = Iq(kk)
      Dpword = Dq(k)
      IF ( Dpword/=0.0D0 ) CALL zblpki
   ENDDO
!
! WRAP UP THIS COLUMN.
!
   CALL bldpkn(mcb(1),0,mcb)
   izscol = izscol + 1
   icol = Iq(jud) + 1
   jud = jud + 1
   IF ( jud>limjud ) jud = limjud
   GOTO 300
99999 END SUBROUTINE sma3a

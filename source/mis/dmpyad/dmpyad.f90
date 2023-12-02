!*==dmpyad.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE dmpyad
!
!     DMPYAD IS THE DMAP DRIVER FOR MATRIX MULTIPLICATION.
!
!     COMMENTS FROM G.CHAN/UNISYS ABOUT PREC1 IN /MPYADX/     1/91
!     ACCORDING TO THE USER'S MANUAL ON P. 3.5-18
!       PREC1 = 0, PERFORM ARITHMETIC IN D.P. IF A,B OR C IS IN D.P.
!             = 1, PERFORM ARITHMETIC IN S.P.
!             = 2, PERFORM ARITHMETIC IN D.P.
!     HOWEVER, THE CODE BELOW ALWAYS SETS
!       PREC1 TO 2, IF ANY OF THE A,B OR C IS IN D.P. AND 1 OTHERWISE
!       IN SUBROUTINE MPYAD, PREC1 IS ALWAYS SET TO 1 FOR CDC MACHINE
!
!     IF ITYPE IN /BLANK/ IS 1 OR 3, MPYAD PRODUCT WILL BE OUPUT IN S.P.
!     AND IN D.P. OF IT IS 2 OR 4
!     IF ITYPE IS 0, MPYAD PRODUCT WILL BE IN S.P. ONLY IF ALL A, B, AND
!     C MATRICES ARE IN S.P. OTHERWISE, THE PRODUCT WILL BE IN D.P.
!
   USE c_blank
   USE c_dmpyx
   USE c_mpyadx
   USE c_saddx
   USE c_system
   USE c_xmssg
   USE c_zzzzzz
   IMPLICIT NONE
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(1) :: alp , bet
   INTEGER , SAVE :: diag , filea , fileb , filec , filed , ident , rect , scrtch , square , symm
   INTEGER , DIMENSION(3) , SAVE :: dosi , refus
   INTEGER :: i , j , jj , k , kprec , ltype , outpt , prec , typa , typb
   INTEGER , DIMENSION(2) , SAVE :: name
   INTEGER , DIMENSION(4) :: p , q , r
   INTEGER , DIMENSION(1) :: zz , zzz
   EXTERNAL dmpy , korsz , mesage , mpyad , rdtrl , sadd , wrttrl
!
! End of declarations rewritten by SPAG
!
   !>>>>EQUIVALENCE (Zz(1),Z(1))
   !>>>>EQUIVALENCE (Zzz(1),Z(1))
   !>>>>EQUIVALENCE (Ksystm(55),Kprec) , (Ksystm(2),Outpt)
   !>>>>EQUIVALENCE (Mcbs(1),P(1)) , (Mcbs(8),Typa) , (Mcbs(9),Alp(1)) , (Mcbs(13),Q(1)) , (Mcbs(20),Typb) , (Mcbs(21),Bet(1)) ,         &
!>>>>    & (Mcbs(61),R(1))
   DATA filea , fileb , filec , filed , scrtch/101 , 102 , 103 , 201 , 301/
   DATA name/4HMPYA , 4HD   /
   DATA dosi/4HSING , 4HDOUB , 4HMLTP/ , refus/2*3H    , 3HREF/
   DATA square , rect , diag , symm , ident/1 , 2 , 3 , 6 , 8/
!
!
!     READ TRAILERS FOR A, B AND C MATRICES.
!
   nz = korsz(z)
   a(1) = filea
   CALL rdtrl(a)
   IF ( a(1)==filea ) THEN
      b(1) = fileb
      CALL rdtrl(b)
      IF ( b(1)==fileb ) THEN
         c(1) = filec
         c(5) = 0
         CALL rdtrl(c)
         IF ( c(1)<0 ) c(1) = 0
         d(1) = filed
         d(3) = a(3)
         IF ( t/=0 ) d(3) = a(2)
         d(4) = rect
!
!     CHECK FOR CONFORMABLE MATRICIES
!
         IF ( ((c(2)/=b(2) .OR. c(3)/=d(3)) .AND. c(1)/=0) .OR. (b(3)/=a(2) .AND. t==0) .OR. (b(3)/=a(3) .AND. t/=0) )              &
            & CALL mesage(-55,0,name)
         trnsp = t
         sab = signab
         sc = signc
         prec = 1
         IF ( itype==0 ) prec = 0
         IF ( itype==2 .OR. itype==4 ) prec = 2
         prec1 = max0(a(5),b(5),c(5))
         IF ( prec1>2 ) prec1 = prec1 - 2
         IF ( prec1<1 .OR. prec1>2 ) prec1 = kprec
         IF ( prec/=prec1 .AND. prec/=0 ) THEN
            IF ( prec<1 .OR. prec>2 ) prec = 3
            WRITE (outpt,99001) swm , dosi(prec) , refus(prec) , name , dosi(prec1)
99001       FORMAT (A27,' 2430, REQUESTED ',A4,'LE PRECISION ',A3,'USED BY ',2A4,2H. ,A4,'LE PRECISION IS LOGICAL CHOICE')
            IF ( prec/=3 ) prec1 = prec
         ENDIF
         ltype = prec1
         IF ( a(5)==3 .OR. a(5)==4 .OR. b(5)==3 .OR. b(5)==4 .OR. c(5)==3 .OR. c(5)==4 ) ltype = prec1 + 2
         IF ( itype/=0 .AND. itype/=ltype ) THEN
            jj = 1
            IF ( itype<1 .OR. itype>4 ) jj = 3
            WRITE (outpt,99002) swm , itype , refus(jj) , name , ltype
99002       FORMAT (A27,' 2431, REQUESTED TYPE ',I4,2H, ,A3,'USED BY ',2A4,7H. TYPE ,I4,'  IS LOGICAL CHOICE.')
            IF ( jj/=3 ) ltype = itype
         ENDIF
         itype = ltype
         d(5) = itype
         scr = scrtch
!
!     IF NEITHER A NOR B IS DIAGONAL, CALL MPYAD AND RETURN.
!
         IF ( a(4)==diag .OR. b(4)==diag ) THEN
!
!     OTHERWISE, CALL DMPY FOR DIAGONAL MULTIPLICATION.
!
            DO i = 1 , 7
               e(i) = a(i)
               f(i) = b(i)
               IF ( a(4)/=diag ) THEN
                  e(i) = b(i)
                  f(i) = a(i)
               ENDIF
               g(i) = d(i)
            ENDDO
            nzz = korsz(zz)
            sgn = signab
            flag = 0
            IF ( b(4)==diag ) flag = 1
            IF ( c(1)/=0 ) g(1) = scrtch
            CALL dmpy(zz,zz)
            IF ( g(2)==g(3) ) THEN
               g(4) = square
               k = 0
               DO i = 4 , 14 , 7
                  j = e(i)
                  IF ( j/=symm .AND. j/=diag .AND. j/=ident ) GOTO 10
                  IF ( j==symm ) k = 1
               ENDDO
               IF ( k==1 ) g(4) = symm
            ENDIF
 10         CALL wrttrl(g)
!
!     IF ADDITION REQUIRED, CALL ADD ROUTINE.
!
            IF ( c(1)==0 ) RETURN
            DO i = 1 , 7
               p(i) = g(i)
               q(i) = c(i)
               r(i) = d(i)
            ENDDO
            DO i = 2 , 4
               alp(i) = 0.0
               bet(i) = 0.0
            ENDDO
            typa = 1
            alp(1) = 1.0
            typb = 1
            bet(1) = 1.0
            IF ( signc<0 ) bet(1) = -1.0
            nzzz = korsz(zzz)
            nomat = 2
            CALL sadd(zzz,zzz)
            IF ( r(2)==r(3) ) THEN
               r(4) = square
               IF ( p(4)==symm .AND. (q(4)==symm .OR. q(4)==diag .OR. q(4)==ident) ) r(4) = symm
               CALL wrttrl(r)
            ENDIF
         ELSE
            CALL mpyad(z,z,z)
            IF ( d(2)==d(3) ) THEN
               d(4) = square
               IF ( c(4)==0 ) c(4) = diag
               k = 0
               DO i = 4 , 21 , 7
                  j = a(i)
                  IF ( j/=symm .AND. j/=diag .AND. j/=ident ) GOTO 20
                  IF ( j==symm ) k = 1
               ENDDO
               IF ( k==1 ) d(4) = symm
            ENDIF
 20         CALL wrttrl(d)
            RETURN
         ENDIF
      ENDIF
   ENDIF
END SUBROUTINE dmpyad

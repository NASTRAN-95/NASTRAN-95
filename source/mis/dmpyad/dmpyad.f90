!*==dmpyad.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
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
   IMPLICIT NONE
   USE C_BLANK
   USE C_DMPYX
   USE C_MPYADX
   USE C_SADDX
   USE C_SYSTEM
   USE C_XMSSG
   USE C_ZZZZZZ
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
   Nz = korsz(Z)
   A(1) = filea
   CALL rdtrl(A)
   IF ( A(1)==filea ) THEN
      B(1) = fileb
      CALL rdtrl(B)
      IF ( B(1)==fileb ) THEN
         C(1) = filec
         C(5) = 0
         CALL rdtrl(C)
         IF ( C(1)<0 ) C(1) = 0
         D(1) = filed
         D(3) = A(3)
         IF ( T/=0 ) D(3) = A(2)
         D(4) = rect
!
!     CHECK FOR CONFORMABLE MATRICIES
!
         IF ( ((C(2)/=B(2) .OR. C(3)/=D(3)) .AND. C(1)/=0) .OR. (B(3)/=A(2) .AND. T==0) .OR. (B(3)/=A(3) .AND. T/=0) )              &
            & CALL mesage(-55,0,name)
         Trnsp = T
         Sab = Signab
         Sc = Signc
         prec = 1
         IF ( Itype==0 ) prec = 0
         IF ( Itype==2 .OR. Itype==4 ) prec = 2
         Prec1 = max0(A(5),B(5),C(5))
         IF ( Prec1>2 ) Prec1 = Prec1 - 2
         IF ( Prec1<1 .OR. Prec1>2 ) Prec1 = kprec
         IF ( prec/=Prec1 .AND. prec/=0 ) THEN
            IF ( prec<1 .OR. prec>2 ) prec = 3
            WRITE (outpt,99001) Swm , dosi(prec) , refus(prec) , name , dosi(Prec1)
99001       FORMAT (A27,' 2430, REQUESTED ',A4,'LE PRECISION ',A3,'USED BY ',2A4,2H. ,A4,'LE PRECISION IS LOGICAL CHOICE')
            IF ( prec/=3 ) Prec1 = prec
         ENDIF
         ltype = Prec1
         IF ( A(5)==3 .OR. A(5)==4 .OR. B(5)==3 .OR. B(5)==4 .OR. C(5)==3 .OR. C(5)==4 ) ltype = Prec1 + 2
         IF ( Itype/=0 .AND. Itype/=ltype ) THEN
            jj = 1
            IF ( Itype<1 .OR. Itype>4 ) jj = 3
            WRITE (outpt,99002) Swm , Itype , refus(jj) , name , ltype
99002       FORMAT (A27,' 2431, REQUESTED TYPE ',I4,2H, ,A3,'USED BY ',2A4,7H. TYPE ,I4,'  IS LOGICAL CHOICE.')
            IF ( jj/=3 ) ltype = Itype
         ENDIF
         Itype = ltype
         D(5) = Itype
         Scr = scrtch
!
!     IF NEITHER A NOR B IS DIAGONAL, CALL MPYAD AND RETURN.
!
         IF ( A(4)==diag .OR. B(4)==diag ) THEN
!
!     OTHERWISE, CALL DMPY FOR DIAGONAL MULTIPLICATION.
!
            DO i = 1 , 7
               E(i) = A(i)
               F(i) = B(i)
               IF ( A(4)/=diag ) THEN
                  E(i) = B(i)
                  F(i) = A(i)
               ENDIF
               G(i) = D(i)
            ENDDO
            Nzz = korsz(zz)
            Sgn = Signab
            Flag = 0
            IF ( B(4)==diag ) Flag = 1
            IF ( C(1)/=0 ) G(1) = scrtch
            CALL dmpy(zz,zz)
            IF ( G(2)==G(3) ) THEN
               G(4) = square
               k = 0
               DO i = 4 , 14 , 7
                  j = E(i)
                  IF ( j/=symm .AND. j/=diag .AND. j/=ident ) GOTO 10
                  IF ( j==symm ) k = 1
               ENDDO
               IF ( k==1 ) G(4) = symm
            ENDIF
 10         CALL wrttrl(G)
!
!     IF ADDITION REQUIRED, CALL ADD ROUTINE.
!
            IF ( C(1)==0 ) RETURN
            DO i = 1 , 7
               p(i) = G(i)
               q(i) = C(i)
               r(i) = D(i)
            ENDDO
            DO i = 2 , 4
               alp(i) = 0.0
               bet(i) = 0.0
            ENDDO
            typa = 1
            alp(1) = 1.0
            typb = 1
            bet(1) = 1.0
            IF ( Signc<0 ) bet(1) = -1.0
            Nzzz = korsz(zzz)
            Nomat = 2
            CALL sadd(zzz,zzz)
            IF ( r(2)==r(3) ) THEN
               r(4) = square
               IF ( p(4)==symm .AND. (q(4)==symm .OR. q(4)==diag .OR. q(4)==ident) ) r(4) = symm
               CALL wrttrl(r)
            ENDIF
         ELSE
            CALL mpyad(Z,Z,Z)
            IF ( D(2)==D(3) ) THEN
               D(4) = square
               IF ( C(4)==0 ) C(4) = diag
               k = 0
               DO i = 4 , 21 , 7
                  j = A(i)
                  IF ( j/=symm .AND. j/=diag .AND. j/=ident ) GOTO 20
                  IF ( j==symm ) k = 1
               ENDDO
               IF ( k==1 ) D(4) = symm
            ENDIF
 20         CALL wrttrl(D)
            RETURN
         ENDIF
      ENDIF
   ENDIF
END SUBROUTINE dmpyad

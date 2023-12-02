!*==optp1a.f90 processed by SPAG 8.01RF 16:19  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE optp1a(Elt,Elop,Ele,Dtyp)
   USE c_blank
   USE c_gpta1
   USE c_matin
   USE c_matout
   USE c_names
   USE c_optpw1
   USE c_system
   USE c_xmssg
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER , DIMENSION(1) :: Elt
   INTEGER , DIMENSION(2,1) :: Elop
   REAL , DIMENSION(1) :: Ele
   INTEGER , DIMENSION(1) :: Dtyp
!
! Local variable declarations rewritten by SPAG
!
   REAL :: c1
   INTEGER :: i , i1 , i2 , ietyp , intyp , j1 , j2 , k1 , k2 , nest
   INTEGER , DIMENSION(1) :: ie , imat
   INTEGER , DIMENSION(21) , SAVE :: ipt
   INTEGER , DIMENSION(28) , SAVE :: iwd
   INTEGER , DIMENSION(2) , SAVE :: name
   EXTERNAL fread , mat , mesage , page2 , read
!
! End of declarations rewritten by SPAG
!
   INTEGER :: spag_nextblock_1
!
   !>>>>EQUIVALENCE (E(1),Ie(1)) , (Omat(1),Imat(1)) , (k1,c1)
   DATA name/4H OPT , 4HP1A /
!
!     POINTER TO IPT ARRAY - ZERO MEANS ELEMENT NOT USED.
!     UPDATE IPT DIMENSIONS AS NEW ELEMENTS ARE ADDED
!
   DATA ipt/15 , 17 , 21 , 11 , 23 , 25 , 11 , 13 , 11 , 1 , 5 , 9 , 7 , 9 , 19 , 9 , 9 , 3 , 27 , 27 , 0/
!
!     WORD POINTER TO EST AND MATERIAL STRESS LIMITS
!     WORD 1 = 100*WORD TO OPTIMIZE (EST - IF.NE.0) + ALTERNATE
!     WORD 2 = 100*WORD FOR STRESS LIMIT + ALTERNATE
!              WHERE 1 = SHEAR
!                    2 = TENSION/COMPRESSION
!                    3 = ANY/ALL NONZERO
!
!                      11        13        15        17        19
!                      21        23        25       27
   DATA iwd/506 , 201 , 500 , 200 , 700 , 100 , 709 , 303 , 700 , 300 , 800 , 300 , 810 , 303 , 1718 , 202 , 910 , 202 , 1011 ,     &
      & 303 , 1300 , 303 , 800 , 303 , 800 , 303 , 1400 , 300/
   spag_nextblock_1 = 1
   SPAG_DispatchLoop_1: DO
      SELECT CASE (spag_nextblock_1)
      CASE (1)
!
         nelw = 0
         sinth = 0.0
         costh = 1.0
         pla = 0.0
         inflag = 2
!
!     COPY POINTER ARRAY INTO CORE
!
         DO i = 1 , ntypes
            Elt(i) = Dtyp(i)
         ENDDO
!
!     ZERO OUT POINTER ARRAY
!
         i1 = 2*(npow+1)
         DO i = 2 , i1
            Elop(i,1) = 0
         ENDDO
         Elop(1,1) = 1
         spag_nextblock_1 = 2
      CASE (2)
!
!     READ IN ELEMENT TYPE
!
         CALL read(*40,*80,est,ietyp,1,noeor,i)
         IF ( ietyp<=ntypes ) THEN
            intyp = Dtyp(ietyp)
            IF ( intyp>0 ) THEN
!
!     DECODE LIMITS NEEDED
!
               i = ipt(intyp)
               j2 = iwd(i)
               j1 = j2/100
               j2 = j2 - j1*100
               i2 = iwd(i+1)
               i1 = i2/100
               i2 = i2 - i1*100
               nest = (ietyp-1)*incr + 12
               nest = ne(nest)
               IF ( nest>ecor ) THEN
                  CALL page2(-2)
                  WRITE (outtap,99002) name , ecor , ietyp
                  nelw = 0
                  spag_nextblock_1 = 3
                  CYCLE SPAG_DispatchLoop_1
               ENDIF
               DO
                  CALL read(*60,*20,est,e,nest,noeor,k1)
                  matid = ie(j1-1)
                  IF ( matid==0 ) matid = ie(j2-1)
                  temp = e(nest)
                  CALL mat(ie(1))
!
!     TEST IF PERTINENT STRESS LIMITS ARE ZERO
!
                  k1 = 0
                  k2 = 0
                  IF ( i1/=2 .OR. i2/=2 ) THEN
!
!     SHEAR
!
                     IF ( omat(15)==0.0 ) THEN
                        IF ( i1/=2 ) k1 = 1
                        IF ( i2==1 .OR. i2==3 ) k2 = 1
                     ENDIF
                  ENDIF
                  IF ( i1/=1 .OR. i2>1 ) THEN
!
!     TENSION
!
                     IF ( omat(13)==0.0 ) THEN
                        IF ( i1>1 ) k1 = k1 + 1
                        IF ( i2>1 ) k2 = k2 + 1
                     ENDIF
!
!     COMPRESSION
!
                     IF ( omat(14)==0.0 ) THEN
                        IF ( i1>1 ) k1 = k1 + 1
                        IF ( i2>1 ) k2 = k2 + 1
                     ENDIF
                  ENDIF
!
                  IF ( k1<i1 .OR. k2<i2 ) THEN
!
!     CHECK IF PROPERTY IS NONZERO AND STORE INFO IN PID POINTER
!
                     IF ( e(j1)/=0.0 ) THEN
!
                        IF ( k1<i1 ) THEN
!
!     PRIMARY PROPERTY USED
!
                           k1 = j1*100 + i1
                           GOTO 2
                        ENDIF
                     ENDIF
                     IF ( e(j2)==0.0 ) CYCLE
!
                     IF ( k2>=i2 ) CYCLE
!
!     ALTERNATE PROPERTY USED
!
                     k1 = j2*100 + i2
 2                   IF ( nelw+5>ycor ) THEN
                        spag_nextblock_1 = 4
                        CYCLE SPAG_DispatchLoop_1
                     ENDIF
                     Ele(nelw+1) = e(1)
                     Ele(nelw+2) = omat(13)
                     Ele(nelw+3) = omat(14)
                     Ele(nelw+4) = omat(15)
!
!     NOTE, K1 = C1
!
                     Ele(nelw+5) = c1
                     nelw = nelw + nwdse
                  ENDIF
               ENDDO
            ENDIF
         ENDIF
!
!     NEW ELEMENT TYPE
!
         CALL fread(est,0,0,nweor)
         IF ( ietyp>ntypes ) GOTO 40
         IF ( intyp<=0 ) THEN
            spag_nextblock_1 = 2
            CYCLE SPAG_DispatchLoop_1
         ENDIF
 20      Elop(1,intyp+1) = nelw + 1
         spag_nextblock_1 = 2
         CYCLE SPAG_DispatchLoop_1
!
!     EOF
!
 40      i1 = npow + 1
         DO i = 2 , i1
            IF ( Elop(1,i)<=0 ) Elop(1,i) = Elop(1,i-1)
         ENDDO
         IF ( nelw==0 ) THEN
            CALL page2(-2)
            WRITE (outtap,99001) ufm
99001       FORMAT (A23,' 2295, NO ELEMENTS EXIST FOR OPTIMIZATION.')
            count = -1
         ENDIF
         spag_nextblock_1 = 3
      CASE (3)
         RETURN
!
!     ILLEGAL EOF
!
 60      CALL mesage(-2,est,name)
!
!     ILLEGAL EOR
!
 80      CALL mesage(-3,est,name)
         spag_nextblock_1 = 4
      CASE (4)
!
!     INSUFFICIENT CORE
!
         CALL page2(-2)
         WRITE (outtap,99002) ufm , name , b1p1 , ie(1)
         nelw = 0
         spag_nextblock_1 = 3
      END SELECT
   ENDDO SPAG_DispatchLoop_1
99002 FORMAT (A23,' 2296, INSUFFICIENT CORE ',2A4,1H(,I10,' ), ELEMENT',I9)
END SUBROUTINE optp1a

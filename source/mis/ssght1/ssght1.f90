!*==ssght1.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE ssght1(Iest,File,Nequiv)
   IMPLICIT NONE
   USE c_condas
   USE c_estout
   USE c_gpta1
   USE c_hmatdd
   USE c_hmtout
   USE c_matin
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Iest
   INTEGER :: File
   INTEGER , DIMENSION(1) :: Nequiv
!
! Local variable declarations rewritten by SPAG
!
   REAL , DIMENSION(100) :: est
   INTEGER :: flag , i , ia , iel , ig , iloc , im , ipt , is , isil , itemp , ith , j , jpoint , np , nwords , type , zp
   INTEGER , DIMENSION(2) :: nest
   INTEGER , DIMENSION(45) :: nesto
   INTEGER , SAVE :: numelt
   REAL :: pi
   INTEGER , DIMENSION(8,20) , SAVE :: point1
   INTEGER , DIMENSION(8,3) , SAVE :: point2
   INTEGER , DIMENSION(8,23) :: pointr
   INTEGER , DIMENSION(2) , SAVE :: subr
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
!     THIS ROUTINE CONVERTS THE EST DATA FOR ALL THERMAL ELEMENTS TO A
!     COMMON FORMAT. OPTIONAL TASKS INCLUDE CALCULATING MATOUT DATA AND
!     CONVERTING  SIL VALUES TO UN VALUES.
!*****
!
!
   !>>>>EQUIVALENCE (Consts(1),Pi)
   !>>>>EQUIVALENCE (Nesto(1),Elid) , (nest(1),est(1))
   !>>>>EQUIVALENCE (point1(1,1),pointr(1,1)) , (point2(1,1),pointr(1,21))
!
   DATA subr/4HSSGH , 4HT1  /
   DATA numelt/23/
!*****
!     THE POINTERS TO THE EST DATA ARE
!
!        IM    MAT ID
!        ITH   THETA
!        IA    AREA
!        IG    GRID POINT DATA
!        IS    SIL MINUS 1
!        NP    NO. OF POINTS
!        SUB   SUBROUTINE TYPE
!                       NO.  IS   ITH  IM   IA   IG   NP   SUB
!                      ----  --   ---  --   --   --   --   ----
   DATA point1/1 , 0 , 0 , 4 , 5 , 9 , 2 , 1 , 3 , 0 , 0 , 4 , 5 , 8 , 2 , 1 , 6 , 0 , 5 , 6 , 7 , 15 , 3 , 2 , 9 , 0 , 5 , 6 , 7 , &
      & 9 , 3 , 2 , 10 , 0 , 0 , 4 , 5 , 9 , 2 , 1 , 16 , 0 , 6 , 7 , 8 , 10 , 4 , 3 , 17 , 0 , 5 , 6 , 7 , 9 , 3 , 2 , 18 , 0 , 6 ,&
      & 7 , 8 , 10 , 4 , 3 , 19 , 0 , 6 , 7 , 8 , 16 , 4 , 3 , 34 , 0 , 0 , 16 , 17 , 34 , 2 , 1 , 36 , 0 , 5 , 6 , 0 , 7 , 3 , 4 , &
      & 37 , 0 , 6 , 7 , 0 , 8 , 4 , 5 , 39 , 1 , 0 , 2 , 0 , 7 , 4 , 6 , 40 , 1 , 0 , 2 , 0 , 9 , 6 , 7 , 41 , 1 , 0 , 2 , 0 , 11 ,&
      & 8 , 8 , 42 , 1 , 0 , 2 , 0 , 11 , 8 , 9 , 52 , 1 , 0 , 15 , 16 , 21 , 8 , 10 , 62 , 0 , 6 , 7 , 8 , 10 , 4 , 3 , 63 , 0 ,   &
      & 6 , 7 , 8 , 10 , 4 , 3 , 65 , 0 , 0 , 10 , 0 , 16 , 8 , 16/
   DATA point2/66 , 0 , 0 , 22 , 0 , 28 , 20 , 16 , 67 , 0 , 0 , 34 , 0 , 40 , 32 , 1 , 76 , 0 , 11 , 12 , 13 , 14 , 8 , 17/
!*****
   CALL delset
 100  CALL read(*300,*500,Iest,type,1,0,flag)
   DO i = 1 , numelt
      iel = i
      IF ( type<pointr(1,i) ) EXIT
      IF ( type==pointr(1,i) ) GOTO 200
   ENDDO
   CALL fwdrec(*600,Iest)
   GOTO 100
!
 200  zp = (type-1)*incr
   name(1) = elem(zp+1)
   name(2) = elem(zp+2)
   nwords = elem(zp+12)
   DO
      CALL read(*600,*400,Iest,est,nwords,0,flag)
      elid = nest(1)
      DO i = 5 , 45
         nesto(i) = 0
      ENDDO
      IF ( type==3 ) est(5) = pi*est(6)*(est(5)-est(6))
      IF ( type==52 .AND. nest(2)==7 ) est(16) = pi*(est(19)+est(20))
      is = pointr(2,iel)
      ith = pointr(3,iel)
      im = pointr(4,iel)
      ia = pointr(5,iel)
      ig = pointr(6,iel)
      sub = pointr(8,iel)
      np = pointr(7,iel)
!
      IF ( sub==10 ) sub = sub + nest(2) - 1
      inflag = 1
      IF ( sub>=16 ) inflag = 3
      IF ( sub>=2 .AND. sub<=5 ) THEN
         inflag = 2
      ELSEIF ( sub>=6 .AND. sub<=9 ) THEN
         inflag = 3
      ENDIF
      IF ( ia>0 ) af = est(ia)
      matid = nest(im)
      IF ( matid>0 ) THEN
         sinth = 0.0
         costh = 1.0
         IF ( inflag==2 ) THEN
            theta = est(ith)*pi/180.0
            IF ( theta/=0.0 ) THEN
               sinth = sin(theta)
               costh = cos(theta)
            ENDIF
         ENDIF
         itemp = ig + 4*np
         eltemp = est(itemp)
         imat = matid
         linear = .FALSE.
         CALL hmat(elid)
!*****
!     TEST IF NONLINEAR
!*****
         IF ( .NOT.(linear) ) THEN
            DO i = 1 , 6
               mato(i) = bufm(i)
            ENDDO
            DO i = 1 , np
               jpoint = 4*(i-1) + ig
               DO j = 1 , 3
                  iloc = jpoint + j
                  r(j,i) = est(iloc)
               ENDDO
               isil = is + i + 1
               ipt = nest(isil)
               IF ( ipt/=0 ) sil(i) = Nequiv(ipt)
            ENDDO
!*****
!     WRITE A UNIFORM EST GROUP OF CONVERTED DATA HERE
!*****
!*****
!     RETURN FOR ANOTHER ELEMENT
!******
            CALL write(File,nesto(1),45,0)
         ENDIF
      ENDIF
   ENDDO
 300  RETURN
!*****
!     DONE WITH THIS ELEMENT TYPE
!*****
 400  IF ( flag==0 ) GOTO 100
!******
 500  j = -3
   GOTO 700
 600  j = -2
 700  CALL mesage(j,Iest,subr)
END SUBROUTINE ssght1

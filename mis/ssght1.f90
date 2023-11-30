
SUBROUTINE ssght1(Iest,File,Nequiv)
   IMPLICIT NONE
   REAL Af , Consts(5) , Costh , Dum(1) , Eltemp , Pi , R(3,8) , Sinth , Theta , Xxx(4)
   INTEGER Bufm(7) , Elem(1) , Elid , Imat , Incr , Inflag , Last , Matid , Mato(6) , Name(2) , Nelems , Nesto(45) , Sil(8) , Sub
   LOGICAL Linear
   COMMON /condas/ Consts
   COMMON /estout/ Elid , Sub , Name , Sil , Imat , Af , Theta , R , Mato
   COMMON /gpta1 / Nelems , Last , Incr , Elem
   COMMON /hmatdd/ Xxx , Linear
   COMMON /hmtout/ Bufm
   COMMON /matin / Matid , Inflag , Eltemp , Dum , Sinth , Costh
   INTEGER File , Iest
   INTEGER Nequiv(1)
   REAL est(100)
   INTEGER flag , i , ia , iel , ig , iloc , im , ipt , is , isil , itemp , ith , j , jpoint , nest(2) , np , numelt , nwords ,     &
         & point1(8,20) , point2(8,3) , pointr(8,23) , subr(2) , type , zp
!*****
!     THIS ROUTINE CONVERTS THE EST DATA FOR ALL THERMAL ELEMENTS TO A
!     COMMON FORMAT. OPTIONAL TASKS INCLUDE CALCULATING MATOUT DATA AND
!     CONVERTING  SIL VALUES TO UN VALUES.
!*****
!
!
   EQUIVALENCE (Consts(1),Pi)
   EQUIVALENCE (Nesto(1),Elid) , (nest(1),est(1))
   EQUIVALENCE (point1(1,1),pointr(1,1)) , (point2(1,1),pointr(1,21))
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
 200  zp = (type-1)*Incr
   Name(1) = Elem(zp+1)
   Name(2) = Elem(zp+2)
   nwords = Elem(zp+12)
   DO
      CALL read(*600,*400,Iest,est,nwords,0,flag)
      Elid = nest(1)
      DO i = 5 , 45
         Nesto(i) = 0
      ENDDO
      IF ( type==3 ) est(5) = Pi*est(6)*(est(5)-est(6))
      IF ( type==52 .AND. nest(2)==7 ) est(16) = Pi*(est(19)+est(20))
      is = pointr(2,iel)
      ith = pointr(3,iel)
      im = pointr(4,iel)
      ia = pointr(5,iel)
      ig = pointr(6,iel)
      Sub = pointr(8,iel)
      np = pointr(7,iel)
!
      IF ( Sub==10 ) Sub = Sub + nest(2) - 1
      Inflag = 1
      IF ( Sub>=16 ) Inflag = 3
      IF ( Sub>=2 .AND. Sub<=5 ) THEN
         Inflag = 2
      ELSEIF ( Sub>=6 .AND. Sub<=9 ) THEN
         Inflag = 3
      ENDIF
      IF ( ia>0 ) Af = est(ia)
      Matid = nest(im)
      IF ( Matid>0 ) THEN
         Sinth = 0.0
         Costh = 1.0
         IF ( Inflag==2 ) THEN
            Theta = est(ith)*Pi/180.0
            IF ( Theta/=0.0 ) THEN
               Sinth = sin(Theta)
               Costh = cos(Theta)
            ENDIF
         ENDIF
         itemp = ig + 4*np
         Eltemp = est(itemp)
         Imat = Matid
         Linear = .FALSE.
         CALL hmat(Elid)
!*****
!     TEST IF NONLINEAR
!*****
         IF ( .NOT.(Linear) ) THEN
            DO i = 1 , 6
               Mato(i) = Bufm(i)
            ENDDO
            DO i = 1 , np
               jpoint = 4*(i-1) + ig
               DO j = 1 , 3
                  iloc = jpoint + j
                  R(j,i) = est(iloc)
               ENDDO
               isil = is + i + 1
               ipt = nest(isil)
               IF ( ipt/=0 ) Sil(i) = Nequiv(ipt)
            ENDDO
!*****
!     WRITE A UNIFORM EST GROUP OF CONVERTED DATA HERE
!*****
!*****
!     RETURN FOR ANOTHER ELEMENT
!******
            CALL write(File,Nesto(1),45,0)
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

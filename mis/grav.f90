
SUBROUTINE grav(Ngrav,Gvect,Nlist,Ilist,Nloop)
   IMPLICIT NONE
   INTEGER Lcore , N(14) , Nobld , Nsys
   REAL Ro(3) , Slt , To(3,3) , Tysys
   COMMON /loadx / Lcore , Slt , N , Nobld
   COMMON /tranx / Nsys , Tysys , Ro , To
   INTEGER Ngrav , Nlist , Nloop
   REAL Gvect(1)
   INTEGER Ilist(1)
   REAL flag , gl(5) , x(3)
   INTEGER i , igl , j , name(2) , nl1 , nlist1 , nsave
!
   EQUIVALENCE (igl,gl(2))
   DATA name/4HGRAV , 4H    /
!
!     CONVERTS GRAV CARD TO BASIC AND STORES
!     GB = G*TON*V
!
   CALL read(*100,*100,Slt,gl(1),5,0,flag)
   GOTO 200
!
 100  CALL mesage(-7,name,name)
 200  Ngrav = Ngrav + 1
   IF ( gl(1)/=0 ) THEN
      CALL fdcstm(gl(1))
      CALL mpyl(To,gl(3),3,3,1,x(1))
      DO i = 1 , 3
         gl(i+2) = x(i)
      ENDDO
   ENDIF
   DO i = 1 , 3
      j = (Ngrav-1)*3 + i
      Gvect(j) = gl(i+2)*gl(2)
   ENDDO
   nl1 = Nloop - Ngrav + 1
   IF ( nl1/=Nlist ) THEN
      nsave = Ilist(nl1)
      nlist1 = Nlist - 1
      DO i = nl1 , nlist1
         Ilist(i) = Ilist(i+1)
      ENDDO
      Ilist(Nlist) = nsave
   ENDIF
   RETURN
END SUBROUTINE grav


SUBROUTINE skpfrm(Bframs)
   IMPLICIT NONE
   REAL Axymax(2) , Edge(2) , Origin(2) , Pxymax(7) , Reg(2,2) , Skpplt(9)
   INTEGER Camera , Model , Ploter
   COMMON /pltdat/ Model , Ploter , Reg , Axymax , Edge , Camera , Skpplt , Pxymax , Origin
   INTEGER Bframs
   INTEGER a(10) , adv10(3) , bfrms , con10 , i
   REAL save(2,4) , xymax(2)
!
   DATA adv10 , con10/1 , 2 , 3 , 3/
!
   DO i = 1 , 2
      save(i,1) = Reg(i,1)
      Reg(i,1) = 0.
      save(i,2) = Reg(i,2)
      Reg(i,2) = Axymax(i) + 2.*Edge(i)
      save(i,3) = Origin(i)
      Origin(i) = 0.
      save(i,4) = Edge(i)
      Edge(i) = 0.
   ENDDO
   xymax(1) = amax1(Reg(1,2),Reg(2,2))
   xymax(2) = amin1(Reg(1,2),Reg(2,2))
   Reg(1,2) = xymax(1)
   Reg(2,2) = xymax(2)
   bfrms = min0(max0(Bframs,1),5)
!
!     PLOTTER 1, 2
!
   a(1) = con10
   a(2) = adv10(Camera)
   DO i = 3 , 6
      a(i) = 0
   ENDDO
   DO i = 1 , bfrms
      CALL wplt10(a,0)
   ENDDO
!
   DO i = 1 , 2
      Reg(i,1) = save(i,1)
      Reg(i,2) = save(i,2)
      Origin(i) = save(i,3)
      Edge(i) = save(i,4)
   ENDDO
!
END SUBROUTINE skpfrm
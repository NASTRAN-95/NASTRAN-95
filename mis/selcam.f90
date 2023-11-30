
SUBROUTINE selcam(Camera,Pltnum,Opt)
   IMPLICIT NONE
   INTEGER Camnum , Model , Ploter
   REAL Edge(2) , Origin(2) , Pd(20,2) , Xymax(2)
   COMMON /pltdat/ Pd
   INTEGER Camera , Opt , Pltnum
   INTEGER a(17) , cam10(3) , con10(2) , i
   REAL save(2,3)
!
!
!
   EQUIVALENCE (Model,Pd(1,1)) , (Ploter,Pd(2,1)) , (Xymax(1),Pd(7,1)) , (Edge(1),Pd(9,1)) , (Camnum,Pd(11,1)) , (Origin(1),Pd(8,2))
   DATA con10 , cam10/1 , 2 , 1 , 2 , 3/
!
   DO i = 1 , 2
      save(i,1) = Edge(i)
      Edge(i) = 0.
      save(i,2) = Origin(i)
      Origin(i) = 0.
      save(i,3) = Xymax(i)
      Xymax(i) = 0.
      a(i) = iabs(Pltnum)
   ENDDO
   Camnum = min0(max0(Camera,1),3)
   IF ( Opt==0 ) THEN
!
!     PLOTTER 1, 2
!
      a(3) = a(1)
      a(1) = con10(1)
      a(2) = 0
      a(4) = save(1,3) + 2.*save(1,1) + .1
      a(5) = save(2,3) + 2.*save(2,1) + .1
      a(6) = 0
      CALL wplt10(a,0)
   ENDIF
   a(1) = con10(2)
   a(2) = cam10(Camnum)
   DO i = 3 , 6
      a(i) = 0
   ENDDO
   CALL wplt10(a,0)
!
   DO i = 1 , 2
      Edge(i) = save(i,1)
      Origin(i) = save(i,2)
      Xymax(i) = save(i,3)
   ENDDO
END SUBROUTINE selcam

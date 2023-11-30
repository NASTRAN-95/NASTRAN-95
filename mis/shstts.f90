
SUBROUTINE shstts(Tab,Uab,Vab)
   IMPLICIT NONE
   REAL Tab(9) , Uab(9) , Vab(4)
!
!     TO CREATE STRESS TENSOR TRANSFORMATION MATRICES FROM AN ORTHOGONAL
!     TRANSFORMATION FOR SHELL ELEMENTS.
!
!     INPUT :
!           TAB    - ORTHOGONAL INPLANE ROTATION TRANSFORMATION
!     OUTPUT:
!           UAB    - TENSOR TRANSFORMATION FOR NORMAL AND INPLANE SHEAR
!                    COMPONENTS
!           VAB    - TENSOR TRANSFORMATION FOR OUT-OF-PLANE SHEAR
!
!     USAGE:
!           THE INPUT IS ASSUMED TO BE ROW-LOADED.
!           OUTPUTS ARE CREATED ROW-LOADED.
!           DEFINING:
!           [S]      AS A 2-D STRESS VECTOR;
!           [E]      AS A 2-D STRAIN VECTOR;
!           [Q]      AS A 2-D SHEAR FORCE VECTOR;
!           [G]      AS A 2-D STRESS/FORCE-STRAIN RELATION; AND
!           [ALPHA]  AS A VECTOR OF THERMAL EXPANSION COEFFICIENTS,
!
!           THEN THE FOLLOWING RELATIONSHIPS ARE TRUE:
!
!                       T                        T
!           [S]  = [UAB] [S]         [G]  = [UAB] [G] [UAB]
!              A            B           A            B
!
!                       T
!           [Q]  = [VAB] [Q]
!              A            B
!
!           IF [TBA] IS INPUT, THE OUTPUT WILL BE:
!
!                -1                      -1       T
!           [UAB]  = [UBA],   AND   [VAB]  = [VAB] = [VBA]
!
!           WHICH MAY BE USED IN THE FOLLOWING:
!
!           [E]  = [UBA] [E]         [ALPHA]  = [UBA] [ALPHA]
!              A            B               A                B
!
!           [Q]  = [VBA][Q]
!              A           B
!
!
!
   Uab(1) = Tab(1)*Tab(1)
   Uab(2) = Tab(4)*Tab(4)
   Uab(3) = Tab(1)*Tab(4)
   Uab(4) = Tab(2)*Tab(2)
   Uab(5) = Tab(5)*Tab(5)
   Uab(6) = Tab(2)*Tab(5)
   Uab(7) = Tab(1)*Tab(2)*2.0
   Uab(8) = Tab(4)*Tab(5)*2.0
   Uab(9) = Tab(1)*Tab(5) + Tab(2)*Tab(4)
!
   Vab(1) = Tab(5)*Tab(9)
   Vab(2) = Tab(2)*Tab(9)
   Vab(3) = Tab(4)*Tab(9)
   Vab(4) = Tab(1)*Tab(9)
!
END SUBROUTINE shstts
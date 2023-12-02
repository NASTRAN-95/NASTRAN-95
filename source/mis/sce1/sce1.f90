!*==sce1.f90 processed by SPAG 8.01RF 14:47  2 Dec 2023
!!SPAG Open source Personal, Educational or Academic User  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
SUBROUTINE sce1
   IMPLICIT NONE
   USE C_PATX
!
! Local variable declarations rewritten by SPAG
!
   INTEGER , SAVE :: bff , bnn , k4ff , k4nn , kff , kfs , knn , kss , mff , mnn , pvect , uf , un , us , uset
   EXTERNAL mpart , upart
!
! End of declarations rewritten by SPAG
!
!
!     MODULE 2.6 SCE PARTITIONS KNN,MNN,BNN,AND K4NN
!
!     TO ELIMINATE THE EFFECTS OF SINGLE POINT CONSTRAINTS IF US IS NOT
!     NULL
!
   DATA un , uf , us/27 , 26 , 31/
   DATA uset , knn , mnn , bnn , k4nn , kff , kfs , kss , mff , bff , k4ff , pvect/101 , 102 , 103 , 104 , 105 , 201 , 202 , 203 ,  &
      & 204 , 205 , 206 , 301/
!
   CALL upart(uset,pvect,un,uf,us)
   CALL mpart(knn,kff,0,kfs,kss)
   CALL mpart(mnn,mff,0,0,0)
   CALL mpart(bnn,bff,0,0,0)
   CALL mpart(k4nn,k4ff,0,0,0)
END SUBROUTINE sce1


SUBROUTINE sce1
   IMPLICIT NONE
   INTEGER N(2) , N3 , Nn(3)
   COMMON /patx  / N , N3 , Nn
   INTEGER bff , bnn , k4ff , k4nn , kff , kfs , knn , kss , mff , mnn , pvect , uf , un , us , uset
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
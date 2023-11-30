
SUBROUTINE fndplt(Ploter,Model,Pmodel)
   IMPLICIT NONE
   INTEGER Model , Ploter
   INTEGER Pmodel(2)
   INTEGER i , i2 , n , n1 , pltmdl(2,6) , pltter(2,6)
!
!     PLOTER = PLOTTER INDEX.
!     MODEL = MODEL INDEX.
!     PMODEL = PLOTTER MODEL ID.
!
!...  DATA FOR PLOTTER + MODEL RECOGNITION.
!
!
!       NASTRAN GENERAL PURPOSE PLOTTER
   DATA pltmdl/1HM , 1 , 1HT , 1 , 1HD , 1 , 1HM , 0 , 1HT , 0 , 1HD , 0/
   DATA pltter/1 , -1 , 2 , -2 , 2 , -3 , 1 , +1 , 2 , +2 , 2 , +3/
!
!     FIND THE MODEL ID.
!
   n = -1
   n1 = Pmodel(2)
   DO i = 1 , 6
      IF ( Pmodel(1)==pltmdl(1,i) ) THEN
         IF ( n<=0 ) n = i
         IF ( n1==pltmdl(2,i) ) n = i
      ENDIF
   ENDDO
!
!     SETUP THE PLOTTER + MODEL INDICES.
!
   i2 = Pmodel(2)
   IF ( n<0 ) i2 = 0
   n = iabs(n)
   DO i = 1 , 2
      IF ( pltmdl(i,n)/=0 ) Pmodel(i) = pltmdl(i,n)
   ENDDO
   Ploter = pltter(1,n)
   Model = pltter(2,n)
!
END SUBROUTINE fndplt
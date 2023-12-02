!*==dsxfsz.f90  processed by SPAG 7.61RG at 01:00 on 21 Mar 2022
 
SUBROUTINE dsxfsz
   IMPLICIT NONE
   USE I_DSIOF
   USE I_GINOX
   USE C_XFIAT
   USE C_XFIST
   USE C_ZZZZZZ
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i , ifrblk , index , indx , ipblks , itotal , itotl1 , itotl2 , lasblk , lblock , lim , maxusd , maxusm , nexblk ,    &
            & numblk , nun
!
! End of declarations rewritten by SPAG
!
   idsn = ifilex
   nun = 0
   itotal = 0
   DO
      lasblk = fcb(6,idsn)
      ifrblk = fcb(5,idsn)
      numblk = lasblk - ifrblk + 1
      IF ( idsn==ifilex ) THEN
         ipblks = numblk
         IF ( fcb(10,ifilex)/=0 ) THEN
            index = fcb(10,ifilex)
            lblock = Mem(index+3)
            ipblks = ipblks + lblock
         ENDIF
      ELSE
         nun = nun + 1
         itotal = itotal + numblk
      ENDIF
      idsn = iand(mdsfcb(3,idsn),maskh2)
      IF ( idsn==0 ) THEN
         lim = 2*Nfist
         DO i = 1 , lim , 2
            IF ( name==Ifist(i) ) THEN
               IF ( Ifist(i+1)>0 ) THEN
                  indx = Ifist(i+1)
                  Ifiat(indx+7) = ipblks*2**16 + nun*2**8
                  Ifiat(indx+8) = itotal*2**16
               ENDIF
               EXIT
            ENDIF
         ENDDO
         maxusm = 0
         maxusd = 0
! ACCUMULATE TOTAL I/O USAGE STATISTICS
         DO i = 1 , 80
            IF ( i/=7 ) THEN
               itotl1 = 0
               itotl2 = 0
               IF ( fcb(4,i)/=0 ) THEN
                  nexblk = fcb(10,i)
                  IF ( nexblk/=0 ) itotl1 = Mem(nexblk+3)
                  IF ( fcb(5,i)/=0 ) itotl2 = fcb(6,i) - fcb(5,i) + 1
                  maxusm = maxusm + itotl1
                  maxusd = maxusd + itotl2
               ENDIF
            ENDIF
         ENDDO
         IF ( maxblk<maxusm ) maxblk = maxusm
         IF ( maxdsk<maxusd ) maxdsk = maxusd
         EXIT
      ENDIF
   ENDDO
END SUBROUTINE dsxfsz

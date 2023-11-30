
BLOCKDATA of1pbd
   IMPLICIT NONE
   INTEGER C1(120) , C21(120) , C41(120) , C61(120) , C81(120)
   COMMON /ofpb1 / C1 , C21 , C41 , C61 , C81
!OF1PBD
!
!     C ARRAY FOR REAL STRESSES SORT 1
!
   DATA C1/75 , 0 , 55 , -1 , 18 , 19 , 93 , 0 , 56 , -1 , 20 , 21 , 75 , 0 , 73 , -1 , 18 , 19 , 115 , 0 , 57 , -1 , 22 , 23 ,     &
      & 115 , 0 , 58 , -1 , 22 , 24 , 130 , 0 , 70 , 389 , 27 , 28 , 130 , 0 , 60 , -1 , 27 , 28 , 130 , 0 , 72 , -1 , 27 , 28 ,    &
      & 152 , 0 , 59 , -1 , 25 , 26 , 75 , 0 , 65 , -1 , 18 , 19 , 54 , 0 , 61 , -1 , 29 , 17 , 54 , 0 , 62 , -1 , 29 , 17 , 54 ,   &
      & 0 , 63 , -1 , 29 , 17 , 0 , 0 , 0 , -1 , 0 , 0 , 130 , 0 , 67 , -1 , 27 , 28 , 152 , 0 , 66 , -1 , 25 , 26 , 130 , 0 , 71 , &
      & 389 , 27 , 28 , 130 , 0 , 69 , 389 , 27 , 28 , 130 , 0 , 68 , 389 , 27 , 28 , 0 , 0 , 0 , -1 , 0 , 0/
   DATA C21/0 , 0 , 0 , -1 , 0 , 0 , 0 , 0 , 0 , -1 , 0 , 0 , 0 , 0 , 0 , -1 , 0 , 0 , 0 , 0 , 0 , -1 , 0 , 0 , 0 , 0 , 0 , -1 , 0 ,&
      & 0 , 0 , 0 , 0 , -1 , 0 , 0 , 0 , 0 , 0 , -1 , 0 , 0 , 0 , 0 , 0 , -1 , 0 , 0 , 0 , 0 , 0 , -1 , 0 , 0 , 0 , 0 , 0 , -1 , 0 ,&
      & 0 , 0 , 0 , 0 , -1 , 0 , 0 , 0 , 0 , 0 , -1 , 0 , 0 , 0 , 0 , 0 , -1 , 0 , 0 , 23 , 0 , 64 , -1 , 34 , 35 , 1106 , 0 , 204 ,&
      & -1 , 0 , 206 , 163 , 0 , 74 , -1 , 75 , 76 , 171 , 0 , 77 , -1 , 78 , 79 , 211 , 0 , 80 , -1 , 81 , 82 , 1137 , 0 , 221 ,   &
      & -1 , 0 , 217 , 1137 , 0 , 223 , -1 , 0 , 217/
   DATA C41/1137 , 0 , 225 , -1 , 0 , 217 , 1137 , 0 , 227 , -1 , 0 , 217 , 0 , 0 , 0 , -1 , 0 , 0 , 0 , 0 , 0 , -1 , 0 , 0 , 0 ,   &
      & 0 , 0 , -1 , 0 , 0 , 0 , 0 , 0 , -1 , 0 , 0 , 1461 , 0 , 231 , -1 , 0 , 250 , 1242 , 0 , 232 , -1 , 0 , 241 , 4126 , 0 ,    &
      & 233 , -1 , 0 , 451 , 65 , 0 , 234 , -1 , 0 , 247 , 1387 , 0 , 235 , -1 , 0 , 244 , 0 , 0 , 0 , 0 , 0 , 0 , 1242 , 0 , 254 , &
      & -1 , 0 , 264 , 1242 , 0 , 255 , -1 , 0 , 264 , 1242 , 0 , 256 , -1 , 0 , 264 , 1242 , 0 , 257 , -1 , 0 , 264 , 1242 , 0 ,   &
      & 258 , -1 , 0 , 264 , 1242 , 0 , 280 , -1 , 0 , 264 , 1242 , 0 , 281 , -1 , 0 , 264 , 1242 , 0 , 282 , -1 , 0 , 264/
   DATA C61/1242 , 0 , 283 , -1 , 0 , 264 , 152 , 0 , 304 , -1 , 25 , 26 , 152 , 0 , 306 , -1 , 323 , 26 , 130 , 0 , 308 , 437 ,    &
      & 438 , 439 , 1801 , 0 , 328 , -1 , 329 , 330 , 1801 , 0 , 328 , -1 , 329 , 330 , 1869 , 0 , 328 , -1 , 329 , 330 , 2079 , 0 ,&
      & 343 , -1 , 344 , 345 , 2132 , 0 , 346 , -1 , 344 , 345 , 2176 , 0 , 347 , -1 , 0 , 348 , 2201 , 0 , 351 , -1 , 0 , 348 , 0 ,&
      & 0 , 0 , 0 , 0 , 0 , 2401 , 0 , 363 , -1 , 364 , 365 , 2291 , 0 , 358 , -1 , 361 , 362 , 2291 , 0 , 346 , -1 , 361 , 362 ,   &
      & 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 3141 , 0 , 407 , -1 , 408 , &
      & 409/
   DATA C81/3634 , 0 , 420 , -1 , 34 , 35 , 0 , 0 , 0 , 0 , 0 , 0 , 130 , 0 , 465 , 437 , 438 , 439 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,   &
      & 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , &
      & 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , &
      & 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1949 , 0 , 336 ,&
      & -1 , 337 , 338/
END BLOCKDATA of1pbd
declare void @print_array (i32, i32*, i32*);


define void @main (){
  %M = alloca i32, align 4
  store i32 6, i32* %M
  %ML = load i32 * %M
  %ML8 = trunc i32 %ML to i8
  %__M = alloca <1 x i32>
  %__RES = alloca <4 x i32>
  %__ML = load <1 x i32>* %__M
  %resvec = insertelement <1 x i32> %__ML, i32 %ML, i32 0
  ; Now strech the vector
  %r1 = shufflevector <1 x i32> %resvec, <1 x i32> undef, <4 x i32> <i32 0, i32 0, i32 0, i32 0>
  store <4 x i32> %r1, <4 x i32>* %__RES
  %ptr = getelementptr <4 x i32>* %__RES, i64 0, i64 0
  %ret = alloca i32
  call void @print_array (i32 4, i32* %ptr, i32* %ret)
  ret void
}

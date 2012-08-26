; ModuleID = 'vector_tests'


declare void @printa(i32*, i32 *, i32 *)
declare void @iprint(float)
declare void @print(<4 x i32>)
declare void @print_t(<2 x i32>)
declare void @printh(<32 x float>)
declare void @printf(<128 x float>)
declare void @printvl(<1000 x float>)
; declare <32 x float> @llvm.fma.f32(<32 x float> %a, <32 x float> %b, <32 x float> %c)
declare float @llvm.fma.f32(float  %a, float  %b, float  %c)

define void @main() {
entry:
    ; %result = add <4 x i32> <i32 2, i32 4, i32 5, i32 6>, <i32 1, i32 2, i32 4, i32 6>
    
    ; ; Testing alloca to vector type 
    ; %A = alloca <4 x i32>
    ; %B = alloca <4 x i32>
    
    ; store <4 x i32> <i32 2, i32 4, i32 5, i32 6>, <4 x i32>* %A
    ; store <4 x i32> <i32 1, i32 2, i32 4, i32 6>, <4 x i32>* %B
    
    ; %AL = load <4 x i32>* %A
    ; %BL = load <4 x i32>* %B
    
    ; %result2 =  add <4 x i32> %AL, %BL

    ; %AH = alloca <32 x float>
    ; store <32 x float> <float 2.0, float 2.0, float 2.0, float 2.0, float 2.0, float 2.0, float 2.0, float 2.0, float 2.0, float 2.0, float 2.0, float 2.0, 
    ; float 2.0, float 2.0, float 2.0, float 2.0, float 2.0, float 2.0, float 2.0, float 2.0, float 2.0, float 2.0, float 2.0, float 2.0, float 2.0, float 2.0, float 2.0, float 2.0, 
    ; float 2.0, float 2.0, float 2.0, float 2.0>,  <32 x float>* %AH
    
    ; %AHL = load <32 x float>* %AH
    ; %RAH = fadd <32 x float> %AHL, %AHL
    ; %RAHM = fmul <32 x float> %AHL, %AHL
    ; %RAHMD = fdiv <32 x float> %AHL, %AHL
    ; %RAHMR = frem <32 x float> %AHL, %AHL

    ; %TUTU = alloca <128 x float> 

    ; store <128 x float> <float 2.0, float 2.0,float 2.0,float 2.0,float
    ; 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float
    ; 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float
    ; 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float
    ; 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float
    ; 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float
    ; 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float
    ; 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float
    ; 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float
    ; 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float
    ; 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float
    ; 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float
    ; 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float
    ; 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float
    ; 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float
    ; 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float
    ; 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float
    ; 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float
    ; 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float
    ; 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float
    ; 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float 2.0,float
    ; 2.0,float 2.0,float 2.0,float 2.0>, <128 x float>* %TUTU

    ; %TUTU2 = alloca <128 x float> 

    ; store <128 x float> <float 1.25e+7, float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7>, <128 x float>* %TUTU2
    
    
    ; %TUTUL = load <128 x float>* %TUTU
    ; %TUTUL2 = load <128 x float>* %TUTU2

    ; %TUTUR = fmul <128 x float> %TUTUL, %TUTUL2
    
    ; ; Try some very large results
    
    ; %TUTUVR1 = alloca <1000 x float> 
    ; %TUTUVR2 = alloca <1000 x float> 
    
    ; ; Now store some value on this stack!!  
    
    ; store <1000 x float> <float 2.25, float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float 2.25,float
    ; 2.25>, <1000 x float>* %TUTUVR1
    
    ; store <1000 x float> <float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7,float 1.25e+7,float 1.25e+7,float
    ; 1.25e+7,float 1.25e+7>, <1000 x float>* %TUTUVR2


    ; %TUTUVR1L = load <1000 x float>* %TUTUVR1
    ; %TUTUVR2L = load <1000 x float>* %TUTUVR2
    
    ; %TUTUVR = fdiv <1000 x float> %TUTUVR1L, %TUTUVR2L

    ; call void @print (<4 x i32> %result)
    ; call void @print (<4 x i32> %result2)
    ; call void @printh (<32 x float> %RAH)
    ; call void @printh (<32 x float> %RAHM)
    ; call void @printh (<32 x float> %RAHMD)
    ; call void @printh (<32 x float> %RAHMR)
    ; call void @printf (<128 x float> %TUTUR)
    ; call void @printvl (<1000 x float> %TUTUVR)
    
    
    ; Now let us test some 2D vectors
    %VA1 = alloca <2 x i32>
    %VA2 = alloca <2 x i32>
    %VA3 = alloca <2 x i32>
    
    store <2 x i32> <i32 1, i32 2>, <2 x i32>* %VA1
    store <2 x i32> <i32 9, i32 4>, <2 x i32>* %VA2
    store <2 x i32> <i32 3, i32 4>, <2 x i32>* %VA3

    ; get the pointer to the first element of the vector
    %gep1 = getelementptr inbounds <2 x i32>* %VA1, i64 0, i64 0
    %gep2 = getelementptr inbounds <2 x i32>* %VA2, i64 0, i64 0
    %gep3 = getelementptr inbounds <2 x i32>* %VA3, i64 0, i64 0
    
    ; ; build a temp vector to insert elements into 
    %temp1 = insertelement <3 x i32*> <i32* null, i32* null, i32* null>, i32* %gep1, i32 0
    %temp2 = insertelement <3 x i32*> %temp1, i32* %gep2, i32 1
    ; ; this is a vector of vectors
    %vec_matrix = insertelement <3 x i32*> %temp2, i32* %gep3, i32 2
    
    ; ; now let us extract the 1 and 3rd rows of the 2D vec_matrix
    ; %r1_0 = extractelement <3 x i32*> %vec_matrix, i32 0
    ; %r3_0 = extractelement <3 x i32*> %vec_matrix, i32 2
    
    ; ; now we can store some stuff into this damn thing !! 
    ; store i32 0, i32* %r1_0 ; storing something in the first element of the array
    ; store i32 0, i32* %r3_0 ; storing something in the third element of the array
    
    ; ;now see if VA1 and VA2 added gives what??
    ; %resl3 = load <2 x i32>* %VA3
    ; %resl1 = load <2 x i32>* %VA1
    ; %resuss = add <2 x i32> %resl3, %resl1

    ; call void @print_t (<2 x i32> %resuss)
    
    ;now let us do the same with shuffles
    %svec = shufflevector <3 x i32*> %vec_matrix, <3 x i32*> undef, <2 x i32> <i32 0, i32 2>
    ; increment the pointers together to the next element in the vectors
    %sveci = ptrtoint <2 x i32*> %svec to <2 x i32>
    %svecn = add <2 x i32> %sveci, <i32 1, i32 1>
    ; convert back to ptr type
    %svecptr = inttoptr <2 x i32> %svecn to <2 x i32*>
    ; store new values into these elements
    
    ; now let us extract the 1 and 3rd rows of the 2D vec_matrix
    %t1_0 = extractelement <2 x i32*> %svecptr, i32 0
    %t3_0 = extractelement <2 x i32*> %svecptr, i32 2
    
    ; now we can store some stuff into this damn thing !! 
    store i32 89, i32* %t1_0 ; storing something in the first element of the array
    store i32 98, i32* %t3_0 ; storing something in the third element of the array
    
    ;now see if VA1 and VA2 added gives what??
    %tesl3 = load <2 x i32>* %VA3
    %tesl1 = load <2 x i32>* %VA1
    %rsuss = add <2 x i32> %tesl3, %tesl1
    
    ; ;now let use try some intrinsics to hopefully screw up
    ; ;movi-assembler
    
    ; %fmares = call float @llvm.fma.f32 (float 3.0, float 3.0, float 3.0)
    ; call void @iprint (float %fmares)
    ; ; %fmares = call <32 x float> @llvm.fma.f32 (<32 x float> %AHL, <32 x float> %AHL, <32 x float> %AHL)

    call void @print_t (<2 x i32> %rsuss)
	
    ;Trying an array of vectors 
    %ATT = alloca [3 x <2 x i32> ]
    %tl = load <2 x i32>* %VA1
    %tl2 = load <2 x i32>* %VA2
    %tl1 = load <2 x i32>* %VA3
    %rest1 = add <2 x i32> %tl1, %tl2
    %rest2 = mul <2 x i32> %tl2, %tl
    %ngep1 = getelementptr inbounds [3 x <2 x i32> ]* %ATT, i64 0, i64 0
    %ngep2 = getelementptr inbounds [3 x <2 x i32> ]* %ATT, i64 0, i64 1
    %ngep3 = getelementptr inbounds [3 x <2 x i32> ]* %ATT, i64 0, i64 2
    store <2 x i32> %rest1, <2 x i32>* %ngep1
    store <2 x i32> %rest2, <2 x i32>* %ngep2
    store <2 x i32> %tl2, <2 x i32>* %ngep3
    
    %p = getelementptr inbounds <2 x i32>* %ngep1, i64 0, i64 0
    %q = getelementptr inbounds <2 x i32>* %ngep2, i64 0, i64 0
    %r = getelementptr inbounds <2 x i32>* %ngep3, i64 0, i64 0
    call void @printa (i32* %p, i32 *%q, i32 *%r)

    ; 
    ret void
}

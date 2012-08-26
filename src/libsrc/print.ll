; ModuleID = 'print.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-darwin10.0.0"

%struct.__sFILE = type { i8*, i32, i32, i16, i16, %struct.__sbuf, i32, i8*, i32 (i8*)*, i32 (i8*, i8*, i32)*, i64 (i8*, i64, i32)*, i32 (i8*, i8*, i32)*, %struct.__sbuf, %struct.__sFILEX*, i32, [3 x i8], [1 x i8], %struct.__sbuf, i32, i64 }
%struct.__sFILEX = type opaque
%struct.__sbuf = type { i8*, i32 }

@__stdoutp = external global %struct.__sFILE*     ; <%struct.__sFILE**> [#uses=15]
@.str = private constant [24 x i8] c"Got a single value: %f\0A\00" ; <[24 x i8]*> [#uses=1]
@.str1 = private constant [24 x i8] c"Got a single value: %d\0A\00" ; <[24 x i8]*> [#uses=1]
@.str2 = private constant [20 x i8] c"Got values: %d, %d\0A\00" ; <[20 x i8]*> [#uses=1]
@.str3 = private constant [4 x i8] c"%d\09\00"    ; <[4 x i8]*> [#uses=1]
@.str4 = private constant [2 x i8] c"\0A\00"      ; <[2 x i8]*> [#uses=1]
@.str5 = private constant [4 x i8] c"%f\09\00"    ; <[4 x i8]*> [#uses=1]
@.str6 = private constant [9 x i8] c"lena.raw\00" ; <[9 x i8]*> [#uses=1]
@.str7 = private constant [2 x i8] c"r\00"        ; <[2 x i8]*> [#uses=1]
@start = common global i64 0, align 8             ; <i64*> [#uses=2]
@end = common global i64 0, align 8               ; <i64*> [#uses=2]
@__stderrp = external global %struct.__sFILE*     ; <%struct.__sFILE**> [#uses=1]
@.str8 = private constant [28 x i8] c"Time for function (ms): %g\0A\00" ; <[28 x i8]*> [#uses=1]

define void @pf1(float %a, i32* %ret) nounwind ssp {
  %1 = alloca float, align 4                      ; <float*> [#uses=2]
  %2 = alloca i32*, align 8                       ; <i32**> [#uses=2]
  store float %a, float* %1
  store i32* %ret, i32** %2
  %3 = load %struct.__sFILE** @__stdoutp          ; <%struct.__sFILE*> [#uses=1]
  %4 = load float* %1                             ; <float> [#uses=1]
  %5 = fpext float %4 to double                   ; <double> [#uses=1]
  %6 = call i32 (%struct.__sFILE*, i8*, ...)* @fprintf(%struct.__sFILE* %3, i8* getelementptr inbounds ([24 x i8]* @.str, i32 0, i32 0), double %5) ; <i32> [#uses=0]
  %7 = load i32** %2                              ; <i32*> [#uses=1]
  store i32 1, i32* %7
  ret void
}

declare i32 @fprintf(%struct.__sFILE*, i8*, ...)

define void @p1(i32 %a, i32* %ret) nounwind ssp {
  %1 = alloca i32, align 4                        ; <i32*> [#uses=2]
  %2 = alloca i32*, align 8                       ; <i32**> [#uses=2]
  store i32 %a, i32* %1
  store i32* %ret, i32** %2
  %3 = load %struct.__sFILE** @__stdoutp          ; <%struct.__sFILE*> [#uses=1]
  %4 = load i32* %1                               ; <i32> [#uses=1]
  %5 = call i32 (%struct.__sFILE*, i8*, ...)* @fprintf(%struct.__sFILE* %3, i8* getelementptr inbounds ([24 x i8]* @.str1, i32 0, i32 0), i32 %4) ; <i32> [#uses=0]
  %6 = load i32** %2                              ; <i32*> [#uses=1]
  store i32 1, i32* %6
  ret void
}

define void @p2(i32 %a, i32 %b, i32* %ret) nounwind ssp {
  %1 = alloca i32, align 4                        ; <i32*> [#uses=2]
  %2 = alloca i32, align 4                        ; <i32*> [#uses=2]
  %3 = alloca i32*, align 8                       ; <i32**> [#uses=2]
  store i32 %a, i32* %1
  store i32 %b, i32* %2
  store i32* %ret, i32** %3
  %4 = load %struct.__sFILE** @__stdoutp          ; <%struct.__sFILE*> [#uses=1]
  %5 = load i32* %1                               ; <i32> [#uses=1]
  %6 = load i32* %2                               ; <i32> [#uses=1]
  %7 = call i32 (%struct.__sFILE*, i8*, ...)* @fprintf(%struct.__sFILE* %4, i8* getelementptr inbounds ([20 x i8]* @.str2, i32 0, i32 0), i32 %5, i32 %6) ; <i32> [#uses=0]
  %8 = load i32** %3                              ; <i32*> [#uses=1]
  store i32 0, i32* %8
  ret void
}

define void @print_array(i32 %size, i32* %A, i32* %ret) nounwind ssp {
  %1 = alloca i32, align 4                        ; <i32*> [#uses=2]
  %2 = alloca i32*, align 8                       ; <i32**> [#uses=2]
  %3 = alloca i32*, align 8                       ; <i32**> [#uses=2]
  %i = alloca i32, align 4                        ; <i32*> [#uses=6]
  store i32 %size, i32* %1
  store i32* %A, i32** %2
  store i32* %ret, i32** %3
  store i32 0, i32* %i
  br label %4

; <label>:4                                       ; preds = %16, %0
  %5 = load i32* %i                               ; <i32> [#uses=1]
  %6 = load i32* %1                               ; <i32> [#uses=1]
  %7 = icmp slt i32 %5, %6                        ; <i1> [#uses=1]
  br i1 %7, label %8, label %19

; <label>:8                                       ; preds = %4
  %9 = load %struct.__sFILE** @__stdoutp          ; <%struct.__sFILE*> [#uses=1]
  %10 = load i32* %i                              ; <i32> [#uses=1]
  %11 = load i32** %2                             ; <i32*> [#uses=1]
  %12 = sext i32 %10 to i64                       ; <i64> [#uses=1]
  %13 = getelementptr inbounds i32* %11, i64 %12  ; <i32*> [#uses=1]
  %14 = load i32* %13                             ; <i32> [#uses=1]
  %15 = call i32 (%struct.__sFILE*, i8*, ...)* @fprintf(%struct.__sFILE* %9, i8* getelementptr inbounds ([4 x i8]* @.str3, i32 0, i32 0), i32 %14) ; <i32> [#uses=0]
  br label %16

; <label>:16                                      ; preds = %8
  %17 = load i32* %i                              ; <i32> [#uses=1]
  %18 = add nsw i32 %17, 1                        ; <i32> [#uses=1]
  store i32 %18, i32* %i
  br label %4

; <label>:19                                      ; preds = %4
  %20 = load %struct.__sFILE** @__stdoutp         ; <%struct.__sFILE*> [#uses=1]
  %21 = call i32 (%struct.__sFILE*, i8*, ...)* @fprintf(%struct.__sFILE* %20, i8* getelementptr inbounds ([2 x i8]* @.str4, i32 0, i32 0)) ; <i32> [#uses=0]
  %22 = load i32* %i                              ; <i32> [#uses=1]
  %23 = load i32** %3                             ; <i32*> [#uses=1]
  store i32 %22, i32* %23
  ret void
}

define void @print_array2(i32 %size, i32* %A, i32* %ret) nounwind ssp {
  %1 = alloca i32, align 4                        ; <i32*> [#uses=4]
  %2 = alloca i32*, align 8                       ; <i32**> [#uses=2]
  %3 = alloca i32*, align 8                       ; <i32**> [#uses=2]
  %i = alloca i32, align 4                        ; <i32*> [#uses=6]
  %j = alloca i32, align 4                        ; <i32*> [#uses=7]
  store i32 %size, i32* %1
  store i32* %A, i32** %2
  store i32* %ret, i32** %3
  %4 = load i32* %1                               ; <i32> [#uses=1]
  %5 = zext i32 %4 to i64                         ; <i64> [#uses=1]
  %6 = mul i64 4, %5                              ; <i64> [#uses=1]
  store i32 0, i32* %i
  store i32 0, i32* %j
  br label %7

; <label>:7                                       ; preds = %33, %0
  %8 = load i32* %i                               ; <i32> [#uses=1]
  %9 = load i32* %1                               ; <i32> [#uses=1]
  %10 = icmp slt i32 %8, %9                       ; <i1> [#uses=1]
  br i1 %10, label %11, label %36

; <label>:11                                      ; preds = %7
  store i32 0, i32* %j
  br label %12

; <label>:12                                      ; preds = %29, %11
  %13 = load i32* %j                              ; <i32> [#uses=1]
  %14 = load i32* %1                              ; <i32> [#uses=1]
  %15 = icmp slt i32 %13, %14                     ; <i1> [#uses=1]
  br i1 %15, label %16, label %32

; <label>:16                                      ; preds = %12
  %17 = load %struct.__sFILE** @__stdoutp         ; <%struct.__sFILE*> [#uses=1]
  %18 = load i32* %j                              ; <i32> [#uses=1]
  %19 = load i32* %i                              ; <i32> [#uses=1]
  %20 = load i32** %2                             ; <i32*> [#uses=1]
  %21 = sext i32 %19 to i64                       ; <i64> [#uses=1]
  %22 = mul i64 %21, %6                           ; <i64> [#uses=1]
  %23 = udiv i64 %22, 4                           ; <i64> [#uses=1]
  %24 = getelementptr inbounds i32* %20, i64 %23  ; <i32*> [#uses=1]
  %25 = sext i32 %18 to i64                       ; <i64> [#uses=1]
  %26 = getelementptr inbounds i32* %24, i64 %25  ; <i32*> [#uses=1]
  %27 = load i32* %26                             ; <i32> [#uses=1]
  %28 = call i32 (%struct.__sFILE*, i8*, ...)* @fprintf(%struct.__sFILE* %17, i8* getelementptr inbounds ([4 x i8]* @.str3, i32 0, i32 0), i32 %27) ; <i32> [#uses=0]
  br label %29

; <label>:29                                      ; preds = %16
  %30 = load i32* %j                              ; <i32> [#uses=1]
  %31 = add nsw i32 %30, 1                        ; <i32> [#uses=1]
  store i32 %31, i32* %j
  br label %12

; <label>:32                                      ; preds = %12
  br label %33

; <label>:33                                      ; preds = %32
  %34 = load i32* %i                              ; <i32> [#uses=1]
  %35 = add nsw i32 %34, 1                        ; <i32> [#uses=1]
  store i32 %35, i32* %i
  br label %7

; <label>:36                                      ; preds = %7
  %37 = load %struct.__sFILE** @__stdoutp         ; <%struct.__sFILE*> [#uses=1]
  %38 = call i32 (%struct.__sFILE*, i8*, ...)* @fprintf(%struct.__sFILE* %37, i8* getelementptr inbounds ([2 x i8]* @.str4, i32 0, i32 0)) ; <i32> [#uses=0]
  %39 = load i32* %i                              ; <i32> [#uses=1]
  %40 = load i32* %j                              ; <i32> [#uses=1]
  %41 = add nsw i32 %39, %40                      ; <i32> [#uses=1]
  %42 = load i32** %3                             ; <i32*> [#uses=1]
  store i32 %41, i32* %42
  ret void
}

define void @print_array3(i32 %size, i32* %A, i32* %ret) nounwind ssp {
  %1 = alloca i32, align 4                        ; <i32*> [#uses=6]
  %2 = alloca i32*, align 8                       ; <i32**> [#uses=2]
  %3 = alloca i32*, align 8                       ; <i32**> [#uses=2]
  %i = alloca i32, align 4                        ; <i32*> [#uses=6]
  %j = alloca i32, align 4                        ; <i32*> [#uses=7]
  %k = alloca i32, align 4                        ; <i32*> [#uses=7]
  store i32 %size, i32* %1
  store i32* %A, i32** %2
  store i32* %ret, i32** %3
  %4 = load i32* %1                               ; <i32> [#uses=1]
  %5 = zext i32 %4 to i64                         ; <i64> [#uses=1]
  %6 = mul i64 4, %5                              ; <i64> [#uses=2]
  %7 = load i32* %1                               ; <i32> [#uses=1]
  %8 = zext i32 %7 to i64                         ; <i64> [#uses=1]
  %9 = mul i64 %6, %8                             ; <i64> [#uses=1]
  store i32 0, i32* %i
  store i32 0, i32* %j
  store i32 0, i32* %k
  br label %10

; <label>:10                                      ; preds = %50, %0
  %11 = load i32* %i                              ; <i32> [#uses=1]
  %12 = load i32* %1                              ; <i32> [#uses=1]
  %13 = icmp slt i32 %11, %12                     ; <i1> [#uses=1]
  br i1 %13, label %14, label %53

; <label>:14                                      ; preds = %10
  store i32 0, i32* %j
  br label %15

; <label>:15                                      ; preds = %46, %14
  %16 = load i32* %j                              ; <i32> [#uses=1]
  %17 = load i32* %1                              ; <i32> [#uses=1]
  %18 = icmp slt i32 %16, %17                     ; <i1> [#uses=1]
  br i1 %18, label %19, label %49

; <label>:19                                      ; preds = %15
  store i32 0, i32* %k
  br label %20

; <label>:20                                      ; preds = %42, %19
  %21 = load i32* %k                              ; <i32> [#uses=1]
  %22 = load i32* %1                              ; <i32> [#uses=1]
  %23 = icmp slt i32 %21, %22                     ; <i1> [#uses=1]
  br i1 %23, label %24, label %45

; <label>:24                                      ; preds = %20
  %25 = load %struct.__sFILE** @__stdoutp         ; <%struct.__sFILE*> [#uses=1]
  %26 = load i32* %k                              ; <i32> [#uses=1]
  %27 = load i32* %j                              ; <i32> [#uses=1]
  %28 = load i32* %i                              ; <i32> [#uses=1]
  %29 = load i32** %2                             ; <i32*> [#uses=1]
  %30 = sext i32 %28 to i64                       ; <i64> [#uses=1]
  %31 = mul i64 %30, %9                           ; <i64> [#uses=1]
  %32 = udiv i64 %31, 4                           ; <i64> [#uses=1]
  %33 = getelementptr inbounds i32* %29, i64 %32  ; <i32*> [#uses=1]
  %34 = sext i32 %27 to i64                       ; <i64> [#uses=1]
  %35 = mul i64 %34, %6                           ; <i64> [#uses=1]
  %36 = udiv i64 %35, 4                           ; <i64> [#uses=1]
  %37 = getelementptr inbounds i32* %33, i64 %36  ; <i32*> [#uses=1]
  %38 = sext i32 %26 to i64                       ; <i64> [#uses=1]
  %39 = getelementptr inbounds i32* %37, i64 %38  ; <i32*> [#uses=1]
  %40 = load i32* %39                             ; <i32> [#uses=1]
  %41 = call i32 (%struct.__sFILE*, i8*, ...)* @fprintf(%struct.__sFILE* %25, i8* getelementptr inbounds ([4 x i8]* @.str3, i32 0, i32 0), i32 %40) ; <i32> [#uses=0]
  br label %42

; <label>:42                                      ; preds = %24
  %43 = load i32* %k                              ; <i32> [#uses=1]
  %44 = add nsw i32 %43, 1                        ; <i32> [#uses=1]
  store i32 %44, i32* %k
  br label %20

; <label>:45                                      ; preds = %20
  br label %46

; <label>:46                                      ; preds = %45
  %47 = load i32* %j                              ; <i32> [#uses=1]
  %48 = add nsw i32 %47, 1                        ; <i32> [#uses=1]
  store i32 %48, i32* %j
  br label %15

; <label>:49                                      ; preds = %15
  br label %50

; <label>:50                                      ; preds = %49
  %51 = load i32* %i                              ; <i32> [#uses=1]
  %52 = add nsw i32 %51, 1                        ; <i32> [#uses=1]
  store i32 %52, i32* %i
  br label %10

; <label>:53                                      ; preds = %10
  %54 = load %struct.__sFILE** @__stdoutp         ; <%struct.__sFILE*> [#uses=1]
  %55 = call i32 (%struct.__sFILE*, i8*, ...)* @fprintf(%struct.__sFILE* %54, i8* getelementptr inbounds ([2 x i8]* @.str4, i32 0, i32 0)) ; <i32> [#uses=0]
  %56 = load i32* %i                              ; <i32> [#uses=1]
  %57 = load i32* %j                              ; <i32> [#uses=1]
  %58 = add nsw i32 %56, %57                      ; <i32> [#uses=1]
  %59 = load i32* %k                              ; <i32> [#uses=1]
  %60 = add nsw i32 %58, %59                      ; <i32> [#uses=1]
  %61 = load i32** %3                             ; <i32*> [#uses=1]
  store i32 %60, i32* %61
  ret void
}

define void @print_float_array(i32 %size, float* %A, i32* %ret) nounwind ssp {
  %1 = alloca i32, align 4                        ; <i32*> [#uses=2]
  %2 = alloca float*, align 8                     ; <float**> [#uses=2]
  %3 = alloca i32*, align 8                       ; <i32**> [#uses=2]
  %i = alloca i32, align 4                        ; <i32*> [#uses=6]
  store i32 %size, i32* %1
  store float* %A, float** %2
  store i32* %ret, i32** %3
  store i32 0, i32* %i
  br label %4

; <label>:4                                       ; preds = %17, %0
  %5 = load i32* %i                               ; <i32> [#uses=1]
  %6 = load i32* %1                               ; <i32> [#uses=1]
  %7 = icmp slt i32 %5, %6                        ; <i1> [#uses=1]
  br i1 %7, label %8, label %20

; <label>:8                                       ; preds = %4
  %9 = load %struct.__sFILE** @__stdoutp          ; <%struct.__sFILE*> [#uses=1]
  %10 = load i32* %i                              ; <i32> [#uses=1]
  %11 = load float** %2                           ; <float*> [#uses=1]
  %12 = sext i32 %10 to i64                       ; <i64> [#uses=1]
  %13 = getelementptr inbounds float* %11, i64 %12 ; <float*> [#uses=1]
  %14 = load float* %13                           ; <float> [#uses=1]
  %15 = fpext float %14 to double                 ; <double> [#uses=1]
  %16 = call i32 (%struct.__sFILE*, i8*, ...)* @fprintf(%struct.__sFILE* %9, i8* getelementptr inbounds ([4 x i8]* @.str5, i32 0, i32 0), double %15) ; <i32> [#uses=0]
  br label %17

; <label>:17                                      ; preds = %8
  %18 = load i32* %i                              ; <i32> [#uses=1]
  %19 = add nsw i32 %18, 1                        ; <i32> [#uses=1]
  store i32 %19, i32* %i
  br label %4

; <label>:20                                      ; preds = %4
  %21 = load %struct.__sFILE** @__stdoutp         ; <%struct.__sFILE*> [#uses=1]
  %22 = call i32 (%struct.__sFILE*, i8*, ...)* @fprintf(%struct.__sFILE* %21, i8* getelementptr inbounds ([2 x i8]* @.str4, i32 0, i32 0)) ; <i32> [#uses=0]
  %23 = load i32* %i                              ; <i32> [#uses=1]
  %24 = load i32** %3                             ; <i32*> [#uses=1]
  store i32 %23, i32* %24
  ret void
}

define void @print_float_array2(i32 %size, i32 %size2, float* %A, i32* %ret) nounwind ssp {
  %1 = alloca i32, align 4                        ; <i32*> [#uses=2]
  %2 = alloca i32, align 4                        ; <i32*> [#uses=3]
  %3 = alloca float*, align 8                     ; <float**> [#uses=2]
  %4 = alloca i32*, align 8                       ; <i32**> [#uses=2]
  %i = alloca i32, align 4                        ; <i32*> [#uses=6]
  %j = alloca i32, align 4                        ; <i32*> [#uses=7]
  store i32 %size, i32* %1
  store i32 %size2, i32* %2
  store float* %A, float** %3
  store i32* %ret, i32** %4
  %5 = load i32* %2                               ; <i32> [#uses=1]
  %6 = zext i32 %5 to i64                         ; <i64> [#uses=1]
  %7 = mul i64 4, %6                              ; <i64> [#uses=1]
  store i32 0, i32* %i
  store i32 0, i32* %j
  br label %8

; <label>:8                                       ; preds = %35, %0
  %9 = load i32* %i                               ; <i32> [#uses=1]
  %10 = load i32* %1                              ; <i32> [#uses=1]
  %11 = icmp slt i32 %9, %10                      ; <i1> [#uses=1]
  br i1 %11, label %12, label %38

; <label>:12                                      ; preds = %8
  store i32 0, i32* %j
  br label %13

; <label>:13                                      ; preds = %31, %12
  %14 = load i32* %j                              ; <i32> [#uses=1]
  %15 = load i32* %2                              ; <i32> [#uses=1]
  %16 = icmp slt i32 %14, %15                     ; <i1> [#uses=1]
  br i1 %16, label %17, label %34

; <label>:17                                      ; preds = %13
  %18 = load %struct.__sFILE** @__stdoutp         ; <%struct.__sFILE*> [#uses=1]
  %19 = load i32* %j                              ; <i32> [#uses=1]
  %20 = load i32* %i                              ; <i32> [#uses=1]
  %21 = load float** %3                           ; <float*> [#uses=1]
  %22 = sext i32 %20 to i64                       ; <i64> [#uses=1]
  %23 = mul i64 %22, %7                           ; <i64> [#uses=1]
  %24 = udiv i64 %23, 4                           ; <i64> [#uses=1]
  %25 = getelementptr inbounds float* %21, i64 %24 ; <float*> [#uses=1]
  %26 = sext i32 %19 to i64                       ; <i64> [#uses=1]
  %27 = getelementptr inbounds float* %25, i64 %26 ; <float*> [#uses=1]
  %28 = load float* %27                           ; <float> [#uses=1]
  %29 = fpext float %28 to double                 ; <double> [#uses=1]
  %30 = call i32 (%struct.__sFILE*, i8*, ...)* @fprintf(%struct.__sFILE* %18, i8* getelementptr inbounds ([4 x i8]* @.str5, i32 0, i32 0), double %29) ; <i32> [#uses=0]
  br label %31

; <label>:31                                      ; preds = %17
  %32 = load i32* %j                              ; <i32> [#uses=1]
  %33 = add nsw i32 %32, 1                        ; <i32> [#uses=1]
  store i32 %33, i32* %j
  br label %13

; <label>:34                                      ; preds = %13
  br label %35

; <label>:35                                      ; preds = %34
  %36 = load i32* %i                              ; <i32> [#uses=1]
  %37 = add nsw i32 %36, 1                        ; <i32> [#uses=1]
  store i32 %37, i32* %i
  br label %8

; <label>:38                                      ; preds = %8
  %39 = load %struct.__sFILE** @__stdoutp         ; <%struct.__sFILE*> [#uses=1]
  %40 = call i32 (%struct.__sFILE*, i8*, ...)* @fprintf(%struct.__sFILE* %39, i8* getelementptr inbounds ([2 x i8]* @.str4, i32 0, i32 0)) ; <i32> [#uses=0]
  %41 = load i32* %i                              ; <i32> [#uses=1]
  %42 = load i32* %j                              ; <i32> [#uses=1]
  %43 = mul i32 %41, %42                          ; <i32> [#uses=1]
  %44 = load i32** %4                             ; <i32*> [#uses=1]
  store i32 %43, i32* %44
  ret void
}

define void @print_float_array3(i32 %size, float* %A, i32* %ret) nounwind ssp {
  %1 = alloca i32, align 4                        ; <i32*> [#uses=6]
  %2 = alloca float*, align 8                     ; <float**> [#uses=2]
  %3 = alloca i32*, align 8                       ; <i32**> [#uses=2]
  %i = alloca i32, align 4                        ; <i32*> [#uses=6]
  %j = alloca i32, align 4                        ; <i32*> [#uses=7]
  %k = alloca i32, align 4                        ; <i32*> [#uses=7]
  store i32 %size, i32* %1
  store float* %A, float** %2
  store i32* %ret, i32** %3
  %4 = load i32* %1                               ; <i32> [#uses=1]
  %5 = zext i32 %4 to i64                         ; <i64> [#uses=1]
  %6 = mul i64 4, %5                              ; <i64> [#uses=2]
  %7 = load i32* %1                               ; <i32> [#uses=1]
  %8 = zext i32 %7 to i64                         ; <i64> [#uses=1]
  %9 = mul i64 %6, %8                             ; <i64> [#uses=1]
  store i32 0, i32* %i
  store i32 0, i32* %j
  store i32 0, i32* %k
  br label %10

; <label>:10                                      ; preds = %51, %0
  %11 = load i32* %i                              ; <i32> [#uses=1]
  %12 = load i32* %1                              ; <i32> [#uses=1]
  %13 = icmp slt i32 %11, %12                     ; <i1> [#uses=1]
  br i1 %13, label %14, label %54

; <label>:14                                      ; preds = %10
  store i32 0, i32* %j
  br label %15

; <label>:15                                      ; preds = %47, %14
  %16 = load i32* %j                              ; <i32> [#uses=1]
  %17 = load i32* %1                              ; <i32> [#uses=1]
  %18 = icmp slt i32 %16, %17                     ; <i1> [#uses=1]
  br i1 %18, label %19, label %50

; <label>:19                                      ; preds = %15
  store i32 0, i32* %k
  br label %20

; <label>:20                                      ; preds = %43, %19
  %21 = load i32* %k                              ; <i32> [#uses=1]
  %22 = load i32* %1                              ; <i32> [#uses=1]
  %23 = icmp slt i32 %21, %22                     ; <i1> [#uses=1]
  br i1 %23, label %24, label %46

; <label>:24                                      ; preds = %20
  %25 = load %struct.__sFILE** @__stdoutp         ; <%struct.__sFILE*> [#uses=1]
  %26 = load i32* %k                              ; <i32> [#uses=1]
  %27 = load i32* %j                              ; <i32> [#uses=1]
  %28 = load i32* %i                              ; <i32> [#uses=1]
  %29 = load float** %2                           ; <float*> [#uses=1]
  %30 = sext i32 %28 to i64                       ; <i64> [#uses=1]
  %31 = mul i64 %30, %9                           ; <i64> [#uses=1]
  %32 = udiv i64 %31, 4                           ; <i64> [#uses=1]
  %33 = getelementptr inbounds float* %29, i64 %32 ; <float*> [#uses=1]
  %34 = sext i32 %27 to i64                       ; <i64> [#uses=1]
  %35 = mul i64 %34, %6                           ; <i64> [#uses=1]
  %36 = udiv i64 %35, 4                           ; <i64> [#uses=1]
  %37 = getelementptr inbounds float* %33, i64 %36 ; <float*> [#uses=1]
  %38 = sext i32 %26 to i64                       ; <i64> [#uses=1]
  %39 = getelementptr inbounds float* %37, i64 %38 ; <float*> [#uses=1]
  %40 = load float* %39                           ; <float> [#uses=1]
  %41 = fpext float %40 to double                 ; <double> [#uses=1]
  %42 = call i32 (%struct.__sFILE*, i8*, ...)* @fprintf(%struct.__sFILE* %25, i8* getelementptr inbounds ([4 x i8]* @.str5, i32 0, i32 0), double %41) ; <i32> [#uses=0]
  br label %43

; <label>:43                                      ; preds = %24
  %44 = load i32* %k                              ; <i32> [#uses=1]
  %45 = add nsw i32 %44, 1                        ; <i32> [#uses=1]
  store i32 %45, i32* %k
  br label %20

; <label>:46                                      ; preds = %20
  br label %47

; <label>:47                                      ; preds = %46
  %48 = load i32* %j                              ; <i32> [#uses=1]
  %49 = add nsw i32 %48, 1                        ; <i32> [#uses=1]
  store i32 %49, i32* %j
  br label %15

; <label>:50                                      ; preds = %15
  br label %51

; <label>:51                                      ; preds = %50
  %52 = load i32* %i                              ; <i32> [#uses=1]
  %53 = add nsw i32 %52, 1                        ; <i32> [#uses=1]
  store i32 %53, i32* %i
  br label %10

; <label>:54                                      ; preds = %10
  %55 = load %struct.__sFILE** @__stdoutp         ; <%struct.__sFILE*> [#uses=1]
  %56 = call i32 (%struct.__sFILE*, i8*, ...)* @fprintf(%struct.__sFILE* %55, i8* getelementptr inbounds ([2 x i8]* @.str4, i32 0, i32 0)) ; <i32> [#uses=0]
  %57 = load i32* %i                              ; <i32> [#uses=1]
  %58 = load i32* %j                              ; <i32> [#uses=1]
  %59 = mul i32 %57, %58                          ; <i32> [#uses=1]
  %60 = load i32* %k                              ; <i32> [#uses=1]
  %61 = mul i32 %59, %60                          ; <i32> [#uses=1]
  %62 = load i32** %3                             ; <i32*> [#uses=1]
  store i32 %61, i32* %62
  ret void
}

define void @fabs_add_point_5(float %sum, float* %ret) nounwind ssp {
  %1 = alloca float, align 4                      ; <float*> [#uses=2]
  %2 = alloca float*, align 8                     ; <float**> [#uses=2]
  store float %sum, float* %1
  store float* %ret, float** %2
  %3 = load float* %1                             ; <float> [#uses=1]
  %4 = call float @fabsf(float %3)                ; <float> [#uses=1]
  %5 = fadd float %4, 5.000000e-01                ; <float> [#uses=1]
  %6 = load float** %2                            ; <float*> [#uses=1]
  store float %5, float* %6
  ret void
}

declare float @fabsf(float)

define void @read_image(i32 %size, float* %O, i32* %ret) nounwind ssp {
  %1 = alloca i32, align 4                        ; <i32*> [#uses=6]
  %2 = alloca float*, align 8                     ; <float**> [#uses=2]
  %3 = alloca i32*, align 8                       ; <i32**> [#uses=3]
  %fp = alloca %struct.__sFILE*, align 8          ; <%struct.__sFILE**> [#uses=5]
  %4 = alloca i8*                                 ; <i8**> [#uses=2]
  %counter = alloca i32, align 4                  ; <i32*> [#uses=4]
  %i = alloca i32, align 4                        ; <i32*> [#uses=6]
  %j = alloca i32, align 4                        ; <i32*> [#uses=7]
  store i32 %size, i32* %1
  store float* %O, float** %2
  store i32* %ret, i32** %3
  %5 = load i32* %1                               ; <i32> [#uses=1]
  %6 = zext i32 %5 to i64                         ; <i64> [#uses=1]
  %7 = mul i64 4, %6                              ; <i64> [#uses=1]
  store %struct.__sFILE* null, %struct.__sFILE** %fp
  %8 = call %struct.__sFILE* @"\01_fopen"(i8* getelementptr inbounds ([9 x i8]* @.str6, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8]* @.str7, i32 0, i32 0)) ; <%struct.__sFILE*> [#uses=1]
  store %struct.__sFILE* %8, %struct.__sFILE** %fp
  %9 = load %struct.__sFILE** %fp                 ; <%struct.__sFILE*> [#uses=1]
  %10 = icmp eq %struct.__sFILE* %9, null         ; <i1> [#uses=1]
  br i1 %10, label %11, label %13

; <label>:11                                      ; preds = %0
  %12 = load i32** %3                             ; <i32*> [#uses=1]
  store i32 0, i32* %12
  br label %13

; <label>:13                                      ; preds = %11, %0
  %14 = call i8* @llvm.stacksave()                ; <i8*> [#uses=1]
  store i8* %14, i8** %4
  %15 = load i32* %1                              ; <i32> [#uses=1]
  %16 = load i32* %1                              ; <i32> [#uses=1]
  %17 = mul i32 %15, %16                          ; <i32> [#uses=1]
  %18 = zext i32 %17 to i64                       ; <i64> [#uses=1]
  %19 = mul i64 1, %18                            ; <i64> [#uses=2]
  %20 = trunc i64 %19 to i32                      ; <i32> [#uses=1]
  %21 = alloca i8, i32 %20, align 1               ; <i8*> [#uses=2]
  %22 = load %struct.__sFILE** %fp                ; <%struct.__sFILE*> [#uses=1]
  %23 = call i64 @fread(i8* %21, i64 1, i64 %19, %struct.__sFILE* %22) ; <i64> [#uses=1]
  %24 = trunc i64 %23 to i32                      ; <i32> [#uses=1]
  %25 = load i32** %3                             ; <i32*> [#uses=1]
  store i32 %24, i32* %25
  %26 = load %struct.__sFILE** %fp                ; <%struct.__sFILE*> [#uses=1]
  %27 = call i32 @fclose(%struct.__sFILE* %26)    ; <i32> [#uses=0]
  store i32 0, i32* %counter
  store i32 0, i32* %i
  br label %28

; <label>:28                                      ; preds = %64, %13
  %29 = load i32* %i                              ; <i32> [#uses=1]
  %30 = load i32* %1                              ; <i32> [#uses=1]
  %31 = icmp slt i32 %29, %30                     ; <i1> [#uses=1]
  br i1 %31, label %32, label %67

; <label>:32                                      ; preds = %28
  store i32 0, i32* %j
  br label %33

; <label>:33                                      ; preds = %56, %32
  %34 = load i32* %j                              ; <i32> [#uses=1]
  %35 = load i32* %1                              ; <i32> [#uses=1]
  %36 = icmp slt i32 %34, %35                     ; <i1> [#uses=1]
  br i1 %36, label %37, label %59

; <label>:37                                      ; preds = %33
  %38 = load i32* %i                              ; <i32> [#uses=1]
  %39 = load i32* %j                              ; <i32> [#uses=1]
  %40 = add nsw i32 %38, %39                      ; <i32> [#uses=1]
  %41 = load i32* %counter                        ; <i32> [#uses=1]
  %42 = add nsw i32 %40, %41                      ; <i32> [#uses=1]
  %43 = sext i32 %42 to i64                       ; <i64> [#uses=1]
  %44 = getelementptr inbounds i8* %21, i64 %43   ; <i8*> [#uses=1]
  %45 = load i8* %44                              ; <i8> [#uses=1]
  %46 = uitofp i8 %45 to float                    ; <float> [#uses=1]
  %47 = load i32* %j                              ; <i32> [#uses=1]
  %48 = load i32* %i                              ; <i32> [#uses=1]
  %49 = load float** %2                           ; <float*> [#uses=1]
  %50 = sext i32 %48 to i64                       ; <i64> [#uses=1]
  %51 = mul i64 %50, %7                           ; <i64> [#uses=1]
  %52 = udiv i64 %51, 4                           ; <i64> [#uses=1]
  %53 = getelementptr inbounds float* %49, i64 %52 ; <float*> [#uses=1]
  %54 = sext i32 %47 to i64                       ; <i64> [#uses=1]
  %55 = getelementptr inbounds float* %53, i64 %54 ; <float*> [#uses=1]
  store float %46, float* %55
  br label %56

; <label>:56                                      ; preds = %37
  %57 = load i32* %j                              ; <i32> [#uses=1]
  %58 = add nsw i32 %57, 1                        ; <i32> [#uses=1]
  store i32 %58, i32* %j
  br label %33

; <label>:59                                      ; preds = %33
  %60 = load i32* %j                              ; <i32> [#uses=1]
  %61 = sub nsw i32 %60, 1                        ; <i32> [#uses=1]
  %62 = load i32* %counter                        ; <i32> [#uses=1]
  %63 = add nsw i32 %62, %61                      ; <i32> [#uses=1]
  store i32 %63, i32* %counter
  br label %64

; <label>:64                                      ; preds = %59
  %65 = load i32* %i                              ; <i32> [#uses=1]
  %66 = add nsw i32 %65, 1                        ; <i32> [#uses=1]
  store i32 %66, i32* %i
  br label %28

; <label>:67                                      ; preds = %28
  %68 = load i8** %4                              ; <i8*> [#uses=1]
  call void @llvm.stackrestore(i8* %68)
  ret void
}

declare %struct.__sFILE* @"\01_fopen"(i8*, i8*)

declare i8* @llvm.stacksave() nounwind

declare void @llvm.stackrestore(i8*) nounwind

declare i64 @fread(i8*, i64, i64, %struct.__sFILE*)

declare i32 @fclose(%struct.__sFILE*)

define void @read_image_o(i32 %size, float* %O, i32* %ret) nounwind ssp {
  %1 = alloca i32, align 4                        ; <i32*> [#uses=3]
  %2 = alloca float*, align 8                     ; <float**> [#uses=2]
  %3 = alloca i32*, align 8                       ; <i32**> [#uses=3]
  %fp = alloca %struct.__sFILE*, align 8          ; <%struct.__sFILE**> [#uses=5]
  %4 = alloca i8*                                 ; <i8**> [#uses=2]
  %i = alloca i32, align 4                        ; <i32*> [#uses=6]
  store i32 %size, i32* %1
  store float* %O, float** %2
  store i32* %ret, i32** %3
  store %struct.__sFILE* null, %struct.__sFILE** %fp
  %5 = call %struct.__sFILE* @"\01_fopen"(i8* getelementptr inbounds ([9 x i8]* @.str6, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8]* @.str7, i32 0, i32 0)) ; <%struct.__sFILE*> [#uses=1]
  store %struct.__sFILE* %5, %struct.__sFILE** %fp
  %6 = load %struct.__sFILE** %fp                 ; <%struct.__sFILE*> [#uses=1]
  %7 = icmp eq %struct.__sFILE* %6, null          ; <i1> [#uses=1]
  br i1 %7, label %8, label %10

; <label>:8                                       ; preds = %0
  %9 = load i32** %3                              ; <i32*> [#uses=1]
  store i32 0, i32* %9
  br label %10

; <label>:10                                      ; preds = %8, %0
  %11 = call i8* @llvm.stacksave()                ; <i8*> [#uses=1]
  store i8* %11, i8** %4
  %12 = load i32* %1                              ; <i32> [#uses=1]
  %13 = zext i32 %12 to i64                       ; <i64> [#uses=1]
  %14 = mul i64 1, %13                            ; <i64> [#uses=2]
  %15 = trunc i64 %14 to i32                      ; <i32> [#uses=1]
  %16 = alloca i8, i32 %15, align 1               ; <i8*> [#uses=2]
  %17 = load %struct.__sFILE** %fp                ; <%struct.__sFILE*> [#uses=1]
  %18 = call i64 @fread(i8* %16, i64 1, i64 %14, %struct.__sFILE* %17) ; <i64> [#uses=1]
  %19 = trunc i64 %18 to i32                      ; <i32> [#uses=1]
  %20 = load i32** %3                             ; <i32*> [#uses=1]
  store i32 %19, i32* %20
  %21 = load %struct.__sFILE** %fp                ; <%struct.__sFILE*> [#uses=1]
  %22 = call i32 @fclose(%struct.__sFILE* %21)    ; <i32> [#uses=0]
  store i32 0, i32* %i
  br label %23

; <label>:23                                      ; preds = %37, %10
  %24 = load i32* %i                              ; <i32> [#uses=1]
  %25 = load i32* %1                              ; <i32> [#uses=1]
  %26 = icmp slt i32 %24, %25                     ; <i1> [#uses=1]
  br i1 %26, label %27, label %40

; <label>:27                                      ; preds = %23
  %28 = load i32* %i                              ; <i32> [#uses=1]
  %29 = sext i32 %28 to i64                       ; <i64> [#uses=1]
  %30 = getelementptr inbounds i8* %16, i64 %29   ; <i8*> [#uses=1]
  %31 = load i8* %30                              ; <i8> [#uses=1]
  %32 = uitofp i8 %31 to float                    ; <float> [#uses=1]
  %33 = load i32* %i                              ; <i32> [#uses=1]
  %34 = load float** %2                           ; <float*> [#uses=1]
  %35 = sext i32 %33 to i64                       ; <i64> [#uses=1]
  %36 = getelementptr inbounds float* %34, i64 %35 ; <float*> [#uses=1]
  store float %32, float* %36
  br label %37

; <label>:37                                      ; preds = %27
  %38 = load i32* %i                              ; <i32> [#uses=1]
  %39 = add nsw i32 %38, 1                        ; <i32> [#uses=1]
  store i32 %39, i32* %i
  br label %23

; <label>:40                                      ; preds = %23
  %41 = load i8** %4                              ; <i8*> [#uses=1]
  call void @llvm.stackrestore(i8* %41)
  ret void
}

define void @start_timer(i32* %ret) nounwind ssp {
  %1 = alloca i32*, align 8                       ; <i32**> [#uses=2]
  store i32* %ret, i32** %1
  %2 = call i64 @"\01_clock"()                    ; <i64> [#uses=1]
  store i64 %2, i64* @start
  %3 = load i32** %1                              ; <i32*> [#uses=1]
  store i32 0, i32* %3
  ret void
}

declare i64 @"\01_clock"()

define void @print_time(i32* %ret) nounwind ssp {
  %1 = alloca i32*, align 8                       ; <i32**> [#uses=2]
  %cpu_time_used = alloca double, align 8         ; <double*> [#uses=2]
  store i32* %ret, i32** %1
  %2 = call i64 @"\01_clock"()                    ; <i64> [#uses=1]
  store i64 %2, i64* @end
  %3 = load i64* @end                             ; <i64> [#uses=1]
  %4 = load i64* @start                           ; <i64> [#uses=1]
  %5 = sub i64 %3, %4                             ; <i64> [#uses=1]
  %6 = uitofp i64 %5 to double                    ; <double> [#uses=1]
  %7 = fdiv double %6, 1.000000e+06               ; <double> [#uses=1]
  store double %7, double* %cpu_time_used
  %8 = load %struct.__sFILE** @__stderrp          ; <%struct.__sFILE*> [#uses=1]
  %9 = load double* %cpu_time_used                ; <double> [#uses=1]
  %10 = fmul double %9, 1.000000e+03              ; <double> [#uses=1]
  %11 = call i32 (%struct.__sFILE*, i8*, ...)* @fprintf(%struct.__sFILE* %8, i8* getelementptr inbounds ([28 x i8]* @.str8, i32 0, i32 0), double %10) ; <i32> [#uses=0]
  %12 = load i32** %1                             ; <i32*> [#uses=1]
  store i32 0, i32* %12
  ret void
}

define void @mysqrt(float %temp, float* %ret) nounwind ssp {
  %1 = alloca float, align 4                      ; <float*> [#uses=2]
  %2 = alloca float*, align 8                     ; <float**> [#uses=2]
  store float %temp, float* %1
  store float* %ret, float** %2
  %3 = load float* %1                             ; <float> [#uses=1]
  %4 = call float @sqrtf(float %3)                ; <float> [#uses=1]
  %5 = load float** %2                            ; <float*> [#uses=1]
  store float %4, float* %5
  ret void
}

declare float @sqrtf(float) readnone

define void @log_base_2(i32 %NN, i32* %ret) nounwind ssp {
  %1 = alloca i32, align 4                        ; <i32*> [#uses=2]
  %2 = alloca i32*, align 8                       ; <i32**> [#uses=2]
  store i32 %NN, i32* %1
  store i32* %ret, i32** %2
  %3 = load i32* %1                               ; <i32> [#uses=1]
  %4 = sitofp i32 %3 to double                    ; <double> [#uses=1]
  %5 = call double @log2(double %4)               ; <double> [#uses=1]
  %6 = fptosi double %5 to i32                    ; <i32> [#uses=1]
  %7 = load i32** %2                              ; <i32*> [#uses=1]
  store i32 %6, i32* %7
  ret void
}

declare double @log2(double)

define void @bit_shift_loop(i32 %k, i32 %j, i32* %rk, i32* %rj) nounwind ssp {
  %1 = alloca i32, align 4                        ; <i32*> [#uses=2]
  %2 = alloca i32, align 4                        ; <i32*> [#uses=2]
  %3 = alloca i32*, align 8                       ; <i32**> [#uses=5]
  %4 = alloca i32*, align 8                       ; <i32**> [#uses=4]
  store i32 %k, i32* %1
  store i32 %j, i32* %2
  store i32* %rk, i32** %3
  store i32* %rj, i32** %4
  %5 = load i32* %1                               ; <i32> [#uses=1]
  %6 = load i32** %3                              ; <i32*> [#uses=1]
  store i32 %5, i32* %6
  %7 = load i32* %2                               ; <i32> [#uses=1]
  %8 = load i32** %4                              ; <i32*> [#uses=1]
  store i32 %7, i32* %8
  br label %9

; <label>:9                                       ; preds = %15, %0
  %10 = load i32** %3                             ; <i32*> [#uses=1]
  %11 = load i32* %10                             ; <i32> [#uses=1]
  %12 = load i32** %4                             ; <i32*> [#uses=1]
  %13 = load i32* %12                             ; <i32> [#uses=1]
  %14 = icmp sle i32 %11, %13                     ; <i1> [#uses=1]
  br i1 %14, label %15, label %24

; <label>:15                                      ; preds = %9
  %16 = load i32** %3                             ; <i32*> [#uses=1]
  %17 = load i32* %16                             ; <i32> [#uses=1]
  %18 = load i32** %4                             ; <i32*> [#uses=2]
  %19 = load i32* %18                             ; <i32> [#uses=1]
  %20 = sub nsw i32 %19, %17                      ; <i32> [#uses=1]
  store i32 %20, i32* %18
  %21 = load i32** %3                             ; <i32*> [#uses=2]
  %22 = load i32* %21                             ; <i32> [#uses=1]
  %23 = ashr i32 %22, 1                           ; <i32> [#uses=1]
  store i32 %23, i32* %21
  br label %9

; <label>:24                                      ; preds = %9
  ret void
}

; ModuleID = 'print.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"
target triple = "x86_64-apple-darwin10.0.0"

%struct.__sFILE = type { i8*, i32, i32, i16, i16, %struct.__sbuf, i32, i8*, i32 (i8*)*, i32 (i8*, i8*, i32)*, i64 (i8*, i64, i32)*, i32 (i8*, i8*, i32)*, %struct.__sbuf, %struct.__sFILEX*, i32, [3 x i8], [1 x i8], %struct.__sbuf, i32, i64 }
%struct.__sFILEX = type opaque
%struct.__sbuf = type { i8*, i32 }

@__stdoutp = external global %struct.__sFILE*     ; <%struct.__sFILE**> [#uses=8]
@.str = private constant [24 x i8] c"Got a single value: %d\0A\00" ; <[24 x i8]*> [#uses=1]
@.str1 = private constant [20 x i8] c"Got values: %d, %d\0A\00" ; <[20 x i8]*> [#uses=1]
@.str2 = private constant [4 x i8] c"%d\09\00"    ; <[4 x i8]*> [#uses=1]
@.str3 = private constant [2 x i8] c"\0A\00"      ; <[2 x i8]*> [#uses=1]

define void @p1(i32 %a, i32* %ret) nounwind ssp {
  %1 = alloca i32, align 4                        ; <i32*> [#uses=2]
  %2 = alloca i32*, align 8                       ; <i32**> [#uses=2]
  store i32 %a, i32* %1
  store i32* %ret, i32** %2
  %3 = load %struct.__sFILE** @__stdoutp          ; <%struct.__sFILE*> [#uses=1]
  %4 = load i32* %1                               ; <i32> [#uses=1]
  %5 = call i32 (%struct.__sFILE*, i8*, ...)* @fprintf(%struct.__sFILE* %3, i8* getelementptr inbounds ([24 x i8]* @.str, i32 0, i32 0), i32 %4) ; <i32> [#uses=0]
  %6 = load i32** %2                              ; <i32*> [#uses=1]
  store i32 1, i32* %6
  ret void
}

declare i32 @fprintf(%struct.__sFILE*, i8*, ...)

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
  %7 = call i32 (%struct.__sFILE*, i8*, ...)* @fprintf(%struct.__sFILE* %4, i8* getelementptr inbounds ([20 x i8]* @.str1, i32 0, i32 0), i32 %5, i32 %6) ; <i32> [#uses=0]
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
  %15 = call i32 (%struct.__sFILE*, i8*, ...)* @fprintf(%struct.__sFILE* %9, i8* getelementptr inbounds ([4 x i8]* @.str2, i32 0, i32 0), i32 %14) ; <i32> [#uses=0]
  br label %16

; <label>:16                                      ; preds = %8
  %17 = load i32* %i                              ; <i32> [#uses=1]
  %18 = add nsw i32 %17, 1                        ; <i32> [#uses=1]
  store i32 %18, i32* %i
  br label %4

; <label>:19                                      ; preds = %4
  %20 = load %struct.__sFILE** @__stdoutp         ; <%struct.__sFILE*> [#uses=1]
  %21 = call i32 (%struct.__sFILE*, i8*, ...)* @fprintf(%struct.__sFILE* %20, i8* getelementptr inbounds ([2 x i8]* @.str3, i32 0, i32 0)) ; <i32> [#uses=0]
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
  %28 = call i32 (%struct.__sFILE*, i8*, ...)* @fprintf(%struct.__sFILE* %17, i8* getelementptr inbounds ([4 x i8]* @.str2, i32 0, i32 0), i32 %27) ; <i32> [#uses=0]
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
  %38 = call i32 (%struct.__sFILE*, i8*, ...)* @fprintf(%struct.__sFILE* %37, i8* getelementptr inbounds ([2 x i8]* @.str3, i32 0, i32 0)) ; <i32> [#uses=0]
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
  %41 = call i32 (%struct.__sFILE*, i8*, ...)* @fprintf(%struct.__sFILE* %25, i8* getelementptr inbounds ([4 x i8]* @.str2, i32 0, i32 0), i32 %40) ; <i32> [#uses=0]
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
  %55 = call i32 (%struct.__sFILE*, i8*, ...)* @fprintf(%struct.__sFILE* %54, i8* getelementptr inbounds ([2 x i8]* @.str3, i32 0, i32 0)) ; <i32> [#uses=0]
  %56 = load i32* %i                              ; <i32> [#uses=1]
  %57 = load i32* %j                              ; <i32> [#uses=1]
  %58 = add nsw i32 %56, %57                      ; <i32> [#uses=1]
  %59 = load i32* %k                              ; <i32> [#uses=1]
  %60 = add nsw i32 %58, %59                      ; <i32> [#uses=1]
  %61 = load i32** %3                             ; <i32*> [#uses=1]
  store i32 %60, i32* %61
  ret void
}

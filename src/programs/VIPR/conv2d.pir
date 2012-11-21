Program [(DeclareFun (Procedure "clamp"[(Variable "fin"Float32)] [(Variable "iout"Int8)](Block [(If (RBinop "<"(Ref (VariableRef "fin"))(Ref (Constant "0.0")))(Block [(Assign (VariableRef "iout")(Cast Int8(Ref (Constant "0"))))])(Block [(If (RBinop ">"(Ref (VariableRef "fin"))(Ref (Constant "255.0")))(Block [(Assign (VariableRef "iout")(Cast Int8(Ref (Constant "255"))))])(Block [(Assign (VariableRef "iout")(Cast Int8(Ref (VariableRef "fin"))))]))]))]))),(DeclareFun (Procedure "conv2Dproduct"[(Array "image"(Aggregate [7, 7] Int8)), (Array "kernel"(Aggregate [7, 7] Float32))] [(Variable "result"Float32)](Block [(DeclareAndAssign (Variable "temp"Float32)(Ref (Constant "0.0"))),(For (Variable "i"Int32s)(Ref (Constant "0"))(RBinop "<="(Ref (VariableRef "i"))(Ref (Constant "6")))(Assign (VariableRef "i")(Binop "+"(Ref (VariableRef "i"))(Ref (Constant "1"))))(Block [(For (Variable "j"Int32s)(Ref (Constant "0"))(RBinop "<="(Ref (VariableRef "j"))(Ref (Constant "6")))(Assign (VariableRef "j")(Binop "+"(Ref (VariableRef "j"))(Ref (Constant "1"))))(Block [(DeclareAndAssign (Variable "fimg"Float32)(Cast Float32(Ref (DynamicArrayRef "image"(DynamicIndex [(Ref (VariableRef "i")), (Ref (VariableRef "j"))]))))),(Assign (VariableRef "temp")(Binop "+"(Ref (VariableRef "temp"))(Binop "*"(Cast Float32(Ref (DynamicArrayRef "image"(DynamicIndex [(Ref (VariableRef "i")), (Ref (VariableRef "j"))]))))(Ref (DynamicArrayRef "kernel"(DynamicIndex [(Ref (VariableRef "i")), (Ref (VariableRef "j"))]))))))]))])),(Assign (VariableRef "result")(Ref (VariableRef "temp")))]))),(DeclareFun (Procedure "conv2D"[(Array "image"(Aggregate [7, 7] Int8)), (Array "kernel"(Aggregate [7, 7] Float32)), (Variable "kernelsum"Float32)] [(Variable "result"Int8)](Block [(DeclareAndAssign (Variable "ftemp"Float32)(Ref (Constant "0.0"))),(CallFun "conv2Dproduct"[(VariableRef "image"), (VariableRef "kernel")][(VariableRef "ftemp")]),(DeclareAndAssign (Variable "fsum"Float32)(Cast Float32(Ref (VariableRef "kernelsum")))),(If (RBinop ">"(Ref (VariableRef "fsum"))(Ref (Constant "0.0")))(Block [(Assign (VariableRef "ftemp")(Binop "/"(Ref (VariableRef "ftemp"))(Ref (VariableRef "fsum"))))])(Block [(Noop)])),(CallFun "clamp"[(VariableRef "ftemp")][(VariableRef "result")])]))),(DeclareFun (Procedure "conv2DLuma"[(Array "image"(Aggregate [54, 720] Int8)), (Array "kernel"(Aggregate [7, 7] Float32)), (Variable "kernelsum"Float32)] [(Array "result"(Aggregate [54, 720] Int8))](Block [(Declare (Array "temp"(Aggregate [7, 7] Int8))),(Par (Variable "i"Int32s)(Ref (Constant "0"))(RBinop "<="(Ref (VariableRef "i"))(Ref (Constant "48")))(Assign (VariableRef "i")(Binop "+"(Ref (VariableRef "i"))(Ref (Constant "1"))))(Block [(Par (Variable "j"Int32s)(Ref (Constant "0"))(RBinop "<="(Ref (VariableRef "j"))(Ref (Constant "714")))(Assign (VariableRef "j")(Binop "+"(Ref (VariableRef "j"))(Ref (Constant "1"))))(Block [(Par (Variable "t"Int32s)(Ref (Constant "0"))(RBinop "<="(Ref (VariableRef "t"))(Ref (Constant "6")))(Assign (VariableRef "t")(Binop "+"(Ref (VariableRef "t"))(Ref (Constant "1"))))(Block [(Par (Variable "w"Int32s)(Ref (Constant "0"))(RBinop "<="(Ref (VariableRef "w"))(Ref (Constant "6")))(Assign (VariableRef "w")(Binop "+"(Ref (VariableRef "w"))(Ref (Constant "1"))))(Block [(Assign (DynamicArrayRef "temp"(DynamicIndex [(Ref (VariableRef "t")), (Ref (VariableRef "w"))]))(Ref (DynamicArrayRef "image"(DynamicIndex [(Binop "+"(Ref (VariableRef "t"))(Ref (VariableRef "i"))), (Binop "+"(Ref (VariableRef "w"))(Ref (VariableRef "j")))]))))]))])),(CallFun "conv2D"[(VariableRef "temp"), (VariableRef "kernel"), (VariableRef "kernelsum")][(DynamicArrayRef "result"(DynamicIndex [(Binop "+"(Ref (VariableRef "i"))(Ref (Constant "3"))), (Binop "+"(Ref (VariableRef "j"))(Ref (Constant "3")))]))])]))]))]))),(DeclareFun (Procedure "ksum"[(Array "kernel"(Aggregate [7, 7] Float32))] [(Variable "sum"Float32)](Block [(Assign (VariableRef "sum")(Ref (Constant "0.0"))),(Par (Variable "_L49_sum"Int32s)(Ref (Constant "0"))(RBinop "<="(Ref (VariableRef "_L49_sum"))(Ref (Constant "48")))(Assign (VariableRef "_L49_sum")(Binop "+"(Ref (VariableRef "_L49_sum"))(Ref (Constant "1"))))(Block [(Assign (VariableRef "sum")(Binop "+"(Ref (VariableRef "sum"))(Ref (DynamicArrayRef "kernel"(DynamicIndex [(Binop "%"(Binop "/"(Ref (VariableRef "_L49_sum"))(Ref (Constant "7")))(Ref (Constant "7"))), (Binop "%"(Ref (VariableRef "_L49_sum"))(Ref (Constant "7")))])))))]))]))),(DeclareFun (Procedure "copy_uv"[(Array "in_plane"(Aggregate [27, 360] Int8))] [(Array "out_plane"(Aggregate [27, 360] Int8))](Block [(Par (Variable "i"Int32s)(Ref (Constant "0"))(RBinop "<="(Ref (VariableRef "i"))(Ref (Constant "26")))(Assign (VariableRef "i")(Binop "+"(Ref (VariableRef "i"))(Ref (Constant "1"))))(Block [(Par (Variable "j"Int32s)(Ref (Constant "0"))(RBinop "<="(Ref (VariableRef "j"))(Ref (Constant "359")))(Assign (VariableRef "j")(Binop "+"(Ref (VariableRef "j"))(Ref (Constant "1"))))(Block [(Assign (DynamicArrayRef "out_plane"(DynamicIndex [(Ref (VariableRef "i")), (Ref (VariableRef "j"))]))(Ref (DynamicArrayRef "in_plane"(DynamicIndex [(Ref (VariableRef "i")), (Ref (VariableRef "j"))]))))]))]))]))),(DeclareEntry (Procedure "MAIN"[(Variable "task_id"Int32), (Array "y"(Aggregate [54, 720] Int8)), (Array "u"(Aggregate [27, 360] Int8)), (Array "v"(Aggregate [27, 360] Int8))] [(Array "outy"(Aggregate [54, 720] Int8)), (Array "outu"(Aggregate [27, 360] Int8)), (Array "outv"(Aggregate [27, 360] Int8))](Block [(Declare (Array "kernel"(Aggregate [7, 7] Float32))),(Par (Variable "_L49_kernel"Int32s)(Ref (Constant "0"))(RBinop "<="(Ref (VariableRef "_L49_kernel"))(Ref (Constant "48")))(Assign (VariableRef "_L49_kernel")(Binop "+"(Ref (VariableRef "_L49_kernel"))(Ref (Constant "1"))))(Block [(Assign (DynamicArrayRef "kernel"(DynamicIndex [(Binop "%"(Binop "/"(Ref (VariableRef "_L49_kernel"))(Ref (Constant "7")))(Ref (Constant "7"))), (Binop "%"(Ref (VariableRef "_L49_kernel"))(Ref (Constant "7")))]))(Ref (Constant "1.0")))])),(Declare (Variable "sum"Float32)),(CallFun "ksum"[(VariableRef "kernel")][(VariableRef "sum")]),(CallFun "conv2DLuma"[(VariableRef "y"), (VariableRef "kernel"), (VariableRef "sum")][(VariableRef "outy")]),(CallFun "copy_uv"[(VariableRef "u")][(VariableRef "outu")]),(CallFun "copy_uv"[(VariableRef "v")][(VariableRef "outv")])])))]

Program [(DeclareFun (Procedure "clamp"[(Variable "fin"Float32)] [(Variable "iout"Int8)](Block [(If (RBinop "<"(Ref (VariableRef "fin"))(Ref (Constant "0.0")))(Block [(Assign (VariableRef "iout")(Cast Int8(Ref (Constant "0"))))])(Block [(If (RBinop ">"(Ref (VariableRef "fin"))(Ref (Constant "255.0")))(Block [(Assign (VariableRef "iout")(Cast Int8(Ref (Constant "255"))))])(Block [(Assign (VariableRef "iout")(Cast Int8(Ref (VariableRef "fin"))))]))]))]))),(DeclareFun (Procedure "gray"[(Variable "yin"Int8)] [(Variable "yout"Int8), (Variable "uout"Int8), (Variable "vout"Int8)](Block [(Assign (VariableRef "yout")(Ref (VariableRef "yin"))),(Assign (VariableRef "uout")(Cast Int8(Ref (Constant "128")))),(Assign (VariableRef "vout")(Cast Int8(Ref (Constant "128"))))]))),(DeclareFun (Procedure "extrapolate"[(Variable "source"Int8), (Variable "degenerate"Int8), (Variable "alpha"Float32)] [(Variable "result"Int8)](Block [(DeclareAndAssign (Variable "intermediate"Float32)(Binop "+"(Binop "*"(Binop "-"(Ref (Constant "1.0"))(Binop "-"(Binop "/"(Ref (VariableRef "alpha"))(Ref (Constant "255.0")))(Ref (Constant "1.0"))))(Cast Float32(Ref (VariableRef "source"))))(Binop "*"(Binop "-"(Binop "/"(Ref (VariableRef "alpha"))(Ref (Constant "255.0")))(Ref (Constant "1.0")))(Cast Float32(Ref (VariableRef "degenerate")))))),(CallFun "clamp"[(VariableRef "intermediate")][(VariableRef "result")])]))),(DeclareFun (Procedure "yuvtorgb"[(Variable "yin"Int8), (Variable "uin"Int8), (Variable "vin"Int8)] [(Variable "rout"Int8), (Variable "gout"Int8), (Variable "bout"Int8)](Block [(DeclareAndAssign (Variable "ydash"Float32)(Binop "/"(Binop "-"(Cast Float32(Ref (VariableRef "yin")))(Ref (Constant "16.0")))(Ref (Constant "219.0")))),(DeclareAndAssign (Variable "udash"Float32)(Binop "/"(Binop "-"(Cast Float32(Ref (VariableRef "uin")))(Ref (Constant "128.0")))(Ref (Constant "224.0")))),(DeclareAndAssign (Variable "vdash"Float32)(Binop "/"(Binop "-"(Cast Float32(Ref (VariableRef "vin")))(Ref (Constant "128.0")))(Ref (Constant "224.0")))),(DeclareAndAssign (Variable "interr"Float32)(Binop "*"(Ref (Constant "255.0"))(Binop "+"(Ref (VariableRef "ydash"))(Binop "*"(Ref (Constant "1.402"))(Ref (VariableRef "vdash")))))),(DeclareAndAssign (Variable "interg"Float32)(Binop "*"(Ref (Constant "255.0"))(Binop "-"(Binop "-"(Ref (VariableRef "ydash"))(Binop "*"(Ref (Constant "0.344136"))(Ref (VariableRef "udash"))))(Binop "*"(Ref (Constant "0.714136"))(Ref (VariableRef "vdash")))))),(DeclareAndAssign (Variable "interb"Float32)(Binop "*"(Ref (Constant "255.0"))(Binop "+"(Ref (VariableRef "ydash"))(Binop "*"(Ref (Constant "1.772"))(Ref (VariableRef "udash")))))),(CallFun "clamp"[(VariableRef "interr")][(VariableRef "rout")]),(CallFun "clamp"[(VariableRef "interg")][(VariableRef "gout")]),(CallFun "clamp"[(VariableRef "interb")][(VariableRef "bout")])]))),(DeclareFun (Procedure "rgbtoyuv"[(Variable "rin"Int8), (Variable "gin"Int8), (Variable "bin"Int8)] [(Variable "yout"Int8), (Variable "uout"Int8), (Variable "vout"Int8)](Block [(DeclareAndAssign (Variable "rdash"Float32)(Binop "/"(Cast Float32(Ref (VariableRef "rin")))(Ref (Constant "255.0")))),(DeclareAndAssign (Variable "gdash"Float32)(Binop "/"(Cast Float32(Ref (VariableRef "gin")))(Ref (Constant "255.0")))),(DeclareAndAssign (Variable "bdash"Float32)(Binop "/"(Cast Float32(Ref (VariableRef "bin")))(Ref (Constant "255.0")))),(DeclareAndAssign (Variable "ydash"Float32)(Binop "+"(Binop "*"(Ref (Constant "0.299"))(Ref (VariableRef "rdash")))(Binop "+"(Binop "*"(Ref (Constant "0.587"))(Ref (VariableRef "gdash")))(Binop "*"(Ref (Constant "0.114"))(Ref (VariableRef "bdash")))))),(DeclareAndAssign (Variable "udash"Float32)(Binop "+"(Binop "*"(Unop "-"(Ref (Constant "0.168736")))(Ref (VariableRef "rdash")))(Binop "+"(Binop "*"(Unop "-"(Ref (Constant "0.331264")))(Ref (VariableRef "gdash")))(Binop "*"(Ref (Constant "0.5"))(Ref (VariableRef "bdash")))))),(DeclareAndAssign (Variable "vdash"Float32)(Binop "-"(Binop "-"(Binop "*"(Ref (Constant "0.5"))(Ref (VariableRef "rdash")))(Binop "*"(Ref (Constant "0.418688"))(Ref (VariableRef "gdash"))))(Binop "*"(Ref (Constant "0.081312"))(Ref (VariableRef "bdash"))))),(DeclareAndAssign (Variable "intery"Float32)(Binop "+"(Binop "*"(Ref (VariableRef "ydash"))(Ref (Constant "219.0")))(Ref (Constant "16.0")))),(DeclareAndAssign (Variable "interu"Float32)(Binop "+"(Binop "*"(Ref (VariableRef "udash"))(Ref (Constant "224.0")))(Ref (Constant "128.0")))),(DeclareAndAssign (Variable "interv"Float32)(Binop "+"(Binop "*"(Ref (VariableRef "vdash"))(Ref (Constant "224.0")))(Ref (Constant "128.0")))),(CallFun "clamp"[(VariableRef "intery")][(VariableRef "yout")]),(CallFun "clamp"[(VariableRef "interu")][(VariableRef "uout")]),(CallFun "clamp"[(VariableRef "interv")][(VariableRef "vout")])]))),(DeclareEntry (Procedure "MAIN"[(Variable "task_id"Int32), (Array "img_y"(Aggregate [54, 720] Int8)), (Array "img_u"(Aggregate [27, 360] Int8)), (Array "img_v"(Aggregate [27, 360] Int8))] [(Array "sat_y"(Aggregate [54, 720] Int8)), (Array "sat_u"(Aggregate [27, 360] Int8)), (Array "sat_v"(Aggregate [27, 360] Int8))](Block [(DeclareAndAssign (Variable "alpha"Float32)(Ref (Constant "0.5"))),(DeclareAndAssign (Variable "greyy"Int8)(Cast Int8(Ref (Constant "0")))),(DeclareAndAssign (Variable "greyu"Int8)(Cast Int8(Ref (Constant "0")))),(DeclareAndAssign (Variable "greyv"Int8)(Cast Int8(Ref (Constant "0")))),(DeclareAndAssign (Variable "greyr"Int8)(Cast Int8(Ref (Constant "0")))),(DeclareAndAssign (Variable "greyg"Int8)(Cast Int8(Ref (Constant "0")))),(DeclareAndAssign (Variable "greyb"Int8)(Cast Int8(Ref (Constant "0")))),(DeclareAndAssign (Variable "origr"Int8)(Cast Int8(Ref (Constant "0")))),(DeclareAndAssign (Variable "origg"Int8)(Cast Int8(Ref (Constant "0")))),(DeclareAndAssign (Variable "origb"Int8)(Cast Int8(Ref (Constant "0")))),(DeclareAndAssign (Variable "satr"Int8)(Cast Int8(Ref (Constant "0")))),(DeclareAndAssign (Variable "satb"Int8)(Cast Int8(Ref (Constant "0")))),(DeclareAndAssign (Variable "satg"Int8)(Cast Int8(Ref (Constant "0")))),(Par (Variable "i"Int32s)(Ref (Constant "0"))(RBinop "<="(Ref (VariableRef "i"))(Ref (Constant "53")))(Assign (VariableRef "i")(Binop "+"(Ref (VariableRef "i"))(Ref (Constant "1"))))(Block [(Par (Variable "j"Int32s)(Ref (Constant "0"))(RBinop "<="(Ref (VariableRef "j"))(Ref (Constant "719")))(Assign (VariableRef "j")(Binop "+"(Ref (VariableRef "j"))(Ref (Constant "1"))))(Block [(CallFun "gray"[(DynamicArrayRef "img_y"(DynamicIndex [(Ref (VariableRef "i")), (Ref (VariableRef "j"))]))][(VariableRef "greyy"), (VariableRef "greyu"), (VariableRef "greyv")]),(CallFun "yuvtorgb"[(VariableRef "greyy"), (VariableRef "greyu"), (VariableRef "greyv")][(VariableRef "greyr"), (VariableRef "greyg"), (VariableRef "greyb")]),(CallFun "yuvtorgb"[(DynamicArrayRef "img_y"(DynamicIndex [(Ref (VariableRef "i")), (Ref (VariableRef "j"))])), (DynamicArrayRef "img_u"(DynamicIndex [(Binop "/"(Ref (VariableRef "i"))(Ref (Constant "2"))), (Binop "/"(Ref (VariableRef "j"))(Ref (Constant "2")))])), (DynamicArrayRef "img_v"(DynamicIndex [(Binop "/"(Ref (VariableRef "i"))(Ref (Constant "2"))), (Binop "/"(Ref (VariableRef "j"))(Ref (Constant "2")))]))][(VariableRef "origr"), (VariableRef "origg"), (VariableRef "origb")]),(CallFun "extrapolate"[(VariableRef "origr"), (VariableRef "greyr"), (VariableRef "alpha")][(VariableRef "satr")]),(CallFun "extrapolate"[(VariableRef "origg"), (VariableRef "greyg"), (VariableRef "alpha")][(VariableRef "satg")]),(CallFun "extrapolate"[(VariableRef "origb"), (VariableRef "greyb"), (VariableRef "alpha")][(VariableRef "satb")]),(CallFun "rgbtoyuv"[(VariableRef "satr"), (VariableRef "satg"), (VariableRef "satb")][(DynamicArrayRef "sat_y"(DynamicIndex [(Ref (VariableRef "i")), (Ref (VariableRef "j"))])), (DynamicArrayRef "sat_u"(DynamicIndex [(Binop "/"(Ref (VariableRef "i"))(Ref (Constant "2"))), (Binop "/"(Ref (VariableRef "j"))(Ref (Constant "2")))])), (DynamicArrayRef "sat_v"(DynamicIndex [(Binop "/"(Ref (VariableRef "i"))(Ref (Constant "2"))), (Binop "/"(Ref (VariableRef "j"))(Ref (Constant "2")))]))])]))]))])))]
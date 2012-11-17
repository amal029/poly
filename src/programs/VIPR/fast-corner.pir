Program [(DeclareFun (Procedure "varMin"[(Array "data"(Aggregate [11] Int32s))] [(Variable "var"Int32s), (Variable "min"Int32s)](Block [(DeclareAndAssign (Variable "local"Int32s)(Ref (StaticArrayRef "data"(StaticIndex [0])))),(DeclareAndAssign (Variable "minFound"Int32s)(Ref (StaticArrayRef "data"(StaticIndex [0])))),(DeclareAndAssign (Variable "variance"Int32s)(Cast Int32s(Ref (Constant "0")))),(For (Variable "k"Int32s)(Ref (Constant "1"))(RBinop "<="(Ref (VariableRef "k"))(Ref (Constant "10")))(Assign (VariableRef "k")(Binop "+"(Ref (VariableRef "k"))(Ref (Constant "1"))))(Block [(Assign (VariableRef "variance")(Binop "+"(Ref (VariableRef "variance"))(Unop "abs"(Binop "-"(Ref (DynamicArrayRef "data"(DynamicIndex [(Ref (VariableRef "k"))])))(Ref (VariableRef "local")))))),(If (RBinop "<"(Ref (DynamicArrayRef "data"(DynamicIndex [(Ref (VariableRef "k"))])))(Ref (VariableRef "minFound")))(Block [(Assign (VariableRef "minFound")(Ref (DynamicArrayRef "data"(DynamicIndex [(Ref (VariableRef "k"))]))))])(Block [(Noop)]))])),(Assign (VariableRef "var")(Ref (VariableRef "variance"))),(Assign (VariableRef "min")(Ref (VariableRef "minFound")))]))),(DeclareFun (Procedure "fast25_7"[(Array "image"(Aggregate [7, 7] Int8))] [(Array "corners"(Aggregate [7, 7] Int8))](Block [(Declare (Array "workdata"(Aggregate [26] Int8))),(Assign (StaticArrayRef "workdata"(StaticIndex [0]))(Ref (StaticArrayRef "image"(StaticIndex [4, 0])))),(Assign (StaticArrayRef "workdata"(StaticIndex [1]))(Ref (StaticArrayRef "image"(StaticIndex [3, 0])))),(Assign (StaticArrayRef "workdata"(StaticIndex [2]))(Ref (StaticArrayRef "image"(StaticIndex [2, 0])))),(Assign (StaticArrayRef "workdata"(StaticIndex [3]))(Ref (StaticArrayRef "image"(StaticIndex [1, 1])))),(Assign (StaticArrayRef "workdata"(StaticIndex [4]))(Ref (StaticArrayRef "image"(StaticIndex [0, 2])))),(Assign (StaticArrayRef "workdata"(StaticIndex [5]))(Ref (StaticArrayRef "image"(StaticIndex [0, 3])))),(Assign (StaticArrayRef "workdata"(StaticIndex [6]))(Ref (StaticArrayRef "image"(StaticIndex [0, 4])))),(Assign (StaticArrayRef "workdata"(StaticIndex [5]))(Ref (StaticArrayRef "image"(StaticIndex [1, 5])))),(Assign (StaticArrayRef "workdata"(StaticIndex [8]))(Ref (StaticArrayRef "image"(StaticIndex [2, 6])))),(Assign (StaticArrayRef "workdata"(StaticIndex [9]))(Ref (StaticArrayRef "image"(StaticIndex [3, 6])))),(Assign (StaticArrayRef "workdata"(StaticIndex [10]))(Ref (StaticArrayRef "image"(StaticIndex [4, 6])))),(Assign (StaticArrayRef "workdata"(StaticIndex [11]))(Ref (StaticArrayRef "image"(StaticIndex [5, 5])))),(Assign (StaticArrayRef "workdata"(StaticIndex [12]))(Ref (StaticArrayRef "image"(StaticIndex [6, 4])))),(Assign (StaticArrayRef "workdata"(StaticIndex [13]))(Ref (StaticArrayRef "image"(StaticIndex [6, 3])))),(Assign (StaticArrayRef "workdata"(StaticIndex [14]))(Ref (StaticArrayRef "image"(StaticIndex [6, 2])))),(Assign (StaticArrayRef "workdata"(StaticIndex [15]))(Ref (StaticArrayRef "image"(StaticIndex [5, 1])))),(Assign (StaticArrayRef "workdata"(StaticIndex [16]))(Ref (StaticArrayRef "image"(StaticIndex [4, 0])))),(Assign (StaticArrayRef "workdata"(StaticIndex [17]))(Ref (StaticArrayRef "image"(StaticIndex [3, 0])))),(Assign (StaticArrayRef "workdata"(StaticIndex [18]))(Ref (StaticArrayRef "image"(StaticIndex [2, 0])))),(Assign (StaticArrayRef "workdata"(StaticIndex [19]))(Ref (StaticArrayRef "image"(StaticIndex [1, 1])))),(Assign (StaticArrayRef "workdata"(StaticIndex [20]))(Ref (StaticArrayRef "image"(StaticIndex [0, 2])))),(Assign (StaticArrayRef "workdata"(StaticIndex [21]))(Ref (StaticArrayRef "image"(StaticIndex [0, 3])))),(Assign (StaticArrayRef "workdata"(StaticIndex [22]))(Ref (StaticArrayRef "image"(StaticIndex [0, 4])))),(Assign (StaticArrayRef "workdata"(StaticIndex [23]))(Ref (StaticArrayRef "image"(StaticIndex [1, 5])))),(Assign (StaticArrayRef "workdata"(StaticIndex [24]))(Ref (StaticArrayRef "image"(StaticIndex [2, 6])))),(Assign (StaticArrayRef "workdata"(StaticIndex [25]))(Ref (StaticArrayRef "image"(StaticIndex [3, 6])))),(DeclareAndAssign (Variable "centre"Int32s)(Cast Int32s(Ref (StaticArrayRef "image"(StaticIndex [3, 3]))))),(DeclareAndAssign (Variable "found"Int8)(Cast Int8(Ref (Constant "0")))),(DeclareAndAssign (Variable "count"Int32)(Cast Int32(Ref (Constant "0")))),(Declare (Array "differences"(Aggregate [11] Int32s))),(Declare (Variable "var"Int32s)),(Declare (Variable "min"Int32s)),(For (Variable "i"Int32s)(Ref (Constant "0"))(RBinop "<="(Ref (VariableRef "i"))(Ref (Constant "15")))(Assign (VariableRef "i")(Binop "+"(Ref (VariableRef "i"))(Ref (Constant "1"))))(Block [(If (RBinop "=="(Ref (VariableRef "found"))(Cast Int8(Ref (Constant "0"))))(Block [(Par (Variable "j"Int32s)(Ref (Constant "0"))(RBinop "<="(Ref (VariableRef "j"))(Ref (Constant "10")))(Assign (VariableRef "j")(Binop "+"(Ref (VariableRef "j"))(Ref (Constant "1"))))(Block [(Assign (DynamicArrayRef "differences"(DynamicIndex [(Ref (VariableRef "j"))]))(Unop "abs"(Binop "-"(Cast Int32s(Ref (DynamicArrayRef "workdata"(DynamicIndex [(Binop "+"(Ref (VariableRef "i"))(Ref (VariableRef "j")))]))))(Ref (VariableRef "centre")))))])),(CallFun "varMin"[(VariableRef "differences")][(VariableRef "var"), (VariableRef "min")]),(If (And (RBinop "<"(Ref (VariableRef "var"))(Cast Int32s(Ref (Constant "20"))))(RBinop ">="(Ref (VariableRef "min"))(Cast Int32s(Ref (Constant "16")))))(Block [(Assign (VariableRef "found")(Cast Int8(Ref (Constant "1"))))])(Block [(Assign (VariableRef "count")(Binop "+"(Ref (VariableRef "count"))(Cast Int32(Ref (Constant "1")))))]))])(Block [(Noop)]))])),(If (RBinop "=="(Ref (VariableRef "found"))(Cast Int8(Ref (Constant "1"))))(Block [(DeclareArrayConst (Array "xcoords"(Aggregate [32] Int32s))["0", "0", "0", "1", "2", "3", "4", "5", "6", "6", "6", "5", "4", "3", "2", "1", "0", "0", "0", "1", "2", "3", "4", "5", "6", "6", "0", "0", "0", "0", "0", "0"]),(DeclareArrayConst (Array "ycoords"(Aggregate [32] Int32s))["4", "3", "2", "1", "0", "0", "0", "1", "2", "3", "4", "5", "6", "6", "6", "5", "4", "3", "2", "1", "0", "0", "0", "1", "2", "3", "0", "0", "0", "0", "0", "0"]),(Par (Variable "i"Int32s)(Ref (Constant "0"))(RBinop "<="(Ref (VariableRef "i"))(Ref (Constant "10")))(Assign (VariableRef "i")(Binop "+"(Ref (VariableRef "i"))(Ref (Constant "1"))))(Block [(Assign (DynamicArrayRef "corners"(DynamicIndex [(Ref (DynamicArrayRef "ycoords"(DynamicIndex [(Binop "+"(Ref (VariableRef "i"))(Ref (VariableRef "count")))]))), (Ref (DynamicArrayRef "xcoords"(DynamicIndex [(Binop "+"(Ref (VariableRef "i"))(Ref (VariableRef "count")))])))]))(Cast Int8(Ref (Constant "0"))))]))])(Block [(Noop)]))]))),(DeclareEntry (Procedure "MAIN"[(Variable "task_id"Int32), (Array "img_y"(Aggregate [54, 720] Int8)), (Array "img_u"(Aggregate [27, 360] Int8)), (Array "img_v"(Aggregate [27, 360] Int8))] [(Array "corners_y"(Aggregate [54, 720] Int8)), (Array "corners_u"(Aggregate [27, 360] Int8)), (Array "corners_v"(Aggregate [27, 360] Int8))](Block [(Par (Variable "i"Int32s)(Ref (Constant "0"))(RBinop "<="(Ref (VariableRef "i"))(Ref (Constant "53")))(Assign (VariableRef "i")(Binop "+"(Ref (VariableRef "i"))(Ref (Constant "1"))))(Block [(Par (Variable "j"Int32s)(Ref (Constant "0"))(RBinop "<="(Ref (VariableRef "j"))(Ref (Constant "719")))(Assign (VariableRef "j")(Binop "+"(Ref (VariableRef "j"))(Ref (Constant "1"))))(Block [(Assign (DynamicArrayRef "corners_y"(DynamicIndex [(Ref (VariableRef "i")), (Ref (VariableRef "j"))]))(Ref (DynamicArrayRef "img_y"(DynamicIndex [(Ref (VariableRef "i")), (Ref (VariableRef "j"))])))),(Assign (DynamicArrayRef "corners_u"(DynamicIndex [(Binop "/"(Ref (VariableRef "i"))(Ref (Constant "2"))), (Binop "/"(Ref (VariableRef "j"))(Ref (Constant "2")))]))(Ref (DynamicArrayRef "img_u"(DynamicIndex [(Binop "/"(Ref (VariableRef "i"))(Ref (Constant "2"))), (Binop "/"(Ref (VariableRef "j"))(Ref (Constant "2")))])))),(Assign (DynamicArrayRef "corners_v"(DynamicIndex [(Binop "/"(Ref (VariableRef "i"))(Ref (Constant "2"))), (Binop "/"(Ref (VariableRef "j"))(Ref (Constant "2")))]))(Ref (DynamicArrayRef "img_v"(DynamicIndex [(Binop "/"(Ref (VariableRef "i"))(Ref (Constant "2"))), (Binop "/"(Ref (VariableRef "j"))(Ref (Constant "2")))]))))]))])),(Declare (Array "local_img"(Aggregate [7, 7] Int8))),(Declare (Array "local_corners"(Aggregate [7, 7] Int8))),(Par (Variable "i"Int32s)(Ref (Constant "0"))(RBinop "<="(Ref (VariableRef "i"))(Ref (Constant "46")))(Assign (VariableRef "i")(Binop "+"(Ref (VariableRef "i"))(Ref (Constant "1"))))(Block [(Par (Variable "j"Int32s)(Ref (Constant "0"))(RBinop "<="(Ref (VariableRef "j"))(Ref (Constant "712")))(Assign (VariableRef "j")(Binop "+"(Ref (VariableRef "j"))(Ref (Constant "1"))))(Block [(Par (Variable "k"Int32s)(Ref (Constant "0"))(RBinop "<="(Ref (VariableRef "k"))(Ref (Constant "6")))(Assign (VariableRef "k")(Binop "+"(Ref (VariableRef "k"))(Ref (Constant "1"))))(Block [(Par (Variable "l"Int32s)(Ref (Constant "0"))(RBinop "<="(Ref (VariableRef "l"))(Ref (Constant "6")))(Assign (VariableRef "l")(Binop "+"(Ref (VariableRef "l"))(Ref (Constant "1"))))(Block [(Assign (DynamicArrayRef "local_img"(DynamicIndex [(Ref (VariableRef "k")), (Ref (VariableRef "l"))]))(Ref (DynamicArrayRef "img_y"(DynamicIndex [(Binop "+"(Ref (VariableRef "i"))(Ref (VariableRef "k"))), (Binop "+"(Ref (VariableRef "j"))(Ref (VariableRef "l")))])))),(Assign (DynamicArrayRef "local_corners"(DynamicIndex [(Ref (VariableRef "k")), (Ref (VariableRef "l"))]))(Ref (DynamicArrayRef "corners_y"(DynamicIndex [(Binop "+"(Ref (VariableRef "i"))(Ref (VariableRef "k"))), (Binop "+"(Ref (VariableRef "j"))(Ref (VariableRef "l")))]))))]))])),(CallFun "fast25_7"[(VariableRef "local_img")][(VariableRef "local_corners")]),(Par (Variable "k"Int32s)(Ref (Constant "0"))(RBinop "<="(Ref (VariableRef "k"))(Ref (Constant "6")))(Assign (VariableRef "k")(Binop "+"(Ref (VariableRef "k"))(Ref (Constant "1"))))(Block [(Par (Variable "l"Int32s)(Ref (Constant "0"))(RBinop "<="(Ref (VariableRef "l"))(Ref (Constant "6")))(Assign (VariableRef "l")(Binop "+"(Ref (VariableRef "l"))(Ref (Constant "1"))))(Block [(Assign (DynamicArrayRef "corners_y"(DynamicIndex [(Binop "+"(Ref (VariableRef "i"))(Ref (VariableRef "k"))), (Binop "+"(Ref (VariableRef "j"))(Ref (VariableRef "l")))]))(Ref (DynamicArrayRef "local_corners"(DynamicIndex [(Ref (VariableRef "k")), (Ref (VariableRef "l"))]))))]))]))]))]))])))]

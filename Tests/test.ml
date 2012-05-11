(* First the simple test *)
let parser = Xmllangparser.Xmllangparser.make () in
let _ = Xmllangparser.Xmllangparser.parse parser "../../Test/xml/simple.xml" in
();;

(* Second the complex test *)
let parser = Xmllangparser.Xmllangparser.make () in
let _ = Xmllangparser.Xmllangparser.parse parser "../../Test/xml/sat2D.xml" in
();;

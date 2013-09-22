structure PDDLToJSON = struct
local
    open JSON
    open PDDLToString
in
fun encodeSolution problem solution =
    let
        val encodeOldInstance = JSON.STRING o instanceToString problem
        (* TODO if somebody really wants Mike's 2:nd format *)
        fun encodeNewInstance _ = raise Fail "Not implemented"

        fun toOldPartialOrder' (first::(rest as second::_)) =
            JSON.ARRAY [encodeOldInstance first, encodeOldInstance second] :: toOldPartialOrder' rest
          | toOldPartialOrder' _ = nil
        fun toOldPartialOrder list = JSON.ARRAY (toOldPartialOrder' list)

        fun toNewPartialOrder' (first::(rest as second::_)) =
            JSON.ARRAY [encodeNewInstance first, encodeNewInstance second] :: toNewPartialOrder' rest
          | toNewPartialOrder' [last] = [JSON.ARRAY [encodeNewInstance last, JSON.STRING "goal"]]
          | toNewPartialOrder' nil = nil
        fun toNewPartialOrder list = JSON.ARRAY (toNewPartialOrder' list)

        fun toArray list = JSON.ARRAY (map encodeOldInstance list)
    in
        (* Magnus' format *)
        JSON.OBJECT [("plan", toArray solution)]
        (* (* Mike's old format *) *)
        (* JSON.OBJECT [("ordering", toOldPartialOrder solution)] *)
    end

fun saveSolution problem solution file =
    let
        val encoding = encodeSolution problem solution
        val file = TextIO.openOut file
    in
        JSONPrinter.print' {strm = file, pretty = true} encoding;
        TextIO.closeOut file
    end
end
end

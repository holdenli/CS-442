-- A RULE is a fact if the rhs is empty
class RULE
    creation make
    feature
        lhs : STRING
        rhs : ARRAY[STRING]
    feature
    make (l : STRING; r : ARRAY[STRING]) is
        do
            lhs := l
            rhs := clone(r)
        end
end -- class RULE

class DATA
    creation make
    feature
        rules : ARRAY[RULE]
    feature
    make (tok : TOKENIZER) is
        local
            s, lhs : STRING;
            rhs : ARRAY[STRING];
            rule : RULE
        do
            !!rules.make(1,0)
            from s := "" until s.is_equal("EOF")
            loop
                lhs := tok.next_token

                s := tok.next_token
                !!rhs.make(1,0)

                if s.is_equal(":-")  then
                    from
                        s := tok.next_token
                    until
                        s.is_equal(".")
                    loop
                        rhs.add_last(s)
                        s := tok.next_token
                    end
                end

                if not s.is_equal("EOF") then
                    !!rule.make(lhs,rhs)
                    rules.add_last(rule)
                end
            end
        end
end -- class DATA

class A6
    creation make
    feature
        rules : ARRAY[RULE]
    feature
    make is
        local
            i : INTEGER
            t : TOKENIZER
            q : ARRAY[STRING]
            data : DATA
            tree : TREE
        do
            !!t.make(argument(1))
            !!q.make(1,0)
            from
                i := 1
            until
                i.is_equal(argument_count)
            loop
                i := i + 1
                q.add_last(argument(i))
                -- print(argument(i))
                -- io.put_new_line
            end
            !!data.make(t)
            !!tree.make(q, data.rules, <<0>>)
        end

end -- class A6

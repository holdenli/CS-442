-- Search is done in constructor due to lack of foresight! HAHAHA.
class TREE
    creation make
    feature
        query : ARRAY[STRING]
        children : ARRAY[TREE] 
        matches : ARRAY[ARRAY[INTEGER]]
        cut : BOOLEAN
    
    feature {}
    print_array (a : ARRAY[INTEGER])  is
        local i : INTEGER
        do
            from i := 1
            until i = a.count
            loop
                i := i + 1
                io.put_integer(a.item(i))
                io.put_string(" ")
            end
            io.put_new_line
        end

    feature
    make (q : ARRAY[STRING] ; rules : ARRAY[RULE]; path : ARRAY[INTEGER]) is
        local
            i : INTEGER
            s, ss : STRING
            rule : RULE
            newq : ARRAY[STRING]
            child : TREE
            newp : ARRAY[INTEGER]
        do
            -- io.put_string("@@@@")
            -- print(q)
            -- io.put_new_line
            query := clone(q)

            if query.count < 1 then
                -- Found a match
                print_array(path)
                !!matches.make(1,1)
                matches.put(path,1)
            else
            !!children.make(1, rules.count)
            !!matches.make(1,0)
            cut := false
            
            -- Get first predicate
            s := query.first
            query.remove_first
            -- Skip cuts when seen
            if s.is_equal("!") then
                s := query.first
                query.remove_first
            end
            -- Check for cut following this predicate
            if query.count <= 0 then
                ss := ""
            else
                ss := query.first
            end

            -- Traverse the children (database)
            from i := 1
            until i > rules.count
            loop
                rule := rules.item(i)
                if s.is_equal(rule.lhs) then
                    newq := clone(rule.rhs)
                    newq.append_collection(query)

                    newp := clone(path)
                    newp.add_last(i)

                    !!child.make(newq, rules, newp)
                    children.put(child,i)
                    matches.append_collection(child.matches)

                    if ss.is_equal("!") then
                        i := rules.count
                        cut := true
                    end
                    if child.cut then
                        i := rules.count
                    end

                    -- print("----")
                    -- io.put_new_line
                end
                i := i + 1
            end
            -- print(q)
            -- io.put_new_line
            -- print(children)
            -- io.put_new_line
            -- print(matches)
            -- io.put_new_line
            end
        end
end -- class TREE

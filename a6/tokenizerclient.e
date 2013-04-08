class TOKENIZERCLIENT
  creation make
  feature
    tok : TOKENIZER
    a : ARRAY[STRING]
    z : RULE
    make is
      local s : STRING
      do
        !!tok.make(argument(1))
        !!a.make(1,0)
        !!z.make("HELLO", a)
        from
          s := ""
        until
          s.is_equal("EOF")
        loop
          s := tok.next_token
          io.put_string(s)
          io.put_new_line
        end
      end
end -- class TOKENIZERCLIENT

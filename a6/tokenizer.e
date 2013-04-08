class TOKENIZER
  creation make
  feature {}
    file : TEXT_FILE_READ
    s : STRING

  feature
    make(nm : STRING) is
      do
        !!file.connect_to(nm)
        s := ""
      end

    next_token : STRING is
      local
        c : CHARACTER
        done : BOOLEAN
      do
        if not file.is_connected then
           Result := "EOF"
        elseif file.end_of_input then
           Result := "EOF"
           file.disconnect
        else
           from
             done := false
           until
             done
           loop
             if s.is_equal(":-") or s.is_equal(".") then
                Result := s
                s := ""
                done := true
             else
                file.read_character
                if file.end_of_input then
                   Result := "EOF"
                   file.disconnect
                   done := true
                else
                   c := file.last_character
                   inspect c
                     when '.' then
                       if not s.is_equal("") then
                         Result := s
                         s := "."
                       else
                         Result := "."
                         s := ""
                       end
                       done := true
                     when ':' then
                       if not s.is_equal("") then
                          Result := s
                          file.read_character
                          s := ":-"
                          done := true
                       else
                          file.read_character
                          Result := ":-"
                          s := ""
                          done := true
                       end
                     when ',' then
                       if not s.is_equal("") then
                          Result := s
                          s := ""
                          done := true
                       end
                     when ' ', '%N' then
                       if not s.is_equal("") then
                          Result := s
                          s := ""
                          done := true
                       end
                     else
                       s.append_character(c)
                   end
                end
             end
           end
        end
      end
end -- class TOKENIZER

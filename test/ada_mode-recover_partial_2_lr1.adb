-- From a real editing session. LR1 parser encountered enqueue limit
-- in error recovery

--EMACS_SKIP_UNLESS:(eq ada-parser 'process)
--EMACSCMD:(switch-to-lr1)
--EMACSCMD:(setq debug-on-error t) ;; abort on enqueue limit
begin
   for Error of Errors loop
      for Op of Error.Recover.Ops loop
         case Op.Op is
            when Delete =>
               if Data.Terminals (Op.Token_Index).First then
                  if Op.Token_Index = Data.Terminals.Last_Index then

                     -- editing condition left next line exposed, with extra right paren

                     (not Data.Terminals (Op.Token_Index + 1).First))
                     then

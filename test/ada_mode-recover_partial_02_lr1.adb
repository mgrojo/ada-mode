-- -*- wisi-mckenzie-task-count: 1 -*-
-- From a real editing session. LR1 parser encountered enqueue limit.
--
-- Still requires a high enqueue limit, because there are two parsers
-- in recovery.

--EMACSCMD:(switch-to-lr1)

-- this must be after switch-to-lr1
--EMACSCMD:(setq wisi-mckenzie-enqueue-limit (* 2 58000))

-- We get different indent results from partial and incremental parse;
--EMACSCMD:(setq skip-reindent-test (not wisi-incremental-parse-enable))
begin
   for Error of Errors loop
      for Op of Error.Recover.Ops loop
         case Op.Op is
            when Delete =>
               if Data.Terminals (Op.Token_Index).First then
                  if Op.Token_Index = Data.Terminals.Last_Index then

                     -- Editing condition left next line exposed, with extra right paren.
                     --
                     -- Desired solution: insert <if> before '(' to match final 'then',
                     -- delete last ')', after 'then' insert 'end if; ...', but recover
                     -- does not find that.

                     (not Data.Terminals (Op.Token_Index + 1).First))
                     then

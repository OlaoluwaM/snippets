module SehllCmdRedirectParsers where

import Data.Attoparsec.Text

redirectSegmentParser =
                asum
                    [ space *> string "&>" *> optional space *> manyTill anyChar (space *> char '&') -- command &> <std_out_&_err_redirect_path> &
                    , space *> string "&>" *> optional space *> many anyChar -- command &> <std_out_&_err_redirect_path>
                    , space *> string ">>" *> optional space *> manyTill anyChar (space *> char '&') -- command >> <std_out_redirect_path> &
                    , space *> string ">>" *> optional space *> many anyChar -- command >> <std_out_redirect_path>
                    , space *> string "2>>" *> optional space *> manyTill anyChar (space *> char '&') -- command 2>> <std_err_redirect_path> &
                    , space *> string "2>>" *> optional space *> many anyChar -- command 2>> <std_err_redirect_path>
                    , space *> string "2>" *> optional space *> manyTill anyChar (space *> char '&') -- command 2> <std_err_redirect_path> &
                    , space *> string "2>" *> optional space *> many anyChar -- command 2> <std_err_redirect_path>
                    , space *> char '>' *> optional space *> manyTill anyChar (string "2&>1") <* (space *> char '&') -- command > <std_out_&_err_redirect_path> 2&>1 &
                    , space *> char '>' *> optional space *> manyTill anyChar (string "2&>1") -- command > <std_out_&_err_redirect_path> 2&>1
                    , space *> char '>' *> optional space *> manyTill anyChar (string "2>") *> manyTill anyChar (space *> char '&') -- command > <std_out_redirect_path> 2> <std_err_redirect_path> &
                    , space *> char '>' *> optional space *> manyTill anyChar (string "2>") *> many anyChar -- command > <std_out_redirect_path> 2> <std_err_redirect_path>
                    , space *> char '>' *> optional space *> manyTill anyChar (space *> char '&') -- command > <std_out_redirect_path> &
                    , space *> char '>' *> optional space *> many anyChar -- command > <std_out_redirect_path>
                    ]

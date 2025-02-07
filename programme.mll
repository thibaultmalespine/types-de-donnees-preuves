(* 1- ['0'-'9']* '\n' *) 
(* 2- '-'['0'-'9']* | ['0'-'9']* '\n' *) 
(* 3- ('O'|'o')('n'|"ff"|'N'|"FF"|"fF"|"Ff") *)
(* 4- (['a'-'f'] | ['0'-'9'])* *)
rule main = parse
'-'['0'-'9']* | ['0'-'9']* '.' '\n'
{ print_string "SUCCES !\n" } | _ { print_string "ECHEC !\n" }
{
main (Lexing.from_channel stdin)
}                                                                                   
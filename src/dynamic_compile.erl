%% Copyright (c) 2007
%%          Mats Cronqvist <mats.cronqvist@ericsson.com>
%%          Chris Newcombe <chris.newcombe@gmail.com> 
%%          Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% 
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%% 
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.

%%%-------------------------------------------------------------------
%%% File : dynamic_compile.erl
%%% Description :
%%% Authors : Mats Cronqvist <mats.cronqvist@ericsson.com>
%%%           Chris Newcombe <chris.newcombe@gmail.com> 
%%%           Jacob Vorreuter <jacob.vorreuter@gmail.com>
%%% TODO :
%%% - add support for limit include-file depth (and prevent circular references)
%%%   prevent circular macro expansion set FILE correctly when -module() is found
%%% -include_lib support $ENVVAR in include filenames
%%%  substitute-stringize (??MACRO)
%%% -undef/-ifdef/-ifndef/-else/-endif
%%% -file(File, Line)
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%%  modify by luoqng@qq.com
%%% - add support for macros which has the same name but different No. of parameters
%%% - add support for include_lib
%%% - add support for undef/-ifdef/-ifndef/-else/-endif/-author/-copyright/-desription/-module/-attribute/-vsn
%%% - add support for ?MODULE/?FILE/?LINE
%%%-------------------------------------------------------------------


-module(dynamic_compile).

%% API
-export([load_from_string/1, load_from_string/2]).
-export([from_string/1, from_string/2]).

-import(lists, [reverse/1, keyreplace/4]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function:
%% Description:
%%   Compile module from string and load into VM
%%--------------------------------------------------------------------
load_from_string(CodeStr) ->
    load_from_string(CodeStr, []).

load_from_string(CodeStr, CompileFormsOptions) ->
    {Mod, Bin} = from_string(CodeStr, CompileFormsOptions),
    code:load_binary(Mod, [], Bin).

%%--------------------------------------------------------------------
%% Function:
%% Description:
%%   Returns a binary that can be used with
%%           code:load_binary(Module, ModuleFilenameForInternalRecords, Binary).
%%--------------------------------------------------------------------
from_string(CodeStr) ->
    from_string(CodeStr, []).

% takes Options as for compile:forms/2
from_string(CodeStr, CompileFormsOptions) ->
    %% Initialise the macro dictionary with the default predefined macros,
    %% (adapted from epp.erl:predef_macros/1
    Filename = "compiled_from_string",
    %%Machine  = list_to_atom(erlang:system_info(machine)),
    Ms0    = dict:new(),
    % Ms1    = dict:store('FILE',          {[], "compiled_from_string"}, Ms0),
    % Ms2    = dict:store('LINE',          {[], 1}, Ms1),  % actually we might add special code for this
    % Ms3    = dict:store('MODULE',        {[], undefined},              Ms2),
    % Ms4    = dict:store('MODULE_STRING', {[], undefined},              Ms3),
    % Ms5    = dict:store('MACHINE',       {[], Machine},                Ms4),
    % InitMD = dict:store(Machine,         {[], true},                   Ms5),
    InitMD = Ms0,

    %% From the docs for compile:forms:
    %%    When encountering an -include or -include_dir directive, the compiler searches for header files in the following directories:
    %%      1. ".", the current working directory of the file server;
    %%      2. the base name of the compiled file;
    %%      3. the directories specified using the i option. The directory specified last is searched first.
    %% In this case, #2 is meaningless.
    IncludeSearchPath = ["." | reverse([Dir || {i, Dir} <- CompileFormsOptions])],
    case scan_and_parse(CodeStr, Filename, 1, [], [], InitMD, IncludeSearchPath) of
        {ok, RevForms, _OutMacroDict} ->
            Forms = [{attribute, 0, file, {"compiled_from_string", 0}}|reverse([{eof, 0}|RevForms])],

            %% note: 'binary' is forced as an implicit option, whether it is provided or not.
            case compile:forms(Forms, CompileFormsOptions) of
                {ok, ModuleName, CompiledCodeBinary} when is_binary(CompiledCodeBinary) ->
                    {ModuleName, CompiledCodeBinary};
                {ok, ModuleName, CompiledCodeBinary, []} when is_binary(CompiledCodeBinary) ->  % empty warnings list
                    {ModuleName, CompiledCodeBinary};
                {ok, ModuleName, CompiledCodeBinary, Warnings} ->
                    {ModuleName, CompiledCodeBinary, Warnings};
                {error, [{_, Errors}], Warnings} ->
                    {error, Errors, Warnings};
                Other ->
                    throw({?MODULE, compile_forms, Other})
            end;
        {error, Errors} ->
            {error, Errors, []}
    end.


%%====================================================================
%% Internal functions
%%====================================================================
%%% Code from Mats Cronqvist
%%% See http://www.erlang.org/pipermail/erlang-questions/2007-March/025507.html
%%%## 'scan_and_parse'
%%%
%%% basically we call the OTP scanner and parser (erl_scan and
%%% erl_parse) line-by-line, but check each scanned line for (or
%%% definitions of) macros before parsing.
%% returns {ReverseForms, FinalMacroDict}
scan_and_parse([], _CurrFilename, _CurrLine, RevForms, Errors, MacroDict, _IncludeSearchPath) ->
    case Errors of
        [] ->
            {ok, RevForms, MacroDict};
        _ ->
            {error, lists:flatten(lists:reverse(Errors))}
    end;

scan_and_parse(RemainingText, CurrFilename, CurrLine, RevForms, Errors, MacroDict, IncludeSearchPath) ->
    case scanner(RemainingText, CurrLine, MacroDict) of
        {tokens, NLine, NRemainingText, Toks} ->
            case erl_parse:parse_form(Toks) of
                {ok, Form0} ->
                    {Form, Forms} = normalize_record(Form0),

                    scan_and_parse(NRemainingText, CurrFilename, NLine, [ Form | Forms ++ RevForms], Errors, MacroDict, IncludeSearchPath);
                {error, E} ->
                    scan_and_parse(NRemainingText, CurrFilename, NLine, RevForms, [E|Errors], MacroDict, IncludeSearchPath)
            end;
        {macro, NLine, NRemainingText, NMacroDict} ->
            scan_and_parse(NRemainingText, CurrFilename, NLine, RevForms, Errors, NMacroDict, IncludeSearchPath);
        {Def, NLine, NRemainingText, NMacroDict} when Def=:= def; Def=:= endif; Def=:= else;
            Def =:= undef; Def =:= attribute ; Def =:= vsn->
            scan_and_parse(NRemainingText, CurrFilename, NLine, RevForms, Errors, NMacroDict, IncludeSearchPath);

        {vsn, NLine, NRemainingText, NMacroDict}  ->
            scan_and_parse(NRemainingText, CurrFilename, NLine, RevForms, Errors, NMacroDict, IncludeSearchPath);

        {Include, NLine, NRemainingText, IncludeFilename} when Include =:= include; Include =:= include_lib ->
            
            {IncludeCurrentFile, IncludeFileRemainingTextents} = case Include of
                include ->
                    read_include_file(IncludeFilename, CurrFilename);
                _ ->
                    read_include_lib_file(IncludeFilename, CurrFilename)

            end,

            %%io:format("include file ~p contents: ~n~p~nRemainingText = ~p~n", [IncludeFilename,IncludeFileRemainingTextents, RemainingText]),
            %% Modify the FILE macro to reflect the filename
            %%IncludeMacroDict = dict:store('FILE', {[],IncludeFilename}, MacroDict),
            IncludeMacroDict = MacroDict,

            %% Process the header file (inc. any nested header files)
            {ok, RevIncludeForms, IncludedMacroDict} = scan_and_parse(IncludeFileRemainingTextents, IncludeCurrentFile, 1, [], Errors, IncludeMacroDict, IncludeSearchPath),
            %io:format("include file results = ~p~n", [R]),
            %% Restore the FILE macro in the NEW MacroDict (so we keep any macros defined in the header file)
            %%NMacroDict = dict:store('FILE', {[],CurrFilename}, IncludedMacroDict),
            NMacroDict = IncludedMacroDict,

            %% Continue with the original file
            scan_and_parse(NRemainingText, CurrFilename, NLine, RevIncludeForms ++ RevForms, Errors, NMacroDict, IncludeSearchPath);
        {continue, Continuation} ->
            scan_and_parse([], CurrFilename, CurrLine, [Continuation|RevForms], Errors, MacroDict, IncludeSearchPath);
        done ->
            scan_and_parse([], CurrFilename, CurrLine, RevForms, Errors, MacroDict, IncludeSearchPath)
    end.


scanner(Text, Line, MacroDict) ->
    case erl_scan:tokens([],Text,Line) of
        {done, {ok, Toks, NLine}, LeftOverChars} ->
            case pre_proc(Toks, MacroDict) of
                {tokens,  NToks}      -> {tokens,  NLine, LeftOverChars, NToks};
                {macro,   NMacroDict} -> {macro,   NLine, LeftOverChars, NMacroDict};
                {include, Filename}   -> {include, NLine, LeftOverChars, Filename};
                {include_lib, Filename}   -> {include_lib, NLine, LeftOverChars, Filename};
                Def when Def=:= def; Def=:= endif; Def=:= else; Def =:= undef; Def =:= attribute; Def =:= vsn ->
                    {Def, NLine, LeftOverChars, MacroDict}
            end;
        {more, {erl_scan_continuation, _, _, Toks, NLine,  _, Any, _} = _Continuation} ->

            %% This is supposed to mean "term is not yet complete" (i.e. a '.' has
            %% not been reached yet).
            %% However, for some bizarre reason we also get this if there is a comment after the final '.' in a file.
            %% So we check to see if Text only consists of comments.
            case is_only_comments(Text) of
                true  ->
                    done;
                {false, _} ->

                    Header =  case string:to_integer(Any) of
                        {Int,[]} ->
                            [{dot, NLine} , {integer,NLine,Int}];
                        _ when is_list(Any) , Any =/= [] ->
                            [{dot, NLine} , {string, NLine, Any}];
                        _ ->
                            [{dot, NLine}]
                    end,

                    case pre_proc(lists:reverse(lists:concat([Header,Toks])), MacroDict) of
                        {tokens,  NToks}      -> {tokens,  NLine, [], NToks};
                        {macro,   NMacroDict} -> {macro,   NLine, [], NMacroDict};
                        {include, Filename}   -> {include, NLine, [], Filename};
                        {include_lib, Filename}   -> {include_lib, NLine, [], Filename};
                        Def when Def=:= def; Def=:= endif; Def=:= else; Def =:= undef; Def =:= attribute ; Def =:= vsn ->
                            {Def, NLine, [], MacroDict}

                    end
            end
    end.





is_endif(Text) ->
    is_endif(Text, not_in_endif).

is_endif([], _) -> false;
is_endif([$   |T], not_in_endif) ->  is_endif(T, not_in_endif);
is_endif([$\t |T], not_in_endif) ->  is_endif(T, not_in_endif);
is_endif([$\n |T], not_in_endif) ->  is_endif(T, not_in_endif);
is_endif("-endif.", not_in_endif) ->  true;
is_endif(_, not_in_endif) -> false.



is_only_comments(Text) ->
    is_only_comments(Text, not_in_comment).

is_only_comments([],       _)              -> true;
is_only_comments([$   |T], not_in_comment) -> is_only_comments(T, not_in_comment); % skipping whitspace outside of comment
is_only_comments([$\t |T], not_in_comment) -> is_only_comments(T, not_in_comment); % skipping whitspace outside of comment
is_only_comments([$\n |T], not_in_comment) -> is_only_comments(T, not_in_comment); % skipping whitspace outside of comment
is_only_comments([$\r |T], not_in_comment) -> is_only_comments(T, not_in_comment);
is_only_comments([$%  |T], not_in_comment) -> is_only_comments(T, in_comment);     % found start of a comment
is_only_comments(Text, not_in_comment) -> {false, Text};
% found any significant char NOT in a comment
is_only_comments([$\n |T], in_comment)     -> is_only_comments(T, not_in_comment); % found end of a comment
is_only_comments([_   |T], in_comment)     -> is_only_comments(T, in_comment).     % skipping over in-comment chars

%%%## 'pre-proc'
%%%
%%% have to implement a subset of the pre-processor, since epp insists
%%% on running on a file.
%%% only handles 2 cases;
%% -define(MACRO, something).
%% -define(MACRO(VAR1,VARN),{stuff,VAR1,more,stuff,VARN,extra,stuff}).
pre_proc([{'-',_},{atom,_,define},{'(',_},{_,_,Name}|DefToks],MacroDict) ->
    case DefToks of
        [{',',_} | Macro] ->
            {macro, dict:store(Name, {[], macro_body_def(Macro, [], [])},  MacroDict)};
        [{'(',_} | Macro] ->
            {macro, dict:store(Name, macro_params_body_def(Macro, []), MacroDict)}
    end;

pre_proc([{'-',_},{atom, _, vsn},{'(',_}, _, {')',_}, {dot,_}], _MacroDict) ->
    vsn;
pre_proc([{'-',_}, {atom,_,include}, {'(',_}, {string,_,Filename}, {')',_}, {dot,_}], _MacroDict) ->
    {include, Filename};

pre_proc([{'-',_}, {atom, _, include_lib}, {'(',_}, {string, _, Filename}, {')',_}, {dot,_}], _MacroDict) ->
    {include_lib, Filename};

pre_proc([{'-',_}, {atom,_,ifndef}, {'(',_}, _, {')',_}, {dot,_}], _MacroDict) ->
    def;
pre_proc([{'-',_}, {atom,_,ifdef}, {'(',_}, _, {')',_}, {dot,_}], _MacroDict) ->
    def;
pre_proc([{'-',_}, {atom,_,endif}, {dot, _}], _MacroDict) ->
    endif;
pre_proc([{'-',_}, {atom,_,else}, {dot, _}], _MacroDict) ->
    else;
pre_proc([{'-',_}, {atom,_,undef}, {'(',_}, _, {')',_},  {dot, _}], _MacroDict) ->
    undef;
pre_proc([{'-',_},{atom, _, author},{'(',_},{'{',_},{_, _, _},{',',_},{_, _, _},{'}',_},{')',_},{dot, _}], _MacroDict) ->
    attribute;
pre_proc([{'-',_}, {atom, _, author}, {'(',_}, _, {')',_}, {dot, _}], _MacroDict) ->
    attribute;
pre_proc([{'-',_}, {atom, _, copyright}, {'(',_}, {'{',_}, _, {',',_}, _, {'}',_}, {')',_}, {dot, _}], _MacroDict) ->
    attribute;
pre_proc([{'-',_}, {atom, _, copyright}, {'(',_}, _, {')',_}, {dot, _}], _MacroDict) ->
    attribute;
pre_proc([{'-',_}, {atom, _, description}, {'(',_}, _, {')',_}, {dot, _}], _MacroDict) ->
    attribute;
pre_proc([{'-',_}, {atom, _, module}, {'(',_}, _, {')',_}, {dot, _}], _MacroDict) ->
    attribute;
pre_proc(Toks, MacroDict) ->
    {tokens, subst_macros(Toks, MacroDict)}.

macro_params_body_def([{')',_},{',',_} | Toks], RevParams) ->
    {reverse(RevParams), macro_body_def(Toks, [], [])};
macro_params_body_def([{var,_,Param} | Toks], RevParams) ->
    macro_params_body_def(Toks, [Param | RevParams]);
macro_params_body_def([{',',_}, {var,_,Param} | Toks], RevParams) ->
    macro_params_body_def(Toks, [Param | RevParams]).

macro_body_def([{')',_}, {dot,_}], _, RevMacroBodyToks) ->
    reverse(RevMacroBodyToks);

macro_body_def([{')', Line}| Toks], LeftList, RevMacroBodyToks) ->
    macro_body_def(Toks, lists:delete('(', LeftList), [{')', Line} |RevMacroBodyToks]);

macro_body_def([{'(', Line}| Toks], [], RevMacroBodyToks) ->
    macro_body_def(Toks, ['('], [{'(', Line} |RevMacroBodyToks]);

macro_body_def([{'(',Line}| Toks], LeftList, RevMacroBodyToks) ->
    macro_body_def(Toks, ['(' | LeftList], [{'(', Line} |RevMacroBodyToks]);

macro_body_def([Tok|Toks], LeftList, RevMacroBodyToks) ->
    macro_body_def(Toks, LeftList, [Tok | RevMacroBodyToks]).

subst_macros(Toks, MacroDict) ->
    reverse(subst_macros_rev(Toks, MacroDict, [])).

%% returns a reversed list of tokes
subst_macros_rev([{'?',_}, {_, LineNum, 'MODULE'} | Toks], MacroDict, RevOutToks) ->
    {[], MacroValue} = dict:fetch('MODULE', MacroDict),
    subst_macros_rev(Toks, MacroDict, [{atom, LineNum, MacroValue}] ++ RevOutToks);

subst_macros_rev([{'?',_}, {_, LineNum, 'FILE'} | Toks], MacroDict, RevOutToks) ->
    {[], MacroValue} = dict:fetch('FILE', MacroDict),
    subst_macros_rev(Toks, MacroDict, [{string, LineNum, MacroValue}] ++ RevOutToks);

subst_macros_rev([{'?',_}, {_,LineNum,'LINE'} | Toks], MacroDict, RevOutToks) ->
    %% special-case for ?LINE, to avoid creating a new MacroDict for every line in the source file
    subst_macros_rev(Toks, MacroDict, [{integer, LineNum, LineNum}] ++ RevOutToks);

subst_macros_rev([{'?',_}, {_,_,Name}, {'(',_} = Paren | Toks], MacroDict, RevOutToks) ->

    case dict:is_key(Name, MacroDict) of
        true ->
            case dict:fetch(Name, MacroDict) of
                {[], MacroValue} ->
                    %% This macro does not have any vars, so ignore the fact that the invocation is followed by "(...stuff"
                    %% Recursively expand any macro calls inside this macro's value
                    %% TODO: avoid infinite expansion due to circular references (even indirect ones)
                    RevExpandedOtherMacrosToks = subst_macros_rev(MacroValue, MacroDict, []),
                    subst_macros_rev([Paren|Toks], MacroDict, RevExpandedOtherMacrosToks ++ RevOutToks);
                ParamsAndBody ->
                    %% This macro does have vars.
                    %% Collect all of the passe arguments, in an ordered list
                    {NToks, Arguments, Line} = subst_macros_args_express(Toks, []),
                    %% Expand the varibles
                    %%io:format("Toks: ~p ~n ParamsAndBody: ~p ~n Arguments: ~p ~n",[Toks, ParamsAndBody, Arguments]),

                    ExpandedParamsToks = subst_macros_subst_args_for_vars(ParamsAndBody, [], Arguments, Line),
                    %% Recursively expand any macro calls inside this macro's value
                    %% TODO: avoid infinite expansion due to circular references (even indirect ones)
                    RevExpandedOtherMacrosToks = subst_macros_rev(ExpandedParamsToks, MacroDict, []),
                    subst_macros_rev(NToks, MacroDict, RevExpandedOtherMacrosToks ++ RevOutToks)
            end;
        false ->
            subst_macros_rev(Toks, MacroDict, RevOutToks)
    end;


subst_macros_rev([{'?',_}, {_,_,Name} | Toks], MacroDict, RevOutToks) ->
    %% This macro invocation does not have arguments.
    %% Therefore the definition should not have parameters
    case dict:is_key(Name, MacroDict) of
        true ->
            {[], MacroValue} = dict:fetch(Name, MacroDict),

            %% Recursively expand any macro calls inside this macro's value
            %% TODO: avoid infinite expansion due to circular references (even indirect ones)
            RevExpandedOtherMacrosToks = subst_macros_rev(MacroValue, MacroDict, []),
            subst_macros_rev(Toks, MacroDict, RevExpandedOtherMacrosToks ++ RevOutToks);
        _ ->
            subst_macros_rev(Toks, MacroDict,  RevOutToks)
    end;

subst_macros_rev([Tok|Toks], MacroDict,  RevOutToks) ->
    subst_macros_rev(Toks, MacroDict, [Tok|RevOutToks]);
subst_macros_rev([], _MacroDict, RevOutToks) -> RevOutToks.

subst_macros_args_express([{var,_, ArgsName}, {')', Line} | Toks], RevArgs) ->
    {Toks, reverse([ArgsName | RevArgs]), Line};
subst_macros_args_express([{var,_, ArgsName}, {',',_} | Toks], RevArgs) ->
    subst_macros_args_express(Toks, [ArgsName | RevArgs]);
subst_macros_args_express(Toks, RevArgs) ->
    subst_macros_get_express(Toks, [], [], RevArgs).

subst_macros_get_express([{')',Line} | Toks], [], RevExpress, RevArgs) ->
    {Toks, reverse([reverse(RevExpress)|RevArgs]), Line};
subst_macros_get_express([{')',Line} | Toks], LeftList, RevExpress, RevArgs) ->
    subst_macros_get_express(Toks,  lists:delete('(', LeftList),[{')',Line} | RevExpress], RevArgs);
subst_macros_get_express([{'}',Line} | Toks], LeftList, RevExpress, RevArgs) ->
    subst_macros_get_express(Toks,  lists:delete('{', LeftList),[{'}',Line} | RevExpress], RevArgs);
subst_macros_get_express([{'end',Line} | Toks], LeftList, RevExpress, RevArgs) ->
    subst_macros_get_express(Toks,  lists:delete('begin', LeftList),[{'end',Line} | RevExpress], RevArgs);
subst_macros_get_express([{']',Line} | Toks], LeftList, RevExpress, RevArgs) ->
    subst_macros_get_express(Toks,  lists:delete('[', LeftList),[{']',Line} | RevExpress], RevArgs);
subst_macros_get_express([{',',_} | Toks], [], RevExpress, RevArgs) ->
    subst_macros_args_express(Toks, [reverse(RevExpress)|RevArgs]);
subst_macros_get_express([{',',Line} | Toks], LeftList, RevExpress, RevArgs) ->
    subst_macros_get_express(Toks,  LeftList,[{',',Line} | RevExpress], RevArgs);
subst_macros_get_express([{'(',Line} | Toks], LeftList, RevExpress, RevArgs) ->
    subst_macros_get_express(Toks,  ['('|LeftList],[{'(',Line} | RevExpress], RevArgs);
subst_macros_get_express([{'begin',Line} | Toks], LeftList, RevExpress, RevArgs) ->
    subst_macros_get_express(Toks,  ['begin'|LeftList],[{'begin',Line} | RevExpress], RevArgs);
subst_macros_get_express([{'[',Line} | Toks], LeftList, RevExpress, RevArgs) ->
    subst_macros_get_express(Toks,  ['['|LeftList],[{'[',Line} | RevExpress], RevArgs);
subst_macros_get_express([{'{',Line} | Toks], LeftList, RevExpress, RevArgs) ->
    subst_macros_get_express(Toks,  ['{'|LeftList],[{'{',Line} | RevExpress], RevArgs);
subst_macros_get_express([Tok |Toks], LeftList, RevExpress,RevArgs) ->
    subst_macros_get_express(Toks, LeftList, [Tok | RevExpress], RevArgs).


subst_macros_get_args([{')',_} | Toks], RevArgs) ->
    {Toks, reverse(RevArgs)};
subst_macros_get_args([{',',_}, {var,_,ArgName} | Toks], RevArgs) ->
    subst_macros_get_args(Toks, [ArgName| RevArgs]);
subst_macros_get_args([{var,_,ArgName} | Toks], RevArgs) ->
    subst_macros_get_args(Toks, [ArgName | RevArgs]).

subst_macros_subst_args_for_vars({[], BodyToks}, NBodyToks, [], Line) ->
    L = lists:map(fun(A) when is_tuple(A)->
        setelement(2, A, Line);
        (A) ->
            A
    end,
        BodyToks),
    lists:concat(lists:reverse([L|NBodyToks]));
subst_macros_subst_args_for_vars({[Param | Params], BodyToks}, NBodyToks, [Arg|Args], Line) ->
    case lists:splitwith(fun({_, _, Var}) when Var =:= Param ->
        false;
        (_) ->
            true
    end, BodyToks) of
        {L1, []} ->
            L = lists:map(fun(A) when is_tuple(A)->
                setelement(2, A, Line);
                (A) ->
                    A
            end, L1),
            lists:concat(lists:reverse([L|NBodyToks]));
        {L1, [_|L2]} ->
            Arg2 = case Arg of
                [_|_] ->
                    lists:map(fun(A) when is_tuple(A)->
                        setelement(2, A, Line);
                        (A) ->
                            A
                    end,
                        Arg);
                _ ->
                    [{var,1,Arg}]

            end,
            L3 = lists:map(fun(A) when is_tuple(A)->
                setelement(2, A, Line);
                (A) ->
                    A
            end, L1),
            subst_macros_subst_args_for_vars({Params, L2}, [Arg2, L3 | NBodyToks], Args, Line)

    end.





read_include_file(Filename, CurrFilename ) ->
    Dir1 = filename:dirname(CurrFilename),
    Dir2 = filename:dirname(Filename),
    Dir = lists:concat([Dir1, "/", Dir2]),
    BaseName = filename:basename(Filename),
    case file:path_open([Dir], BaseName, [read, raw, binary]) of
        {ok, IoDevice, FullName} ->
            {ok, Data} = file:read(IoDevice, filelib:file_size(FullName)),
            file:close(IoDevice),
            {FullName, binary_to_list(Data)};
        {error, Reason} ->
            throw({failed_to_read_include_file, Reason, Filename, CurrFilename})
    end.

get_inlcude_lib_path(FileName) ->
    case filename:split(FileName) of
        [LibName| _]  when LibName =/= "/" ->
            OTPLIB = code:lib_dir(),
            case z_lib:list_file(OTPLIB, 1) of
                {ok, [_|_] = FileList } ->
                    [ FilePath] = [ lists:concat([OTPLIB, "/",  File,  "/","include"]) || {File, _, directory, _, _}  <- FileList, string:str(File, LibName) > 0],
                    {ok, FilePath};
                _ ->
                    {error, no_path}
            end;
        _ ->
            {error, invalid_path}
    end.


read_include_lib_file(Filename, CurrFilename) ->
    case get_inlcude_lib_path(Filename) of
        {ok, FilePath} ->
            BaseName = filename:basename(Filename),
            case file:path_open([FilePath],  BaseName, [read, raw, binary]) of
                {ok, IoDevice, FullName} ->
                    {ok, Data} = file:read(IoDevice, filelib:file_size(FullName)),
                    file:close(IoDevice),
                    {FullName, binary_to_list(Data)};
                {error, Reason} ->
                    throw({read_include_lib_file, Reason, Filename, CurrFilename})
            end;
        {error, Error} ->
            throw({get_inlcude_lib_path, Error, Filename, CurrFilename})
    end.



normalize_record({attribute, La, record, {Record, Fields}} = Form) ->
    case epp:normalize_typed_record_fields(Fields) of
        {typed, NewFields} ->
            {{attribute, La, record, {Record, NewFields}},
                [{attribute, La, type,
                    {{record, Record}, Fields, []}}]};
        not_typed ->
            {Form, []}
    end;
normalize_record(Form) ->
    {Form, []}.


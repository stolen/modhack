-module(modhack).

-export([alias/2, overlay/2, copy/2, patch/2]).

% @doc Create alias 'Alias' for module 'Origin'
alias(Origin, Alias) ->
    overlay([Origin], Alias).

% @doc Create virtual module 'Acceptor' which is overlay
% of modules in list (functions are proxied to first 
% module containing them)
overlay(Donors = [_|_], Acceptor) ->
    % Get aggregated list of exports and their definitions
    {Exports, AbstractFunctions} = collect_exports(Donors),

    % Construct new module abstract form
    AbstractModule = [
        {attribute, 1, module, Acceptor},
        {attribute, 2, export, Exports} |
        AbstractFunctions ],

    % Compile and load constructed module
    {ok, _, BinaryModule} = compile:forms(AbstractModule),
    code:load_binary(Acceptor, "/dev/null", BinaryModule).

% @doc Create binary module copy with given name
copy(Source, Destination) ->
    {_, SrcBinary, _ModFilename} = code:get_object_code(Source),
    ModFilename = "/dev/null",
    ModBinary = beam_renamer:rename(SrcBinary, Destination),
    {module, _} = code:load_binary(Destination, ModFilename, ModBinary).

% @doc patch is something like overlay but it
% backs up target module and includes it on
% the end of overlay chain

% If there are two or more donors, first create
% single donor as overlay of all them
patch(Target, Donors = [_, _|_]) ->
    PolyDonor = list_to_atom("_" ++ atom_to_list(Target) ++ "_polydonor"),
    {module, _} = overlay(Donors, PolyDonor),
    patch(Target, PolyDonor);

patch(Target, [Donor]) ->
    patch(Target, Donor);

patch(Target, Donor) ->
    % Ensure Donor is module
    [_|_] = Donor:module_info(exports),

    % check if one of donors had special function that 
    % returns the name for backed-up module
    TargetBackup = try
        Donor:'_module_backup'(Target)
    catch 
        % OK, I know, this is ugly. Please suggest better solution
        _:_ -> try
                Donor:'_module_backup'()
            catch
                _:_ -> list_to_atom("_" ++ atom_to_list(Target) ++ "_backup")
            end
    end,

    code:unstick_mod(Target),
    copy(Target, TargetBackup),
    overlay([Donor, TargetBackup], Target).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     collect_exports/1,3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Here we cycle through donor list and look for
% exports that are not collected yet.
% Then we construct full list of exports and
% full list of abstract function definitions

% Simple way of calling this function
collect_exports(Donors) -> collect_exports(Donors, [], []).

% process first donor
collect_exports([Donor|MoreDonors], Exports, AbstractFunctions) ->
    % Get exports from Donor and filter out already collected exports
    % module_info/0,1 is added automatically, so ignore them too
    NewExports = Donor:module_info(exports) 
                 -- [{module_info,0}, {module_info,1} | Exports],
    % Generate definitions for new functions
    NewFunctions = [abstract_proxy(Donor, Function, Arity)
                    || {Function, Arity} <- NewExports],
    % Iterate
    collect_exports(MoreDonors, NewExports ++ Exports, NewFunctions ++ AbstractFunctions);

% Final call
collect_exports([], Exports, AbstractFunctions) ->
    {Exports, AbstractFunctions}.

% Construct proxy function in abstract form.
% Usual Erlang syntax for this would be something like
%         Function(Arg1, Arg2, ...) ->
%             Donor:Function(Arg1, Arg2, ...).
abstract_proxy(Donor, Function, Arity) ->
    ArgList = [ {var, 3, list_to_atom("Arg" ++ integer_to_list(N))}
                || N <- lists:seq(1, Arity)],

    % actually generate function
    {function, 3, Function, Arity, [
        {clause, 3, ArgList, [],
            [{call, 4,
                {remote, 4, {atom, 4, Donor}, {atom, 4, Function}},
                ArgList
            }]
        }
    ]}.

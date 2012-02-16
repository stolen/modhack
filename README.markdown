### modhack ###

## What is modhack? ##
modhack is collection of utilities to manipulate wit Erlang modules.

# Exported functions #
* alias(Target, Alias) --- make an alias for module Target (indeed it is proxy)
* copy(Source, Destination) --- copy module at binary-level
* overlay([Mod1, Mod2|...], ResultName) --- combine modules into one supermodule
* patch(Module, Patch) or patch(Module, [Patch1|...]) --- modify Module so that
functions existing in Patches take precedence. 

## Usage example ##
    Eshell V5.9  (abort with ^G)
    1> victim:module_info(exports).
    [{fun4,0},{module_info,0},{module_info,1}]
    2> modhack:overlay([mod1, mod2, mod3], victim).
    {module,victim}
    3> victim:module_info(exports).                
    [{fun2,0},
     {fun3,0},
     {fun2,1},
     {fun1,0},
     {module_info,0},
     {module_info,1}]
    4> victim:fun1().
    mod1
    5> victim:fun2(x).
    {mod2,x}
    6> victim:fun2(). 
    mod3
    7> l(victim), victim:module_info(exports).
    [{fun4,0},{module_info,0},{module_info,1}]
    8> modhack:patch(victim, mod1), victim:fun1().
    mod1


## Example files sources ##
# mod1.erl #
    -module(mod1).
    -compile(export_all).
    fun1() -> ?MODULE.

# mod2.erl #
    -module(mod2).
    -compile(export_all).
    fun1() -> ?MODULE.
    fun2(Arg) -> {?MODULE, Arg}.

# mod3.erl #
    -module(mod3).
    -compile(export_all).
    fun2() -> ?MODULE.
    fun3() -> ?MODULE.

# victim.erl #
    -module(victim).
    -compile(export_all).
    fun4() -> ?MODULE.


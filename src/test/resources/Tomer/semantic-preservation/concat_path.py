#! Start of synth number: 1 of: out
#! 1) a = 'Hello', b = 'World' => out = 'Hello.World' 
#! 2) a = '', b = 'world' => out = 'world' 
if a.isalpha():
    #! Start of synth number: 2 of: out
    #! 1) a = 'Hello', b = 'World' => out = 'Hello.World' 
    #! 2) a = 'path', b = 'field' => out = 'path.field' 
    out = a + "." + b
    #! End of synth number: 2 
else:
    out = b
#! End of synth number: 1
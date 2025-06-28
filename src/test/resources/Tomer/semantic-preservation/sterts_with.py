#! Start of synth number: 1 of: rs
#! 1) s = 'abcdb', c = 'a' => rs = 'abcdb' 
#! 2) s = 'dbcb', c = 'a' => rs = 'adbcb' 
if s.startswith(c):
    rs = s
else:
    #! Start of synth number: 2 of: rs
    #! 1) s = 'dbcb', c = 'a' => rs = 'adbcb' 
    #! 2) s = 'rbcd', c = 'ad' => rs = 'arbcd' 
    rs = c[0] + s
    #! End of synth number: 2
#! End of synth number: 1
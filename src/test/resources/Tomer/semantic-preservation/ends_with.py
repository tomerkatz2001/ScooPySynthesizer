 #! Start of synth number: 1 of: rs
#! 1) s = 'Dcba', c = 'a' => rs = 'Dcba' 
#! 2) s = 'Abcd', c = 'a' => rs = 'Abcda' 
if s.endswith(c):
    rs = s
else:
    #! Start of synth number: 2 of: rs
    #! 1) s = 'Abcd', c = 'a' => rs = 'Abcda' 
    #! 2) s = 'rbcd', c = 'ad' => rs = 'rbcda' 
    rs = s + c[0]
    #! End of synth number: 2
    
#! End of synth number: 1
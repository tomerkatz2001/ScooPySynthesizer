#! Start of synth number: 1 of: out
#! 1) a = 123456 => out = 61728 
#! 2) a = 1234567 => out = 3703702 
if 1 > a % 2:
    #! Start of synth number: 2 of: out
    #! 1) a = 123456 => out = 61728 
    #! 2) a = 500000004 => out = 250000002 
    out = a // 2
    #! End of synth number: 2
    
else:
    #! Start of synth number: 3 of: out
    #! 1) a = 1234567 => out = 3703702 
    #! 2) a = 500000005 => out = 1500000016 
    out = a + a + a + 1
    #! End of synth number: 3
    
#! End of synth number: 1
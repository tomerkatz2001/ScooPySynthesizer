#! Start of synth number: 21 of: rs
#! 1) a = ['Hello', 'World'] => rs = 'H-W' 
#! 2) a = ['Augusta', 'Ada', 'King'] => rs = 'A.A.K'  
if len(a) > 2:
    #! Start of synth number: 22 of: rs
    #! 1) a = ['Augusta', 'Ada', 'King'] => rs = 'A.A.K' 
    #! 2) a = ['Augusta', 'ada', 'King'] => rs = 'A.A.K' 
    rs = ".".join([var[0] for var in a]).upper()
#! End of synth number: 22

else:
    rs = "-".join([var[0] for var in a])
#! End of synth number: 21
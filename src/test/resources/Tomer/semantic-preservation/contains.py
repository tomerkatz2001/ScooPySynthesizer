#! Start of synth number: 1 of: contained
#! 1) n = 40, l = [10, 20, 30, 40] => contained = True 
#! 2) n = 40, l = [20, 50, 70, 20] => contained = False 
contained = n in l
#! End of synth number: 1

#! Start of synth number: 2 of: rs
#! 1) n = 40, l = [10, 20, 30, 40], contained = True => rs = 40 
#! 2) n = 40, l = [20, 50, 70, 20], contained = False => rs = 80 
if contained:
    rs = n
else:
    rs = n + n
#! End of synth number: 2
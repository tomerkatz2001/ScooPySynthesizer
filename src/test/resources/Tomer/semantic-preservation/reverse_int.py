#! Start of synth number: 16 of: rs
#! 1) a = 531 => rs = 135 
#! 2) a = -13 => rs = -31 
if a > -1:
    rs = int(str(a)[::-1])
else:
    #! Start of synth number: 18 of: rs
    #! 1) a = -13 => rs = -31 
    #! 2) a = -341 => rs = -143 
    rs = -1 * int(str(a * -1)[::-1])
    #! End of synth number: 18

#! End of synth number: 16